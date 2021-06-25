*          DATA SET ACENQ02    AT LEVEL 041 AS OF 02/13/19                      
*PHASE T62002B                                                                  
*                                                                               
***********************************************************************         
* US LEVELS                                                                     
* ---------                                                                     
* ID   LVL DATE    JIRA         DESCRIPTION                                     
* ---- --- ------- ------------ ---------------------------------------         
* SGAV 039 10JAN18 <SPEC-12485> ADDING ASSOCIATED CURRENCY FOR CANADIAN         
*                               AGENCIES                                        
* CPAT 039 29MAR18 <SPEC-21361> ADDING MEDIA FILE ALPHA ID FOR PAYABLES         
*                               AND RECEIVABLES TRANSACTIONS                    
* CPAT 040 14JUN18 <SPEC-24463> ADDING MEDIA FILE ALPHA ID FOR CASH             
*                               ACCOUNT (SC).                                   
* RGUP 041 12OCT18 <SPEC-28550> REPORT ON CREDIT CARD PAYMENT METHOD            
*                               CCARD                                           
*                                                                               
***********************************************************************         
*                                                                               
T62002   TITLE 'ACCOUNT ENQUIRY - DETAIL'                                       
T62002   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,**ENQ2**,R5,R6,R7,R8,CLEAR=YES,RR=RE                           
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         LHI   RC,OVERWORK-WORKD                                                
         LA    RC,WORKD(RC)        R8=A(LOCAL WORKIN STORAGE)                   
         USING OVERWRKD,RC                                                      
         ST    RE,ORELO                                                         
         ST    R5,AB1                                                           
         ST    R6,AB2                                                           
         ST    R7,AB3                                                           
         ST    R8,AB4                                                           
*                                                                               
         L     RE,=A(GCTBL)        SET UP GRID COLUMN TABLE                     
         A     RE,ORELO                                                         
         ST    RE,AGCTBL                                                        
         L     RE,=A(SPCGR)        GRID SPECIAL ROUTINE                         
         A     RE,ORELO                                                         
         ST    RE,ASPCGR                                                        
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
         GOTO1 (RF),(R1),C'L   ',DCCAP,DSCAP                                    
*                                                                               
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN10              NO                                           
         GOTOR FSTDIS              YES PERFORM FIRST DISPLAY FUNCTIONS          
         BE    MAIN08                                                           
         TM    DISPFLAG,DISIOMAX                                                
         BO    OKXIT                                                            
         B     ERXIT                                                            
MAIN08   OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
         B     MAIN50                                                           
*                                                                               
MAIN10   TM    OVRSTAT,OVRGDONE    GRIDS PROCESSING FINISHED?                   
         BO    MAINXGX                                                          
         TM    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY?                       
         BNO   *+12                                                             
         TM    OVRSTAT,OVRREVQ     REVERSAL?                                    
         BNO   MAINX                                                            
         TM    DISPFLAG,ALLREADQ   ALL RECORDS READ?                            
         BO    MAIN20                                                           
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BO    MAIN15                                                           
         GOTO1 AIO,IOREAD+IOACCDIR+IO1 RE-ESTABLISH IO SEQUENCE                 
         BE    MAIN15                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     MAINX                                                            
*                                                                               
MAIN15   CLC   TSCURRNO,TSLSTREC   HAVE WE ALLREADY GOT RECORD IN TSAR?         
         BH    MAIN35              NO                                           
*                                                                               
MAIN20   GOTO1 ATSARGET,TSCURRNO   GET TSAR RECORD                              
*                                                                               
         L     R2,ATSARREC         R2=A(TSAR RECORD)                            
         USING TSARRECD,R2                                                      
*                                                                               
         CLI   TSARFMT,TSTOTITM    TOTAL LINE ITEM?                             
         BNE   MAIN22                                                           
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BO    *+12                 YES                                         
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
         B     MAIN65                                                           
*                                                                               
MAIN22   MVC   WORK(L'TSARKYNO),TSARKYNO   SAVE TSAR REC NO                     
         MVC   IODAOVER,TSAROVER       GET DISK ADDRESS                         
         MVC   DADDRESS,IODAOVER       SET DISK ADDRESS                         
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    MAIN30                                                           
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BNO   *+12                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
MAIN30   L     R3,AIO1                                                          
         BAS   RE,FILTER           APPLY FILTERING TO TRANS RECORD              
         BNE   MAIN65              DO WE WANT TO KEEP THIS RECORD?              
         BAS   RE,BLDTSDAT         BUILD TSAR RECORD FOR DATA LINE(S)           
         B     MAIN65                                                           
         DROP  R2                                                               
*                                                                               
MAIN35   TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BNO   MAIN40              YES                                          
         LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATOOMNY)                                           
         B     ERXIT                                                            
*                                                                               
MAIN40   TM    DISPFLAG,ALLREADQ   HAVE ALL RECORDS BEEN READ?                  
         BO    MAINX               YES                                          
*                                                                               
MAIN45   GOTO1 ANXTREC,NXTRMODE    GET NEXT RECORD                              
         BE    MAIN50                                                           
         TM    DISPFLAG,DISIOMAX                                                
         BO    MAINX                                                            
         DC    H'0'                                                             
*                                                                               
MAIN50   LA    R3,IOKEY            R3=A(IOAREA1 CONTAINING RECORD)              
         USING TRNRECD,R3                                                       
         MVC   KEYSAVE,TRNKEY      SAVE KEY FOR SEQ RESTORE                     
         CLC   TRNKCULA,IOKEYSAV                                                
         BNE   MAIN70                                                           
*                                                                               
         LA    R3,IOKEY            R3=A(RECORD KEY)                             
         CLC   TRNKDATE,SPACES     TRANSACTION RECORD?                          
         BNH   MAIN45                                                           
         BAS   RE,FILTKEY          APPLY FILTERING TO TRANS KEY                 
         BNE   MAIN45              DO WE WANT TO KEEP THIS RECORD?              
         MVC   DADDRESS,TRNKDA     GET DISK ADDRESS                             
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    MAIN60                                                           
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BNO   *+12                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
*                                                                               
MAIN60   L     R3,AIO1                                                          
         GOTO1 AOFFTRN             APPLY SECURITY CHECK                         
         CLI   OFFLFLAG,0                                                       
         BNE   MAIN45                                                           
         BAS   RE,FILTER           APPLY FILTERING TO TRANS RECORD              
         BNE   MAIN45              DO WE WANT TO KEEP THIS RECORD?              
         BAS   RE,BLDTSDAT         BUILD TSAR RECORD FOR DATA LINE(S)           
         BAS   RE,BLDTSDET         BUILD TSAR RECORD (DISK ADDR)                
         BNE   ERXIT                                                            
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BNL   MAIN65                                                           
         ICM   RF,3,TSCURRNO       UPDATE TSAR RECORD COUNTER                   
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN35                                                           
*                                                                               
MAIN65   GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   MAINX               SCREEN IS FULL                               
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN15                                                           
*                                                                               
MAIN70   OC    TSLSTREC,TSLSTREC   ANY RECORDS DISPLAYED SO FAR?                
         BNZ   MAIN75                                                           
         OI    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY                        
         TM    OVRSTAT,OVRREVQ     DOES ACCOUNT CONTAIN REVERSALS?              
         BNO   MAINX                                                            
*                                                                               
MAIN75   OI    DISPFLAG,ALLREADQ   ALL RECORDS READ                             
         BAS   RE,TOTAL            DEAL WITH TOTAL LINE                         
         B     MAINX                                                            
*                                                                               
MAINX    B     OKXIT                                                            
*                                                                               
MAINXGX  GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL)                                   
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    OKXIT               SCREEN IS FULL                               
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        FILTER TRANSACTION KEYS (ACCDIR)                             *         
* ON ENTRY R3=A(TRANSACTION KEY)                                      *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
FILTKEY  NTR1                                                                   
*                                                                               
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         USING TRNRECD,R3                                                       
*                                                                               
         CLI   TRNKSTAT,TIMKRI1Q   X'61'   IS THIS A TIME RECORD?               
         BE    FILTKRJX            YES, EXIT                                    
*                                                                               
         CLC   TRNKREF,=C'*TIME*'          IS THIS A TIME RECORD?               
         BE    FILTKRJX            YES, EXIT                                    
*                                                                               
         TM    TRNKSTAT,TRNSREVS   REVERSED                                     
         BNO   *+8                                                              
         OI    OVRSTAT,OVRREVQ     SET ACCOUNT HAS REVERSALS                    
*                                                                               
         CLI   CONTLEN,0                                                        
         BE    FILTK08                                                          
         MVC   WORK,SPACES                                                      
         LA    RF,L'TRNKULC                                                     
         LA    R1,TRNKULC                                                       
         CLI   0(R1),C' '                                                       
         BH    FILTK05                                                          
         LA    R1,1(R1)                                                         
         BCT   RF,*-12                                                          
         B     FILTK06                                                          
FILTK05  BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R1)                                                    
         SR    RF,RF                                                            
         ICM   RF,1,CONTLEN        LENGTH OF CONTRA CODE                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),CONTRA      MATCHES CONTRA?                              
         BNE   FILTK06                                                          
         CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER?                             
         BE    FILTKRJX                                                         
         B     FILTK08                                                          
FILTK06  CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER?                             
         BNE   FILTKRJX                                                         
*                                                                               
FILTK08  TM    TRNKSTAT,TRNSDRFT   DRAFT?                                       
         BNO   FILTK10                                                          
         CLI   ODRAFT,C'Y'                                                      
         BE    FILTK20                                                          
         CLI   ODRAFT,C'O'                                                      
         BE    FILTK20                                                          
         B     FILTKRJX                                                         
FILTK10  CLI   ODRAFT,C'O'                                                      
         BE    FILTKRJX                                                         
*                                                                               
FILTK20  TM    TRNKSTA2,TRNSPEEL    PEELED?                                     
         BNO   FILTK30                                                          
         CLI   OPEELED,C'Y'                                                     
         BE    FILTK40                                                          
         CLI   OPEELED,C'O'                                                     
         BE    FILTK40                                                          
         B     FILTKRJX                                                         
FILTK30  CLI   OPEELED,C'O'                                                     
         BE    FILTKRJX                                                         
*                                                                               
FILTK40  CLI   TRNKREF+MBVOFFSQ,MBVINDIQ 127+ TRANSACTIONS?                     
         BE    FILTK45             ACCMST MUST BE USED FOR FILTER               
         OC    OREF,OREF           REF FILTER ?                                 
         BZ    FILTK45                                                          
         GOTO1 ASCOMP,DMCB,TRNKREF,(OREFLN1,OREFVL1),(OREFLN2,OREFVL2),C        
               OREFFI                                                           
         BNE   FILTREJX                                                         
*                                                                               
FILTK45  OC    OKREF,OKREF          REF KEY FILTER ?                            
         BZ    FILTK50                                                          
         GOTO1 ASCOMP,DMCB,TRNKREF,(OKREFLN1,OKREFVL1),                C        
               (OKREFLN2,OKREFVL2),OKREFFI                                      
         BNE   FILTREJX                                                         
*                                                                               
FILTK50  OC    ODATE,ODATE         FILTERING ON TRANSACTION DATE?               
         BZ    FILTK60                                                          
         GOTO1 ADCOMP,DMCB,(L'TRNKDATE,TRNKDATE),ODATEST,ODATEEN,ODATEFC        
               I                                                                
         BNE   FILTREJX                                                         
*                                                                               
FILTK60  OC    OMOS,OMOS           FILTERING ON TRANSACTION MOS                 
         BZ    FILTK70                                                          
         GOTO1 ADCOMP,DMCB,(L'TRNKSMOS,TRNKSMOS),OMOSST,OMOSEN,OMOSFI           
         BNE   FILTREJX                                                         
*                                                                               
FILTK70  OC    OFIXD,OFIXD         FIXED REF FILTER?                            
         BZ    FILTK80                                                          
         CLC   SBNKUL,TRNKCUNT     BANK TO BANK?                                
         BNE   FILTKX                                                           
         GOTO1 ASCOMP,DMCB,TRNKREF,(OFIXDLN1,OFIXDVL1),(OFIXDLN2,      X        
               OFIXDVL2),OFIXDFI                                                
         BNE   FILTKRJX                                                         
*                                                                               
FILTK80  CLC   SBNKUL,BASKEY       SC LEDGER DATE/REF= TRANS DATE/REF           
         BNE   FILTKX                                                           
         GOTO1 CHEQFILT,DMCB,TRNKREF,TRNKDATE                                   
         BNE   FILTREJX                                                         
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
FILTER   NTR1                                                                   
*                                                                               
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         L     R3,AIO1             R3=A(ACCMST RECORD)                          
         USING TRNRECD,R3                                                       
*                                                                               
         GOTO1 ASETELE,TRNRFST     SET ELEMENT ADDRESSES                        
*                                                                               
         XC    ASSOCURR,ASSOCURR   INIT ASSOCIATED CURRENCY                     
         CLI   TRNKREF+MBVOFFSQ,MBVINDIQ 127+ TRANSACTIONS?                     
         BNE   FILT08              ALREADY FILTERED ACCDIR                      
         OC    OREF,OREF           REF FILTER ?                                 
         BZ    FILT08                                                           
         LA    RF,TRNKREF          RF=A(TRANSACTION REF)                        
         ICM   RE,15,AFFTELD       RE=A(FREE FORM TEXT ELEMENT)                 
         BZ    FILT06                                                           
         USING FFTELD,RE                                                        
         SR    R0,R0                                                            
FILT04   CLI   FFTEL,0                                                          
         BE    FILT06                                                           
         CLI   FFTEL,FFTELQ                                                     
         BNE   *+12                                                             
         CLI   FFTTYPE,FFTTKREF    KEY REF NUMBER FOR BANK VOID?                
         BE    *+14                                                             
         IC    R0,FFTLN                                                         
         AR    RE,R0                                                            
         B     FILT04                                                           
         LA    RF,FFTDATA                                                       
         DROP  RE                                                               
*                                                                               
FILT06   GOTO1 ASCOMP,DMCB,(RF),(OREFLN1,OREFVL1),(OREFLN2,OREFVL2),   C        
               OREFFI                                                           
         BNE   FILTREJX                                                         
*-----------------------------------                                            
* TRANSACTION ELEMENT X'44'                                                     
*-----------------------------------                                            
         USING TRNELD,R4                                                        
FILT08   ICM   R4,15,ATRNELD                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TRANCURR,COMPCURR   SET TRANSACTION CURRENCY FROM AGENCY         
         ZAP   TRANAMNT,TRNAMNT    AGENCY TRANSACTION AMOUNT                    
*                                                                               
         TM    TRNSTAT,TRNSAUTH    AUTHORISED?                                  
         BNO   FILT10                                                           
         CLI   OAUTH,C'N'                                                       
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT10   CLI   OAUTH,C'O'                                                       
         BE    FILTREJX                                                         
*                                                                               
         TM    TRNSTAT,TRNSDR      DEBIT/CREDIT?                                
         BNO   FILT20                                                           
         CLI   OCREDIT,C'O'                                                     
         BE    FILTREJX                                                         
         CLI   ODEBIT,C'N'                                                      
         BE    FILTREJX                                                         
         B     FILT30                                                           
FILT20   CLI   ODEBIT,C'O'                                                      
         BE    FILTREJX                                                         
         CLI   OCREDIT,C'N'                                                     
         BE    FILTREJX                                                         
*                                                                               
FILT30   TM    TRNSTAT,TRNSHOLD    HELD?                                        
         BNO   FILT40                                                           
         CLI   OHELD,C'N'                                                       
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT40   CLI   OHELD,C'O'                                                       
         BE    FILTREJX                                                         
*                                                                               
         CLC   SBNKUL,BASKEY       IF BANK LEDGER MEANS RECONCILED              
         BE    FILT60                                                           
*                                                                               
         TM    TRNSTAT,TRNSAPPR    SELECTED?                                    
         BNO   FILT50                                                           
         CLI   OSELECT,C'N'                                                     
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT50   CLI   OSELECT,C'O'                                                     
         BE    FILTREJX                                                         
         B     FILT80                                                           
*                                                                               
FILT60   TM    TRNSTAT,TRNSBREC    RECONCILED?                                  
         BNO   FILT70                                                           
         CLI   ORECON,C'N'                                                      
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT70   CLI   ORECON,C'O'                                                      
         BE    FILTREJX                                                         
*                                                                               
FILT80   TM    TRNSTAT,TRNSURG     URGENT TRANSACTION?                          
         BNO   FILT90                                                           
         CLI   OURGENT,C'N'                                                     
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT90   CLI   OURGENT,C'O'                                                     
         BE    FILTREJX                                                         
*                                                                               
         OC    OBATCH,OBATCH       BATCH REF FILTER?                            
         BZ    FILT100                                                          
         GOTO1 ASCOMP,DMCB,TRNBTCH,(OBATLN1,OBATVL1),(OBATLN2,OBATVL2),C        
               OBATFI                                                           
         BNE   FILTREJX                                                         
*                                                                               
FILT100  SR    RF,RF                                                            
         ICM   RF,1,OBTYPEVL                                                    
         BZ    FILT120                                                          
         SR    RE,RE                                                            
         IC    RE,TRNTYPE                                                       
         CR    RE,RF                                                            
         BE    FILT110                                                          
         CLI   OBTYPEFI,NEGFILTR                                                
         BNE   FILTREJX                                                         
         B     FILT120                                                          
FILT110  CLI   OBTYPEFI,NEGFILTR                                                
         BE    FILTREJX                                                         
*                                                                               
FILT120  CLI   OTIME,0             IF TIME FILTER                               
         BE    FILT130                                                          
         OC    APRTELD,APRTELD     THEN PERSONNEL RATE REQUIRED                 
         BZ    FILTREJX                                                         
*                                                                               
FILT130  OC    OFIXD,OFIXD                                                      
         BZ    FILT140                                                          
         CLC   SBNKUL,TRNKCUNT     IF FIXED # FILTER (NOT BNK TO BNK)           
         BNE   FILT155                                                          
FILT140  OC    OPAYDATE,OPAYDATE   OR PAYMENT DATE FILTER                       
         BNZ   FILT150                                                          
         OC    OPAYREF,OPAYREF     OR PAYMENT REFERENCE FILTER                  
         BZ    FILT165                                                          
FILT150  CLC   SBNKUL,BASKEY       SC LEDGER DATE/REF= TRANS DATE/REF           
         BE    FILT165                                                          
FILT155  ICM   RF,15,ATRSELD       RF=A(TRANSACTION STATUS ELEMENT)             
         BZ    *+12                                                             
         TM    TRSSTAT-TRSELD(RF),TRSSOFFS    CONTRA'D?                         
         BO    FILT165                                                          
         CLC   SRECUL,BASKEY       RECEIVABLE?                                  
         BE    FILT160                                                          
         OC    AMPYELD,AMPYELD     THEN MANUAL PAYMENT REQUIRED                 
         BZ    FILTREJX                                                         
         B     FILT165                                                          
*                                                                               
FILT160  OC    ARALELD,ARALELD     THEN REC ALLOC REQUIRED                      
         BZ    FILTREJX                                                         
*                                                                               
FILT165  OC    OSUBREF,OSUBREF     IF SUB-REF FILTER                            
         BNZ   *+14                                                             
         OC    OPJOB,OPJOB         OR PROJOB FILTER                             
         BZ    *+14                                                             
         OC    AOTHELD,AOTHELD     THEN OTHERS ELEMENT REQUIRED                 
         BZ    FILTREJX                                                         
*                                                                               
         OC    ORUNNO,ORUNNO       IF RUN NUMBER FILTER                         
         BZ    *+14                                                             
         OC    AAFPELD,AAFPELD     THEN ARTISTE FEE PAYMENT REQUIRED            
         BZ    FILTREJX                                                         
*                                                                               
         TM    SLEDGATR,LEDGDEB    IF DEBTORS LEDGER                            
         BNO   FILT175                                                          
         CLI   OCONT,C'O'          AND CONTRA'D FILTER                          
         BNE   FILT175                                                          
         OC    ARALELD,ARALELD     THEN RECV ALLOC REQUIRED                     
         BZ    FILTREJX                                                         
*-----------------------------------                                            
* W/C,CLIENT,PRODUCT,JOB,EST,SRC                                                
*-----------------------------------                                            
FILT175  OC    OWCODE,OWCODE       FILTERING ON WORKCODE?                       
         BNZ   FILT180                                                          
         OC    OCLIENT,OCLIENT     FILTERING ON CLIENT?                         
         BNZ   FILT225                                                          
         OC    OPROD,OPROD         FILTERING ON PRODUCT?                        
         BNZ   FILT225                                                          
         OC    OJOB,OJOB           FILTERING ON JOB?                            
         BNZ   FILT180                                                          
         OC    OESTIMAT,OESTIMAT   FILTERING ON ESTIMATE?                       
         BNZ   FILT180                                                          
         OC    OSRC,OSRC           FILTERING ON SOURCE?                         
         BZ    FILT255                                                          
*                                                                               
FILT180  TM    SLEDGATR,LEDGCRD    CREDITOR LEDGER?                             
         BO    FILT190                                                          
         CLC   ONER,BASKEY         PERSONNEL?                                   
         BNE   FILT185                                                          
         OC    APCIELD,APCIELD     PROJ CONTROL ELEMENT REQUIRED                
         BZ    FILTREJX                                                         
         B     FILT255                                                          
*                                                                               
FILT185  CLC   SRECUL,BASKEY       RECEIVABLE?                                  
         BE    *+14                                                             
         CLC   SI,BASKEY           INCOME?                                      
         BNE   FILT190                                                          
*                                                                               
         OC    AMDTELD,AMDTELD     MEDIA TRANSFER ELEMENT REQUIRED              
         BNZ   FILT255                                                          
*                                                                               
         OC    AMDPELD,AMDPELD     MEDIA TRANSFER ELEMENT REQUIRED              
         BNZ   FILT255                                                          
*                                                                               
         OC    ASORELD,ASORELD     SOURCE ELEMENT REQUIRED                      
         BZ    FILTREJX                                                         
         B     FILT255                                                          
*                                                                               
FILT190  GOTO1 ABLDSRC                                                          
         OC    OCLIENT,OCLIENT     FILTERING ON CLIENT?                         
         BZ    FILT195                                                          
         CLC   SPROUL,SRCWORK                                                   
         BNE   FILTREJX                                                         
         GOTO1 ASCOMP,DMCB,SRCWORK+L'SPROUL,(OCLINLN1,OCLINVL1),       X        
               (OCLINLN2,OCLINVL2),OCLINFI                                      
         BNE   FILTREJX                                                         
*                                                                               
FILT195  OC    OPROD,OPROD         FILTERING ON PRODUCT?                        
         BZ    FILT200                                                          
         CLC   SPROUL,SRCWORK                                                   
         BNE   FILTREJX                                                         
         GOTO1 ASCOMP,DMCB,SRCWORK+L'SPROUL,(OPRODLN1,OPRODVL1),       X        
               (OPRODLN2,OPRODVL2),OPRODFI                                      
         BNE   FILTREJX                                                         
*                                                                               
FILT200  OC    OJOB,OJOB           FILTERING ON JOB?                            
         BZ    FILT205                                                          
         CLC   SPROUL,SRCWORK                                                   
         BNE   FILTREJX                                                         
         GOTO1 ASCOMP,DMCB,SRCWORK+L'SPROUL,(OJOBLN1,OJOBVL1),         X        
               (OJOBLN2,OJOBVL2),OJOBFI                                         
         BNE   FILTREJX                                                         
*                                                                               
FILT205  OC    OESTIMAT,OESTIMAT   FILTERING ON ESTIMATE?                       
         BZ    FILT210                                                          
         CLC   SPROUL,SRCWORK                                                   
         BNE   FILTREJX                                                         
         GOTO1 ASCOMP,DMCB,SRCWORK+L'SPROUL,(OESTLN1,OESTVL1),         X        
               (OESTLN2,OESTVL2),OESTFI                                         
         BNE   FILTREJX                                                         
*                                                                               
FILT210  OC    OSRC,OSRC           FILTERING ON SOURCE?                         
         BZ    FILT255                                                          
         CLC   SRCWORK,SPACES                                                   
         BE    FILTREJX                                                         
         GOTO1 ASCOMP,DMCB,SRCWORK,(OSRCLN1,OSRCVL1),(OSRCLN2,OSRCVL2),X        
               OSRCFI                                                           
         BNE   FILTREJX                                                         
         B     FILT255                                                          
*                                                                               
         USING FFTELD,R4           FREE FORM TEXT X'DB'                         
FILT225  ICM   R4,15,AFFTELD       CHECK FREE FORM FOR CLI, PRO                 
         BZ    FILT232                                                          
FILT230  CLI   FFTEL,0                                                          
         BE    FILT232                                                          
         CLI   FFTEL,FFTELQ        FREE FORM TEXT ELEMENT                       
         BNE   *+12                                                             
         CLI   FFTTYPE,FFTTCLPR    ASSOCIATED CURRENCY?                         
         BE    FILT235                                                          
         SR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         AR    R4,RF                                                            
         B     FILT230                                                          
FILT232  ICM   R4,15,ATRNELD                                                    
         B     FILT180                                                          
*                                                                               
FILT235  OC    OCLIENT,OCLIENT     FILTERING ON CLIENT?                         
         BZ    FILT240                                                          
         GOTO1 ASCOMP,DMCB,(0,FFTCLAC),(OCLINLN1,OCLINVL1),            X        
               (OCLINLN2,OCLINVL2),OCLINFI                                      
         BNE   FILTREJX                                                         
*                                                                               
FILT240  OC    OPROD,OPROD         FILTERING ON PRODUCT?                        
         BZ    FILT245                                                          
         GOTO1 ASCOMP,DMCB,(0,FFTPRAC),(OPRODLN1,OPRODVL1),            X        
               (OPRODLN2,OPRODVL2),OPRODFI                                      
         BNE   FILTREJX                                                         
FILT245  ICM   R4,15,ATRNELD                                                    
         B     FILT200                                                          
         DROP  R4                                                               
*-----------------------------------                                            
* MEDIA TRANSFER ELEMENT X'1A' OR X'6A'                                         
*-----------------------------------                                            
         USING MDTELD,R4           MEDIA TRANSFER ELEMENT                       
FILT255  ICM   R4,15,AMDTELD                                                    
         BNZ   FILT256                                                          
         ICM   R4,15,AMDPELD                                                    
         BZ    FILT266                                                          
*                                                                               
FILT256  MVC   TEMP(L'ACTKULA),SPACES                                           
         LA    RE,TEMP                                                          
         MVC   0(L'SPROUL,RE),SPROUL UNIT/LEDGER                                
         LA    RE,L'SPROUL(RE)                                                  
         MVC   0(L'MDTCLI,RE),MDTCLI CLIENT                                     
         LA    RE,L'MDTCLI(RE)                                                  
         MVC   0(L'MDTPRD,RE),MDTPRD PRODUCT                                    
         LA    RE,L'MDTPRD(RE)                                                  
         MVC   0(L'MDTJOB,RE),MDTJOB JOB/ESTIMATE                               
*                                                                               
         OC    OCLIENT,OCLIENT     FILTERING ON CLIENT?                         
         BZ    FILT260                                                          
         GOTO1 ASCOMP,DMCB,TEMP+L'SPROUL,(OCLINLN1,OCLINVL1),          X        
               (OCLINLN2,OCLINVL2),OCLINFI                                      
         BNE   FILTREJX                                                         
*                                                                               
FILT260  OC    OPROD,OPROD         FILTERING ON PRODUCT?                        
         BZ    FILT262                                                          
         GOTO1 ASCOMP,DMCB,TEMP+L'SPROUL,(OPRODLN1,OPRODVL1),          X        
               (OPRODLN2,OPRODVL2),OPRODFI                                      
         BNE   FILTREJX                                                         
*                                                                               
FILT262  OC    OJOB,OJOB           FILTERING ON JOB?                            
         BZ    FILT264                                                          
         GOTO1 ASCOMP,DMCB,TEMP+L'SPROUL,(OJOBLN1,OJOBVL1),            X        
               (OJOBLN2,OJOBVL2),OJOBFI                                         
         BNE   FILTREJX                                                         
*                                                                               
FILT264  OC    OESTIMAT,OESTIMAT   FILTERING ON ESTIMATE?                       
         BZ    FILT266                                                          
         GOTO1 ASCOMP,DMCB,TEMP+L'SPROUL,(OESTLN1,OESTVL1),            X        
               (OESTLN2,OESTVL2),OESTFI                                         
         BNE   FILTREJX                                                         
         DROP  R4                                                               
*-----------------------------------                                            
* OTHERS ELEMENT X'23'                                                          
*-----------------------------------                                            
         USING OTHELD,R4                                                        
FILT266  ICM   R4,15,AOTHELD                                                    
         BZ    FILT270                                                          
         OC    OPJOB,OPJOB         PROJOB?                                      
         BZ    FILT268                                                          
         GOTO1 ASCOMP,DMCB,OTHNUM,(OPJLN1,OPJVL1),(OPJLN2,OPJVL2),OPJFI         
         BNE   FILTREJX                                                         
FILT268  OC    OSUBREF,OSUBREF     SUBREF?                                      
         BZ    FILT270                                                          
         GOTO1 ASCOMP,DMCB,OTHNUM,(OSUBRLN1,OSUBRVL1),(OSUBRLN2,OSUBRVLC        
               2),OSUBRFI                                                       
         BNE   FILTREJX                                                         
         DROP  R4                                                               
*-----------------------------------                                            
* PERSONNEL RATE ELEMENT X'40' *                                                
*-----------------------------------                                            
         USING PRTELD,R4                                                        
FILT270  ICM   R4,15,APRTELD                                                    
         BZ    FILT275                                                          
         CLI   OTIME,0             TIME FILTER?                                 
         BE    FILT275                                                          
         CLI   OTIME,C'B'                                                       
         BNE   *+12                                                             
         TM    PRTSTAT,PRTSBILQ    BILLABLE TIME                                
         BNO   FILTREJX                                                         
         CLI   OTIME,C'N'                                                       
         BNE   *+12                                                             
         TM    PRTSTAT,PRTSNOTQ    NON-BILLABLE TIME                            
         BNO   FILTREJX                                                         
         CLI   OTIME,C'R'                                                       
         BNE   FILT275                                                          
         TM    PRTSTAT,PRTSRTEQ    SPECIAL NON-BILLABLE TIME                    
         BNO   FILTREJX                                                         
         B     FILT275                                                          
*-----------------------------------                                            
* SUBSIDIARY CASH INFO ELEMENT X'50'                                            
*-----------------------------------                                            
         USING SCIELD,R4           SUBSIDIARY CASH ELEMENT                      
FILT275  ICM   R4,15,ASCIELD                                                    
         BZ    FILT290                                                          
FILT280  CLI   SCIEL,SCIELQ                                                     
         BNE   FILT290                                                          
         CLI   SCITYPE,SCITCDSC    CASH DISCOUNT?                               
         BE    FILT285                                                          
         SR    RF,RF                                                            
         IC    RF,SCILN                                                         
         AR    R4,RF                                                            
         B     FILT280                                                          
*                                                                               
FILT285  CLI   ODISC,C'N'          NO CASH DISCOUNT ITEMS REQUIRED?             
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT290  CLI   ODISC,C'O'          ONLY DISCOUNT ITEMS REQUIRED?                
         BE    FILTREJX                                                         
*-----------------------------------                                            
* PROJECT CONTROL ELEMENT                                                       
*-----------------------------------                                            
         USING PCIELD,R4           PROJECT CONTROL ELEMENT                      
         ICM   R4,15,APCIELD                                                    
         BZ    FILT320                                                          
*                                                                               
         OC    OWCODE,OWCODE       FILTERING ON WORKCODE?                       
         BZ    FILT300                                                          
         CLC   PCITSK,SPACES                                                    
         BNH   FILTREJX                                                         
         CLC   OWCODEVL,PCITSK                                                  
         BE    FILT295                                                          
         CLI   OWCODEFI,NEGFILTR                                                
         BNE   FILTREJX                                                         
         B     FILT300                                                          
FILT295  CLI   OWCODEFI,NEGFILTR                                                
         BE    FILTREJX                                                         
*                                                                               
FILT300  OC    OCLIENT,OCLIENT     FILTERING ON CLIENT?                         
         BZ    FILT305                                                          
         GOTO1 ASCOMP,DMCB,PCIPRJT+3,(OCLINLN1,OCLINVL1),              X        
               (OCLINLN2,OCLINVL2),OCLINFI                                      
         BNE   FILTREJX                                                         
*                                                                               
FILT305  OC    OPROD,OPROD         FILTERING ON PRODUCT?                        
         BZ    FILT310                                                          
         GOTO1 ASCOMP,DMCB,PCIPRJT+3,(OPRODLN1,OPRODVL1),              X        
               (OPRODLN2,OPRODVL2),OPRODFI                                      
         BNE   FILTREJX                                                         
*                                                                               
FILT310  OC    OJOB,OJOB           FILTERING ON JOB?                            
         BZ    FILT315                                                          
         GOTO1 ASCOMP,DMCB,PCIPRJT+3,(OJOBLN1,OJOBVL1),                X        
               (OJOBLN2,OJOBVL2),OJOBFI                                         
         BNE   FILTREJX                                                         
*                                                                               
FILT315  OC    OSRC,OSRC           FILTERING ON SOURCE?                         
         BZ    FILT320                                                          
         GOTO1 ASCOMP,DMCB,PCIPRJT+1,(OSRCLN1,OSRCVL1),                X        
               (OSRCLN2,OSRCVL2),OSRCFI                                         
         BNE   FILTREJX                                                         
*-----------------------------------                                            
* TRANSACTION STATUS ELEMENT X'60'                                              
*-----------------------------------                                            
         USING TRSELD,R4                                                        
FILT320  ICM   R4,15,ATRSELD                                                    
         BZ    FILT370                                                          
         CLI   OCONT,0             CONTRA'D OPTION?                             
         BE    FILT330                                                          
         TM    SLEDGATR,LEDGCRD    CREDITOR LEDGER?                             
         BNO   FILT330                                                          
         TM    TRSSTAT,TRSSOFFS    CONTRA'D?                                    
         BNO   FILT328                                                          
         CLI   OCONT,C'N'          NO CONTRA'D ITEMS REQUIRED?                  
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT328  CLI   OCONT,C'O'          ONLY CONTRA'D ITEMS REQUIRED?                
         BE    FILTREJX                                                         
*                                                                               
FILT330  TM    TRSSTAT,TRSSOFFS    CONTR'D?                                     
         BNO   FILT335                                                          
         OC    OPAYDATE,OPAYDATE   PAID DATE FILTER?                            
         BZ    FILT335                                                          
         GOTO1 VDATCON,DMCB,(2,TRSUDAT),(1,TEMPDAT) USED DATE                   
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),OPAYDST,OPAYDEN,        X        
               OPAYDFI                                                          
         BNE   FILTREJX                                                         
*                                                                               
FILT335  OC    OFIXD,OFIXD         FIXED REF FILTER?                            
         BZ    *+12                                                             
         TM    TRSSTAT2,TRSSANBP   ANALYSED PAYMENT?                            
         BNO   FILTREJX                                                         
*                                                                               
         OC    OSTATDT,OSTATDT     FILTERING ON STATEMENT DATE?                 
         BZ    FILT340              NO                                          
         OC    TRSBSTDT,TRSBSTDT   ANY STATEMENT DATE?                          
         BNZ   FILT338              YES                                         
         CLI   OSTATDT,C'Y'        GOOD                                         
         BE    FILT340                                                          
         CLI   OSTATDT,C'N'        GOOD                                         
         BE    FILT340                                                          
         B     FILTREJX             THEN REJECT                                 
FILT338  CLI   OSTATDT,C'Y'        GOOD                                         
         BE    FILT340                                                          
         CLI   OSTATDT,C'O'        GOOD                                         
         BE    FILT340                                                          
         CLI   OSTATDT,C'N'        IF A DATE AND NO DATE WANTED                 
         BE    FILTREJX             THEN REJECT                                 
         GOTO1 VDATCON,DMCB,(2,TRSBSTDT),(1,TEMPDAT) ACTIVITY DATE              
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),OSTATST,OSTATEN,OSTATFI          
         BNE   FILTREJX                                                         
*                                                                               
FILT340  TM    TRSSTAT,TRSSVOID    ITEM VOIDED?                                 
         BNO   FILT342                                                          
         CLI   OVOID,C'N'                                                       
         BE    FILTREJX                                                         
         B     FILT345                                                          
FILT342  CLI   OVOID,C'O'                                                       
         BE    FILTREJX                                                         
*                                                                               
FILT345  MVC   POSTPROG,TRSSTAT3                                                
         NI    POSTPROG,TRSSPROG                                                
         OC    OACT,OACT          FILTERING ON ACTIVITY DATE?                   
         BZ    FILT350                                                          
         OC    TRSDATE,TRSDATE                                                  
         BZ    FILTREJX                                                         
         GOTO1 VDATCON,DMCB,(2,TRSDATE),(1,TEMPDAT) ACTIVITY DATE               
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),OACTST,OACTEN,OACTFI             
         BNE   FILTREJX                                                         
*                                                                               
FILT350  OC    OUSED,OUSED        FILTERING ON USED DATE?                       
         BZ    FILT355                                                          
         OC    TRSUDAT,TRSUDAT                                                  
         BZ    FILTREJX                                                         
         GOTO1 VDATCON,DMCB,(2,TRSUDAT),(1,TEMPDAT) USED DATE                   
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),OUSEST,OUSEEN,OUSEFI             
         BNE   FILTREJX                                                         
*                                                                               
FILT355  CLI   OTTYPE,0           TIME TYPE FILTER?                             
         BE    FILT360                                                          
         CLI   OTTYPE,C'T'                                                      
         BNE   *+12                                                             
         TM    TRSSTAT2,TRSSTIME  TIME SHEET REGULAR?                           
         BNO   FILTREJX                                                         
         CLI   OTTYPE,C'M'                                                      
         BNE   *+12                                                             
         TM    TRSSTAT2,TRSSTMSS  TIME SHEET MISSING?                           
         BNO   FILTREJX                                                         
         CLI   OTTYPE,C'A'                                                      
         BNE   FILT360                                                          
         TM    TRSSTAT2,TRSSTADJ  TIME SHEET ADJUSTED?                          
         BNO   FILTREJX                                                         
*                                                                               
FILT360  TM    TRNRSTAT,TRNSREVS   REVERSAL?                                    
         BNO   FILT365                                                          
         CLI   OREVERSE,C'Y'                                                    
         BE    FILT370                                                          
         CLI   OREVERSE,C'O'                                                    
         BE    FILT370                                                          
         OC    OMOS,OMOS           FILTERING ON TRANSACTION MOS?                
         BZ    FILTREJX                                                         
         OC    TRSRMOS,TRSRMOS     IF NO REV MOS ASSUME SAME AS TRAN            
         BZ    FILTREJX                                                         
         GOTO1 ADCOMP,DMCB,(L'TRSRMOS,TRSRMOS),OMOSST,OMOSEN,OMOSFI             
         BE    FILTREJX                                                         
         B     FILT370                                                          
FILT365  CLI   OREVERSE,C'O'                                                    
         BE    FILTREJX                                                         
         DROP  R4                                                               
*-----------------------------------                                            
* DUE DATE ELEMENT X'61'                                                        
*-----------------------------------                                            
         USING DUEELD,R4                                                        
FILT370  OC    ODUE,ODUE           FILTERING ON DUE DATE?                       
         BZ    FILT380              NO                                          
         ICM   R4,15,ADUEELD                                                    
         BZ    FILT375                                                          
         GOTO1 VDATCON,DMCB,(2,DUEDATE),(1,TEMPDAT) DUE DATE                    
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),ODUEST,ODUEEN,ODUEFI             
         BNE   FILTREJX                                                         
         B     FILT380                                                          
*                                                                               
FILT375  GOTO1 ADCOMP,DMCB,(L'TRNKDATE,TRNKDATE),ODUEST,ODUEEN,ODUEFI           
         BNE   FILTREJX                                                         
         DROP  R4                                                               
*-----------------------------------                                            
* MANUAL PAYMENT ELEMENT X'64'                                                  
*-----------------------------------                                            
         USING MPYELD,R4           MANUAL PAYMENT ELEMENT                       
FILT380  ICM   R4,15,AMPYELD                                                    
         BZ    FILT390                                                          
         GOTO1 CHEQFILT,DMCB,MPYNO,MPYDTE                                       
         BNE   FILTREJX                                                         
         OC    OFIXD,OFIXD         FIXED REF FILTER?                            
         BZ    FILT390                                                          
         CLC   SBNKUL,TRNKCUNT     IF BANK LEDGER FILTER ALREADY DONE           
         BE    FILT390                                                          
         GOTO1 ASCOMP,DMCB,MPYNO,(OFIXDLN1,OFIXDVL1),(OFIXDLN2,        X        
               OFIXDVL2),OFIXDFI                                                
         BNE   FILTREJX                                                         
         DROP  R4                                                               
*-----------------------------------                                            
* SOURCE ELEMENT X'7B'                                                          
*-----------------------------------                                            
         USING SORELD,R4                                                        
FILT390  ICM   R4,15,ASORELD                                                    
         BZ    FILT410                                                          
         LA    RF,WORK                                                          
         CLI   SORSYS,SORSMED                                                   
         BNE   FILT395                                                          
         MVC   WORK(L'SPROUL),SPROUL PRODUCTION UNIT/LEDGER                     
         LA    RF,L'SPROUL(RF)                                                  
         SR    RE,RE                                                            
         IC    RE,CLEN                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SORMCLI                                                  
         LA    RF,1(RE,RF)                                                      
         SR    RE,RE                                                            
         IC    RE,CPLEN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SORMPRO                                                  
         LA    RF,1(RE,RF)                                                      
         SR    RE,RE                                                            
         IC    RE,CPJLEN                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SORMCAM                                                  
         B     *+10                                                             
*                                                                               
FILT395  MVC   WORK(L'SORAULA),SORAULA                                          
*                                                                               
         OC    OCLIENT,OCLIENT     FILTERING ON CLIENT?                         
         BZ    FILT400                                                          
         GOTO1 ASCOMP,DMCB,WORK+L'SPROUL,(OCLINLN1,OCLINVL1),          X        
               (OCLINLN2,OCLINVL2),OCLINFI                                      
         BNE   FILTREJX                                                         
*                                                                               
FILT400  OC    OPROD,OPROD         FILTERING ON PRODUCT?                        
         BZ    FILT404                                                          
         GOTO1 ASCOMP,DMCB,WORK+L'SPROUL,(OPRODLN1,OPRODVL1),          X        
               (OPRODLN2,OPRODVL2),OPRODFI                                      
         BNE   FILTREJX                                                         
*                                                                               
FILT404  OC    OJOB,OJOB           FILTERING ON JOB?                            
         BZ    FILT408                                                          
         GOTO1 ASCOMP,DMCB,WORK+L'SPROUL,(OJOBLN1,OJOBVL1),            X        
               (OJOBLN2,OJOBVL2),OJOBFI                                         
         BNE   FILTREJX                                                         
*                                                                               
FILT408  OC    OSRC,OSRC           FILTERING ON SOURCE?                         
         BZ    FILT410                                                          
         GOTO1 ASCOMP,DMCB,WORK,(OSRCLN1,OSRCVL1),(OSRCLN2,OSRCVL2),   X        
               OSRCFI                                                           
         BNE   FILTREJX                                                         
*-----------------------------------                                            
* RECEIVABLE ALLOCATION ELEMENT X'D9'                                           
*-----------------------------------                                            
         USING RALELD,R4                                                        
FILT410  CLI   OCONT,0             CONTRA'D OPTION?                             
         BE    FILT420                                                          
         TM    SLEDGATR,LEDGCRD    CREDITOR LEDGER?                             
         BO    FILT420                                                          
         ICM   R4,15,ARALELD                                                    
         BZ    FILT416                                                          
FILT412  CLI   RALEL,RALELQ                                                     
         BNE   FILT416                                                          
         CLI   RALTYPE,RALTOFS     CONTRA'D?                                    
         BNE   FILT418                                                          
         CLI   OCONT,C'N'          NO CONTRA'D ITEMS REQUIRED?                  
         BE    FILTREJX                                                         
         B     FILT420                                                          
FILT416  CLI   OCONT,C'O'          ONLY CONTRA'D ITEMS REQUIRED?                
         BE    FILTREJX                                                         
         B     FILT420                                                          
*                                                                               
FILT418  SR    RF,RF                                                            
         IC    RF,RALLN                                                         
         AR    R4,RF                                                            
         B     FILT412                                                          
*-----------------------------------                                            
FILT420  OC    OPAYREF,OPAYREF                                                  
         BNZ   *+14                                                             
         OC    OPAYDATE,OPAYDATE                                                
         BZ    FILT440                                                          
         TM    SLEDGATR,LEDGDEB    DEBTORS LEDGER                               
         BNO   FILT440                                                          
         ICM   R4,15,ARALELD                                                    
         BZ    FILTREJX                                                         
FILT425  CLI   RALEL,RALELQ                                                     
         BNE   FILTREJX                                                         
*                                                                               
         CLI   RALTYPE,RALTALC                                                  
         BNE   FILT430                                                          
         GOTO1 CHEQFILT,DMCB,RALAREF,RALADAT                                    
         BNE   FILTREJX                                                         
         B     FILT440                                                          
*                                                                               
FILT430  SR    RF,RF                                                            
         IC    RF,RALLN                                                         
         AR    R4,RF                                                            
         B     FILT425                                                          
         DROP  R4                                                               
*-----------------------------------                                            
* FREE FORM TEXT X'DB'                                                          
*-----------------------------------                                            
         USING FFTELD,R4                                                        
FILT440  ICM   R4,15,AFFTELD                                                    
         BZ    FILT640                                                          
FILT442  CLI   FFTEL,0                                                          
         BE    FILT640                                                          
         CLI   FFTEL,FFTELQ        FREE FORM TEXT ELEMENT                       
         BNE   FILT444                                                          
         CLI   FFTTYPE,FFTTACUR    ASSOCIATED CURRENCY?                         
         BE    FILT450                                                          
*        CLI   FFTTYPE,FFTTCLPR                                                 
*        BE    FILT460                                                          
FILT444  SR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         AR    R4,RF                                                            
         B     FILT442                                                          
*        -----------------------------------                                    
*        FREE FORM - ASSOCIATED CURRENCY                                        
*        -----------------------------------                                    
FILT450  CLI   OVALUE,C'L'         LOCAL CURRENCY?                              
         BE    FILT444                                                          
         MVC   TRANCURR,FFTDATA    OVERRIDE TRANSACTION CURRENCY                
         MVC   ASSOCURR,FFTDATA    GET ASSOCIATED CURR IN TSAR                  
         B     FILT444                                                          
*        -----------------------------------                                    
*        FREE FORM - CLIENT AND PRODUCT                                         
*        -----------------------------------                                    
FILT460  OC    OCLIENT,OCLIENT     FILTERING ON CLIENT?                         
         BZ    FILT480                                                          
         GOTO1 ASCOMP,DMCB,(0,FFTCLAC),(OCLINLN1,OCLINVL1),            X        
               (OCLINLN2,OCLINVL2),OCLINFI                                      
         BNE   FILTREJX                                                         
*                                                                               
FILT480  OC    OPROD,OPROD         FILTERING ON PRODUCT?                        
         BZ    FILT444                                                          
         GOTO1 ASCOMP,DMCB,(0,FFTPRAC),(OPRODLN1,OPRODVL1),            X        
               (OPRODLN2,OPRODVL2),OPRODFI                                      
         BNE   FILTREJX                                                         
         B     FILT444                                                          
         DROP  R4                                                               
*-----------------------------------                                            
* ARTIST FEE PAYMENT ELEMENT X'DD'                                              
*-----------------------------------                                            
         USING AFPELD,R4                                                        
FILT640  OC    ORUNNO,ORUNNO       IF RUN NUMBER FILTER                         
         BZ    FILT675                                                          
         ICM   R4,15,AAFPELD                                                    
         BZ    FILT675                                                          
         CLC   ORUNVL,AFPPAYNO     MATCH ON RUN NUMBER?                         
         BE    FILT650                                                          
         CLI   ORUNFI,NEGFILTR                                                  
         BNE   FILTREJX                                                         
         B     *+12                                                             
FILT650  CLI   ORUNFI,NEGFILTR                                                  
         BE    FILTREJX                                                         
         DROP  R4                                                               
*-----------------------------------                                            
* GENERAL DATE ELEMENT X'E5'                                                    
*-----------------------------------                                            
         USING GDAELD,R4                                                        
FILT675  OC    ORECDTE,ORECDTE     HANDLE THE RECONCILED DATE                   
         BZ    FILT710                                                          
         CLI   ORECON,C'Y'         IF YES,                                      
         BE    FILT710                                                          
         CLI   ORECON,C'N'            NO,                                       
         BE    FILT710                                                          
         CLI   ORECON,C'O'            OR ONLY                                   
         BE    FILT710                                                          
*                                                                               
         ICM   R4,15,AGDAELD       NO RECONCILE OR CLEARED DATE?                
         BZ    FILTREJX                                                         
FILT690  CLI   GDATYPE,GDATRECN    BANKREC TYPE - TYPE 17?                      
         BE    FILT700              YES                                         
FILT695  SR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         CLI   GDAEL,0             END OF RECORD?                               
         BE    FILTREJX                                                         
         CLI   GDAEL,GDAELQ        STILL GENERAL DATE?                          
         BE    FILT690                                                          
         B     FILT695                                                          
*                                                                               
FILT700  GOTO1 ADCOMP,DMCB,(L'GDADATE,GDADATE),ORECST,ORECEN,ORECON             
         BNE   FILTREJX                                                         
*                                                                               
FILT710  OC    OCLRDTE,OCLRDTE     HANDLE THE CLEARED DATE                      
         BZ    FILT740                                                          
*                                                                               
         ICM   R4,15,AGDAELD       NO RECONCILE OR CLEARED DATE?                
         BZ    FILT725                                                          
FILT715  CLI   GDATYPE,GDATRECN    BANKREC TYPE - TYPE 17?                      
         BE    FILT730              YES                                         
FILT720  SR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         CLI   GDAEL,0             END OF RECORD?                               
         BE    FILT725                                                          
         CLI   GDAEL,GDAELQ        STILL GENERAL DATE?                          
         BE    FILT715                                                          
         B     FILT720                                                          
*                                                                               
FILT725  CLI   OCLRON,C'Y'                                                      
         BE    FILT740                                                          
         CLI   OCLRON,C'N'                                                      
         BE    FILT740                                                          
         B     FILTREJX             THEN REJECT                                 
*                                                                               
FILT730  OC    GDADATE2,GDADATE2                                                
         BZ    FILT725                                                          
         CLI   OCLRON,C'Y'                                                      
         BE    FILT740                                                          
         CLI   OCLRON,C'O'                                                      
         BE    FILT740                                                          
         CLI   OCLRON,C'N'         IF A DATE AND NO DATE WANTED                 
         BE    FILTREJX             THEN REJECT                                 
         GOTO1 ADCOMP,DMCB,(L'GDADATE2,GDADATE2),OCLRST,OCLREN,OCLRON           
         BNE   FILTREJX                                                         
         DROP  R4                                                               
*-----------------------------------                                            
* INVOICE NUMBER                                                                
*-----------------------------------                                            
FILT740  OC    OSERIAL,OSERIAL      LONG INVOICE # FILTER                       
         BZ    FILT840                                                          
         XC    WORK,WORK                                                        
         XC    TEMP,TEMP                                                        
*        -----------------------------------                                    
*        PRODUCTION LONG INVOICE NUMBER                                         
*        -----------------------------------                                    
         USING FFTELD,R4                                                        
         ICM   R4,15,AFFTELD        RE=A(FREE FORM TEXT ELEMENT)                
         BZ    FILT760                                                          
FILT744  CLI   FFTEL,0                                                          
         BE    FILT760                                                          
         CLI   FFTEL,FFTELQ                                                     
         BNE   *+12                                                             
         CLI   FFTTYPE,FFTTINVN    LONG INVOICE NUMBER                          
         BE    FILT750                                                          
         SR    R0,R0                                                            
         IC    R0,FFTLN                                                         
         AR    R4,R0                                                            
         B     FILT744                                                          
*                                                                               
FILT750  SR    RF,RF                DATA LENGTH                                 
         ICM   RF,1,FFTDLEN         DATA LENGTH                                 
         BNP   FILT760                                                          
         BCTR  RF,0                 DECREMENT FOR EX MVC                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TEMP(0),FFTDATA                                                  
*                                                                               
FILT753  LA    RF,WORK                                                          
         LA    R1,TEMP                                                          
*                                                                               
FILT755  CLI   0(R1),0              END OF DATA?                                
         BE    FILT770                                                          
         CLI   0(R1),C'-'           LOOK FOR DASHES                             
         BNE   *+12                                                             
         AHI   R1,1                 SKIP THIS CHARACTER                         
         B     FILT755                                                          
         MVC   0(1,RF),0(R1)        MOVE INTO WORK                              
         AHI   RF,1                                                             
         AHI   R1,1                                                             
         B     FILT755                                                          
*        -----------------------------------                                    
*        MEDIA INVOICE OR REFERENCE                                             
*        -----------------------------------                                    
         USING XPYELD,R4                                                        
FILT760  ICM   R4,15,AXPYELD        IS EXTRA PAYMENT ELEMENT PRESENT ?          
         BZ    *+14                 NO:GET TRAN-REF                             
         MVC   TEMP(L'XPYINV),XPYINV                                            
         B     FILT753              SKIP FOR '-' IN INVOICE NO.                 
         MVC   WORK(L'TRNKREF),TRNKREF                                          
*                                                                               
FILT770  GOTO1 ASCOMP,DMCB,WORK,(OSERLN1,OSERVL1),(OSERLN2,OSERVL2),   +        
               OSERFI                                                           
         BNE   FILTREJX                                                         
         DROP  R4                                                               
*----------------------------------                                             
* FILTERING TRANSACTION AMOUNT?                                                 
*-----------------------------------                                            
FILT840  OC    OAMOUNT,OAMOUNT                                                  
         BZ    FILT890                                                          
         TM    SECFFLAG,SECFRATE   AUTHORISED FOR RATE?                         
         BO    FILTX                                                            
         CP    OAMTVL,TRANAMNT     FILTER AMOUNT=TRANS AMOUNT?                  
         BE    FILT880                                                          
         CLI   OAMTSIGN,0          HAS A '+' OR '-' BEEN SPECIFIED?             
         BNE   FILT850                                                          
         CLI   OAMTRNG,0           HAS A '>' OR '<' BEEN SPECIFIED?             
         BNE   FILT850                                                          
         ZAP   TEMPNUM,OAMTVL      SEE IF AMOUNT IS -VE EQUIVALENT              
         AP    TEMPNUM,TRANAMNT                                                 
         BZ    FILT880             YES IT IS                                    
         B     FILT870                                                          
FILT850  CLI   OAMTRNG,C'>'        MORE THAN SIGN?                              
         BNE   FILT860                                                          
         CP    OAMTVL,TRANAMNT                                                  
         BL    FILT880                                                          
         B     FILT870                                                          
FILT860  CLI   OAMTRNG,C'<'        LESS THAN SIGN?                              
         BNE   FILT870                                                          
         CP    OAMTVL,TRANAMNT                                                  
         BH    FILT880                                                          
FILT870  CLI   OAMTFI,NEGFILTR     NEGATIVE FILTER SPECIFIED?                   
         BNE   FILTREJX                                                         
         B     FILT890                                                          
FILT880  CLI   OAMTFI,NEGFILTR     NEGATIVE FILTER SPECIFIED?                   
         BE    FILTREJX                                                         
*                                                                               
FILT890  CP    TRANAMNT,=P'0'                                                   
         BNL   FILT900                                                          
         CLI   ONEGATIV,C'N'       DO WE ONLY WANT +VE NUMBERS/ZERO?            
         BE    FILTREJX                                                         
         B     *+12                                                             
FILT900  CLI   ONEGATIV,C'O'       DO WE ONLY WANT -VE NUMBERS?                 
         BE    FILTREJX                                                         
*                                                                               
FILTX    J     OKXIT                                                            
FILTREJX J     ERXIT                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        FIND OUT IF TRANSACTION IS CHECK OR PAYMENT/TRANSFER         *         
* ON ENTRY 'PAYTYPE' MUST BE SET FROM TRSELD                          *         
*          R4=A(TRANSACTION ELEMENT)                                  *         
*          PARAMETER 1 HOLDS ADDRESS OF CHECK/PAYMENT REFERENCE       *         
*          PARAMETER 2 HOLDS ADDRESS OF CHECK/PAYMENT DATE            *         
* ON EXIT  CC UNEQUAL MEANS RECORD REJECTED                           *         
*          CC EQUL MEANS KEEP RECORD                                  *         
***********************************************************************         
         USING TRNRECD,R3                                                       
         USING TRNELD,R4                                                        
CHEQFILT NTR1                                                                   
         LA    R4,TRNRFST-TRNRECD(R3)                                           
         LM    RE,RF,0(R1)                                                      
         MVC   PARAMREF,0(RE)                                                   
         LA    R1,L'PARAMDAT-1                                                  
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         CLC   SBNKUL,BASKEY                                                    
         BE    CHQF10                                                           
         CLC   SR,BASKEY                                                        
         BE    CHQF10                                                           
         BCTR  R1,0                                                             
CHQF10   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PARAMDAT(0),0(RF)                                                
*                                                                               
         OC    OPAYREF,OPAYREF     FILTERING ON PAYMENT REFERENCE?              
         BZ    CHQF80                                                           
         GOTO1 ASCOMP,DMCB,PARAMREF,(OPAYRLN1,OPAYRVL1),(OPAYRLN2,OPAYRC        
               VL2),OPAYRFI                                                     
         BNE   CHQFREJX                                                         
*                                                                               
CHQF80   OC    OPAYDATE,OPAYDATE   FILTERING ON PAYMENT DATE?                   
         BZ    CHQFX                                                            
         ICM   RF,15,ATRSELD                                                    
         BZ    *+12                                                             
         TM    TRSSTAT-TRSELD(RF),TRSSOFFS IF CONTR'D FILTER USED DATE          
         BO    CHQFX                                                            
         CLC   SR,BASKEY                                                        
         BE    *+14                                                             
         CLC   SBNKUL,BASKEY                                                    
         BNE   *+14                                                             
         MVC   TEMPDAT,PARAMDAT                                                 
         B     CHQF85                                                           
         GOTO1 VDATCON,DMCB,(2,PARAMDAT),(1,TEMPDAT) PAYMENT DATE               
CHQF85   CLC   PARAMREF,SPACES                                                  
         BNH   CHQFREJX                                                         
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),OPAYDST,OPAYDEN,OPAYDFI          
         BNE   CHQFREJX                                                         
         B     CHQFX                                                            
*                                                                               
CHQFX    J     OKXIT                                                            
CHQFREJX J     ERXIT                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        BUILD TSAR RECORD (DISK ADDRESS)                             *         
*        FILL DUMMY SCREEN LINES                                      *         
* ON ENTRY R3=A(AIO AREA CONTAINING TRANSACTION RECORD)               *         
* ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL                              *         
***********************************************************************         
         USING TRNRECD,R3                                                       
BLDTSDET NTR1                                                                   
         L     R2,ATSARREC         R2=A(TSAR RECORD)                            
         USING TSARRECD,R2                                                      
         MVC   0(TSAROVER-TSARRECD,R2),SPACES                                   
         MVC   TSARKYNO,TSCURRNO                                                
         MVC   TSARLINE,LINSUSED                                                
         MVC   TSAROVER(L'DADDRESS),DADDRESS  GET DISK ADDRESS                  
*                                                                               
         LA    R4,TSAROVER                                                      
         LA    R4,L'DADDRESS(R4)                                                
         MVI   0(R4),EOR           MARK END OF TSAR RECORD                      
         LA    R4,1(R4)                                                         
         SR    R4,R2               R4=L'(TSAR RECORD)                           
         STCM  R4,3,TSARLEN                                                     
         L     RE,ATSARREC                                                      
*                                                                               
         GOTO1 ATSARADD            ADD RECORD                                   
         BE    *+10                                                             
         LTR   RB,RB               TSAR BLOCK IS FULL                           
         B     BLDTSX                                                           
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
         CR    RB,RB                                                            
BLDTSX   J     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        BUILD TSAR RECORD FOR SCREEN DATA ITEM,                      *         
*        FILL DUMMY SCREEN LINES                                      *         
* ON ENTRY R3=A(AIO AREA CONTAINING TRANSACTION RECORD)               *         
* ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL                              *         
***********************************************************************         
         USING TRNRECD,R3                                                       
BLDTSDAT NTR1                                                                   
         GOTO1 ASETELE,TRNRFST     SET ELEMENT ADDRESSES                        
*                                                                               
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LHI   R1,TSARRECL                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         NI    DETFLAG,X'FF'-DETSNARQ                                           
         L     R2,ATSARREC         R2=A(TSAR RECORD)                            
         USING TSARRECD,R2                                                      
*                                                                               
         MVC   TSARKYNO,TSCURRNO                                                
         CLC   TSCURRNO,TSLSTREC   RECORD IN TSAR?                              
         BNH   *+10                                                             
         MVC   TSARKYNO,WORK                                                    
         LA    R2,TSARDATA                                                      
         USING TSARDATD,R2                                                      
         XC    TSDACDAT,TSDACDAT   ACTIVITY DATE                                
         XC    TSDUSDAT,TSDUSDAT   USED DATE                                    
         XC    TSDOFDAT,TSDOFDAT   OFFSET DATE                                  
         XC    TSDSTDAT,TSDSTDAT   BANK STATEMENT DATE                          
         XC    TSDRCDAT,TSDRCDAT   RECONCILED DATE (MARKER,ACCR)                
         XC    TSDCLDAT,TSDCLDAT   BANK CLEARED DATE                            
         XC    TSDPEDAT,TSDPEDAT   PEELED DATE                                  
         XC    TSDBLBDT,TSDBLBDT   BILLABLE DATE                                
         XC    TSDAAPT,TSDAAPT     APPROVAL TYPE                                
         MVI   TSDTAMTY,0          AMOUNT TYPE                                  
         ZAP   TSDATAMT,=P'0'      INIT TRAN AMOUNT                             
         MVI   TSDFMT,TSITEM1      SCREEN DATA ITEM 1                           
         MVC   TSDCONT,SPACES                                                   
         XC    TSDTACUR,TSDTACUR   CURRENCY CODE                                
         MVC   TSDMFILE,DEFMFILE   SET DEFAULT 'N/A' FOR MEDIA FILE             
         LA    RF,L'TRNKULC                                                     
         LA    R1,TRNKULC                                                       
         CLI   0(R1),C' '                                                       
         BH    BLDT05                                                           
         LA    R1,1(R1)                                                         
         BCT   RF,*-12                                                          
         B     BLDT06                                                           
BLDT05   BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TSDCONT(0),0(R1)    CONTRA CODE                                  
         MVC   TSDKREF,TRNKREF     THIS SHOWS 123*00 IF OVER 255                
         LA    RF,TRNKREF                                                       
         CLI   TRNKREF+MBVOFFSQ,MBVINDIQ 127+ TRANSACTIONS?                     
         BNE   BLDT06                                                           
*-----------------------------------                                            
* FREE FORM TEXT ELEMENT-KEY REF #                                              
*-----------------------------------                                            
         ICM   RE,15,AFFTELD       RE=A(FREE FORM TEXT ELEMENT)                 
         BZ    BLDT06                                                           
         USING FFTELD,RE                                                        
         SR    R0,R0                                                            
BLDT05A  CLI   FFTEL,0                                                          
         BE    BLDT06                                                           
         CLI   FFTEL,FFTELQ                                                     
         BNE   BLDT06                                                           
         CLI   FFTTYPE,FFTTKREF    KEY REF NUMBER FOR BANK VOID?                
         BE    *+14                                                             
         IC    R0,FFTLN                                                         
         AR    RE,R0                                                            
         B     BLDT05A                                                          
         LA    RF,FFTDATA                                                       
         DROP  RE                                                               
*-----------------------------------                                            
* TRANSACTION DATA                                                              
*-----------------------------------                                            
BLDT06   MVC   TSDTREF,0(RF)       TRANSACTION REFERENCE NUMBER                 
         MVC   TSDTDAT,TRNKDATE    TRANSACTION DATE                             
         MVC   TSDTRST,TRNRSTAT    TRANSACTION RECORD STATUS                    
         MVC   TSDSBREF,TRNKSBR    TRANSACTION SUB-REFERENCE NUMBER             
         MVC   TSDTMOA,TRNRSMOS    MOA                                          
         MVC   TSDTACUR,ASSOCURR   CURRENCY CODE                                
         MVC   TSDDA,DADDRESS      GET DISK ADDRESS                             
         GOTO1 ABLDSRC                                                          
         MVC   TSDSRCAC,SRCWORK    GET SOURCE ACCOUNT FOR CREDITORS             
         LA    R4,TSDRFST          R4=A(FIRST ELEMENT FOR TSAR RECORD)          
         LA    R3,TRNRFST          R3=A(FIRST ELEMENT OF TRANS RECORD)          
         USING TRNELD,R3                                                        
         MVC   TSDBREF,TRNBTCH     BATCH REFERENCE                              
         MVC   TSDOFF,TRNOFFC      OFFICE                                       
         MVC   TSDTEST,TRNSTAT     TRANSACTION ELEMENT STATUS                   
         MVC   TSDTTYPE,TRNTYPE    INPUT TYPE                                   
         MVC   TRANCURR,COMPCURR   SET TRANSACTION CURRENCY FROM AGENCY         
         ZAP   TRANAMNT,TRNAMNT    AGENCY TRANSACTION AMOUNT                    
*                                                                               
BLDT10   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         CLI   0(R3),EOR           END OF RECORD?                               
         BE    BLDT360                                                          
         CLI   0(R3),MDTELQ        MEDIA TRANSFER ELEMENT?                      
         BE    BLDT20                                                           
         CLI   0(R3),NAMELQ        NAME ELEMENT?                                
         BE    BLDT27                                                           
         CLI   0(R3),ADRELQ        ADDRESS ELEMENT?                             
         BE    BLDT28                                                           
         CLI   0(R3),OTHELQ        OTHERS ELEMENT?                              
         BE    BLDT30                                                           
         CLI   0(R3),PRTELQ        PERSONNEL RATE  ELEMENT?                     
         BE    BLDT36                                                           
         CLI   0(R3),FFNELQ        FREE FORM NUMBER ELEMENT?                    
         BE    BLDT40                                                           
         CLI   0(R3),SPAELQ        SPECIAL POSTING A/C ELEMENT?                 
         BE    BLDT50                                                           
         CLI   0(R3),MXPELQ        MEDLINE EXTRA PAYMENT ELEMENT?               
         BE    BLDT60                                                           
         CLI   0(R3),CPJELQ        CLI/PRO/JOB ELEMENT?                         
         BE    BLDT70                                                           
         CLI   0(R3),SCIELQ        SUBSIDIARY CASH INFO ELEMEN?                 
         BE    BLDT90                                                           
         CLI   0(R3),PCIELQ        PROJECT CONTROL INFO ELEMEN?                 
         BE    BLDT210                                                          
         CLI   0(R3),TRSELQ        TRANSACTION STATUS ELEMENT?                  
         BE    BLDT220                                                          
         CLI   0(R3),DUEELQ        DUE DATE ELEMENT?                            
         BE    BLDT230                                                          
         CLI   0(R3),MPYELQ        MANUAL PAYMENT ELEMENT?                      
         BE    BLDT240                                                          
         CLI   0(R3),OAMELQ        ORDER AMOUNT ELEMENT?                        
         BE    BLDT250                                                          
         CLI   0(R3),AFCELQ        ACCOUNT FOREIGN CURRENCY ELEMENT?            
         BE    BLDT260                                                          
         CLI   0(R3),SORELQ        SOURCE ELEMENT?                              
         BE    BLDT280                                                          
         CLI   0(R3),APEELQ        ANALYSIS POINTER ELEMENT?                    
         BE    BLDT290                                                          
         CLI   0(R3),FFTELQ        FREE FORM TEXT ELEMENT?                      
         BE    BLDT300                                                          
         CLI   0(R3),AFPELQ        ARTISTE FEE PAYMENT ELEMENT?                 
         BE    BLDT310                                                          
         CLI   0(R3),RALELQ        RECEIVABLE ALLOCATION ELEMENT?               
         BE    BLDT320                                                          
         CLI   0(R3),PIDELQ        X'D8' PERSON ID ELEMENT?                     
         BE    BLDT325                                                          
         CLI   0(R3),GDAELQ        X'E5' GENERAL DATE ELEMENT?                  
         BE    BLDT330                                                          
         CLI   0(R3),XPYELQ        X'46' EXTRA PAYMENT ELEMENT                  
         BE    BLDT334                                                          
         CLI   0(R3),MBIELQ        X'F3' BILLING XFER  ELEMENT                  
         BE    BLDT336                                                          
         CLI   0(R3),CEDELQ        X'8A' BILLING XFER  ELEMENT                  
         BE    BLDT338                                                          
         CLI   0(R3),MDPELQ        X'6A' MEDIA TRANSFER ELEMENT                 
         BE    BLDT340                                                          
         B     BLDT10                                                           
*-----------------------------------                                            
* MEDIA TRANSFER ELEMENT                                                        
*-----------------------------------                                            
         USING MDTELD,R3           MEDIA TRANSFER ELEMENT                       
BLDT20   TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    BLDT25                                                           
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BO    BLDT24                                                           
         CLI   ODETAIL,C'Y'                                                     
         BNE   BLDT10                                                           
         CLI   OGROSS,C'Y'                                                      
         BNE   *+8                                                              
         OI    DETFLAG,DETSNARQ    NO NARRATIVE IF GROSS BILL OPT/VAL           
BLDT24   CLI   OTAX,C'Y'           TAX COLUMN OVERRIDE?                         
         BE    *+12                                                             
         CLI   OTAXALL,C'Y'        GST AND PST?                                 
         BNE   BLDT26                                                           
         DROP  RF                                                               
*                                                                               
BLDT25   ICM   RF,15,MDTVAT                                                     
         CVD   RF,DUB                                                           
*                                                                               
         CLC   TSCURRNO,TSLSTREC   RECORD IN TSAR?                              
         BNH   *+10                DON'T ADD AGAIN                              
         AP    HRSTOT,DUB                                                       
BLDT26   SR    RF,RF                                                            
         IC    RF,MDTLN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* NAME ELEMENT                                                                  
*-----------------------------------                                            
         USING NAMELD,R3           NAME ELEMENT                                 
BLDT27   TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    *+12                                                             
         TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BO    BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* ADDRESS ELEMENT                                                               
*-----------------------------------                                            
         USING ADRELD,R3           ADDRESS ELEMENT                              
BLDT28   TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    *+12                                                             
         TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BO    BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,ADRLN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* OTHERS ELEMENT                                                                
*-----------------------------------                                            
         USING OTHELD,R3           OTHERS ELEMENT                               
BLDT30   TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    BLDT32                                                           
*                                                                               
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         CLI   OXDETAIL,C'Y'       XTRA DETAIL REQUIRED?                        
         BNE   BLDT10                                                           
         DROP  RF                                                               
*                                                                               
BLDT32   SR    RF,RF                                                            
         IC    RF,OTHLN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* PERSONNEL RATE ELEMENT                                                        
*-----------------------------------                                            
         USING PRTELD,R3           PERSONNEL RATE ELEMENT                       
BLDT36   TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    BLDT38                                                           
*                                                                               
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   ODETAIL-OPTVALSD(RF),C'Y'                                        
         BNE   BLDT10                                                           
*                                                                               
BLDT38   SR    RF,RF                                                            
         IC    RF,PRTLN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* FREE FORM NUMBER ELEMENT                                                      
*-----------------------------------                                            
         USING FFNELD,R3           FREE FORM NUMBER ELEMENT                     
BLDT40   TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    *+12                                                             
         TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BO    BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,FFNLN                                                         
         CHI   RF,FFNLN1Q          ANYTHING ON IT?                              
         BE    BLDT10                                                           
         B     BLDT350             ADD ELEMENT TO TSAR RECORD                   
*-----------------------------------                                            
* SPECIAL POSTING A/C ELEMENT                                                   
*-----------------------------------                                            
         USING SPAELD,R3           SPECIAL POSTING A/C ELEMENT                  
BLDT50   TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    *+12                                                             
         TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BO    BLDT10                                                           
         CLI   SPATYPE,SPATFACC    FACTORING ACCOUNT?                           
         BNE   BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,SPALN                                                         
         B     BLDT350             ADD ELEMENT TO TSAR RECORD                   
*-----------------------------------                                            
* MEDLINE EXTRA PAYMENT ELEMENT                                                 
*-----------------------------------                                            
         USING MXPELD,R3           MEDLINE EXTRA PAYMENT ELEMENT                
BLDT60   SR    RF,RF                                                            
         ICM   RF,7,MXPSER                                                      
         BZ    *+10                                                             
         MVC   TSDSERNO,MXPSER                                                  
         B     BLDT10                                                           
*-----------------------------------                                            
* CLI/PRO/JOB ELEMENT                                                           
*-----------------------------------                                            
         USING CPJELD,R3           CLI/PRO/JOB ELEMENT                          
BLDT70   TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    *+12                                                             
         TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BO    BLDT10                                                           
         OC    ASORELD,ASORELD                                                  
         BNZ   BLDT10                                                           
         CLI   CPJTYPE,CPJTJOB     PRODUCTION JOB?                              
         BE    BLDT80                                                           
         CLI   CPJTYPE,CPJTCOM     T&R COMMERCIAL?                              
         BE    BLDT80                                                           
         CLI   CPJTYPE,CPJTEXP     EXPENSE ACCOUNT (SE)?                        
         BE    BLDT80                                                           
         CLI   CPJTYPE,CPJTOTH     OTHER SOURCE CODES?                          
         BNE   BLDT10                                                           
BLDT80   SR    RF,RF                                                            
         IC    RF,CPJLN                                                         
         B     BLDT350             ADD ELEMENT TO TSAR RECORD                   
*-----------------------------------                                            
* SUBSIDIARY CASH INFO ELEMENT                                                  
*-----------------------------------                                            
         USING SCIELD,R3           ANALYSIS POINTER ELEMENT                     
BLDT90   L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    *+12                                                             
         CLI   ONET,C'Y'                                                        
         BNE   BLDT110                                                          
*                                                                               
         CLI   SCITYPE,SCITGLEV    GROSS LESS VAT?                              
         BNE   BLDT110                                                          
         OI    DETFLAG,DETSNARQ    NO NARRATIVE IF NET OPT/VAL                  
         B     BLDT135                                                          
*                                                                               
BLDT110  TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    *+12                                                             
         CLI   OHOURS,C'Y'         HOURS COLUMN OVERRIDE?                       
         BNE   BLDT114                                                          
*                                                                               
         CLI   SCITYPE,SCITHOUR    HOURS?                                       
         BE    BLDT130                                                          
         CLI   SCITYPE,SCITSJHR    HOURS?                                       
         BE    BLDT130                                                          
         CLI   SCITYPE,SCITLOAT    GERMAN LEAVE OF ABSENCE?                     
         BE    BLDT130                                                          
         TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BZ    BLDT132                                                          
*                                                                               
BLDT114  TM    SECFFLAG,SECFRATE   AUTHORISED FOR RATE?                         
         BNO   BLDT118                                                          
         CLI   SCITYPE,SCITHOUR    HOURS?                                       
         BE    *+12                                                             
         CLI   SCITYPE,SCITSJHR    HOURS (UK SJ TIME)?                          
         BNE   BLDT118                                                          
         ZAP   TSDATAMT,SCIAMNT    TRANSACTION AMOUNT                           
*                                                                               
         CLC   TSCURRNO,TSLSTREC   RECORD IN TSAR?                              
         BNH   BLDT118                                                          
         AP    TRNTOT,SCIAMNT      TRANSACTIONS TOTAL                           
*                                                                               
         CP    TRNTOT,=P'0'                                                     
         BH    *+14                                                             
         SP    TRNTCR,SCIAMNT                                                   
         B     *+10                                                             
         AP    TRNTDR,SCIAMNT                                                   
*                                                                               
BLDT118  CLI   SCITYPE,SCITTAXP    TAX PAID?                                    
         BNE   BLDT122                                                          
         OC    AMDTELD,AMDTELD     GET GST FROM MDTEL IF PRESENT                
         BNZ   BLDT122                                                          
         OC    AMDPELD,AMDPELD     GET GST FROM MDPEL IF PRESENT                
         BNZ   BLDT122                                                          
         TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    BLDT130                                                          
         CLI   OTAX,C'Y'           TAX COLUMN OVERRIDE?                         
         BE    BLDT130                                                          
         CLI   OTAXALL,C'Y'        GST AND PST?                                 
         BE    BLDT130                                                          
         B     BLDT132                                                          
*                                                                               
BLDT122  CLI   SCITYPE,SCITTQST    CANADIAN PROVINCE TAX?                       
         BNE   BLDT126                                                          
         TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    BLDT130                                                          
         CLI   OPST,C'Y'           TAX COLUMN OVERRIDE?                         
         BE    BLDT130                                                          
         CLI   OTAXALL,C'Y'        GST AND PST?                                 
         BE    BLDT130                                                          
         B     BLDT132                                                          
*                                                                               
BLDT126  CLI   SCITYPE,SCITGLEV    GROSS LESS VAT (BASIS)?                      
         BNE   BLDT132                                                          
         TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    BLDT130                                                          
         CLI   OBASIS,C'Y'         BASIS COLUMN OVERRIDE?                       
         BNE   BLDT132                                                          
*                                                                               
BLDT130  CLC   TSCURRNO,TSLSTREC   RECORD IN TSAR?                              
         BNH   *+10                                                             
         AP    HRSTOT,SCIAMNT                                                   
         B     BLDT135                                                          
*                                                                               
BLDT132  TM    PCDRIVEN,PCGRIDQ    ALWAYS DETAIL IF GRIDS                       
         BO    *+12                                                             
         CLI   OXDETAIL,C'Y'       XTRA DETAIL REQUIRED?                        
         BNE   BLDT10                                                           
         DROP  RF                                                               
*                                                                               
BLDT135  SR    RF,RF                                                            
         IC    RF,SCILN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* PROJECT CONTROL INFO ELEMENT                                                  
*-----------------------------------                                            
         USING PCIELD,R3           PROJECT CONTROL INFO ELEMENT                 
BLDT210  L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    *+12                                                             
         CLI   ODETAIL,C'Y'                                                     
         BNE   BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,PCILN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* TRANSACTION STATUS ELEMENT                                                    
*-----------------------------------                                            
         USING TRSELD,R3                                                        
BLDT220  MVC   TSDACDAT,TRSDATE    ACTIVITY DATE                                
         MVC   TSDUSDAT,TRSUDAT    USED DATE                                    
         TM    TRSSTAT,TRSSOFFS    IS THIS AN OFFSET?                           
         BNO   *+10                                                             
         MVC   TSDOFDAT,TRSUDAT    MOVE IN OFFSET DATE                          
         MVC   TSDSTDAT,TRSBSTDT   BANK STATEMENT DATE                          
         MVC   TSDPEDAT,TRSPDAT    PEELED DATE                                  
         MVC   TSDRVDAT,TRSREVD    REVERSED DATE                                
         MVC   TSDSTAT1,TRSSTAT    STATUS BYTE 1                                
         MVC   TSDSTAT2,TRSSTAT2   STATUS BYTE 2                                
         TM    TRSMARK,TRSMUMQ     IS ACTION "UNDONE"?                          
         BO    BLDT10              YES, SKIP THIS                               
         CLI   TRSMARK,TRSMCSQ     LAST MARKER ACTION = APPROVE?                
         BNE   *+8                                                              
         MVI   TSDAAPT,TSDAAM      SAVE APPROVAL TYPE                           
         B     BLDT10                                                           
*-----------------------------------                                            
* DUE DATE ELEMENT                                                              
*-----------------------------------                                            
         USING DUEELD,R3           DUE DATE ELEMENT                             
BLDT230  TM    PCDRIVEN,PCGRIDQ                                                 
         BO    BLDT235                                                          
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         CLI   ODETAIL,C'Y'        SHOWING TRANSASCTIN DETAIL?                  
         BNE   BLDT10                                                           
         DROP  RF                                                               
         TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BO    BLDT10                                                           
BLDT235  SR    RF,RF                                                            
         IC    RF,DUELN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* MANUAL PAYMENT ELEMENT                                                        
*-----------------------------------                                            
         USING MPYELD,R3           MANUAL PAYMENT ELEMENT                       
BLDT240  TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    *+12                                                             
         TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BO    BLDT10                                                           
*                                                                               
         L     RF,AIO1                                                          
         LA    RF,TRNRFST-TRNKEY(RF)                                            
         USING TRNELD,RF                                                        
*                                                                               
         MVI   TSDPYMET,TSDNOCHK  NO CHECK INFO POSTED YET                      
*                                                                               
         CLC   MPYNO,SPACES                                                     
         BNH   BLDT10                                                           
*                                                                               
         TM    TRNSTAT,TRNSDR     IS IT A DEBIT                                 
         BZ    BLDT245                                                          
*                                                                               
         MVI   TSDPYMET,TSDCHK     CHECK WAS PRINTED                            
         CLI   MPYLN,MPYLN4Q                                                    
         BL    BLDT245                                                          
*                                                                               
         TM    MPYBYTE,MPYEFT      WAS THIS CHECK SENT VIA EFT ?                
         BZ    *+8                                                              
         MVI   TSDPYMET,TSDEFT     SAVE PAYMENT METHOD STATUS BYTE              
*                                                                               
         TM    MPYBYTE,MPYPCRD     WAS THIS CHECK SENT VIA PCARD?               
         BZ    *+8                                                              
         MVI   TSDPYMET,TSDPCRD    SAVE PAYMENT METHOD STATUS BYTE              
*                                                                               
         TM    MPYBYTE,MPYCCRD     WAS THIS CHECK SENT VIA CCARD?               
         BZ    BLDT245                                                          
         MVI   TSDPYMET,TSDCCRD    SAVE PAYMENT METHOD STATUS BYTE              
*                                                                               
BLDT245  DS    0H                                                               
         L     RE,AOPTVALS         RE=A(OPTION VALUES)                          
         CLI   OCREDIT-OPTVALSD(RE),C'O'                                        
         BE    BLDT175                                                          
         CLI   TRNTYPE,129         CHECKS?                                      
         BE    BLDT170                                                          
         CLI   TRNTYPE,60          PAYMENTS?                                    
         BE    BLDT170                                                          
         CLI   TRNTYPE,57          TRANSFERS(GERMANY)?                          
         BE    BLDT170                                                          
         CLI   TRNTYPE,36          MANUAL PAYMENT?                              
         BNE   BLDT10                                                           
         CLI   POSTPROG,TRSSMRK1   POSTED VIA =MARKER?                          
         BNE   BLDT10                                                           
BLDT170  OI    DETFLAG,DETSNARQ    NO NARRATIVE IF MAN PAYMENT ELE              
BLDT175  SR    RF,RF                                                            
         IC    RF,MPYLN                                                         
         B     BLDT350                                                          
         DROP  RF                                                               
*-----------------------------------                                            
* ORDER AMOUNT ELEMENT                                                          
*-----------------------------------                                            
         USING OAMELD,R3           ORDER AMOUNT ELEMENT                         
BLDT250  AP    TRANAMNT,OAMAMNT    ADD ORDER AMOUNT                             
         SP    TRANAMNT,OAMIVAL    SUBTRACT INVOICE TODATE AMOUNT               
         OI    TSDTAMTY,TSDTORDQ   ORDER AMOUNT INCLUDED                        
         B     BLDT10                                                           
*-----------------------------------                                            
* ACCOUNT FOREIGN CURRENCY ELEMENT                                              
*-----------------------------------                                            
         USING AFCELD,R3           ACCOUNT FOREIGN CURRENCY ELEMENT             
BLDT260  L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         TM    AFCXSTAT,AFCXSMEM   MEMO ITEM?                                   
         BNO   BLDT262                                                          
         CLI   OMEMOAFC,C'Y'                                                    
         BE    BLDT262                                                          
         CLI   OMEMOAFC,C'O'                                                    
         BNE   BLDT266                                                          
BLDT262  CLI   OVALUE,C'L'         SHOWING LOCAL COLUMN ONLY?                   
         BNE   *+14                                                             
         ZAP   TRANAMNT,AFCAMNT    LOCAL CURRENCY AMOUNT                        
         B     BLDT264                                                          
         CLI   OVALUE,C'B'         SHOW BOTH LOCAL AND AGENCY?                  
         BNE   BLDT266                                                          
*                                                                               
         CLC   TSCURRNO,TSLSTREC   RECORD IN TSAR?                              
         BNH   BLDT264                                                          
         TM    TSDTEST,TRNSDR      DEBIT?                                       
         BNO   *+14                                                             
         AP    HRSTOT,AFCAMNT      LOCAL CURRENCY TOTAL                         
         B     BLDT264                                                          
         SP    HRSTOT,AFCAMNT                                                   
BLDT264  MVC   TRANCURR,AFCCURR    GET LOCAL CURRENCY                           
         B     BLDT268                                                          
BLDT266  CLI   OXDETAIL,C'Y'       SHOW XTRA DETAIL?                            
         BNE   BLDT10                                                           
         DROP  RF                                                               
BLDT268  SR    RF,RF                                                            
         IC    RF,AFCLN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* SOURCE ELEMENT                                                                
*-----------------------------------                                            
         USING SORELD,R3           SOURCE ELEMENT                               
BLDT280  TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    *+12                                                             
         TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BO    BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,SORLN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* ANALYSIS POINTER ELEMENT                                                      
*-----------------------------------                                            
         USING APEELD,R3           ANALYSIS POINTER ELEMENT                     
BLDT290  TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    BLDT292                                                          
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         CLI   OATTRIB,C'Y'        ATTRIBUTE OPTION?                            
         BNE   BLDT10                                                           
         DROP  RF                                                               
BLDT292  SR    RF,RF                                                            
         IC    RF,APELN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* FREE FORM TEXT ELEMENT                                                        
*-----------------------------------                                            
         USING FFTELD,R3                                                        
BLDT300  CLI   FFTTYPE,FFTTINVN    LONG PROD INVOICE NUMBER                     
         BE    BLDT305                                                          
         CLI   FFTTYPE,FFTTCLPR    CLIENT AND PRODUCT?                          
         BE    BLDT305                                                          
         CLI   FFTTYPE,FFTTMXTY    MEDIA AGENCY?                                
         BE    BLDT301                                                          
         CLI   FFTTYPE,FFTTACUR    ASSOCIATED CURRENCY?                         
         BNE   BLDT302                                                          
         L     RF,AOPTVALS         RF=A(OPTION BLOCK)                           
         CLI   OVALUE-OPTVALSD(RF),C'L' LOCAL CURRENCY?                         
         BE    BLDT302                                                          
         MVC   TRANCURR,FFTDATA    OVERRIDE TRANSACTION CURRENCY                
         CLI   OXDETAIL-OPTVALSD(RF),C'Y'                                       
         BE    BLDT305                                                          
         B     BLDT10                                                           
*                                                                               
BLDT301  CLI   FFTDLEN,FFTMLN2Q    DO WE HAVE THE RIGHT LENGTH?                 
         JL    BLDT302                                                          
         CLC   FFTMXTAG,SPACES     DO WE HAVE ORIGINAL MEDIA AGENCY ?           
         JNH   BLDT302                                                          
         MVC   TSDMFILE,SPACES     INITALIZE MEDIA FILE                         
         MVC   TSDMFILE(L'FFTMXTAG),FFTMXTAG   OVERRIDE TSR MEDIA FILE          
         J     BLDT10                                                           
*                                                                               
BLDT302  TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BO    BLDT10                                                           
         CLI   FFTTYPE,FFTTTAXI    TAX INDICATOR?                               
         BE    *+12                                                             
         CLI   FFTTYPE,FFTTPNAM    PAYEE NAME (CASH BATCHES?)                   
         BNE   BLDT10                                                           
BLDT305  SR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* ARTISTE FEE PAYMENT ELEMENT                                                   
*-----------------------------------                                            
         USING AFPELD,R3                                                        
BLDT310  TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    *+12                                                             
         TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BO    BLDT10                                                           
         SR    RF,RF                                                            
         IC    RF,AFPLN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* RECEIVABLE ALLOCATION ELEMENT                                                 
*-----------------------------------                                            
         USING RALELD,R3                                                        
BLDT320  OI    DETFLAG,DETSNARQ    SET FLAG TO IGORE TRANSACTION NARR           
         OC    TSDOFDAT,TSDOFDAT   DO WE HAVE AN OFFSET DATE?                   
         BNZ   BLDT321                                                          
         CLI   RALTYPE,RALTOFS                                                  
         BNE   BLDT321                                                          
         GOTO1 VDATCON,DMCB,(1,RALODAT),(2,WORK) OFFSET DATE                    
         MVC   TSDOFDAT,WORK                                                    
*                                                                               
BLDT321  SR    RF,RF                                                            
         IC    RF,RALLN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* PERSON ID ELEMENT X'D8'                                                       
*-----------------------------------                                            
         USING PIDELD,R3                                                        
BLDT325  LLC   RF,PIDLN                                                         
         CLC   PIDAGY,SPACES                                                    
         JNH   BLDT350                                                          
         MVC   TSDMFILE,SPACES     INITALIZE MEDIA FILE                         
         MVC   TSDMFILE(L'PIDAGY),PIDAGY   OVERRIDE TSR MEDIA FILE              
         J     BLDT350                                                          
*-----------------------------------                                            
* GENERAL DATE ELEMENT X'E5'                                                    
*-----------------------------------                                            
         USING GDAELD,R3                                                        
BLDT330  CLI   GDATYPE,GDATRECN    BANKREC TYPE                                 
         BNE   BLDT331                                                          
         MVC   TSDRCDAT,GDADATE    RECONCILED DATE (MARKER,ACCR)                
         MVC   TSDCLDAT,GDADATE2   CHECK CLEARED DATE (FROM BANK)               
         SR    RF,RF                                                            
         IC    RF,GDALN                                                         
         B     BLDT350                                                          
*                                                                               
BLDT331  CLI   GDATYPE,GDAMMOS     IS THIS MEDIA MOS?                           
         BNE   BLDT332                                                          
         SR    RF,RF                                                            
         IC    RF,GDALN                                                         
         B     BLDT350                                                          
*                                                                               
BLDT332  CLI   GDATYPE,GDAAPP      IS THIS APPROVAL TYPE?                       
         BNE   BLDT10                                                           
         TM    TSDTEST,TRNSAPPR                                                 
         BZ    BLDT10                                                           
         MVI   TSDAAPT,TSDAAA      SAVE APPROVAL TYPE                           
         CLI   GDATSUB,GDAAPPAA                                                 
         BE    BLDT333                                                          
*                                                                               
         MVI   TSDAAPT,TSDAAM                                                   
         CLI   GDATSUB,GDAAPPMK                                                 
         BNE   BLDT10                                                           
*                                                                               
BLDT333  SR    RF,RF                                                            
         IC    RF,GDALN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* EXTRA PAYMENT ELEMENT X'46'                                                   
*-----------------------------------                                            
         USING XPYELD,R3                                                        
BLDT334  SR    RF,RF                                                            
         IC    RF,XPYLN                                                         
         CLC   XPYAGY,SPACES                                                    
         JNH   BLDT335                                                          
         MVC   TSDMFILE,SPACES     INITALIZE MEDIA FILE                         
         MVC   TSDMFILE(L'XPYAGY),XPYAGY                                        
BLDT335  CLI   XPYLN,XPYLN2Q       CHECK FOR BILLABLE DATE                      
         BNH   *+10                                                             
         MVC   TSDBLBDT,XPYBLDTE                                                
         B     BLDT350                                                          
*-----------------------------------                                            
* BILLING XFER ELEMENT X'F3'                                                    
*-----------------------------------                                            
         USING MBIELD,R3                                                        
BLDT336  SR    RF,RF                                                            
         IC    RF,MBILN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* ** CHEQUE EXTRA DETAIL ELEMENT **                                             
*-----------------------------------                                            
         USING CEDELD,R3                                                        
BLDT338  DS    0H                                                               
         MVI   TSDPYMET,TSDCHK     CHECK WAS PRINTED                            
         CLI   CEDLN,CEDLN2Q                                                    
         BL    BLDT310                                                          
*                                                                               
         TM    CEDBYTE,CEDEFT      WAS THIS EFT?                                
         BZ    *+8                 NO                                           
         MVI   TSDPYMET,TSDEFT     CHECK SENT VIA EFT                           
*                                                                               
         TM    CEDBYTE,CEDPCRD     WAS THIS PCARD?                              
         BZ    *+8                 NO                                           
         MVI   TSDPYMET,TSDPCRD    CHECK SENT VIA PCARD                         
*                                                                               
         TM    CEDBYTE,CEDCCRD     WAS THIS CCARD?                              
         BZ    BLDT310             NO                                           
         MVI   TSDPYMET,TSDCCRD    CHECK SENT VIA CCARD                         
         B     BLDT310                                                          
*-----------------------------------                                            
* MEDIA TRANSFER ELEMENT                                                        
*-----------------------------------                                            
         USING MDPELD,R3           MEDIA TRANSFER ELEMENT                       
BLDT340  TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    BLDT344                                                          
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BO    BLDT342                                                          
         CLI   ODETAIL,C'Y'                                                     
         BNE   BLDT10                                                           
         CLI   OGROSS,C'Y'                                                      
         BNE   *+8                                                              
         OI    DETFLAG,DETSNARQ    NO NARRATIVE IF GROSS BILL OPT/VAL           
BLDT342  CLI   OTAX,C'Y'           TAX COLUMN OVERRIDE?                         
         BE    *+12                                                             
         CLI   OTAXALL,C'Y'        GST AND PST?                                 
         BNE   BLDT346                                                          
         DROP  RF                                                               
*                                                                               
BLDT344  ZAP   DUB,MDPVAT                                                       
         CLC   TSCURRNO,TSLSTREC   RECORD IN TSAR?                              
         BNH   *+10                DON'T ADD AGAIN                              
         AP    HRSTOT,DUB                                                       
*                                                                               
BLDT346  SR    RF,RF                                                            
         IC    RF,MDPLN                                                         
         B     BLDT350                                                          
*-----------------------------------                                            
* MOVE WHOLE ELEMENT ONTO TSAR RECORD                                           
*-----------------------------------                                            
BLDT350  BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(RF,R4)                                                      
         B     BLDT10                                                           
         DROP  R3                                                               
*-----------------------------------                                            
* FINISH PROCESSING                                                             
*-----------------------------------                                            
BLDT360  CLC   CURRLAST,SPACES     CURRENCY NOT SET?                            
         BE    BLDT368                                                          
         CLC   CURRLAST,TRANCURR   MIXED CURRENCIES DISPLAYED?                  
         BE    BLDT370                                                          
         OI    CURRFLAG,CURRSRTQ   SUPPRESS REQUEST TOTALS                      
BLDT368  MVC   CURRLAST,TRANCURR                                                
BLDT370  TM    SECFFLAG,SECFRATE   AUTHORISED FOR RATE?                         
         BO    BLDT380                                                          
         ZAP   TSDATAMT,TRANAMNT   LOCAL/AGENCY TRANSACTION AMOUNT              
         L     R3,AIO1                                                          
         LA    R3,TRNRFST-TRNRECD(R3)                                           
*                                                                               
         TM    TSDTAMTY,TSDTORDQ   IF ORDER DO NOT ADD TO TOTAL                 
         BO    BLDT380                                                          
*                                                                               
         CLC   TSCURRNO,TSLSTREC   RECORD IN TSAR?                              
         BNH   BLDT380                                                          
         TM    TRNSTAT-TRNELD(R3),TRNSDR                                        
         BZ    BLDT375                                                          
         AP    TRNTOT,TSDATAMT     TRANSACTIONS TOTAL                           
         AP    TRNTDR,TSDATAMT                                                  
         B     BLDT380                                                          
BLDT375  SP    TRNTOT,TSDATAMT     TRANSACTIONS TOTAL                           
         AP    TRNTCR,TSDATAMT     TRANSACTIONS TOTAL                           
BLDT380  TM    DETFLAG,DETSNARQ+DETCOLMQ   DISPLAY TRANSACTION NARR?            
         BNZ   BLDT400             NO                                           
         L     R3,AIO1                                                          
         LA    R3,TRNRFST-TRNKEY(R3) R3=A(TRANSACTION ELEMNT)                   
         USING TRNELD,R3                                                        
         USING NARELD,R4                                                        
         SR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         SHI   RF,TRNLN1Q+1                                                     
         BM    BLDT400             NO NARRATIVE ON TRANSACTION                  
         MVI   NAREL,ELETSARQ      INTERNAL ELEMENT                             
         MVI   NARELTP,NARELQ      NARRATIVE ELEMENT                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NARREC(0),TRNNARR   GET NARRATIVE                                
         AHI   RF,NARLN1Q+1                                                     
         STC   RF,NARLN            ELEMENT LENGTH                               
         CLI   TRNTYPE,37          VOID?                                        
         BNE   BLDT390                                                          
         CLI   POSTPROG,TRSSMRK2                                                
         BNE   BLDT395                                                          
         B     *+12                                                             
BLDT390  CLI   TRNTYPE,129         CHECKS?                                      
         BNE   BLDT395                                                          
         CLI   TRNLN,TRNNARR+26-TRNELD                                          
         BNH   BLDT395                                                          
         CLC   MYCO,NARREC+26                                                   
         BNE   BLDT395                                                          
         MVI   NARREC+26,C' '      DO NOT DISPLAY COMPANY BYTE                  
BLDT395  LA    R4,0(RF,R4)                                                      
*                                                                               
BLDT400  MVI   0(R4),EOR           MARK END OF TSAR RECORD                      
         LA    R4,1(R4)                                                         
         LHI   RF,TSDLENQ                                                       
         AHI   RF,TSARDATA-TSARRECD                                             
         LA    RE,TSDRFST                                                       
         SR    R4,RE                                                            
         AR    RF,R4               RF=L'(TSAR RECORD)                           
         L     RE,ATSARREC                                                      
         STCM  RF,3,TSARLEN-TSARRECD(RE)                                        
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BO    *+12                                                             
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
*                                                                               
         MVC   TSDLINES,LINSUSED                                                
*                                                                               
BLDTX    J     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
FORMTSAR NTR1                                                                   
         MVI   LINSUSED,0          NUMBER OF LINES USED                         
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN LINES                     
         LHI   R1,DUMLINLN                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         L     R4,ATSARREC         R4=A(TSAR RECORD )                           
         USING TSARRECD,R4                                                      
         CLI   TSARFMT,TSTOTITM    TOTAL LINE ITEM?                             
         BE    FORM120                                                          
*                                                                               
         USING SCRLIN1D,R2         DSECT FOR SCREEN DATA LINE                   
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         LA    R1,TSDRFST          R1=A(FIRST ELEMENT ON TSAR RECORD)           
         L     RF,ASETELE                                                       
         BASR  RE,RF                                                            
*                                                                               
         MVC   SCR1CONT,TSDCONT    CONTRA ACCOUNT                               
         GOTO1 VDATCON,DMCB,(1,TSDTDAT),(17,SCR1TDAT) TRAN DATE                 
         MVC   SCR1TREF,TSDTREF    TRANSACTION REFERENCE                        
         MVC   SCR1BREF,TSDBREF    BATCH REFERENCE                              
         MVC   SCR1OFF,TSDOFF      OFFICE                                       
*                                                                               
         TM    TSDTAMTY,TSDTORDQ   IF ORDER INCLUDED USE BRACKETS               
         BNO   FORM80                                                           
         PACK  TEMP(8),NINES(L'SCR1NAMT-4) GET STRING OF NINES                  
         CP    TEMP(8),TSDATAMT IS AMOUNT TOO HIGH TO FIT?                      
         BL    FORM70                                                           
         MP    TEMP(8),=P'-1'                                                   
         CP    TEMP(8),TSDATAMT IS AMOUNT TO LOW TO FIT?                        
         BH    FORM70                                                           
         CURED (P6,TSDATAMT),(L'SCR1NAMT,SCR1NAMT),2,MINUS=YES,        X        
               BRACKET=YES                                                      
         B     FORM100                                                          
FORM70   CURED (P6,TSDATAMT),(L'SCR1NAMT,SCR1NAMT),2,MINUS=YES,        X        
               DECS=ROUND,BRACKET=YES                                           
         B     FORM100                                                          
FORM80   L     RF,AOPTVALS         RF=A(OPTIONS BLOCK)                          
         CLI   OVALUE-OPTVALSD(RF),C'L' LOCAL CURRENCY DISPLAY?                 
         BNE   FORM90                                                           
         MVC   TRANCURR,COMPCURR                                                
         SR    R3,R3                                                            
         ICM   R3,15,AAFCELD       ACCOUNT FOREIGN CURRENCY ELEMENT?            
         BZ    FORM85                                                           
         TM    AFCXSTAT-AFCELD(R3),AFCXSMEM MEMO ITEM?                          
         BNO   FORM81                                                           
         CLI   OMEMOAFC-OPTVALSD(RF),C'Y'                                       
         BE    FORM81                                                           
         CLI   OMEMOAFC-OPTVALSD(RF),C'O'                                       
         BNE   FORM85                                                           
FORM81   MVC   TRANCURR,AFCCURR-AFCELD(R3)                                      
FORM85   GOTO1 AGETCURR,DMCB,TRANCURR                                           
         ICM   RF,15,0(R1)         RF=A(CURRENCY CODE ENTRY)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CURED (P6,TSDATAMT),(L'SCR1WAMT,SCR1WAMT),(RF),MINUS=YES,     X        
               CURSYMB=YES                                                      
         B     FORM100                                                          
FORM90   CURED (P6,TSDATAMT),(L'SCR1NAMT,SCR1NAMT),2,MINUS=YES                  
*                                                                               
FORM100  TM    SECFFLAG,SECFRATE   AUTHORISED FOR RATE?                         
         BO    FORM110                                                          
         TM    TSDTEST,TRNSDR      DEBIT OR CREDIT?                             
         BO    *+14                                                             
         MVC   SCR1SIGN,MX@CR                                                   
         B     *+10                                                             
         MVC   SCR1SIGN,MX@DR                                                   
*                                                                               
FORM110  BAS   RE,STANDLNS         STANDARD DESCRIPTION LINES                   
         BAS   RE,STATLNS          STATUS LINES                                 
         CLI   LINSUSED,0          NOTHING IN DESCRIPTION FIELD?                
         BNE   *+12                                                             
         LA    R2,L'DUMLIN1(R2)                                                 
         MVI   LINSUSED,1                                                       
         BAS   RE,ATTRLNS          ATTRIBUTE LINES                              
         LR    R1,RC                                                            
         L     RF,=A(XDETLNS)                                                   
         A     RF,ORELO                                                         
         BASR  RE,RF               EXTRA DETAIL LINES                           
         NI    DETFLAG,X'FF'-DETSNARQ                                           
         B     FORMX                                                            
*                                                                               
         USING SCRTOT1D,R2         DSECT FOR TOTAL LINE                         
FORM120  LA    R2,L'DUMLIN1(R2)    BLANK LINE                                   
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARTOTD,R4                                                      
         MVC   SCRTOTAL,MX@ACCT                                                 
         L     R3,AOPTVALS         R3=A(OPTION VALUES)                          
         USING OPTVALSD,R3                                                      
         TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BNO   FORM150                                                          
         MVC   SCRTOTAL,MX@ACCTS                                                
         CLC   CURRLAST,SPACES     ONLY ZERO TOTAL DISPLAY?                     
         BE    FORM140                                                          
         L     RF,AOPTVALS                                                      
         CLI   OVALUE-OPTVALSD(RF),C'B' BOTH LOCAL AND AGENCY CURR?             
         BNE   FORM140                                                          
         TM    CURRFLAG,CURRSRTQ   SUPPRESS REQUEST TOTALS                      
         BO    FORM160                                                          
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CURED (P8,TSTHTOT),(L'SCRHTOT,SCRHTOT),(RF),MINUS=YES,        X        
               CURSYMB=YES                                                      
         B     FORM150                                                          
FORM140  CURED (P8,TSTHTOT),(L'SCRHTOT,SCRHTOT),2,MINUS=YES                     
FORM150  CLC   CURRLAST,SPACES     ONLY ZERO TOTAL DISPLAY?                     
         BE    FORM160                                                          
         L     RF,AOPTVALS                                                      
         CLI   OVALUE-OPTVALSD(RF),C'L'                                         
         BNE   FORM160                                                          
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CURED (P8,TSTTTOT),(L'SCRTTOT,SCRTTOT),(RF),MINUS=YES,        X        
               CURSYMB=YES                                                      
         B     FORM170                                                          
FORM160  CURED (P8,TSTTTOT),(L'SCRTTOT,SCRTTOT),2,MINUS=YES                     
*                                                                               
FORM170  MVI   LINSUSED,2          NUMBER OF LINES USED                         
         MVI   DISATRIB,HILIGHTQ                                                
*                                                                               
FORMX    J     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES FOR GRIDS                 
***********************************************************************         
FGRMTSAR NTR1                                                                   
*                                                                               
         L     R4,ATSARREC              R4=A(TSAR RECORD)                       
         USING TSARRECD,R4                                                      
         CLI   TSARFMT,TSTOTITM         TOTAL LINE ITEM?                        
         BE    FGRM01                   YES, NO NEED TO SETUP ELEMENTS          
*                                                                               
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         GOTO1 ASETELE,TSDRFST                                                  
         DROP  R4                                                               
*                                                                               
FGRM01   MVI   LINSUSED,0               NUMBER OF LINES USED                    
         MVI   DISATRIB,0               DISPLAY ATTRIBUTES                      
         L     R0,ADUMLINE              CLEAR DUMMY SCREEN LINES                
         LHI   R1,DUMLINLN                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,ADUMLINE              R2=A(FIRST DUMMY SCREEN LINE)           
         L     R4,ATSARREC              R4=A(TSAR RECORD)                       
         USING TSARRECD,R4                                                      
*                                                                               
         TM    DETFLAG,DETGRINQ                                                 
         BO    FGRM10                                                           
         GOTO1 ADISGRD,DMCB,('DWNINIT',AGCTBL)                                  
         GOTO1 ADISPLAY,DISATRIB        DISPLAY DUMMY SCREEN LINES              
         OI    DETFLAG,DETGRINQ                                                 
         B     FGRM01                                                           
*                                                                               
FGRM10   CLI   TSARFMT,TSTOTITM         TOTAL LINE ITEM?                        
         BNE   FGRM20                                                           
         GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL)                                   
         B     FGRMX                                                            
*                                                                               
FGRM20   GOTO1 ADISGRD,DMCB,(0,AGCTBL)                                          
*                                                                               
FGRMX    J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT STANDARD LINES IN DESCRIPTION FIELD                   *         
* ON ENTRY     R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
* ON EXIT      R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
* ON EXIT      'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
***********************************************************************         
STANDLNS NTR1                                                                   
*                                                                               
         USING SCRLIN1D,R2         DSECT FOR SCREEN DATA LINE                   
         SR    R3,R3               R3=(LINE COUNTER)                            
         CLC   SBNKUL,BASKEY       BANK LEDGER?                                 
         BNE   STAND05                                                          
         L     R4,ATSARREC         R4=A(TSAR RECORD )                           
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
*                                                                               
         TM    TSDTEST-TSARDATD(R4),TRNSBREC  BANK RECONCILED?                  
         BNO   STAND02                                                          
         MVC   SCR1WDSC(L'MX@RCND),MX@RCND                                      
         OC    TSDRCDAT,TSDRCDAT                                                
         BZ    STAND01                                                          
         LA    RF,SCR1WDSC                                                      
         LA    RF,L'MX@RCND-1(RF)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         GOTO1 VDATCON,DMCB,(1,TSDRCDAT),(17,2(RF))                             
         LA    RF,L'MX@RCND                                                     
STAND01  LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
STAND02  OC    TSDSTDAT,TSDSTDAT                                                
         BZ    STAND03                                                          
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         OC    OSTATDT,OSTATDT     FILTERING ON STATEMENT DATE?                 
         BZ    STAND03                                                          
         DROP  RF                                                               
         LA    RF,SCR1WDSC                                                      
         LA    RE,DSUPG                                                         
         AHI   RE,UP@STMNL-DSUPG                                                
         MVC   0(L'UP@STMNL,RF),0(RE)                                           
         LA    RF,L'UP@STMNL-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         GOTO1 VDATCON,DMCB,(2,TSDSTDAT),(17,2(RF))                             
         LA    RF,L'UP@STMNL                                                    
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
STAND03  OC    TSDCLDAT,TSDCLDAT                                                
         BZ    STAND05                                                          
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         OC    OCLRDTE,OCLRDTE     FILTERING ON STATEMENT DATE?                 
         BZ    STAND05                                                          
         DROP  RF                                                               
         LA    RF,SCR1WDSC                                                      
         LA    RE,DSUPG                                                         
         AHI   RE,UP@CLRDL-DSUPG                                                
         MVC   0(L'UP@CLRDL,RF),0(RE)                                           
         LA    RF,L'UP@CLRDL-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         GOTO1 VDATCON,DMCB,(1,TSDCLDAT),(17,2(RF))                             
         LA    RF,L'UP@CLRDL                                                    
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*-----------------------------------                                            
* RECEIVABLE ALLOCATION ELEMENT                                                 
*-----------------------------------                                            
STAND05  ICM   R4,15,ARALELD                                                    
         BZ    *+12                                                             
         BAS   RE,RALDIS           DISPLAY RECEIVABLE ALLOC ELEMENT             
         B     STAND08                                                          
         L     R4,ATSARREC         R4=A(TSAR RECORD )                           
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         TM    TSDSTAT1,TRSSOFFS   OFFSET/CONTRA'D?                             
         BNO   STAND08                                                          
         OC    TSDUSDAT,TSDUSDAT                                                
         BZ    STAND08                                                          
         LA    RF,SCR1WDSC                                                      
         MVC   0(L'MX@CTRON,RF),MX@CTRON                                        
         LA    RF,L'MX@CTRON-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         GOTO1 VDATCON,DMCB,(2,TSDUSDAT),(17,2(RF))                             
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
STAND08  TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BNO   STAND30                                                          
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         CLC   SRECUL,BASKEY       RECEIVABLE LEDGER?                           
         BNE   STAND09D                                                         
*                                                                               
         USING MDTELD,R4                                                        
         ICM   R4,15,AMDTELD                                                    
         BZ    STAND09                                                          
         LA    RF,MDTVAT                                                        
         CLI   OTAX,C'Y'           TAX COLUMN OVERRIDE?                         
         BNE   STAND09A                                                         
         CURED MDTVAT,(L'SCR1AMTO,SCR1AMTO),2,MINUS=YES                         
         B     STAND09B                                                         
*                                                                               
         USING MDPELD,R4                                                        
STAND09  ICM   R4,15,AMDPELD                                                    
         BZ    STAND09D                                                         
         LA    RF,MDPVAT                                                        
         CLI   OTAX,C'Y'           TAX COLUMN OVERRIDE?                         
         BNE   STAND09A                                                         
         CURED MDPVAT,(L'SCR1AMTO,SCR1AMTO),2,MINUS=YES                         
         B     STAND09B                                                         
*                                                                               
STAND09A CLI   OTAXALL,C'Y'        SHOWING BOTH GST AND PST?                    
         BNE   STAND09D                                                         
         MVC   TEMP(L'AC@VAT),AC@VAT                                            
         MVI   TEMP+L'AC@VAT,C'/'                                               
         CURED (RF),(L'SCR1AMTO-4,TEMP+4),2,MINUS=YES,ALIGN=LEFT                
         LR    RF,R0                                                            
         LA    RF,L'AC@VAT+1(RF)                                                
         LA    RE,L'SCR1AMTO                                                    
         SR    RE,RF                                                            
         BNP   STAND09B                                                         
         BCTR  RF,0                                                             
         LA    RE,SCR1AMTO(RE)                                                  
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),TEMP                                                     
*                                                                               
STAND09B LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
         USING SCIELD,R4                                                        
STAND09D ICM   R4,15,ASCIELD                                                    
         BZ    STAND25                                                          
STAND10  L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         CLI   OHOURS,C'Y'         HOURS OPTION?                                
         BNE   STAND12                                                          
         CLI   SCITYPE,SCITHOUR    HOURS?                                       
         BE    STAND19                                                          
         CLI   SCITYPE,SCITSJHR    HOURS (UK SJ TIME)?                          
         BE    STAND19                                                          
         CLI   SCITYPE,SCITLOAT    GERMAN LEAVE OF ABSENCE?                     
         BE    STAND19                                                          
         B     STAND20                                                          
STAND12  CLI   OTAX,C'Y'           TAX COLUMN OVERRIDE?                         
         BE    *+12                                                             
         CLI   OTAXALL,C'Y'        SHOWING BOTH GST AND PST?                    
         BNE   STAND12A                                                         
         OC    AMDTELD,AMDTELD                                                  
         BNZ   STAND12A                                                         
         OC    AMDPELD,AMDPELD                                                  
         BNZ   STAND12A                                                         
         CLI   SCITYPE,SCITTAXP    TAX COLUMN OVERRIDE?                         
         BE    STAND19                                                          
*                                                                               
STAND12A CLI   OPST,C'Y'           CANADIAN PROVINCE COL OVERRIDE?              
         BE    *+12                                                             
         CLI   OTAXALL,C'Y'        SHOWING BOTH GST AND PST?                    
         BNE   STAND13                                                          
         CLI   SCITYPE,SCITTQST                                                 
         BNE   STAND20                                                          
         MVC   HALF,SCISUBPR                                                    
         B     STAND19                                                          
*                                                                               
STAND13  CLI   OBASIS,C'Y'         BASIS COLUMN OVERRIDE?                       
         BNE   STAND20                                                          
         CLI   SCITYPE,SCITGLEV    GROSS LESS VAT (BASIS)?                      
         BNE   STAND20                                                          
*                                                                               
STAND19  MVC   SCR1AMTO(L'SCR1AMTO),SPACES                                      
         CLI   OTAXALL,C'Y'                                                     
         BE    *+12                                                             
         CLI   OPST,C'Y'           CANADIAN PROVINCE COL OVERRIDE?              
         BNE   STAND19D                                                         
         CLI   SCITYPE,SCITTAXP    GST AMOUNT?                                  
         BNE   *+14                                                             
         MVC   TEMP(L'AC@VAT),AC@VAT                                            
         B     STAND19B                                                         
         LA    R1,PRVTAB                                                        
*                                                                               
STAND19A CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    STAND19D                                                         
         CLC   HALF,0(R1)                                                       
         BE    *+12                                                             
         LA    R1,L'PRVTAB(R1)                                                  
         B     STAND19A                                                         
         MVC   WORK(2),LARF        PASS DESCRIPTION BACK                        
         MVC   WORK+2(2),2(R1)                                                  
         EX    0,WORK                                                           
         MVC   TEMP(L'AC@HON),0(RF)                                             
STAND19B MVI   TEMP+L'AC@HON,C'/'                                               
         CURED (P6,SCIAMNT),(L'SCR1AMTO-7,TEMP+7),2,MINUS=YES,         X        
               ALIGN=LEFT                                                       
         LR    RF,R0                                                            
         LA    RF,L'AC@HON+1(RF)                                                
         LA    RE,L'SCR1AMTO                                                    
         SR    RE,RF                                                            
         BNP   STAND19E                                                         
         BCTR  RF,0                                                             
         LA    RE,SCR1AMTO(RE)                                                  
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),TEMP                                                     
         B     STAND19E                                                         
*                                                                               
STAND19D CURED (P6,SCIAMNT),(L'SCR1AMTO,SCR1AMTO),2,MINUS=YES                   
STAND19E LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
STAND20  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,SCILN                                                         
         AR    R4,R0                                                            
*                                                                               
         CLI   SCIEL,SCIELQ        SUBSIDIARY CASH ELEMENT?                     
         BE    STAND10                                                          
         B     STAND290                                                         
*-----------------------------------                                            
* ACCOUNT FOREIGN CURRENCY ELEMENT                                              
*-----------------------------------                                            
         USING AFCELD,R4                                                        
STAND25  ICM   R4,15,AAFCELD       ACCOUNT FOREIGN CURRENCY ELEMENT?            
         BZ    STAND290                                                         
*                                                                               
STAND30  SR    R4,R4                                                            
         ICM   R4,15,ANARELD       ANY NARRATIVE TO DISPLAY?                    
         BZ    STAND40                                                          
         USING NARELD,R4                                                        
         SR    RF,RF                                                            
         IC    RF,NARLN                                                         
         SHI   RF,NARLN1Q                                                       
         LA    R0,L'SCR1WDSC                                                    
         L     R1,AOPTVALS         R1=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(R1),C'L'                                         
         BNE   *+8                                                              
         LA    R0,L'SCR1NDSC                                                    
         GOTO1 VCHOPPER,DMCB,((RF),NARREC),((R0),SCANBLK),11                    
         L     RE,DMCB+8                                                        
         LTR   RE,RE               RE=(NUMBER OF NARRATIVE LINES)               
         BZ    STAND36                                                          
         AR    R3,RE               NUMBER OF LINES USED SO FAR                  
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         LA    R4,SCANBLK                                                       
         CLI   OVALUE-OPTVALSD(RF),C'L' LOCAL CURRENCY DISPLAY?                 
         BE    STAND35                                                          
         MVC   SCR1WDSC,0(R4)      USE WIDE NARRATIVE COLUMN                    
         LA    R4,L'SCR1WDSC(R4)                                                
         LA    R2,L'DUMLIN1(R2)                                                 
         BCT   RE,*-14                                                          
         B     STAND36                                                          
STAND35  MVC   SCR1NDSC,0(R4)      USE NARROW NARRATIVE COLUMN                  
         LA    R4,L'SCR1NDSC(R4)                                                
         LA    R2,L'DUMLIN1(R2)                                                 
         BCT   RE,*-14                                                          
STAND36  L     RF,AOPTVALS                                                      
         CLI   OCREDIT-OPTVALSD(RF),C'O'                                        
         BNE   STAND160                                                         
*                                                                               
STAND40  ICM   R4,15,AMPYELD       GET MANUAL PAYMENT ELEMENT                   
         BZ    STAND70                                                          
         USING MPYELD,R4                                                        
         L     RF,ATSARREC         RF=A(TSAR RECORD )                           
         LA    RF,TSARDATA-TSARRECD(RF)                                         
         CLI   TSDTTYPE-TSARDATD(RF),129 CHECKS?                                
         BE    STAND50                                                          
         CLI   TSDTTYPE-TSARDATD(RF),36 MANUAL CHECK POSTED VIA =MARK?          
         BE    STAND50                                                          
         MVC   SCR1WDSC(L'MX@PYMNT),MX@PYMNT PAYMENT/TRANSFER(GERMANY)          
         LA    RF,SCR1WDSC+L'MX@PYMNT-1                                         
         B     *+14                                                             
STAND50  MVC   SCR1WDSC(L'MX@CHK),MX@CHK CHECK                                  
         LA    RF,SCR1WDSC+L'MX@CHK-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         MVC   2(L'MPYNO,RF),MPYNO                                              
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
         MVC   SCR1WDSC(L'MX@DATE),MX@DATE DISPLAY CHECK DATE                   
         LA    RF,SCR1WDSC+L'MX@DATE-1                                          
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         GOTO1 VDATCON,DMCB,(2,MPYDTE),(8,2(RF))                                
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
         MVC   SCR1WDSC(L'MX@BNK),MX@BNK DISPLAY BANK DETAILS                   
         LA    RF,SCR1WDSC+L'MX@BNK-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         MVC   2(L'MPYBNK,RF),MPYBNK                                            
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
         L     RF,ATSARREC         RF=A(TSAR RECORD)                            
         LA    RF,TSARDATA-TSARRECD(RF)                                         
         CLI   TSDTTYPE-TSARDATD(RF),57 GERMAN TRANSFERS?                       
         BE    *+12                                                             
         CLI   TSDTTYPE-TSARDATD(RF),60 PAYMENTS?                               
         BNE   STAND60                                                          
         MVC   SCR1WDSC(L'MX@PAYTO),MX@PAYTO                                    
         LA    RF,SCR1WDSC+L'MX@PAYTO-1                                         
         B     *+14                                                             
STAND60  MVC   SCR1WDSC(L'MX@CHKTO),MX@CHKTO                                    
         LA    RF,SCR1WDSC+L'MX@CHKTO-1                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         CURED MPYAMNT,(11,2(RF)),2,ALIGN=LEFT                                  
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
         B     STAND160                                                         
*                                                                               
STAND70  ICM   R4,15,ASCIELD       DISPLAY GROSS/NET IF REQUIRED                
         BZ    STAND160                                                         
         USING SCIELD,R4                                                        
STAND80  L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         CLI   ONET,C'Y'           NET OPTION?                                  
         BNE   STAND160                                                         
         DROP  RF                                                               
         CLI   SCITYPE,SCITGLEV    GROSS LESS VAT (NET)                         
         BNE   STAND110                                                         
         MVC   SCR1WDSC(L'MX@NET),MX@NET                                        
         LA    RF,SCR1WDSC+L'MX@NET-1                                           
         B     STAND100                                                         
STAND100 CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         CURED SCIAMNT,(12,2(RF)),2,ALIGN=LEFT,MINUS=YES                        
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
STAND110 SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,SCILN                                                         
         AR    R4,R0                                                            
*                                                                               
         CLI   SCIEL,SCIELQ        SUBSIDIARY CASH ELEMENT?                     
         BE    STAND80                                                          
         B     STAND160                                                         
         USING SCIELD,R4                                                        
STAND130 CLI   SCITYPE,SCITCDSC    CASH DISCOUNT (OR DPRECIATION 41)?           
         BE    STAND140                                                         
         CLI   SCITYPE,SCITFTAX    FREELANCER?                                  
         BNE   STAND150                                                         
         MVC   SCR1WDSC(L'MX@FLTX),MX@FLTX                                      
         LA    RF,SCR1WDSC+L'MX@NET-1                                           
         B     STAND145                                                         
STAND140 CLC   UL41,BASKEY                                                      
         BE    STAND141                                                         
         MVC   SCR1WDSC(L'MX@DISS),MX@DISS                                      
         LA    RF,SCR1WDSC+L'MX@DISS-1                                          
         B     STAND145                                                         
STAND141 MVC   SCR1WDSC(L'MX@DPR),MX@DPR                                        
         LA    RF,SCR1WDSC+L'MX@DPR-1                                           
STAND145 CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         CURED SCIAMNT,(12,2(RF)),2,ALIGN=LEFT,MINUS=YES                        
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
STAND150 SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,SCILN                                                         
         AR    R4,R0                                                            
*                                                                               
         CLI   SCIEL,SCIELQ        SUBSIDIARY CASH ELEMENT?                     
         BE    STAND130                                                         
*                                                                               
STAND160 ICM   R4,15,AMDTELD       MEDIA TRANSFER ELEMENT?                      
         BZ    STAND162                                                         
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OGROSS-OPTVALSD(RF),C'Y' GROSS OPTION?                           
         BNE   STAND165                                                         
         USING MDTELD,R4                                                        
         MVC   SCR1WDSC(L'MX@GROSS),MX@GROSS                                    
         LA    RF,SCR1WDSC+L'MX@GROSS-1                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         CURED MDTGRS,(12,2(RF)),2,ALIGN=LEFT,MINUS=YES                         
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
         B     STAND165                                                         
*                                                                               
STAND162 ICM   R4,15,AMDPELD       MEDIA TRANSFER ELEMENT?                      
         BZ    STAND165                                                         
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OGROSS-OPTVALSD(RF),C'Y' GROSS OPTION?                           
         BNE   STAND165                                                         
         USING MDPELD,R4                                                        
         MVC   SCR1WDSC(L'MX@GROSS),MX@GROSS                                    
         LA    RF,SCR1WDSC+L'MX@GROSS-1                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         CURED MDPGRS,(12,2(RF)),2,ALIGN=LEFT,MINUS=YES                         
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
         B     STAND165                                                         
*                                                                               
STAND165 BAS   RE,PRDINF           PRODUCTION INFO                              
         BAS   RE,TIMEINF          TIME INFORMATION                             
         ICM   R4,15,AFFNELD       DISPLAY ORDER NUMBER?                        
         BZ    STAND170                                                         
         USING FFNELD,R4                                                        
         MVC   SCR1WDSC(L'MX@ORDER),MX@ORDER ORDER                              
         LA    RF,SCR1WDSC+L'MX@ORDER-1                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         SR    RE,RE                                                            
         IC    RE,FFNLN                                                         
         SHI   RE,FFNLN1Q+1                                                     
         CH    RE,=H'10'           MORE THAN 10 BYTES?                          
         BNH   *+8                 NO                                           
         LA    RE,10               YES, ONLY MOVE 10                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,RF),FFNUMBER                                                 
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
STAND170 CLC   SRECUL,BASKEY       RECEIVABLE LEDGER?                           
         BNE   *+14                                                             
         OC    ASORELD,ASORELD     SOURCE ELEMENT REPLACES SUBREF               
         BNZ   STAND180                                                         
         ICM   R4,15,AOTHELD       DISPLAY SUB REFERENCE?                       
         BZ    STAND188                                                         
         USING OTHELD,R4                                                        
         MVC   SCR1WDSC(L'MX@SUBR),MX@SUBR SUBREF                               
         LA    RF,SCR1WDSC+L'MX@SUBR-1                                          
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         MVC   2(L'OTHNUM,RF),OTHNUM                                            
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
STAND180 ICM   R4,15,ASORELD                                                    
         BZ    STAND188                                                         
         USING SORELD,R4                                                        
         MVC   WORK,SPACES                                                      
         LA    RF,WORK                                                          
         CLI   SORSYS,SORSMED                                                   
         BNE   STAND181                                                         
*        MVC   WORK(L'SORMMED),SORMMED                                          
*        LA    RF,L'SORMMED(RF)                                                 
         SR    RE,RE                                                            
         IC    RE,CLEN                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SORMCLI                                                  
         LA    RF,1(RE,RF)                                                      
         SR    R1,R1                                                            
         IC    R1,CLEN                                                          
         SR    RE,RE                                                            
         IC    RE,CPLEN                                                         
         SR    RE,R1                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SORMPRO                                                  
         LA    RF,1(RE,RF)                                                      
         SR    R1,R1                                                            
         IC    R1,CPLEN                                                         
         SR    RE,RE                                                            
         IC    RE,CPJLEN                                                        
         SR    RE,R1                                                            
         CHI   RE,L'SORMCAM                                                     
         BNH   *+8                                                              
         LA    RE,L'SORMCAM                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SORMCAM                                                  
         B     *+10                                                             
*                                                                               
STAND181 MVC   WORK(L'SORAACT),SORAACT                                          
*                                                                               
         LA    R1,WORK                                                          
         BAS   RE,CPJDET                                                        
*                                                                               
STAND188 ICM   R4,15,ACPJELD                                                    
         BZ    STAND245                                                         
         USING CPJELD,R4                                                        
         CLI   CPJTYPE,CPJTJOB     JOB?                                         
         BE    STAND190                                                         
         CLI   CPJTYPE,CPJTCOM     T&R COMMERCIAL?                              
         BNE   STAND220                                                         
         MVC   SCR1WDSC(L'MX@CMRCL),MX@CMRCL                                    
         LA    RF,SCR1WDSC+L'MX@CMRCL-1                                         
         B     *+14                                                             
STAND190 MVC   SCR1WDSC(L'MX@JOB),MX@JOB                                        
         LA    RF,SCR1WDSC+L'MX@JOB-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         MVC   2(L'CPJCLI,RF),CPJCLI                                            
         LA    RF,2+L'CPJCLI+1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         CLI   CPJTYPE,CPJTCOM                                                  
         BE    STAND200                                                         
         MVC   1(L'CPJPRO,RF),CPJPRO                                            
         LA    RF,1+L'CPJPRO-1(RF)                                              
         B     STAND210                                                         
*                                                                               
STAND200 MVC   1(L'CPJPRO-L'CPJCAT,RF),CPJPRO                                   
         LA    RF,1+L'CPJPRO-L'CPJCAT-1(RF)                                     
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   1(L'CPJCAT,RF),CPJCAT                                            
         LA    RF,1+L'CPJCAT-1(RF)                                              
*                                                                               
STAND210 CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   1(L'CPJJOB,RF),CPJJOB                                            
         LA    RF,1+L'CPJJOB-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C' '                                                       
         B     STAND240                                                         
*                                                                               
STAND220 CLI   CPJTYPE,CPJTEXP     EXPENSE ACCOUNT?                             
         BNE   STAND230                                                         
         MVC   SCR1WDSC(L'MX@EXP),MX@EXP                                        
         LA    RF,SCR1WDSC+L'MX@EXP-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         MVC   2(L'CPJEXP,RF),CPJEXP                                            
         LA    RF,2+L'CPJEXP-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C' '                                                       
         B     STAND240                                                         
*                                                                               
STAND230 CLI   CPJTYPE,CPJTOTH     OTHER SOURCE CODES?                          
         BNE   STAND250                                                         
         MVC   SCR1WDSC(L'MX@SRC),MX@SRC                                        
         LA    RF,SCR1WDSC+L'MX@SRC-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         MVC   2(L'CPJOULA,RF),CPJOULA                                          
         LA    RF,2+L'CPJOULA-1(RF)                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C' '                                                       
STAND240 LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
STAND245 L     R4,ATSARREC         R4=A(TSAR RECORD )                           
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         CLI   TSDSRCAC,C'*'                                                    
         BNE   STAND250                                                         
         MVC   SCR1WDSC(L'MX@JOB),MX@JOB                                        
         LA    RF,SCR1WDSC+L'MX@JOB-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         MVC   2(L'LC@SPLIT,RF),LC@SPLIT                                        
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*-----------------------------------                                            
* DUE DATE ELEMENT                                                              
*-----------------------------------                                            
STAND250 ICM   R4,15,ADUEELD                                                    
         BZ    STAND260                                                         
         USING DUEELD,R4                                                        
         MVC   SCR1WDSC(L'MX@DUEDT),MX@DUEDT                                    
         LA    RF,SCR1WDSC+L'MX@DUEDT-1                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         GOTO1 VDATCON,DMCB,(2,DUEDATE),(8,2(RF))                               
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*-----------------------------------                                            
* SERIAL NUMBER (TSDSERNO)                                                      
*-----------------------------------                                            
STAND260 L     RF,ATSARREC         RF=A(TSAR RECORD)                            
         LA    RF,TSARDATA-TSARRECD(RF)                                         
         USING TSARDATD,RF                                                      
         CLC   TSDSERNO,SPACES     SERIAL-NUMBER?                               
         BE    STAND270                                                         
         MVC   SCR1WDSC(L'MX@SERNO),MX@SERNO                                    
         LA    RE,SCR1WDSC+L'MX@SERNO-1                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C'='                                                       
         ICM   R1,7,TSDSERNO+1                                                  
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(7,RE),DUB                                                      
         MVC   2(1,RE),TSDSERNO  1ST CHAR IS ALPHA                              
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
         DROP  RF                                                               
*-----------------------------------                                            
* ARTIST FEE PAYMENT ELEMENT                                                    
*-----------------------------------                                            
STAND270 ICM   R4,15,AAFPELD       ARTIST FEE PAYMENT?                          
         BZ    STAND280                                                         
         USING AFPELD,R4                                                        
         MVC   SCR1WDSC(L'MX@RUNNO),MX@RUNNO                                    
         LA    RF,SCR1WDSC+L'MX@RUNNO-1                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         CURED AFPPAYNO,(4,2(RF)),0,ALIGN=LEFT                                  
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*-----------------------------------                                            
* FACTORING ACCOUNT                                                             
*-----------------------------------                                            
STAND280 ICM   R4,15,ASPAELD       DISPLAY FACTORING ACCOUNT?                   
         BZ    STAND281                                                         
         USING SPAELD,R4                                                        
         MVC   SCR1WDSC(L'MX@FACC),MX@FACC                                      
         LA    RF,SCR1WDSC+L'MX@FACC-1                                          
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         MVC   2(L'SPAAULA,RF),SPAAULA                                          
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
STAND281 CLC   SBNKUL,BASKEY       BANK UNIT/LEDGER?                            
         BE    STAND282                                                         
         ICM   R4,15,ANAMELD       NAME ELEMENT?                                
         BZ    STAND290                                                         
         B     STAND285                                                         
*-----------------------------------                                            
* FREE FORM TEXT - PAYEE NAME                                                   
*-----------------------------------                                            
STAND282 ICM   R4,15,AFFTELD       FREE FORM TEXT ELEMENT                       
         BZ    STAND290                                                         
         USING FFTELD,R4                                                        
STAND283 CLI   FFTEL,FFTELQ        FREE FORM TEXT ELEMENT?                      
         BNE   STAND290                                                         
         CLI   FFTTYPE,FFTTPNAM    PAYEE NAME?                                  
         BE    STAND285                                                         
         SR    R0,R0                                                            
         IC    R0,FFTLN                                                         
         AR    R4,R0                                                            
         B     STAND283                                                         
*                                                                               
STAND285 MVC   WORK(L'MX@PAYEE),MX@PAYEE                                        
         LA    RF,WORK+L'MX@PAYEE-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         SR    R1,R1                                                            
         CLI   0(R4),NAMELQ                                                     
         BE    STAND287                                                         
         USING FFTELD,R4                                                        
         IC    R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         LA    RE,FFTDATA                                                       
         B     STAND288                                                         
         USING NAMELD,R4                                                        
STAND287 IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1        R1=X L'(NAME)                                
         LA    RE,NAMEREC                                                       
STAND288 EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,RF),0(RE)       GET NAME                                     
         LA    RF,3(RF,R1)                                                      
         LA    RE,WORK                                                          
         SR    RF,RE                                                            
         LA    R0,L'SCR1WDSC                                                    
         L     R1,AOPTVALS         R1=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(R1),C'L'                                         
         BNE   *+8                                                              
         LA    R0,L'SCR1NDSC                                                    
         GOTO1 VCHOPPER,DMCB,((RF),WORK),((R0),SCANBLK),3                       
         L     RE,DMCB+8                                                        
         LTR   RE,RE               RE=(NUMBER OF NARRATIVE LINES)               
         BZ    STAND290                                                         
         AR    R3,RE               NUMBER OF LINES USED SO FAR                  
         LA    R4,SCANBLK                                                       
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(RF),C'L' LOCAL CURRENCY DISPLAY?                 
         BE    STAND289                                                         
         MVC   SCR1WDSC,0(R4)      USE WIDE NARRATIVE COLUMN                    
         LA    R4,L'SCR1WDSC(R4)                                                
         LA    R2,L'DUMLIN1(R2)                                                 
         BCT   RE,*-14                                                          
         B     STAND290                                                         
STAND289 MVC   SCR1NDSC,0(R4)      USE NARROW NARRATIVE COLUMN                  
         LA    R4,L'SCR1NDSC(R4)                                                
         LA    R2,L'DUMLIN1(R2)                                                 
         BCT   RE,*-14                                                          
*-----------------------------------                                            
* FREE FORM TEXT                                                                
*-----------------------------------                                            
STAND290 ICM   R4,15,AFFTELD       FREE FORM TEXT ELEMENT                       
         BZ    STAND390                                                         
         USING FFTELD,R4                                                        
STAND300 CLI   FFTEL,FFTELQ        FREE FORM TEXT ELEMENT?                      
         BNE   STAND390                                                         
*                                                                               
         L     RF,ATSARREC         RF=A(TSAR RECORD)                            
         LA    RF,TSARDATA-TSARRECD(RF)                                         
         CLI   TSDTTYPE-TSARDATD(RF),26                                         
         BE    STAND305                                                         
         CLI   TSDTTYPE-TSARDATD(RF),55                                         
         BE    STAND305                                                         
*                                                                               
         CLI   FFTTYPE,FFTTCLPR    CLIENT AND PRODUCT?                          
         BE    STAND310                                                         
*                                                                               
STAND305 SR    R0,R0                                                            
         IC    R0,FFTLN                                                         
         AR    R4,R0                                                            
         B     STAND300                                                         
*-----------------------------------                                            
* CLIENT AND PRODUCT                                                            
*-----------------------------------                                            
STAND310 MVC   WORK,SPACES                                                      
         LA    RF,WORK                                                          
         SR    RE,RE                                                            
         IC    RE,CLEN                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),FFTCLAC                                                  
         LA    RF,1(RE,RF)                                                      
         SR    R1,R1                                                            
         IC    R1,CLEN                                                          
         SR    RE,RE                                                            
         IC    RE,CPLEN                                                         
         SR    RE,R1                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),FFTPRAC                                                  
         LA    R1,WORK                                                          
         BAS   RE,CPJDET                                                        
*-----------------------------------                                            
STAND390 SR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         AR    RF,R3                                                            
         STC   RF,LINSUSED                                                      
*                                                                               
STANDX   J     XITR2               EXIT WITHOUT RESTORING R2                    
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT RALELD LINES IN DESCRIPTION FIELD                     *         
* ON ENTRY     R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              R4=A(RALELD)                                           *         
* ON EXIT      R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
* ON EXIT      'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
***********************************************************************         
RALDIS   NTR1                                                                   
         USING SCRLIN1D,R2         DSECT FOR SCREEN DATA LINE                   
         L     R5,AB1                                                           
         L     R6,AB2                                                           
         L     R7,AB3                                                           
         TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BO    RALDX                                                            
         SR    R3,R3               R3=(LINE COUNTER)                            
         USING RALELD,R4                                                        
         CLI   RALLN,L'RALEL+L'RALLN+L'RALTYPE                                  
         BNH   RALDX                                                            
         CLI   RALTYPE,RALTALC     REGULAR ALLOCATION?                          
         BE    RALD40                                                           
         CLI   RALTYPE,RALTOFS     OFFSET/CONTRA'D?                             
         BE    RALD30                                                           
         CLI   RALTYPE,RALTWOF     WRITE OFF?                                   
         BE    RALD20                                                           
         CLI   RALTYPE,RALTTTO     TRANFERRED TO?                               
         BE    RALD10                                                           
         CLI   RALTYPE,RALTTFR     TRANSFERRED FROM?                            
         BNE   RALDX                                                            
*                                                                               
         MVC   SCR1WDSC(L'MX@XFRFR),MX@XFRFR                                    
         B     *+10                                                             
RALD10   MVC   SCR1WDSC(L'MX@XFRTO),MX@XFRTO                                    
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
         MVC   SCR1WDSC(L'RALTULA),RALTULA XFER TO/FROM ACCOUNT                 
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
         LA    RF,SCR1WDSC                                                      
         MVC   0(L'MX@DATE,RF),MX@DATE XFER DATE                                
         LA    RF,L'MX@DATE-1(RF)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         GOTO1 VDATCON,DMCB,(1,RALTDAT),(17,2(RF)) TRANSFER DATE                
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
         B     RALDX                                                            
*                                                                               
RALD20   MVC   SCR1WDSC(L'MX@WRTFA),MX@WRTFA                                    
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
         MVC   SCR1WDSC(L'RALWULA),RALWULA WRITE OFF ACCOUNT                    
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
         LA    RF,SCR1WDSC                                                      
         MVC   0(L'MX@REF,RF),MX@REF                                            
         LA    RF,L'MX@REF-1(RF)                                                
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(L'RALWREF,RF),RALWREF WRITE OFF REF                            
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
         LA    RF,SCR1WDSC                                                      
         MVC   0(L'MX@DATE,RF),MX@DATE                                          
         LA    RF,L'MX@DATE-1(RF)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         GOTO1 VDATCON,DMCB,(1,RALWDAT),(17,2(RF)) WRITE OFF DATE               
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
         B     RALDX                                                            
*                                                                               
RALD30   LA    RF,SCR1WDSC                                                      
         MVC   0(L'MX@CTRON,RF),MX@CTRON                                        
         LA    RF,L'MX@CTRON-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         GOTO1 VDATCON,DMCB,(1,RALODAT),(17,2(RF)) OFFSET DATE                  
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
         B     RALDX                                                            
*                                                                               
RALD40   LA    RF,SCR1WDSC                                                      
         MVC   0(L'MX@CHKC,RF),MX@CHKC                                          
         LA    RF,L'MX@CHKC-1(RF)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(L'RALAREF,RF),RALAREF CHECK REFERENCE                          
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
         LA    RF,SCR1WDSC                                                      
         MVC   0(L'MX@DATED,RF),MX@DATED                                        
         LA    RF,L'MX@DATED-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         GOTO1 VDATCON,DMCB,(1,RALADAT),(17,2(RF)) CHECK DATED                  
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
         LA    RF,SCR1WDSC                                                      
         MVC   0(L'MX@DPSON,RF),MX@DPSON                                        
         LA    RF,L'MX@DPSON-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         GOTO1 VDATCON,DMCB,(1,RALADEP),(17,2(RF)) DEPOSITED ON                 
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
         B     RALDX                                                            
RALDX    SR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         AR    RF,R3                                                            
         STC   RF,LINSUSED                                                      
         J     XITR2               EXIT WITHOUT RESTORING R2                    
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT STATUS LINE(2) IN DESCRIPTION FIELD                   *         
* ON ENTRY     R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              R4=A(TSAR DATA)                                        *         
* ON EXIT      R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
***********************************************************************         
STATLNS  NTR1                                                                   
         USING SCRLIN1D,R2         DSECT FOR SCREEN DATA LINE                   
         USING TSARDATD,R4                                                      
         TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BO    STATX                                                            
         MVI   TEMP,C' '                                                        
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
         LA    R3,TEMP                                                          
         TM    TSDTRST,TRNSDRFT    DRAFT TRANSACTION?                           
         BNO   STAT10                                                           
         MVC   0(L'MX@DRAFT,R3),MX@DRAFT                                        
         LA    RF,L'MX@DRAFT-1(R3)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R3,2(RF)                                                         
STAT10   TM    COMPSTA4,CPYSIREG   INVOICE REGISTER IN USE?                     
         BNO   STAT30                                                           
         CLI   BASKEY,C'S'                                                      
         BNE   STAT30                                                           
         TM    TSDTEST,TRNSAUTH    INVOICE AUTHORISED?                          
         BNO   STAT20                                                           
         MVC   0(L'MX@ATHED,R3),MX@ATHED                                        
         LA    RF,L'MX@ATHED-1(R3)                                              
         B     *+14                                                             
STAT20   MVC   0(L'MX@UATH,R3),MX@UATH                                          
         LA    RF,L'MX@UATH-1(R3)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R3,2(RF)                                                         
STAT30   TM    TSDTEST,TRNSHOLD    TRANSACTION HELD?                            
         BNO   STAT40                                                           
         MVC   0(L'MX@HELD,R3),MX@HELD                                          
         LA    RF,L'MX@HELD-1(R3)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R3,2(RF)                                                         
STAT40   TM    TSDTEST,TRNSURG     URGENT TRANSACTION?                          
         BNO   STAT42                                                           
         MVC   0(L'MX@URG,R3),MX@URG                                            
         LA    RF,L'MX@URG-1(R3)                                                
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R3,2(RF)                                                         
STAT42   CLC   SBNKUL,BASKEY                                                    
         BE    STAT50                                                           
         TM    TSDTEST,TRNSAPPR    SELECTED FOR PAYMENT?                        
         BNO   STAT50                                                           
         MVC   0(L'MX@APRVD,R3),MX@APRVD                                        
         LA    RF,L'MX@APRVD-1(R3)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R3,2(RF)                                                         
STAT50   TM    TSDTRST,TRNSREVS   REVERSAL?                                     
         BNO   STAT60                                                           
         MVC   0(L'MX@RVRSD,R3),MX@RVRSD                                        
         LA    R3,L'MX@RVRSD-1(R3)                                              
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         LA    R3,1(R3)                                                         
         OC    TSDRVDAT,TSDRVDAT                                                
         BZ    STAT60                                                           
         MVI   0(R3),C'='                                                       
         GOTO1 VDATCON,DMCB,(2,TSDRVDAT),(17,1(R3)) REVERSED DATE               
         LA    R3,9(R3)                                                         
STAT60   LA    RF,TEMP                                                          
         SR    R3,RF                                                            
         BZ    STATX                                                            
         LA    R0,L'SCR1WDSC                                                    
         L     R1,AOPTVALS         R1=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(R1),C'L'                                         
         BNE   *+8                                                              
         LA    R0,L'SCR1NDSC                                                    
         GOTO1 VCHOPPER,DMCB,((R3),TEMP),((R0),SCANBLK),2                       
         L     RE,DMCB+8           RE=(NUMBER OF STATUS LINES)                  
         LTR   RE,RE                                                            
         BZ    STATX                                                            
         SR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         SR    R1,R1                                                            
         IC    R1,MAXLINES                                                      
         AR    RF,RE                                                            
         CR    RF,R1                                                            
         BH    STATX                                                            
         STC   RF,LINSUSED                                                      
         LA    R4,SCANBLK                                                       
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(RF),C'L'                                         
         BE    STAT70                                                           
         MVC   SCR1WDSC,0(R4)                                                   
         LA    R4,L'SCR1WDSC(R4)                                                
         LA    R2,L'DUMLIN1(R2)                                                 
         BCT   RE,*-14                                                          
         B     STATX                                                            
STAT70   MVC   SCR1NDSC,0(R4)                                                   
         LA    R4,L'SCR1NDSC(R4)                                                
         LA    R2,L'DUMLIN1(R2)                                                 
         BCT   RE,*-14                                                          
*                                                                               
STATX    J     XITR2               EXIT WITHOUT RESTORING R2                    
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT PROD INFORMATION USA ONLY                             *         
* ON ENTRY     R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
* ON EXIT      R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
***********************************************************************         
PRDINF   NTR1                                                                   
*                                                                               
         USING SCRLIN1D,R2         DSECT FOR SCREEN DATA LINE                   
         TM    DETFLAG,DETCOLMQ    COLUMN OVERRIDE OPTION?                      
         BO    PRDX                                                             
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   ODETAIL-OPTVALSD(RF),C'Y'                                        
         BNE   PRDX                                                             
         L     R4,ATSARREC         R4=A(TSAR RECORD)                            
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         CLC   TSDSRCAC,SPACES                                                  
         BNH   PRD10                                                            
         LA    R1,TSDSRCAC+L'SPROUL                                             
         BAS   RE,CPJDET                                                        
         B     PRDX                                                             
         DROP  R4                                                               
*                                                                               
PRD10    ICM   R4,15,APCIELD       PROJECT CONTROL INFORMATION                  
         BNZ   PRD60                                                            
         ICM   R4,15,AMDTELD       MEDIA TRANSFER ELEMENT?                      
         BZ    PRD20                                                            
         USING MDTELD,R4                                                        
         MVC   WORK(L'MDTCLI),MDTCLI                                            
         MVC   WORK+L'MDTCLI(L'MDTPRD),MDTPRD                                   
         MVC   WORK+L'MDTCLI+L'MDTPRD(L'MDTJOB),MDTJOB                          
         LA    R1,WORK                                                          
         BAS   RE,CPJDET                                                        
         B     PRD40                                                            
*                                                                               
PRD20    ICM   R4,15,AMDPELD       MEDIA TRANSFER ELEMENT?                      
         BZ    PRDX                                                             
         USING MDPELD,R4                                                        
         MVC   WORK(L'MDPCLI),MDPCLI                                            
         MVC   WORK+L'MDPCLI(L'MDPPRD),MDPPRD                                   
         MVC   WORK+L'MDPCLI+L'MDPPRD(L'MDPJOB),MDPJOB                          
         LA    R1,WORK                                                          
         BAS   RE,CPJDET                                                        
         B     PRD40                                                            
*                                                                               
         USING MDTELD,R4                                                        
PRD40    OC    MDTMOS,MDTMOS                                                    
         BZ    PRD50                                                            
         CLI   MDTMOS,C' '                                                      
         BNH   PRD50                                                            
         CLI   MDTMOS,C'0'                                                      
         BNL   PRD50                                                            
         MVC   SCR1WDSC(L'UP@MOS3),UP@MOS3 MONTH OF SERVICE                     
         LA    RF,SCR1WDSC+L'UP@MOS3-1                                          
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         MVC   DUB(L'MDTMOS),MDTMOS                                             
         MVI   DUB+L'MDTMOS,1                                                   
         GOTO1 VDATCON,DMCB,(1,DUB),(6,2(RF)) MONTH OF SERVICE                  
         CLI   MDTMOS+1,X'13'      SPECIAL 13TH MONTH?                          
         BNE   PRD44               NO                                           
         LA    RF,SCR1WDSC+L'UP@MOS3-1                                          
         MVC   2(3,RF),=C' 13'     YES, CHANGE MONTH                            
*                                                                               
PRD44    SR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         LA    RF,1(RF)                                                         
         STC   RF,LINSUSED                                                      
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
PRD50    LA    R0,L'SCR1WDSC                                                    
         L     R1,AOPTVALS         R1=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(R1),C'L'                                         
         BNE   *+8                                                              
         LA    R0,L'SCR1NDSC                                                    
         GOTO1 VCHOPPER,DMCB,(L'MDTDSCP,MDTDSCP),((R0),SCANBLK),3               
         L     RE,DMCB+8           RE=(NUMBER OF EST DESC LINES)                
         LTR   RE,RE                                                            
         BZ    PRDX                                                             
         SR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         SR    R1,R1                                                            
         IC    R1,MAXLINES                                                      
         AR    RF,RE                                                            
         CR    RF,R1                                                            
         BNH   *+6                                                              
         LR    RF,R1                                                            
         STC   RF,LINSUSED                                                      
         LA    R3,SCANBLK                                                       
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(RF),C'L'                                         
         BE    PRD55                                                            
         MVC   SCR1WDSC,0(R3)                                                   
         LA    R3,L'SCR1WDSC(R3)                                                
         LA    R2,L'DUMLIN1(R2)                                                 
         BCT   RE,*-14                                                          
         B     PRDX                                                             
PRD55    MVC   SCR1NDSC,0(R3)                                                   
         LA    R3,L'SCR1NDSC(R3)                                                
         LA    R2,L'DUMLIN1(R2)                                                 
         BCT   RE,*-14                                                          
         B     PRDX                                                             
*                                                                               
         USING PCIELD,R4           PROJECT CONTROL INFO ELEMENT                 
PRD60    CLI   PCILN,PCILN1Q                                                    
         BNH   PRDX                                                             
         LA    R1,PCIPRJT+3                                                     
         BAS   RE,CPJDET                                                        
*                                                                               
         CLC   PCITSK,SPACES                                                    
         BNH   PRDX                                                             
         MVI   TEMP,C' '                                                        
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
         LA    R3,TEMP                                                          
         MVC   0(L'UP@WCS,R3),UP@WCS                                            
         LA    R3,L'UP@WCS-1(R3)                                                
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C'='                                                       
         MVC   2(L'PCITSK,R3),PCITSK                                            
         LA    R3,L'PCITSK+2-1(R3)                                              
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         LA    R3,2(R3)                                                         
*                                                                               
         LA    RF,TEMP                                                          
         SR    R3,RF                                                            
         BZ    PRDX                                                             
         LA    R0,L'SCR1WDSC                                                    
         L     R1,AOPTVALS         R1=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(R1),C'L'                                         
         BNE   *+8                                                              
         LA    R0,L'SCR1NDSC                                                    
         GOTO1 VCHOPPER,DMCB,((R3),TEMP),((R0),SCANBLK),2                       
         L     RE,DMCB+8           RE=(NUMBER OF STATUS LINES)                  
         LTR   RE,RE                                                            
         BZ    PRDX                                                             
         SR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         SR    R1,R1                                                            
         IC    R1,MAXLINES                                                      
         AR    RF,RE                                                            
         CR    RF,R1                                                            
         BNH   *+6                                                              
         LR    RF,R1                                                            
         STC   RF,LINSUSED                                                      
         LA    R3,SCANBLK                                                       
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(RF),C'L'                                         
         BE    PRD90                                                            
         MVC   SCR1WDSC,0(R3)                                                   
         LA    R3,L'SCR1WDSC(R3)                                                
         LA    R2,L'DUMLIN1(R2)                                                 
         BCT   RE,*-14                                                          
         B     PRDX                                                             
PRD90    MVC   SCR1NDSC,0(R3)                                                   
         LA    R3,L'SCR1NDSC(R3)                                                
         LA    R2,L'DUMLIN1(R2)                                                 
         BCT   RE,*-14                                                          
*                                                                               
PRDX     J     XITR2               EXIT WITHOUT RESTORING R2                    
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        DISPLAY CLIENT/PRODUCR/JOB                                   *         
* ON ENTRY     R1=A(CLI/PRO/JOB ACCOUNT)                              *         
* ON ENTRY     R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
* ON EXIT      R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
***********************************************************************         
CPJDET   NTR1                                                                   
         MVI   TEMP,C' '                                                        
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
*                                                                               
         MVC   TEMP(L'MX@CLINT),MX@CLINT                                        
         LA    RF,TEMP+L'MX@CLINT-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         SR    RE,RE                                                            
         IC    RE,CLEN                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,RF),0(R1)                                                    
         LA    RF,3+1(RE,RF)                                                    
*                                                                               
         SR    RE,RE                                                            
         IC    RE,CLEN                                                          
         AR    R1,RE                                                            
         CLI   0(R1),C' '                                                       
         BE    CPJ40                                                            
*                                                                               
         MVC   0(L'MX@PRO,RF),MX@PRO                                            
         LA    RF,L'MX@PRO-1(RF)                                                
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         SR    R0,R0                                                            
         IC    R0,CLEN                                                          
         SR    RE,RE                                                            
         IC    RE,CPLEN                                                         
         SR    RE,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,RF),0(R1)                                                    
         LA    RF,3+1(RE,RF)                                                    
*                                                                               
         SR    R0,R0                                                            
         IC    R0,CLEN                                                          
         SR    RE,RE                                                            
         IC    RE,CPLEN                                                         
         SR    RE,R0                                                            
         AR    R1,RE                                                            
         CLI   0(R1),C' '                                                       
         BE    CPJ40                                                            
         TM    SLEDGATR,LEDGSPOT+LEDGPRNT SPOT OR PRINT                         
         BNZ   CPJ35                                                            
*                                                                               
         MVC   0(L'MX@JOB,RF),MX@JOB                                            
         LA    RF,L'MX@JOB-1(RF)                                                
         CLC   SRECUL,BASKEY                                                    
         BE    *+14                                                             
         CLC   SI,BASKEY                                                        
         BNE   CPJ36                                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'/'                                                       
         MVC   2(L'MX@EST,RF),MX@EST                                            
         LA    RF,L'MX@EST-1+2(RF)                                              
         B     CPJ36                                                            
CPJ35    MVC   0(L'MX@EST,RF),MX@EST                                            
         LA    RF,L'MX@EST-1(RF)                                                
CPJ36    CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         SR    R0,R0                                                            
         IC    R0,CPLEN                                                         
         SR    RE,RE                                                            
         IC    RE,CPJLEN                                                        
         SR    RE,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,RF),0(R1)                                                    
         LA    RF,2(RE,RF)                                                      
*                                                                               
CPJ40    CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RE,TEMP-1                                                        
         SR    RF,RE                                                            
         BZ    CPJX                                                             
*                                                                               
         LA    R0,L'SCR1WDSC                                                    
         L     R1,AOPTVALS         R1=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(R1),C'L'                                         
         BNE   *+8                                                              
         LA    R0,L'SCR1NDSC                                                    
         GOTO1 VCHOPPER,DMCB,((RF),TEMP),((R0),SCANBLK),2                       
         L     RE,DMCB+8           RE=(NUMBER OF STATUS LINES)                  
         LTR   RE,RE                                                            
         BZ    PRD40                                                            
         SR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         SR    R1,R1                                                            
         IC    R1,MAXLINES                                                      
         AR    RF,RE                                                            
         CR    RF,R1                                                            
         BNH   *+6                                                              
         LR    RF,R1                                                            
         STC   RF,LINSUSED                                                      
         LA    R3,SCANBLK                                                       
         USING SCRLIN1D,R2                                                      
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OVALUE-OPTVALSD(RF),C'L'                                         
         BE    CPJ50                                                            
         MVC   SCR1WDSC,0(R3)                                                   
         LA    R3,L'SCR1WDSC(R3)                                                
         LA    R2,L'DUMLIN1(R2)                                                 
         BCT   RE,*-14                                                          
         B     CPJX                                                             
CPJ50    MVC   SCR1NDSC,0(R3)                                                   
         LA    R3,L'SCR1NDSC(R3)                                                
         LA    R2,L'DUMLIN1(R2)                                                 
         BCT   RE,*-14                                                          
*                                                                               
CPJX     J     XITR2                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT TIME INFORMATION USA ONLY                             *         
* ON ENTRY     R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
* ON EXIT      R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
***********************************************************************         
         USING SCRLIN1D,R2         DSECT FOR SCREEN DATA LINE                   
TIMEINF  NTR1                                                                   
*                                                                               
         SR    R3,R3                                                            
         MVI   TEMP,C' '                                                        
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
         LA    RF,TEMP                                                          
         SR    RE,RE                                                            
         ICM   R4,15,APRTELD       PERSONNEL RATE ELEMENT?                      
         BZ    TIME15                                                           
         USING PRTELD,R4                                                        
         CP    PRTHOUR,=P'0'                                                    
         BE    TIME05                                                           
         MVC   SCR1WDSC(L'MX@HOURS),MX@HOURS                                    
         LA    RF,SCR1WDSC+L'MX@HOURS-1                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         CURED (P3,PRTHOUR),(12,2(RF)),2,MINUS=YES,ALIGN=LEFT                   
         LA    R3,1(R3)                                                         
         LA    R2,L'DUMLIN1(R2)                                                 
*                                                                               
TIME05   LA    RF,TEMP                                                          
         TM    PRTSTAT,PRTSBILQ+PRTSNOTQ+PRTSRTEQ                               
         BZ    TIME15                                                           
         MVC   0(L'MX@TIME,RF),MX@TIME                                          
         LA    RF,L'MX@TIME-1(RF)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         TM    PRTSTAT,PRTSBILQ    BILLABLE TIME?                               
         BNO   *+12                                                             
         MVI   2(RF),C'B'                                                       
         B     TIME10                                                           
         TM    PRTSTAT,PRTSNOTQ    NON-BILLABLE TIME?                           
         BNO   *+12                                                             
         MVI   2(RF),C'N'                                                       
         B     TIME10                                                           
         MVI   2(RF),C'R'          ASSUME SPECIAL NON-BILLABLE TIME             
*                                                                               
TIME10   LA    RF,4(RF)                                                         
         LA    RE,1                                                             
TIME15   L     R4,ATSARREC         R4=A(TSAR RECORD )                           
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         TM    TSDSTAT2,TRSSTIME+TRSSTADJ+TRSSTMSS                              
         BZ    TIME20                                                           
         MVC   0(L'MX@TMTYP,RF),MX@TMTYP                                        
         LA    RF,L'MX@TMTYP-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         TM    TSDSTAT2,TRSSTIME   TIME SHEET REGULAR?                          
         BNO   *+12                                                             
         MVI   2(RF),C'T'                                                       
         B     TIME17                                                           
         TM    TSDSTAT2,TRSSTADJ   TIME SHEET ADJUSTMENT?                       
         BNO   *+12                                                             
         MVI   2(RF),C'A'                                                       
         B     TIME17                                                           
         MVI   2(RF),C'M'          ASSUME TIME SHEET MISSING                    
TIME17   LA    RE,1                                                             
TIME20   LTR   RE,RE                                                            
         BZ    TIME30                                                           
         MVC   SCR1NDSC(L'SCR1NDSC),TEMP                                        
         LA    R2,L'DUMLIN1(R2)                                                 
TIME30   AR    R3,RE                                                            
         BZ    TIMEX                                                            
         SR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         SR    R1,R1                                                            
         IC    R1,MAXLINES                                                      
         AR    RF,R3                                                            
         CR    RF,R1                                                            
         BNH   *+6                                                              
         LR    RF,R1                                                            
         STC   RF,LINSUSED                                                      
*                                                                               
TIMEX    J     XITR2               EXIT WITHOUT RESTORING R2                    
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT ATTRIBUTE LINE(S)                                     *         
* ON ENTRY     R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              R4=A(TSAR DATA)                                        *         
* ON EXIT      R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
***********************************************************************         
         USING SCRLIN1D,R2         DSECT FOR SCREEN DATA LINE                   
ATTRLNS  NTR1                                                                   
*                                                                               
         ICM   R4,15,AAPEELD       ANALYSIS POINTER ELEMENT?                    
         BZ    ATTRLX                                                           
         USING APEELD,R4                                                        
         MVI   TEMP,C' '             < USE TEMP AND WORK                        
         MVC   TEMP+1(L'TEMP-1),TEMP < TO BUILD ATTRIBUTE                       
         MVC   WORK,SPACES           < LIST                                     
         LA    RF,WORK                                                          
         MVC   0(L'MX@ATTRS,RF),MX@ATTRS                                        
         LA    RF,L'MX@ATTRS-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(RF)                                                         
         SR    R0,R0                                                            
         CLI   APELN,APELN1Q                                                    
         BNH   ATTRLX                                                           
         ICM   R0,1,APENUM         NUMBER OF SUB ELEMENTS                       
         BZ    ATTRLX                                                           
         LA    R4,APENTRY          R4=A(FIRST SUB ELEMENT)                      
         USING APENTRY,R4                                                       
ATTRL10  SR    RE,RE                                                            
         IC    RE,APENLEN          RE=L'(SUB ELEMENT)                           
         SHI   RE,APELN2Q+1                                                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),APENACT     GET ACCOUNT CODE                             
         LA    RF,2(RE,RF)         BUMP RF TO NEXT POSITION                     
         SR    RE,RE                                                            
         IC    RE,APENLEN                                                       
         AR    R4,RE               BUMP R4 TO NEXT SUB ELEMENT                  
         BCT   R0,ATTRL10                                                       
         LA    RE,WORK                                                          
         SR    RF,RE                                                            
         BZ    ATTRLX                                                           
         GOTO1 VCHOPPER,DMCB,((RF),WORK),(L'DUMLIN1,SCANBLK),2                  
         L     RE,DMCB+8           R4=(NUMBER OF ATTRIBUTE LINES)               
         LTR   RE,RE                                                            
         BZ    ATTRLX                                                           
         SR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         SR    R1,R1                                                            
         IC    R1,MAXLINES                                                      
         AR    RF,RE                                                            
         CR    RF,R1                                                            
         BH    ATTRLX                                                           
         STC   RF,LINSUSED                                                      
         LA    R4,SCANBLK                                                       
         MVC   0(L'DUMLIN1,R2),0(R4)                                            
         LA    R4,L'DUMLIN1(R4)                                                 
         LA    R2,L'DUMLIN1(R2)                                                 
         BCT   RE,*-14                                                          
*                                                                               
ATTRLX   J     XITR2               EXIT WITHOUT RESTORING R2                    
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        DEAL WITH THE TOTAL LINE                                     *         
***********************************************************************         
TOTAL    NTR1                                                                   
*NMAL BELOW CODE IS COMMENETED TO DISPLAY TOTAL AMOUNT LINE                     
*     FOR MULTIPLE CURRENCIES SPEC-13637 REGRESSION BUG                         
*NMAL    L     RF,AOPTVALS                                                      
*NMAL    CLI   OVALUE-OPTVALSD(RF),C'B' IF VAL=BOTH SHOW AGENCY TOT             
*NMAL    BE    *+12                                                             
*NMAL    TM    CURRFLAG,CURRSRTQ   SUPPRESS REQUEST TOTALS                      
*NMAL    BO    TOTX                                                             
*                                                                               
TOT02    L     R0,ATSARREC         CLEAR TSAR RECORD                            
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
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARTOTD,R3                                                      
         MVI   TSTFMT,TSTOTITM     TOTAL ITEM                                   
         ZAP   TSTTTOT,TRNTOT      TRANS TOTAL                                  
         ZAP   TSTHTOT,HRSTOT      HOURS TOTAL                                  
         ZAP   TSTTDR,TRNTDR      TRANS TOTAL                                   
         ZAP   TSTTCR,TRNTCR      TRANS TOTAL                                   
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BO    *+12                 YES                                         
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
*                                                                               
TOT09    MVC   TSTLINES,LINSUSED   NUMBER OF LINES USED BY TOTAL                
         GOTO1 ATSARADD                                                         
         MVC   TSLSTREC,TSCURRNO                                                
*                                                                               
TOT10    GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BNE   *+10                                                             
         MVC   TSLSTLIN,TSCURRNO                                                
TOTX     J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
OKXIT    CR    RB,RB                                                            
         J     XIT                                                              
ERXIT    LTR   RB,RB                                                            
XIT      XIT1                                                                   
XITR2    XIT1  REGS=(R2)                                                        
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                                     
***********************************************************************         
         LTORG                                                                  
*                                                                               
NINES    DC    C'999999999'                                                     
UNDERLIN DC    C'--------------'                                                
ONEP     DC    C'1P'                                                            
ONER     DC    C'1R'                                                            
ONEJ     DC    C'1J'                                                            
SI       DC    C'SI'                                                            
SR       DC    C'SR'                                                            
UL41     DC    C'41'               FIXED ASSETS                                 
DEFMFILE DC    C'N/A'              DEFAULT MEDIA FILE                           
LARF     DC    X'41F0'                                                          
*                                                                               
HRSCOLQ  EQU   C'H'                HOURS COLUMN OVERRIDE?                       
BASCOLQ  EQU   C'B'                BASIS COLUMN OVERRIDE?                       
TAXCOLQ  EQU   C'G'                GST COLUMN OVERRIDE?                         
PSTCOLQ  EQU   C'P'                PST COLUMN OVERRIDE?                         
TXACOLQ  EQU   C'T'                GST AND PST TAX OVERRIDE?                    
MBVOFFSQ EQU   3                   OFFSET TO 127+ TRANS INDI IN REF             
MBVINDIQ EQU   C'*'                INDICATOR FOR 127+ TRANS                     
         EJECT                                                                  
         DS    0D                                                               
       ++INCLUDE ACPRVTABN         CANADIAN PROVINCE NAME TABLE                 
         EJECT                                                                  
*                                                                               
DCCAP    DS    0X                                                               
         DCDDL AC#ONT,3                                                         
         DCDDL AC#PST,3                                                         
         DCDDL AC#VAT,3                                                         
         DCDDL AC#QST,3                                                         
         DCDDL AC#HST,3                                                         
         DCDDL AC#HBC,6                                                         
         DCDDL AC#HAL,6                                                         
         DCDDL AC#HSA,6                                                         
         DCDDL AC#HMA,6                                                         
         DCDDL AC#HPQ,6                                                         
         DCDDL AC#HON,6                                                         
         DCDDL AC#HPE,6                                                         
         DCDDL AC#HNB,6                                                         
         DCDDL AC#HNS,6                                                         
         DCDDL AC#HNF,6                                                         
         DC    AL1(EOT)                                                         
*                                                                               
DCMIX    DS    0X                                                               
         DCDDL AC#ENH14,78                                                      
         DCDDL AC#ENH15,78                                                      
         DCDDL AC#RCND,L'MX@RCND                                                
         DCDDL AC#DATE,L'MX@DATE                                                
         DCDDL AC#DRAFT,9                                                       
         DCDDL AC#PYMNT,10                                                      
         DCDDL AC#CHK,10                                                        
         DCDDL AC#BNK,10                                                        
         DCDDL AC#CHKTO,10                                                      
         DCDDL AC#PAYTO,10                                                      
         DCDDL AC#GROSS,7                                                       
         DCDDL AC#NET,7                                                         
         DCDDL AC#DISS,7                                                        
         DCDDL AC#FLTX,7                                                        
         DCDDL AC#ORDER,7                                                       
         DCDDL AC#SUBR,7                                                        
         DCDDL AC#CMRCL,7                                                       
         DCDDL AC#CLINT,7                                                       
         DCDDL AC#PRO,3                                                         
         DCDDL AC#JOB,7                                                         
         DCDDL AC#EST,3                                                         
         DCDDL AC#EXP,7                                                         
         DCDDL AC#SRC,7                                                         
         DCDDL AC#TIME,5                                                        
         DCDDL AC#TMTYP,5                                                       
         DCDDL AC#DUEDT,8                                                       
         DCDDL AC#SERNO,6                                                       
         DCDDL AC#RUNNO,7                                                       
         DCDDL AC#FACC,7                                                        
         DCDDL AC#ATTRS,L'MX@ATTRS                                              
         DCDDL AC#ATHED,9                                                       
         DCDDL AC#UATH,9                                                        
         DCDDL AC#HELD,9                                                        
         DCDDL AC#SELED,9                                                       
         DCDDL AC#APRVD,9                                                       
         DCDDL AC#URG,9                                                         
         DCDDL AC#RVRSD,9                                                       
         DCDDL AC#HOURS,5                                                       
         DCDDL AC#HOURS,5,R                                                     
         DCDDL AC#BASIS,5,R                                                     
         DCDDL AC#VAT,5,R                                                       
         DCDDL AC#PST,5,R                                                       
         DCDDL AC#TAX,5,R                                                       
         DCDDL AC#TYPE,2                                                        
         DCDDL AC#STT,2                                                         
         DCDDL AC#OTHER,2                                                       
         DCDDL AC#UDAT,2                                                        
         DCDDL AC#PELDT,2                                                       
         DCDDL AC#CR,2                                                          
         DCDDL AC#DR,2                                                          
         DCDDL AC#SEQ,2                                                         
         DCDDL AC#ACCT,15                                                       
         DCDDL AC#ACCTS,15                                                      
         DCDDL AC#CHKC,20                                                       
         DCDDL AC#DPSON,20                                                      
         DCDDL AC#CTRON,20                                                      
         DCDDL AC#WRTFA,20                                                      
         DCDDL AC#XFRFR,20                                                      
         DCDDL AC#XFRTO,20                                                      
         DCDDL AC#DATED,10                                                      
         DCDDL AC#REF,10                                                        
         DCDDL AC#PAYEE,10                                                      
         DCDDL AC#ADR,7                                                         
         DCDDL AC#LCLCU,16,R                                                    
         DCDDL AC#DSCAD,3                                                       
         DCDDL AC#DPR,6                                                         
* GRID COLUMNS                                                                  
         DCDDL AC#ACTDT,L'MX@ACTDT                                              
         DCDDL AC#GSTBA,L'MX@GSTBA                                              
         DCDDL AC#RSBRF,L'MX@RSBRF                                              
         DCDDL AC#NFA67,L'MX@BANKA                                              
         DCDDL AC#RSCDT,L'MX@RSCDT                                              
         DCDDL AC#RSCNO,L'MX@RSCNO                                              
         DCDDL AC#CHKT,L'MX@CHKT                                                
         DCDDL AC#CLRDA,L'MX@CLRDA                                              
         DCDDL AC#RSDDT,L'MX@RSDDT                                              
         DCDDL AC#RSCOC,L'MX@RSCOC                                              
         DCDDL AC#DISS,L'MX@DISL                                                
         DCDDL AC#DISB,L'MX@DISB                                                
         DCDDL AC#RSDUD,L'MX@RSDUD                                              
         DCDDL AC#EST,L'MX@ESTL                                                 
         DCDDL AC#VAT,L'MX@VAT                                                  
         DCDDL AC#INCM,L'MX@INCM                                                
         DCDDL AC#MEMO,L'MX@MEMO                                                
         DCDDL AC#MOA,L'MX@MOA                                                  
         DCDDL AC#MMOS,L'MX@MMOS               MEDIA MONTH OF SERVICE           
         DCDDL AC#OFFST,L'MX@OFFST                                              
         DCDDL AC#PELED,L'MX@PELED                                              
         DCDDL AC#PRO,L'MX@PROL                                                 
         DCDDL AC#STMDT,L'MX@STMDT                                              
         DCDDL AC#TIMTP,L'MX@TIMTP                                              
         DCDDL AC#RSUSD,L'MX@RSUSD                                              
         DCDDL AC#VOID,L'MX@VOID                                                
         DCDDL AC#WC,L'MX@WC                                                    
         DCDDL AC#TRANT,L'MX@TRANT                                              
         DCDDL AC#BAL,L'MX@BAL                                                  
         DCDDL AC#DRCR,L'MX@DRCR                                                
         DCDDL AC#TRANO,L'MX@TRANO                                              
         DCDDL AC#TRANR,L'MX@TRANR                                              
         DCDDL AC#TRAND,L'MX@TRAND                                              
         DCDDL AC#REVDT,L'MX@REVDT                                              
         DCDDL AC#GRAMT,L'MX@GRAMT                                              
         DCDDL AC#STMD2,L'MX@STMD2                                              
         DCDDL AC#RSODT,L'MX@RSODT                                              
         DCDDL AC#DISAM,L'MX@DISAM                                              
         DCDDL AC#CMNAM,L'MX@CMNAM                                              
         DCDDL AC#NETA2,L'MX@NETA2                                              
         DCDDL AC#RSPID,L'MX@PID                                                
         DCDDL AC#INVC2,L'MX@INVCN  INVOICE NUMBER                              
         DCDDL AC#JOEST,L'MX@JOEST  JOB/ESTIMATE                                
         DCDDL AC#PAYME,L'MX@PAYME  PAYMENT METHOD 'EFT/CHECK/PCARD'            
         DCDDL AC#KEYRF,L'MX@KEYRF                                              
         DCDDL AC#BLBDT,L'MX@BLBDT  BILLABLE DATE                               
         DCDDL AC#APRVM,L'MX@APRVM  APPROVAL METHOD                             
         DCDDL AC#AAPP,L'MX@AAPP    AUTO APPROVED                               
         DCDDL AC#MAPP,L'MX@MAPP    MARKER APPROVED                             
         DCDDL AC#RSCYC,L'MX@CURC   CURRENCY CODE                               
         DCDDL AC#MFILE,L'MX@MFILE  MEDIA FILE (AGENCY ALPHA ID)                
*                                                                               
         DC    AL1(EOT)                                                         
***********************************************************************         
*        DETAIL GRID COLUMN TABLE - COVERED BY GCTBLD                           
***********************************************************************         
GCTBL    DS    0F                                                               
*        -----------------------------------                                    
GCTCONT  DC    AL1(GCONTLQ,GCCONT,L'MX@RSCOC,L'TSDCONT)  CONTRA                 
         DC    AL2(MX@RSCOC-OVERWRKD,TSDCONT-TSARDATD)                          
         DC    AL1(GCTITOT+GCTIOVER,0,0,0)                                      
         DC    AL1(0,L'MX@ACCT),AL2(MX@ACCT-OVERWRKD)                           
GCONTLQ  EQU   *-GCTCONT                                                        
*        -----------------------------------                                    
GCTDATE  DC    AL1(GDATLQ,GCDATE,L'MX@TRAND,1)           DATE                   
         DC    AL2(MX@TRAND-OVERWRKD,TSDTDAT-TSARDATD)                          
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GDATLQ   EQU   *-GCTDATE                                                        
*        -----------------------------------                                    
GCTCOL4  DC    AL1(GCT4LQ,GCREF,L'MX@TRANR,L'TSDTREF)    TRANS REF              
         DC    AL2(MX@TRANR-OVERWRKD,TSDTREF-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GCT4LQ   EQU   *-GCTCOL4                                                        
*        -----------------------------------                                    
GCTCOL06 DC    AL1(GCT06LQ,GCOFF,L'MX@TRANO,L'TSDOFF)    OFFICE                 
         DC    AL2(MX@TRANO-OVERWRKD,TSDOFF-TSARDATD)                           
         DC    AL1(GCTIOVER,0,GCTFCENT,0)                                       
         DC    AL1(0,0),AL2(0)                                                  
GCT06LQ  EQU   *-GCTCOL06                                                       
*        -----------------------------------                                    
GCTMOA   DC    AL1(GMOALQ,GCMOA,L'MX@MOA,1)              MOA                    
         DC    AL2(MX@MOA-OVERWRKD,TSDTMOA-TSARDATD)                            
         DC    AL1(GCTIOVER,GCTIMONO,GCTFDAT+GCTFRGHT,0)                        
         DC    AL1(0,0),AL2(0)                                                  
GMOALQ   EQU   *-GCTMOA                                                         
*        -----------------------------------                                    
GCTAMNT  DC    AL1(GAMNTLQ,GCAMNT,L'LC@AMT,L'TSDATAMT)   AMOUNT                 
         DC    AL2(LC@AMT-WORKD,TSDATAMT-TSARDATD)                              
         DC    AL1(GCTITOT,0,GCTFNUM+GCTFRGHT,0)                                
         DC    AL1(0,L'TSTTTOT),AL2(TSTTTOT-TSARTOTD)                           
GAMNTLQ  EQU   *-GCTAMNT                                                        
*        -----------------------------------                                    
GCTDRCR  DC    AL1(GDRCRLQ,GCDRCR,L'MX@DRCR,0)           DR/CR                  
         DC    AL2(MX@DRCR-OVERWRKD,ASPCGR-OVERWRKD)                            
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
GDRCRLQ  EQU   *-GCTDRCR                                                        
*        -----------------------------------                                    
GCTCOL5  DC    AL1(GCT5LQ,GCBATR,L'MX@RSBRF,L'TSDBREF)   BATCH REF              
         DC    AL2(MX@RSBRF-OVERWRKD,TSDBREF-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GCT5LQ   EQU   *-GCTCOL5                                                        
*        -----------------------------------                                    
GCTMMOS1 DC    AL1(GMMOS1LQ,GCMMOS,L'MX@MMOS-6,1)  MOS MONTH OF SERVICE         
         DC    AL2(MX@MMOS+6-OVERWRKD),AL1(MBIELQ,MBIMOS-MBIELD)                
         DC    AL1(GCTIOVER+GCTIELEM,GCTIMONO,GCTFDAT+GCTFRGHT,0)               
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SI',C'SR',C'SK',C'SP',C'SQ',C'SU',C'SS',C'ST'                  
GMMOS1LQ EQU   *-GCTMMOS1                                                       
*                                                                               
GCTMMOS2 DC    AL1(GMMOS2LQ,GCMMOS,L'MX@MMOS-6,1)  MOS MONTH OF SERVICE         
         DC    AL2(MX@MMOS+6-OVERWRKD),AL1(OTHELQ,OTHDATE-OTHELD)               
         DC    AL1(GCTIOVER+GCTIELEM,GCTIMONO,GCTFDAT+GCTFRGHT,0)               
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SI',C'SR',C'SK',C'SP',C'SQ',C'SU',C'SS',C'ST'                  
GMMOS2LQ EQU   *-GCTMMOS2                                                       
*                                                                               
GCTMMOS3 DC    AL1(GMMOS3LQ,GCMMOS,L'MX@MMOS-6,1)  MOS MONTH OF SERVICE         
         DC    AL2(MX@MMOS+6-OVERWRKD),AL1(GDAELQ,GDADATE-GDAELD)               
         DC    AL1(GCTIOVER+GCTIELEM,GCTIMONO,GCTFDAT+GCTFRGHT,0)               
         DC    AL1(GDAMMOS,0),AL2(0)                                            
         DC    C'SI',C'SR',C'SK',C'SP',C'SQ',C'SU',C'SS',C'ST'                  
GMMOS3LQ EQU   *-GCTMMOS3                                                       
*                                                                               
GCTMMOS4 DC    AL1(GMMOS4LQ,GCMMOS,L'MX@MMOS-6,0)  MOS MONTH OF SERVICE         
         DC    AL2(MX@MMOS+6-OVERWRKD,ASPCGR-OVERWRKD)                          
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SI',C'SR',C'SK',C'SP',C'SQ',C'SU',C'SS',C'ST'                  
GMMOS4LQ EQU   *-GCTMMOS4                                                       
*        -----------------------------------                                    
GCTCOL10 DC    AL1(GCT10LQ,GCTRNTYP,L'MX@TRANT,1)        TRN TYPE               
         DC    AL2(MX@TRANT-OVERWRKD,TSDTTYPE-TSARDATD)                         
         DC    AL1(GCTIOVER,GCTIBIN,GCTFNUM+GCTFCENT,0)                         
         DC    AL1(0,0),AL2(0)                                                  
GCT10LQ  EQU   *-GCTCOL10                                                       
*        -----------------------------------                                    
GCTCOL12 DC    AL1(GCT12LQ,GCACTDTE,L'MX@ACTDT,2)        ACTIVITY DATE          
         DC    AL2(MX@ACTDT-OVERWRKD,TSDACDAT-TSARDATD)                         
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GCT12LQ  EQU   *-GCTCOL12                                                       
*        -----------------------------------                                    
GCTDD01  DC    AL1(GDD01LQ,GCUSDTE,L'MX@RSUSD,2)         USED DATE              
         DC    AL2(MX@RSUSD-OVERWRKD,TSDUSDAT-TSARDATD)                         
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                        (PAYABLES)                
         DC    C'SS',C'ST',C'SU',C'SP',C'SQ',C'SV',C'SY',C'SW',C'SX'            
GDD01LQ  EQU   *-GCTDD01                                                        
*        -----------------------------------                                    
GCTCOL13 DC    AL1(GCT13LQ,GCREVDTE,L'MX@REVDT,2)        REVERSAL DATE          
         DC    AL2(MX@REVDT-OVERWRKD,TSDRVDAT-TSARDATD)                         
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GCT13LQ  EQU   *-GCTCOL13                                                       
*        -----------------------------------                                    
GCTCOL14 DC    AL1(GCT14LQ,GCAPPR,L'MX@APRVD,TRNSAPPR)   APPROVED               
         DC    AL2(MX@APRVD-OVERWRKD,TSDTEST-TSARDATD)                          
         DC    AL1(GCTIOVER,GCTIYON,0,0)                                        
         DC    AL1(0,0),AL2(0)                          (PAYABLES)              
         DC    C'SS',C'ST',C'SU',C'SP',C'SQ',C'SV',C'SY',C'SW',C'SX'            
GCT14LQ  EQU   *-GCTCOL14                                                       
*        -----------------------------------                                    
GCTAMET  DC    AL1(GCTAMELQ,GCMETH,L'MX@APRVM,0)       APPROVAL METHOD          
         DC    AL2(MX@APRVM-OVERWRKD,ASPCGR-OVERWRKD)                           
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SS',C'ST',C'SU',C'SP',C'SQ',C'SV',C'SY',C'SW',C'SX'            
GCTAMELQ EQU   *-GCTAMET                                                        
*        -----------------------------------                                    
GCTURG   DC    AL1(GURGLQ,GCURG,L'MX@URG,TRNSURG)        URGENT                 
         DC    AL2(MX@URG-OVERWRKD,TSDTEST-TSARDATD)                            
         DC    AL1(GCTIOVER,GCTIYON,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SV',C'SW',C'SX',C'SY'                                          
GURGLQ   EQU   *-GCTURG                                                         
*        -----------------------------------                                    
GCTHELD  DC    AL1(GHELDLQ,GCHELD,L'MX@HELD,TRNSHOLD)    HOLD                   
         DC    AL2(MX@HELD-OVERWRKD,TSDTEST-TSARDATD)                           
         DC    AL1(GCTIOVER,GCTIYON,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SP',C'SQ',C'SS',C'ST'                                    
         DC    C'SU',C'SV',C'SW',C'SX',C'SY'                                    
GHELDLQ  EQU   *-GCTHELD                                                        
*        -----------------------------------                                    
GCTVOID  DC    AL1(GVOIDLQ,GCVOID,L'MX@VOID,TRSSVOID)    VOID                   
         DC    AL2(MX@VOID-OVERWRKD,TSDSTAT1-TSARDATD)                          
         DC    AL1(GCTIOVER,GCTIYON,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SS',C'ST',C'SU',C'SP',C'SQ',C'SV',C'SY',C'SW',C'SX'            
GVOIDLQ  EQU   *-GCTVOID                                                        
*        -----------------------------------                                    
GCTHOUR  DC    AL1(GHOURLQ,GCHOUR,L'MX@HOURR,L'PRTHOUR)  HOURS                  
         DC    AL2(MX@HOURR-OVERWRKD),AL1(PRTELQ,PRTHOUR-PRTELD)                
         DC    AL1(GCTIOVER+GCTIELEM,0,GCTFNUM+GCTFRGHT,0)                      
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'1J',C'1R'                                                      
GHOURLQ  EQU   *-GCTHOUR                                                        
*        -----------------------------------                                    
GCTTIMT  DC    AL1(GTIMTLQ,GCTIMT,L'MX@TIMTP,0)          TIME TYPE              
         DC    AL2(MX@TIMTP-OVERWRKD),AL2(ASPCGR-OVERWRKD)                      
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'1J',C'1R'                                                      
GTIMTLQ  EQU   *-GCTTIMT                                                        
*        -----------------------------------                                    
GCTCOL19 DC    AL1(GCT19LQ,GCCHKNO,L'MX@RSCNO,L'MPYNO)   CHECK NUMBER           
         DC    AL2(MX@RSCNO-OVERWRKD),AL1(MPYELQ,MPYNO-MPYELD)                  
         DC    AL1(GCTIOVER+GCTIELEM,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SP',C'SQ',C'SS',C'ST'                                    
         DC    C'SU',C'SV',C'SW',C'SX',C'SY'                                    
GCT19LQ  EQU   *-GCTCOL19                                                       
GCTCOL20 DC    AL1(GCT20LQ,GCCHKNO,L'MX@RSCNO,L'RALAREF) CHECK NUMBER           
         DC    AL2(MX@RSCNO-OVERWRKD),AL1(RALELQ,RALAREF-RALELD)                
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0,0,0)                            
         DC    AL1(RALTALC,0),AL2(0)                                            
         DC    C'SR',C'SP',C'SQ',C'SS',C'ST'                                    
         DC    C'SU',C'SV',C'SW',C'SX',C'SY'                                    
GCT20LQ  EQU   *-GCTCOL20                                                       
*        -----------------------------------                                    
GCTCHKDT DC    AL1(GCHDTLQ,GCCHKDTE,L'MX@RSCDT,2)        CHECK DATE             
         DC    AL2(MX@RSCDT-OVERWRKD),AL1(MPYELQ,MPYDTE-MPYELD)                 
         DC    AL1(GCTIOVER+GCTIELEM,0,GCTFDAT+GCTFRGHT,0)                      
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SP',C'SQ',C'SS',C'ST'                                    
         DC    C'SU',C'SV',C'SW',C'SX',C'SY'                                    
GCHDTLQ  EQU   *-GCTCHKDT                                                       
GCTCOL21 DC    AL1(GCT21LQ,GCCHKDTE,L'MX@RSCDT,1)        CHECK DATE             
         DC    AL2(MX@RSCDT-OVERWRKD),AL1(RALELQ,RALADAT-RALELD)                
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0,GCTFDAT+GCTFRGHT,0)             
         DC    AL1(RALTALC,0),AL2(0)                                            
         DC    C'SR',C'SP',C'SQ',C'SS',C'ST'                                    
         DC    C'SU',C'SV',C'SW',C'SX',C'SY'                                    
GCT21LQ  EQU   *-GCTCOL21                                                       
*        -----------------------------------                                    
GCTDEPDT DC    AL1(GDEPDLQ,GCDEPDTE,L'MX@RSDDT,1)        DEPOSIT DATE           
         DC    AL2(MX@RSDDT-OVERWRKD),AL1(RALELQ,RALADEP-RALELD)                
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0,GCTFDAT+GCTFRGHT,0)             
         DC    AL1(RALTALC,0),AL2(0)                                            
         DC    C'SC',C'SR'                                                      
GDEPDLQ  EQU   *-GCTDEPDT                                                       
*        -----------------------------------                                    
GCTPYMET DC    AL1(GCTPMLQ,GCPYMET,L'MX@PAYME,0)        PAYMENT METHOD          
         DC    AL2(MX@PAYME-OVERWRKD,ASPCGR-OVERWRKD)                           
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SS',C'ST',C'SU',C'SP',C'SQ',C'SV',C'SY',C'SW',C'SX'            
         DC    C'SC'                                                            
GCTPMLQ  EQU   *-GCTPYMET                                                       
*        -----------------------------------                                    
GCTDUEDT DC    AL1(GDUEDLQ,GCDUE,L'MX@DUEDT,2)           DUE DATE               
         DC    AL2(MX@DUEDT-OVERWRKD),AL1(DUEELQ,DUEDATE-DUEELD)                
         DC    AL1(GCTIOVER+GCTIELEM,0,GCTFDAT+GCTFRGHT,0)                      
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SU'                                                      
         DC    C'SV',C'SW',C'SX',C'SY'                                          
GDUEDLQ  EQU   *-GCTDUEDT                                                       
*                                                                               
GCTDUED1 DC    AL1(GDUEDLQ1,GCDUE,L'MX@DUEDT,1)          DUE DATE               
         DC    AL2(MX@DUEDT-OVERWRKD,TSDTDAT-TSARDATD)                          
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SU'                                                      
         DC    C'SV',C'SW',C'SX',C'SY'                                          
GDUEDLQ1 EQU   *-GCTDUED1                                                       
*        -----------------------------------                                    
GCTCHTO  DC    AL1(GCHTOLQ,GCCHKTOT,L'MX@CHKT,L'MPYAMNT) CHECK TOTAL            
         DC    AL2(MX@CHKT-OVERWRKD),AL1(MPYELQ,MPYAMNT-MPYELD)                 
         DC    AL1(GCTIOVER+GCTIELEM,0,GCTFNUM+GCTFRGHT,0)                      
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SP',C'SQ',C'SS',C'ST'                                    
         DC    C'SU',C'SV',C'SW',C'SX',C'SY'                                    
GCHTOLQ  EQU   *-GCTCHTO                                                        
*        -----------------------------------                                    
GCTBNK   DC    AL1(GBNKLQ,GCBANK,L'MX@BANKA,L'MPYBNK)     BANK ACCOUNT          
         DC    AL2(MX@BANKA-OVERWRKD),AL1(MPYELQ,MPYBNK-MPYELD)                 
         DC    AL1(GCTIOVER+GCTIELEM,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SP',C'SQ',C'SS',C'ST'                                    
         DC    C'SU',C'SV',C'SW',C'SX',C'SY'                                    
GBNKLQ   EQU   *-GCTBNK                                                         
*        -----------------------------------                                    
GCTCOL23 DC    AL1(GCT23LQ,GCOFFSET,L'MX@RSODT,2)        OFFSET                 
         DC    AL2(MX@RSODT-OVERWRKD),AL2(TSDOFDAT-TSARDATD)                    
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SP',C'SQ',C'SS',C'ST'                                    
         DC    C'SU',C'SV',C'SW',C'SX',C'SY'                                    
GCT23LQ  EQU   *-GCTCOL23                                                       
*        -----------------------------------                                    
GCTCLI1  DC    AL1(GCLI1LQ,GCCLI,L'MX@CLINT,L'MDTCLI)    CLIENT                 
         DC    AL2(MX@CLINT-OVERWRKD),AL1(MDTELQ,MDTCLI-MDTELD)                 
         DC    AL1(GCTIOVER+GCTIELEM,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SI',C'SE',C'SK',C'1C',C'SX'                              
         DC    C'SP',C'SQ',C'SU',C'SS',C'ST',C'SV'                              
GCLI1LQ  EQU   *-GCTCLI1                                                        
*        -----------------------------------                                    
GCTCLI2  DC    AL1(GCLI2LQ,GCCLI,L'MX@CLINT,L'MDPCLI)    CLIENT                 
         DC    AL2(MX@CLINT-OVERWRKD),AL1(MDPELQ,MDPCLI-MDPELD)                 
         DC    AL1(GCTIOVER+GCTIELEM,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SI',C'SE',C'SK',C'1C',C'SX'                              
         DC    C'SP',C'SQ',C'SU',C'SS',C'ST',C'SV'                              
GCLI2LQ  EQU   *-GCTCLI2                                                        
*        -----------------------------------                                    
GCTCLI3  DC    AL1(GCLI3LQ,GCCLI,L'MX@CLINT,L'CPJCLI)    CLIENT                 
         DC    AL2(MX@CLINT-OVERWRKD),AL1(CPJELQ,CPJCLI-CPJELD)                 
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0,0,0)                            
         DC    AL1(CPJTJOB,0),AL2(0)                                            
         DC    C'SR',C'SI',C'SE',C'SK',C'1C',C'SX'                              
         DC    C'SP',C'SQ',C'SU',C'SS',C'ST',C'SV'                              
GCLI3LQ  EQU   *-GCTCLI3                                                        
*        -----------------------------------                                    
GCTCLI4  DC    AL1(GCLI4LQ,GCCLI,L'MX@CLINT,L'SORMCLI)   CLIENT                 
         DC    AL2(MX@CLINT-OVERWRKD),AL1(SORELQ,SORMCLI-SORELD)                
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0,0,0)                            
         DC    AL1(SORSMED,0),AL2(0)                                            
         DC    C'SR',C'SI',C'SE',C'SK',C'1C',C'SX'                              
         DC    C'SP',C'SQ',C'SU',C'SS',C'ST',C'SV'                              
GCLI4LQ  EQU   *-GCTCLI4                                                        
*        -----------------------------------                                    
GCTCLI5  DC    AL1(GCLI5LQ,GCCLI,L'MX@CLINT,L'FFTCLAC)   CLIENT                 
         DC    AL2(MX@CLINT-OVERWRKD),AL1(FFTELQ,FFTCLAC-FFTELD)                
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0,0,0)                            
         DC    AL1(FFTTCLPR,0),AL2(0)                                           
         DC    C'SR',C'SI',C'SE',C'SK',C'1C',C'SX'                              
         DC    C'SP',C'SQ',C'SU',C'SS',C'ST',C'SV'                              
GCLI5LQ  EQU   *-GCTCLI5                                                        
*        -----------------------------------                                    
GCTCLI6  DC    AL1(GCLI6LQ,GCCLI,L'MX@CLINT,3)           CLIENT                 
         DC    AL2(MX@CLINT-OVERWRKD),AL1(PCIELQ,PCIPRJT+3-PCIELD)              
         DC    AL1(GCTIOVER+GCTIELEM,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SI',C'SE',C'SK',C'1C',C'SX'                              
         DC    C'SP',C'SQ',C'SU',C'SS',C'ST',C'SV'                              
GCLI6LQ  EQU   *-GCTCLI6                                                        
*        -----------------------------------                                    
GCTCLI7  DC    AL1(GCLI7LQ,GCCLI,L'MX@CLINT,0)           CLIENT                 
         DC    AL2(MX@CLINT-OVERWRKD),AL2(ASPCGR-OVERWRKD)                      
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SE'                                                            
GCLI7LQ  EQU   *-GCTCLI7                                                        
*        -----------------------------------                                    
GCTCLI8  DC    AL1(GCLI8LQ,GCCLI,L'MX@CLINT,3)           CLIENT                 
         DC    AL2(MX@CLINT-OVERWRKD),AL2(TSDSRCAC+2-TSARDATD)                  
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SI',C'SE',C'SK',C'1C',C'SX'                              
         DC    C'SP',C'SQ',C'SU',C'SS',C'ST',C'SV'                              
GCLI8LQ  EQU   *-GCTCLI8                                                        
*        -----------------------------------                                    
GCTPRO1  DC    AL1(GPRO1LQ,GCPRO,L'MX@PROL,L'MDTPRD)     PRODUCT                
         DC    AL2(MX@PROL-OVERWRKD),AL1(MDTELQ,MDTPRD-MDTELD)                  
         DC    AL1(GCTIOVER+GCTIELEM,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SI',C'SE',C'SK',C'1C',C'SX'                              
         DC    C'SP',C'SQ',C'SU',C'SS',C'ST',C'SV'                              
GPRO1LQ  EQU   *-GCTPRO1                                                        
*        -----------------------------------                                    
GCTPRO2  DC    AL1(GPRO2LQ,GCPRO,L'MX@PROL,L'MDPPRD)     PRODUCT                
         DC    AL2(MX@PROL-OVERWRKD),AL1(MDPELQ,MDPPRD-MDPELD)                  
         DC    AL1(GCTIOVER+GCTIELEM,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SI',C'SE',C'SK',C'1C',C'SX'                              
         DC    C'SP',C'SQ',C'SU',C'SS',C'ST',C'SV'                              
GPRO2LQ  EQU   *-GCTPRO2                                                        
*        -----------------------------------                                    
GCTPRO3  DC    AL1(GPRO3LQ,GCPRO,L'MX@PRO,L'FFTPRAC)     PRODUCT                
         DC    AL2(MX@PRO-OVERWRKD),AL1(FFTELQ,FFTPRAC-FFTELD)                  
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0,0,0)                            
         DC    AL1(FFTTCLPR,0),AL2(0)                                           
         DC    C'SR',C'SI',C'SE',C'SK',C'1C',C'SX'                              
         DC    C'SP',C'SQ',C'SU',C'SS',C'ST',C'SV'                              
GPRO3LQ  EQU   *-GCTPRO3                                                        
*        -----------------------------------                                    
GCTPRO4  DC    AL1(GPRO4LQ,GCPRO,L'MX@PRO,L'CPJPRO)      PRODUCT                
         DC    AL2(MX@PRO-OVERWRKD),AL1(CPJELQ,CPJPRO-CPJELD)                   
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0,0,0)                            
         DC    AL1(CPJTJOB,0),AL2(0)                                            
         DC    C'SR',C'SI',C'SE',C'SK',C'1C',C'SX'                              
         DC    C'SP',C'SQ',C'SU',C'SS',C'ST',C'SV'                              
GPRO4LQ  EQU   *-GCTPRO4                                                        
*        -----------------------------------                                    
GCTPRO5  DC    AL1(GPRO5LQ,GCPRO,L'MX@PRO,3)             PRODUCT                
         DC    AL2(MX@PRO-OVERWRKD),AL2(TSDSRCAC+5-TSARDATD)                    
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SI',C'SE',C'SK',C'1C',C'SX'                              
         DC    C'SP',C'SQ',C'SU',C'SS',C'ST',C'SV'                              
GPRO5LQ  EQU   *-GCTPRO5                                                        
*        -----------------------------------                                    
GCTJOB1  DC    AL1(GJOB1LQ,GCJOB,L'MX@JOB,L'MDTJOB)      JOB                    
         DC    AL2(MX@JOEST-OVERWRKD),AL1(MDTELQ,MDTJOB-MDTELD)                 
         DC    AL1(GCTIOVER+GCTIELEM,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SV',C'SW',C'SI',C'SK',C'1C'                              
         DC    C'SX',C'SE'                                                      
GJOB1LQ  EQU   *-GCTJOB1                                                        
*        -----------------------------------                                    
GCTJOB2  DC    AL1(GJOB2LQ,GCJOB,L'MX@JOB,L'MDPJOB)      JOB                    
         DC    AL2(MX@JOEST-OVERWRKD),AL1(MDPELQ,MDPJOB-MDPELD)                 
         DC    AL1(GCTIOVER+GCTIELEM,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SV',C'SW',C'SI',C'SK',C'1C'                              
         DC    C'SX',C'SE'                                                      
GJOB2LQ  EQU   *-GCTJOB2                                                        
*        -----------------------------------                                    
GCTJOB3  DC    AL1(GJOB3LQ,GCJOB,L'MX@JOB,L'CPJPRO)      JOB                    
         DC    AL2(MX@JOEST-OVERWRKD),AL1(CPJELQ,CPJJOB-CPJELD)                 
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0,0,0)                            
         DC    AL1(CPJTJOB,0),AL2(0)                                            
         DC    C'SR',C'SV',C'SW',C'SI',C'SK',C'1C'                              
         DC    C'SX',C'SE'                                                      
GJOB3LQ  EQU   *-GCTJOB3                                                        
*        -----------------------------------                                    
GCTJOB4  DC    AL1(GJOB4LQ,GCJOB,L'MX@JOB,6)             JOB                    
         DC    AL2(MX@JOEST-OVERWRKD),AL2(TSDSRCAC+8-TSARDATD)                  
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SR',C'SV',C'SW',C'SI',C'SK',C'1C'                              
         DC    C'SX',C'SE'                                                      
         DC    C'SP',C'SQ',C'SS',C'ST',C'SU'                                    
GJOB4LQ  EQU   *-GCTJOB4                                                        
*        -----------------------------------                                    
GCTDISC  DC    AL1(GDISCLQ,GCDISC,L'MX@DISAM,L'SCIAMNT)  DISCOUNT               
         DC    AL2(MX@DISAM-OVERWRKD),AL1(SCIELQ,SCIAMNT-SCIELD)                
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0,GCTFNUM+GCTFRGHT,0)             
         DC    AL1(SCITCDSC,0),AL2(0)                                           
         DC    C'SR',C'SP',C'SQ',C'SS',C'ST'                                    
         DC    C'SU',C'SV',C'SW',C'SX',C'SY'                                    
GDISCLQ  EQU   *-GCTDISC                                                        
*        -----------------------------------                                    
GCTGST   DC    AL1(GGSTLQ,GCGST,L'MX@VAT,L'SCIAMNT)      GST                    
         DC    AL2(MX@VAT-OVERWRKD),AL1(SCIELQ,SCIAMNT-SCIELD)                  
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0)                                
         DC    AL1(GCTFNUM+GCTFRGHT,GCTICAN)                                    
         DC    AL1(SCITTAXP,0),AL2(0)                                           
         DC    C'SR',C'SP',C'SQ',C'SS',C'ST'                                    
         DC    C'SU',C'SV',C'SW',C'SX',C'SY',C'SG'                              
GGSTLQ   EQU   *-GCTGST                                                         
*        -----------------------------------                                    
GCTPST   DC    AL1(GPSTLQ,GCPST,L'MX@PSTR,L'SCIAMNT)      PST                   
         DC    AL2(MX@PSTR-OVERWRKD),AL1(SCIELQ,SCIAMNT-SCIELD)                 
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0)                                
         DC    AL1(GCTFNUM+GCTFRGHT,GCTICAN)                                    
         DC    AL1(SCITTQST,0),AL2(0)                                           
         DC    C'SR',C'SP',C'SQ',C'SS',C'ST'                                    
         DC    C'SU',C'SV',C'SW',C'SX',C'SY',C'SG'                              
GPSTLQ   EQU   *-GCTPST                                                         
*        -----------------------------------                                    
GCTGSTB  DC    AL1(GGSTBLQ,GCGSTB,L'MX@GSTBA,L'SCIAMNT)  GST BASIS              
         DC    AL2(MX@GSTBA-OVERWRKD),AL1(SCIELQ,SCIAMNT-SCIELD)                
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0)                                
         DC    AL1(GCTFNUM+GCTFRGHT,0)                                          
         DC    AL1(SCITGLEV,0),AL2(0)                                           
         DC    C'SG'                                                            
GGSTBLQ  EQU   *-GCTGSTB                                                        
*        -----------------------------------                                    
GCTGRS1  DC    AL1(GGRS1LQ,GCGROSS,L'MX@GRAMT,L'MDTGRS)           GROSS         
         DC    AL2(MX@GRAMT-OVERWRKD),AL1(MDTELQ,MDTGRS-MDTELD)                 
         DC    AL1(GCTIOVER+GCTIELEM,GCTIBIN,GCTFNUM+GCTFRGHT,0)                
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SP',C'SQ',C'SS',C'ST',C'SU',C'SV',C'SW',C'SX',C'SY'            
GGRS1LQ  EQU   *-GCTGRS1                                                        
*        -----------------------------------                                    
GCTGROS  DC    AL1(GGROSLQ,GCGROSS,L'MX@GRAMT,L'SCIAMNT)          GROSS         
         DC    AL2(MX@GRAMT-OVERWRKD),AL1(SCIELQ,SCIAMNT-SCIELD)                
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0)                                
         DC    AL1(GCTFNUM+GCTFRGHT,0)                                          
         DC    AL1(SCITGRSS,0),AL2(0)                                           
         DC    C'SP',C'SQ',C'SS',C'ST',C'SU',C'SV',C'SW',C'SX',C'SY'            
         DC    C'SI'                                                            
GGROSLQ  EQU   *-GCTGROS                                                        
*        -----------------------------------                                    
GCTSC01  DC    AL1(GSC01LQ,GCREC,L'MX@RCND,TRNSBREC)     RECONCILED             
         DC    AL2(MX@RCND-OVERWRKD,TSDTEST-TSARDATD)                           
         DC    AL1(GCTIOVER,GCTIYON,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SC'                                                            
GSC01LQ  EQU   *-GCTSC01                                                        
*        -----------------------------------                                    
GCTSC02  DC    AL1(GSC02LQ,GCRECDTE,L'MX@RCND+L'MX@DATE,1) RECON DATE           
         DC    AL2(MX@RCND-OVERWRKD,TSDRCDAT-TSARDATD)                          
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SC'                                                            
GSC02LQ  EQU   *-GCTSC02                                                        
*        -----------------------------------                                    
GCTSC03  DC    AL1(GSC03LQ,GCSTDTE,L'MX@STMD2,2)       STATEMENT DATE           
         DC    AL2(MX@STMD2-OVERWRKD,TSDSTDAT-TSARDATD)                         
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SC'                                                            
GSC03LQ  EQU   *-GCTSC03                                                        
*        -----------------------------------                                    
GCTSC04  DC    AL1(GSC04LQ,GCCLDTE,L'MX@CLRDA,1)       CLEARED DATE             
         DC    AL2(MX@CLRDA-OVERWRKD,TSDCLDAT-TSARDATD)                         
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SC'                                                            
GSC04LQ  EQU   *-GCTSC04                                                        
*        -----------------------------------                                    
GCTPID   DC    AL1(GPIDLQ,GCPRSN,L'MX@PID,0)      PID  PERSON ID NUMBER         
         DC    AL2(MX@PID-OVERWRKD),AL2(ASPCGR-OVERWRKD)                        
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
GPIDLQ   EQU   *-GCTPID                                                         
*        -----------------------------------                                    
GCTSEQU  DC    AL1(GSEQULQ,GCTSEQ,L'MX@SEQ,1)          SEQUENCE NUMBER          
         DC    AL2(MX@SEQ-OVERWRKD,TSDSBREF-TSARDATD)                           
         DC    AL1(GCTIOVER,GCTIBIN,GCTFNUM+GCTFRGHT,0)                         
         DC    AL1(0,0),AL2(0)                                                  
GSEQULQ  EQU   *-GCTSEQU                                                        
*        -----------------------------------                                    
GCTCOL09 DC    AL1(GCT09LQ,GCNAR,L'LC@RSNAR,0)          NARRATIVE               
         DC    AL2(LC@RSNAR-WORKD),AL1(ELETSARQ,NARREC-NARELD)                  
         DC    AL1(GCTIELEM+GCTIELST,0,GCTFSIZ,GCTIEVL)                         
         DC    AL1(NARELQ,0),AL2(0)                                             
GCT09LQ  EQU   *-GCTCOL09                                                       
*        -----------------------------------                                    
GCTDD02  DC    AL1(GDD02LQ,GCTRNST,L'LC@STT,L'TSDTEST) TRN STAT                 
         DC    AL2(LC@STT-WORKD,TSDTEST-TSARDATD)                               
         DC    AL1(0,GCTIHEX+GCTIDDS,0,0)                                       
         DC    AL1(0,0),AL2(0)                                                  
GDD02LQ  EQU   *-GCTDD02                                                        
*        -----------------------------------                                    
GCTDD03  DC    AL1(GDD03LQ,GCDADDR,L'MX@DSCAD,L'TSDDA) DISK ADDRESS             
         DC    AL2(MX@DSCAD-OVERWRKD,TSDDA-TSARDATD)                            
         DC    AL1(GCTIOVER,GCTIHEX++GCTIDDS,0,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GDD03LQ  EQU   *-GCTDD03                                                        
*        -----------------------------------                                    
GCTCOL15 DC    AL1(GCT15LQ,GCPEEDTE,L'MX@PELED,2)       PEELED DATE             
         DC    AL2(MX@PELED-OVERWRKD,TSDPEDAT-TSARDATD)                         
         DC    AL1(GCTIOVER,GCTIDDS,GCTFDAT+GCTFRGHT,0)                         
         DC    AL1(0,0),AL2(0)                                                  
GCT15LQ  EQU   *-GCTCOL15                                                       
*        -----------------------------------                                    
GCTAPEL  DC    AL1(GAPELLQ,GCAPE,L'MX@ATTRS,0)        ANALYSIS POINTER          
         DC    AL2(MX@ATTRS-OVERWRKD),AL2(ASPCGR-OVERWRKD)                      
         DC    AL1(GCTIOVER+GCTIROUT,0,GCTFSIZ,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GAPELLQ  EQU   *-GCTAPEL                                                        
*        -----------------------------------                                    
GCTDRAFT DC    AL1(GDRAFLQ,GCDRAFT,L'MX@DRAFT,TRNSDRFT)  DRAFT                  
         DC    AL2(MX@DRAFT-OVERWRKD,TSDTRST-TSARDATD)                          
         DC    AL1(GCTIOVER,GCTIYON,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
GDRAFLQ  EQU   *-GCTDRAFT                                                       
*        -----------------------------------                                    
GCTCOL07 DC    AL1(GCT07LQ,GCDRS,L'LC@DRS,L'TSDATAMT)    DEBITS                 
         DC    AL2(LC@DRS-WORKD,TSDATAMT-TSARDATD)                              
         DC    AL1(GCTITOT,GCTIDR,GCTFNUM+GCTFRGHT,0)                           
         DC    AL1(TSDTEST-TSDATAMT,L'TSTTDR),AL2(TSTTDR-TSARTOTD)              
GCT07LQ  EQU   *-GCTCOL07                                                       
*        -----------------------------------                                    
GCTCOL08 DC    AL1(GCT08LQ,GCCRS,L'LC@CRS,L'TSDATAMT)    CREDITS                
         DC    AL2(LC@CRS-WORKD,TSDATAMT-TSARDATD)                              
         DC    AL1(GCTITOT,GCTICR,GCTFNUM+GCTFRGHT,0)                           
         DC    AL1(TSDTEST-TSDATAMT,L'TSTTCR),AL2(TSTTCR-TSARTOTD)              
GCT08LQ  EQU   *-GCTCOL08                                                       
*        -----------------------------------                                    
GCTABAL  DC    AL1(GABALLQ,GCABAL,L'MX@BAL,0)            BALANCE                
         DC    AL2(MX@BAL-OVERWRKD,0)                                           
         DC    AL1(GCTITOT+GCTIOVER,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,L'TSTTTOT),AL2(TSTTTOT-TSARTOTD)                           
GABALLQ  EQU   *-GCTABAL                                                        
*        -----------------------------------                                    
GCTINVN  DC    AL1(GINVNLQ,GCINVN,L'MX@INVCN,0)              INVOICE            
         DC    AL2(MX@INVCN-OVERWRKD),AL1(FFTELQ,FFTDATA-FFTELD)                
         DC    AL1(GCTIOVER+GCTIELEM+GCTIELST,0,0,GCTIEVL)                      
         DC    AL1(FFTTINVN,0),AL2(0)                                           
GINVNLQ  EQU   *-GCTINVN                                                        
GCTINV2  DC    AL1(GINV2LQ,GCINVN,L'MX@INVCN,L'XPYINV)                          
         DC    AL2(MX@INVCN-OVERWRKD),AL1(XPYELQ,XPYINV-XPYELD)                 
         DC    AL1(GCTIOVER+GCTIELEM,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
GINV2LQ  EQU   *-GCTINV2                                                        
GCTINV3  DC    AL1(GINV3LQ,GCINVN,L'MX@INVCN,L'TSDTREF)                         
         DC    AL2(MX@INVCN-OVERWRKD,TSDTREF-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GINV3LQ  EQU   *-GCTINV3                                                        
*        -----------------------------------                                    
GCTKREF  DC    AL1(GCTKRFLQ,GCKREF,L'MX@KEYRF,L'TSDKREF) TRANS KEY REF          
         DC    AL2(MX@KEYRF-OVERWRKD,TSDKREF-TSARDATD)                          
         DC    AL1(GCTIOVER,GCTIDDS,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
GCTKRFLQ EQU   *-GCTKREF                                                        
*        -----------------------------------                                    
GCTBDAT  DC    AL1(GCTBDLQ,GCBLBDT,L'MX@BLBDT,1)         BILLABLE DATE          
         DC    AL2(MX@BLBDT-OVERWRKD,TSDBLBDT-TSARDATD)                         
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SP',C'SQ'                                                      
GCTBDLQ  EQU   *-GCTBDAT                                                        
*        -----------------------------------                                    
GCTCURC  DC    AL1(GCTCRCLQ,GCCURC,L'MX@CURC,L'TSDTACUR) CURRENCY CODE          
         DC    AL2(MX@CURC-OVERWRKD,TSDTACUR-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GCTCRCLQ EQU   *-GCTCURC                                                        
*        ----------------------------------MEDIA FILE (AGENCY ALPHA ID)         
GCTMFILE DC    AL1(GDRMFILQ,GCMFILE,L'MX@MFILE,L'TSDMFILE)                      
         DC    AL2(MX@MFILE-OVERWRKD,TSDMFILE-TSARDATD)                         
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SC'                                      CASH ACCOUNT          
         DC    C'SP',C'SQ',C'SS',C'ST',C'SU',C'SV',C'SW',C'SX',C'SY'            
         DC    C'SR'                                      RECEIVABLE            
GDRMFILQ EQU   *-GCTMFILE                                                       
*        -----------------------------------                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*---------------------------------------------------------------------*         
* GRID COLUMN EQUATES                                                 *         
*---------------------------------------------------------------------*         
GCCONT   EQU   99     CONTRA ACCOUNT                                            
GCDATE   EQU   1      DATE                                                      
GCREF    EQU   2      REFERENCE                                                 
GCOFF    EQU   3      OFFICE                                                    
GCDRS    EQU   4      DEBITS                                                    
GCCRS    EQU   5      CREDITS                                                   
GCNAR    EQU   6      NARRATIVE                                                 
GCBATR   EQU   7      BATCH REFERENCE                                           
GCTRNTYP EQU   8      TRANSACTION TYPE                                          
GCTSEQ   EQU   9      TRANSACTION SEQUENCE                                      
GCACTDTE EQU   10     ACTIVITY DATE                                             
GCREVDTE EQU   11     REVERSAL DATE                                             
GCAPPR   EQU   12     APPROVED                                                  
GCPEEDTE EQU   13     PEELED DATE                                               
GCURG    EQU   14     URGENT                                                    
GCDRAFT  EQU   15     DRAFT                                                     
GCHOUR   EQU   16     HOURS                                                     
GCCHKNO  EQU   17     CHECK NUMBER                                              
GCCHKDTE EQU   18     CHECK DATE                                                
GCDEPDTE EQU   19     DEPOSIT DATE                                              
GCCHKTOT EQU   20     CHECK TOTAL                                               
GCOFFSET EQU   21     OFFSET                                                    
GCCLI    EQU   22     CLIENT                                                    
GCPRO    EQU   23     PRODUCT                                                   
GCJOB    EQU   24     JOB                                                       
GCBANK   EQU   25     BANK                                                      
GCDUE    EQU   26     DUE DATE                                                  
GCHELD   EQU   27     HELD                                                      
GCDISC   EQU   28     CASH DISCOUNT                                             
GCMOA    EQU   29     MOA                                                       
GCMMOS   EQU   30     MONTH OF SERVICE                                          
GCVOID   EQU   31     VOID                                                      
GCTIMT   EQU   32     TIME TYPE                                                 
GCAPE    EQU   33     ANALYSIS POINTER                                          
GCGST    EQU   34     GST                                                       
GCGSTB   EQU   35     GST BASIS                                                 
GCPST    EQU   36     PST                                                       
GCABAL   EQU   37     ACCOUNT BALANCE                                           
GCAMNT   EQU   38     AMOUNT                                                    
GCDRCR   EQU   39     DR/CR                                                     
GCINVN   EQU   40     INVOICE NUMBER                                            
GCPYMET  EQU   41     PAYMENT METHOD     CHECK/EFT/PCARD                        
GCBLBDT  EQU   42     BILLABLE DATE                                             
GCREC    EQU   71     RECONCILED                                                
GCRECDTE EQU   72     RECONCILED DATE                                           
GCSTDTE  EQU   73     STATEMENT DATE                                            
GCCLDTE  EQU   74     CLEARED DATE                                              
GCGROSS  EQU   75     GROSS                                                     
GCPRSN   EQU   77     PERSON                                                    
GCMETH   EQU   78     APPROVAL METHOD                                           
GCCURC   EQU   79     CURRENCY CODE                                             
GCMFILE  EQU   80     MEDIA FILE (AGENCY ALPHA ID)                              
*-----------------------------------                                            
* DDS ONLY                                                                      
*-----------------------------------                                            
GCUSDTE  EQU   81     USED DATE                                                 
GCTRNST  EQU   82     TRANSACTION STATUS                                        
GCDADDR  EQU   83     DISK ADDRESS                                              
GCKREF   EQU   84     KEY REFERENCE  NOT ADJUSTED FOR 255+                      
         EJECT                                                                  
***********************************************************************         
*        DISPLAY DIFFERENCE COLUMN FOR GRIDS                          *         
*         R3 = ADDRESS OF GRID TAB ENTRY                                        
***********************************************************************         
         USING GCTBLD,R3                                                        
         USING TSARRECD,R4                                                      
SPCGR    NTR1  BASE=*,LABEL=*                                                   
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)               R8=LOCAL WRKING STR                   
         LHI   R1,1                                                             
         NI    DETFLAG,X'FF'-DETREADQ     DO NOT RE-READ RECORD                 
         XC    TEMP,TEMP                                                        
         MVI   TEMP,C' '                                                        
         L     R4,ATSARREC                R2=A(TSAR RECORD)                     
         LA    R4,TSARDATA                                                      
         USING TSARDATD,R4                                                      
*                                                                               
         CLI   GCTCOID,GCDRCR             DR/CR COLUMN                          
         BE    SGR10                                                            
         CLI   GCTCOID,GCTIMT             TIME TYPE                             
         BE    SGR20                                                            
         CLI   GCTCOID,GCAPE              ANALYSIS POINTER                      
         BE    SGR30                                                            
         CLI   GCTCOID,GCCLI              CLIENT                                
         BE    SGR40                                                            
         CLI   GCTCOID,GCPRSN             PERSON                                
         BE    SGR50                                                            
         CLI   GCTCOID,GCMMOS             MONTH OF SERVICE                      
         BE    SGR70                                                            
         CLI   GCTCOID,GCPYMET            PAYMENT METHOD                        
         BE    SGR80                                                            
*        CLI   GCTCOID,GCMMOS             MONTH OF SERVICE                      
*        BE    SGR90                                                            
         CLI   GCTCOID,GCMETH             APPROVAL METHOD                       
         BE    SGR100                                                           
         B     SGREX                                                            
*-----------------------------------------                                      
* DEBIT OR CREDIT                                                               
*-----------------------------------------                                      
SGR10    MVC   TEMP(2),UP@CRS                                                   
         TM    TSDTEST,TRNSDR                                                   
         BZ    *+10                                                             
         MVC   TEMP(2),UP@DRS                                                   
         LHI   R1,2                                                             
         B     SGRX                                                             
*-----------------------------------------                                      
* TIME TYPE                                                                     
*-----------------------------------------                                      
         USING PRTELD,R2                                                        
SGR20    ICM   R2,15,APRTELD              THEN PERSONNEL RATE REQUIRED          
         BZ    SGREX                                                            
         TM    PRTSTAT,PRTSBILQ+PRTSNOTQ+PRTSRTEQ                               
         BZ    SGREX                                                            
         TM    PRTSTAT,PRTSBILQ           BILLABLE TIME?                        
         BZ    *+12                                                             
         MVI   TEMP,C'B'                                                        
         B     SGRX                                                             
         TM    PRTSTAT,PRTSNOTQ           NON-BILLABLE TIME?                    
         BZ    *+12                                                             
         MVI   TEMP,C'N'                                                        
         B     SGRX                                                             
         MVI   TEMP,C'R'                  ASSUME SPECIAL NON-BILL TIME          
         B     SGRX                                                             
         DROP  R2                                                               
*-----------------------------------------                                      
* ANALYSIS POINTER ELEMENT                                                      
*-----------------------------------------                                      
         USING APEELD,R2                                                        
SGR30    ICM   R2,15,AAPEELD              ANALYSIS POINTER ELEMENT?             
         BZ    SGREX                                                            
         SR    R0,R0                                                            
         MVI   TEMP,C' '                  < USE TEMP AND WORK                   
         MVC   TEMP+1(L'TEMP-1),TEMP      < TO BUILD ATTRIBUTE                  
         LA    RE,TEMP                                                          
*                                                                               
         CLI   APELN,APELN1Q                                                    
         BNH   SGREX                                                            
         ICM   R0,1,APENUM                NUMBER OF SUB ELEMENTS                
         BZ    SGREX                                                            
         LA    R5,APENTRY                 R4=A(FIRST SUB ELEMENT)               
         USING APENTRY,R5                                                       
*                                                                               
SPCGR10  SR    R6,R6                                                            
         IC    R6,APENLEN                 R6=L'(SUB ELEMENT)                    
         SHI   R6,APELN2Q                                                       
         BNP   SGREX                      ERROR IF NO ACCOUNT                   
         LA    RF,TEMP                    CHECK IF IT FITS                      
         LA    R1,0(R6,RE)                                                      
         SR    R1,RF                                                            
         CHI   R1,L'TEMP                  NEW LENGTH BIGGER THEN TEMP?          
         BH    SPCGR19                    . YES, WE CAN'T FIT IT                
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),APENACT            GET ACCOUNT CODE                      
*                                                                               
         LA    RE,2(R6,RE)                BUMP RF TO NEXT POSITION              
         SR    R6,R6                                                            
         IC    R6,APENLEN                                                       
         AR    R5,R6                      BUMP R4 TO NEXT SUB ELEMENT           
         BCT   R0,SPCGR10                                                       
*                                                                               
SPCGR19  LA    RF,TEMP                                                          
         SR    RE,RF                                                            
         BNP   SGREX                                                            
         LR    R1,RE                                                            
         B     SGRX                                                             
*-----------------------------------------                                      
* CLIENT                                                                        
*-----------------------------------------                                      
SGR40    CLC   SPROUL,TSDCONT                                                   
         BNE   SGREX                                                            
         MVC   TEMP(L'MDTCLI),TSDCONT+2   GET CLIENT AFTER U/L                  
         LHI   R1,L'MDTCLI                                                      
         B     SGRX                                                             
*-----------------------------------------                                      
* PERSON IDENTIFICATION ELEMENT                                                 
*-----------------------------------------                                      
         USING PIDELD,R2                                                        
SGR50    ICM   R2,15,APIDELD              PERSON ID ELEMENT                     
         BZ    SGREX                                                            
*                                                                               
         LHI   R5,PIDMAXQ                 FIRST CHECK PID TABLE                 
         LA    R1,PIDTAB                                                        
SGR51    CLC   0(L'PIDNO,R1),PIDNO        MATCHED HEX?                          
         BNE   *+14                       . NO                                  
         MVC   TEMP(L'SAPALPID),2(R1)     . YES, GET THE TEXT                   
         B     SGR66                                                            
         LA    R1,L'PIDTAB(R1)                                                  
         BCT   R5,SGR51                                                         
         OI    DETFLAG,DETREADQ           RE-READ RECORD WHEN DONE              
*                                                                               
         MVC   KEYSAVE,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         XC    SVAGY,SVAGY                                                      
         XC    TEMPAGY,TEMPAGY                                                  
*                                                                               
         OC    PIDAGY,PIDAGY                                                    
         BZ    SGR52                                                            
         MVC   TEMPAGY,PIDAGY      SAVE PIDAGY                                  
         B     SGR53                                                            
*                                                                               
         USING XPYELD,R1                                                        
SGR52    ICM   R1,15,AXPYELD              GET CLEARING AGENCY                   
         BZ    SGR56                                                            
         CLC   XPYAGY,SPACES              MAKE SURE IT EXISTS                   
         BNH   SGR56                                                            
         MVC   TEMPAGY,XPYAGY             SAVE AGENCY                           
*                                                                               
         USING CT5REC,R6                                                        
SGR53    LA    R6,IOKEY                                                         
         MVI   CT5KTYP,CT5KTYPQ           GET OVERRIDE'S SEC AGY                
         MVC   CT5KALPH,TEMPAGY                                                 
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI ',=C'CTFILE ',IOKEY,AIO2                 
         BNE   SGR56                                                            
         L     R6,AIO2                                                          
         CLC   IOKEY(L'CT5KEY),0(R6)                                            
         BNE   SGR56                      NOT FOUND                             
         LA    R6,CT5DATA                                                       
*                                                                               
         USING SAACTD,R6                                                        
SGR54    CLI   SAACTEL,EOR                                                      
         BE    SGR56                      NOT FOUND                             
         CLI   SAACTEL,SAACTELQ           X'B8' - ACTION ELEM                   
         BE    SGR55                                                            
         SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         AR    R6,RF                                                            
         B     SGR54                                                            
*                                                                               
SGR55    MVC   SVAGY,SAACTWRD             OVERRIDE'S SEC AGY                    
*                                                                               
         USING SA0REC,R6                                                        
SGR56    LA    R6,IOKEY                   NOT IN TABLE, SO READ FOR IT          
         XC    IOKEY,IOKEY                                                      
         MVI   SA0KTYP,SA0KTYPQ           PERSONAL AUTH. RECORD                 
*                                                                               
         CLC   SVAGY,SPACES                                                     
         BNH   *+14                                                             
         MVC   SA0KAGY,SVAGY                                                    
         B     SGR58                                                            
*                                                                               
         OC    TEMPAGY,TEMPAGY                                                  
         BZ    SGR57                                                            
         MVC   SA0KAGY,TEMPAGY                                                  
         B     SGR58                                                            
*                                                                               
SGR57    MVC   SA0KAGY,AGYSEC             ALPHA SECURITY AGENCY CODE            
         CLC   AGYSEC,SPACES              WASN'T SURE IF ALWAYS SET             
         BH    *+10                                                             
         MVC   SA0KAGY,TWAAGY             SECURITY ALPHA ID                     
*                                                                               
SGR58    MVC   SA0KNUM,PIDNO              PID HEX NUMBER                        
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI ',=C'CTFILE ',IOKEY,AIO2                 
         BNE   SGR65                      NOT FOUND, ADD WITH SPACES            
         L     R6,AIO2                                                          
         CLC   IOKEY(L'SA0KEY),0(R6)                                            
         BNE   SGR65                      NOT FOUND, ADD WITH SPACES            
*                                                                               
         LA    R6,SA0DATA                                                       
         USING SAPALD,R6                                                        
SGR60    CLI   SAPALEL,EOR                                                      
         BE    SGR65                      NOT FOUND, ADD WITH SPACES            
         CLI   SAPALEL,SAPALELQ           X'C3' - PERSONAL ID ELEM              
         BE    SGR62                                                            
         SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         AR    R6,RF                                                            
         B     SGR60                                                            
SGR62    MVC   TEMP(L'SAPALPID),SAPALPID  PUT TEXT ID ON OUTPUT LINE            
         B     *+10                                                             
SGR65    MVC   TEMP(L'SAPALPID),SPACES    PUT TEXT ID ON OUTPUT LINE            
*                                                                               
         SR    R1,R1                                                            
         IC    R1,PIDTABN                 SPOT IN PID SAVE TABLE                
         MHI   R1,L'PIDTAB                                                      
         LA    RF,PIDTAB                                                        
         AR    RF,R1                                                            
         MVC   0(L'PIDNO,RF),PIDNO        SAVE HEX NUMBER                       
         MVC   2(L'SAPALPID,RF),SAPALPID  SAVE TEXT                             
         SR    R1,R1                                                            
         IC    R1,PIDTABN                                                       
         CHI   R1,PIDMAXQ                 MAX ENTRIES IN PID TABLE?             
         BNE   *+12                       . NO, POINT TO NEXT SPOT              
         MVI   PIDTABN,0                  . YES, START TO OVERWRITE             
         B     *+12                                                             
         AHI   R1,1                                                             
         STC   R1,PIDTABN                                                       
*                                                                               
SGR66    LHI   R1,L'SAPALPID              LENGTH OF PID TEXT                    
         TM    DETFLAG,DETREADQ           RE-READ RECORD?                       
         BZ    SGRX                       . NO                                  
         MVC   IOKEY,KEYSAVE                                                    
         GOTO1 AIO,IOREAD+IOACCDIR+IO1    RESTORE READ SEQUENCE                 
         BE    SGRX                                                             
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         B     SGRX                                                             
*-----------------------------------------                                      
* APPROVAL DATE                                                                 
*-----------------------------------------                                      
         USING MDPELD,R2                                                        
SGR70    ICM   R2,15,AMDPELD              APPROVAL DATE                         
         BZ    SGR90                                                            
         OC    MDPMOS,MDPMOS                                                    
         BZ    SGREX                                                            
         CLI   MDPMOS,C' '                                                      
         BNH   SGREX                                                            
         CLI   MDPMOS,C'0'                                                      
         BNL   SGREX                                                            
         MVC   DUB(L'MDPMOS),MDPMOS                                             
         MVI   DUB+L'MDPMOS,1                                                   
         GOTO1 VDATCON,DMCB,(1,DUB),(6,TEMP) MONTH OF SERVICE                   
         CLI   MDPMOS+1,X'13'      SPECIAL 13TH MONTH?                          
         BNE   SGR95               NO                                           
         MVC   TEMP(3),=C' 13'     YES, CHANGE MONTH                            
         B     SGRX                                                             
*-----------------------------------------                                      
* PAYMENT METHOD CHECK/EFT/PCARD                                                
*-----------------------------------------                                      
SGR80    DS    0H                                                               
         CLI   TSDPYMET,TSDNOCHK   NO CHECK INFO POSTED/CHECK NOT CUR.          
         BE    SGRX                                                             
         MVC   TEMP(3),=CL3'EFT'                                                
         TM    TSDPYMET,TSDEFT     EFT RUN                                      
         BO    SGR85                                                            
         MVC   TEMP(5),=CL5'CHECK'                                              
         TM    TSDPYMET,TSDCHK                                                  
         BO    SGR85                                                            
         MVC   TEMP(5),=CL5'PCARD'                                              
         TM    TSDPYMET,TSDPCRD                                                 
         BO    SGR85                                                            
         MVC   TEMP(5),=CL5'CCARD'                                              
         TM    TSDPYMET,TSDCCRD                                                 
         BZ    SGRX                                                             
*                                                                               
SGR85    LHI   R1,5                                                             
         B     SGRX                                                             
*-----------------------------------------                                      
* PAYMENT METHOD CHECK OR EFT                                                   
*-----------------------------------------                                      
         USING MDTELD,R2                                                        
SGR90    ICM   R2,15,AMDTELD              MEDIA TRANSFER ELEMENT                
         BZ    SGREX                                                            
         OC    MDTMOS,MDTMOS                                                    
         BZ    SGREX                                                            
         CLI   MDTMOS,C' '                                                      
         BNH   SGREX                                                            
         CLI   MDTMOS,C'0'                                                      
         BNL   SGREX                                                            
         MVC   DUB(L'MDTMOS),MDTMOS                                             
         MVI   DUB+L'MDTMOS,1                                                   
         GOTO1 VDATCON,DMCB,(1,DUB),(6,TEMP) MONTH OF SERVICE                   
         CLI   MDTMOS+1,X'13'      SPECIAL 13TH MONTH?                          
         BNE   SGR95               NO                                           
         MVC   TEMP(3),=C' 13'     YES, CHANGE MONTH                            
*                                                                               
SGR95    LHI   R1,6                                                             
         B     SGRX                                                             
*-----------------------------------------                                      
* APPROVAL METHOD                                                               
*-----------------------------------------                                      
SGR100   DS    0H                                                               
         CLI   TSDAAPT,0           NO APPROVAL                                  
         BE    SGRX                                                             
         MVC   TEMP(L'MX@MAPP),MX@MAPP-OVERWRKD(R8)                             
         CLI   TSDAAPT,TSDAAM                                                   
         BE    *+10                                                             
         MVC   TEMP(L'MX@AAPP),MX@AAPP-OVERWRKD(R8)                             
         LHI   R1,12                                                            
         B     SGRX                                                             
*-----------------------------------------                                      
SGREX    MVI   TEMP,C' '                                                        
         LHI   R1,1                                                             
SGRX     XIT1  REGS=(R1)                                                        
         LTORG                                                                  
         DROP  R3,R4,RB                                                         
         EJECT                                                                  
***********************************************************************         
*        DISPLAY MOS MONTH OF SERVICE COLUMN FOR GRIDS                *         
*         R3 = ADDRESS OF GRID TAB ENTRY                              *         
***********************************************************************         
*        USING GCTBLD,R3                                                        
*        USING TSARRECD,R4                                                      
*OSGR    NTR1  BASE=*,LABEL=*                                                   
*        LHI   R8,OVERWORK-WORKD                                                
*        LA    R8,WORKD(R8)               R8=LOCAL WRKING STR                   
*        XC    TEMP,TEMP                                                        
*        L     R4,ATSARREC                R2=A(TSAR RECORD)                     
*        LA    R4,TSARDATA                                                      
*        USING TSARDATD,R4                                                      
*                                                                               
*        EJECT                                                                  
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
FSTDIS   NTR1  BASE=*,LABEL=*                                                   
         MVI   DETFLAG,0           INIT DETAIL FLAG                             
         ZAP   TRNTOT,=P'0'        TRANSACTIONS TOTAL                           
         ZAP   TRNTDR,=P'0'        TRANSACTIONS DR TOTAL                        
         ZAP   TRNTCR,=P'0'        TRANSACTIONS CR TOTAL                        
         ZAP   HRSTOT,=P'0'        HOURS TOTAL                                  
         MVI   NXTRMODE,0          NEXT RECODE MODE                             
         MVI   CURRFLAG,0          INIT CURRENCY FLAG                           
         MVI   PIDTABN,0           INIT PID TABLE                               
         MVC   CURRLAST,SPACES     LAST CURRENCY CODE                           
         MVC   FVMSGNO,=AL2(EALDGINV)  FIRST POSSIBLE ERROR (INV LEDG)          
*                                                                               
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         CLI   OTAXALL,C'Y'        BOTH GST AND PST REQUIRED?                   
         BE    FSTD04                                                           
         CLI   OHOURS,C'Y'         OPTION TO OVERRIDE COL ALREADY SET?          
         BE    FSTD04                                                           
         CLI   OBASIS,C'Y'                                                      
         BE    FSTD04                                                           
         CLI   OTAX,C'Y'                                                        
         BE    FSTD04                                                           
         CLI   OPST,C'Y'           PROVINCIAL SALES TAX                         
         BE    FSTD04                                                           
         LA    RE,OTAXALL          RF=A(BOTH GST AND PST)                       
         CLI   PHBGOVER,TXACOLQ    GST AND PST OVERRIDE?                        
         BE    FSTD02                                                           
         LA    RE,OHOURS           RF=A(HOURS OPTION)                           
         CLI   PHBGOVER,HRSCOLQ    HOURS COLUMN OVERRIDE?                       
         BE    FSTD02                                                           
         LA    RE,OBASIS           RE=A(BASIS OPTION)                           
         CLI   PHBGOVER,BASCOLQ    BASIS COLUMN OVERRIDE?                       
         BE    FSTD02                                                           
         LA    RE,OTAX             RE=A(TAX OPTION)                             
         CLI   PHBGOVER,TAXCOLQ    GST COLUMN OVERRIDE?                         
         BE    FSTD02                                                           
         LA    RE,OPST             RE=A(PROVINCIAL SALES TAX OPTION)            
         CLI   PHBGOVER,PSTCOLQ    PST COLUMN OVERRIDE?                         
         BNE   FSTD04                                                           
*                                                                               
FSTD02   CLI   0(RE),C'N'          COLUMN OVERRIDE SET TO NO?                   
         BE    FSTD04                                                           
         MVI   0(RE),C'Y'          SET COLUMN OVERRIDE TO YES                   
*                                                                               
FSTD04   GOTO1 AUNITLDG,SPROUNIT   READ PRODUCTION UNIT/LEDG                    
         CLI   LEDGTLVA,0                                                       
         BE    FSTDERR                                                          
*                                                                               
         MVC   CLEN(L'CLEN+L'CPLEN+L'CPJLEN),LEDGTLVA LEDGER STRUCTURE          
         GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,0,ACOMFACS,(0,0)                  
         LA    R2,BASKEYH          R2=A(KEY FIELD)                              
         USING FLDHDRD,R2                                                       
         ST    R2,FVADDR                                                        
         CLC   FLDDATA(L'SPROUL),SPROUL PROD NOT ALLOWED                        
         BE    FSTDERR                                                          
         GOTO1 AUNITLDG,FLDDATA    READ UNIT/LEDGER RECORDS                     
         BNE   FSTDERR                                                          
*                                                                               
         MVI   SLEDGATR,0                                                       
         CLI   BASKEY,C'S'         SUBSIDIARY UNIT?                             
         BNE   FSTD08                                                           
*                                                                               
         L     RF,ALDGTAB          RF=A(LEDGER ATTRIBUTE TABLE)                 
         USING LEDGTABD,RF                                                      
FSTD06   CLI   LEDGTYPE,EOT        END OF TABLE?                                
         BE    FSTD08                                                           
         CLC   LEDGTYPE,BASKEY+1   MATCH ON LEDGER?                             
         BE    *+12                                                             
         LA    RF,LEDGLNQ(RF)                                                   
         B     FSTD06                                                           
         MVC   SLEDGATR,LEDGATTR   SAVE LEDGER ATTRIBUTES                       
         DROP  RF                                                               
*                                                                               
FSTD08   GOTO1 AGETACC,0                                                        
*        MVC   FVMSGNO,=AL2(EAIFNTFN) ACCOUNT NOT FOUND                         
         BNE   FSTDERR                                                          
*                                                                               
         MVI   SECFFLAG,0          INIT SECURITY FLAG                           
         OC    TWALEN,TWALEN                                                    
         BZ    FSTD12                                                           
         CLC   ONEP,BASKEY                                                      
         BE    FSTD10                                                           
         CLC   ONER,BASKEY                                                      
         BE    FSTD10                                                           
         CLC   ONEJ,BASKEY                                                      
         BNE   FSTD12                                                           
*                                                                               
FSTD10   L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
         GOTO1 VSECRET,DMCB,('SECPFLDP',SECD),=AL1(1)                           
         BNL   FSTD12                                                           
         OI    SECFFLAG,SECFRATE   SET RATE CANNOT BE SHOWN                     
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         MVI   OHOURS-OPTVALSD(RF),C'Y'                                         
*-------------------------                                                      
* DISPLAY SCREEN HEADINGS                                                       
*-------------------------                                                      
FSTD12   TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BZ    FSTD13                                                           
         GOTO1 ADISACC                                                          
         B     FSTD50                                                           
*                                                                               
FSTD13   LA    R2,ENQDAT1H                                                      
         GOTO1 ADISUL              DISPLAY UNIT AND LEDGER NAMES                
*                                                                               
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         LA    R1,OHOURS           MAKE SURE ONLY ONE COL OVERRIDE              
         BAS   RE,CHKCONF          OPTION IS SET                                
         LA    R1,OTAXALL                                                       
         BAS   RE,CHKCONF                                                       
         LA    R1,OTAX                                                          
         BAS   RE,CHKCONF                                                       
         LA    R1,OBASIS                                                        
         BAS   RE,CHKCONF                                                       
         LA    R1,OVALUE                                                        
         BAS   RE,CHKCONF                                                       
         LA    R1,OPST                                                          
         BAS   RE,CHKCONF                                                       
         DROP  RF                                                               
*                                                                               
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         GOTO1 ADISACC                                                          
*                                                                               
         LA    R2,ENQDAT2H-ENQDAT1H(R2)  DISPLAY COLUMN HEADINGS                
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         LA    R3,L'ENQDAT1H(R2)                                                
         USING SCRLIN1D,R3                                                      
         MVC   FLDDATA(L'MX@ENH14),MX@ENH14                                     
         TM    SECFFLAG,SECFRATE   AUTHORISED FOR RATE?                         
         BNO   FSTD15                                                           
         MVC   SCR1NAMT,SPACES                                                  
         LA    RE,SCR1NAMT+(L'SCR1NAMT-L'MX@HOURR)-1                            
         MVC   0(L'MX@HOURR,RE),MX@HOURR                                        
         MVI   OHOURS,0                                                         
         NI    DETFLAG,X'FF'-DETCOLMQ    DO NOT TREAT AS OVERRIDE               
         B     FSTD28                                                           
*                                                                               
FSTD15   CLI   OTAXALL,C'Y'        TAX OPTION CHANGES HEADINGS                  
         BNE   FSTD16                                                           
         MVC   SCR1WDSC,SPACES                                                  
         MVC   SCR1HRT,MX@TAXR                                                  
         B     FSTD26                                                           
*                                                                               
FSTD16   CLI   OBASIS,C'Y'         BASIS OPTION CHANGES HEADINGS                
         BNE   FSTD18                                                           
         MVC   SCR1WDSC,SPACES                                                  
         MVC   SCR1HRT,MX@BASIR                                                 
         B     FSTD26                                                           
*                                                                               
FSTD18   CLI   OTAX,C'Y'           TAX OPTION CHANGES HEADINGS                  
         BNE   FSTD20                                                           
         MVC   SCR1WDSC,SPACES                                                  
         MVC   SCR1HRT,MX@VATR                                                  
         B     FSTD26                                                           
*                                                                               
FSTD20   CLI   OPST,C'Y'           TAX OPTION CHANGES HEADINGS                  
         BNE   FSTD22                                                           
         MVC   SCR1WDSC,SPACES                                                  
         MVC   SCR1HRT,MX@PSTR                                                  
         B     FSTD26                                                           
*                                                                               
FSTD22   CLI   OHOURS,C'Y'         HOURS OPTION CHANGES HEADINGS                
         BNE   FSTD24                                                           
         MVC   SCR1WDSC,SPACES                                                  
         MVC   SCR1HRT,MX@HOURR                                                 
         B     FSTD26                                                           
*                                                                               
FSTD24   CLI   OVALUE,C'B'         LOCAL CURRENCY AND AGENCY CURRENCY?          
         BNE   FSTD28                                                           
         MVC   SCR1WDSC,SPACES                                                  
         MVC   SCR1AMTO-1,MX@LCLCU                                              
         LA    R1,L'SCR1AMTO-1                                                  
         LA    RE,SCR1AMTO-1                                                    
         B     *+12                                                             
*                                                                               
FSTD26   LA    R1,L'SCR1HRT-1                                                   
         LA    RE,SCR1HRT                                                       
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
*                                                                               
FSTD28   OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         LA    R3,ENQDAT2H-ENQDAT1H(R3)                                         
         MVC   FLDDATA(L'MX@ENH15),MX@ENH15                                     
         TM    SECFFLAG,SECFRATE   AUTHORISED FOR RATE?                         
         BNO   FSTD30                                                           
         MVC   SCR1NAMT+L'ENQDAT1H-SCRLIN1D(L'SCR1NAMT,R2),SPACES               
         LA    RE,SCR1NAMT+(L'SCR1NAMT-L'MX@HOURR)-1                            
         MVC   0(L'MX@HOURR,RE),UNDERLIN                                        
         B     FSTD45                                                           
*                                                                               
FSTD30   CLI   OTAXALL,C'Y'                                                     
         BE    FSTD40                                                           
         CLI   OHOURS,C'Y'                                                      
         BE    FSTD40                                                           
         CLI   OBASIS,C'Y'                                                      
         BE    FSTD40                                                           
         CLI   OTAX,C'Y'                                                        
         BE    FSTD40                                                           
         CLI   OPST,C'Y'                                                        
         BE    FSTD40                                                           
         CLI   OVALUE,C'B'         LOCAL CURRENCY AND AGENCY CURRENCY?          
         BNE   FSTD45                                                           
*                                                                               
FSTD40   MVC   SCR1WDSC+L'ENQDAT2H-SCRLIN1D(L'SCR1WDSC,R2),SPACES               
         LA    RE,ENQDAT2H-ENQDAT1H(RE)                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),UNDERLIN                                                 
*                                                                               
FSTD45   OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         DROP  RF,R3                                                            
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         B     FSTD50                                                           
*                                                                               
FSTD50   GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
*                                                                               
         L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
*                                                                               
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         CLI   OALLTRAN,C'Y'       SHOW ALL TRANSACTIONS?                       
         BNE   FSTD55                                                           
*                                                                               
         CLI   ODRAFT,0                                                         
         BNE   *+8                                                              
         MVI   ODRAFT,C'Y'                                                      
         CLI   OREVERSE,0                                                       
         BNE   *+8                                                              
         MVI   OREVERSE,C'Y'                                                    
         CLI   OPEELED,0                                                        
         BNE   *+8                                                              
         MVI   OPEELED,C'Y'                                                     
*                                                                               
FSTD55   CLI   OREVERSE,0                                                       
         BNE   FSTD60                                                           
         CLI   PREVS,C'Y'          PROFILE TO SHOW REVERSALS                    
         BNE   *+8                                                              
         MVI   OREVERSE,C'Y'                                                    
*                                                                               
FSTD60   CLI   ODETAIL,0           IF NO OPTION OVERRIDE USE PROFILE            
         BNE   *+10                                                             
         MVC   ODETAIL,PDETAIL                                                  
         DROP  RF                                                               
*                                                                               
         GOTO1 AREADUP                                                          
         BE    FSTDX                                                            
*                                                                               
FSTDERR  J     ERXIT                                                            
FSTDX    J     OKXIT                                                            
         DROP  R2                                                               
*----------------------------------------------------------------------         
*        CHECK COLUMN OVERRIDE CONFLICT                                         
* ON ENTRY R1=A(OVERRIDE OPTION)                                                
* ON EXIT  OPTION SET TO NULL IF ALREADY AN OVERRIDE                            
*----------------------------------------------------------------------         
CHKCONF  LR    R0,RE                                                            
         CLI   0(R1),C'Y'          HOURS/TAX/BASIS?                             
         BE    *+12                                                             
         CLI   0(R1),C'B'          VALUE=BOTH?                                  
         BNE   CHKCX                                                            
         TM    DETFLAG,DETCOLMQ    ALREADY AN OVERRIDE OPTION                   
         BNO   *+12                                                             
         MVI   0(R1),0             YES SO CLEAR OPTION SETTING                  
         B     *+8                                                              
         OI    DETFLAG,DETCOLMQ    SET COLUMN OVERRIDE OPTION                   
CHKCX    LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT EXTRA DETAIL LINE(S)                                  *         
* ON ENTRY     R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              R4=A(TSAR DATA)                                        *         
* ON EXIT      R2=A(NEXT AVAILABLE DESCRIPTION LINE)                  *         
*              'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
***********************************************************************         
XDETLNS  CSECT                                                                  
         NMOD1 0,**XDET**                                                       
         LR    RC,R1                                                            
         STCM  R2,15,ANXTLINE      SAVE ADDRESS OF NEXT FREE LINE               
         USING TSARDATD,R4                                                      
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         CLI   OXDETAIL,C'Y'       XTRA DETAIL REQUIRED?                        
         BNE   XDETLX                                                           
         DROP  RF                                                               
         LA    R0,UNSCANBK         CLEAR UNSCAN BLOCK                           
         LHI   R1,MAXUNSCN*UNSCNLNQ                                             
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R3,UNSCANBK                                                      
         USING UNSCAND,R3                                                       
         CLI   TSDTTYPE,0          INPUT TYPE?                                  
         BE    XDETL10                                                          
         MVC   UNSCNLHS(L'MX@TYPE),MX@TYPE                                      
         OI    UNSCNLHS,X'40'                                                   
         SR    RF,RF                                                            
         IC    RF,TSDTTYPE                                                      
         CURED (RF),(4,UNSCNRHS),0,ALIGN=LEFT                                   
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL10  CLI   TWAOFFC,C'*'        SHOW TRANSACTION ELEMENT STATUS?             
         BNE   XDETL20                                                          
         MVC   UNSCNLHS(L'MX@STT),MX@STT                                        
         GOTO1 VHEXOUT,DMCB,TSDTEST,UNSCNRHS,1,=C'MIX'                          
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL20  ICM R2,15,AOTHELD                                                      
         BZ    XDETL100                                                         
         USING OTHELD,R2                                                        
         CLC   OTHNUM(L'OTHNUM+L'OTHPROF),SPACES                                
         BE    XDETL100                                                         
         MVC   UNSCNLHS(L'MX@OTHER),MX@OTHER                                    
         SR    R1,R1                                                            
         IC    R1,OTHLN            R1=L'(ELEMENT)                               
         SHI   R1,L'OTHEL+L'OTHLN                                               
         LA    R2,OTHNUM           R2=A( BEGINNING OF DATA)                     
         DROP  R2                                                               
         CLI   0(R2),C' '                                                       
         BE    XDETL80                                                          
XDETL30  SR    RF,RF               RF=LENGTH FOR EXECUTED MOVE                  
         LA    RE,1(R2)            RE=LOCATION IN ELEMENT                       
         BCT   R1,*+8                                                           
         B     XDETL70             END OF ELEMENT - WRITE OUT STRING            
XDETL40  CLI   0(RE),C'A'          SEARCH FOR END OF STRING OR DATE             
         BL    XDETL60                                                          
XDETL50  LA    RE,1(RE)            KEEP ON GOING                                
         LA    RF,1(RF)                                                         
         BCT   R1,XDETL40                                                       
         B     XDETL70             END OF ELEMENT                               
XDETL60  CLI   0(RE),X'80'                                                      
         BH    XDETL70             SHOULD BE A PACKED DATE - STOP               
         CLI   0(RE),C' '                                                       
         BH    XDETL50             SPECIAL CHARACTER - KEEP ON GOING            
XDETL70  EX    RF,*+8              AT END - MOVE TO UNSCAN BLOCK                
         B     *+10                                                             
         MVC   UNSCNRHS(0),0(R2)                                                
         LA    R3,UNSCNLNQ(R3)                                                  
         SR    RF,RF                                                            
         LTR   R1,R1                                                            
         BZ    XDETL100            END OF ELEMENT                               
         LR    R2,RE                                                            
XDETL80  CLI   0(R2),C' '          SCAN FOR NEXT FIELD (IF ANY)                 
         BNH   XDETL90             SEARCH FARTHER                               
         CLI   0(R2),C'A'          IS THIS A STRING                             
         BNL   XDETL30             YES                                          
         CLI   0(R2),X'80'         OR A SPECIAL CHARACTER                       
         BL    XDETL30             YES                                          
         B     XDETL100            NO - MUST BE A DATE                          
XDETL90  LA    R2,1(R2)                                                         
         BCT   R1,XDETL80                                                       
*                                                                               
XDETL100 ICM   R2,15,ASCIELD       DISPLAY DISCOUNT                             
         BZ    XDETL180                                                         
         USING SCIELD,R2                                                        
XDETL110 CLI   SCITYPE,SCITCDSC                                                 
         BNE   XDETL120                                                         
         CLC   UL41,BASKEY         FIXED ASSETS?                                
         BE    *+14                                                             
         MVC   UNSCNLHS(L'MX@DISS),MX@DISS                                      
         B     *+10                                                             
         MVC   UNSCNLHS(L'MX@DPR),MX@DPR                                        
         CURED (P6,SCIAMNT),(L'UNSCNRHS,UNSCNRHS),2,ALIGN=LEFT,        X        
               MINUS=YES                                                        
*                                                                               
         LA    R3,UNSCNLNQ(R3)                                                  
         B     XDETL130                                                         
*                                                                               
XDETL120 SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,SCILN                                                         
         AR    R2,RF                                                            
         CLI   SCIEL,SCIELQ                                                     
         BE    XDETL110                                                         
         CLI   SCIEL,0                                                          
         BNE   XDETL120                                                         
*                                                                               
XDETL130 ICM   R2,15,ASCIELD       DISPLAY REST OF SUBSIDIARY ITEMS?            
         USING SCIELD,R2                                                        
XDETL140 CLI   SCITYPE,SCITCDSC    ALREADY DONE DISCOUNT                        
         BE    XDETL170                                                         
         CLI   SCITYPE,SCITTQST    PST?                                         
         BNE   XDETL145            NO                                           
         MVC   UNSCNLHS(L'SCISUBPR),SCISUBPR                                    
         B     XDETL150                                                         
*                                                                               
XDETL145 MVC   UNSCNLHS(L'SCITYPE),SCITYPE                                      
XDETL150 CURED (P6,SCIAMNT),(L'UNSCNRHS,UNSCNRHS),2,ALIGN=LEFT,        X        
               MINUS=YES                                                        
XDETL160 SR    RF,RF                                                            
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL170 SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,SCILN                                                         
         AR    R2,RF                                                            
         CLI   SCIEL,SCIELQ                                                     
         BE    XDETL140                                                         
*                                                                               
XDETL180 OC    TSDACDAT,TSDACDAT   ACTIVITY DATE?                               
         BZ    XDETL190                                                         
         MVC   UNSCNLHS(L'MX@UDAT),MX@DATE                                      
         GOTO1 VDATCON,DMCB,(2,TSDACDAT),(0,UNSCNRHS)                           
         CLI   UNSCNRHS,C'9'                                                    
         BNH   *+16                                                             
         IC    R0,UNSCNRHS                                                      
         SH    R0,=H'10'                                                        
         STC   R0,UNSCNRHS                                                      
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL190 OC    TSDUSDAT,TSDUSDAT   USED DATE?                                   
         BZ    XDETL200                                                         
         MVC   UNSCNLHS(L'MX@UDAT),MX@UDAT                                      
         GOTO1 VDATCON,DMCB,(2,TSDUSDAT),(0,UNSCNRHS)                           
         CLI   UNSCNRHS,C'9'                                                    
         BNH   *+16                                                             
         IC    R0,UNSCNRHS                                                      
         SH    R0,=H'10'                                                        
         STC   R0,UNSCNRHS                                                      
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL200 OC    TSDPEDAT,TSDPEDAT   PEELED DATE?                                 
         BZ    XDETL201                                                         
         MVC   UNSCNLHS(L'MX@PELDT),MX@PELDT                                    
         GOTO1 VDATCON,DMCB,(2,TSDPEDAT),(0,UNSCNRHS)                           
         CLI   UNSCNRHS,C'9'                                                    
         BNH   *+16                                                             
         IC    R0,UNSCNRHS                                                      
         SH    R0,=H'10'                                                        
         STC   R0,UNSCNRHS                                                      
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL201 MVC   UNSCNLHS(L'MX@SEQ),MX@SEQ TRANS SUB-REFERENCE NUMBER             
         SR    RF,RF                                                            
         IC    RF,TSDSBREF                                                      
         CURED (RF),(4,UNSCNRHS),0,ALIGN=LEFT                                   
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
         CLI   TWAOFFC,C'*'        DDS?                                         
         BNE   XDETL215                                                         
         MVC   UNSCNLHS(L'MX@DSCAD),MX@DSCAD TRANS DISK ADDRESS                 
         LA    RF,L'TRNKDA                                                      
         GOTO1 VHEXOUT,DMCB,TSDDA,UNSCNRHS,(RF),1,=C'MIX'                       
         LA    R3,UNSCNLNQ(R3)                                                  
*                                                                               
XDETL215 ICM   R2,15,ANXTLINE      R2=A(NEXT DUMMY SCREEN LINE)                 
         GOTO1 AXDETDIS            DISPLAY ON DUMMY SCREEN LINES                
*                                                                               
         ICM   RF,15,AADRELD       ADDRESS ELEMENT?                             
         USING ADRELD,RF                                                        
         BZ    XDETLX                                                           
         SR    R3,R3                                                            
         ICM   R3,1,ADRNUM                                                      
         BZ    XDETLX                                                           
         SR    RE,RE                                                            
         IC    RE,MAXLINES                                                      
         SR    R1,R1                                                            
         IC    R1,LINSUSED                                                      
         LA    R0,1                NUMBER OF DISPLAY LINES FOR ADDRESS          
         LA    R1,1(R1)                                                         
         CR    R1,RE               MAX LINES EXCEEDED?                          
         BH    XDETLX                                                           
         BE    XDETL220                                                         
         CLI   ADRLN,ADRLN2Q       2 DISPLAY LINES REQUIRED?                    
         BNH   XDETL220                                                         
         LA    R0,2                                                             
         LA    R1,1(R1)                                                         
         CLI   ADRLN,ADRLN4Q       3 DISPLAY LINES REQUIRED?                    
         BNH   XDETL220                                                         
         LA    R0,3                                                             
         LA    R1,1(R1)                                                         
XDETL220 STCM  R1,1,LINSUSED                                                    
         USING SCRLINAD,R2         ADDRESS FORMAT                               
         MVC   SCRAADDR,MX@ADR     ADDRESS                                      
         LA    R1,ADRADD1                                                       
XDETL230 MVC   SCRAADLH,0(R1)                                                   
         BCT   R3,*+8                                                           
         B     *+12                                                             
         MVC   SCRAADRH,L'ADRADD1(R1)                                           
         BCTR  R3,0                                                             
         LA    R2,L'DUMLIN1(R2)                                                 
         LA    R1,2*L'ADRADD1(R1)                                               
         BCT   R0,XDETL230                                                      
*                                                                               
XDETLX   XIT1  REGS=(R2)                                                        
         DROP  R3,R4                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DETAIL WORKING STORAGE                                                        
**********************************************************************          
OVERWRKD DSECT                                                                  
AB1      DS    F                                                                
AB2      DS    F                                                                
AB3      DS    F                                                                
AB4      DS    F                                                                
ORELO    DS    A                   OVERLAY RELOCATION                           
AGCTBL   DS    A                   ADDRESS OF GRID COLUMN TABLE                 
ASPCGR   DS    A                   GRID SPECIAL ROUTINES                        
ANXTLINE DS    A                   ADDRESS OF NEXT DUMMY LINE                   
TRANAMNT DS    CL(L'TRNAMNT)       AGENCY/LOCAL CURRENCY AMOUNT                 
PARAMREF DS    CL(L'TRNKREF)       PAYEMNT/CHECK NUMBER                         
PARAMDAT DS    CL(L'TRNKDATE)      PAYMENT/CHECK DATE                           
DADDRESS DS    CL(L'TRNKDA)        TRANSACTION DISK ADDRESS                     
POSTPROG DS    XL(L'TRSSTAT3)      POSTING PROGRAM                              
TEMPNUM  DS    PL8                 TEMPORARY NUMBER STORAGE                     
TEMPDAT  DS    PL3                 TEMPORARY DATE STORAGE                       
TRANCURR DS    CL(L'AFCCURR)       TRANSACTION CURRENCY                         
TEMPAGY  DS    CL2                 HOLD AREA FOR AGY                            
ASSOCURR DS    CL(L'AFCCURR)       ASSOCIATED CURRENCY                          
*                                                                               
***********************************************************************         
* DATA DICTIONARY ENTRIES                                                       
***********************************************************************         
DSMIX    DS    0C                                                               
MX@ENH14 DS    CL(L'ENQDAT1)                                                    
MX@ENH15 DS    CL(L'ENQDAT1)                                                    
MX@RCND  DS    CL11                                                             
MX@DATE  DS    CL4                                                              
MX@DRAFT DS    CL9                                                              
MX@PYMNT DS    CL10                                                             
MX@CHK   DS    CL10                                                             
MX@BNK   DS    CL10                                                             
MX@CHKTO DS    CL10                                                             
MX@PAYTO DS    CL10                                                             
MX@GROSS DS    CL7                                                              
MX@NET   DS    CL7                                                              
MX@DISS  DS    CL7                                                              
MX@FLTX  DS    CL7                                                              
MX@ORDER DS    CL7                                                              
MX@SUBR  DS    CL7                                                              
MX@CMRCL DS    CL7                                                              
MX@CLINT DS    CL7                                                              
MX@PRO   DS    CL3                                                              
MX@JOB   DS    CL7                                                              
MX@EST   DS    CL3                                                              
MX@EXP   DS    CL7                                                              
MX@SRC   DS    CL7                                                              
MX@TIME  DS    CL5                                                              
MX@TMTYP DS    CL5                                                              
MX@DUEDT DS    CL8                                                              
MX@SERNO DS    CL6                                                              
MX@RUNNO DS    CL7                                                              
MX@FACC  DS    CL7                                                              
MX@ATTRS DS    CL10                                                             
MX@ATHED DS    CL9                                                              
MX@UATH  DS    CL9                                                              
MX@HELD  DS    CL9                                                              
MX@SELED DS    CL9                                                              
MX@APRVD DS    CL9                                                              
MX@URG   DS    CL9                                                              
MX@RVRSD DS    CL9                                                              
MX@HOURS DS    CL5                                                              
MX@HOURR DS    CL5                                                              
MX@BASIR DS    CL5                                                              
MX@VATR  DS    CL5                                                              
MX@PSTR  DS    CL5                                                              
MX@TAXR  DS    CL5                                                              
MX@TYPE  DS    CL2,CL1                                                          
MX@STT   DS    CL2,CL1                                                          
MX@OTHER DS    CL2,CL1                                                          
MX@UDAT  DS    CL2,CL1                                                          
MX@PELDT DS    CL2,CL1                                                          
MX@CR    DS    CL2,CL1                                                          
MX@DR    DS    CL2,CL1                                                          
MX@SEQ   DS    CL2,CL1                                                          
MX@ACCT  DS    CL15                                                             
MX@ACCTS DS    CL15                                                             
MX@CHKC  DS    CL20                                                             
MX@DPSON DS    CL20                                                             
MX@CTRON DS    CL20                                                             
MX@WRTFA DS    CL20                                                             
MX@XFRFR DS    CL20                                                             
MX@XFRTO DS    CL20                                                             
MX@DATED DS    CL10                                                             
MX@REF   DS    CL10                                                             
MX@PAYEE DS    CL10                                                             
MX@ADR   DS    CL7                                                              
MX@LCLCU DS    CL(L'SCR1AMTO)                                                   
MX@DSCAD DS    CL3                                                              
MX@DPR   DS    CL6                                                              
*----------------------------------------------------------------------         
* GRID COLUMNS                                                                  
*----------------------------------------------------------------------         
MX@ACTDT DS    CL13              ACTIVITY DATE                                  
MX@GSTBA DS    CL9               GST BASIS                                      
MX@RSBRF DS    CL9               BATCH REF                                      
MX@BANKA DS    0CL12             BANK ACCOUNT                                   
MX@NFA67 DS    CL12              BANK ACCOUNT/                                  
MX@RSCDT DS    CL11              CHECK/CHEQUE DATE                              
MX@RSCNO DS    CL13              CHECK/CHEQUE NUMBER                            
MX@CHKT  DS    CL12              CHECK/CHEQUE TOTAL                             
MX@CLRDA DS    CL7               CLEARED                                        
MX@RSDDT DS    CL12              DEPOSIT DATE                                   
MX@RSCOC DS    CL14              CONTRA ACCOUNT                                 
MX@DISL  DS    CL8               DISCOUNT                                       
MX@DISB  DS    CL9               DISBURSED                                      
MX@RSDUD DS    CL8               DUE DATE                                       
MX@ESTL  DS    CL8               ESTIMATE                                       
MX@VAT   DS    CL3               GST                                            
MX@INCM  DS    CL6               INCOME                                         
MX@MEMO  DS    CL4               MEMO                                           
MX@MOA   DS    CL3               MOA                                            
MX@MMOS  DS    CL22              MEDIA MONTH OF SERVICE                         
MX@OFFST DS    CL6               OFFSET                                         
MX@PELED DS    CL6               PEELED                                         
MX@PROL  DS    CL7               PRODUCT                                        
MX@STMDT DS    CL14              STATEMENT DATE                                 
MX@TIMTP DS    CL9               TIME TYPE                                      
MX@RSUSD DS    CL9               USED DATE                                      
MX@VOID  DS    CL4               VOID                                           
MX@WC    DS    CL8               WORKCODE                                       
MX@TRANT DS    CL10              TRANS TYPE                                     
MX@BAL   DS    CL8               BALANCE                                        
MX@DRCR  DS    CL3               D/C                                            
MX@TRANO DS    CL9               TRANS OFF                                      
MX@TRANR DS    CL9               TRANS REF                                      
MX@TRAND DS    CL10              TRANS DATE                                     
MX@REVDT DS    CL13              REVERSAL DATE                                  
MX@GRAMT DS    CL12              GROSS AMOUNT                                   
MX@STMD2 DS    CL14              STATEMENT DATE                                 
MX@RSODT DS    CL11              OFFSET DATE                                    
MX@DISAM DS    CL15              DISCOUNT AMOUNT                                
MX@CMNAM DS    CL17              COMMISSION AMOUNT                              
MX@NETA2 DS    CL10              NET AMOUNT                                     
MX@PID   DS    CL6               PERSON PID                                     
MX@INVCN DS    CL14              INVOICE NUMBER                                 
MX@JOEST DS    CL12              JOB/ESTIMATE                                   
MX@PAYME DS    CL14              PAYMENT METHOD                                 
*                                                                               
MX@KEYRF DS    CL13              KEY REFERENCE                                  
MX@BLBDT DS    CL13              BILLABLE DATE                                  
MX@APRVM DS    CL15              APPROVAL METHOD                                
MX@AAPP  DS    CL12              AUTO APPROVED                                  
MX@MAPP  DS    CL6               MARKER APPROVED                                
MX@CURC  DS    CL13              CURRENCY CODE                                  
MX@MFILE DS    CL10              MEDIA FILE (AGENCY ALPHA ID)                   
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
DSCAP    DS    0C                                                               
AC@ONT   DS    CL3                                                              
AC@PST   DS    CL3                                                              
AC@VAT   DS    CL3                                                              
AC@QST   DS    CL3                                                              
AC@HST   DS    CL3                                                              
AC@HBC   DS    CL6                                                              
AC@HAL   DS    CL6                                                              
AC@HSA   DS    CL6                                                              
AC@HMA   DS    CL6                                                              
AC@HPQ   DS    CL6                                                              
AC@HON   DS    CL6                                                              
AC@HPE   DS    CL6                                                              
AC@HNB   DS    CL6                                                              
AC@HNS   DS    CL6                                                              
AC@HNF   DS    CL6                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
* SCREEN OUTPUT LINE DSECT                                                      
***********************************************************************         
SCRLIN1D DSECT                     COVER SCREEN ITEM LINE1                      
SCR1CONT DS    CL(L'TRNKULA)       CONTRA CODE                                  
         DS    CL1                                                              
SCR1TDAT DS    CL8                 TRANSACTION DATE                             
         DS    CL1                                                              
SCR1TREF DS    CL6                 TRANSACTION REFERENCE                        
         DS    CL1                                                              
SCR1BREF DS    CL6                 BATCH REFERENCE                              
         DS    CL1                                                              
SCR1OFF  DS    CL2                 OFFICE                                       
         DS    CL1                                                              
SCR1NDSC DS    0CL18               NARROW DESCRIPTION COLUMN                    
SCR1WDSC DS    CL21                WIDE DESCRIPTION COLUMN                      
         ORG   SCR1WDSC+1                                                       
SCR1AMTO DS    0CL16               AMOUNT OVERRIDE COLUMN                       
         ORG   SCR1WDSC+11                                                      
SCR1HRT  DS    0CL5                HOURS TITLE                                  
         ORG   SCR1NDSC+L'SCR1NDSC+1                                            
SCR1WAMT DS    CL16                WIDE AMOUNT                                  
         ORG   SCR1WDSC+L'SCR1WDSC+1                                            
SCR1NAMT DS    CL13                NARROW AMOUNT                                
SCR1SIGN DS    CL2                 DR/CR                                        
SCR1LNQ  EQU   *-SCRLIN1D                                                       
*                                                                               
*---------------------*                                                         
* SCREEN ADDRESS LINE *                                                         
*---------------------*                                                         
SCRLINAD DSECT                     COVER SCREEN ADDRESS                         
SCRAADDR DS    CL(L'MX@ADR)        ADDRESS                                      
         DS    CL1                                                              
SCRAADLH DS    CL(L'ADRADD1)       LEFT HAND ADDRESS                            
         DS    CL1                                                              
SCRAADRH DS    CL(L'ADRADD1)       RIGHT HAND ADDRESS                           
SCRALNQ  EQU   *-SCRLINAD                                                       
*                                                                               
*-------------------*                                                           
* SCREEN TOTAL LINE *                                                           
*-------------------*                                                           
SCRTOT1D DSECT                     COVER SCREEN TOTAL LINE 1                    
SCRTOTAL DS    CL(L'MX@ACCT)                                                    
         DS    CL23                                                             
SCRHTOT  DS    CL20                COLUMN OVERRIDE TOTAL-HOURS ETC              
         DS    CL1                                                              
SCRTTOT  DS    CL17                TRANS TOTAL                                  
*                                                                               
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
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSITEM1  EQU   1                   ITEM 1 FORMAT                                
TSDCONT  DS    CL(L'TRNKULC)       CONTRA CODE                                  
TSDTDAT  DS    PL(L'TRNKDATE)      TRANSACTION DATE                             
TSDTREF  DS    CL(L'TRNKREF)       TRANSACTION REFERENCE                        
TSDKREF  DS    CL(L'TRNKREF)       TRAN REF WITHOUT 255 ADJUSTMENT DDS          
TSDBREF  DS    CL(L'TRNBTCH)       BATCH REFERENCE                              
TSDOFF   DS    CL(L'TRNOFFC)       OFFICE                                       
TSDSBREF DS    CL(L'TRNKSBR)       TRANSACTION SUB-REFERENCE NUMBER             
TSDATAMT DS    PL(L'TRNAMNT)       AGENCY CURRENCY TRANSACTION AMOUNT           
TSDTAMTY DS    XL1                 TRANSACTION AMOUNT TYPE                      
TSDTORDQ EQU   X'01'               ORDER AMOUNTS INCLUDED                       
TSDTRST  DS    XL(L'TRNRSTAT)      TRANSACTION RECORD STATUS                    
TSDTEST  DS    XL(L'TRNSTAT)       TRANSACTION ELEMENT STATUS                   
*                                                                               
TSDPYMET DS    XL(L'MPYBYTE)       PAYMENT METHOD STATUS FOR DEBITS             
TSDEFT   EQU   X'80'               CHECK WAS SENT VIA EFT                       
TSDCHK   EQU   X'40'               CHECK WAS PRINTED                            
TSDPCRD  EQU   X'20'               PAYMENT VIA PCARD                            
TSDCCRD  EQU   X'10'               PAYMENT VIA CCARD                            
TSDNOCHK EQU   X'00'               NO CHECK INFO POSTED YET.                    
*                                                                               
TSDSERNO DS    CL(L'MXPSER)        SERIAL NUMBER                                
TSDTTYPE DS    CL(L'TRNTYPE)       INPUT TYPE                                   
TSDACDAT DS    XL(L'TRSDATE)       ACTIVITY DATE                                
TSDTMOA  DS    PL(L'TRNKSMOS)      MOA                                          
TSDUSDAT DS    XL(L'TRSUDAT)       USED DATE                                    
TSDOFDAT DS    XL(L'TRSUDAT)       OFFSET DATE                                  
TSDSTDAT DS    XL(L'TRSBSTDT)      BANK STATEMENT DATE                          
TSDRCDAT DS    XL(L'GDADATE)       RECONCILED DATE (MARKER,ACCR)                
TSDCLDAT DS    XL(L'GDADATE2)      BANK CLEARED DATE                            
TSDPEDAT DS    XL(L'TRSPDAT)       PEELED DATE                                  
TSDRVDAT DS    XL(L'TRSREVD)       REVERSED DATE                                
TSDSTAT1 DS    XL(L'TRSSTAT)       TRANSACTION STATUS ELE STAT BYTE 1           
TSDSTAT2 DS    XL(L'TRSSTAT2)      TRANSACTION STATUS ELE STAT BYTE 2           
TSDSRCAC DS    CL(L'TRNKULA)       SOURCE ACCOUNT                               
TSDDA    DS    XL(L'TRNKDA)        DISK ADDRESS                                 
TSDBLBDT DS    PL(L'XPYBLDTE)      BILLABLE DATE                                
TSDAAPT  DS    CL1                 APPROVAL TYPE                                
TSDAAA   EQU   X'80'               AUTO APPROVED                                
TSDAAM   EQU   X'40'               MARKER APPROVED                              
TSDTACUR DS    CL(L'TRANCURR)      CURRENCY CODE                                
TSDMFILE DS    CL3                 MEDIA FILE (AGENCY ALPHA ID)                 
TSDRFST  EQU   *                   FIRST ELEMENT ON TSAR RECORD                 
TSDLENQ  EQU   *-TSARDATD                                                       
*                                                                               
*-----------------*                                                             
* TSAR TOTAL LINE *                                                             
*-----------------*                                                             
TSARTOTD DSECT                     COVER SCREEN LINE 1                          
TSTLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSTFMT   DS    CL1                 ITME FOMAT TYPE                              
TSTOTITM EQU   2                   TOTAL ITEM TYPE                              
TSTHTOT  DS    PL8                 HOURS AMOUNT                                 
TSTTTOT  DS    PL8                 TRANSACTION AMOUNT                           
TSTTDR   DS    PL8                 TRANSACTION AMOUNT                           
TSTTCR   DS    PL8                 TRANSACTION AMOUNT                           
TSTLNQ   EQU   *-TSARTOTD                                                       
         EJECT                                                                  
*                                                                               
**********************************************************************          
       ++INCLUDE ACENQWORK                                                      
         EJECT                                                                  
*                                                                               
**********************************************************************          
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
DETFLAG  DS    X                   GENERAL FLAG FOR OVERLAYS                    
DETSNARQ EQU   X'80'               SUPPRESS NARRATIVE                           
DETCOLMQ EQU   X'40'               COLUMN OVERRIDE                              
DETGRINQ EQU   X'20'               SCREEN INTIALIZED FOR GRIDS                  
DETREADQ EQU   X'10'               NEED TO RE-READ RECORD                       
DEVALS   DS    0PL8                CREDITOR VALUES                              
TRNTOT   DS    PL8                 TRANSACTION TOTAL                            
HRSTOT   DS    PL8                 HOURS TOTAL                                  
TRNTDR   DS    PL8                 TRANSACTION DR AMOUNT                        
TRNTCR   DS    PL8                 TRANSACTION CR AMOUNT                        
DEVALLNQ EQU   *-DEVALS                                                         
*                                                                               
PIDTABN  DS    X                   PID TABLE ENTRY NUMBER                       
PIDTAB   DS    40CL10              PID SAVE TABLE                               
PIDMAXQ  EQU   40                                                               
*                                                                               
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
**********************************************************************          
       ++INCLUDE SEACSFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041ACENQ02   02/13/19'                                      
         END                                                                    
