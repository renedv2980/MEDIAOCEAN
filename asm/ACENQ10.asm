*          DATA SET ACENQ10    AT LEVEL 006 AS OF 09/20/07                      
*PHASE T62010A                                                                  
T62010   TITLE 'ACCOUNT ENQUIRY - CONTRA LIST'                                  
T62010   CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,**ENQ10**,R7,CLEAR=YES,RR=RE                                   
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         LH    R8,=Y(OVERWORK-WORKD)                                            
         LA    R8,WORKD(R8)        R8=A(LOCAL WORKIN STORAGE)                   
         USING OVERWRKD,R8                                                      
         ST    RE,ORELO                                                         
*                                                                               
         A     RE,=A(GCTBL)        SET UP GRID COLUMN TABLE                     
         ST    RE,AGCTBL                                                        
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
*                                                                               
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN10              NO                                           
         BAS   RE,FSTDIS           YES PERFORM FIRST DISPLAY FUNCTIONS          
         BNE   ERRXIT                                                           
         TM    DISPFLAG,DISIOMAX   MAX IO'S?                                    
         BO    MAINX                                                            
         OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
         LA    R3,IOKEY            R3=A(IOKEY)                                  
         B     MAIN60                                                           
*                                                                               
MAIN10   TM    OVRSTAT,OVRGDONE                                                 
         BO    MAINXGX                                                          
*                                                                               
         TM    IOMAXSW,IOMRRDUP+IOMRRD25 LAST TIME READUP HIT MAX IO'S?         
         BZ    MAIN12              NO, SKIP                                     
         GOTO1 AREADUP             CALL READUP                                  
         BNE   MAINX               EXIT                                         
         CLC   IOKEY,SPACES                                                     
         BE    MAIN60                                                           
         B     MAIN15              CC = EQUAL, GOOD READ                        
*                                                                               
MAIN12   TM    IOMAXSW,IOMRLOCL    RESTART LOCAL ROUTINE ?                      
         BZ    MAIN15              NO, SKIP                                     
         GOTO1 GETBALA             RESTART GETBALA                              
         TM    DISPFLAG,DISIOMAX   MAX IOS REACHED ?                            
         BO    MAINX               YES, EXIT                                    
*                                                                               
         GOTO1 AREADUP             CALL READUP                                  
         BNE   MAINX               CC = NOT EQUAL, EXIT                         
*                                                                               
MAIN15   TM    DISPFLAG,ALLREADQ   ALL RECORDS READ?                            
         BO    MAIN20                                                           
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BO    MAIN20                                                           
         GOTO1 AIO,IOREAD+IOACCDIR+IO1 RE-ESTABLISH IO SEQUENCE                 
         BE    MAIN20                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     MAINX                                                            
*                                                                               
MAIN20   CLC   TSCURRNO,TSLSTREC   HAVE WE ALLREADY GOT RECORD IN TSAR?         
         BH    MAIN40              NO                                           
*                                                                               
MAIN30   GOTO1 ATSARGET,TSCURRNO   GET TSAR RECORD                              
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BO    *+12                 YES                                         
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
*                                                                               
         BAS   RE,DISBAL           DISPLAY BALANCE LINES                        
*                                                                               
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   MAINX               SCREEN IS FULL                               
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN20                                                           
*                                                                               
MAIN40   TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BZ    MAIN50              YES                                          
         LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATOOMNY)                                           
         B     ERRXIT                                                           
*                                                                               
MAIN50   TM    DISPFLAG,ALLREADQ   HAVE ALL RECORDS BEEN READ?                  
         BO    MAINX               YES                                          
*                                                                               
MAIN60   TM    STATFLAG,ROFFBUKQ   READING OFFICE/CONTRA BUCKETS?               
         BZ    *+12                 (NO)                                        
         BAS   RE,GETODET           (YES)                                       
         B     *+8                                                              
         BAS   RE,GETDET                                                        
*                                                                               
MAIN61   CLC   SAVEACC,IOKEY                                                    
         BE    MAINX                                                            
         OI    DISPFLAG,ALLREADQ                                                
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    *+8                                                              
         BAS   RE,TOTAL                                                         
         BAS   RE,BALBF                                                         
         BAS   RE,TOTAL                                                         
MAINX    B     OKXIT                                                            
*                                                                               
MAINXGX  GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),UNILDG                            
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    OKXIT               SCREEN IS FULL                               
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
FSTDIS   NTR1                                                                   
*                                                                               
         MVI   DETFLAG,0           INIT DETAIL FLAG                             
         ZAP   BUCKDRS,=P'0'       BUCKET/'TRANS' BUCKET DEBITS                 
         ZAP   BUCKCRS,=P'0'       BUCKET/'TRANS' BUCKET CREDITS                
         ZAP   BALFDRS,=P'0'       BALANCE BROUGHT FORWARD DEBITS               
         ZAP   BALFCRS,=P'0'       BALANCE BROUGHT FORWARD CREDITS              
         ZAP   DEBTOT,=P'0'        CLEAR DEBIT TOTALS                           
         ZAP   CRETOT,=P'0'        CLEAR CREDIT TOTALS                          
         MVC   KEYSAVE,SPACES                                                   
         OI    DISPFLAG,BALANCEQ   THIS DISPLAY HAS BALANCE LINE FORMAT         
         MVI   SCRLLADJ,2          NUM OF BALNCE LINES FOR SCROLL ADJST         
         MVI   STATFLAG,0          BAL BROUGHT FRWD FLAG                        
         MVC   UNILDG,SPACES                                                    
*                                                                               
         GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,0,ACOMFACS,(0,0)                  
*                                                                               
         USING FLDHDRD,R2                                                       
         LA    R2,BASKEYH          R2=A(KEY FIELD)                              
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         SR    R4,R4                                                            
         ICM   R4,1,FLDILEN        R4=L'(KEY FIELD INPUT)                       
         BZ    FSTDERR                                                          
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         CLI   FLDILEN,L'ACTKULA   ENSURE LENGTH NOT TOO LONG                   
         BH    FSTDERR                                                          
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
         CLC   FLDDATA(L'SPROUNIT+L'SPROLEDG),SPROUNIT PROD NOT ALLOWED         
         BE    FSTDERR                                                          
         GOTO1 AUNITLDG,FLDDATA    READ UNIT/LEDGER RECORDS                     
         BNE   FSTDERR                                                          
         MVC   UNILDG,FLDDATA      SAVE UNIT/LEDGER                             
*                                                                               
         GOTO1 AGETACC,0                                                        
         BNE   FSTDERR                                                          
*                                                                               
         MVI   NXTRMODE,0                                                       
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
*                                                                               
         CLI   OBFWD,0             DEFAULT IS BF=Y                              
         BNE   *+8                                                              
         MVI   OBFWD,C'D'          USE 'D' TO INDICATE WE SET IT                
*                                                                               
*------------------------------*                                                
* HOW TO GET THE DATA          *                                                
*------------------------------*                                                
         MVI   NXTRMODE,0          SET READING TRANSACTIONS                     
         CLI   ODRAFT,C'O'         DRAFT TRANSACTIONS ONLY?                     
         BE    FSTD16              . YES, READ TRANSACTIONS THEN                
         CLI   ODRAFT,C'Y'         INCLUDE DRAFTS?                              
         BE    FSTD16              . YES, CAN'T READ CONTRAS ONLY               
*                                                                               
         OC    OOFFICVL,OOFFICVL   OFFICE FILTER?                               
         BNZ   FSTD16              . YES, NEED OFFICE DETAIL                    
         CLI   LDGTOFFP,LDGOTRAN   OFFICE IN TRANSACTIONS?                      
         BNE   FSTD14              . NO,  DO NOT NEED OFFICE DETAIL             
         CLI   TERMACCS,C'*'       LIMIT ACCESS?                                
         BE    FSTD16              . YES, NEED OFFICE DETAIL                    
         CLI   TERMACCS,C'$'       LIMIT ACCESS?                                
         BE    FSTD16              . YES, NEED OFFICE DETAIL                    
FSTD14   OI    NXTRMODE,NXTRCACQ   READ CONTRA ACCOUNTS ONLY                    
         B     FSTD18                                                           
*                                                                               
FSTD16   TM    COMPSTA4,CPYSOFF2   2 CHAR OFFICE COMPANY?                       
         BZ    FSTD18              . NO, NO OFFICE BUCKETS AVAILABLE            
         CLI   UNILDG,C'S'         SUBSIDIARY LEDGER?                           
         BE    *+12                                                             
         CLI   UNILDG,C'G'         GENERAL LEDGER?                              
         BNE   FSTD18                                                           
         OI    STATFLAG,ROFFBUKQ   READ OFFICE/CONTRA BUCKETS                   
*                                                                               
*------------------------------*                                                
FSTD18   GOTO1 ASETFIS                                                          
*                                                                               
         MVC   FVMSGNO,=AL2(EAWRNGLV)                                           
         LA    RF,LEDGTLVA         RF=A(HIERARCHY LEVELS)                       
         LR    RE,RF               RE=A(HIERARCHY LEVELS)                       
         SR    R1,R1               R1=MINIMUM LENGTH OF ACCOUNT                 
         CLI   0(RF),X'0C'         FULL HIERARCHY?                              
         BE    *+12                                                             
         LA    RF,1(RF)            NOPE BUMP RF                                 
         B     *-12                                                             
         CR    RF,RE               YEP IS IT JUST ONE LEVEL?                    
         BE    *+10                                                             
         BCTR  RF,0                NO THEN GET MIN LENGTH                       
         IC    R1,0(RF)                                                         
         LR    RE,R4               RE=L'(INPUT)                                 
         SH    RE,=Y(L'ACTKUNT+L'ACTKLDG) SUBTRACT UNIT LEDGER LENGTH           
         CR    RE,R1               IS INPUT LONG ENOUGH?                        
         BNH   FSTDERR             NOPE, WRONG LEVEL ACCOUNT                    
         BCTR  R4,0                                                             
         LA    R3,IOKEY            R3=A(KEY FOR LOW LEVEL ACCOUNT)              
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,MYCO                                                     
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ACTKUNT(0),FLDDATA                                               
         MVC   FVMSGNO,=AL2(EAIFNTFN)                                           
         GOTO1 AIO,IOREAD+IOACCMST+IO1 READ LOW LEVEL ACCOUNT                   
         BE    FSTD20                                                           
         CLI   IOERR,IOERNF                                                     
         BE    FSTDERR             LOW LEVEL ACCOUNT NOT FOUND                  
         DC    H'0'                                                             
*                                                                               
FSTD20   MVC   FVMSGNO,=AL2(EASECLOC)                                           
         GOTO1 AOFFACC                                                          
         TM    OFFLFLAG,OFFLSEC                                                 
         BO    FSTDERR                                                          
         OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
         MVC   SAVEACC,ACTKCULA                                                 
*                                                                               
         MVI   SECFFLAG,0          INIT SECURITY FIELD FLAG                     
         OC    TWALEN,TWALEN       NEW SECURITY?                                
         BZ    FSTD25                                                           
         CLC   ONEP,BASKEY                                                      
         BE    FSTD24                                                           
         CLC   ONER,BASKEY                                                      
         BE    FSTD24                                                           
         CLC   ONEJ,BASKEY                                                      
         BNE   FSTD25                                                           
FSTD24   L     RF,ASECBLK         SEE IF NOT ALLOWED TO SEE RATE AMT            
         USING SECD,RF                                                          
         GOTO1 VSECRET,DMCB,('SECPFLDP',SECD),=AL1(1)                           
         BNL   FSTD25                                                           
         OI    SECFFLAG,SECFRATE   SET RATE CANNOT BE SHOWN                     
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    FSTD25                                                           
         CLI   AGYCTRY,CTRYUSA                                                  
         BE    FSTD25                                                           
         CLC   ONEJ,BASKEY                                                      
         BE    FSTDERR                                                          
*                                                                               
FSTD25   LA    R2,ENQDAT1H                                                      
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    FSTD27                                                           
         LA    R2,GRDDAT1H                                                      
         GOTO1 ADISACC                                                          
         B     FSTD28                                                           
*                                                                               
FSTD27   GOTO1 ADISUL              DISPLAY UNIT AND LEDGER NAMES                
*                                                                               
         LA    R2,ENQDAT1H+ENQDAT2H-ENQDAT1H                                    
         MVC   FLDDATA(L'ENQDAT1),SPACES                                        
         MVC   FLDDATA(L'MX@ACC),MX@ACC                                         
         LA    R1,FLDDATA+L'MX@ACC-1                                            
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'='                                                       
FSTD28   L     R3,AIO1                                                          
         LA    RF,ACTRFST                                                       
FSTD30   CLI   0(RF),EOR           END OF RECORD?                               
         BE    FSTD60                                                           
         CLI   0(RF),NAMELQ        NAME ELEMENT?                                
         BE    FSTD50                                                           
         CLI   0(RF),RSTELQ        RECORD STATUS ELEMENT?                       
         BE    FSTD55                                                           
*                                                                               
FSTD40   SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     FSTD30                                                           
*                                                                               
         USING NAMELD,RF                                                        
FSTD50   TM    PCDRIVEN,PCGRIDQ                                                 
         BO    FSTD40                                                           
         SR    RE,RE               GET ACCOUNT NAME                             
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R1),NAMEREC                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         CLC   SVCACN,SPACES       HAVE WE SPECIFIED STARTING CONTRA?           
         BE    FSTD40                                                           
         LA    R1,4(RE,R1)                                                      
         MVI   0(R1),C'/'                                                       
         MVC   2(L'MX@STCON,R1),MX@STCON                                        
         LA    R1,L'MX@STCON+1(R1)                                              
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'='                                                       
         LA    R1,2(R1)                                                         
         LA    RE,FLDDATA+L'ENQDAT1-1                                           
         SR    RE,R1                                                            
         CH    RE,=Y(L'SVCACN-1)                                                
         BNH   *+8                                                              
         LA    RE,L'SVCACN-1                                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SVCACN    DISPLAY AS MUCH OF NAME AS POSSIBLE            
         B     FSTD40                                                           
*                                                                               
         USING RSTELD,RF                                                        
FSTD55   MVC   CLOSEDAT,RSTBDATE                                                
         B     FSTD40                                                           
*                                                                               
FSTD60   TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    FSTD62                                                           
         LA    R2,GRDDAT1H                                                      
         GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
         B     FSTD64                                                           
*                                                                               
FSTD62   LA    R2,ENQDAT2H-ENQDAT1H(R2)  DISPLAY COLUMN HEADINGS                
         MVC   FLDDATA(L'MX@ENH9),MX@ENH9                                       
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
*                                                                               
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         MVC   FLDDATA(L'MX@ENH10),MX@ENH10                                     
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
*                                                                               
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
*                                                                               
FSTD64   L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
*                                                                               
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         CLI   OBUKTYPE,0                                                       
         BNE   FSTD65                                                           
         CLI   OHOURS,C'Y'                                                      
         BNE   *+12                                                             
         MVI   OBUKTYPE,C'H'                                                    
         B     FSTD65                                                           
         CLC   ONEC,BASKEY         IF UNIT/LEDGER 1C                            
         BNE   FSTD65                                                           
         TM    COMPSTA5,CPYSNCST   AND NEW COST AGENCY                          
         BNO   FSTD65                                                           
         MVI   OBUKTYPE,C'1'       DEFAULT IS METHOD 1                          
FSTD65   OI    OBUKTYPE,C' '                                                    
         OI    OPAYTYPE,C' '                                                    
         TM    SECFFLAG,SECFRATE   NOT ALLOWED TO SEE RATE?                     
         BNO   FSTD70                                                           
         CLI   OBUKTYPE,C' '                                                    
         BH    FSTD70                                                           
         MVI   OBUKTYPE,C'H'       SET TO HOURS                                 
         B     FSTD70                                                           
         DROP  R2                                                               
*                                                                               
FSTD70   L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         LA    R2,BASOPTH          R2=A(OPTION FIELD)                           
         OC    OOFFICVL,OOFFICVL   OFFICE FILTER                                
         BZ    FSTD75                                                           
         CLI   LDGTOFFP,LDGOFLT1   OFFICE POS = FN (1 CHAR)?                    
         BL    FSTD75              NO                                           
         CLI   LDGTOFFP,LDGOFLT4       "                                        
         BH    FSTD75                  "                                        
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$OPTNV)                                           
         B     FSTDERR                                                          
         DROP  RF                                                               
*                                                                               
FSTD75   BAS   RE,GETBALA          GET BALANCE FROM OFFICE RECORDS              
         TM    DISPFLAG,DISIOMAX   MAX IO ?                                     
         BZ    FSTD90              NO, CONTINUE                                 
         OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
         B     FSTDX               EXIT OKAY                                    
*                                                                               
FSTD90   GOTO1 ADISMOS,DMCB,(L'MX@MOA,MX@MOA),(L'MX@BFW,MX@BFW)                 
*                                                                               
         GOTO1 AREADUP                                                          
         BE    FSTDX               CC = EQUAL, GOOD READ                        
         TM    DISPFLAG,DISIOMAX   MAX IO ?                                     
         BZ    FSTDERR             NO, RETURN AN ERROR                          
         OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
*                                                                               
FSTDX    CR    RB,RB               EXIT OKAY                                    
         B     *+6                                                              
FSTDERR  LTR   RB,RB               ERROR EXIT                                   
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* READ ALL OFFICE RECORDS UP FRONT FOR BALANCE BROUGHT FORWARD.       *         
* THIS IS ONLY NEEDED IF GETTING PASSED RECS IN CONTRA SEQUENCE.  THE *         
* CONTROLLER DOES NOT PASS OFFICE RECS WHEN READING IN CONTRA SEQ.    *         
* ON ENTRY AIOAREA1 CONTAINS ACCOUNT RECORD                           *         
***********************************************************************         
         USING TRNRECD,R3                                                       
         USING OPTVALSD,R5                                                      
GETBALA  NTR1                                                                   
*                                                                               
         TM    STATFLAG,ROFFBUKQ                                                
         BO    GETBAX                                                           
         TM    NXTRMODE,NXTRCACQ       CONTRA ACCOUNTS ONLY                     
         BO    GETBAX                                                           
         TM    COMPSTA4,CPYSOFF2       2 CHAR OFFICE?                           
         BZ    GETBAX                  . NO, EXIT                               
         CLI   CONTLEN,0                                                        
         BE    GETBAX                                                           
         CLC   CLOSEDAT,COMPFIN        BBF BEFORE FINANCIAL YEAR?               
         BL    GETBAX                  . YES                                    
*                                                                               
         LA    R3,IOKEY                R3 = KEY RECORD                          
         L     R5,AOPTVALS             R5=A(OPTION VALUES)                      
*                                                                               
         OC    ENDBBF,ENDBBF                                                    
         BZ    GETBAX                                                           
*                                                                               
*----------------------------------                                             
* DECIDE WHICH OFFICE LIST WE NEED                                              
*----------------------------------                                             
         OC    OOFFICVL,OOFFICVL       OFFICE LIST OVERRIDE?                    
         BZ    *+12                    . NO                                     
         LA    R4,OOFFICLS             . YES, USE WANTED OFFICES                
         B     GETBA5                                                           
         L     R4,AOFFBLK              LIMITTED ACCESS                          
         CLI   LDGTOFFP,LDGOTRAN       SECURITY OFFICE IN TRANSACTIONS?         
         BE    *+8                     . YES                                    
         L     R4,AOFFBLK2             . NO, USE ALL OFFICES                    
         LA    R4,OFFAWORK-OFFALD(,R4)                                          
*                                                                               
GETBA5   TM    IOMAXSW,IOMRLOCL        RESTART LOCAL ROUTINE ?                  
         BO    GETBA10                 . YES, SKIP INITIALIZATION               
         MVC   ACNTKEY,IOKEY           SAVE THE INPUT KEY                       
         MVC   TWAOFWK#,=H'1'          START OF OFFICE LIST                     
GETBA10  CLC   TWAOFWK#,0(R4)          END OF OFFICE LIST?                      
         BH    GETBA90                 . YES, NEXT CONTRA                       
         LH    R1,TWAOFWK#                                                      
         SLL   R1,1                    SHIFT NO OF OFFICES FOR INDEX            
         LA    RF,0(R1,R4)                                                      
         SRL   R1,1                                                             
         LA    R1,1(,R1)                                                        
         STH   R1,TWAOFWK#             BUMP TO NEXT                             
         CLC   0(L'TRNKOFF,RF),SPACES  SPACES MEANS FILLER OFFICE               
         BE    GETBA10                                                          
         MVC   TRNKCULA,ACNTKEY        SET ACCOUNT CODE                         
         MVC   TRNKOFF,0(RF)           SET OFFICE  CODE                         
*                                                                               
         GOTO1 AIO,IOREAD+IOACCDIR+IO1 GET OFFICE RECORD                        
         BE    GETBA20                                                          
         TM    IOERR,IOMAX             MAX IOS REACHED?                         
         BZ    GETBA10                 . NO, GET NEXT OFFICE                    
         B     GETBAER                 . YES, ERROR TOO MANY I/O'S              
*                                                                               
GETBA20  GOTO1 AOFFOFF                 SECURITY CHECK FOR OFFICE REC            
         BNE   GETBA10                 SECURITY ERRORS                          
         GOTO1 AIO,IOGET+IOACCMST+IO1  GET THE RECORD                           
         BE    GETBA45                                                          
         TM    IOERR,IOMAX             MAX IOS REACHED?                         
         BO    GETBAER                 . YES, ERROR TOO MANY I/O'S              
         DC    H'0'                                                             
*                                                                               
GETBA45  L     R3,AIO1                 SHOULD BE OFFICE RECORD                  
         TM    DISPFLAG,DISIOMAX       MAX IOS REACHED?                         
         BO    GETBAER                 . YES, ERROR TOO MANY I/O'S              
*                                                                               
         LA    R3,TRNRFST              . NO, ADDRESS FIRST ELEMENT              
GETBA60  CLI   0(R3),EOR               END OF RECORD?                           
         BE    GETBA10                 . YES                                    
         CLI   0(R3),ABLELQ            BALANCE ELEMENT?                         
         BE    GETBA65                 . YES                                    
*                                                                               
         SR    R0,R0                   BUMP TO NEXT ELEMENT                     
         IC    R0,1(,R3)                                                        
         AR    R3,R0                                                            
         B     GETBA60                                                          
*                                                                               
         USING ABLELD,R3                                                        
GETBA65  CLI   OMOSFI,NEGFILTR         NEGATIVE FILTER?                         
         BE    *+14                    . YES                                    
         CLC   CLOSEDAT,OMOSST         BALANCE BEFORE START MOA?                
         BL    *+8                     . YES                                    
         OI    STATFLAG,BBFFBLQ        ACCOUNT BALANCE ELEMENT                  
         AP    BALFDRS,ABLFRWD         ADD BALANCE BROUGHT FORWARD              
         B     GETBA10                 GET NEXT OFFICE                          
*                                      ERROR TOO MANY I/O'S                     
GETBAER  OI    IOMAXSW,IOMRLOCL        RESTART FROM LOCAL ROUTINE               
         OI    DISPFLAG,DISIOMAX       TURN ON MAX IO'S                         
         LH    R1,TWAOFWK#             BACK UP OFFICE COUNT BY ONE              
         BCTR  R1,0                                                             
         STH   R1,TWAOFWK#                                                      
         MVC   KEYSAVE,ACNTKEY         RESTART FROM ACCOUNT KEY                 
         B     GETBAX                  EXIT                                     
*                                                                               
GETBA90  MVC   IOKEY,ACNTKEY                                                    
         NI    IOMAXSW,TURNOFF-IOMRLOCL TURN OFF RESTART LOCAL ROUTINE          
*                                                                               
GETBAX   B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT ,                                                                
*                                                                               
***********************************************************************         
*        GET AMOUNTS FROM OFFICE/CONTRA BUCKET RECORDS                *         
* ON ENTRY 'KEYSAVE' SET TO SPACES MEANS 1ST TIME THROUGH             *         
*          'OFFDONE' IS COUNT OF PROCESSED OFFICES (2 CHAR OFF ONLY)  *         
***********************************************************************         
         USING TRNRECD,R3                                                       
         USING OPTVALSD,R2                                                      
GETODET  NTR1                                                                   
*                                                                               
         L     R2,AOPTVALS          R2=A(OPTION VALUES)                         
GETO10   LA    R3,IOKEY                                                         
         MVC   KEYSAVE,IOKEY        SAVE THE LAST GOOD IOKEY                    
         CLC   TRNKCULA,SAVEACC     RECORD PART OF CORRECT ACCOUNT?             
         BE    GETO20                                                           
         BAS   RE,BLDDIS            DEAL WITH ANY OUTSTANDING AMOUNTS           
         B     GETOX                                                            
*                                                                               
GETO20   CLC   TRNKCULC,SPACES      CONTRA?                                     
         BE    GETO150              . NO                                        
         CLC   TRNKOFF,SPACES       OFFICE?                                     
         BE    GETO150              . NO                                        
         CLC   TRNKDATE,SPACES      TRANSACTION                                 
         BH    GETO140              . YES                                       
         CLC   CHDKNULL-CHDRECD(L'CHDKNULL,R3),SPACES                           
         BL    GETO45                                                           
         CLI   ODRAFT,C'O'          DRAFTS ONLY?                                
         BE    GETD150              . YES                                       
*                                                                               
         CLC   =C'1C',BASKEY        COSTING?                                    
         BNE   GETO40                                                           
         CLI   OBUKTYPE,C'1'        METHOD=?                                    
         BL    GETO40                                                           
         CLI   OBUKTYPE,C'9'        METHOD=?                                    
         BH    GETO40                                                           
         CLC   =C'14',TRNKCUNT                                                  
         BE    GETO40                                                           
         CLC   =C'15',TRNKCUNT                                                  
         BE    GETO40                                                           
         CLC   =C'16',TRNKCUNT                                                  
         BNE   GETO45                                                           
GETO40   CLC   OBUKTYPE,CHDKBTYP-CHDRECD(R3)                                    
         BNE   GETO150                                                          
         CLC   CHDKNULL-CHDRECD(L'CHDKNULL,R3),SPACES                           
         BL    GETO45                                                           
         CLI   OPAYTYPE,C' '        LUMP TOGETHER IF NO PAYTYPE FILT            
         BE    GETO50                                                           
         CLC   OPAYTYPE,CACKSTYP-CACRECD(R3)                                    
         BNE   GETO150                                                          
         B     GETO50                                                           
GETO45   BAS   RE,BLDDIS            DEAL WITH ANY OUTSTANDING AMOUNTS           
         BNE   GETO175                                                          
*                                                                               
*--------------------------*                                                    
* FILTER ON CONTRA ACCOUNT *                                                    
*--------------------------*                                                    
GETO50   CLI   CONTLEN,0                                                        
         BE    GETO80               CONTRA FILTER?                              
         MVC   WORK,SPACES                                                      
         LA    RF,L'TRNKULC         RF=MAX LENGTH OF CONTRA ACCOUNT             
         LA    RE,TRNKULC           RE=POSITION IN CONTRA ACCOUNT               
         CLI   0(RE),C' '           IF SPACE THEN BUMP                          
         BH    GETO55                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,*-12                                                          
         B     GETO60                                                           
GETO55   BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)        PULL IN CONTRA AFTER SPACES                 
         SR    RF,RF                                                            
         ICM   RF,1,CONTLEN         LENGTH OF CONTRA CODE                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),CONTRA       MATCHES CONTRA?                             
         BE    GETO80                                                           
         CLI   NEGCONT,NEGFILTR     NEGATIVE FILTER?                            
         BE    GETO80                                                           
         B     GETO150                                                          
GETO60   CLI   NEGCONT,NEGFILTR     NEGATIVE FILTER?                            
         BE    GETO150                                                          
*                                                                               
*-----------------------*                                                       
* GET THE CONTRA RECORD *                                                       
*-----------------------*                                                       
GETO80   GOTO1 AOFFOFF              CHECK OFFICE VALUE FILTER                   
         BNE   GETO150                                                          
         GOTO1 AIO,IOGET+IOACCMST+IO1 GET THE CONTRA                            
         BE    *+14                                                             
         TM    IOERR,IOMAX          MAX IOS REACHED?                            
         BO    GETO160                                                          
         DC    H'0'                                                             
*                                                                               
*----------------------*                                                        
* PROCESS THE ELEMENTS *                                                        
*----------------------*                                                        
         L     R3,AIO1              R3=A(CONTRA RECORD)                         
         LA    R4,TRNRFST           R4=A(FIRST ELEMENT ON RECORD)               
GETO100  CLI   0(R4),EOR            END OF RECORD?                              
         BE    GETO150                                                          
         CLI   0(R4),CACELQ         CONTRA ACCOUNT ELEMENT?                     
         BE    GETO112                                                          
         CLI   0(R4),BUKELQ         BUCKET ELEMENT?                             
         BE    GETO115                                                          
         CLI   0(R4),PBKELQ         PRIOR BUCKET ELEMENT?                       
         BE    GETO125                                                          
GETO105  SR    RF,RF                BUMP TO NEXT ELEMENT                        
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     GETO100                                                          
*                                                                               
*---------------------*                                                         
* CONTRA NAME ELEMENT *                                                         
*---------------------*                                                         
         USING CACELD,R4            CONTRA NAME ELEMENT                         
GETO112  MVC   CONTCODE,SPACES      CLEAR CONTRA CODE                           
         MVC   CONTNAME,SPACES      AND CONTRA NAME                             
         LA    RE,CACCNTU                                                       
         LA    RF,L'CACCNTU+L'CACCNTL+L'CACCNTA-1                               
         CLI   CACCNTU,C' '                                                     
         BH    *+12                                                             
         LA    RE,CACCNTA                                                       
         LA    RF,L'CACCNTA-1                                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONTCODE(0),0(RE)    CONTRA CODE                                 
         SR    RF,RF                                                            
         IC    RF,CACLN                                                         
         SH    RF,=Y(CACLN1Q+1)                                                 
         BM    GETO105                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONTNAME(0),CACNAME                                              
         B     GETO105                                                          
         DROP  R4                                                               
*                                                                               
*-----------------------*                                                       
* CONTRA BUCKET ELEMENT *                                                       
*-----------------------*                                                       
         USING BUKELD,R4            CONTRA BUCKET ELEMENT                       
GETO115  OC    OMOS,OMOS            MOS FILTER?                                 
         BZ    GETO120                                                          
         GOTO1 ADCOMP,DMCB,(L'BUKMOS,BUKMOS),OMOSST,OMOSEN,OMOSFI               
         BE    GETO120                                                          
         CLI   OMOSFI,NEGFILTR      NEGATIVE FILTER?                            
         BE    GETO105              . YES                                       
         CLC   BUKMOS,OMOSST                                                    
         BNL   GETO105                                                          
         CLC   BUKMOS,COMPFIN       NO BBF OUTSIDE FIN YEAR                     
         BL    GETO105                                                          
GETO119  AP    BALFDRS,BUKDR                                                    
         AP    BALFCRS,BUKCR                                                    
         B     GETO105                                                          
*                                                                               
GETO120  AP    BUCKDRS,BUKDR                                                    
         AP    BUCKCRS,BUKCR                                                    
         OI    STATFLAG,TRNMDPQ     TRAN FOR THIS CONTRA/PERIOD                 
         B     GETO105                                                          
         DROP  R4                                                               
*                                                                               
*--------------------------------*                                              
* CONTRA PREVIOUS BUCKET ELEMENT *                                              
*--------------------------------*                                              
         USING PBKELD,R4            PREVIOUS BUCKET ELEMENT                     
GETO125  OC    OMOS,OMOS            MOS FILTER?                                 
         BZ    GETO130                                                          
         GOTO1 ADCOMP,DMCB,(L'PBKHI,PBKHI),OMOSST,OMOSEN,OMOSFI                 
         BE    GETO130                                                          
         CLI   OMOSFI,NEGFILTR      NEGATIVE FILTER?                            
         BE    GETO105              . YES                                       
         CLC   PBKHI,OMOSST                                                     
         BNL   GETO105                                                          
         CLC   PBKHI,COMPFIN        NO BBF OUTSIDE FIN YEAR                     
         BL    GETO105                                                          
GETO129  AP    BALFDRS,PBKDR                                                    
         AP    BALFCRS,PBKCR                                                    
         B     GETO105                                                          
*                                                                               
GETO130  AP    BUCKDRS,PBKDR                                                    
         AP    BUCKCRS,PBKCR                                                    
         OI    STATFLAG,TRNMDPQ     TRAN FOR THIS CONTRA/PERIOD                 
         B     GETO105                                                          
         DROP  R4                                                               
*                                                                               
*--------------------                                                           
* DRAFT TRANSACTIONS                                                            
*--------------------                                                           
GETO140  TM    TRNKSTA,TRNSDRFT     DRAFT TRANSACTION?                          
         BZ    GETO150              . NO, GET NEXT                              
         CLI   ODRAFT,C'Y'          INCLUDE DRAFTS?                             
         BE    GETD25               . YES                                       
         CLI   ODRAFT,C'O'          DRAFTS ONLY?                                
         BE    GETD25               . YES                                       
*                                                                               
*---------------------                                                          
* GET THE NEXT RECORD                                                           
*---------------------                                                          
GETO150  GOTO1 ANXTREC,NXTRMODE     RETURN NEXT RECORD                          
         BE    GETO10                                                           
         TM    DISPFLAG,DISIOMAX    MAX I/O ?                                   
         BO    GETOX                . YES, EXIT                                 
         DC    H'0'                                                             
*---------------------                                                          
*                                                                               
GETO160  OI    DISPFLAG,DISIOMAX    MAX I/O                                     
         B     GETOX                EXIT                                        
*                                                                               
GETO175  MVC   KEYSAVE,IOKEY                                                    
GETOX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
         DROP  R3                                                               
*                                                                               
***********************************************************************         
*        GET AMOUNTS FROM TRANSACTION/BUCKET RECORDS                  *         
* ON ENTRY 'KEYSAVE' SET TO SPACES MEANS 1ST TIME THROUGH             *         
*          'OFFDONE' IS COUNT OF PROCESSED OFFICES (2 CHAR OFF ONLY)  *         
***********************************************************************         
GETDET   NTR1                                                                   
*                                                                               
         USING TRNRECD,R3                                                       
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
*                                                                               
GETD10   LA    R3,IOKEY                                                         
         MVC   KEYSAVE,IOKEY       SAVE THE LAST GOOD IOKEY                     
         CLC   TRNKCULA,SAVEACC    RECORD PART OF CORRECT ACCOUNT?              
         BE    GETD20                                                           
         BAS   RE,BLDDIS           DEAL WITH ANY OUTSTANDING AMOUNTS            
         B     GETDX                                                            
*                                                                               
GETD20   CLC   TRNKCULC,SPACES     TRANSACTION OR CONTRA?                       
         BE    GETD150                                                          
         TM    NXTRMODE,NXTRCACQ   CONTRA RECORDS ONLY?                         
         BO    GETD35                                                           
         CLC   TRNKDATE,SPACES     WE REQUIRE TRANS AND NAME CONTRAS            
         BNH   GETD30                                                           
*                                                                               
         TM    TRNKSTA2,TRNSPEEL   PEELED TRANSACTION                           
         BO    GETD150                                                          
*                                                                               
         TM    TRNKSTA,TRNSDRFT    DRAFT TRANSACTION?                           
         BZ    GETD22              . NO                                         
         CLI   ODRAFT,C'Y'         INCLUDE DRAFTS?                              
         BE    GETD25              . YES                                        
         CLI   ODRAFT,C'O'         DRAFTS ONLY?                                 
         BE    GETD25              . YES                                        
         B     GETD150             OTHERWISE, SKIP                              
GETD22   CLI   ODRAFT,C'O'         DRAFTS ONLY?                                 
         BE    GETD150             . YES, SKIP                                  
*                                                                               
GETD25   NI    STATFLAG,X'FF'-BBFTRNQ  DO NOT ADD TRANS TO BBF                  
         OC    OMOS,OMOS           MOS FILTER?                                  
         BZ    GETD50                                                           
         GOTO1 ADCOMP,DMCB,(L'TRNKSMOS,TRNKSMOS),OMOSST,OMOSEN,OMOSFI           
         BE    GETD50                                                           
         CLI   OMOSFI,NEGFILTR     NEGATIVE FILTER?                             
         BE    GETD150             YES                                          
         CLC   TRNKSMOS,OMOSST                                                  
         BNL   GETD150                                                          
         CLC   TRNKSMOS,COMPFIN      YES, THEN NO BBF OUTSIDE FIN YEAR          
         BL    GETD150                                                          
GETD29   OI    STATFLAG,BBFTRNQ    ADD THIS TRANS TO BBF                        
         B     GETD50                                                           
*                                                                               
GETD30   CLC   CHDKNULL-CHDRECD(L'CHDKNULL,R3),SPACES                           
         BE    GETD150                                                          
         CLC   TRNKULC,CONTCODE                                                 
         BE    GETD50                                                           
         BAS   RE,BLDDIS           DEAL WITH ANY OUTSTANDING AMOUNTS            
         BE    GETD50                                                           
         B     GETD175                                                          
*                                                                               
GETD35   CLC   TRNKDATE,SPACES     DITCH THE TRANNIES                           
         BH    GETD150                                                          
         CLC   TRNKOFF,SPACES      NO OFFICE CONTRA RECORDS                     
         BNE   GETD150                                                          
         CLC   CHDKNULL-CHDRECD(L'CHDKNULL,R3),SPACES                           
         BL    GETD45                                                           
         CLC   =C'1C',BASKEY       COSTING?                                     
         BNE   GETD40                                                           
         CLI   OBUKTYPE,C'1'       METHOD=?                                     
         BL    GETD40                                                           
         CLI   OBUKTYPE,C'9'       METHOD=?                                     
         BH    GETD40                                                           
         CLC   =C'14',TRNKCUNT                                                  
         BE    GETD40                                                           
         CLC   =C'15',TRNKCUNT                                                  
         BE    GETD40                                                           
         CLC   =C'16',TRNKCUNT                                                  
         BNE   GETD45                                                           
GETD40   CLC   OBUKTYPE,CHDKBTYP-CHDRECD(R3)                                    
         BNE   GETD150                                                          
         CLC   CHDKNULL-CHDRECD(L'CHDKNULL,R3),SPACES                           
         BL    GETD45                                                           
         CLI   OPAYTYPE,C' '       LUMP TOGETHER IF NO PAYTYPE FILT             
         BE    GETD50                                                           
         CLC   OPAYTYPE,CACKSTYP-CACRECD(R3)                                    
         BNE   GETD150                                                          
         B     GETD50                                                           
GETD45   BAS   RE,BLDDIS           DEAL WITH ANY OUTSTANDING AMOUNTS            
         BNE   GETD175                                                          
*                                                                               
GETD50   CLI   CONTLEN,0                                                        
         BE    GETD80              CONTRA FILTER?                               
         MVC   WORK,SPACES                                                      
         LA    RF,L'TRNKULC        RF=MAX LENGTH OF CONTRA ACCOUNT              
         LA    RE,TRNKULC          RE=POSITION IN CONTRA ACCOUNT                
         CLI   0(RE),C' '          IF SPACE THEN BUMP                           
         BH    GETD55                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,*-12                                                          
         B     GETD60                                                           
GETD55   BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)       PULL IN CONTRA AFTER SPACES                  
         SR    RF,RF                                                            
         ICM   RF,1,CONTLEN        LENGTH OF CONTRA CODE                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),CONTRA      MATCHES CONTRA?                              
         BE    GETD80                                                           
         CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER?                             
         BE    GETD80                                                           
         B     GETD150                                                          
GETD60   CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER?                             
         BE    GETD150                                                          
*                                                                               
GETD80   GOTO1 AIO,IOGET+IOACCMST+IO1 GET THE CONTRA/TRANSACTION                
         BE    *+14                                                             
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BO    GETD160                                                          
         DC    H'0'                                                             
         L     R3,AIO1             R3=A(CONTRA RECORD)                          
         TM    CORDFLAG,CORDMODQ   IF CONTRA ORDERED MODE SECURITY              
         BO    GETD99              ALREADY APPLIED                              
         CLI   TRNRFST,TRNELQ      TRANSACTION RECORD?                          
         BNE   GETD99                                                           
         GOTO1 AOFFTRN             CHECK SECURITY ON TRANSACTION                
         TM    DISPFLAG,DISIOMAX                                                
         BO    GETD160                                                          
         CLI   OFFLFLAG,0                                                       
         BNE   GETD150                                                          
GETD99   LA    R4,TRNRFST          R4=A(FIRST ELEMENT ON RECORD)                
GETD100  CLI   0(R4),EOR           END OF RECORD?                               
         BE    GETD150                                                          
         CLI   0(R4),CACELQ        CONTRA ACCOUNT ELEMENT?                      
         BE    GETD112                                                          
         CLI   0(R4),BUKELQ        BUCKET ELEMENT?                              
         BE    GETD115                                                          
         CLI   0(R4),PBKELQ        PRIOR BUCKET ELEMENT?                        
         BE    GETD125                                                          
         CLI   0(R4),TRNELQ        TRANSACTION ELEMENT?                         
         BE    GETD135                                                          
         CLI   0(R4),SCIELQ        SUBSIDIARY CASH INFO ELEMENT?                
         BE    GETD145                                                          
*                                                                               
GETD105  SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     GETD100                                                          
*                                                                               
         USING CACELD,R4           CONTRA NAME ELEMENT                          
GETD112  MVC   CONTCODE,SPACES     CLEAR CONTRA CODE                            
         MVC   CONTNAME,SPACES     AND CONTRA NAME                              
         LA    RE,CACCNTU                                                       
         LA    RF,L'CACCNTU+L'CACCNTL+L'CACCNTA-1                               
         CLI   CACCNTU,C' '                                                     
         BH    *+12                                                             
         LA    RE,CACCNTA                                                       
         LA    RF,L'CACCNTA-1                                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONTCODE(0),0(RE)   CONTRA CODE                                  
         SR    RF,RF                                                            
         IC    RF,CACLN                                                         
         SH    RF,=Y(CACLN1Q+1)                                                 
         BM    GETD105                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONTNAME(0),CACNAME                                              
         B     GETD105                                                          
         DROP  R4                                                               
*                                                                               
         USING BUKELD,R4           CONTRA BUCKET ELEMENT                        
GETD115  CLC   TRNKOFF,SPACES      IF THIS RECORD HAS AN OFFICE                 
         BE    *+14                AND A CONTRA - SKIP HISTORY                  
         CLC   TRNKCULC,SPACES                                                  
         BNE   GETD150                                                          
*                                                                               
         OC    OMOS,OMOS           MOS FILTER?                                  
         BZ    GETD120                                                          
         GOTO1 ADCOMP,DMCB,(L'BUKMOS,BUKMOS),OMOSST,OMOSEN,OMOSFI               
         BE    GETD120                                                          
         CLI   OMOSFI,NEGFILTR     NEGATIVE FILTER?                             
         BE    GETD105             YES                                          
         CLC   BUKMOS,OMOSST                                                    
         BNL   GETD105                                                          
         CLC   BUKMOS,COMPFIN       YES, THEN NO BBF OUTSIDE FIN YEAR           
         BL    GETD105                                                          
GETD119  AP    BALFDRS,BUKDR                                                    
         AP    BALFCRS,BUKCR                                                    
         B     GETD105                                                          
*                                                                               
GETD120  AP    BUCKDRS,BUKDR                                                    
         AP    BUCKCRS,BUKCR                                                    
         OI    STATFLAG,TRNMDPQ    TRAN FOR THIS CONTRA/PERIOD                  
         B     GETD105                                                          
         DROP  R4                                                               
*                                                                               
         USING PBKELD,R4           PREVIOUS BUCKET ELEMENT                      
GETD125  OC    OMOS,OMOS           MOS FILTER?                                  
         BZ    GETD130                                                          
         GOTO1 ADCOMP,DMCB,(L'PBKHI,PBKHI),OMOSST,OMOSEN,OMOSFI                 
         BE    GETD130                                                          
         CLI   OMOSFI,NEGFILTR     NEGATIVE FILTER?                             
         BE    GETD105             YES                                          
         CLC   PBKHI,OMOSST                                                     
         BNL   GETD105                                                          
         CLC   PBKHI,COMPFIN        YES, THEN NO BBF OUTSIDE FIN YEAR           
         BL    GETD105                                                          
GETD129  AP    BALFDRS,PBKDR                                                    
         AP    BALFCRS,PBKCR                                                    
         B     GETD105                                                          
*                                                                               
GETD130  AP    BUCKDRS,PBKDR                                                    
         AP    BUCKCRS,PBKCR                                                    
         OI    STATFLAG,TRNMDPQ     TRAN FOR THIS CONTRA/PERIOD                 
         B     GETD105                                                          
         DROP  R4                                                               
*                                                                               
         USING TRNELD,R4           TRANSACTION ELEMENT                          
GETD135  CLI   OBUKTYPE,C' '       CASH BUCKETS WANTED?                         
         BNE   GETD105                                                          
         TM    STATFLAG,BBFTRNQ    ADD TRANS TO BBF?                            
         BZ    GETD140             NO                                           
         TM    TRNSTAT,TRNSDR      DEBIT OR CREDIT?                             
         BO    *+14                                                             
         AP    BALFCRS,TRNAMNT                                                  
         B     GETD105                                                          
         AP    BALFDRS,TRNAMNT                                                  
         B     GETD105                                                          
*                                                                               
GETD140  OI    STATFLAG,TRNMDPQ    TRAN FOR THIS CONTRA/PERIOD                  
         TM    TRNSTAT,TRNSDR      DEBIT OR CREDIT?                             
         BO    *+14                                                             
         AP    BUCKCRS,TRNAMNT                                                  
         B     GETD105                                                          
         AP    BUCKDRS,TRNAMNT                                                  
         B     GETD105                                                          
         DROP  R4                                                               
*                                                                               
         USING SCIELD,R4           SUBSIDIARY CASH INFO ELEMENT                 
GETD145  GOTO1 ABUKMAKE,DMCB,(R4),AIO1,PBDR,PBCR GET 'BUCKET' AMOUNTS           
         AP    BUCKDRS,PBDR                                                     
         AP    BUCKCRS,PBCR                                                     
         OI    STATFLAG,TRNMDPQ                                                 
         B     GETD105             NO 'BUCKET' AMOUNTS FROM SCIELD?             
         DROP  R4                                                               
*                                                                               
GETD150  TM    STATFLAG,ROFFBUKQ                                                
         BO    GETO150                                                          
*                                                                               
         GOTO1 ANXTREC,NXTRMODE    RETURN NEXT RECORD                           
         BE    GETD10                                                           
         TM    DISPFLAG,DISIOMAX   MAX I/O ?                                    
         BO    GETDX               YES, EXIT                                    
         DC    H'0'                                                             
*                                                                               
GETD160  OI    DISPFLAG,DISIOMAX   MAX I/O                                      
         B     GETDX               EXIT                                         
*                                                                               
GETD175  MVC   KEYSAVE,IOKEY                                                    
GETDX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD TSAR RECORD FOR SCREEN DATA ITEM,                      *         
*        FILL DUMMY SCREEN LINES                                      *         
* ON ENTRY R3=A(AIOAREA 1 AREA CONTAINING CONTRA ACCOUNT RECORTD)     *         
* ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL                              *         
***********************************************************************         
*                                                                               
BLDDIS   NTR1                                                                   
         L     R3,AIO1                                                          
         USING CACRECD,R3                                                       
         TM    STATFLAG,TRNMDPQ    TRANSCTIONS FOR THIS CONTRA?                 
         BZ    BLDDX               NO                                           
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ATSARREC                                                      
         USING TSARRECD,R2                                                      
         MVC   TSARKYNO,TSCURRNO                                                
         LH    RF,=Y(TSDLENQ)                                                   
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN        LENGTH OF TSAR RECORD                        
         LA    R2,TSARDATA                                                      
         USING TSARDATD,R2                                                      
         MVI   TSDFMT,TSITEM1      SCREEN DATA ITEM 1                           
         MVC   TSDCONT,CONTCODE    CONTRA CODE                                  
         MVC   TSDNAME,CONTNAME    CONTRA NAME                                  
         ZAP   TSDBFDR,DEBTOT      BROUGHT FWD DR                               
         ZAP   TSDBFCR,CRETOT      BROUGHT FWD CR                               
         ZAP   TSDDR,BUCKDRS       DEBITS                                       
         ZAP   TSDCR,BUCKCRS       CREDITS                                      
         ZAP   TSDDIF,TSDDR        DIFFERENCE                                   
         SP    TSDDIF,TSDCR                                                     
         ZAP   BUCKDRS,=P'0'                                                    
         ZAP   BUCKCRS,=P'0'                                                    
*                                                                               
BLDT130  AP    DEBTOT,TSDDR        DEBIT TOTAL                                  
         AP    CRETOT,TSDCR        CREDIT TOTAL                                 
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BO    *+12                 YES                                         
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
*                                                                               
         MVC   TSDLINES,LINSUSED                                                
*                                                                               
         GOTO1 ATSARADD            ADD RECORD                                   
         BE    BLDD140                                                          
         TM    DISPFLAG,TSARFULQ                                                
         BO    BLDDERRX                                                         
         TM    DISPFLAG,DISIOMAX                                                
         BO    BLDDERRX                                                         
         DC    H'0'                                                             
*                                                                               
BLDD140  MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BL    BLDD160                                                          
         BAS   RE,DISBAL           DISPLAY BALANCE LINES                        
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   BLDD170             SCREEN IS FULL                               
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
BLDD160  SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     BLDDX                                                            
*                                                                               
BLDD170  SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO       UPDATE TSAR RECORD COUNTER                   
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     BLDDERRX                                                         
*                                                                               
BLDDX    NI    STATFLAG,X'FF'-TRNMDPQ    NEED MORE DATA TO OUTPUT               
         CR    RB,RB                                                            
         B     XIT                                                              
BLDDERRX NI    STATFLAG,X'FF'-TRNMDPQ    NEED MORE DATE TO OUTPUT               
         LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
FORMTSAR NTR1                                                                   
*                                                                               
         MVI   LINSUSED,0          NUMBER OF LINES DISPLAYED                    
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN  LINES                    
         LH    R1,=Y(L'DUMLIN1*NDUMLINE)                                        
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         L     R3,ATSARREC         R4=A(TSAR RECORD)                            
         USING TSARRECD,R3                                                      
         CLI   TSARFMT,TSITEM1     ITEM DETAIL LINE?                            
         BNE   FORMT70                                                          
*                                                                               
         USING SCRLINED,R2         DSECT FOR NORMAL DATA LINE                   
         LA    R4,TSARDATA         R4=A(TSAR RECORD DATA)                       
         USING TSARDATD,R4                                                      
         MVC   SCRLCONT,TSDCONT    CONTRA ACCOUNT CODE                          
         MVC   TMPNBLK,SPACES                                                   
         GOTO1 VCHOPPER,DMCB,(L'TSDNAME,TSDNAME),(L'TMPNB1,TMPNBLK),2           
         MVC   SCRLNAME,TMPNB1     LINE 1 OF JOB NAME                           
*                                                                               
         PACK  TEMP(L'TSDDR),NINES(L'SCRLDR-2) GET STRING OF NINES              
         CP    TEMP(L'TSDDR),TSDDR IS AMOUNT TOO HIGH TO FIT?                   
         BL    FORMT10                                                          
         MP    TEMP(L'TSDDR),=P'-1'                                             
         CP    TEMP(L'TSDDR),TSDDR IS AMOUNT TO LOW TO FIT?                     
         BH    FORMT10                                                          
         CURED TSDDR,(L'SCRLDR,SCRLDR),2,MINUS=YES                              
         B     FORMT20                                                          
FORMT10  CURED TSDDR,(L'SCRLDR,SCRLDR),2,MINUS=YES,DECS=ROUND                   
*                                                                               
FORMT20  PACK  TEMP(L'TSDCR),NINES(L'SCRLCR-2) GET STRING OF NINES              
         CP    TEMP(L'TSDCR),TSDCR IS AMOUNT TOO HIGH TO FIT?                   
         BL    FORMT30                                                          
         MP    TEMP(L'TSDCR),=P'-1'                                             
         CP    TEMP(L'TSDCR),TSDCR IS AMOUNT TO LOW TO FIT?                     
         BH    FORMT30                                                          
         CURED TSDCR,(L'SCRLCR,SCRLCR),2,MINUS=YES                              
         B     FORMT40                                                          
FORMT30  CURED TSDCR,(L'SCRLCR,SCRLCR),2,MINUS=YES,DECS=ROUND                   
*                                                                               
FORMT40  ZAP   DIFFO,TSDDR                                                      
         SP    DIFFO,TSDCR         BALANCE ON CONTRA ACCOUNT                    
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    *+12                                                             
         CLI   AGYCTRY,CTRYUSA                                                  
         BNE   FORMT45                                                          
         CLC   ONER,BASKEY                                                      
         BNE   FORMT45                                                          
         L     RF,AOPTVALS                                                      
         USING OPTVALSD,RF                                                      
         CLI   OBUKTYPE,C'H'       BUCKET TYPE HOURS?                           
         BNE   FORMT45                                                          
         ZAP   DIFFO,TSDCR                                                      
         DROP  RF                                                               
FORMT45  PACK  TEMP(L'DIFFO),NINES(L'SCRLDIF-2) GET STRING OF NINES             
         CP    TEMP(L'DIFFO),DIFFO IS AMOUNT TOO HIGH TO FIT?                   
         BL    FORMT50                                                          
         MP    TEMP(L'DIFFO),=P'-1'                                             
         CP    TEMP(L'DIFFO),DIFFO IS AMOUNT TO LOW TO FIT?                     
         BH    FORMT50                                                          
         CURED (P8,DIFFO),(L'SCRLDIF,SCRLDIF),2,MINUS=YES                       
         B     FORMT60                                                          
FORMT50  CURED (P8,DIFFO),(L'SCRLDIF,SCRLDIF),2,MINUS=YES,DECS=ROUND            
*                                                                               
FORMT60  MVI   LINSUSED,1          NUMBER OF DUMMY SCREEN LINES USED            
         CLC   TMPNB2,SPACES       SECOND LINE NAME?                            
         BE    FORMTX                                                           
         LA    R2,L'DUMLIN1(R2)                                                 
         MVC   SCRLNAME,TMPNB2     LINE 2 OF JOB NAME                           
         MVI   LINSUSED,2          NUMBER OF DUMMY SCREEN LINES USED            
         B     FORMTX                                                           
         DROP  R4                                                               
*                                                                               
FORMT70  LA    R2,L'DUMLIN1(R2)                                                 
         CLI   TSARFMT,TSBBFITM    BBF LINE ITEM?                               
         BNE   FORMT71                                                          
         MVC   SCRLNAME(L'MX@BALBF),MX@BALBF                                    
         TM    STATFLAG,BBFFBLQ                                                 
         BZ    FORMT73                                                          
         MVI   SCRLNAME+L'MX@BALBF,C'*'                                         
         B     FORMT73                                                          
         DROP  R2                                                               
*                                                                               
         USING SCRLINED,R2         DSECT FOR TOTAL LINE                         
FORMT71  CLI   TSARFMT,TSSUBITM    BCF LINE ITEM?                               
         BNE   FORMT72                                                          
         MVC   SCRLNAME(L'MX@SUBT),MX@SUBT                                      
         B     FORMT73                                                          
FORMT72  MVC   SCRLNAME(L'MX@TOTAL),MX@TOTAL                                    
FORMT73  LA    R4,TSARDATA         R4=A(TSAR RECORD DATA)                       
         USING TSARTOTD,R4                                                      
         PACK  TEMP(L'TSTDR),NINES(L'SCRLDR-2) GET STRING OF NINES              
         CP    TEMP(L'TSTDR),TSTDR IS AMOUNT TOO HIGH TO FIT?                   
         BL    FORMT80                                                          
         MP    TEMP(L'TSTDR),=P'-1'                                             
         CP    TEMP(L'TSTDR),TSTDR IS AMOUNT TO LOW TO FIT?                     
         BH    FORMT80                                                          
         CURED (P8,TSTDR),(L'SCRLDR,SCRLDR),2,MINUS=YES                         
         B     FORMT90                                                          
FORMT80  CURED (P8,TSTDR),(L'SCRLDR,SCRLDR),2,MINUS=YES,DECS=ROUND              
*                                                                               
FORMT90  PACK  TEMP(L'TSTCR),NINES(L'SCRLCR-2) GET STRING OF NINES              
         CP    TEMP(L'TSTCR),TSTCR IS AMOUNT TOO HIGH TO FIT?                   
         BL    FORMT100                                                         
         MP    TEMP(L'TSTCR),=P'-1'                                             
         CP    TEMP(L'TSTCR),TSTCR IS AMOUNT TO LOW TO FIT?                     
         BH    FORMT100                                                         
         CURED (P8,TSTCR),(L'SCRLCR,SCRLCR),2,MINUS=YES                         
         B     FORMT110                                                         
FORMT100 CURED (P8,TSTCR),(L'SCRLCR,SCRLCR),2,MINUS=YES,DECS=ROUND              
*                                                                               
FORMT110 ZAP   DIFFO,TSTDR                                                      
         SP    DIFFO,TSTCR   BALANCE ON TOTAL                                   
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    *+12                                                             
         CLI   AGYCTRY,CTRYUSA                                                  
         BNE   FORMT115                                                         
         CLC   ONER,BASKEY                                                      
         BNE   FORMT115                                                         
         L     RF,AOPTVALS                                                      
         USING OPTVALSD,RF                                                      
         CLI   OBUKTYPE,C'H'       BUCKET TYPE ALREADY DEFINED?                 
         BNE   FORMT115                                                         
         ZAP   DIFFO,TSTCR                                                      
         DROP  RF                                                               
FORMT115 PACK  TEMP(L'DIFFO),NINES(L'SCRLDIF-2) GET STRING OF NINES             
         CP    TEMP(L'DIFFO),DIFFO IS AMOUNT TOO HIGH TO FIT?                   
         BL    FORMT120                                                         
         MP    TEMP(L'DIFFO),=P'-1'                                             
         CP    TEMP(L'DIFFO),DIFFO IS AMOUNT TO LOW TO FIT?                     
         BH    FORMT120                                                         
         CURED (P8,DIFFO),(L'SCRLDIF,SCRLDIF),2,MINUS=YES                       
         B     FORMT130                                                         
FORMT120 CURED (P8,DIFFO),(L'SCRLDIF,SCRLDIF),2,MINUS=YES,DECS=ROUND            
*                                                                               
FORMT130 MVI   LINSUSED,2          NUMBER OF LINES USED                         
         MVI   DISATRIB,HILIGHTQ   DISPLAY ATTRIBUTE                            
*                                                                               
FORMTX   B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES FOR GRIDS       *         
***********************************************************************         
FGRMTSAR NTR1                                                                   
*                                                                               
FGRM10   MVI   LINSUSED,0          NUMBER OF LINES DISPLAYED                    
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN  LINES                    
         LH    R1,=Y(L'DUMLIN1*NDUMLINE)                                        
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         L     R3,ATSARREC         R4=A(TSAR RECORD)                            
         USING TSARRECD,R3                                                      
*                                                                               
         TM    DETFLAG,DETGRINQ                                                 
         BO    FGRM20                                                           
         GOTO1 ADISGRD,DMCB,('DWNINIT',AGCTBL),UNILDG                           
         GOTO1 ADISPLAY,DISATRIB        DISPLAY DUMMY SCREEN LINES              
         OI    DETFLAG,DETGRINQ                                                 
         B     FGRM10                                                           
*                                                                               
FGRM20   CLI   TSARFMT,TSTOTITM    TOTAL LINE ITEM?                             
         BE    FGRM30                                                           
         GOTO1 ADISGRD,DMCB,(0,AGCTBL),UNILDG                                   
         B     FGRMX                                                            
*                                                                               
FGRM30   GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),UNILDG                            
*                                                                               
FGRMX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DEAL WITH THE BALANCE BROUGHT FORWARD LINE                   *         
***********************************************************************         
BALBF    NTR1                                                                   
*                                                                               
         L     R2,AOPTVALS                                                      
         USING OPTVALSD,R2                                                      
         OC    ENDBBF,ENDBBF       SHOW BAL BFWD?                               
         BZ    BALBFX              NO                                           
         OC    OMOSST,OMOSST       START DATE?                                  
         BNZ   BALBF10                                                          
         TM    STATFLAG,BBFFBLQ    BBF TAKEN FROM PEELED INFO?                  
         BZ    BALBFX                                                           
*                                                                               
BALBF10  AP    DEBTOT,BALFDRS      ADD BBF TO TOTAL                             
         AP    CRETOT,BALFCRS                                                   
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LH    RF,=Y(TSTLNQ)                                                    
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN        LENGTH OF TSAR RECORD                        
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARTOTD,R3                                                      
         MVI   TSTFMT,TSBBFITM     BALANCE BROUGHT FORWARD ITEM                 
         ZAP   TSTDR,BALFDRS       DEBIT BBF                                    
         ZAP   TSTCR,BALFCRS       CREDIT BBF                                   
         ZAP   TSTDIF,TSTDR                                                     
         SP    TSTDIF,TSTCR                                                     
         MVC   TSTCONT,MX@BALBF                                                 
         MVC   TSTNAME(L'MX@BALBF),MX@BALBF                                     
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BZ    *+12                                                             
         BAS   RE,FGRMTSAR                                                      
         B     *+8                                                              
         BAS   RE,FORMTSAR                                                      
*                                                                               
         MVC   TSTLINES,LINSUSED   NUMBER OF LINES USED BY TOTAL                
         GOTO1 ATSARADD                                                         
         MVC   TSLSTREC,TSCURRNO                                                
         GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BNE   *+10                SCREEN IS FULL                               
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
BALBFX   B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DEAL WITH THE TOTAL LINE                                     *         
***********************************************************************         
TOTAL    NTR1                                                                   
*                                                                               
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LH    RF,=Y(TSTLNQ)                                                    
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN        LENGTH OF TSAR RECORD                        
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARTOTD,R3                                                      
         TM    STATFLAG,BCFOUTQ    IS BALACE CARRIED FORWARD DONE?              
         BO    TOTAL10                                                          
         TM    PCDRIVEN,PCGRIDQ    GRIDS                                        
         BO    TOTAL10                                                          
                                                                                
         MVC   TSTCONT,MX@SUBT                                                  
         MVC   TSTNAME(L'MX@SUBT),MX@SUBT                                       
*                                                                               
         MVI   TSTFMT,TSSUBITM                                                  
         OI    STATFLAG,BCFOUTQ                                                 
         B     *+8                                                              
TOTAL10  MVI   TSTFMT,TSTOTITM     TOTAL ITEM                                   
         ZAP   TSTDR,DEBTOT        DEBIT TOTAL                                  
         ZAP   TSTCR,CRETOT        CREDIT TOTAL                                 
         ZAP   TSTDIF,TSTDR        DIFFERENCE                                   
         SP    TSTDIF,TSTCR                                                     
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BO    *+12                                                             
         BAS   RE,FORMTSAR                                                      
         B     *+8                                                              
         BAS   RE,FGRMTSAR                                                      
*                                                                               
         MVC   TSTLINES,LINSUSED   NUMBER OF LINES USED BY TOTAL                
         GOTO1 ATSARADD                                                         
         MVC   TSLSTREC,TSCURRNO                                                
*                                                                               
TOTAL20  GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BNE   *+10                SCREEN IS FULL                               
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY BALANCE LINES                                        *         
***********************************************************************         
DISBAL   NTR1                                                                   
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    DISBX                                                            
*                                                                               
         L     R1,ATSARREC                                                      
         CLI   TSARFMT-TSARRECD(R1),TSITEM1  ITEM DETAIL LINE?                  
         BNE   DISBX                                                            
         SR    RF,RF                                                            
         IC    RF,DISLINE                                                       
         LR    RE,RF                                                            
         CLC   DISLINE,DISSTART    START OF DISPLAY?                            
         BNE   DISB10                                                           
         CLC   TSCURRNO,=H'1'      IF NOT FIRST TSAR REC                        
         BE    DISBX                                                            
         LA    RE,1(RE)                                                         
         STC   RE,DISLINE          DISPLAY BALANCE C/F                          
         MH    RF,=Y(ENQDAT2H-ENQDAT1H)                                         
         LA    R2,ENQDAT1H(RF)                                                  
         USING FLDHDRD,R2                                                       
         LA    R3,FLDDATA                                                       
         MVC   FLDDATA(L'ENQDAT1),SPACES                                        
         USING SCRLINED,R3                                                      
         MVC   SCRLCONT,MX@BALCF                                                
         B     DISB20                                                           
*                                                                               
DISB10   SR    R3,R3                                                            
         IC    R3,DISEND                                                        
         SR    R0,R0                                                            
         IC    R0,LINSUSED                                                      
         BCTR  R0,0                                                             
         AR    RF,R0                                                            
         CR    RF,R3                                                            
         BL    DISBX               IF CURRENT TSAR WONT FIT                     
         GOTO1 ASCRNCLR,DISLINE                                                 
         LA    R3,1(R3)                                                         
         STC   R3,DISLINE          DISPLAY BALNCE C/F                           
         LA    R2,ENQDATLH                                                      
         USING FLDHDRD,R2                                                       
         LA    R3,FLDDATA                                                       
         MVC   FLDDATA(L'ENQDAT1),SPACES                                        
         USING SCRLINED,R3                                                      
         MVC   SCRLCONT,MX@BALCF                                                
*                                                                               
DISB20   L     R4,ATSARREC         R4=A(TSAR RECORD)                            
         USING TSARRECD,R4                                                      
         LA    R4,TSARDATA         R4=A(TSAR RECORD DATA)                       
         USING TSARDATD,R4                                                      
         PACK  TEMP(L'TSDBFDR),NINES(L'SCRLDR-2) GET STRING OF NINES            
         CP    TEMP(L'TSDBFDR),TSDBFDR IS AMOUNT TOO HIGH TO FIT?               
         BL    DISB30                                                           
         MP    TEMP(L'TSDBFDR),=P'-1'                                           
         CP    TEMP(L'TSDBFDR),TSDBFDR IS AMOUNT TO LOW TO FIT?                 
         BH    DISB30                                                           
         CURED (P8,TSDBFDR),(L'SCRLDR,SCRLDR),2,MINUS=YES                       
         B     DISB40                                                           
DISB30   CURED (P8,TSDBFDR),(L'SCRLDR,SCRLDR),2,MINUS=YES,DECS=ROUND            
*                                                                               
DISB40   PACK  TEMP(L'TSDBFCR),NINES(L'SCRLCR-2) GET STRING OF NINES            
         CP    TEMP(L'TSDBFCR),TSDBFCR IS AMOUNT TOO HIGH TO FIT?               
         BL    DISB50                                                           
         MP    TEMP(L'TSDBFCR),=P'-1'                                           
         CP    TEMP(L'TSDBFCR),TSDBFCR IS AMOUNT TO LOW TO FIT?                 
         BH    DISB50                                                           
         CURED (P8,TSDBFCR),(L'SCRLCR,SCRLCR),2,MINUS=YES                       
         B     DISB60                                                           
DISB50   CURED (P8,TSDBFCR),(L'SCRLCR,SCRLCR),2,MINUS=YES,DECS=ROUND            
*                                                                               
DISB60   ZAP   DIFFO,TSDBFDR                                                    
         SP    DIFFO,TSDBFCR                                                    
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    *+12                                                             
         CLI   AGYCTRY,CTRYUSA                                                  
         BNE   DISB65                                                           
         CLC   ONER,BASKEY                                                      
         BNE   DISB65                                                           
         L     RF,AOPTVALS                                                      
         USING OPTVALSD,RF                                                      
         CLI   OBUKTYPE,C'H'       BUCKET TYPE ALREADY DEFINED?                 
         BNE   DISB65                                                           
         ZAP   DIFFO,TSDBFCR                                                    
         DROP  RF                                                               
DISB65   PACK  TEMP(L'DIFFO),NINES(L'SCRLDIF-2) GET STRING OF NINES             
         CP    TEMP(L'DIFFO),DIFFO IS AMOUNT TOO HIGH TO FIT?                   
         BL    DISB70                                                           
         MP    TEMP(L'DIFFO),=P'-1'                                             
         CP    TEMP(L'DIFFO),DIFFO IS AMOUNT TO LOW TO FIT?                     
         BH    DISB70                                                           
         CURED (P8,DIFFO),(L'SCRLDIF,SCRLDIF),2,MINUS=YES                       
         B     DISB80                                                           
DISB70   CURED (P8,DIFFO),(L'SCRLDIF,SCRLDIF),2,MINUS=YES,DECS=ROUND            
DISB80   OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         B     DISBX                                                            
*                                                                               
DISBX    B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*                                                                               
*********************************************************************           
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
ERRXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*********************************************************************           
*                                                                               
         LTORG                                                                  
*                                                                               
NINES    DC    C'99999999999999999999'                                          
ONEC     DC    C'1C'                                                            
ONEP     DC    C'1P'                                                            
ONER     DC    C'1R'                                                            
ONEJ     DC    C'1J'                                                            
SINCUL   DC    C'SI'                                                            
GPROUL   DS    C'GP'                                                            
         EJECT                                                                  
DCMIX    DS    0X                                                               
         DCDDL AC#ENH9,78                                                       
         DCDDL AC#ENH10,78                                                      
         DCDDL AC#ACC,9                                                         
         DCDDL AC#STCON,14                                                      
         DCDDL AC#BALBF,15                                                      
         DCDDL AC#BALCF,15                                                      
         DCDDL AC#SUBT,12                                                       
         DCDDL AC#TOTAL,9                                                       
         DCDDL AC#MOA,3                                                         
         DCDDL AC#BALBF,3                                                       
         DCDDL AC#DFRNC,L'MX@DFRNC                                              
         DCDDL AC#NAME,L'MX@NAME                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                                                               
*********************************************************************           
*        CONTRA LIST GRID COLUMN TABLE - COVERED BY GCTBLD                      
*********************************************************************           
GCTBL    DS    0F                                                               
*                                                                               
GCTCOL1  DC    AL1(GCT1LQ,91,L'LC@CTR,L'TSDCONT)    CONTRA CODE                 
         DC    AL2(LC@CTR-WORKD,TSDCONT-TSARDATD)                               
         DC    AL1(GCTITOT,0,0,0)                                               
         DC    AL1(0,L'MX@TOTAL),AL2(MX@TOTAL-OVERWRKD)                         
GCT1LQ   EQU   *-GCTCOL1                                                        
*                                                                               
GCTCOL2  DC    AL1(GCT2LQ,02,L'MX@NAME,L'TSDNAME)    CONTRA NAME                
         DC    AL2(MX@NAME-OVERWRKD,TSDNAME-TSARDATD)                           
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GCT2LQ   EQU   *-GCTCOL2                                                        
*                                                                               
GCTCOL3  DC    AL1(GCT3LQ,03,L'LC@DRS,L'TSDDR)      DEBITS                      
         DC    AL2(LC@DRS-WORKD,TSDDR-TSARDATD)                                 
         DC    AL1(GCTITOT,0,GCTFNUM+GCTFRGHT,0)                                
         DC    AL1(0,L'TSTDR),AL2(TSTDR-TSARTOTD)                               
GCT3LQ   EQU   *-GCTCOL3                                                        
*                                                                               
GCTCOL4  DC    AL1(GCT4LQ,04,L'LC@CRS,L'TSDCR)      CREDITS                     
         DC    AL2(LC@CRS-WORKD,TSDCR-TSARDATD)                                 
         DC    AL1(GCTITOT,0,GCTFNUM+GCTFRGHT,0)                                
         DC    AL1(0,L'TSTCR),AL2(TSTCR-TSARTOTD)                               
GCT4LQ   EQU   *-GCTCOL4                                                        
*                                                                               
GCTCOL5  DC    AL1(GCT5LQ,05,L'MX@DFRNC,L'TSDDIF)     DIFFERENCE                
         DC    AL2(MX@DFRNC-OVERWRKD,TSDDIF-TSARDATD)                           
         DC    AL1(GCTITOT+GCTIOVER,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,L'TSTDIF),AL2(TSTDIF-TSARTOTD)                             
GCT5LQ   EQU   *-GCTCOL5                                                        
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
*                                                                               
OVERWRKD DSECT                                                                  
MYOFFLST DS    CL10                                                             
TMPNBLK  DS    0CL40               TEMPORARY JOB NAME BLOCK                     
TMPNB1   DS    CL20                TEMP JOB NAME LINE 1                         
TMPNB2   DS    CL20                TEMP JOB NAME LINE 2                         
BALFLAG  DS    X                   BALANCE FLAG                                 
ADDQ     EQU   1                   ADD AMOUNTS                                  
DIFFO    DS    PL8                 DIFFERENCE BETWEEN DEBIT AND CREDIT          
TEMPDR   DS    PL8                 TEMP DEBITS                                  
TEMPCR   DS    PL8                 TEMP CREDITS                                 
PBDR     DS    PL8                                                              
PBCR     DS    PL8                                                              
*                                                                               
*********************************************************************           
*                                                                               
DSMIX    DS    0C                                                               
MX@ENH9  DS    CL78                                                             
MX@ENH10 DS    CL78                                                             
MX@ACC   DS    CL9                                                              
MX@STCON DS    CL14                                                             
MX@BALBF DS    CL15                                                             
MX@BALCF DS    CL15                                                             
MX@SUBT  DS    CL12                                                             
MX@TOTAL DS    CL9                                                              
MX@MOA   DS    CL3                                                              
MX@BFW   DS    CL3                                                              
MX@DFRNC DS    CL10              DIFFERENCE                                     
MX@NAME  DS    CL4               NAME                                           
         EJECT                                                                  
*                                                                               
*********************************************************************           
*                                                                               
SCRLINED DSECT                     COVER SCREEN ITEM LINE1                      
SCRLCONT DS    CL(L'CHDKULA)       CONTRA CODE                                  
         DS    CL1                                                              
SCRLNAME DS    CL20                CONTRA ACCOUNT NAME (PART 1)                 
         DS    CL4                                                              
SCRLDR   DS    CL11                PRESENT DEBITS FOR CONTRA                    
         DS    CL3                                                              
SCRLCR   DS    CL11                PRESENT CREDITS FOR CONTRA                   
         DS    CL3                                                              
SCRLDIF  DS    CL11                DIFFERENCE                                   
*                                                                               
*********************************************************************           
*                                                                               
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSITEM1  EQU   1                   ITEM 1 FORMAT                                
TSDCONT  DS    CL(L'TRNKULA)       CONTRA CODE                                  
TSDNAME  DS    CL(L'NAMEREC)       CONTRA NAME                                  
TSDDR    DS    PL8                 DEBITS                                       
TSDCR    DS    PL8                 CREDITS                                      
TSDDIF   DS    PL8                 DIFFERENCE                                   
TSDBFDR  DS    PL8                 DEBITS BROUGHT FORWARD                       
TSDBFCR  DS    PL8                 CREBITS BROUGHT FORWARD                      
TSDLENQ  EQU   *-TSARDATD                                                       
*                                                                               
*********************************************************************           
*                                                                               
TSARTOTD DSECT                     COVER SCREEN LINE 1                          
TSTLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSTFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSSUBITM EQU   2                   SUB TOTAL ITEM                               
TSBBFITM EQU   3                   BALANCE BROUGHT FORWARD ITEM                 
TSTOTITM EQU   4                   TOTAL ITEM TYPE                              
TSTCONT  DS    CL(L'TRNKULA)       SPACES                                       
TSTNAME  DS    CL(L'NAMEREC)       SUB/BBF                                      
TSTDR    DS    PL8                 DEBITS                                       
TSTCR    DS    PL8                 CREDITS                                      
TSTDIF   DS    PL8                 DIFFERENCE                                   
TSTLNQ   EQU   *-TSARTOTD                                                       
         EJECT                                                                  
*                                                                               
*********************************************************************           
*                                                                               
       ++INCLUDE ACENQWORK                                                      
         EJECT                                                                  
*                                                                               
*********************************************************************           
*                                                                               
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
ORELO    DS    A                                                                
AGCTBL   DS    A                   ADDRESS OF GRID COLUMN TABLE                 
UNILDG   DS    CL2                 UNIT AND LEDGER                              
DETFLAG  DS    X                   GENERAL FLAG                                 
DETGRINQ EQU   X'80'               SCREEN INITIALIZED FOR GRIDS                 
STATFLAG DS    X                   STATEMENT FLAG                               
TRNMDPQ  EQU   X'80'               TRANSACTION FOR THIS CONTRA/PERIOD           
BBFTRNQ  EQU   X'40'               ADD TRAN TO BAL BROUGHT FORWARD              
BBFFBLQ  EQU   X'20'               BAL BR FRWD FROM BAL ELEM                    
BCFOUTQ  EQU   X'08'               BCF PRINTED                                  
ROFFBUKQ EQU   X'04'               GET DATA FROM OFFICE BUCKETS                 
CONTCODE DS    CL(L'CHDKULA)       CONTRA CODE                                  
CONTNAME DS    CL(L'NAMEREC)       CONTRA NAME                                  
CLVALS   DS    0PL8                CREDITOR VALUES                              
DEBTOT   DS    PL8                 TOTAL DEBITS                                 
CRETOT   DS    PL8                 TOTAL CREDITS                                
BUCKDRS  DS    PL8                 BUCKET DEBIT                                 
BUCKCRS  DS    PL8                 BUCKET CREDIT                                
CLVALLNQ EQU   *-CLVALS                                                         
BALFDRS  DS    PL8                 BBF DEBIT                                    
BALFCRS  DS    PL8                 BBF CREDIT                                   
CLOSEDAT DS    XL(L'RSTBDATE)      LAST CLOSED MOS                              
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACENQ10   09/20/07'                                      
         END                                                                    
