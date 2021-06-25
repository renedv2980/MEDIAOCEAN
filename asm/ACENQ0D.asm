*          DATA SET ACENQ0D    AT LEVEL 011 AS OF 01/11/16                      
*PHASE T6200DA                                                                  
T6200D   TITLE 'ACCOUNT ENQUIRY - ORDER LIST'                                   
                                                                                
*TKLU 044 021215 <RD009622> ORDER NAME SEARCH SUPPORT                           
                                                                                
T6200D   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         NMOD1 0,**ENQD**,R7,CLEAR=YES,RR=RE                                    
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
         L     RE,ORELO                                                         
         A     RE,=A(GRDSP)        GRID SPECIAL ROUTINE                         
         ST    RE,AGRDSP                                                        
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
*                                                                               
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN10              NO                                           
         BAS   RE,FSTDIS           YES PERFORM FIRST DISPLAY FUNCTIONS          
         BNE   ERRXIT                                                           
         TM    DISPFLAG,NORECQ     HAVE NO ORDER RECORDS BEEN FOUND?            
         BO    MAINX               YES                                          
         TM    DISPFLAG,DISIOMAX   HAVE THE MAX IO'S BEEN REACHED?              
         BO    MAINX               YES                                          
         OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
         B     MAIN70                                                           
*                                                                               
MAIN10   TM    OVRSTAT,OVRGDONE                                                 
         BO    MAINXGX                                                          
*                                                                               
         TM    DISPFLAG,ALLREADQ   ALL RECORDS READ?                            
         BO    MAIN30              YES                                          
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BO    MAIN20              YES                                          
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,IOKEY            R3=A(IOAREA1 CONTAINING ORDER REC)           
*                                                                               
MAIN20   CLC   TSCURRNO,TSLSTREC   HAVE WE ALLREADY GOT RECORD IN TSAR?         
         BH    MAIN40              NO                                           
*                                                                               
MAIN30   GOTO1 ATSARGET,TSCURRNO   GET TSAR RECORD                              
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    *+12                                                             
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
         B     MAIN80                                                           
*                                                                               
MAIN40   TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BNO   MAIN50              YES                                          
         LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATOOMNY)                                           
         B     ERRXIT                                                           
*                                                                               
MAIN50   TM    DISPFLAG,ALLREADQ   HAVE ALL RECORDS BEEN READ?                  
         BO    MAINX               YES                                          
         B     *+10                                                             
MAIN55   MVC   KEYSAVE,IOKEY                                                    
         GOTO1 AIO,IOSEQ+IOACCDIR+IO1 READ NEXT ORDER RECORD                    
         BE    MAIN56                                                           
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
MAIN56   LA    R3,IOKEY                                                         
         USING ORDRECD,R3                                                       
         CLC   IOKEYSAV(ORDKORD-ORDKEY),0(R3) DID WE GET AN ORDER REC?          
         BE    MAIN70              IS RECORD AN ORDER RECORD?                   
         OC    TSLSTREC,TSLSTREC   ANY RECORDS DISPLAYED SO FAR?                
         BNZ   *+12                                                             
         OI    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY                        
         B     MAIN60                                                           
         TM    DISPFLAG,ALLREADQ                                                
         BO    *+12                                                             
         OI    DISPFLAG,ALLREADQ   ALL RECORDS READ                             
         BAS   RE,TOTAL            DEAL WITH TOTAL LINE                         
MAIN60   B     MAINX                                                            
*                                                                               
MAIN70   LA    R3,IOKEY            R3=A(ORDER KEY)                              
         BAS   RE,FILTKEY          APPLY FILTERING TO ORDER KEY                 
         BNE   MAIN55              DO WE WANT THIS RECORD?                      
         GOTO1 AIO,IOGET+IOACCMST+IO1 READ NEXT ORDER RECORD                    
         BE    MAIN71                                                           
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
MAIN71   L     R3,AIO1                                                          
         CLI   ORDRFST-ORDKEY(R3),ONCELQ DON'T WANT ORDER NR CONTR RE           
         BE    MAIN55                                                           
         BAS   RE,FILTER           APPLY FILTERING TO ORDER RECORD              
         BNE   MAIN55              DO WE WANT TO KEEP THIS RECORD?              
         BAS   RE,OFFSEC           APPLY SECURITY CHECK                         
         TM    DISPFLAG,DISIOMAX                                                
         BO    MAINX                                                            
         TM    INTFLAG,NOTAUTHQ                                                 
         BO    MAIN55                                                           
         MVC   KEYSAVE,ORDKEY      SAVE THE KEY FOR SEQRESTORE                  
         BAS   RE,BLDTSDAT         BUILD TSAR RECORD FOR DATA LINE              
         BE    *+6                                                              
         DC    H'0'                                                             
*        BNE   ERRXIT                                                           
         CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BNL   MAIN80              YES                                          
         SR    RF,RF               UPDATE TSAR RECORD NUMBER COUNT              
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN55                                                           
*                                                                               
MAIN80   GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   MAINX               SCREEN IS FULL                               
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN20                                                           
*                                                                               
MAINX    GOTO1 ASCRNCLR,DISLINE    CLEAR REST OF THE SCREEN                     
         B     OKXIT                                                            
*                                                                               
MAINXGX  GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),0                                 
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    OKXIT               SCREEN IS FULL                               
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
         SPACE 1                                                                
FSTDIS   NTR1                                                                   
         ZAP   ORDTOT,=P'0'        ORDER AMIOUNT TOTAL                          
         ZAP   INVTOT,=P'0'        INVOICED TODATE TOTAL                        
         MVC   LDGTUL,SPACES       CLEAR  SAVED UNIT/LEDG                       
         MVC   UNILDG,SPACES                                                    
         LA    R2,BASCACH          R2=A(CONTRA ACCOUNT FIELD)                   
         USING FLDHDRD,R2                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EGIFNOTV) INPUT IN THIS FIELD IS NOT VALID          
         CLI   FLDILEN,0           INPUT NOT ALLOWED IN THIS FIELD              
         BNE   ERRXIT                                                           
         OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
         LA    R2,BASKEYH          R2=A(KEY FIELD)                              
         USING FLDHDRD,R2                                                       
         ST    R2,FVADDR                                                        
*                                                                               
         CLI   FLDDATA,C'='        SEARCH?                                      
         JNE   FSTD04S                                                          
         GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,ORD,ACOMFACS,0                    
*                                                                               
FSTD04S  LA    R3,IOKEY            R3=A(KEY FOR ORDER RECORD READ)              
         USING ORDRECD,R3                                                       
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,MYCO                                                     
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        RF=L'(KEY FIELD INPUT)                       
         BZ    FSTD10                                                           
         CLI   FLDILEN,L'ORDKORD   ENSURE LENGTH NOT TOO LONG                   
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         B     FSTDERR                                                          
         LA    RE,ORDKORD                                                       
         CLI   AGYCTRY,CTRYUSA                                                  
         BE    FSTD05                                                           
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    FSTD05                                                           
         MVC   ORDKORD,=C'000000'                                               
         LA    RE,ORDKORD+L'ORDKORD RIGHT JUSTIFY THE ORDER NUMBER              
         SR    RE,RF                                                            
FSTD05   BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),FLDDATA                                                  
FSTD10   OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1 READ FIRST ORDER RECORD                  
         TM    IOERR,IOMAX             TOO MANY IOS?                            
         BNO   *+12                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     FSTDX                                                            
         LA    R3,IOKEY                R3=A(IOAREA1)                            
         CLC   IOKEYSAV(ORDKORD-ORDKEY),0(R3) DID WE GET AN ORDER REC?          
         BE    *+12                                                             
         OI    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY                        
         B     FSTDX                                                            
*                                                                               
         LA    R2,GRDDAT1H                                                      
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    FSTD30                                                           
         LA    R2,ENQDAT1H         DISPLAY SCREEN HEADINGS                      
         USING FLDHDRD,R2                                                       
         MVC   FLDDATA(L'MX@ENH3),MX@ENH3                                       
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         MVC   FLDDATA(L'MX@ENH4),MX@ENH4                                       
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
*                                                                               
FSTD30   LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    FSTD40                                                           
         GOTO1 ASCRNDIM,DMCB,(1,(R2))                                           
         B     FSTD50                                                           
*                                                                               
FSTD40   LA    R2,GRDDAT1H                                                      
         GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
         LA    R2,GRDPFAH                                                       
         MVC   GRDPFA(2),LC@PFK    DISPLAY PF ON LAST LINE OF SCREEN            
*                                  LAST RECD WAS MISSING AND NEED PF            
*                                  FOR EMULATOR FOR GRIDS                       
         OI    FLDOIND,FOUTTRN                                                  
         B     *+10                                                             
*                                                                               
FSTD50   L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
*                                                                               
FSTDX    CR    RB,RB                                                            
         B     *+6                                                              
FSTDERR  LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        APPLY SECURITY CHECKING TO ACCOUNT RECORD                    *         
* ON ENTRY AIOAREA1 CONTAINS ORDER RECORD                                       
* ON EXIT CC EQUAL USE THE ORDER RECORD                               *         
*         CC UNEQUAL DISCARD THE ORDER RECORD                         *         
***********************************************************************         
         SPACE 1                                                                
OFFSEC   NTR1                                                                   
         USING ORDRECD,R3                                                       
         CLI   TERMACCS,C'*'       LIMIT ACCESS?                                
         BE    *+12                                                             
         CLI   TERMACCS,C'$'                                                    
         BNE   OFFSX                                                            
         MVC   SVORDKEY,ORDKEY                                                  
         NI    INTFLAG,X'FF'-NOTAUTHQ                                           
         LA    R4,ORDRFST          R4=A(FIRST ELEMENT)                          
*                                                                               
OFFS10   CLI   0(R4),EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),ORDELQ        PRODUCTION ORDER ELEMENT?                    
         BE    OFFS30                                                           
*                                                                               
OFFS20   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     OFFS10                                                           
*                                                                               
         USING ORDELD,R4           PRODUCTION ORDER ELEMENT                     
OFFS30   CLC   LDGTUL,ORDACCU      GET ACCOUNT RECORD                           
         BE    OFFS35                                                           
         GOTO1 AUNITLDG,ORDACCU                                                 
         MVC   LDGTUL,ORDACCU                                                   
         MVC   UNILDG,ORDACCU                                                   
OFFS35   LA    R3,IOKEY            GET ACCOUNT RECORD                           
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,ORDJOB                                                  
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    OFFS40                                                           
         TM    IOERR,IOERNF                                                     
         BO    OFFS50                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     OFFSX                                                            
*                                                                               
OFFS40   GOTO1 AOFFACC                                                          
         BE    *+8                                                              
         OI    INTFLAG,NOTAUTHQ                                                 
         TM    DISPFLAG,DISIOMAX                                                
         BO    OFFSX                                                            
OFFS50   MVC   IOKEY,SVORDKEY                                                   
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    OFFSX                                                            
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     OFFSX                                                            
*                                                                               
OFFSX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        APPLY FILTERS TO THE ORDER KEY                               *         
* ON ENTRY R3=A(ORDER RECORD KEY)                                     *         
* ON EXIT CC EQUAL USE THE ORDER RECORD                               *         
*         CC UNEQUAL DISCARD THE ORDER RECORD                         *         
***********************************************************************         
         SPACE 1                                                                
FILTKEY  NTR1                                                                   
         USING ORDRECD,R3                                                       
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         CLC   ORDKORD,CNTRLREC                                                 
         BE    FILTKRJX            WE DONT WANT CONTROL RECORDS                 
         CLI   ORDKSEQ,0                                                        
         BNE   FILTKRJX            WE DONT WANT NARRATIVE RECS                  
*                                                                               
         TM    ORDKSTAT,ORDSFMCH   IS ORDER FULLY MATCHED?                      
         BNO   FILTK10                                                          
         CLI   OFMATCH,C'N'        REJECT FULLY MATCHED?                        
         BE    FILTKRJX                                                         
         CLI   OFULLDEL,C'N'       REJECT FULLY MATCHED OR DELETED?             
         BE    FILTKRJX                                                         
         B     FILTK30                                                          
FILTK10  CLI   OFULLDEL,C'O'       DO WE WANT FULLY MATCHED OR DELETED?         
         BE    FILTK20                                                          
         CLI   OFMATCH,C'O'        WE WANT FULLY MATCHED SO REJECT              
         BE    FILTKRJX                                                         
         B     FILTK30                                                          
FILTK20  TM    ORDKSTAT,ORDSLDEL   NOT DELETED OR MATCHED SO REJECT             
         BNO   FILTKRJX                                                         
         B     *+12                                                             
FILTK30  TM    ORDKSTAT,ORDSLDEL   IS ORDER DELETED                             
         BNO   FILTK40                                                          
         CLI   ODELETED,C'N'       REJECT DELETED ORDERS?                       
         BE    FILTKRJX                                                         
         CLI   OFULLDEL,C'N'       REJECT FULLY MATCHED OR DELETED?             
         BE    FILTKRJX                                                         
         B     FILTK50                                                          
FILTK40  CLI   OFULLDEL,C'O'       DO WE WANT FULLY MATCHED OR DELETED?         
         BE    FILTK150                                                         
         CLI   ODELETED,C'O'       WE WANT DELETED SO REJECT                    
         BE    FILTKRJX                                                         
*                                                                               
*                                                                               
FILTK50  TM    ORDKSTA2,ORDSDRFT   DRAFT OR IN PROGRESS ?                       
         BNO   FILTK60                                                          
         CLI   OINPRGRS,C'Y'                                                    
         BE    FILTK70                                                          
         CLI   OINPRGRS,C'O'                                                    
         BE    FILTK150                                                         
         CLI   OINPRGRS,C'N'                                                    
         BE    FILTKRJX                                                         
FILTK60  CLI   OINPRGRS,C'O'                                                    
         BE    FILTKRJX                                                         
FILTK70  DS    0H                                                               
         TM    ORDKSTA2,ORDSPAPP   PART APPROVED?                               
         BNO   FILTK80                                                          
         CLI   OPARTAPP,C'Y'                                                    
         BE    FILTK90                                                          
         CLI   OPARTAPP,C'O'                                                    
         BE    FILTK150                                                         
         CLI   OPARTAPP,C'N'                                                    
         BE    FILTKRJX                                                         
FILTK80  CLI   OPARTAPP,C'O'                                                    
         BE    FILTKRJX                                                         
FILTK90  DS    0H                                                               
         TM    ORDKSTA,ORDGDRCV    GOODS RECEIVED = APPROVED                    
         BO    FILTK95                                                          
         TM    ORDKSTA2,ORDSAPPR   APPROVED?                                    
         BNO   FILTK100                                                         
FILTK95  CLI   OSELECT,C'Y'                                                     
         BE    FILTK110                                                         
         CLI   OSELECT,C'O'                                                     
         BE    FILTK150                                                         
         CLI   OSELECT,C'N'                                                     
         BE    FILTKRJX                                                         
FILTK100 CLI   OSELECT,C'O'                                                     
         BE    FILTKRJX                                                         
FILTK110 DS    0H                                                               
         TM    ORDKSTA2,ORDSOREJ   REJECTED?                                    
         BNO   FILTK120                                                         
         CLI   OREJECT,C'Y'                                                     
         BE    FILTK130                                                         
         CLI   OREJECT,C'O'                                                     
         BE    FILTK150                                                         
         CLI   OREJECT,C'N'                                                     
         BE    FILTKRJX                                                         
FILTK120 CLI   OREJECT,C'O'                                                     
         BE    FILTKRJX                                                         
FILTK130 DS    0H                                                               
         TM    ORDKSTA2,ORDSSUBM   SUBMITTED?                                   
         BNO   FILTK140                                                         
         CLI   OSUBMIT,C'Y'                                                     
         BE    FILTK142                                                         
         CLI   OSUBMIT,C'O'                                                     
         BE    FILTK150                                                         
         CLI   OSUBMIT,C'N'                                                     
         BE    FILTKRJX                                                         
FILTK140 CLI   OSUBMIT,C'O'                                                     
         BE    FILTKRJX                                                         
*                                                                               
FILTK142 DS    0H                                                               
*        TM    ORDKSTAT,ORDGDRCV   RECEIVED ?                                   
*        BNO   FILTK145                                                         
*        CLI   ORECEIVE,C'Y'                                                    
*        BE    FILTK150                                                         
*        CLI   ORECEIVE,C'O'                                                    
*        BE    FILTK150                                                         
*        CLI   ORECEIVE,C'N'                                                    
*        BE    FILTKRJX                                                         
* FILTK145 CLI   ORECEIVE,C'O'                                                  
*        BE    FILTKRJX                                                         
*         B     FILTKX                                                          
*                                                                               
FILTK150 DS    0H                                                               
         OC    OOFF1,OOFF1        OFFICE FILTER                                 
         BZ    FILTK160                                                         
         GOTO1 ASCOMP,DMCB,ORDSOFF,(OOFF1LN1,OOFF1VL1),                C        
               (OOFF1LN2,OOFF1VL2),OOFF1FI                                      
         BNE   FILTREJX                                                         
*                                                                               
FILTK160 DS    0H                                                               
         B     FILTKX                                                           
*                                                                               
FILTKX   CR    RB,RB               USE RECORD                                   
         B     *+6                                                              
FILTKRJX LTR   RB,RB               REJECT RECORD                                
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        APPLY FILTERS TO THE ORDER RECORD                            *         
* ON ENTRY R3=A(IOAREA1 CONTAINING AN ORDERRECORD)                    *         
* ON EXIT CC EQUAL USE THE ORDER RECORD                               *         
*         CC UNEQUAL DISCARD THE ORDER RECORD                         *         
***********************************************************************         
         SPACE 1                                                                
FILTER   NTR1                                                                   
         USING ORDRECD,R3                                                       
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         LA    R4,ORDRFST          R4=A(FIRST ELEMENT)                          
         MVI   FILFLAG,0           INIT FILTER FLAG                             
*                                                                               
FILT60   CLI   0(R4),EOR           END OF RECORD?                               
         BE    FILT400                                                          
         CLI   0(R4),ORDELQ        PRODUCTION ORDER ELEMENT?                    
         BE    FILT80                                                           
         CLI   0(R4),OAMELQ        ORDER AMOUNT ELEMENT?                        
         BE    FILT210                                                          
         CLI   0(R4),RACELQ        RECORD ACTIVITY ELEMENT?                     
         BE    FILT300                                                          
         CLI   0(R4),TRSELQ        TRANSACTION STATUS ELEMENT?                  
         BE    FILT310                                                          
*                                                                               
FILT70   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     FILT60                                                           
*                                                                               
         USING ORDELD,R4           PRODUCTION ORDER ELEMENT                     
FILT80   OC    OODATE,OODATE       FILTERING ON ORDER DATE?                     
         BZ    FILT85                                                           
         GOTO1 ADCOMP,DMCB,(L'ORDDATE,ORDDATE),OODATEST,OODATEEN,      X        
               OODATEFI                                                         
         BNE   FILTKRJX                                                         
*                                                                               
FILT85   OC    ODATE,ODATE         FILTERING ON ORDER DATE?                     
         BZ    FILT90                                                           
         GOTO1 ADCOMP,DMCB,(L'ORDDATE,ORDDATE),ODATEST,ODATEEN,        X        
               ODATEFI                                                          
         BNE   FILTKRJX                                                         
                                                                                
FILT90   OC    OSUPP,OSUPP         SUPPLIER FILTER?                             
         BZ    FILT100                                                          
         GOTO1 ASCOMP,DMCB,ORDSUPA,(OSUPPLN1,OSUPPVL1),(OSUPPLN2,OSUPPV*        
               L2),OSUPPFI                                                      
         BNE   FILTKRJX                                                         
*                                                                               
FILT100  OC    OCLIENT,OCLIENT     CLIENT FILTER?                               
         BZ    FILT110                                                          
         GOTO1 ASCOMP,DMCB,ORDACCA,(OCLINLN1,OCLINVL1),(OCLINLN2,OCLINV*        
               L2),OCLINFI                                                      
         BNE   FILTKRJX                                                         
*                                                                               
FILT110  OC    OPROD,OPROD         PRODUCT FILTER?                              
         BZ    FILT120                                                          
         GOTO1 ASCOMP,DMCB,ORDACCA,(OPRODLN1,OPRODVL1),(OPRODLN2,OPRODV*        
               L2),OPRODFI                                                      
         BNE   FILTKRJX                                                         
*                                                                               
FILT120  OC    OJOB,OJOB           JOB FILTER?                                  
         BZ    FILT180                                                          
         GOTO1 ASCOMP,DMCB,ORDACCA,(OJOBLN1,OJOBVL1),(OJOBLN2,OJOBVL2),*        
               OJOBFI                                                           
         BNE   FILTKRJX                                                         
*                                                                               
FILT180  CLC   SPROUNIT(L'SPROUNIT+L'SPROLEDG),ORDACCU                          
         BE    FILT190                                                          
         CLI   OTYPE1,PRODUCTN     PRODUCTION FILTER                            
         BE    FILTREJX                                                         
         B     FILT200                                                          
FILT190  CLI   OTYPE1,EXPENSE      EXPENSE FILTER?                              
         BE    FILTREJX                                                         
*                                                                               
FILT200  B     FILT70                                                           
         DROP  R4                                                               
*                                                                               
         USING OAMELD,R4           ORDER AMOUNT ELEMENT                         
FILT210  OC    OWCODE,OWCODE       WORKCODE FILTER?                             
         BZ    FILT230                                                          
         SR    RF,RF                                                            
         IC    RF,OWCODELN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   OWCODEVL(0),OAMWORK                                              
         BNE   FILT220                                                          
         CLI   OWCODEFI,NEGFILTR                                                
         BE    FILT230                                                          
         OI    FILFLAG,FILMWCQ     SET MATCH ON WORKCODE                        
         B     FILT230                                                          
FILT220  CLI   OWCODEFI,NEGFILTR                                                
         BNE   FILT230                                                          
         OI    FILFLAG,FILMWCQ     SET MATCH ON WORKCODE                        
*                                                                               
FILT230  TM    ORDRSTAT,ORDSFMCH   IS ORDER PARTLY MATCHED?                     
         BNO   FILT235                                                          
         CLI   OPMATCH,C'O'        REJECT PARTLY MATCHED?                       
         BE    FILTREJX                                                         
         CLI   OPARTDEL,C'O'       DO WE WANT PART MATCHED OR DELETED?          
         BNE   FILT235                                                          
         TM    ORDRSTAT,ORDSLDEL   DELETED?                                     
         BNO   FILTREJX                                                         
FILT235  CP    OAMINUM,=P'0'       IS ORDER PARTLY MATCHED?                     
         BE    FILT240                                                          
         CLI   OPMATCH,C'N'        REJECT PARTLY MATCHED?                       
         BE    FILTREJX                                                         
         CLI   OPARTDEL,C'N'       REJECT PARTLY MATCHED OR DELETED?            
         BE    FILTREJX                                                         
         B     FILT260                                                          
FILT240  CLI   OPARTDEL,C'O'       DO WE WANT PART MATCHED OR DELETED?          
         BE    FILT250                                                          
         CLI   OPMATCH,C'O'        WE WANT PARTLY MATCHED SO REJECT             
         BE    FILTREJX                                                         
         B     FILT260                                                          
FILT250  TM    ORDRSTAT,ORDSLDEL   NOT DELETED OR MATCHED SO REJECT             
         BNO   FILTREJX                                                         
         B     *+12                                                             
FILT260  TM    ORDRSTAT,ORDSLDEL   IS ORDER DELETED                             
         BNO   FILT270                                                          
         CLI   ODELETED,C'N'       REJECT DELETED ORDERS?                       
         BE    FILTREJX                                                         
         CLI   OPARTDEL,C'N'       REJECT PARTLY MATCHED OR DELETED?            
         BE    FILTREJX                                                         
         B     FILT70                                                           
FILT270  CLI   OPARTDEL,C'O'       DO WE WANT PARTLY MTCHED OR DELETED?         
         BE    FILT70                                                           
         CLI   ODELETED,C'O'       WE WANT DELETED SO REJECT                    
         BE    FILTREJX                                                         
         B     FILT70                                                           
         DROP  R4                                                               
*                                                                               
         USING RACELD,R4                                                        
FILT300  DS    0H                                                               
         CLI   RACTYPE,RACTADD     IS THIS ADD TYPE                             
         BNE   FILT70                                                           
         OC    OACT,OACT           FILTERING ON ORDER DATE?                     
         BZ    FILT70                                                           
         OI    FILFLAG,FLGCDATE                                                 
         GOTO1 ADCOMP,DMCB,(L'RACDATE,RACDATE),OACTST,OACTEN,OACTFI             
         BNE   FILTKRJX                                                         
         B     FILT70                                                           
         DROP  R4                                                               
*                                                                               
         USING TRSELD,R4                                                        
FILT310  DS    0H                                                               
         OC    OACT,OACT           FILTERING ON ORDER DATE?                     
         BZ    FILT70                                                           
         OI    FILFLAG,FLGCDATE                                                 
         OC    TRSDATE,TRSDATE                                                  
         BZ    FILTKRJX                                                         
         GOTO1 VDATCON,DMCB,(2,TRSDATE),(1,WORK) ACTIVITY DATE                  
         GOTO1 ADCOMP,DMCB,(L'RACDATE,WORK),OACTST,OACTEN,OACTFI                
         BNE   FILTKRJX                                                         
         B     FILT70                                                           
         DROP  R4                                                               
*                                                                               
FILT400  DS    0H                                                               
         OC    OACT,OACT           DO WE WANT TO FILTER ON CREATED DATE         
         BZ    FILT410             NO.                                          
         TM    FILFLAG,FLGCDATE    FOUND X'60' OR 'F9' ?                        
         BNO   FILTKRJX                                                         
         NI    FILFLAG,X'FF'-FLGCDATE                                           
*                                                                               
FILT410  OC    OWCODE,OWCODE       WORKCODE FILTER?                             
         BZ    FILT420                                                          
         TM    FILFLAG,FILMWCQ     MATCH ON WORKCODE?                           
         BNO   FILTREJX                                                         
         B     FILTX                                                            
FILT420  DS    0H                                                               
*                                                                               
FILTX    CR    RB,RB               USE RECORD                                   
         B     *+6                                                              
FILTREJX LTR   RB,RB               REJECT RECORD                                
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        BUILD TSAR RECORD FOR SCREEN DATA ITEM,                      *         
*        FILL DUMMY SCREEN LINES                                      *         
* ON ENTRY R3=A(AIO AREA CONTAINING ORDER RECORD)                     *         
* ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL                              *         
***********************************************************************         
         SPACE 1                                                                
BLDTSDAT NTR1                                                                   
         USING ORDRECD,R3                                                       
         GOTO1 ASETELE,ORDRFST     SET ELEMENT ADDRESSES                        
*                                                                               
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LH    R1,=Y(TSARRECL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ATSARREC                                                      
         USING TSARRECD,R2                                                      
         LH    RF,=Y(TSDLENQ)                                                   
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO                                                
         LA    R2,TSARDATA                                                      
         USING TSARDATD,R2                                                      
         MVC   TSDORDNO,ORDKORD    ORDER NUMBER                                 
         ZAP   TSDOAMT,=P'0'                                                    
         ZAP   TSDIAMT,=P'0'                                                    
         MVI   TSDFMT,TSDATLN1     SCREEN DATA ITEM 1                           
         LA    R4,ORDRFST          R4=A(FIRST ELEMENT)                          
         TM    ORDRSTAT,ORDSFMCH                                                
         BO    BLDT10                                                           
         TM    ORDRSTAT,ORDSLDEL                                                
         BNO   BLDT12                                                           
         MVC   TSDSTATB(2),UP@DELD ORDER IS DELETED                             
         B     BLDT20                                                           
BLDT10   TM    ORDRSTAT,ORDSLDEL                                                
         BO    *+14                                                             
         MVC   TSDSTATB(1),UP@FMTCS FULLY MATCHED                               
         B     BLDT20                                                           
         MVC   TSDSTATB,UP@FULDS   FULLY MATCHED AND DELETED                    
         B     BLDT20                                                           
*                                                                               
BLDT12   DS    0H                                                               
         TM    ORDRSTAT,ORDGDRCV    IS ORDER RECEIVED?                          
         BNO   *+14                                                             
*        MVC   TSDSTATB(2),MX@RCVD  RECEIVED ORDER                              
         MVC   TSDSTATB(2),UP@APRVS APPROVED,AS PER HWEI CQTST00021390          
         B     BLDT20                                                           
*                                                                               
         TM    ORDRSTA2,ORDSDRFT                                                
         BNO   *+14                                                             
         MVC   TSDSTATB(2),MX@INPR  DRAFT OR IN PROGRESS ORDER                  
         B     BLDT20                                                           
*                                                                               
         TM    ORDRSTA2,ORDSPAPP   IS THIS ORDER PARTIALLY APPROVED?            
         BNO   *+14                                                             
         MVC   TSDSTATB(2),MX@PAPPR PARTIALLY APPROVED                          
         B     BLDT20                                                           
*                                                                               
         TM    ORDRSTA2,ORDSAPPR   IS THIS ORDER APPROVED?                      
         BNO   *+14                                                             
         MVC   TSDSTATB(2),UP@APRVS APPROVED                                    
         B     BLDT20                                                           
*                                                                               
         TM    ORDRSTA2,ORDSOREJ   IS THIS ORDER REJECTED?                      
         BNO   *+14                                                             
         MVC   TSDSTATB(2),MX@REJEC ORDER REJECTED                              
         B     BLDT20                                                           
*                                                                               
         TM    ORDRSTA2,ORDSSUBM   IS THIS ORDER SUBMITTED?                     
         BNO   *+14                                                             
         MVC   TSDSTATB(2),MX@SUBMT ORDER SUBMITTED                             
         B     BLDT20                                                           
*                                                                               
BLDT20   CLI   0(R4),EOR           END OF RECORD?                               
         BE    BLDT100                                                          
         CLI   0(R4),ORDELQ        PRODUCTION ORDER ELEMENT?                    
         BE    BLDT40                                                           
         CLI   0(R4),OAMELQ        ORDER AMOUNT ELEMENT?                        
         BE    BLDT50                                                           
         CLI   0(R4),TRSELQ        ORDER STATUS ELEMENT?                        
         BE    BLDT80                                                           
*                                                                               
BLDT30   SR    R0,R0               BUMP TO NEXT RECORD                          
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     BLDT20                                                           
*                                                                               
         USING ORDELD,R4           PRODUCTION ORDER ELEMENT                     
BLDT40   MVC   TSDORDDT,ORDDATE    ORDER DATE                                   
         MVC   TSDSUPP,ORDSUPU                                                  
         CLC   SPROUNIT(L'SPROUNIT+L'SPROLEDG),ORDACCU PROD OR EXP?             
         BE    *+14                                                             
         MVC   TSDJBEX,ORDACCU     SHOW UNIT AND LEDGER FOR EXPENSE             
         B     *+10                                                             
         MVC   TSDJBEX(L'ORDACCA),ORDACCA ONLY SHOW ACCOUNT FOR PRODCTN         
         B     BLDT30                                                           
         DROP  R4                                                               
*                                                                               
         USING OAMELD,R4           ORDER AMOUNT ELEMENT                         
BLDT50   CLI   AGYCTRY,CTRYCAN                                                  
         BE    *+12                                                             
         CLI   AGYCTRY,CTRYUSA                                                  
         BNE   BLDT60                                                           
         TM    ORDRSTAT,ORDSFMCH   FULLY MATCHED?                               
         BO    BLDT60                                                           
         CP    OAMINUM,=P'0'       PART MATCHED?                                
         BE    BLDT60                                                           
         MVC   TSDSTATB,SPACES     CLEAR STATUS                                 
         TM    ORDRSTAT,ORDSLDEL   PART MATCHED AND DELETED?                    
         BO    *+14                                                             
         MVC   TSDSTATB(1),MX@PART SET PART MATCHED                             
         B     BLDT60                                                           
         MVC   TSDSTATB,MX@PARDL   SET PART MATCHED AND DELETED                 
BLDT60   LA    RF,TSDWC                                                         
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    *+12                                                             
         CLI   AGYCTRY,CTRYUSA                                                  
         BNE   BLDT70                                                           
         LA    R0,MAXPOWCQ         MAX WORKCODES ON ORDER                       
         CLC   0(L'OAMWORK,RF),SPACES                                           
         BNH   *+14                                                             
         LA    RF,L'OAMWORK(RF)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
BLDT70   MVC   0(L'OAMWORK,RF),OAMWORK                                          
         AP    TSDOAMT,OAMAMNT                                                  
         AP    TSDIAMT,OAMIVAL                                                  
*                                                                               
         AP    ORDTOT,OAMAMNT      ORDER AMOUNT TOTAL                           
         AP    INVTOT,OAMIVAL      INVOICE AMOUNT TOTAL                         
         B     BLDT30                                                           
         DROP  R4                                                               
*                                                                               
         USING TRSELD,R4                                                        
BLDT80   OC    TRSDATE,TRSDATE                                                  
         BZ    BLDT30                                                           
         MVC   TSDCREDT,TRSDATE    GET CREATE DATE                              
         B     BLDT30                                                           
         DROP  R4                                                               
*                                                                               
BLDT100  TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    *+12                                                             
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
         MVC   TSDLINES,LINSUSED                                                
*                                                                               
         GOTO1 ATSARADD            ADD RECORD                                   
         BE    *+12                                                             
         TM    DISPFLAG,DISIOMAX                                                
         BNO   BLDTERRX                                                         
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
*                                                                               
BLDTX    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
BLDTERRX LTR   RB,RB               TSAR BLOCK IS FULL                           
         B     BLDTX                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
FORMTSAR NTR1                                                                   
*                                                                               
         MVI   LINSUSED,0          NUMBER OF DISPLAY LINES                      
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR SCAN BLOCK                             
         LH    R1,=Y(DUMLINLN)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         CLI   TSARFMT,TSTOTLN1    TOTAL LINE OR REGULAR DATA LINE?             
         BE    FORM15                                                           
*                                                                               
         USING SCRLIN1D,R2         DSECT FOR NORMAL DATA LINE                   
         LA    R4,TSARDATA         R4=A(TSAR RECORD DATA)                       
         USING TSARDATD,R4                                                      
         MVC   SCRORDNO,TSDORDNO   ORDER NUMBER                                 
         GOTO1 VDATCON,DMCB,(1,TSDORDDT),(17,SCRORDDT) ORDER DATE               
         MVC   SCRSUPP,TSDSUPP     SUPPLIER U/L/ACC                             
         MVC   SCRJBEX,TSDJBEX     JOB/EXP U/L/ACC                              
         MVC   SCRWC(L'OAMWORK),TSDWC WORK CODE                                 
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    *+12                                                             
         CLI   AGYCTRY,CTRYUSA                                                  
         BNE   FORM04                                                           
         CLC   TSDWC+L'OAMWORK(L'OAMWORK),SPACES                                
         BNH   FORM04                                                           
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OEXP-OPTVALSD(RF),C'Y'                                           
         BE    FORM04                                                           
         MVI   SCRWC+L'OAMWORK,C'>'                                             
FORM04   MVC   SCRSTAT,TSDSTATB    STATUS                                       
*                                                                               
         PACK  TEMP(L'TSDOAMT),NINES(L'SCROAMT-2) GET STRING OF NINES           
         CP    TEMP(L'TSDOAMT),TSDOAMT IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM05                                                           
         MP    TEMP(L'TSDOAMT),=P'-1'                                           
         CP    TEMP(L'TSDOAMT),TSDOAMT IS AMOUNT TO LOW TO FIT?                 
         BH    FORM05                                                           
         CURED (P6,TSDOAMT),(L'SCROAMT,SCROAMT),2,MINUS=YES                     
         B     FORM06                                                           
FORM05   CURED (P6,TSDOAMT),(L'SCROAMT,SCROAMT),2,MINUS=YES,DECS=ROUND          
*                                                                               
FORM06   PACK  TEMP(L'TSDIAMT),NINES(L'SCRIAMT-2) GET STRING OF NINES           
         CP    TEMP(L'TSDIAMT),TSDIAMT IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM07                                                           
         MP    TEMP(L'TSDIAMT),=P'-1'                                           
         CP    TEMP(L'TSDIAMT),TSDIAMT IS AMOUNT TO LOW TO FIT?                 
         BH    FORM07                                                           
         CURED (P6,TSDIAMT),(L'SCRIAMT,SCRIAMT),2,ZERO=BLANK,MINUS=YES          
         B     FORM08                                                           
FORM07   CURED (P6,TSDIAMT),(L'SCRIAMT,SCRIAMT),2,ZERO=BLANK,MINUS=YES,*        
               DECS=ROUND                                                       
FORM08   MVI   LINSUSED,1          NUMBER OF DUMMY SCREEN LINES USED            
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OEXP-OPTVALSD(RF),C'Y'                                           
         BNE   FORMTX                                                           
         CLC   TSDWC+L'OAMWORK(L'OAMWORK),SPACES                                
         BNH   FORMTX                                                           
         LA    R2,L'DUMLIN1(R2)                                                 
         MVI   LINSUSED,2          NUMBER OF DUMMY SCREEN LINES USED            
         LA    RE,SCRWC                                                         
         LA    RF,TSDWC+L'OAMWORK                                               
         LA    R0,MAXMFWCQ-1                                                    
         LHI   R6,MAXPOWCQ-1                                                    
FORM09   MVC   0(L'OAMWORK,RE),0(RF)                                            
         CLC   L'OAMWORK(L'OAMWORK,RF),SPACES                                   
         BNH   FORMTX                                                           
         MVI   L'OAMWORK(RE),C','                                               
         LA    RE,L'OAMWORK+1(RE)                                               
         LA    RF,L'OAMWORK(RF)                                                 
         BCT   R6,FORM10                                                        
         B     FORMTX                                                           
FORM10   BCT   R0,FORM09                                                        
                                                                                
         LA    R2,L'DUMLIN1(R2)                                                 
         LA    RE,SCRWC                                                         
         LA    R0,MAXMFWCQ-1                                                    
         SR    R5,R5                                                            
         IC    R5,LINSUSED                                                      
         AHI   R5,1                                                             
         STC   R5,LINSUSED                                                      
         B     FORM09                                                           
         DROP  R2,R4                                                            
*                                                                               
         USING SCRTOT1D,R2         DSECT FOR TOTAL LINE                         
FORM15   LA    R2,L'DUMLIN1(R2)    BLANK LINE                                   
         LA    R4,TSARDATA         R4=A(TSAR RECORD DATA)                       
         USING TSARTOTD,R4                                                      
         MVC   SCRTOTAL,MX@TOTAL                                                
         PACK  TEMP(L'TSTOTOT),NINES(L'SCROTOT-2) GET STRING OF NINES           
         CP    TEMP(L'TSTOTOT),TSTOTOT IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM20                                                           
         MP    TEMP(L'TSTOTOT),=P'-1'                                           
         CP    TEMP(L'TSTOTOT),TSTOTOT IS AMOUNT TO LOW TO FIT?                 
         BH    FORM20                                                           
         CURED (P6,TSTOTOT),(L'SCROTOT,SCROTOT),2,MINUS=YES                     
         B     FORM30                                                           
FORM20   CURED (P6,TSTOTOT),(L'SCROTOT,SCROTOT),2,MINUS=YES,DECS=ROUND          
*                                                                               
FORM30   PACK  TEMP(L'TSTITOT),NINES(L'SCRITOT-2) GET STRING OF NINES           
         CP    TEMP(L'TSTITOT),TSTITOT IS AMOUNT TOO HIGH TO FIT?               
         BL    FORM40                                                           
         MP    TEMP(L'TSTITOT),=P'-1'                                           
         CP    TEMP(L'TSTITOT),TSTITOT IS AMOUNT TO LOW TO FIT?                 
         BH    FORM40                                                           
         CURED (P6,TSTITOT),(L'SCRITOT,SCRITOT),2,MINUS=YES                     
         B     FORM50                                                           
FORM40   CURED (P6,TSTITOT),(L'SCRITOT,SCRITOT),2,MINUS=YES,DECS=ROUND          
*                                                                               
FORM50   MVI   LINSUSED,2          NUMBER OF LINES USED                         
         MVI   DISATRIB,HILIGHTQ   DISPLAY ATTRIBUTES                           
         B     FORMTX                                                           
*                                                                               
FORMTX   B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES FOR GRIDS       *         
***********************************************************************         
FGRMTSAR NTR1                                                                   
*                                                                               
FGRM10   MVI   LINSUSED,0          NUMBER OF DISPLAY LINES                      
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR SCAN BLOCK                             
         LH    R1,=Y(DUMLINLN)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         USING TSARRECD,R3                                                      
         L     R3,ATSARREC                                                      
*                                                                               
         TM    OVRSTAT,OVRGINIT                                                 
         BO    FGRM20                                                           
         GOTO1 ADISGRD,DMCB,('DWNINIT',AGCTBL),UNILDG                           
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES                   
         B     FGRM10                                                           
*                                                                               
FGRM20   CLI   TSARFMT,TSTOTLN1    TOTAL LINE ITEM?                             
         BE    FGRM30                                                           
         CLI   TSARFMT,TSDATLN1                                                 
         BNE   FGRMX                                                            
*                                                                               
         GOTO1 ADISGRD,DMCB,(0,AGCTBL),UNILDG                                   
         B     FGRMX                                                            
*                                                                               
FGRM30   GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),UNILDG                            
*                                                                               
FGRMX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        DEAL WITH THE TOTAL LINE                                     *         
***********************************************************************         
         SPACE 1                                                                
TOTAL    NTR1                                                                   
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
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARTOTD,R3                                                      
         MVI   TSTFMT,TSTOTLN1     TOTAL LINE TYPE                              
         ZAP   TSTOTOT,ORDTOT      ORDER TOTAL                                  
         ZAP   TSTITOT,INVTOT      INVOICE TOTAL                                
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    *+12                                                             
         BAS   RE,FORMTSAR                                                      
         B     *+8                                                              
         BAS   RE,FGRMTSAR                                                      
         MVC   TSTLINES,LINSUSED                                                
         GOTO1 ATSARADD                                                         
         MVC   TSLSTREC,TSCURRNO                                                
         GOTO1 ADISPLAY,DISATRIB   DISPLAY TOTAL LINE                           
         BNE   *+10                                                             
         MVC   TSLSTLIN,TSCURRNO                                                
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
ERRXIT   LTR   RB,RB                                                            
         GOTO1 ASCRNCLR,DISLINE    CLEAR REST OF THE SCREEN                     
XIT      XIT1                                                                   
XITR1    XIT1  REGS=(R1)                                                        
         EJECT                                                                  
ORD      DC    CL3'ORD'                                                         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
CNTRLREC DC    C'000000'           CONTROL RECORD NUMBER                        
NINES    DC    C'99999999999999999999'                                          
         SPACE 1                                                                
MAXPOWCQ EQU   20                                                               
MAXMFWCQ EQU   8                                                                
         EJECT                                                                  
DCMIX    DS    0X                                                               
         DCDDL AC#ENH3,78                                                       
         DCDDL AC#ENH4,78                                                       
         DCDDL AC#ACC,9                                                         
         DCDDL AC#TOTAL,9                                                       
         DCDDL AC#PART,3                                                        
         DCDDL AC#PARDL,3                                                       
         DCDDL AC#RSPO,L'MX@RSPO                                                
         DCDDL AC#RSPOD,L'MX@RSPOD                                              
         DCDDL AC#ORDAM,L'MX@ORDAM                                              
         DCDDL AC#INVCD,L'MX@INVCD             INVOICED AMOUNT                  
         DCDDL AC#PAPPR,L'MX@PAPPR          PA, PARTIALLY APPROVED              
         DCDDL AC#REJEC,L'MX@REJEC          R,  REJECTED                        
         DCDDL AC#SUBMT,L'MX@SUBMT          S,  SUBMITTED                       
         DCDDL AC#CRTDT,L'MX@DATE           CREATED DATE                        
         DCDDL AC#INPR,L'MX@INPR           IP, IN PROGRESS                      
*        DCDDL AC#RCVE,L'MX@RCVD            RC, RECEIVED                        
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
*       ORDER LIST GRID COLUMN TABLE - COVERED BY GCTBLD              *         
***********************************************************************         
GCTBL    DS    0F                                                               
*                                                                               
GCTCOL1  DC    AL1(GCT1LQ,91,L'MX@RSPO,L'TSDORDNO)   ORDER NUMBER               
         DC    AL2(MX@RSPO-OVERWRKD,TSDORDNO-TSARDATD)                          
         DC    AL1(GCTITOT+GCTIOVER,0,0,0)                                      
         DC    AL1(0,L'MX@TOTAL)                                                
         DC    AL2(MX@TOTAL-OVERWRKD)                                           
GCT1LQ   EQU   *-GCTCOL1                                                        
*                                                                               
GCTCOL2  DC    AL1(GCT2LQ,02,L'MX@RSPOD,1)            ORDER DATE                
         DC    AL2(MX@RSPOD-OVERWRKD,TSDORDDT-TSARDATD)                         
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0,0,0)                                                     
GCT2LQ   EQU   *-GCTCOL2                                                        
*                                                                               
GCTCOL3  DC    AL1(GCT3LQ,03,L'MX@VEND,L'TSDSUPP)     VENDOR                    
         DC    AL2(MX@VEND-OVERWRKD,TSDSUPP-TSARDATD)                           
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0,0,0)                                                     
GCT3LQ   EQU   *-GCTCOL3                                                        
*                                                                               
GCTCOL4  DC    AL1(GCT4LQ,04,L'MX@JOBE,L'TSDJBEX)     JOB/EXPENSE               
         DC    AL2(MX@JOBE-OVERWRKD,TSDJBEX-TSARDATD)                           
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0,0,0)                                                     
GCT4LQ   EQU   *-GCTCOL4                                                        
*                                                                               
GCTCOL5  DC    AL1(GCT5LQ,GCTWCQ,L'MX@WC,0)            WORKCODE(S)              
         DC    AL2(MX@WC-OVERWRKD,AGRDSP-OVERWRKD)                              
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0,0,0)                                                     
GCT5LQ   EQU   *-GCTCOL5                                                        
*                                                                               
GCTCOL6  DC    AL1(GCT6LQ,06,L'MX@STAT,L'TSDSTATB)     STATUS                   
         DC    AL2(MX@STAT-OVERWRKD,TSDSTATB-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0,0,0)                                                     
GCT6LQ   EQU   *-GCTCOL6                                                        
*                                                                               
GCTCOL7  DC    AL1(GCT7LQ,07,L'MX@ORDAM,L'TSDOAMT)     ORDER AMOUNT             
         DC    AL2(MX@ORDAM-OVERWRKD,TSDOAMT-TSARDATD)                          
         DC    AL1(GCTITOT+GCTIOVER,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,L'TSTOTOT)                                                 
         DC    AL2(TSTOTOT-TSARTOTD)                                            
GCT7LQ   EQU   *-GCTCOL7                                                        
*                                                                               
GCTCOL8  DC    AL1(GCT8LQ,08,L'MX@INVCD,L'TSDIAMT)     INVOICED AMOUNT          
         DC    AL2(MX@INVCD-OVERWRKD,TSDIAMT-TSARDATD)                          
         DC    AL1(GCTITOT+GCTIOVER,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,L'TSTITOT)                                                 
         DC    AL2(TSTITOT-TSARTOTD)                                            
GCT8LQ   EQU   *-GCTCOL8                                                        
*                                                                               
GCTDATE  DC    AL1(GCTDATLQ,GCTDATEQ,L'MX@DATE,1) DATE ORDER ADDED              
         DC    AL2(MX@DATE-OVERWRKD),AL1(RACELQ,RACDATE-RACELD)                 
         DC    AL1(GCTIOVER+GCTIELEM,0,GCTFDAT+GCTFRGHT,0)                      
         DC    AL1(RACTADD,0),AL2(0)                                            
GCTDATLQ EQU   *-GCTDATE                                                        
*                                                                               
GCTTRSDT DC    AL1(GCTTRSLQ,GCTDATEQ,L'MX@DATE,2) CREATE DATE                   
         DC    AL2(MX@DATE-OVERWRKD,TSDCREDT-TSARDATD)                          
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0,0,0)                                                     
GCTTRSLQ EQU   *-GCTTRSDT                                                       
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
         DROP  R7,RB                                                            
*                                                                               
GCTWCQ   EQU   5                                                                
GCTDATEQ EQU   9                                                                
         EJECT                                                                  
***********************************************************************         
*        DISPLAY DIFFERENCE COLUMN FOR GRIDS                          *         
*         R3 = ADDRESS OF GRID TAB ENTRY                              *         
***********************************************************************         
         USING GCTBLD,R3                                                        
         USING TSARRECD,R4                                                      
GRDSP    NTR1  BASE=*,LABEL=*                                                   
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)        R8=RE-ESTABLISH LOCAL WORKING STOR           
         XC    TEMP,TEMP                                                        
*                                                                               
         L     R4,ATSARREC             R2=A(TSAR RECORD)                        
         LA    R4,TSARDATA                                                      
         USING TSARDATD,R4                                                      
*                                                                               
*-------------------*                                                           
* CHECK COLUMN TYPE *                                                           
*-------------------*                                                           
GRDSP20  CLI   GCTCOID,GCTWCQ       WORKCODES                                   
         BE    GRDSP25                                                          
         B     GRDSPERX                                                         
*                                                                               
*------------*                                                                  
* WORK CODES *                                                                  
*------------*                                                                  
GRDSP25  LA    R5,TEMP                      BUILD WORKCODES                     
         LA    RE,TSDWC                     WORKCODE OR LIST                    
         LA    R0,MAXPOWCQ                  MAXIMUM WORKCODES                   
GRDSP40  MVC   0(L'OAMWORK,R5),0(RE)        COPY OVER WORKCODES                 
         LA    R5,L'OAMWORK(R5)             BUMP TO NEXT AVAILABLE SPOT         
         LA    RE,L'OAMWORK(RE)             BUMP TO NEXT WORKCODE               
         CLC   0(L'OAMWORK,RE),SPACES       IS THERE A NEXT WORKCODE?           
         BNH   GRDSP75                      (NO) DONE                           
         MVI   0(R5),C','                   (YES) OUTPUT A COMMA                
         LA    R5,1(R5)                                                         
         BCT   R0,GRDSP40                   MOVE NEXT WORKCODE                  
*                                                                               
GRDSP75  LA    RE,TEMP                                                          
         SR    R5,RE                                                            
         BNP   GRDSPERX                                                         
         LR    R1,R5                                                            
*                                                                               
GRDSPX   J     XITR1                                                            
*                                                                               
GRDSPERX MVI   TEMP,C' '                                                        
         LHI   R1,1                                                             
         J     XITR1                                                            
*                                                                               
         LTORG                                                                  
         DROP  R3,R4,RB                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
OVERWRKD DSECT                                                                  
AGCTBL   DS    A                   ADDRESS OF GRID COLUMN TABLE                 
AGRDSP   DS    A                   GRID SPECIAL ROUTINE                         
SVORDKEY DS    CL(L'ORDKEY)        ORDER KEY                                    
INTFLAG  DS    X                                                                
NOTAUTHQ EQU   1                   NOT AUTHORISED FOR RECORD                    
ORESTT   DS    PL6                 ORDER TOTALS                                 
ORNUMT   DS    PL6                                                              
ORACTT   DS    PL6                                                              
FILFLAG  DS    X                   FILTER FLAG                                  
FILMWCQ  EQU   X'80'               MATCH ON WORKCODE                            
FLGCDATE EQU   X'40'               CREATED DATE FILTERING DONE.                 
*                                                                               
**********************************************************************          
* DATA DICTIONARY                                                               
**********************************************************************          
DSMIX    DS    0C                                                               
*-------------------*                                                           
MX@ENH3  DS    0CL78                                                            
         DS    CL11               ---ORDER---                                   
         DS    CL5                                                              
MX@VEND  DS    CL6                VENDOR                                        
         DS    CL9                                                              
MX@JOBE  DS    CL11               JOB/EXPENSE                                   
         DS    CL4                                                              
MX@WC    DS    CL3                W/C                                           
         DS    CL2                                                              
MX@STAT  DS    CL4                STAT                                          
         DS    CL6                                                              
         DS    CL5                ORDER                                         
         DS    CL3                                                              
MX@INVC  DS    CL8                INVOICED                                      
         DS    CL1                                                              
*-------------------*                                                           
MX@ENH4  DS    CL78                                                             
MX@ACC   DS    CL9                                                              
MX@TOTAL DS    CL9                                                              
MX@PART  DS    CL3                                                              
MX@PARDL DS    CL3                                                              
MX@RSPO  DS    CL12               ORDER NUMBER                                  
MX@RSPOD DS    CL10               ORDER DATE                                    
MX@ORDAM DS    CL12                                                             
MX@INVCD DS    CL15               INVOICED AMOUNT                               
MX@PAPPR DS    CL2                PA, PARTIALLY APPROVED                        
MX@REJEC DS    CL2                R, REJECT                                     
MX@SUBMT DS    CL2                S, SUBMITTED                                  
MX@DATE  DS    CL12               CREATED DATE                                  
MX@INPR  DS    CL2                IP, IN PROGRESS                               
*MX@RCVD  DS    CL2                ORDER RECEIVED                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
* SCREEN                                                                        
**********************************************************************          
SCRLIN1D DSECT                     COVER SCREEN LINE 1                          
SCRORDNO DS    CL(L'ORDKORD)       ORDER NUMBER                                 
         DS    CL1                                                              
SCRORDDT DS    CL8                 ORDER DATE                                   
         DS    CL1                                                              
SCRSUPP  DS    CL(L'ORDSUPU+L'ORDSUPL+L'ORDSUPA) SUPPLIER U/L/ACC               
         DS    CL1                                                              
SCRJBEX  DS    CL(L'ORDACCU+L'ORDACCL+L'ORDACCA) JOB/EXP U/L/ACC                
         DS    CL1                                                              
SCRWC    DS    CL(L'OAMWORK)       WORK CODE                                    
         DS    CL3                                                              
SCRSTAT  DS    CL2                 STATUS                                       
         DS    CL3                                                              
SCROAMT  DS    CL10                ORDER AMOUNT                                 
         DS    CL1                                                              
SCRIAMT  DS    CL10                INVOICED TODATE AMOUNT                       
         DS    CL1                                                              
SCRCREDT DS    CL8                 CREATE DATE                                  
         SPACE 1                                                                
SCRTOT1D DSECT                     COVER SCREEN TOTAL LINE 1                    
SCRTOTAL DS    CL(L'MX@TOTAL)      TOTAL                                        
         DS    CL46                                                             
SCROTOT  DS    CL11                ORDER TOTAL                                  
SCRITOT  DS    CL11                INVOICED TOTAL                               
*                                                                               
**********************************************************************          
* TSAR DATA DSECT                                                               
**********************************************************************          
TSARDATD DSECT                     COVER SCREEN LINE 1                          
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 LINE FOMAT TYPE                              
TSDATLN1 EQU   1                   DATA LINE 1 FORMAT                           
TSDORDNO DS    CL(L'ORDKORD)       ORDER NUMBER                                 
TSDORDDT DS    XL3                 ORDER DATE                                   
TSDSUPP  DS    CL(L'ORDSUPU+L'ORDSUPL+L'ORDSUPA) SUPPLIER U/L/ACC               
TSDJBEX  DS    CL(L'ORDACCU+L'ORDACCL+L'ORDACCA) JOB/EXP U/L/ACC                
TSDWC    DS    20CL(L'OAMWORK)      WORK CODE LIST                              
TSDSTATB DS    CL2                 STATUS                                       
TSDOAMT  DS    PL(L'OAMAMNT)       ORDER AMOUNT                                 
TSDIAMT  DS    PL(L'OAMINUM)       INVOICED TODATE AMOUNT                       
TSDCREDT DS    XL2                 CREATE DATE                                  
TSDLENQ  EQU   *-TSARDATD                                                       
*                                                                               
TSARTOTD DSECT                     COVER SCREEN LINE 1                          
TSTLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSTFMT   DS    CL1                 LINE FOMAT TYPE                              
TSTOTLN1 EQU   2                   TOTAL LINE 1 FORMAT                          
TSTOTOT  DS    PL(L'ORDTOT)        ORDER TOTAL                                  
TSTITOT  DS    PL(L'INVTOT)        INVOICE TOTAL                                
TSTLNQ   EQU   *-TSARTOTD                                                       
         EJECT                                                                  
       ++INCLUDE ACENQWORK                                                      
         EJECT                                                                  
*                                                                               
**********************************************************************          
* SAVED STORAGE                                                                 
**********************************************************************          
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
ORELO    DS    A                                                                
UNILDG   DS    CL2                 UNIT AND LEDGER                              
OLVALS   DS    0PL6                CREDITOR VALUES                              
ORDTOT   DS    PL6                 ORDER TOTAL                                  
INVTOT   DS    PL6                 INVOICE TOTAL                                
OLVALLNQ EQU   *-OLVALS                                                         
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACENQ0D   01/11/16'                                      
         END                                                                    
