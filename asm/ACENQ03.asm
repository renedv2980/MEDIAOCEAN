*          DATA SET ACENQ03    AT LEVEL 008 AS OF 06/11/18                      
*PHASE T62003A                                                                  
T62003   TITLE '$FIS - TYPES MO, MF AND FM'                                     
***********************************************************************         
*                                                                     *         
* NMAL 008 12JUN18  ERROR MESSAGE IF NUMBER OF TRANSACTION  SPEC-13487*         
*                   GROW MORE THAN TSAR AREA PREMIT                   *         
***********************************************************************         
T62003   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ENQ3**,R7,CLEAR=YES,RR=RE                                    
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         LH    R8,=Y(OVERWORK-WORKD)                                            
         LA    R8,WORKD(R8)        R8=A(LOCAL WORKING STORAGE)                  
         USING OVERWRKD,R8                                                      
         ST    RE,ORELO                                                         
*----------------------------------                                             
* SET GRID ADDRESSES                                                            
*----------------------------------                                             
         L     RF,=A(DISDIFF)                                                   
         AR    RF,RE               SET UP GRID DIFFERENCE DISPLAY               
         ST    RF,ADISDIFF                                                      
         L     RF,=A(DISMOOF)                                                   
         AR    RF,RE               SET UP GRID MONTH AND OFFICE DISPLAY         
         ST    RF,ADISMOOF                                                      
*                                                                               
         L     RF,=A(GCTBLMO)      ASSUME TYPE=MO                               
         CLI   SVACTNUM,ACTNMNOF          TYPE=MF?                              
         BNE   *+8                        . NO                                  
         L     RF,=A(GCTBLMF)                                                   
         CLI   SVACTNUM,ACTNOFMN          TYPE=FM?                              
         BNE   *+8                        .NO                                   
         L     RF,=A(GCTBLFM)                                                   
         AR    RF,RE                      SET UP GRID COLUMN TABLE              
         ST    RF,AGCTBL                                                        
*                                                                               
MAIN     GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
*----------------------------------                                             
* FIRST TIME THROUGH                                                            
*----------------------------------                                             
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN02              NO                                           
         BAS   RE,FSTDIS           YES PERFORM FIRST DISPLAY FUNCTIONS          
         BNE   ERRXIT                                                           
         TM    DISPFLAG,DISIOMAX   MAX IO'S?                                    
         BO    MAINX               YES                                          
         OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
*----------------------------------                                             
* CONTINUE PROCESSING                                                           
*----------------------------------                                             
MAIN02   TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    MAIN03                                                           
         TM    DETFLAG,DETDONE                                                  
         BO    MAIN08                                                           
*                                                                               
MAIN03   TM    DISPFLAG,NORECQ     ANY RECORDS ON JOB?                          
         BO    MAINX               NO                                           
         BAS   RE,TSARNOW          READ ALL RECORDS UP FRONT                    
         TM    DISPFLAG,DISIOMAX   MAX IO'S?                                    
         BO    MAINX               . YES                                        
         TM    DISPFLAG,NORECQ     ANY RECORDS ON JOB?                          
         BO    MAINX               . NO                                         
*                                                                               
MAIN04   CLC   TSCURRNO,TSLSTREC   HAVE WE ALREADY GOT RECORD IN TSAR?          
         BNH   MAIN06              . YES                                        
         TM    DISPFLAG,TSARFULQ   IS TSAR BLOCK FULL?                          
         BZ    MAIN08              . NO                                         
         LA    R2,BASKEYH          . YES, TELL THEM                             
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATOOMNY)                                           
         B     ERRXIT                                                           
*                                                                               
MAIN06   GOTO1 ATSARGET,TSCURRNO   GET RECORD FROM TSAR                         
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID                      
         BO    MAIN07               YES                                         
         BAS   RE,FORMTSAR         FORMAT ONTO DUMMY SCREEN LINES               
         BAS   RE,DISBAL           DISPLAY BALANCE BROUGHT FORWARD              
         B     *+8                                                              
MAIN07   BAS   RE,FGRMTSAR         FORMAT TSAR FOR GRIDS                        
*                                                                               
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES                   
         BNE   MAIN09              SCREEN IS FULL                               
*                                                                               
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN04                                                           
*                                                                               
MAIN08   TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    MAIN09                                                           
         OI    DETFLAG,DETDONE                                                  
         BAS   RE,FGRMTSAR                                                      
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES                   
         B     MAINX                                                            
*                                                                               
MAIN09   TM    PCDRIVEN,PCGRIDQ                                                 
         BO    MAINX                                                            
         BAS   RE,TOTAL            DISPLAY TOTAL LINE                           
*                                                                               
MAINX    TM    DISPFLG2,DISMOMAX   IS MAX DATA REACHED                          
         JZ    MAINX1                                                           
         MVC   FVMSGNO,=AL2(EAMNYTRN)    MSG 1092                               
         J     ERRXIT                                                           
MAINX1   OC    TSLSTLIN,TSLSTLIN   ANYTHING DISPLAYED?                          
         BNZ   OKXIT               . YES                                        
         OI    DISPFLAG,NORECQ     . NO NOTHING                                 
         B     OKXIT                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
FSTDIS   NTR1                                                                   
*                                                                               
         MVC   STARTMTH,HEXFFS     START MONTH FOR PRIOR BUCKET ELEMENT         
         XC    ENDMTH,ENDMTH       END MONTH FOR PRIOR BUCKET ELEMENT           
         XC    TEMPBLKN,TEMPBLKN   NUMBER OF ENTRIES IN BLOCK                   
         XC    STATFLAG,STATFLAG                                                
         MVI   DETFLAG,0           INIT DETAIL FLAG                             
         ZAP   DEBTOT,=P'0'        TOTAL DEBITS FOR ACCOUNT                     
         ZAP   CRETOT,=P'0'        TOTAL CREDITS FOR ACCOUNT                    
         MVI   NXTRMODE,0          NEXT RECODE MODE                             
*                                                                               
         GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,0,ACOMFACS,(0,0)                  
*                                                                               
         USING FLDHDRD,R2                                                       
         LA    R2,BASKEYH          R2=A(KEY FIELD)                              
         ST    R2,FVADDR                                                        
*                                                                               
         GOTO1 AGETACC,0           GET ACCOUNT RECORD                           
         BNE   FSTDERR                                                          
*                                                                               
         L     R3,AOPTVALS         ADDRESS THE OPTIONS                          
         USING OPTVALSD,R3                                                      
         CLI   OBFWD,0             DEFAULT IS BF=Y                              
         BNE   *+8                                                              
         MVI   OBFWD,C'D'          USE 'D' TO INDICATE WE SET IT                
*---------------------                                                          
* HOW TO GET THE DATA                                                           
*---------------------                                                          
         CLC   SPROUL,UNITLEDG                                                  
         BE    FSTD14                                                           
*                                                                               
         CLI   ODRAFT,C'O'         DRAFT TRANSACTIONS ONLY?                     
         BE    FSTD16              . YES, READ TRANSACTIONS THEN                
         CLI   ODRAFT,C'Y'         INCLUDE DRAFTS?                              
         BE    FSTD16              . YES, CAN'T READ CONTRAS ONLY               
         CLI   SVACTNUM,ACTNMNTH   TYPE=MO?                                     
         BNE   FSTD16               NO,  NEED OFFICE DETAIL                     
*                                                                               
         OC    OOFFICVL,OOFFICVL   OFFICE FILTER?                               
         BNZ   FSTD16               YES, NEED OFFICE DETAIL                     
         CLI   LDGTOFFP,LDGOTRAN   OFFICE IN TRANSACTIONS?                      
         BNE   FSTD14               NO,  DO NOT NEED OFFICE DETAIL              
         CLI   TERMACCS,C'*'       LIMIT ACCESS?                                
         BE    FSTD16               YES, NEED OFFICE DETAIL                     
         CLI   TERMACCS,C'$'                                                    
         BE    FSTD16               YES, NEED OFFICE DETAIL                     
*                                                                               
FSTD14   OI    NXTRMODE,NXTRCACQ   READ CONTRA ACCOUNTS ONLY                    
         B     FSTD20                                                           
*                                                                               
FSTD16   TM    COMPSTA4,CPYSOFF2   2 CHAR OFFICE COMPANY?                       
         BZ    FSTD20                                                           
*                                                                               
         CLI   UNITLEDG,C'S'       SUBSIDIARY LEDGER?                           
         BE    *+12                                                             
         CLI   UNITLEDG,C'G'       GENERAL LEDGER?                              
         BNE   FSTD20                                                           
         OI    STATFLAG,ROFFBUKQ   READ OFFICE/CONTRA BUCKETS                   
*-----------------                                                              
* SET FISCAL YEAR                                                               
*-----------------                                                              
FSTD20   GOTO1 ASETFIS                                                          
*                                                                               
*---------------------------------                                              
* PROCESS ACCOUNT RECORD ELEMENTS                                               
*---------------------------------                                              
FSTD21   L     RE,AIO1                                                          
         LA    RF,ACTRFST-ACTRECD(RE)                                           
FSTD22   CLI   0(RF),EOR           END OF RECORD?                               
         BE    FSTD35                                                           
         CLI   0(RF),JOBELQ        JOB ELEMENT?                                 
         BE    FSTD26                                                           
         CLI   0(RF),RSTELQ        RECORD STATUS ELEMENT?                       
         BE    FSTD28                                                           
         CLI   0(RF),APOELQ        ACCOUNT PEEL-OFF ELEMENT?                    
         BE    FSTD30                                                           
FSTD25   SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     FSTD22                                                           
*                                                                               
         USING JOBELD,RF           JOB ELEMENT                                  
FSTD26   MVC   JOBSTAT,JOBSTA1     JOB STATUS                                   
         B     FSTD25                                                           
*                                  RECORD STATUS ELEMENT                        
         USING RSTELD,RF                                                        
FSTD28   MVC   CLOSEDAT,RSTBDATE   SAVE PEEL DATE                               
         B     FSTD25                                                           
*                                                                               
         USING APOELD,RF                                                        
FSTD30   OC    APOPLDT,APOPLDT     HAS ACCOUNT BEEN PEELED?                     
         BNZ   FSTD25                                                           
         XC    CLOSEDAT,CLOSEDAT   IF NOT PEELED, CLEAR CLOSED DATE             
         B     FSTD25                                                           
*---------------------------------*                                             
*                                                                               
FSTD35   OC    CLOSEDAT,CLOSEDAT   BACK UP TO ACTUAL MONTH CLOSED               
         BZ    FSTD40                                                           
         GOTO1 VDATCON,DMCB,(1,CLOSEDAT),(0,WORK)                               
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,-1                                
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,CLOSEDAT)                             
*                                                                               
FSTD40   CLI   OBFWD,C'N'          SHOW BALANCE FORWARD?                        
         BNE   FSTD50              YES                                          
         CLI   SVACTNUM,ACTNMNTH   NO, TYPE=MO?                                 
         BNE   FSTD50              NO                                           
         OI    DISPFLAG,BALANCEQ   THIS DISPLAY HAS BALANCE LINE FORMAT         
         MVI   SCRLLADJ,2          BOTH TOP AND BOTTOM BALANCE LINES            
*                                                                               
FSTD50   TM    PCDRIVEN,PCGRIDQ                                                 
         BO    FSTD55                                                           
*                                                                               
         LA    R2,ENQDAT1H                                                      
         GOTO1 ADISUL              DISPLAY UNIT AND LEDGER NAMES                
         LA    R2,ENQDAT2H-ENQDAT1H(R2)  DISPLAY CONTRA NAMES                   
*                                                                               
FSTD55   GOTO1 ADISACC                                                          
         GOTO1 ADISMOS,DMCB,(L'MX@MOA,MX@MOA),(L'MX@BFW,MX@BFW)                 
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    FSTD78                                                           
*                                                                               
FSTD60   LA    R2,ENQDAT2H-ENQDAT1H(R2)  DISPLAY COLUMN HEADINGS                
         LA    RF,MX@ENH13                                                      
         CLI   SVACTNUM,ACTNMNTH   TYPE=MO?                                     
         BE    FSTD70              YES                                          
         LA    RF,MX@ENH32                                                      
         CLI   SVACTNUM,ACTNMNOF   TYPE=MF?                                     
         BE    FSTD70              YES                                          
         LA    RF,MX@ENH33         ASSUME TYPE=FM                               
*                                                                               
FSTD70   MVC   FLDDATA(L'MX@ENH13),0(RF)                                        
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
*                                                                               
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         GOTO1 ASCRNDIM,DMCB,(1,(R2))                                           
         B     FSTD80                                                           
*                                                                               
FSTD78   GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
FSTD80   GOTO1 ADISPFK             DISPLAY PFKEYS                               
         MVC   KEYSAVE,SPACES      CLEAR SAVED KEY                              
*                                                                               
         CLI   OBUKTYPE,0          ANY BUCKET SPECIFIED?                        
         BNE   FSTD90              YES                                          
         CLI   OHOURS,C'Y'         NO, HOURS?                                   
         BNE   *+12                NO                                           
         MVI   OBUKTYPE,C'H'       YES, SET BUKTYPE                             
         B     FSTD90                                                           
*                                                                               
         CLC   ONEC,BASKEY         IF UNIT/LEDGER 1C                            
         BNE   FSTD90                                                           
         TM    COMPSTA5,CPYSNCST   AND NEW COST AGENCY                          
         BNO   FSTD90                                                           
         MVI   OBUKTYPE,C'1'       DEFAULT IS METHOD 1                          
*                                                                               
FSTD90   OI    OBUKTYPE,C' '                                                    
         OI    OPAYTYPE,C' '                                                    
         TM    SECFFLAG,SECFRATE   AUTHORIZED TO SEE RATE?                      
         BNO   FSTD100             YES                                          
         CLI   OBUKTYPE,C' '       NO, BUCKET TYPE ALREADY DEFINED?             
         BH    FSTD100             YES                                          
         MVI   OBUKTYPE,C'H'       NO, SET TO HOURS                             
*                                                                               
FSTD100  GOTO1 AREADUP             READ AHEAD AND RETURN                        
         BNE   FSTDERR                                                          
*                                                                               
FSTDX    CR    RB,RB                                                            
         B     *+6                                                              
FSTDERR  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        READ ALL RECORDS UP FRONT AND BUILD TSAR RECORDS             *         
*        'KEYSAVE' SET MEANS RE-ENTER AFTER MAX IO                    *         
***********************************************************************         
TSARNOW  NTR1                                                                   
*                                                                               
         TM    DISPFLAG,ALLREADQ   ALL RECORDS READ?                            
         BO    TSARNX               YES                                         
         CLC   KEYSAVE,SPACES      RE-ENTER OR FIRST TIME FOR ACCOUNT?          
         BNE   *+12                                                             
         L     R4,ATEMPBLK         R4=(BLOCK USED BY 'MTHTAB')                  
         MVI   0(R4),EOT           SET BLOCK TO EMPTY                           
*                                                                               
         TM    STATFLAG,ROFFBUKQ   READ OFFICE BUCKETS?                         
         BZ    *+12                                                             
         BAS   RE,GETOBUK                                                       
         B     TSARN06                                                          
         TM    NXTRMODE,NXTRCACQ   READ CONTRA BUCKETS?                         
         BZ    *+12                 NO                                          
         BAS   RE,GETBUK            YES, GET AMOUNTS FROM BUCKETS               
         B     *+8                                                              
         BAS   RE,GETTRN           GET AMOUNTS FROM TRANSACTIONS                
*                                                                               
TSARN06  TM    DISPFLAG,DISIOMAX   MAX IO'S?                                    
         BO    TSARNX               YES                                         
*                                                                               
TSARN10  L     RF,AOPTVALS         ADDRESS THE OPTIONS                          
         USING OPTVALSD,RF                                                      
         MVC   KEYSAVE,SPACES                                                   
*                                                                               
         OC    TEMPBLKN,TEMPBLKN   HAVE WE ANY TSAR RECORDS?                    
         BNZ   TSARN20              YES                                         
         OI    DISPFLAG,NORECQ     NO RECORDS FOUND FOR ACCOUNT                 
*                                                                               
         CLI   OBFWD,C'N'          WANT BALANCE BROUGHT FORWARD?                
         BE    TSARNX               NO                                          
         OC    OMOSST,OMOSST       MISSING START MONTH (MO=-MMM/YY)?            
         BZ    TSARNX               YES, EXIT NO BBF NEEDED                     
         B     TSARN30                                                          
*                                                                               
TSARN20  CLI   OBFWD,C'N'          WANT BALANCE BROUGHT FORWARD?                
         BE    TSARN40              NO                                          
         OC    OMOSST,OMOSST       MISSING START MONTH (MO=-MMM/YY)?            
         BZ    TSARN40              YES, CONTINUE NO ZERO BBF NEEDED            
         DROP  RF                                                               
*                                                                               
TSARN30  TM    DISPFLAG,ALLREADQ   ALL RECORDS READ                             
         BO    TSARNX                                                           
         NI    DISPFLAG,X'FF'-NORECQ     DISPLAYING 0 BBF                       
         MVC   OFFICE,ALLOFF                                                    
         MVC   PBMOS,BFWMOS        BFWD MOS                                     
         ZAP   PBDR,=P'0'                                                       
         ZAP   PBCR,=P'0'                                                       
         BAS   RE,TABBLD                                                        
*                                                                               
TSARN40  OI    DISPFLAG,ALLREADQ   ALL RECORDS READ                             
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,3,TEMPBLKN       R5=(NUM OF ENTRIES IN TEMPBLK)               
         CH    R5,TSLSTREC         ALL TSAR RECORDS BUILT?                      
         BNH   TSARNX              . YES                                        
*                                                                               
         LA    RF,MTHLNQ           RF=L'(MTHTAB ENTRY)                          
         GOTO1 VXSORT,DMCB,(0,ATEMPBLK),(R5),(RF),4,0                           
*                                                                               
         CLI   SVACTNUM,ACTNMNTH   TYPE=MO?                                     
         BNE   TSARN75             . YES                                        
         L     R4,ATEMPBLK         R4=A(MONTH TABLE)                            
         USING MTHTABD,R4                                                       
         OC    ENDMTH,ENDMTH       PRIOR BUCKETS ENCOUNTERED?                   
         BZ    TSARN75             . NO                                         
         ZAP   PDEBITS,=P'0'                                                    
         ZAP   PCREDITS,=P'0'                                                   
         SR    RF,RF                                                            
*                                                                               
TSARN50  CLI   0(R4),EOT           END OF TABLE                                 
         BE    TSARN60                                                          
*                                                                               
         LA    RE,MTHDATE1         MTHDATE1 FOR TYPE=MO & MF                    
         CLI   SVACTNUM,ACTNOFMN                                                
         BNE   *+8                                                              
         LA    RE,MTHDATE2         MTHDATE2 FOR TYPE=FM                         
*                                                                               
         CLC   0(L'MTHDATE1,RE),ENDMTH  LAST PRIOR BUCKET ENTRY?                
         BE    TSARN60                                                          
         LA    RF,1(RF)                    NUMBER OF ROLLED UP ENTRIES          
         AP    PDEBITS,MTHDR               ROLL UP ENTRY                        
         AP    PCREDITS,MTHCR                                                   
         MVC   0(L'MTHDATE1,RE),HEXFFS     SET ENTRY TO 'NULL'                  
         LA    R4,MTHLNQ(R4)               BUMP TO NEXT ENTRY                   
         B     TSARN50                                                          
*                                                                               
TSARN60  AP    MTHDR,PDEBITS       ADD ON TO LAST PRIOR BUCKET ENTRY            
         AP    MTHCR,PCREDITS                                                   
         XC    ENDMTH,ENDMTH       CLEAR LATEST PRIOR BUCKET DATE               
*                                                                               
TSARN62  LTR   RF,RF               ANY ENTRIES ROLLED UP?                       
         BZ    TSARN75              NO                                          
*                                                                               
         LR    RE,RF               RE=RF=(NUMBER OF ROLLED UP ENTRIES)          
         MH    RE,=Y(MTHLNQ)       RE=(DISP TO 1ST NON 'NULL' ENTRY)            
         L     R1,ATEMPBLK         R1=A(START OF 1ST 'NILL' ENTRY)              
         LR    R0,R1                                                            
         LA    RE,0(R1,RE)         RE=A(1ST NON 'NULL' ENTRY)                   
         SR    R5,RF               REDUCE NUMBER OF 'MTHTAB' ENTRIES            
         STCM  R5,3,TEMPBLKN                                                    
         LR    RF,R5                                                            
         MH    RF,=Y(MTHLNQ)       RF=L'(NEW 'MTHTAB')                          
         LR    R1,RF                                                            
         MVCL  R0,RE               MOVE TO START OF OLD 'MTHTAB'                
*                                                                               
TSARN75  BAS   RE,BLDTSDAT         BUILD TSAR RECORDS                           
         BNE   TSARNX              MAX IO OR TSAR BLOCK FULL                    
         MVC   TSCURRNO,=H'1'      SET CURRENT TSAR TO FIRST ONE                
         B     TSARNX                                                           
*                                                                               
TSARNX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*     GET AMOUNTS FROM OFFICE BUCKET RECORDS                          *         
*      ON ENTRY 'KEYSAVE' SET TO SPACES MEANS 1ST TIME THROUGH        *         
*                                                                     *         
*     THIS ROUTINE IS EXECUTED WHEN :                                 *         
*     THE TYPE = MO AND NOT FILTERING BY OFFICE AND NO LIMITED ACCESS *         
*                OR THERE IS LIMITED ACCESS BUT THE OFFICE IS NOT IN  *         
*                THE TRANSACTION                                      *         
*                                                                     *         
***********************************************************************         
GETOBUK  NTR1                                                                   
*                                                                               
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         LA    R3,IOKEY                                                         
         USING CACRECD,R3                                                       
         CLC   KEYSAVE,SPACES      RE-ENTER OF FIRST FOR ACCOUNT?               
         BE    GETO05                                                           
         GOTO1 AIO,IOREAD+IOACCDIR+IO1 GET CONTRA RECORD                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1                                                          
*                                                                               
GETO05   CLC   CACKCULA,SAVEACC    SAME ACCOUNT?                                
         BNE   GETOX                NO, DONE                                    
         CLC   CACKCULC,SPACES     IS THERE A CONTRA ACCOUNT                    
         BE    GETO75               NO, NEXT                                    
         CLC   CACKOFF,SPACES      OFFICE BUCKETS?                              
         BNH   GETO75               NO, GET NEXT                                
         CLC   CACKSPAC,SPACES     TRANSACTION RECORD?                          
         BH    GETO74               YES                                         
         CLC   CHDKNULL-CHDRECD(L'CHDKNULL,R3),SPACES   HEADER?                 
         BL    *+12                                     . YES                   
         CLI   ODRAFT,C'O'         DRAFTS ONLY?                                 
         BE    GETO75              . YES, SKIP                                  
*---------------------------------                                              
* COSTING ACCOUNT?                                                              
*---------------------------------                                              
         CLC   =C'1C',BASKEY       COSTING?                                     
         BNE   GETO10               NO                                          
         CLI   OBUKTYPE,C'1'       METHOD=?                                     
         BL    GETO10                                                           
         CLI   OBUKTYPE,C'9'       METHOD=?                                     
         BH    GETO10                                                           
         CLC   =C'14',CACKCUNT     1-9 VALID FOR 14, 15 AND 16 ONLY             
         BE    GETO10                                                           
         CLC   =C'15',CACKCUNT                                                  
         BE    GETO10                                                           
         CLC   =C'16',CACKCUNT                                                  
         BNE   GETO20                                                           
*                                                                               
GETO10   CLC   OBUKTYPE,CACKBTYP   DOES BUCKET MATCH?                           
         BNE   GETO75              NO, GET NEXT                                 
         CLI   OPAYTYPE,C' '       YES, ANY PAYTYPE?                            
         BE    *+14                NO                                           
         CLC   OPAYTYPE,CACKSTYP   YES, DOES IT MATCH SUBTYPE?                  
         BNE   GETO75              NO, GET NEXT                                 
*                                                                               
*---------------------------------                                              
* CONTRA FILTERING                                                              
*---------------------------------                                              
GETO20   SR    RF,RF                                                            
         ICM   RF,1,CONTLEN        ANY CONTRA FILTER?                           
         BZ    GETO30              NO                                           
*                                                                               
         BCTR  RF,0                                                             
         LA    RE,CACKULC          RE=A(CONTRA CODE)                            
         CLI   0(RE),C' '          STRIP OFF LEADING SPACES                     
         BH    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   CONTRA(0),0(RE)     DOES CONTRA ACCOUNT MATCH?                   
         BE    GETO25              YES                                          
         CLI   NEGCONT,NEGFILTR    NO, TRY NEGATIVE FILTER                      
         BNE   GETO75                                                           
         B     *+12                                                             
*                                                                               
GETO25   CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER                              
         BE    GETO75                                                           
*---------------------------------                                              
* WORKCODE FILTERING                                                            
*---------------------------------                                              
GETO30   CLC   BASKEY(L'SPROUNIT+L'SPROLEDG),SPROUNIT                           
         BNE   GETO40              PROD LEDGER ONLY                             
         OC    OWCODE,OWCODE       WORKCODE FILTER?                             
         BZ    GETO40              NO                                           
*                                                                               
         SR    RF,RF                                                            
         IC    RF,OWCODELN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   CACKWRK(0),OWCODEVL DOES WORKCODE MATCH?                         
         BE    GETO35              YES                                          
         CLI   OWCODEFI,NEGFILTR   NO, TRY NEGATIVE FILTER                      
         BNE   GETO75                                                           
         B     *+12                                                             
GETO35   CLI   OWCODEFI,NEGFILTR                                                
         BE    GETO75                                                           
*---------------------------------                                              
GETO40   GOTO1 AIO,IOGET+IOACCMST+IO1 GET THE CONTRA RECORD                     
         BE    *+14                NO ERRORS                                    
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BO    GETO90              YES                                          
         DC    H'0'                                                             
*                                                                               
         GOTO1 AOFFOFF                                                          
         BNE   GETO75               YES, GET NEXT RECORD                        
*                                                                               
         L     R3,AIO1             R3=A(CONTRA RECORD)                          
         MVC   OFFICE,SPACES                                                    
         CLI   SVACTNUM,ACTNMNTH   TYPE=MO?                                     
         BE    *+10                YES                                          
         MVC   OFFICE,CACKOFF      NO, SAVE THE OFFICE                          
         LA    R3,CACRFST          R3=A(FIRST ELEMENT ON RECORD)                
GETO50   CLI   0(R3),EOR           END OF RECORD?                               
         BE    GETO75                                                           
         CLI   0(R3),CACELQ        CONTRA NAME ELEMENT?                         
         BE    GETO75              YES, GET NEXT                                
         CLI   0(R3),PBKELQ        PREVIOUS BUCKET ELEMENT?                     
         BE    GETO60                                                           
         CLI   0(R3),BUKELQ        BUCKET ELEMENT?                              
         BE    GETO70                                                           
*                                                                               
GETO55   SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GETO50                                                           
*--------------------------------                                               
* PREVIOUS BUCKET ELEMENT- X'55'                                                
*--------------------------------                                               
         USING PBKELD,R3                                                        
GETO60   OI    PBSTATUS,MTHPREVQ   SET STATUS                                   
         MVC   PBMOS,PBKHI         MOA                                          
         ZAP   PBDR,PBKDR          DEBITS                                       
         ZAP   PBCR,PBKCR          CREDITS                                      
         BAS   RE,TABBLD           BUILD TABLE ENTRY                            
*                                                                               
         TM    PBSTATUS,MTHPREVQ   DID IT GO INTO BALANCE FORWARD?              
         BZ    GETO55               YES, WE'RE GOOD                             
         CLC   ENDMTH,PBKHI        SAVE LATEST MONTH                            
         BH    *+10                                                             
         MVC   ENDMTH,PBKHI                                                     
         CLC   STARTMTH,PBKLOW     SAVE EARLIEST MONTH                          
         BL    *+10                                                             
         MVC   STARTMTH,PBKLOW                                                  
         NI    PBSTATUS,X'FF'-MTHPREVQ                                          
         B     GETO55                                                           
         DROP  R3                                                               
*-----------------------                                                        
* BUCKET ELEMENT- X'45'                                                         
*-----------------------                                                        
         USING BUKELD,R3                                                        
GETO70   MVC   PBMOS,BUKMOS        MOA                                          
         ZAP   PBDR,BUKDR          DEBITS                                       
         ZAP   PBCR,BUKCR          CREDITS                                      
         BAS   RE,TABBLD           BUILD TABLE ENTRY                            
         B     GETO55                                                           
*-----------------------                                                        
         USING TRNRECD,R3                                                       
GETO74   TM    TRNKSTA,TRNSDRFT    DRAFT TRANSACTION?                           
         BZ    GETO75              . NO                                         
         CLI   ODRAFT,C'Y'         INCLUDE DRAFTS?                              
         BE    GETT16              . YES - GOTO TO TRN DETAIL ROUTINE           
         CLI   ODRAFT,C'O'         INCLUDE DRAFTS?                              
         BE    GETT16              . YES - GOTO TO TRN DETAIL ROUTINE           
*                                                                               
GETO75   LA    R3,IOKEY                                                         
         GOTO1 ANXTREC,NXTRMODE    GET NEXT RECORD                              
         BE    GETO05              NO ERRORS                                    
         TM    DISPFLAG,DISIOMAX                                                
         BO    GETO90                                                           
         DC    H'0'                                                             
*                                                                               
GETO90   OI    DISPFLAG,DISIOMAX                                                
         MVC   KEYSAVE,IOKEY                                                    
*                                                                               
GETOX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*        GET AMOUNTS FROM BUCKET RECORDS                              *         
* ON ENTRY 'KEYSAVE' SET TO SPACES MEANS 1ST TIME THROUGH             *         
*                                                                     *         
* THIS ROUTINE IS EXECUTED WHEN :                                     *         
*     THE TYPE = MO AND NOT FILTERING BY OFFICE AND NO LIMITED ACCESS *         
*                OR THERE IS LIMITED ACCESS BUT THE OFFICE IS NOT IN  *         
*                THE TRANSACTION                                      *         
*                                                                     *         
***********************************************************************         
*                                                                               
GETBUK   NTR1                                                                   
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         LA    R3,IOKEY                                                         
         USING CACRECD,R3                                                       
         CLC   KEYSAVE,SPACES      RE-ENTER OF FIRST FOR ACCOUNT?               
         BE    GETB01                                                           
         GOTO1 AIO,IOREAD+IOACCDIR+IO1 GET CONTRA RECORD                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1                                                          
*                                                                               
GETB01   CLC   CACKCULA,SAVEACC    SAME ACCOUNT?                                
         BNE   GETBX               NO, DONE                                     
*                                                                               
         CLC   CACKCULC,SPACES     IS THERE A CONTRA ACCOUNT                    
         BE    GETB30              NO, NEXT                                     
         CLC   CACKSPAC,SPACES     TRANSACTION RECORD?                          
         BH    GETB30              YES, GET NEXT                                
         CLC   BASKEY(L'SPROUNIT+L'SPROLEDG),SPROUNIT   PROD LEDG?              
         BE    *+14                                     YES, WC IS IN           
         CLC   CACKOFF,SPACES      IS THERE AN OFFICE?                          
         BNE   GETB30              YES, NEXT                                    
*                                                                               
         CLC   =C'1C',BASKEY       COSTING?                                     
         BNE   GETB10              NO                                           
         CLI   OBUKTYPE,C'1'       METHOD=?                                     
         BL    GETB10                                                           
         CLI   OBUKTYPE,C'9'       METHOD=?                                     
         BH    GETB10                                                           
         CLC   =C'14',CACKCUNT     1-9 VALID FOR 14, 15 AND 16 ONLY             
         BE    GETB10                                                           
         CLC   =C'15',CACKCUNT                                                  
         BE    GETB10                                                           
         CLC   =C'16',CACKCUNT                                                  
         BNE   GETB12                                                           
*                                                                               
GETB10   CLC   OBUKTYPE,CACKBTYP   DOES BUCKET MATCH?                           
         BNE   GETB30              NO, GET NEXT                                 
         CLI   OPAYTYPE,C' '       YES, ANY PAYTYPE?                            
         BE    *+14                NO                                           
         CLC   OPAYTYPE,CACKSTYP   YES, DOES IT MATCH SUBTYPE?                  
         BNE   GETB30              NO, GET NEXT                                 
*                                                                               
*--------------------------                                                     
* CONTRA ACCOUNT FILTERING                                                      
*--------------------------                                                     
GETB12   SR    RF,RF                                                            
         ICM   RF,1,CONTLEN        ANY CONTRA FILTER?                           
         BZ    GETB16              NO                                           
*                                                                               
         BCTR  RF,0                                                             
         LA    RE,CACKULC          RE=A(CONTRA CODE)                            
         CLI   0(RE),C' '          STRIP OFF LEADING SPACES                     
         BH    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   CONTRA(0),0(RE)     DOES CONTRA ACCOUNT MATCH?                   
         BE    GETB14              YES                                          
         CLI   NEGCONT,NEGFILTR    NO, TRY NEGATIVE FILTER                      
         BNE   GETB30                                                           
         B     *+12                                                             
GETB14   CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER                              
         BE    GETB30                                                           
*--------------------                                                           
* WORKCODE FILTERING                                                            
*--------------------                                                           
GETB16   CLC   BASKEY(L'SPROUNIT+L'SPROLEDG),SPROUNIT                           
         BNE   GETB20              PROD LEDGER ONLY                             
         OC    OWCODE,OWCODE       WORKCODE FILTER?                             
         BZ    GETB20              NO                                           
         SR    RF,RF                                                            
         IC    RF,OWCODELN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   CACKWRK(0),OWCODEVL DOES WORKCODE MATCH?                         
         BE    GETB18              YES                                          
         CLI   OWCODEFI,NEGFILTR   NO, TRY NEGATIVE FILTER                      
         BNE   GETB30                                                           
         B     *+12                                                             
GETB18   CLI   OWCODEFI,NEGFILTR                                                
         BE    GETB30                                                           
*-------------------------------                                                
* GET CONTRA RECORD AND PROCESS                                                 
*-------------------------------                                                
GETB20   GOTO1 AIO,IOGET+IOACCMST+IO1 GET THE CONTRA RECORD                     
         BE    *+14                NO ERRORS                                    
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BO    GETB32              YES                                          
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO1             R3=A(CONTRA RECORD)                          
         MVC   OFFICE,SPACES                                                    
         CLI   SVACTNUM,ACTNMNTH   TYPE=MO?                                     
         BE    *+10                YES                                          
         MVC   OFFICE,CACKOFF      NO, SAVE THE OFFICE                          
*                                                                               
         LA    R3,CACRFST          R3=A(FIRST ELEMENT ON RECORD)                
GETB22   CLI   0(R3),EOR           END OF RECORD?                               
         BE    GETB30                                                           
         CLI   0(R3),CACELQ        CONTRA NAME ELEMENT?                         
         BE    GETB30              YES, GET NEXT                                
         CLI   0(R3),PBKELQ        PREVIOUS BUCKET ELEMENT?                     
         BE    GETB26                                                           
         CLI   0(R3),BUKELQ        BUCKET ELEMENT?                              
         BE    GETB28                                                           
GETB24   SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GETB22                                                           
*--------------------------------                                               
* PREVIOUS BUCKET ELEMENT- X'55'                                                
*--------------------------------                                               
         USING PBKELD,R3                                                        
GETB26   OI    PBSTATUS,MTHPREVQ   SET STATUS                                   
         MVC   PBMOS,PBKHI         MOA                                          
         ZAP   PBDR,PBKDR          DEBITS                                       
         ZAP   PBCR,PBKCR          CREDITS                                      
         BAS   RE,TABBLD           BUILD TABLE ENTRY                            
*                                                                               
         TM    PBSTATUS,MTHPREVQ   DID IT GO INTO BALANCE FORWARD?              
         BZ    GETB24              YES                                          
         CLC   ENDMTH,PBKHI        SAVE LATEST MONTH                            
         BH    *+10                                                             
         MVC   ENDMTH,PBKHI                                                     
         CLC   STARTMTH,PBKLOW     SAVE EARLIEST MONTH                          
         BL    *+10                                                             
         MVC   STARTMTH,PBKLOW                                                  
         NI    PBSTATUS,X'FF'-MTHPREVQ   RESET STATUS                           
         B     GETB24                                                           
         DROP  R3                                                               
*-----------------------                                                        
* BUCKET ELEMENT- X'45'                                                         
*-----------------------                                                        
         USING BUKELD,R3                                                        
GETB28   MVC   PBMOS,BUKMOS        MOA                                          
         ZAP   PBDR,BUKDR          DEBITS                                       
         ZAP   PBCR,BUKCR          CREDITS                                      
         BAS   RE,TABBLD           BUILD TABLE ENTRY                            
         B     GETB24                                                           
*---------------------                                                          
* GET THE NEXT RECORD                                                           
*---------------------                                                          
GETB30   LA    R3,IOKEY                                                         
         GOTO1 ANXTREC,NXTRMODE    GET NEXT RECORD                              
         BE    GETB01              NO ERRORS                                    
         TM    DISPFLAG,DISIOMAX                                                
         BO    GETB32                                                           
         DC    H'0'                                                             
*                                                                               
GETB32   OI    DISPFLAG,DISIOMAX                                                
         MVC   KEYSAVE,IOKEY                                                    
GETBX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*        GET AMOUNTS FROM TRANSACTION RECORDS                         *         
* ON ENTRY 'KEYSAVE' SET TO SPACES MEANS 1ST TIME THROUGH             *         
*                                                                     *         
* THIS ROUTINE IS EXECUTED WHEN :                                     *         
*     THE TYPE = FM                                                   *         
*     THE TYPE = MF                                                   *         
*     THE TYPE = MO AND FILTERING BY OFFICE                           *         
*     THE TYPE = MO AND NOT FILTERING BY OFFICE BUT OFFICE IS IN THE  *         
*                TRANSACTION AND THERE IS LIMITED ACCESS.             *         
*                                                                     *         
***********************************************************************         
GETTRN   NTR1                                                                   
*                                                                               
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
         LA    R3,IOKEY                                                         
         USING TRNRECD,R3                                                       
         CLC   KEYSAVE,SPACES                                                   
         BE    GETT02                                                           
         GOTO1 AIO,IOREAD+IOACCDIR+IO1 RE-ESTABLISH SEQUENCE                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETT02   CLC   TRNKCULA,SAVEACC    SAME ACCOUNT?                                
         BNE   GETTX                NO, DONE                                    
         CLC   TRNKDATE,SPACES     TRANSACTION RECORD?                          
         BH    GETT14               YES                                         
         CLC   TRNKCULC,SPACES      NO, ACCOUNT/CONTRA RECORD?                  
         BNE   GETT36                YES, GET NEXT                              
         CLC   TRNKOFF,SPACES        NO, ACCOUNT/OFFICE RECORD?                 
         BNH   GETT36                 NO, GET NEXT                              
         OC    CONTLEN,CONTLEN     LIMITED BY CONTRA?                           
         BNZ   GETT36               YES, CAN'T DO THIS WAY                      
*                                                                               
         GOTO1 AOFFOFF                                                          
         BNE   GETT36               YES, GET NEXT RECORD                        
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO1 GET THE RECORD                            
         BE    *+14                 NO ERRORS                                   
         TM    IOERR,IOMAX          MAX IOS REACHED?                            
         BO    GETT38                                                           
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO1             SHOULD BE OFFICE/ACCOUNT RECORD              
         MVC   OFFICE,OFAKOFF-OFAKEY(R3)                                        
*                                                                               
         LA    RF,TRNRFST           NO, ADDRESS FIRST ELEMENT                   
         XC    PBMOS,PBMOS         CLEAR IN CASE NO DATE FOUND                  
GETT04   CLI   0(RF),EOR           END OF RECORD?                               
         BE    GETT36              YES, GET NEXT RECORD                         
         CLI   0(RF),RSTELQ        STATUS ELEMENT?                              
         BE    GETT08              YES                                          
         CLI   0(RF),ABLELQ        BALANCE ELEMENT?                             
         BE    GETT10              YES                                          
*                                                                               
GETT06   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     GETT04                                                           
*                                                                               
GETT08   MVC   PBMOS,RSTBDATE-RSTEL(RF)                                         
         B     GETT06                                                           
*                                                                               
GETT10   CLI   PBALDFLT,C'Y'                                                    
         BNE   GETT36                                                           
*                                                                               
         ZAP   PBCR,=P'0'                                                       
         ZAP   PBDR,ABLFRWD-ABLEL(8,RF)                                         
         BZ    GETT36              NOTHING TO POST                              
         BP    GETT12                                                           
         SP    PBCR,PBDR                                                        
         ZAP   PBDR,=P'0'                                                       
*                                                                               
GETT12   OI    STATFLAG,SHOWAST                                                 
         BAS   RE,TABBLD                                                        
         B     GETT36              GET NEXT                                     
*--------------------------                                                     
* CONTRA ACCOUNT FILTERING                                                      
*--------------------------                                                     
GETT14   SR    RF,RF                                                            
         ICM   RF,1,CONTLEN        CONTRA FILTER?                               
         BZ    GETT17              NO                                           
         BCTR  RF,0                                                             
         LA    RE,TRNKULC          RE=A(CONTRA CODE)                            
         CLI   0(RE),C' '          STRIP OFF LEADING SPACES                     
         BH    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   CONTRA(0),0(RE)     DOES CONTRA MATCH?                           
         BE    GETT16              YES                                          
         CLI   NEGCONT,NEGFILTR    NO, TRY NEGATIVE FILTER                      
         BNE   GETT36                                                           
         B     *+12                                                             
GETT16   CLI   NEGCONT,NEGFILTR    NEGATIVE FILTER                              
         BE    GETT36                                                           
*                                                                               
*------------------------------                                                 
* FILTER OUT DRAFTS AND PEELED                                                  
*------------------------------                                                 
GETT17   TM    TRNKSTA2,TRNSPEEL   PEELED TRANS?                                
         BO    GETT36              . YES, SKIP                                  
*                                                                               
         TM    TRNKSTA,TRNSDRFT    DRAFT TRANSACTION?                           
         BZ    GETT18              . NO                                         
         CLI   ODRAFT,C'Y'         INCLUDE DRAFTS?                              
         BE    GETT19              . YES                                        
         CLI   ODRAFT,C'O'         DRAFTS ONLY?                                 
         BE    GETT19              . YES                                        
         B     GETT36              OTHERWISE, SKIP                              
GETT18   CLI   ODRAFT,C'O'         DRAFTS ONLY?                                 
         BE    GETT36              . YES, SKIP                                  
*                                                                               
*--------------------                                                           
* WORKCODE FILTERING                                                            
*--------------------                                                           
GETT19   CLC   BASKEY(L'SPROUNIT+L'SPROLEDG),SPROUNIT                           
         BNE   GETT22               PROD LEDGER ONLY                            
         OC    OWCODE,OWCODE        WORKCODE FILTER?                            
         BZ    GETT22               NO                                          
         SR    RF,RF                                                            
         IC    RF,OWCODELN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKWORK(0),OWCODEVL DOES WORKCODE MATCH?                        
         BE    GETT20               YES                                         
         CLI   OWCODEFI,NEGFILTR    NO, TRY NEGATIVE FILTER                     
         BNE   GETT36                                                           
         B     *+12                                                             
GETT20   CLI   OWCODEFI,NEGFILTR                                                
         BE    GETT36                                                           
*-----------------------------                                                  
* GET TRANSACTION AND PROCESS                                                   
*-----------------------------                                                  
GETT22   GOTO1 AIO,IOGET+IOACCMST+IO1 GET THE RECORD                            
         BE    *+14                 NO ERRORS                                   
         TM    IOERR,IOMAX          MAX IOS REACHED?                            
         BO    GETT38                                                           
         DC    H'0'                                                             
*                                                                               
         GOTO1 AOFFTRN             CHECK SECURITY ON TRANSACTION                
         TM    DISPFLAG,DISIOMAX                                                
         BO    GETT38              ERROR                                        
         CLI   OFFLFLAG,0          ANY SECURITY ERRORS?                         
         BNE   GETT36              YES, GET NEXT RECORD                         
*                                                                               
         L     R3,AIO1             R3=A(TRANSACTION RECORD)                     
         MVC   PBMOS,TRNRSMOS                                                   
         MVC   OFFICE,SPACES                                                    
         CLI   SVACTNUM,ACTNMNTH   TYPE=MO?                                     
         BE    *+10                YES                                          
         MVC   OFFICE,TRNKOFF      NO, SAVE THE OFFICE                          
*                                                                               
         LA    R3,TRNRFST          R3=A(FIRST ELEMENT ON RECORD)                
GETT24   CLI   0(R3),EOR           END OF RECORD?                               
         BE    GETT36              YES, GET NEXT RECORD                         
         CLI   0(R3),TRNELQ        TRANSACTION ELEMENT?                         
         BE    GETT28              YES                                          
         CLI   0(R3),SCIELQ        SUBSIDIARY CASH INFO ELEMENT?                
         BE    GETT34              YES                                          
GETT26   SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GETT24                                                           
*---------------------                                                          
* TRANSACTION ELEMENT                                                           
*---------------------                                                          
         USING TRNELD,R3           TRANSACTION ELEMENT                          
GETT28   CLI   OBUKTYPE,C' '       CASH BUCKETS WANTED?                         
         BNE   GETT26              YES                                          
         TM    TRNSTAT,TRNSDR                                                   
         BO    GETT30                                                           
         ZAP   PBDR,=P'0'          SAVE CREDITS HERE                            
         ZAP   PBCR,TRNAMNT                                                     
         B     GETT32                                                           
GETT30   ZAP   PBDR,TRNAMNT        SAVE DEBITS HERE                             
         ZAP   PBCR,=P'0'                                                       
GETT32   BAS   RE,TABBLD           BUILD TABLE ENTRY                            
         B     GETT26                                                           
         DROP  R3                                                               
*-------------------------                                                      
* SUBSIDIARY CASH ELEMENT                                                       
*-------------------------                                                      
         USING SCIELD,R3           SUBSIDIARY CASH INFO ELEMENT                 
GETT34   GOTO1 ABUKMAKE,DMCB,(R3),AIO1,PBDR,PBCR GET 'BUCKET' AMOUNTS           
         BNE   GETT26              NO 'BUCKET' AMOUNTS FROM SCIELD?             
         BAS   RE,TABBLD           BUILD TABLE ENTRY                            
         B     GETT26                                                           
         DROP  R3                                                               
*-----------------                                                              
* GET NEXT RECORD                                                               
*-----------------                                                              
GETT36   TM    STATFLAG,ROFFBUKQ   CAME FROM OFFICE BUCKET ROUTINE?             
         BO    GETO75              . YES, THEN GO BACK                          
*                                                                               
         LA    R3,IOKEY                                                         
         GOTO1 ANXTREC,NXTRMODE    GET NEXT RECORD                              
         BE    GETT02              NO ERRORS                                    
         TM    DISPFLAG,DISIOMAX                                                
         BO    GETT38                                                           
         DC    H'0'                                                             
*-----------------                                                              
GETT38   OI    DISPFLAG,DISIOMAX                                                
         MVC   KEYSAVE,IOKEY                                                    
GETTX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        BUILD TABLE ENTRY                                            *         
* ON ENTRY PBSTATUS SET TO 'MTHPREVQ'  IF PARMS FROM PREV BUCK ELEMENT*         
*          PBMOS    CONTAINS MOA                                      *         
*          PBDR     CONTAINS DEBIT AMOUNT                             *         
*          PBCR     CONTAINS CREDIT AMOUNT                            *         
* ON EXIT  PBSTATUS CLEARED IF AMOUNT PLACED IN BAL BFWD              *         
***********************************************************************         
TABBLD   NTR1                                                                   
*                                                                               
         L     R2,AOPTVALS                                                      
         USING OPTVALSD,R2                                                      
*                                                                               
**NOP    CLI   PBALDFLT,C'Y'       DEFAULT TO FISCAL YEAR?                      
*        BNE   *+14                (NO) SKIP THIS CHECK                         
*        CLC   PBMOS,COMPFIN       MOS BEFORE FINANCIAL YEAR START?             
*        BL    TABB50              (YES)                                        
*                                                                               
         OC    OMOS,OMOS           ANY MOA ENTERED?                             
         BZ    TABB40              (NO)                                         
*                                                                               
         CLI   OMOSFI,NEGFILTR     NEGATIVE FILTERING?                          
         BE    TABB20              (YES)                                        
*--------------------                                                           
* POSITIVE FILTERING                                                            
*--------------------                                                           
         OC    OMOSST,OMOSST       ANY START MONTH?                             
         BNZ   TABB08              (YES)                                        
         OC    PBMOS,PBMOS         ANY POSTING DATE?                            
         BZ    TABB10              (NO) TREAT AS LOW                            
*                                                                               
TABB08   CLC   PBMOS,OMOSST        TRAN MOA BEFORE START MOA?                   
         BNL   TABB15              (NO)                                         
*                                                                               
TABB10   CLI   OBFWD,C'N'          SHOW AS BALANCE FORWARD?                     
         BE    TABB50              (NO)                                         
         CLC   PBMOS,COMPFIN       MOS BEFORE FINANCIAL YEAR START?             
         BL    TABB50              (YES)                                        
*                                                                               
         MVC   PBMOS,BFWMOS        BFWD MOS                                     
         NI    PBSTATUS,X'FF'-MTHPREVQ                                          
         B     TABB40                                                           
*                                                                               
TABB15   CLC   PBMOS,OMOSEN        TRAN MOA AFTER END MOA?                      
         BNH   TABB40              (NO)                                         
         OC    OMOSEN,OMOSEN       YES, ANY MOA END SET?                        
         BZ    TABB40              (NO)                                         
         B     TABB50              (YES)                                        
*--------------------                                                           
* NEGATIVE FILTERING                                                            
*--------------------                                                           
TABB20   OC    OMOSST,OMOSST       ANY START?                                   
         BZ    *+14                 NO                                          
         OC    OMOSEN,OMOSEN        YES, ANY END?                               
         BNZ   TABB30               YES                                         
*                                                                               
         CLC   PBMOS,OMOSEN        TRAN MOA BEYOND NEGATIVE END MOA?            
         BH    TABB25               YES                                         
         CLI   OBFWD,C'N'           NO, SHOW AS BAL BFWD?                       
         BE    TABB50               NO                                          
         MVC   PBMOS,BFWMOS        BFWD MOS                                     
         NI    PBSTATUS,X'FF'-MTHPREVQ                                          
         B     TABB40                                                           
*                                                                               
TABB25   CLC   PBMOS,OMOSST        TRAN MOA BEFORE NEGATIVE START MOA?          
         BL    TABB40               YES                                         
         OC    OMOSST,OMOSST       ANY NEGATIVE START MOA?                      
         BZ    TABB40               NO                                          
         B     TABB50               YES                                         
*                                                                               
TABB30   CLC   PBMOS,OMOSST        TRAN MOA BEFORE START MOA?                   
         BL    TABB40               YES                                         
         CLC   PBMOS,OMOSEN        NO, TRAN MOA BEYOND END MOA?                 
         BNH   TABB50               NO                                          
*----------------------------------                                             
* GET THE DETAILS AND ADD TO TABLE                                              
*----------------------------------                                             
TABB40   GOTO1 GETDET,DMCB,(PBSTATUS,PBMOS),(L'PBDR,PBDR),(L'PBCR,PBCR)X        
               ,ALLOFF                                                          
         CLI   SVACTNUM,ACTNMNTH   TYPE=MO?                                     
         BE    TABB46              YES                                          
         GOTO1 GETDET,DMCB,(PBSTATUS,PBMOS),(L'PBDR,PBDR),(L'PBCR,PBCR)X        
               ,OFFICE                                                          
*                                                                               
TABB46   MVC   PBMOS,ALLMOS        ALL MOS                                      
         GOTO1 GETDET,DMCB,(PBSTATUS,PBMOS),(L'PBDR,PBDR),(L'PBCR,PBCR)X        
               ,OFFICE                                                          
         CLI   SVACTNUM,ACTNMNTH   TYPE=MO?                                     
         BE    TABBX               YES                                          
         GOTO1 GETDET,DMCB,(PBSTATUS,PBMOS),(L'PBDR,PBDR),(L'PBCR,PBCR)X        
               ,ALLOFF                                                          
         B     TABBX                                                            
*----------------------------------                                             
TABB50   NI    PBSTATUS,X'FF'-MTHPREVQ                                          
*                                                                               
TABBX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        GET DETAILS FOR MONTH TABLE                                  *         
* ON ENTRY PARM 1 BYTE 0   STATUS                                     *         
*                      1-3 A(MONTH DATE)                              *         
*          PARM 2 BYTE 0   L'(DEBITS FIELD)                           *         
*                      1-3 A(DEBITS FIELD)                            *         
*          PARM 3 BYTE 0   L'(CREDITS FIELD)                          *         
*                      1-3 A(CREDITS FILED)                           *         
*          PARM 4 BYTE 0                                              *         
*                      1-3 A(OFFICE)                                  *         
* ON EXIT CC EQUAL ENTRY ADDED TO TABLE                               *         
*         CC UNEQUAL ENTRY REJECTED                                   *         
***********************************************************************         
GETDET   NTR1                                                                   
*                                                                               
         LM    R2,R5,0(R1)         R2=A(DATE),R3=A(DEBITS),R4=A(CREDTS)         
         MVC   PDATE,0(R2)                                                      
*                                                                               
         LR    RF,R3                                                            
         SRL   RF,24               RF=L'(DEBITS PARM)                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         ZAP   PDEBITS,0(0,R3)     GET DEBITS                                   
*                                                                               
         LR    RF,R4                                                            
         SRL   RF,24               RF=L'(CREDITS PARM)                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         ZAP   PCREDITS,0(0,R4)    GET CREDITS                                  
*                                                                               
         MVC   HALF,0(R5)                                                       
*                                                                               
         USING MTHTABD,R4                                                       
         L     R4,ATEMPBLK         R4=A(MONTH TABLE)                            
         LA    RE,1                RE=(NUMBER OF ENTRIES IN TABLE)              
*                                                                               
GETD02   CLI   0(R4),EOT           END OF TABLE?                                
         BE    GETD06              YES                                          
         LA    RF,MTHDATE1         MTHDATE1 FOR TYPE=MO & MF                    
         CLI   SVACTNUM,ACTNOFMN                                                
         BNE   *+8                                                              
         LA    RF,MTHDATE2         MTHDATE2 FOR FOR TYPE=FM                     
*                                                                               
         CLC   PDATE,0(RF)         MATCH ON MONTH?                              
         BNE   GETD04              NO                                           
*                                                                               
         CLI   SVACTNUM,ACTNMNTH   TYPE=MO?                                     
         BE    GETD10              YES                                          
*                                                                               
         LA    RF,MTHOFF1          MTHOFF1 FOR TYPE=MF                          
         CLI   SVACTNUM,ACTNOFMN                                                
         BNE   *+8                                                              
         LA    RF,MTHOFF2          MTHOFF2 FOR TYPE=FM                          
*                                                                               
         CLC   HALF,0(RF)          MATCH ON OFFICE?                             
         BE    GETD10              YES                                          
*                                                                               
GETD04   LA    RE,1(RE)                                                         
         LA    R4,MTHLNQ(R4)       BUMP UP TABLE                                
         B     GETD02                                                           
*                                                                               
GETD06   CH    RE,=Y(MAXMTHQ)      MAXIMUM ENTRIES EXCEEDED?                    
         BNH   GETD08              NO                                           
         OI    DISPFLG2,DISMOMAX   YES, INDICATE DATA MISSING                   
         B     GETDREJX                                                         
*                                                                               
GETD08   STCM  RE,3,TEMPBLKN       REFRESH NUMBER OF ENTRIES                    
*                                                                               
         LA    RF,MTHDATE1         MTHDATE1 FOR TYPE=MO & MF                    
         CLI   SVACTNUM,ACTNOFMN                                                
         BNE   *+8                                                              
         LA    RF,MTHDATE2         MTHDATE2 FOR TYPE=FM                         
         MVC   0(L'MTHDATE1,RF),PDATE DATE                                      
*                                                                               
         LA    RF,MTHOFF1          MTHOFF1 FOR TYPE=MO & MF                     
         CLI   SVACTNUM,ACTNOFMN                                                
         BNE   *+8                                                              
         LA    RF,MTHOFF2          MTHOFF2 FOR TYPE=FM                          
         MVC   0(L'MTHOFF1,RF),HALF OFFICE                                      
*                                                                               
         ZAP   MTHDR,PDEBITS       DEBITS                                       
         ZAP   MTHCR,PCREDITS      CREDITS                                      
         B     GETD12                                                           
*                                                                               
GETD10   AP    MTHDR,PDEBITS                                                    
         AP    MTHCR,PCREDITS                                                   
GETD12   STCM  R2,8,MTHSTAT        NO, TAKE WHAT WAS PASSED                     
*                                                                               
GETDX    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
GETDREJX LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        BUILD TSAR RECORDS FOR SCREEN DATA ITEMS                     *         
* ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL OR MAX IO                    *         
***********************************************************************         
BLDTSDAT NTR1                                                                   
*                                                                               
         L     R4,ATEMPBLK         R4=A(TEMP BLOCK)                             
         USING MTHTABD,R4                                                       
         SR    RF,RF                                                            
         ICM   RF,3,TSLSTREC       RF=(NO OF RECS IN TSAR)                      
         SR    R5,RF               R5=(NO OF RECS STILL TO PUT IN TSAR)         
         MHI   RF,TSARDLNQ                                                      
         LA    R4,0(RF,R4)         R4=A(NEXT TABLE ENTRY FOR TSAR)              
*                                                                               
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
*                                                                               
         LHI   RF,TSARDLNQ                                                      
         AHI   RF,TSARDATA-TSARRECD                                             
         STCM  RF,3,TSARLEN                                                     
*                                                                               
         LA    R2,TSARDATA         R2=A(TSAR DATA)                              
         USING TSARDATD,R2                                                      
*                                                                               
BLDT02   MVC   TSARKYNO,TSCURRNO                                                
         MVI   TSDFMT,TSITEM1      ITEM FORMAT TYPE                             
*                                                                               
         LA    RF,MTHDATE1         MTHDATE1 FOR TYPE=MO & MF                    
         CLI   SVACTNUM,ACTNOFMN                                                
         BNE   *+8                                                              
         LA    RF,MTHDATE2         MTHDATE2 FOR TYPE=FM                         
         MVC   TSDDATE,0(RF)       SET MONTH                                    
*                                                                               
         LA    RF,MTHOFF1          MTHOFF1 FOR TYPE=MO & MF                     
         CLI   SVACTNUM,ACTNOFMN                                                
         BNE   *+8                                                              
         LA    RF,MTHOFF2          MTHOFF2 FOR TYPE=FM                          
         MVC   TSDOFF,0(RF)        SET OFFICE                                   
*                                                                               
         XC    TSDDATE2,TSDDATE2   MONTH 2(FOR PRIOR BUCKET RANGE)              
         TM    MTHSTAT,MTHPREVQ    PRIOR BUCKET ENTRY?                          
         BZ    *+10                NO                                           
         MVC   TSDDATE2,STARTMTH   SET MONTH RANGE                              
*                                                                               
         ZAP   TSDDR,MTHDR         DEBITS FOR MONTH                             
         ZAP   TSDCR,MTHCR         CREDITS FOR MONTH                            
         ZAP   TSDCFDR,DEBTOT      DEBITS CARRIED FORWARD                       
         ZAP   TSDCFCR,CRETOT      CREDITS CARRIED FORWARD                      
*                                                                               
         CLC   TSDDATE,ALLMOS      ALL MOS?                                     
         BE    BLDT04              YES                                          
         CLI   SVACTNUM,ACTNMNTH   TYPE=MO?                                     
         BE    *+14                 YES                                         
         CLC   TSDOFF,ALLOFF       ALL OFFICE?                                  
         BE    BLDT10              YES                                          
         AP    DEBTOT,MTHDR        TOTAL DEBITS                                 
         AP    CRETOT,MTHCR        TOTAL CREDITS                                
         CLI   SVACTNUM,ACTNMNTH   TYPE=MO?                                     
         B     BLDT10               YES                                         
*                                                                               
BLDT04   CLC   TSDOFF,ALLOFF       ALL MOS AND ALL OFFICE?                      
         BNE   BLDT10               NO                                          
         ZAP   TSDDR,DEBTOT        DEBITS FOR MONTH                             
         ZAP   TSDCR,CRETOT        CREDITS FOR MONTH                            
*                                                                               
BLDT10   TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID                      
         BO    *+8                  YES                                         
         BAS   RE,FORMTSAR         FORMAT ONTO DUMMY SCREEN LINES               
*                                                                               
         MVC   TSDLINES,LINSUSED   NUMBER OF SCREEN LINES USED                  
*                                                                               
         GOTO1 ATSARADD            ADD RECORD                                   
         BNE   BLDTERRX            TSAR ERROR                                   
*                                                                               
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
BLDT20   LA    R4,MTHLNQ(R4)       BUMP R4 TO NEXT TABLE ENTRY                  
         BCT   R5,BLDT02                                                        
         B     BLDTX                                                            
*                                                                               
BLDTERRX LTR   RB,RB                                                            
         B     XIT                                                              
BLDTX    CR    RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
FORMTSAR NTR1                                                                   
         MVI   LINSUSED,0          NUMBER OF LINES DISPLAYED                    
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
*                                                                               
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN LINES                     
         LH    R1,=Y(L'DUMLIN1*NDUMLINE)                                        
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING SCRLIN1D,R2                                                      
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         L     R4,ATSARREC                                                      
         LA    R3,TSARDATA-TSARRECD(R4)                                         
*                                                                               
         USING TSARDATD,R3                                                      
         CLI   SVACTNUM,ACTNMNTH   TYPE=MO?                                     
         BE    FORMT02             YES                                          
*                                                                               
         LA    RF,SCR1MFOF                                                      
         CLI   SVACTNUM,ACTNOFMN   SET OFFICE FOR MF AND FM                     
         BNE   *+8                                                              
         LA    RF,SCR1FMOF                                                      
         MVC   0(L'SCR1MFOF,RF),TSDOFF                                          
*                                                                               
         CLC   TSDOFF,ALLOFF                                                    
         BNE   *+10                                                             
         MVC   0(L'MX@ALL,RF),MX@ALL ALL OFFICE                                 
*                                                                               
FORMT02  LA    RF,SCR1DATE         SET DATE FIELD                               
         CLI   SVACTNUM,ACTNOFMN                                                
         BNE   *+8                                                              
         LA    RF,SCR1FMDT                                                      
*                                                                               
         MVI   STAR,C' '           CLEAR STAR                                   
         OC    TSDDATE,TSDDATE     ANY DATE AT ALL?                             
         BZ    *+14                MUST BE A BALANCE FORWARD THEN               
         CLC   TSDDATE,BFWMOS      BALANCE FORWARD DATA                         
         BNE   FORMT04             NO                                           
*                                                                               
         MVC   0(L'MX@BAL11,RF),MX@BAL11                                        
         MVI   DISATRIB,HILIGHTQ                                                
*                                                                               
         USING OPTVALSD,RF                                                      
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         OC    CLOSEDAT,CLOSEDAT   ANY CLOSE DATE?                              
         BZ    FORMT08             NO                                           
         OC    OMOS,OMOS           ANY FILTERING?                               
         BZ    FORMT08             NO                                           
         CLC   OMOSST,CLOSEDAT     YES, CHECK AGAINST PEEL DATE                 
         BH    FORMT08             PAST CLOSE                                   
*                                                                               
         TM    STATFLAG,SHOWAST    BBF TAKEN FROM BALANCE REC?                  
         BNO   FORMT08                                                          
*                                                                               
         MVI   STAR,C'*'                                                        
         B     FORMT08                                                          
         DROP  RF                                                               
*                                                                               
FORMT04  CLC   TSDDATE,ALLMOS                                                   
         BNE   *+14                                                             
         MVC   0(L'MX@ALL,RF),MX@ALL                                            
         B     FORMT08                                                          
*                                                                               
         OC    TSDDATE2,TSDDATE2                DATE RANGE?                     
         BZ    FORMT06                           NO                             
*                                                                               
         MVC   WORK(L'TSDDATE2),TSDDATE2                                        
         MVI   WORK+2,X'01'                                                     
         GOTO1 VDATCON,DMCB,(1,WORK),(9,(RF))    START MONTH                    
         LA    RF,SCR1DATE                                                      
         CLI   SVACTNUM,ACTNOFMN   SET DATE FIELD                               
         BNE   *+8                                                              
         LA    RF,SCR1FMDT                                                      
         LA    RF,L'SCR1DATE-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'-'                                                       
         LA    RF,2(RF)                                                         
*                                                                               
FORMT06  MVC   WORK(L'TSDDATE),TSDDATE                                          
         MVI   WORK+2,X'01'                                                     
         GOTO1 VDATCON,DMCB,(1,WORK),(9,(RF)) END MONTH/SINGLE MONTH            
*                                                                               
FORMT08  CURED (P8,TSDCR),(L'SCR1CR,SCR1CR),2,MINUS=YES                         
         LA    RF,SCR1DR                                                        
         CLI   SVACTNUM,ACTNMNTH   TYPE=MO?                                     
         BE    FORMT10             YES                                          
         LA    RF,SCR1MFDR                                                      
         CLI   SVACTNUM,ACTNMNOF   TYPE=MF?                                     
         BE    FORMT10             YES                                          
         LA    RF,SCR1FMDR         ASSUME TYPE=FM                               
*                                                                               
FORMT10  CURED (P8,TSDDR),(L'SCR1DR,(RF)),2,MINUS=YES                           
         ZAP   BALANCE,TSDDR                                                    
         SP    BALANCE,TSDCR                                                    
*                                                                               
         CLC   ONER,BASKEY         1R ACCOUNT?                                  
         BNE   FORMT12             NO                                           
         L     RF,AOPTVALS                                                      
         USING OPTVALSD,RF                                                      
         CLI   OBUKTYPE,C'H'       BUCKET TYPE=H?                               
         BNE   FORMT12             NO                                           
         ZAP   BALANCE,TSDCR                                                    
*                                                                               
FORMT12  CURED (P8,BALANCE),(L'SCR1BAL,SCR1BAL),2,MINUS=YES                     
         MVC   SCR1BAL+22(L'STAR),STAR                                          
         MVI   LINSUSED,1                                                       
         CLI   SVACTNUM,ACTNOFMN   TYPE=FM?                                     
         BNE   FORMT14             NO                                           
         CLC   TSDDATE,ALLMOS                                                   
         BNE   FORMT14                                                          
         MVI   DISATRIB,HILIGHTQ                                                
*                                                                               
FORMT14  CLI   SVACTNUM,ACTNMNOF   TYPE=MF?                                     
         BNE   FORMTX              NO                                           
         CLC   TSDOFF,ALLOFF                                                    
         BNE   FORMTX                                                           
         MVI   DISATRIB,HILIGHTQ                                                
*                                                                               
FORMTX   B     XIT                                                              
         DROP  R2,R3,RF                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES FOR GRIDS       *         
***********************************************************************         
FGRMTSAR NTR1                                                                   
*                                                                               
FGRM10   MVI   LINSUSED,0          NUMBER OF LINES DISPLAYED                    
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
*                                                                               
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN LINES                     
         LH    R1,=Y(L'DUMLIN1*NDUMLINE)                                        
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,ATSARREC                                                      
         USING TSARRECD,R4                                                      
         LA    R4,TSARDATA                                                      
         USING TSARDATD,R4                                                      
*                                                                               
         TM    DETFLAG,DETDONE          DONE WITH GRIDS?                        
         BO    FGRM30                   . YES                                   
         TM    DETFLAG,DETGRINQ         INITIALIZED ALREADY?                    
         BO    FGRM20                   . YES                                   
*                                                                               
         GOTO1 ADISGRD,DMCB,('DWNINIT',AGCTBL)                                  
         GOTO1 ADISPLAY,DISATRIB        DISPLAY DUMMY SCREEN LINES              
         OI    DETFLAG,DETGRINQ                                                 
         B     FGRM10                                                           
*                                                                               
FGRM20   GOTO1 ADISGRD,DMCB,(0,AGCTBL)                                          
         B     FGRMX                                                            
*                                                                               
FGRM30   TM    DETFLAG,DETGRINQ         INITIALIZED ALREADY?                    
         BZ    FGRMX                                                            
FGRM35   GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL)                                   
*                                                                               
FGRMX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY BALANCE BROUGHT/CARRIED FORWARD LINES                *         
*        IF BFWD = N, THIS ROUTINE WILL PRINT A BALANCE CARRIED       *         
*        FORWARD FROM ONE SCREEN TO THE NEXT FOR RECORD = MO          *         
***********************************************************************         
DISBAL   NTR1                                                                   
         TM    DISPFLAG,BALANCEQ   DOES DISPLAY HAVE BALANCE LINE?              
         BNO   DISBX               NO                                           
*                                                                               
         SR    RF,RF                                                            
         IC    RF,DISLINE          RF=(NEXT FREE SCREEN LINE NUMBER)            
         LR    RE,RF                                                            
         CLC   DISLINE,DISSTART    START OF DISPLAY?                            
         BNE   DISB02              NO                                           
         CLC   TSCURRNO,=H'1'      FIRST TSAR REC?                              
         BE    DISBX               YES                                          
*                                                                               
         LA    RE,1(RE)                                                         
         STC   RE,DISLINE          DISPLAY BALANCE B/F                          
         MHI   RF,ENQDAT2H-ENQDAT1H                                             
         LA    R2,ENQDAT1H(RF)                                                  
         USING FLDHDRD,R2                                                       
         LA    R3,FLDDATA                                                       
         MVC   FLDDATA(L'ENQDAT1),SPACES                                        
         USING SCRLIN1D,R3                                                      
         MVC   SCR1DATE,MX@BALBF                                                
         B     DISB04                                                           
*                                                                               
DISB02   SR    R3,R3                                                            
         IC    R3,DISEND           R3=(LAST ALLOWED LINE NUMBER)                
         SR    R0,R0                                                            
         IC    R0,LINSUSED                                                      
         BCTR  R0,0                                                             
         AR    RF,R0                                                            
         CR    RF,R3               ROOM TO INCLUDE THIS ITEM?                   
         BL    DISBX                                                            
*                                                                               
         LA    R2,ENQDAT1H         DISPLAY BALANCE CARRIED FORWARD              
         MHI   RE,ENQDAT2H-ENQDAT1H                                             
         LA    R2,0(RE,R2)                                                      
         GOTO1 ASCRNCLR,DISLINE                                                 
         LA    R3,1(R3)                                                         
         STC   R3,DISLINE          DISPLAY BALANCE CARRIED FORWARD              
         USING FLDHDRD,R2                                                       
         LA    R3,FLDDATA                                                       
         MVC   FLDDATA(L'ENQDAT1),SPACES                                        
         USING SCRLIN1D,R3                                                      
         MVC   SCR1DATE,MX@BALCF                                                
*                                                                               
DISB04   L     R4,ATSARREC         R4=A(TSAR RECORD)                            
         USING TSARRECD,R4                                                      
         LA    R4,TSARDATA         R4=A(TSAR RECORD DATA)                       
         USING TSARDATD,R4                                                      
         CURED (P8,TSDCFCR),(L'SCR1CR,SCR1CR),2,MINUS=YES                       
         CURED (P8,TSDCFDR),(L'SCR1DR,SCR1DR),2,MINUS=YES                       
*                                                                               
         ZAP   BALANCE,TSDCFDR                                                  
         SP    BALANCE,TSDCFCR                                                  
         CLC   ONER,BASKEY         1R ACCOUNT?                                  
         BNE   DISB06              NO                                           
*                                                                               
         USING OPTVALSD,RF                                                      
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         CLI   OBUKTYPE,C'H'       BUCKET TYPE=H?                               
         BNE   DISB06              NO                                           
         ZAP   BALANCE,TSDCFCR                                                  
*                                                                               
DISB06   CURED (P8,BALANCE),(L'SCR1BAL,SCR1BAL),2,MINUS=YES                     
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         B     DISBX                                                            
*                                                                               
DISBX    B     XIT                                                              
         DROP  R2,R3,R4,RF                                                      
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DEAL WITH THE TOTAL LINE                                     *         
***********************************************************************         
         USING SCRLIN1D,R2                                                      
TOTAL    NTR1                                                                   
*                                                                               
         GOTO1 ASCRNCLR,DISLINE                                                 
         LA    R2,ENQDATL          R2=A(TOTAL LINE)                             
         MVC   SCR1DATE(L'MX@TOTAL),MX@TOTAL                                    
*                                                                               
         LA    RF,SCR1DR                                                        
         CLI   SVACTNUM,ACTNMNTH   TYPE=MO?                                     
         BE    TOT02               YES                                          
*                                                                               
         LA    RF,SCR1MFDR                                                      
         CLI   SVACTNUM,ACTNMNOF   TYPE=MF?                                     
         BE    TOT02               YES                                          
*                                                                               
         LA    RF,SCR1FMDR         ASSUME TYPE=FM                               
*                                                                               
TOT02    CURED (P8,DEBTOT),(L'SCR1DR,(RF)),2,MINUS=YES                          
         CURED (P8,CRETOT),(L'SCR1CR,SCR1CR),2,MINUS=YES                        
         ZAP   BALANCE,DEBTOT                                                   
         SP    BALANCE,CRETOT                                                   
*                                                                               
         CLC   ONER,BASKEY         1R ACCOUNT?                                  
         BNE   TOT04               NO                                           
         L     RF,AOPTVALS                                                      
         USING OPTVALSD,RF                                                      
         CLI   OBUKTYPE,C'H'       BUCKET TYPE=H?                               
         BNE   TOT04               NO                                           
         ZAP   BALANCE,CRETOT                                                   
*                                                                               
TOT04    CURED (P8,BALANCE),(L'SCR1BAL,SCR1BAL),2,MINUS=YES                     
*                                                                               
         SR    RF,RF                                                            
         IC    RF,DISEND                                                        
         LA    RF,2(RF)                                                         
         STC   RF,DISLINE                                                       
         LA    R2,ENQDATLH                                                      
         OI    FLDOIND-FLDHDRD(R2),FOUTTRN                                      
         OI    FLDATB-FLDHDRD(R2),FATBHIGH                                      
         B     XIT                                                              
         DROP  R2,RF                                                            
         EJECT                                                                  
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
ERRXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
XIT1     XIT1  REGS=(R1)                                                        
         EJECT                                                                  
*                                                                               
***********************************************************************         
* CONSTANTS                                                                     
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
HEXFFS   DC    X'FFFFFF'                                                        
ONEC     DC    C'1C'                                                            
ONER     DC    C'1R'                                                            
*                                                                               
SINCUL   DC    C'SI'                                                            
GPROUL   DC    C'GP'                                                            
*                                                                               
BFWMOS   DC    X'0162'             BROUGHT FORWARD MOS                          
ALLMOS   DC    X'FFFF'             ALL MOS                                      
ALLOFF   DC    X'FFFF'             ALL OFFICE                                   
*                                                                               
MAXMTHQ  EQU   (L'TEMPBLK/MTHLNQ)  MAXIMUM NUMBER OF ENTRIES IN TABLE           
         EJECT                                                                  
DCMIX    DS    0X                                                               
         DCDDL AC#ENH13,78         MONTH HEADINGS                               
         DCDDL AC#ENH32,78         MONTH BY OFFICE HEADINGS                     
         DCDDL AC#ENH33,78         OFFICE BY MONTH HEADINGS                     
         DCDDL AC#ACC,9                                                         
         DCDDL AC#CTR,9                                                         
         DCDDL AC#TOTAL,9                                                       
         DCDDL AC#BALBF,3                                                       
         DCDDL AC#BALBF,15                                                      
         DCDDL AC#BALCF,15                                                      
         DCDDL AC#XJOB,11                                                       
         DCDDL AC#MOA,3                                                         
         DCDDL AC#ALL,3                                                         
         DCDDL AC#BALBF,11                                                      
         DCDDL AC#DFRNC,10                                                      
         DCDDL AC#SUBT,L'MX@SUBT                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DISPLAY MONTH AND OFFICE COLUMNS FOR GRIDS                   *         
*         R3 = ADDRESS OF GRID TAB ENTRY                              *         
***********************************************************************         
         USING GCTBLD,R3                                                        
DISMOOF  NTR1  BASE=*,LABEL=*                                                   
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)        R8=RE-ESTABLISH LOCAL WORKING STOR           
*                                                                               
         L     R4,ATSARREC         R2=A(TSAR RECORD)                            
         USING TSARRECD,R4                                                      
         LA    R4,TSARDATA                                                      
         USING TSARDATD,R4                                                      
*                                                                               
         XC    TEMP,TEMP                                                        
         MVI   TEMP,C' '                                                        
         SR    R1,R1                                                            
*----------------------------------------------------------------------         
* OFFICE/WORKCODE COLUMN                                                        
*----------------------------------------------------------------------         
         CLI   GCTCOID,GCOFF                 OFFICE COLUMN?                     
         BE    *+12                          . YES                              
         CLI   GCTCOID,GCWC                  WORKCODE COLUMN?                   
         BNE   DISM20                        . NO                               
*                                                                               
         CLI   SVACTNUM,ACTNOFMN             TYPE=FM?                           
         BNE   DISM10                        .NO                                
*----------------------                                                         
* TYPE OFFICE BY MONTH                                                          
*----------------------                                                         
         CLC   TSDOFF,ALLOFFI                ALL OFFICES?                       
         BNE   DISM4                         . NO                               
         CLC   TSDDATE,ALLMOSI               ALL MOS?                           
         BNE   DISM6                         . NO                               
         MVC   TEMP+1(L'LC@TOTAL),LC@TOTAL                                      
         LHI   R1,L'LC@TOTAL+1                                                  
         MVI   TEMP,C'T'                                                        
         B     DISMOOX                                                          
*                                                                               
DISM4    MVC   TEMP+1(L'TSDOFF),TSDOFF                                          
         LHI   R1,L'TSDOFF+1                                                    
         CLC   TSDDATE,ALLMOSI               ALL MOS?                           
         BE    *+14                          . YES                              
         CLC   TSDDATE,BFWMOSI               BBF MOS?                           
         BNE   DISMOOX                       . NO                               
         MVI   TEMP,C'S'                                                        
         B     DISMOOX                                                          
*                                                                               
DISM6    MVC   TEMP+1(L'MX@ALL),MX@ALL                                          
         LHI   R1,L'MX@ALL+1                                                    
         MVI   TEMP,C'S'                                                        
         B     DISMOOX                                                          
*----------------------                                                         
* TYPE MONTH BY OFFICE                                                          
*----------------------                                                         
DISM10   CLC   TSDOFF,ALLOFFI                                                   
         BNE   DISM14                                                           
         CLC   TSDDATE,ALLMOSI                                                  
         BE    DISMOOX                                                          
*                                                                               
         MVC   TEMP(L'MX@SUBT),MX@SUBT                                          
         LHI   R1,L'MX@SUBT                                                     
         B     DISMOOX                                                          
*                                                                               
DISM14   MVC   TEMP(L'TSDOFF),TSDOFF                                            
         LHI   R1,L'TSDOFF                                                      
         B     DISMOOX                                                          
*----------------------------------------------------------------------         
* MONTH COLUMN                                                                  
*----------------------------------------------------------------------         
DISM20   CLI   SVACTNUM,ACTNOFMN          TYPE=FM?                              
         BE    DISM40                     . YES                                 
*--------------------------                                                     
* MONTH OR MONTH BY OFFICE                                                      
*--------------------------                                                     
         CLC   TSDDATE,BFWMOSI                                                  
         BE    DISM24                                                           
         CLC   TSDDATE,ALLMOSI     ALL MOS?                                     
         BE    DISM30              YES                                          
         B     DISM50                                                           
*                                                                               
DISM24   MVI   TEMP,C'S'                                                        
         MVC   TEMP+1(L'MX@BALBF),MX@BALBF                                      
         LHI   R1,L'MX@BALBF+1                                                  
         B     DISMOOX                                                          
*                                                                               
DISM30   CLI   SVACTNUM,ACTNMNTH   IS IT JUST MONTH?                            
         BE    DISM34                                                           
         CLC   TSDOFF,ALLOFFI                                                   
         BE    DISM34                                                           
         MVI   TEMP,C'S'                                                        
         MVC   TEMP+1(L'MX@ALL),MX@ALL                                          
         LHI   R1,L'MX@ALL+1                                                    
         B     DISMOOX                                                          
DISM34   MVI   TEMP,C'T'                                                        
         MVC   TEMP+1(L'LC@TOTAL),LC@TOTAL                                      
         LHI   R1,L'LC@TOTAL+1                                                  
         B     DISMOOX                                                          
*-----------------                                                              
* OFFICE BY MONTH                                                               
*-----------------                                                              
DISM40   CLC   TSDDATE,BFWMOSI                                                  
         BE    DISM44                                                           
         CLC   TSDDATE,ALLMOSI     ALL MOS?                                     
         BE    DISM46              YES                                          
         B     DISM49                                                           
*                                                                               
DISM44   MVC   TEMP(L'MX@BALBF),MX@BALBF                                        
         LHI   R1,L'MX@BALBF                                                    
         B     DISMOOX                                                          
*                                                                               
DISM46   CLC   TSDOFF,ALLOFFI                                                   
         BE    DISMOOX                                                          
         MVC   TEMP(L'MX@SUBT),MX@SUBT                                          
         LHI   R1,L'MX@SUBT                                                     
         B     DISMOOX                                                          
*                                                                               
DISM49   LA    R5,TEMP                                                          
         B     *+8                                                              
DISM50   LA    R5,TEMP+1           LEAVE A SPACE FOR ROW DESCRIPTION            
*                                                                               
         OC    TSDDATE2,TSDDATE2   DATE RANGE?                                  
         BZ    DISM55              NO                                           
         MVC   0(L'TSDDATE2,R5),TSDDATE2                                        
         MVI   2(R5),X'01'                                                      
         GOTO1 VDATCON,DMCB,(1,(R5)),(9,(R5)) START MONTH                       
         LA    R5,L'SCR1DATE-1(R5)                                              
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   2(R5),C'-'                                                       
         LA    R5,4(R5)                                                         
DISM55   MVC   0(L'TSDDATE,R5),TSDDATE                                          
         MVI   2(R5),X'01'                                                      
         GOTO1 VDATCON,DMCB,(1,(R5)),(9,(R5))  END/SINGLE MONTH                 
*                                                                               
         LA    R5,L'SCR1DATE-1(R5)         BUMP BACK TO GET LENGTH              
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         AHI   R5,1                                                             
         LR    R1,R5                                                            
         LA    R5,TEMP                                                          
         SR    R1,R5                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   SVACTNUM,ACTNMNTH        JUST MONTH?                             
         BE    DISMOOX                  . YES                                   
         CLI   SVACTNUM,ACTNOFMN        OFFICE BY MONTH?                        
         BE    DISMOOX                  . YES                                   
         CLC   TSDOFF,ALLOFFI           OFFICE=ALL?                             
         BNE   DISMOOX                  . NO                                    
         MVI   TEMP,C'S'                SUB TOTAL LINE                          
*                                                                               
DISMOOX  J     XIT1                                                             
*                                                                               
BFWMOSI  DC    X'0162'             BROUGHT FORWARD MOS                          
ALLMOSI  DC    X'FFFF'             ALL MOS                                      
ALLOFFI  DC    X'FFFF'             ALL OFFICE                                   
         LTORG                                                                  
*                                                                               
         DROP  R3,R4,RB                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DISPLAY DIFFERENCE COLUMN FOR GRIDS                          *         
*         R3 = ADDRESS OF GRID TAB ENTRY                                        
***********************************************************************         
         USING GCTBLD,R3                                                        
DISDIFF  NTR1  BASE=*,LABEL=*                                                   
         LHI   R8,OVERWORK-WORKD                                                
         LA    R8,WORKD(R8)        R8=RE-ESTABLISH LOCAL WORKING STOR           
*                                                                               
         L     R4,ATSARREC         R2=A(TSAR RECORD)                            
         USING TSARRECD,R4                                                      
         LA    R4,TSARDATA                                                      
         USING TSARDATD,R4                                                      
*                                                                               
         XC    TEMP,TEMP                                                        
*                                                                               
         ZAP   BALANCE,TSDDR                    DEBITS FOR MONTH                
         SP    BALANCE,TSDCR                    CREDITS FOR MONTH               
         CURED BALANCE,(20,TEMP),2,FLOAT=-,COMMAS=YES,ALIGN=LEFT                
         LR    R1,R0                                                            
         J     XIT1                                                             
*                                                                               
         LTORG                                                                  
         DROP  R3,R4,RB                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        MO    GRID COLUMN TABLE - COVERED BY GCTBLD                  *         
***********************************************************************         
GCTBLMO  DS    0F                                                               
*                                                                               
GCMOM    DC    AL1(GMOMLQ,GCMON,L'LC@MOS,0)          MONTH                      
         DC    AL2(LC@MOS-WORKD,ADISMOOF-OVERWRKD)                              
         DC    AL1(GCTINOTO+GCTIROUT,GCTIMONO,GCTFDAT+GCTFLEFT,0)               
         DC    AL1(0,0,0,0)                                                     
GMOMLQ   EQU   *-GCMOM                                                          
*                                                                               
GCMOB    DC    AL1(GMOBLQ,GCDRS,L'LC@DRS,L'TSDDR)    DEBITS                     
         DC    AL2(LC@DRS-WORKD,TSDDR-TSARDATD)                                 
         DC    AL1(0,0,GCTFNUM+GCTFRGHT,0)                                      
         DC    AL1(0,0,0,0)                                                     
GMOBLQ   EQU   *-GCMOB                                                          
*                                                                               
GCMOC    DC    AL1(GMOCLQ,GCCRS,L'LC@CRS,L'TSDCR)    CREDITS                    
         DC    AL2(LC@CRS-WORKD,TSDCR-TSARDATD)                                 
         DC    AL1(0,0,GCTFNUM+GCTFRGHT,0)                                      
         DC    AL1(0,0,0,0)                                                     
GMOCLQ   EQU   *-GCMOC                                                          
*                                                                               
GCMOF    DC    AL1(GMOFLQ,GCDIF,L'MX@DFRNC,0)        DIFFERENCE                 
         DC    AL2(MX@DFRNC-OVERWRKD,ADISDIFF-OVERWRKD)                         
         DC    AL1(GCTIROUT+GCTIOVER,0,GCTFNUM+GCTFRGHT,0)                      
         DC    AL1(0,0),AL2(0)                                                  
GMOFLQ   EQU   *-GCMOF                                                          
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        MF       GRID COLUMN TABLE - COVERED BY GCTBLD               *         
***********************************************************************         
GCTBLMF  DS    0F                                                               
*                                                                               
GCMFM    DC    AL1(GMFMLQ,GCMON,L'LC@MOS,0)          MONTH                      
         DC    AL2(LC@MOS-WORKD,ADISMOOF-OVERWRKD)                              
         DC    AL1(GCTINOTO+GCTIROUT,GCTIMONO,GCTFDAT+GCTFLEFT,0)               
         DC    AL1(0,0,0,0)                                                     
GMFMLQ   EQU   *-GCMFM                                                          
*                                                                               
GCMFO    DC    AL1(GMFOLQ,GCOFF,L'LC@OFFC,0)         OFFICE                     
         DC    AL2(LC@OFFC-WORKD,ADISMOOF-OVERWRKD)                             
         DC    AL1(GCTIROUT+GCTIXLDG,0,0,0)                                     
         DC    AL1(0,0,0,0)                                                     
         DC    CL2'SJ'                                                          
GMFOLQ   EQU   *-GCMFO                                                          
*                                                                               
GCMFW    DC    AL1(GMFWLQ,GCWC,L'LC@WC2,0)           W/C                        
         DC    AL2(LC@WC2-WORKD,ADISMOOF-OVERWRKD)                              
         DC    AL1(GCTIROUT,0,0,0)                                              
         DC    AL1(0,0,0,0)                                                     
         DC    CL2'SJ'                                                          
GMFWLQ   EQU   *-GCMFW                                                          
*                                                                               
GCMFB    DC    AL1(GMFBLQ,GCDRS,L'LC@DRS,L'TSDDR)    DEBITS                     
         DC    AL2(LC@DRS-WORKD,TSDDR-TSARDATD)                                 
         DC    AL1(0,0,GCTFNUM+GCTFRGHT,0)                                      
         DC    AL1(0,0,0,0)                                                     
GMFBLQ   EQU   *-GCMFB                                                          
*                                                                               
GCMFC    DC    AL1(GMFCLQ,GCCRS,L'LC@CRS,L'TSDCR)    CREDITS                    
         DC    AL2(LC@CRS-WORKD,TSDCR-TSARDATD)                                 
         DC    AL1(0,0,GCTFNUM+GCTFRGHT,0)                                      
         DC    AL1(0,0,0,0)                                                     
GMFCLQ   EQU   *-GCMFC                                                          
*                                                                               
GCMFI    DC    AL1(GMFILQ,GCDIF,L'MX@DFRNC,0)       DIFFERENCE                  
         DC    AL2(MX@DFRNC-OVERWRKD,ADISDIFF-OVERWRKD)                         
         DC    AL1(GCTIROUT+GCTIOVER,0,GCTFNUM+GCTFRGHT,0)                      
         DC    AL1(0,0),AL2(0)                                                  
GMFILQ   EQU   *-GCMFI                                                          
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
*        FM       GRID COLUMN TABLE - COVERED BY GCTBLD               *         
***********************************************************************         
GCTBLFM  DS    0F                                                               
*                                                                               
GCFMO    DC    AL1(GFMOLQ,GCOFF,L'LC@OFFC,0)         OFFICE                     
         DC    AL2(LC@OFFC-WORKD,ADISMOOF-OVERWRKD)                             
         DC    AL1(GCTINOTO+GCTIROUT+GCTIXLDG,0,0,0)                            
         DC    AL1(0,0,0,0)                                                     
         DC    CL2'SJ'                                                          
GFMOLQ   EQU   *-GCFMO                                                          
*                                                                               
GCFMW    DC    AL1(GFMWLQ,GCWC,L'LC@WC2,0)           W/C                        
         DC    AL2(LC@WC2-WORKD,ADISMOOF-OVERWRKD)                              
         DC    AL1(GCTINOTO+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0,0,0)                                                     
         DC    CL2'SJ'                                                          
GFMWLQ   EQU   *-GCFMW                                                          
*                                                                               
GCFMM    DC    AL1(GFMMLQ,GCMON,L'LC@MOS,0)          MONTH                      
         DC    AL2(LC@MOS-WORKD,ADISMOOF-OVERWRKD)                              
         DC    AL1(GCTINOTO+GCTIROUT,GCTIMONO,GCTFDAT+GCTFLEFT,0)               
         DC    AL1(0,0,0,0)                                                     
GFMMLQ   EQU   *-GCFMM                                                          
*                                                                               
GCFMB    DC    AL1(GFMBLQ,GCDRS,L'LC@DRS,L'TSDDR)    DEBITS                     
         DC    AL2(LC@DRS-WORKD,TSDDR-TSARDATD)                                 
         DC    AL1(0,0,GCTFNUM+GCTFRGHT,0)                                      
         DC    AL1(0,0,0,0)                                                     
GFMBLQ   EQU   *-GCFMB                                                          
*                                                                               
GCFMC    DC    AL1(GFMCLQ,GCCRS,L'LC@CRS,L'TSDCR)    CREDITS                    
         DC    AL2(LC@CRS-WORKD,TSDCR-TSARDATD)                                 
         DC    AL1(0,0,GCTFNUM+GCTFRGHT,0)                                      
         DC    AL1(0,0,0,0)                                                     
GFMCLQ   EQU   *-GCFMC                                                          
*                                                                               
GCFMF    DC    AL1(GFMFLQ,GCDIF,L'MX@DFRNC,0)        DIFFERENCE                 
         DC    AL2(MX@DFRNC-OVERWRKD,ADISDIFF-OVERWRKD)                         
         DC    AL1(GCTIROUT+GCTIOVER,0,GCTFNUM+GCTFRGHT,0)                      
         DC    AL1(0,0),AL2(0)                                                  
GFMFLQ   EQU   *-GCFMF                                                          
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
* GRID COLUMN EQUATES                                                           
*----------------------------------------------------------------------         
GCMON    EQU   1                         MONTH                                  
GCOFF    EQU   2                         OFFICE                                 
GCWC     EQU   3                         WORKCODE                               
GCDRS    EQU   4                         DEBITS                                 
GCCRS    EQU   5                         CREDITS                                
GCDIF    EQU   6                         DIFFERENCE                             
*                                                                               
**********************************************************************          
* WORKING STORAGE                                                               
**********************************************************************          
OVERWRKD DSECT                                                                  
AGCTBL   DS    A                   ADDRESS OF GRID COLUMN TABLE                 
ADISDIFF DS    A                   GRID DISPLAY DIFFERENCE ROUTINE              
ADISMOOF DS    A                   DISPLAY MONTH AND OFFICE INFO                
PBSTATUS DS    XL1                 STATUS BYTE                                  
PBMOS    DS    PL(L'TRNMOS)        TRANSACTION MOS                              
PBDR     DS    PL8                 TRANSACTION 'BUCKET' DEBIT                   
PBCR     DS    PL8                 TRANSACTION 'BUCKET' CREDIT                  
PDATE    DS    PL(L'BUKMOS)        DATE                                         
PDEBITS  DS    PL8                 DEBITS                                       
PCREDITS DS    PL8                 CREDITS                                      
BALANCE  DS    PL8                 BALANCE (BUDGET-ACTUALS)                     
*                                                                               
STAR     DS    C                                                                
*                                                                               
JOBSTAT  DS    XL(L'JOBSTA1)       JOB STATUS                                   
OFFICE   DS    CL(L'CACKOFF)       OFFICE                                       
         EJECT                                                                  
*                                                                               
**********************************************************************          
* DATA DICTIONARY ENTRIES                                                       
**********************************************************************          
DSMIX    DS    0C                                                               
MX@ENH13 DS    CL78                MONTH HEADINGS                               
MX@ENH32 DS    CL78                MONTH BY OFFICE HEADINGS                     
MX@ENH33 DS    CL78                OFFICE BY MONTH HEADINGS                     
MX@ACC   DS    CL9                                                              
MX@CTR   DS    CL9                                                              
MX@TOTAL DS    CL9                                                              
MX@BFW   DS    CL3                                                              
MX@BALBF DS    CL15                                                             
MX@BALCF DS    CL15                                                             
MX@XJOB  DS    CL11                                                             
MX@MOA   DS    CL3                                                              
MX@ALL   DS    CL3                ALL                                           
MX@BAL11 DS    CL11               BALANCE B/F                                   
MX@DFRNC DS    CL10               DIFFERENCE                                    
MX@SUBT  DS    CL8                SUBTOTAL                                      
         EJECT                                                                  
*                                                                               
**********************************************************************          
* COVER SCREEN                                                                  
**********************************************************************          
SCRLIN1D DSECT                     COVER SCREEN ITEM LINE1                      
SCR1DATE DS    CL14                MONTH                                        
SCR1SPA  DS    CL2                                                              
SCR1DR   DS    CL16                DEBITS FOR MONTH                             
         DS    CL7                                                              
         ORG   SCR1SPA             MONTH BY OFFICE FORMAT                       
SCR1MFOF DS    CL2                                                              
         DS    CL6                                                              
SCR1MFDR DS    CL16                                                             
         ORG   SCR1DATE                                                         
SCR1FMOF DS    CL2                                                              
         DS    CL5                                                              
SCR1FMDT DS    CL14                                                             
         DS    CL1                                                              
SCR1FMDR DS    CL16                                                             
         DS    CL1                                                              
         ORG                                                                    
SCR1CR   DS    CL16                CREDITS FOR MONTH                            
         DS    CL1                                                              
SCR1BAL  DS    CL22                CREDITS FOR MONTH                            
         EJECT                                                                  
*                                                                               
**********************************************************************          
* TSAR DATA ITEM DSECT                                                          
**********************************************************************          
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSITEM1  EQU   1                   ITEM 1 FORMAT                                
TSDDATE  DS    CL(L'BUKMOS)        MONTH                                        
TSDDATE2 DS    CL(L'BUKMOS)        MONTH PERIOD (PRIOR BUCKET RANGE)            
TSDOFF   DS    CL(L'CACKOFF)       OFFICE USED FOR MF AND FM ACTIONS            
TSDCR    DS    PL8                 CREDITS FOR MONTH                            
TSDDR    DS    PL8                 DEBITS FOR MONTH                             
TSDCFDR  DS    PL8                 DEBITS CARRIED FORWARD                       
TSDCFCR  DS    PL8                 CREDITS CARRIED FORWARD                      
TSARDLNQ EQU   *-TSARDATD                                                       
         EJECT                                                                  
*                                                                               
**********************************************************************          
* TSAR TOTAL                                                                    
**********************************************************************          
TSARTOTD DSECT                     TSAR DATA ITEM                               
TSTLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSTFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSTTOT   EQU   2                   TOTAL LINE                                   
TSTDATE  DS    CL(L'BUKMOS)        MONTH                                        
TSTDATE2 DS    CL(L'BUKMOS)        MONTH PERIOD (PRIOR BUCKET RANGE)            
TSTOFF   DS    CL(L'CACKOFF)       OFFICE USED FOR MF AND FM ACTIONS            
TSTCR    DS    PL8                 CREDITS FOR MONTH                            
TSTDR    DS    PL8                 DEBITS FOR MONTH                             
TSTCFDR  DS    PL8                 DEBITS CARRIED FORWARD                       
TSTCFCR  DS    PL8                 CREDITS CARRIED FORWARD                      
TSTLNQ   EQU   *-TSARTOTD                                                       
         EJECT                                                                  
*                                                                               
**********************************************************************          
* MONTH TABLE DSECT                                                             
**********************************************************************          
MTHTABD  DSECT                     MONTH TABLE                                  
MTHDATE1 DS    CL(L'BUKMOS)        MONTH                                        
MTHOFF1  DS    CL(L'CACKOFF)       OFFICE (2 CHAR OFFICES ONLY)                 
         ORG   MTHDATE1                                                         
MTHOFF2  DS    CL(L'CACKOFF)       OFFICE (2 CHAR OFFICES ONLY)                 
MTHDATE2 DS    CL(L'BUKMOS)        MONTH                                        
         ORG                                                                    
MTHKLNQ  EQU   *-MTHTABD           KEY LENGTH                                   
MTHSTAT  DS    XL1                 STATUS BYTE                                  
MTHPREVQ EQU   X'80'               PRIOR BUCKET ENTRY                           
MTHDR    DS    PL8                 DEBITS FOR MONTH                             
MTHCR    DS    PL8                 CREDITS FOR MONTH                            
MTHLNQ   EQU   *-MTHTABD                                                        
         EJECT                                                                  
**********************************************************************          
       ++INCLUDE ACENQWORK                                                      
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
ORELO    DS    A                                                                
STARTMTH DS    PL2                 START MONTH                                  
ENDMTH   DS    PL2                 END MONTH                                    
*                                                                               
MOVALS   DS    0PL8                CREDITOR VALUES                              
DEBTOT   DS    PL8                 TOTAL DEBITS                                 
CRETOT   DS    PL8                 TOTAL CREDITS                                
*                                                                               
DETFLAG  DS    X                                                                
DETGRINQ EQU   X'80'               SCREEN INITIALIZED FOR GRIDS                 
DETDONE  EQU   X'40'               NO MORE TSAR RECORDS                         
*                                                                               
STATFLAG DS    X                                                                
ROFFBUKQ EQU   X'80'               READ OFFICE BUCKETS                          
SHOWAST  EQU   X'20'               BBF TAKEN FROM PEELED AMOUNT                 
*                                                                               
MOVALLNQ EQU   *-MOVALS                                                         
*                                                                               
CLOSEDAT DS    XL(L'RSTBDATE)      LAST CLOSED MOS                              
*                                                                               
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACENQ03   06/11/18'                                      
         END                                                                    
