*          DATA SET ACENQ0F    AT LEVEL 008 AS OF 05/15/08                      
*PHASE T6200FA                                                                  
T6200F   TITLE 'ENQUIRY- BUDGETS'                                               
T6200F   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         NMOD1 0,**ENQF**,R7,CLEAR=YES,RR=RE                                    
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         LH    R8,=Y(OVERWORK-WORKD)                                            
         LA    R8,WORKD(R8)        R8=A(LOCAL WORKING STORAGE)                  
         USING OVERWRKD,R8                                                      
         ST    RE,ORELO                                                         
                                                                                
         L     RF,=A(GCTBL)                                                     
         AR    RF,RE                                                            
         ST    RF,AGCTBL                                                        
                                                                                
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
                                                                                
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN10              NO                                           
         BAS   RE,FSTDIS           YES PERFORM FIRST DISPLAY FUNCTIONS          
         BNE   ERXIT                                                            
         TM    DISPFLAG,NORECQ     NO RECORDS ON JOB?                           
         BO    MAINX                                                            
         TM    DISPFLAG,DISIOMAX   MAX IO'S?                                    
         BO    MAINX                                                            
         OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
         B     MAIN20                                                           
                                                                                
MAIN10   TM    OVRSTAT,OVRGDONE    GRIDS PROCESSING FINISHED?                   
         BO    MAINXGX                                                          
         TM    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY?                       
         BO    MAINX                                                            
         TM    DISPFLAG,ALLREADQ   ALL RECORDS READ?                            
         BO    MAIN20                                                           
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    MAIN20                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     MAINX                                                            
                                                                                
MAIN20   BAS   RE,TSARNOW          READ ALL RECORDS UP FRONT                    
         BNE   ERXIT                                                            
         TM    DISPFLAG,DISIOMAX   MAX IO'S?                                    
         BO    MAINX                                                            
         TM    DISPFLAG,NORECQ                                                  
         BO    MAINX                                                            
                                                                                
MAIN30   CLC   TSCURRNO,TSLSTREC   HAVE WE ALLREADY GOT RECORD IN TSAR?         
         BNH   MAIN40                                                           
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BNO   MAINX                                                            
         LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATOOMNY)                                           
         B     ERXIT                                                            
                                                                                
MAIN40   GOTO1 ATSARGET,TSCURRNO   GET TSAR RECORD                              
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    *+12                                                             
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
         B     *+8                                                              
         BAS   RE,FGRMTSAR         FORMAT TSAR INTO GRID FORMAT                 
                                                                                
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   MAINX               SCREEN IS FULL                               
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUM               
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     MAIN30                                                           
                                                                                
MAINX    B     OKXIT                                                            
                                                                                
MAINXGX  GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),BASKEY                            
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    OKXIT                                                            
         DC    H'0'                                                             
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
FSTDIS   NTR1                                                                   
         XC    TEMPBLKN,TEMPBLKN   NUMBER OF ENTRIES IN BLOCK                   
         ZAP   ACTTOT,=P'0'        ACTUALS TOTAL                                
         ZAP   BUDTOT,=P'0'        BUDGET TOTAL                                 
         XC    BUDNUM,BUDNUM       BUDGET TYPE NUMBER                           
         MVC   CONTRA,SPACES       CONTRA ACOUNT                                
         MVI   CONTLEN,0           LENGTH OF INPUT CONTRA CODE                  
         MVI   NEGCONT,0           NEGATIVE FILTER                              
         MVC   SVCACN,SPACES       CONTRA ACCOUNT NAME                          
                                                                                
         LA    R2,BASCACH          R2=A(CONTRA ACCOUNT FIELD)                   
         USING FLDHDRD,R2                                                       
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        ANYTHING INPUT FOR CONTRA FILTER?            
         BZ    FSTD10                                                           
         LA    RE,FLDDATA                                                       
         CLI   0(RE),NEGFILTR    NEGATIVE FILTER?                               
         BNE   FSTD05                                                           
         MVI   NEGCONT,NEGFILTR                                                 
         LA    RE,1(RE)                                                         
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         SH    RF,=H'1'                                                         
         BZ    FSTDERR             NO ACCOUNT ENTERED?                          
                                                                                
FSTD05   STC   RF,CONTLEN          LENGTH OF CONTRA CODE INPUT                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONTRA(0),0(RE)     GET CONTRA ACCOUNT CODE                      
         GOTO1 ACNAME              ATTEMPT TO FIND CONTRA NAME                  
                                                                                
FSTD10   OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
         GOTO1 VACSRCHC,DMCB,(4,BASKEYH),TWAD,0,ACOMFACS,(X'10',0)              
         LA    R2,BASKEYH          R2=A(KEY FIELD)                              
         USING FLDHDRD,R2                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         CLI   FLDILEN,L'ACTKULA   ENSURE LENGTH NOT TOO LONG                   
         BH    FSTDERR                                                          
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         SR    R4,R4                                                            
         ICM   R4,1,FLDILEN        R4=L'(KEY FIELD INPUT)                       
         BZ    FSTDERR                                                          
         BCTR  R4,0                                                             
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
         CLC   FLDDATA(L'SPROUNIT+L'SPROLEDG),SPROUNIT                          
         BE    FSTDERR                                                          
         GOTO1 AUNITLDG,FLDDATA    READ UNIT/LEDGER RECORDS                     
         BNE   FSTDERR                                                          
         TM    DISPFLAG,DISIOMAX   MAX IO'S                                     
         BO    FSTDX                                                            
         LA    R3,IOKEY            R3=A(KEY FOR LOW LEVEL ACCOUNT)              
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,MYCO                                                     
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ACTKULA(0),FLDDATA                                               
         MVC   FVMSGNO,=AL2(EAIFNTFN)                                           
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    FSTD20                                                           
         TM    IOERR,IOMAX                                                      
         BNO   *+12                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     FSTDX                                                            
         TM    IOERR,IOERNF                                                     
         BO    FSTDERR             LOW LEVEL ACCOUNT NOT FOUND                  
         DC    H'0'                                                             
                                                                                
FSTD20   MVC   FVMSGNO,=AL2(EASECLOC)                                           
         GOTO1 AOFFACC                                                          
         TM    DISPFLAG,DISIOMAX                                                
         BO    FSTDX                                                            
         CLI   OFFLFLAG,OFFLSEC                                                 
         BE    FSTDERR                                                          
         OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
                                                                                
         LA    R2,GRDDAT1H                                                      
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BO    FSTD22                                                           
                                                                                
         LA    R2,ENQDAT1H                                                      
         GOTO1 ADISUL              DISPLAY UNIT AND LEDGER NAMES                
         LA    R2,ENQDAT1H+ENQDAT2H-ENQDAT1H                                    
                                                                                
FSTD22   L     R3,AIO1                                                          
         LA    RF,ACTRFST                                                       
FSTD30   CLI   0(RF),EOR           END OF RECORD?                               
         BE    FSTD50                                                           
         CLI   0(RF),NAMELQ        NAME ELEMENT?                                
         BE    FSDT40                                                           
                                                                                
         SR    R0,R0               BUMP RF TO NEXT ELEMENT                      
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     FSTD30                                                           
                                                                                
         USING NAMELD,RF                                                        
FSDT40   SR    RE,RE               GET ACCOUNT NAME                             
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ACCNAME(0),NAMEREC                                               
         DROP  RF                                                               
                                                                                
FSTD50   BAS   RE,BUDFST           GET FIRST BUDGET RECORD KEY                  
                                                                                
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BZ    FSTD55                                                           
         GOTO1 ADISACC                                                          
         B     FSTD85                                                           
                                                                                
FSTD55   OI    FLDOIND,FOUTTRN                                                  
         LA    R0,UNSCANBK         SET UNSCAN BLOCK TO SPACES                   
         LH    R1,=Y(MAXUNSCN*UNSCNLNQ)                                         
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         LA    R4,UNSCANBK         R4=A(UNSCAN BLOCK)                           
         USING MYUNSCND,R4         USE NON-STANDARD VALUE LENGTHS               
         MVC   MYUNSCLH(L'MX@ACC),MX@ACC                                        
         MVC   MYUNSCRH,ACCNAME    ACCOUNT NAME                                 
         LA    R4,L'MYUNSCLH+L'MYUNSCRH(R4)                                     
         CLC   SVCACN,SPACES       CONTRA NAME?                                 
         BE    FSDT60                                                           
         MVC   MYUNSCLH(L'MX@CTR),MX@CTR                                        
         MVC   MYUNSCRH,SVCACN                                                  
         LA    R4,L'MYUNSCLH+L'MYUNSCRH(R4)                                     
FSDT60   MVC   MYUNSCLH(L'MX@TYPE),MX@TYPE                                      
         MVC   MYUNSCRH(L'BUDCODE),BUDCODE  BUDGET TYPE                         
                                                                                
         LA    R4,UNSCANBK         R4=A(UNSCAN BLOCK)                           
         LA    RF,FLDDATA          RF=A(SCREEN FIELD)                           
         CLC   MYUNSCLH,SPACES     NOTHING IN UNSCAN BLOCK?                     
         BE    FSDT80                                                           
FSDT70   LA    RE,FLDDATA+L'ENQDAT1-1                                           
         SR    RE,RF               SCREEN LINE FULL?                            
         BM    FSDT80                                                           
         CH    RE,=Y(L'MYUNSCLH-1) CAN WE FIT KEYWORD ON?                       
         BNH   FSDT80                                                           
         MVC   0(L'MYUNSCLH,RF),MYUNSCLH GET KEYWORD FROM LHS                   
         LA    RF,L'MYUNSCLH-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
         LA    RE,FLDDATA+L'ENQDAT1-1                                           
         SR    RE,RF               SCREEN LINE FULL?                            
         BM    FSDT80                                                           
         CH    RE,=Y(L'MYUNSCRH-1)                                              
         BNH   *+8                                                              
         LA    RE,L'MYUNSCRH-1                                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),MYUNSCRH    DISPLAY AS MUCH OF VALUE AS POSSIBLE         
         LA    RF,0(RE,RF)                                                      
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RE,FLDDATA+L'ENQDAT1-4                                           
         CR    RF,RE               ANY FURTHER ROOM?                            
         BNL   FSDT80                                                           
         LA    R4,L'MYUNSCLH+L'MYUNSCRH(R4) BUMP UP SCAN BLOCK                  
         CLC   MYUNSCLH,SPACES     ANYTHING TO DISPLAY?                         
         BE    FSDT80                                                           
         MVI   2(RF),C'/'          SEPERATOR                                    
         LA    RF,4(RF)            RF=A(NEXT POSITION IN SCREEN LINE)           
         B     FSDT70                                                           
                                                                                
FSDT80   LA    R2,ENQDAT2H-ENQDAT1H(R2)  DISPLAY COLUMN HEADINGS                
         MVC   FLDDATA(L'MX@ENH28),MX@ENH28                                     
         MVC   FLDDATA(L'TMPH28),TMPH28                                         
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         MVC   FLDDATA(L'MX@ENH29),MX@ENH29                                     
         MVC   FLDDATA(L'TMPH29),TMPH29                                         
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
                                                                                
FSTD85   GOTO1 ASCRNDIM,DMCB,(0,(R2))                                           
                                                                                
FSTD90   L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
                                                                                
         MVC   KEYSAVE,SPACES      CLEAR SAVED KEY                              
FSTDX    CR    RB,RB                                                            
         B     *+6                                                              
FSTDERR  LTR   RB,RB                                                            
         B     XIT                                                              
TMPH28   DC    C'Contra Account Budg/Contra OF Start  End     Budge'            
TMPH29   DC    C'-------------- ----Name--- -- ----   ---     -----'            
         EJECT                                                                  
***********************************************************************         
* CHECK BUDGET TYPES AND READ FIRST BUDGET RECORD                     *         
* ON EXIT 'IOKEY' CONTAINS FIRST BUDGET RECORD KEY FROM ACCDIR        *         
*         'BUDCOD' HOLDS BUDGET TYPE CODE                             *         
*         'BUDNUM' HOLDS BUDGET TYPE NUMBER                           *         
***********************************************************************         
BUDFST   NTR1                                                                   
         LA    R3,IOKEY                                                         
         USING BUDRECD,R3                                                       
         XC    BUDKEY,BUDKEY       CLEAR KEY                                    
         MVI   BUDKTYP,BUDKTYPQ    BUDGET RECORD TYPE                           
         MVC   BUDKCPY,MYCO        COMPANY                                      
                                                                                
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         OC    OTYPE,OTYPE         HAS A BUDGET TYPE BEEN SPECIFIED?            
         BZ    BUDF10                                                           
         MVC   BUDKCOD,SPACES                                                   
         SR    R4,R4               MAKE SURE TYPE EXISTS                        
         IC    R4,OTYPLN                                                        
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   BUDKCOD(0),OTYPEVAL BUDGET TYPE                                  
         DROP  RF                                                               
                                                                                
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BE    *+14                                                             
         TM    IOERR,IOMAX                                                      
         BO    BUDF30                                                           
         DC    H'0'                                                             
                                                                                
         CLC   BUDKEY(BUDKNO2-BUDRECD),IOKEYSAV TYPE FOUND?                     
         BE    *+12                                                             
         OI    DISPFLAG,NORECQ                                                  
         B     BUDFX                                                            
                                                                                
         MVC   BUDCODE,BUDKCOD     SAVE THE BUDGET TYPE CODE                    
         MVC   BUDNUM,BUDKNO2      SAVE THE BUDGET TYPE NUMBER                  
         XC    BUDKEY,BUDKEY       CLEAR KEY                                    
         MVI   BUDKTYP,BUDKTYPQ                                                 
         MVC   BUDKCPY,MYCO                                                     
*                                  READ 1ST BUDGET RECORD FOR ACCOUNT           
BUDF10   MVC   BUDKUNT(L'BUDKCULA+L'BUDKWORK-1),SPACES                          
         SR    R4,R4                                                            
         IC    R4,BASKEYH+FLDILEN-FLDHDRD                                       
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   BUDKUNT(0),BASKEY                                                
                                                                                
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         OC    OOFFICVL,OOFFICVL   TEST OFFICE FILTER                           
         BNZ   BUDF15              YES, SKIP CODE TO ADD CONTRA                 
         DROP  RF                                                               
                                                                                
         CLI   CONTLEN,0           HAS A CONTRA FILTER BEEN SPECIFIED?          
         BE    BUDF15                                                           
         CLI   NEGCONT,NEGFILTR                                                 
         BE    BUDF15                                                           
         MVC   BUDKCUNT(L'BUDKCULA-1),CONTRA CONTRA CODE                        
         MVC   BUDKCCPY,MYCO       COMPANY                                      
                                                                                
BUDF15   MVC   BUDKBUDN,BUDNUM     BUDGET TYPE NUMBER                           
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BE    BUDF17                                                           
         TM    IOERR,IOMAX                                                      
         BO    BUDF30                                                           
         DC    H'0'                                                             
                                                                                
BUDF17   CLC   BUDKCPY(L'BUDKCULA),IOKEYSAV+L'BUDKTYP                           
         BNE   BUDF25                                                           
         BAS   RE,FILTR            FILTER OFFICE & CONTRA                       
         BE    BUDF26                                                           
                                                                                
BUDF22   GOTO1 AIO,IOSEQ+IOACCDIR+IO1                                           
         BE    BUDF17                                                           
         TM    IOERR,IOMAX                                                      
         BO    BUDF30                                                           
         DC    H'0'                                                             
                                                                                
BUDF25   OI    DISPFLAG,NORECQ                                                  
         B     BUDFX                                                            
                                                                                
BUDF26   OC    BUDNUM,BUDNUM       BUDGET TYPE ALREADY FOUND?                   
         BNZ   BUDFX                                                            
         MVC   BUDNUM,BUDKBUDN     GET BUDGET REC FOR BUDGET TYPE CODE          
         MVC   KEYSAVE,BUDKEY      SAVE 1ST BUDG REC FOR ACCOUNT KEY            
         XC    BUDKEY,BUDKEY                                                    
         MVC   BUDKEY(L'BUDKTYP+L'BUDKCPY),KEYSAVE                              
         MVC   BUDKNO1,BUDNUM                                                   
                                                                                
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BE    *+14                                                             
         TM    IOERR,IOMAX                                                      
         BO    BUDF30                                                           
         DC    H'0'                                                             
                                                                                
         CLC   BUDKEY(BUDKNO1-BUDKEY),IOKEYSAV BUDGET TYPE FOUND?               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUDCODE,BUDKCOD     SAVE BUDGET TYPE CODE                        
         MVC   IOKEY,KEYSAVE       RE-READ FIRST BUDGET FOR ACCOUNT             
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    BUDFX                                                            
         TM    IOERR,IOMAX                                                      
         BO    BUDF30                                                           
         DC    H'0'                                                             
                                                                                
BUDF30   OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
                                                                                
BUDFX    B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
*        READ ALL RECORDS UP FRONT                                    *         
* ON ENTRY 'IOKEY' CONTAINS KEY OF NEXT BUDGET OR CONTRA RECORD       *         
*          UNLESS ALL RECORDS HAVE BEEN READ                          *         
***********************************************************************         
TSARNOW  NTR1                                                                   
         TM    DISPFLAG,ALLREADQ   ALL RECORDS READ?                            
         BO    TSARN20                                                          
         LA    R3,IOKEY                                                         
         CLC   KEYSAVE,SPACES      RE-ENTER OR FIRST TIME FOR ACCOUNT?          
         BNE   TSARN10                                                          
         L     R4,ATEMPBLK                                                      
         MVI   0(R4),EOT           SET BLOCK TO EMPTY                           
         B     *+12                                                             
TSARN10  CLI   KEYSAVE,BUDKTYPQ    BUDGET RECORD?                               
         BNE   TSARN12                                                          
         BAS   RE,GETBUDG          GET ALL BUDGET RECORDS FOR ACCOUNT           
         BNE   TSARNERX            TOO MANY BUDGET RECS TO PROCESS              
TSARN12  TM    DISPFLAG,DISIOMAX                                                
         BO    TSARNX                                                           
                                                                                
         BAS   RE,GETACT           GET ACTUAL AMOUNTS FROM BUCKETS              
         TM    DISPFLAG,DISIOMAX                                                
         BO    TSARNX                                                           
                                                                                
TSARN20  MVC   KEYSAVE,SPACES                                                   
         OC    TEMPBLKN,TEMPBLKN   HAVE WE ANY TSAR RECORDS?                    
         BNZ   *+12                                                             
         OI    DISPFLAG,NORECQ     NO RECORDS FOUND FOR ACCOUNT                 
         B     TSARNX                                                           
         OI    DISPFLAG,ALLREADQ   ALL RECORDS READ                             
         DROP  R3                                                               
                                                                                
         SR    R5,R5                                                            
         ICM   R5,3,TEMPBLKN       R5=(NUM OF ENTRIES IN TEMPBLK)               
         CH    R5,TSLSTREC         ALL TSAR RECORDS BUILT?                      
         BNH   TSARNX                                                           
                                                                                
         L     R4,ATEMPBLK         R4=A(BUDGET V ACTUALS TABLE)                 
         USING BVATABD,R4                                                       
TSARN30  CLI   0(R4),EOT           END OF TABLE                                 
         BE    TSARN80                                                          
         CLC   BVAULACT,SPACES     NO CONTRA CODE?                              
         BE    TSARN70                                                          
         CLC   BVANAME,SPACES      NO NAME FOUND YET?                           
         BNE   TSARN60                                                          
         LA    R3,IOKEY            GET NAME FORM ACCOUNT                        
         USING ACTRECD,R3                                                       
         MVC   ACTKCPY,MYCO                                                     
         MVC   ACTKULA,BVAULACT                                                 
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    TSARN35                                                          
         TM    IOERR,IOMAX                                                      
         BO    TSARN90                                                          
         B     TSARN70                                                          
TSARN35  L     R3,AIO1             R3=A(ACCOUNT RECORD OF CONTRA)               
         LA    R3,ACTRFST                                                       
TSARN40  CLI   0(R3),EOR           END OF RECORD?                               
         BE    TSARN60                                                          
         CLI   0(R3),NAMELQ        NAME ELEMENT?                                
         BE    TSARN50                                                          
                                                                                
         SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     TSARN40                                                          
                                                                                
         USING NAMELD,R3           NAME ELEMENT                                 
TSARN50  SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BVANAME(0),NAMEREC  GET NAME                                     
         DROP  R3                                                               
                                                                                
TSARN60  CLC   UL1C,BASKEY         ACCOUNT ON 1C LEDGER?                        
         BNE   TSARN70                                                          
         CLC   UL1P,BVAULACT       CONTRA ON 1P LEDGER?                         
         BNE   TSARN70                                                          
         LA    RF,BVAULACT+L'BVAULACT-1 SUBSTITUTE 13N FOR 1PXXN                
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   BVALDG,C'3'                                                      
         MVC   BVAACT,0(RF)                                                     
         MVC   BVAACT+1(L'BVAACT-1),SPACES                                      
TSARN70  LA    R4,BVALNQ(R4)                                                    
         B     TSARN30                                                          
                                                                                
TSARN80  BAS   RE,BLDTSDAT                                                      
         BE    *+12                                                             
         TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BO    TSARNX                                                           
                                                                                
         BAS   RE,TOTAL                                                         
                                                                                
         MVC   TSCURRNO,=H'1'      SET CURRENT TSAR TO FIRST ONE                
         B     TSARNX                                                           
                                                                                
TSARN90  OI    DISPFLAG,DISIOMAX                                                
         MVC   KEYSAVE,IOKEY                                                    
                                                                                
TSARNX   J     OKXIT                                                            
TSARNERX J     ERXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        GET ALL BUDGET RECORDS FOR ACCOUNT                           *         
* ON ENTRY 'IOKEY' CONTAINS NEXT BUDGET RECORD KEY                    *         
***********************************************************************         
GETBUDG  NTR1                                                                   
         USING BUDRECD,R3                                                       
         LA    R3,IOKEY                                                         
GETB10   CLC   BUDKBUDN,BUDNUM     MATCH BUDGET TYPE?                           
         BNE   GETB120                                                          
         BAS   RE,FILTR            OFFICE & CONTRA FILTER                       
         BNE   GETB120                                                          
                                                                                
GETB20   GOTO1 AIO,IOGET+IOACCMST+IO1 GET THE BUDGET RECORD                     
         BE    *+14                                                             
         TM    IOERR,IOMAX           MAX IOS REACHED?                           
         BO    GETB140                                                          
         DC    H'0'                                                             
                                                                                
         L     R3,AIO1             R3=A(BUDGET RECORD)                          
         MVC   BUDNAME,SPACES      CLEAR BUDGET NAME                            
         XC    BUDSTART,BUDSTART   START DATE                                   
         XC    BUDENDMN,BUDENDMN   END DATE                                     
         ZAP   BUDAMT,=P'0'        BUDGET AMOUNT                                
         LA    R4,BUDRFST          R4=A(FIRST ELEMENT ON BUDGET RECORD)         
GETB30   CLI   0(R4),EOR           END OF RECORD?                               
         BE    GETB90                                                           
         CLI   0(R4),BAMELQ        BUDGET AMOUNT ELEMENT?                       
         BE    GETB50                                                           
         CLI   0(R4),NAMELQ        NAME ELEMENT?                                
         BE    GETB80                                                           
                                                                                
GETB40   SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     GETB30                                                           
                                                                                
         USING BAMELD,R4           BUDGET AMOUNT ELEMENT                        
GETB50   L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         OC    OMOS,OMOS           MOS FILTER?                                  
         BZ    GETB60                                                           
         GOTO1 ADCOMP,DMCB,(L'BAMMNTH,BAMMNTH),OMOSST,OMOSEN,OMOSFI             
         BNE   GETB40                                                           
         DROP  RF                                                               
                                                                                
GETB60   OC    BUDSTART,BUDSTART                                                
         BNZ   GETB70                                                           
         MVC   BUDSTART,BAMMNTH    START DATE                                   
GETB70   MVC   BUDENDMN,BAMMNTH    END DATE                                     
         AP    BUDAMT,BAMBUDG      BUDGET AMOUNT                                
         B     GETB40                                                           
         DROP  R4                                                               
                                                                                
         USING NAMELD,R4           NAME ELEMENT                                 
GETB80   SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         BM    GETB40                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BUDNAME(0),NAMEREC  BUDGET NAME                                  
         B     GETB40                                                           
         DROP  R4                                                               
                                                                                
GETB90   OC    BUDSTART,BUDSTART   ANY BUDGET INFO FOUND?                       
         BZ    GETB120                                                          
         AP    BUDTOT,BUDAMT       UPDATE BUDGET TOTAL                          
                                                                                
         LA    R4,WORK                                                          
         USING BVATABD,R4                                                       
         MVC   BVATABD(BVALNQ),SPACES                                           
                                                                                
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         CLI   OOFFDET,C'Y'        OD=Y OFFICE DETAIL                           
         BNE   *+10                                                             
         MVC   BVOFFC,BUDKWORK     OFFICE                                       
         DROP  RF                                                               
                                                                                
         MVC   BVAULACT,BUDKCUNT   BUDGET CONTRA ACCOUNT                        
         LA    RF,L'BVAULACT       RF=L'(LONGEST ACCOUNT)                       
         LA    RE,BVAULACT(RF)                                                  
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-14                                                          
         STC   RF,BVAULALN         SAVE LENGTH OF ACCOUNT                       
         MVC   BVASTART,BUDSTART   BUDGET START DATE                            
         MVC   BVAEND,BUDENDMN     BUDGET END DATE                              
         ZAP   BVABAMT,BUDAMT      BUDGET AMOUNT                                
         ZAP   BVAAAMT,=P'0'       INITIALISE ACTUALS AMOUNT                    
         MVC   BVANAME,BUDNAME     BUDGET NAME                                  
                                                                                
         L     R4,ATEMPBLK         R4=A(BUDGET V ACTUAL TABLE 'BVATAB')         
         USING BVATABD,R4                                                       
                                                                                
         LA    RE,1                RE=(NUMBER OF ENTRIES IN BVATAB)             
GETB100  CLI   0(R4),EOT           END OF TABLE?                                
         BE    GETB110                                                          
         CLC   BVATABD(BVABAMT-BVATABD),WORK                                    
         BE    GETB105                                                          
         LA    RE,1(RE)                                                         
         LA    R4,BVALNQ(R4)                                                    
         B     GETB100                                                          
                                                                                
GETB105  AP    BVABAMT,WORK+(BVABAMT-BVATABD)(L'BVABAMT)                        
         B     GETB120                                                          
                                                                                
GETB110  CHI   RE,MAXBUDQ          MAXIMUM ENTRIES EXCEEDED?                    
         BH    GETBERX                                                          
         STCM  RE,3,TEMPBLKN       RE=(NUM OF ENTRIES IN TEMPBLK)               
         MVC   BVATABD(BVALNQ),WORK ADD NEW ENTRY                               
         DROP  R4                                                               
                                                                                
GETB120  GOTO1 AIO,IOSEQ+IOACCDIR+IO1 GET NEXT BUDGET RECORD                    
         BE    GETB130                                                          
         TM    IOERR,IOMAX           MAX IOS REACHED?                           
         BO    GETB140                                                          
         DC    H'0'                                                             
GETB130  LA    R3,IOKEY                                                         
         CLC   IOKEY(BUDKWORK-BUDKEY),IOKEYSAV SAME ACCOUNT?                    
         BE    GETB10                                                           
         B     GETBX                                                            
                                                                                
GETB140  OI    DISPFLAG,DISIOMAX                                                
         MVC   KEYSAVE,IOKEY                                                    
                                                                                
GETBX    J     OKXIT                                                            
GETBERX  LA    R2,BASKEYH          R2=A(KEY FIELD)                              
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$BUDMX)                                           
         J     ERXIT                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER ROUTINE                                                      *         
***********************************************************************         
         USING BUDRECD,R3                                                       
FILTR    NTR1  ,                                                                
         L     RF,AOPTVALS         RF=A(OPTION VALUES)                          
         USING OPTVALSD,RF                                                      
         XR    R0,R0                                                            
         ICM   R0,3,OOFFICVL       NUMBER OF OFFICE CODES                       
         BZ    FILTR5              NONE                                         
         CLC   BUDKWORK,SPACES     SKIP NON-OFFICE BUDGETS                      
         BE    FILTRN                                                           
         LA    R2,OOFFICVL+2                                                    
*                                                                               
FILTR3   CLC   BUDKWORK,0(R2)      MATCH OFFICE CODE                            
         BE    FILTR5              YES,                                         
         LA    R2,2(R2)            CHECK NEXT OFFICE                            
         BCT   R0,FILTR3                                                        
         B     FILTRN                                                           
         DROP  RF                                                               
                                                                                
FILTR5   SR    RF,RF                                                            
         ICM   RF,1,CONTLEN        TEST CONTRA FILTER                           
         BZ    FILTRY              NONE,                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   BUDKCUNT(0),IOKEYSAV+BUDKCUNT-BUDKEY MATCH ON CONTRA?            
         BE    FILTR7                                                           
         CLI   NEGCONT,NEGFILTR                                                 
         BE    FILTRY                                                           
         B     FILTRN                                                           
                                                                                
FILTR7   CLI   NEGCONT,NEGFILTR                                                 
         BNE   FILTRY                                                           
         B     FILTRN                                                           
*                                                                               
FILTRY   CR    RE,RE                                                            
         B     XIT                                                              
FILTRN   LTR   RE,RE                                                            
         B     XIT                                                              
                                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        GET ACTUAL AMOUNTS FROM BUCKET ELEMENTS                      *         
* ON ENTRY 'IOKEY' CONTAINS NEXT CONTRA RECORD OR IS BLANK            *         
***********************************************************************         
GETACT   NTR1                                                                   
         USING CACRECD,R3                                                       
         LA    R3,IOKEY                                                         
         CLC   KEYSAVE,SPACES      RE-ENTER OF FIRST FOR ACCOUNT?               
         BNE   GETA20                                                           
         MVC   CACKEY,SPACES       CLEAR KEY                                    
         MVC   CACKCPY,MYCO        COMPANY                                      
         SR    RF,RF                                                            
         IC    RF,BASKEYH+FLDILEN-FLDHDRD                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CACKULA(0),BASKEY   ACCOUNT CODE                                 
GETA10   MVI   CACKSPAC,X'FF'                                                   
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1 GET CONTRA RECORD                        
         BE    *+14                                                             
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    GETA120                                                          
         DC    H'0'                                                             
GETA20   CLC   CACKCULA,IOKEYSAV   SAME ACCOUNT?                                
         BNE   GETAX                                                            
         CLI   CACKBTYP,C' '       CHECK BUCKET TYPE                            
         BNE   GETA10                                                           
         CLI   CACRFST,TRNELQ      TRANSACTION RECORD?                          
         BE    GETA10                                                           
         L     R4,ATEMPBLK         R4=A(BUDGET V ACTUALS TABLE)                 
         USING BVATABD,R4                                                       
         LA    RE,1                                                             
GETA30   CLI   0(R4),EOT           END OF TABLE?                                
         BE    GETA110                                                          
         SR    RF,RF                                                            
         ICM   RF,1,BVAULALN       LENGTH OF ACCOUNT                            
         BZ    GETA35                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   BVAULACT(0),CACKULC MATCH ON CONTRA ACCOUNT?                     
         BE    GETA40                                                           
GETA35   CLC   BVAULACT,SPACES     NO CONTRA ACCOUNT?                           
         BE    GETA40                                                           
         LA    R4,BVALNQ(R4)       BUMP UP TABLE                                
         B     GETA30                                                           
                                                                                
GETA40   GOTO1 AIO,IOGET+IOACCMST+IO1 GET THE CONTRA RECORD                     
         BE    *+14                                                             
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BO    GETA120                                                          
         DC    H'0'                                                             
         L     R3,AIO1             R3=A(CONTRA RECORD)                          
         LA    R3,CACRFST          R3=A(FIRST ELEMENT ON RECORD)                
GETA50   CLI   0(R3),EOR           END OF RECORD?                               
         BE    GETA110                                                          
         CLI   0(R3),CACELQ        CONTRA ACCOUNT ELEMENT?                      
         BE    GETA70                                                           
         CLI   0(R3),BUKELQ        BUCKET ELEMENT?                              
         BE    GETA80                                                           
                                                                                
GETA60   SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GETA50                                                           
                                                                                
         USING CACELD,R3           CONTRA ACCOUNT ELEMENT                       
GETA70   CLC   BVANAME,SPACES      ALREADY GOT NAME FROM BUDGET RECORD?         
         BNE   GETA60                                                           
         L     RF,AIO1                                                          
         CLC   BVAULACT,CACKULC-CACRECD(RF) FULL MATCH ON CONTRA?               
         BNE   GETA60                                                           
         SR    RF,RF                                                            
         IC    RF,CACLN                                                         
         SH    RF,=Y(CACLN1Q+1)                                                 
         BM    GETA60                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BVANAME(0),CACNAME  GET CONTRA ACCOUNT NAME                      
         B     GETA60                                                           
         DROP  R3                                                               
                                                                                
         USING BUKELD,R3           BUCKET ELEMENT                               
GETA80   CLC   BVASTART,BUKMOS     MATCH ON DATES?                              
         BH    GETA60                                                           
         CLC   BVAEND,BUKMOS                                                    
         BL    GETA60                                                           
         CLC   UL28,IOKEY+CACKULA-CACKEY SOME LEDGERS ARE CREDITS ONLY          
         BE    GETA90                                                           
         CLC   UL29,IOKEY+CACKULA-CACKEY                                        
         BE    GETA90                                                           
         CLC   UL13,IOKEY+CACKULA-CACKEY                                        
         BE    GETA90                                                           
         CLC   UL1C,IOKEY+CACKULA-CACKEY                                        
         BNE   GETA100                                                          
         CLC   UL11,IOKEY+CACKCULA-CACKEY                                       
         BE    GETA100                                                          
         CLC   UL12,IOKEY+CACKCULA-CACKEY                                       
         BE    GETA100                                                          
GETA90   MP    BUKCR,=P'-1'                                                     
GETA100  AP    BVAAAMT,BUKDR       GET ACTUAL AMOUNTS                           
         SP    BVAAAMT,BUKCR                                                    
         AP    ACTTOT,BUKDR        UPDATE ACTUAL TOTAL                          
         SP    ACTTOT,BUKCR                                                     
         B     GETA60                                                           
         DROP  R3                                                               
                                                                                
GETA110  LA    R3,IOKEY                                                         
         CLC   CHDKNULL-CHDKEY(L'CHDKNULL,R3),SPACES  NAME OR BUCKET?           
         BE    GETA10                                                           
         GOTO1 AIO,IOSEQ+IOACCDIR+IO1 NAME CONTRA SO GET NEXT RECORD            
         BE    GETA20                                                           
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    GETA120                                                          
         DC    H'0'                                                             
                                                                                
GETA120  OI    DISPFLAG,DISIOMAX                                                
         MVC   KEYSAVE,IOKEY                                                    
                                                                                
GETAX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD TSAR RECORD FOR SCREEN DATA ITEM,                      *         
* ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                         *         
*          CC NOT EQUAL- TSAR BLOCK FULL OR MAX IO                    *         
***********************************************************************         
BLDTSDAT NTR1                                                                   
                                                                                
         L     R4,ATEMPBLK         R4=A(TEMP BLOCK)                             
         USING BVATABD,R4                                                       
         SR    RF,RF                                                            
         ICM   RF,3,TSLSTREC       RF=(NO OF RECS IN TSAR)                      
         SR    R5,RF               R5=(NO OF RECS STILL TO PUT IN TSAR)         
         MH    RF,=Y(TSARDLNQ)                                                  
         LA    R4,0(RF,R4)         R4=A(NEXT TABLE ENTRY FOR TSAR)              
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LH    RF,=Y(TSARDLNQ)                                                  
         AH    RF,=Y(TSARDATA-TSARRECD)                                         
         STCM  RF,3,TSARLEN                                                     
         LA    R2,TSARDATA         R2=A(TSAR DATA)                              
         USING TSARDATD,R2                                                      
BLDT10   MVC   TSARKYNO,TSCURRNO                                                
                                                                                
         MVI   TSDFMT,TSITEM1      ITEM FORMAT TYPE                             
         MVC   TSDOFFC,BVOFFC      OFFICE                                       
         MVC   TSDCONT,BVAULACT    CONTRA CODE                                  
         MVC   TSDNAME,BVANAME     CONTRA/BUDGET NAME                           
         MVC   TSDSTART,BVASTART   START DATE                                   
         MVC   TSDEND,BVAEND       END DATE                                     
         ZAP   TSDBAMT,BVABAMT     BUDGET AMOUNT                                
         ZAP   TSDAAMT,BVAAAMT     ACTUAL AMOUNT                                
                                                                                
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    *+8                                                              
         BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
                                                                                
         MVC   TSDLINES,LINSUSED                                                
         GOTO1 ATSARADD            ADD RECORD                                   
         BNE   BLDTERRX                                                         
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         LA    R4,BVALNQ(R4)       BUMP R4 TO NEXT TABLE ENTRY                  
         BCT   R5,BLDT10                                                        
         B     BLDTX                                                            
                                                                                
BLDTERRX LTR   RB,RB                                                            
         B     XIT                                                              
BLDTX    CR    RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
FORMTSAR NTR1                                                                   
         MVI   LINSUSED,0          NUMBER OF DISPLAY LINES                      
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN LINES                     
         LH    R1,=Y(L'DUMLIN1*NDUMLINE)                                        
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
                                                                                
         USING SCRLIN1D,R2         DSECT FOR SCREEN LINE ITEM                   
         L     R4,ATSARREC                                                      
         LA    R3,TSARDATA-TSARRECD(R4)                                         
         CLI   TSARFMT-TSARRECD(R4),TSTOTITM TOTAL ITEM                         
         BE    FORMT30                                                          
         USING TSARDATD,R3                                                      
         MVC   SCR1CONT,TSDCONT    CONTRA CODE                                  
         MVC   SCR1OFFC,TSDOFFC    OFFICE                                       
         ZAP   BUDAMT,TSDBAMT      BUDGET AMOUNT                                
         ZAP   ACTAMT,TSDAAMT      ACTUAL AMOUNT                                
         MVC   WORK(2),TSDSTART    GET  START YYMM                              
         MVI   WORK+2,X'01'        MAKE DAY   01                                
         GOTO1 VDATCON,DMCB,(1,WORK),(9,SCR1STRT) START DATE                    
         MVC   WORK(2),TSDEND      GET  END   YYMM                              
         MVI   WORK+2,X'01'        MAKE DAY   01                                
         GOTO1 VDATCON,DMCB,(1,WORK),(9,SCR1END)  END   DATE                    
         MVC   TMPNBLK,SPACES      BUDGET/CONTRA NAME                           
         GOTO1 VCHOPPER,DMCB,(L'TSDNAME,TSDNAME),(L'TMPNB1,TMPNBLK),3           
         L     RE,8(R1)                                                         
         LTR   R0,RE                                                            
         BZ    FORMT20                                                          
         LA    RF,TMPNBLK                                                       
FORMT10  CLC   0(L'TMPNB1,RF),SPACES                                            
         BE    FORMT20                                                          
         MVC   SCR1NAME,0(RF)                                                   
         LA    R2,L'DUMLIN1(R2)                                                 
         LA    RF,L'TMPNB1(RF)                                                  
         BCT   RE,FORMT10                                                       
FORMT20  L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         LTR   R0,R0               NO NAME?                                     
         BNZ   *+8                                                              
         LA    R0,1                                                             
         STC   R0,LINSUSED                                                      
         B     FORMT40                                                          
         DROP  R3                                                               
                                                                                
         USING TSARTOTD,R3         TOTAL LINE ITEM                              
FORMT30  LA    R2,L'DUMLIN1(R2)    BLANK LINE                                   
         MVC   SCR1CONT(L'MX@TOTAL),MX@TOTAL                                    
         ZAP   BUDAMT,TSTBTOT                                                   
         ZAP   ACTAMT,TSTATOT                                                   
         MVI   LINSUSED,2                                                       
         MVI   DISATRIB,HILIGHTQ                                                
         DROP  R3                                                               
                                                                                
FORMT40  CURED (P8,BUDAMT),(L'SCR1BAMT,SCR1BAMT),2,MINUS=YES,DECS=ROUND         
         CURED (P8,ACTAMT),(L'SCR1AAMT,SCR1AAMT),2,MINUS=YES,DECS=ROUND         
         ZAP   BALANCE,BUDAMT                                                   
         SP    BALANCE,ACTAMT BUDGET-ACTUAL                                     
         CURED (P8,BALANCE),(L'SCR1BAL,SCR1BAL),2,MINUS=YES,DECS=ROUND          
         ZAP   PL16,BALANCE                    ACTUAL                           
         MP    PL16,=P'10000'      VARIANCE = (------ X 100 ) - 100             
         CP    BUDAMT,=P'0'                    BUDGET                           
         BNE   *+14                                                             
         ZAP   VARIANCE,=P'0'                                                   
         B     FORMT50                                                          
         DP    PL16,BUDAMT                                                      
         DP    PL16(8),=P'100'                                                  
         ZAP   VARIANCE,PL16(6)                                                 
FORMT50  CURED (P8,VARIANCE),(L'SCR1VAR,SCR1VAR),2,MINUS=YES                    
                                                                                
FORMTX   B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO GRID FORMAT                        *         
***********************************************************************         
FGRMTSAR NTR1                                                                   
FGRM01   MVI   LINSUSED,0               NUMBER OF LINES USED                    
         MVI   DISATRIB,0               DISPLAY ATTRIBUTES                      
         L     R0,ADUMLINE              CLEAR DUMMY SCREEN LINES                
         LHI   R1,DUMLINLN                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
                                                                                
         TM    OVRSTAT,OVRGINIT                                                 
         BO    FGRM10                                                           
         GOTO1 ADISGRD,DMCB,('DWNINIT',AGCTBL),BASKEY                           
         GOTO1 ADISPLAY,DISATRIB        DISPLAY DUMMY SCREEN LINES              
         B     FGRM01                                                           
                                                                                
         USING TSARRECD,R4                                                      
FGRM10   L     R4,ATSARREC              R4=A(TSAR RECORD)                       
         LA    R2,TSARDATA-TSARRECD(R4)                                         
                                                                                
         USING TSARDATD,R2                                                      
         CLI   TSARFMT,TSTOTITM         TOTAL LINE ITEM?                        
         BE    FGRM20                                                           
         ZAP   BUDAMT,TSDBAMT                                                   
         ZAP   ACTAMT,TSDAAMT                                                   
         B     FGRM25                                                           
                                                                                
         USING TSARTOTD,R2                                                      
FGRM20   ZAP   BUDAMT,TSTBTOT                                                   
         ZAP   ACTAMT,TSTATOT                                                   
         DROP  R2                                                               
                                                                                
FGRM25   ZAP   BALANCE,BUDAMT                                                   
         SP    BALANCE,ACTAMT      BUDGET-ACTUAL                                
         ZAP   PL16,BALANCE                    ACTUAL                           
         MP    PL16,=P'10000'      VARIANCE = (------ X 100 ) - 100             
         CP    BUDAMT,=P'0'                    BUDGET                           
         BNE   *+14                                                             
         ZAP   VARIANCE,=P'0'                                                   
         B     FGRM30                                                           
         DP    PL16,BUDAMT                                                      
         DP    PL16(8),=P'100'                                                  
         ZAP   VARIANCE,PL16(6)                                                 
                                                                                
FGRM30   CLI   TSARFMT,TSTOTITM         TOTAL LINE ITEM?                        
         BE    FGRM60                                                           
         GOTO1 ADISGRD,DMCB,(0,AGCTBL),BASKEY                                   
         B     FGRMX                                                            
                                                                                
FGRM60   GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL),BASKEY                            
         B     FGRMX                                                            
                                                                                
FGRMX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        DEAL WITH THE TOTAL LINE                                     *         
***********************************************************************         
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
         MVI   TSTFMT,TSTOTITM     TOTAL ITEM                                   
         ZAP   TSTBTOT,BUDTOT      BUDGET TOTAL                                 
         MVC   TSTATOT,ACTTOT      ACTUAL TOTAL                                 
                                                                                
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    *+8                                                              
         BAS   RE,FORMTSAR         FORMAT TSAR INTO GRID FORMAT                 
                                                                                
         MVC   TSTLINES,LINSUSED   NUMBER OF LINES USED BY TOTAL                
         GOTO1 ATSARADD                                                         
         MVC   TSLSTREC,TSCURRNO                                                
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
OKXIT    CR    RB,RB                                                            
         J     XIT                                                              
ERXIT    LTR   RB,RB                                                            
XIT      XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
                                                                                
UL28     DC    C'28'               LEDGER 28                                    
UL29     DC    C'29'               LEDGER 29                                    
UL13     DC    C'13'               LEDGER 13                                    
UL1C     DC    C'1C'               LEDGER 1C                                    
UL11     DC    C'11'               LEDGER 11                                    
UL12     DC    C'12'               LEDGER 12                                    
UL1P     DC    C'1P'               LEDGER 1P                                    
                                                                                
MAXBUDQ  EQU   (L'TEMPBLK/BVALNQ)  MAXIMUM NUMBER OF ENTRIES IN TABLE           
         EJECT                                                                  
DCMIX    DS    0X                                                               
         DCDDL AC#ENH28,78                                                      
         DCDDL AC#ENH29,78                                                      
         DCDDL AC#ACC,9                                                         
         DCDDL AC#CTR,9                                                         
         DCDDL AC#TOTAL,9                                                       
         DCDDL AC#TYPE,9                                                        
         DCDDL AC#CTRAN,L'MX@CTRAN      CONTRA ACCOUNT NAME                     
         DCDDL AC#STRMN,L'MX@STRMN      STARTING MONTH                          
         DCDDL AC#ENDMN,L'MX@ENDMN      ENDING MONTH                            
         DCDDL AC#BGT,L'MX@BGT          BUDGET                                  
         DCDDL AC#ACL,L'MX@ACL          ACTUAL                                  
         DCDDL AC#BAL,L'MX@BAL          BALANCE                                 
         DCDDL AC#VAR,L'MX@VAR          VARIANCE                                
         DCDDL AC#OFFC,L'MX@OFFC        OFFICE                                  
         DC    AL1(EOT)                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
*        BUDGET GRID COLUMN TABLE - COVERED BY GCTBLD                           
***********************************************************************         
GCTBL    DS    0F                                                               
                                                                                
GCTCON   DC    AL1(GCONLQ,GCCONTRA,L'LC@CTRA,L'TSDCONT)  CONTRA                 
         DC    AL2(LC@CTRA-WORKD,TSDCONT-TSARDATD)                              
         DC    AL1(GCTITOT,0,0,0)                                               
         DC    AL1(0,L'MX@TOTAL),AL2(MX@TOTAL-OVERWRKD)                         
GCONLQ   EQU   *-GCTCON                                                         
                                                                                
GCTNAME  DC    AL1(GNAMELQ,GCNAME,L'MX@CTRAN,L'TSDNAME)  CONTRA NAME            
         DC    AL2(MX@CTRAN-OVERWRKD,TSDNAME-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GNAMELQ  EQU   *-GCTNAME                                                        
                                                                                
GCTOFFC  DC    AL1(GOFFCLQ,GCOFFC,L'MX@OFFC,L'TSDOFFC)  OFFICE CODE             
         DC    AL2(MX@OFFC-OVERWRKD,TSDOFFC-TSARDATD)                           
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GOFFCLQ  EQU   *-GCTOFFC                                                        
                                                                                
GCTSTDT  DC    AL1(GSTDLQ,GCSTDT,L'MX@STRMN,1)           START DATE             
         DC    AL2(MX@STRMN-OVERWRKD,TSDSTART-TSARDATD)                         
         DC    AL1(GCTIOVER,GCTIMONO,GCTFDAT+GCTFRGHT,0)                        
         DC    AL1(0,0,0,0)                                                     
GSTDLQ   EQU   *-GCTSTDT                                                        
                                                                                
GCTEND   DC    AL1(GENDLQ,GCEND,L'MX@ENDMN,1)            END DATE               
         DC    AL2(MX@ENDMN-OVERWRKD,TSDEND-TSARDATD)                           
         DC    AL1(GCTIOVER,GCTIMONO,GCTFDAT+GCTFRGHT,0)                        
         DC    AL1(0,0,0,0)                                                     
GENDLQ   EQU   *-GCTEND                                                         
                                                                                
GCTBUD   DC    AL1(GBUDLQ,GCBUD,L'MX@BGT,L'TSDBAMT)      BUDGET                 
         DC    AL2(MX@BGT-OVERWRKD,TSDBAMT-TSARDATD)                            
         DC    AL1(GCTITOT+GCTIOVER,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,L'TSTBTOT),AL2(TSTBTOT-TSARTOTD)                           
GBUDLQ   EQU   *-GCTBUD                                                         
                                                                                
GCTACTU  DC    AL1(GACTULQ,GCACTU,L'MX@ACL,L'TSDAAMT)    ACTUAL                 
         DC    AL2(MX@ACL-OVERWRKD,TSDAAMT-TSARDATD)                            
         DC    AL1(GCTITOT+GCTIOVER,0,GCTFNUM+GCTFRGHT,0)                       
         DC    AL1(0,L'TSTATOT),AL2(TSTATOT-TSARTOTD)                           
GACTULQ  EQU   *-GCTACTU                                                        
                                                                                
GCTBAL   DC    AL1(GBALLQ,GCBAL,L'MX@BAL,L'BALANCE)      BALANCE                
         DC    AL2(MX@BAL-OVERWRKD,BALANCE-OVERWRKD)                            
         DC    AL1(GCTIOVER+GCTITOT,GCTISPDA,GCTFNUM+GCTFRGHT,GCTISPTO)         
         DC    AL1(0,L'BALANCE),AL2(BALANCE-OVERWRKD)                           
GBALLQ   EQU   *-GCTBAL                                                         
                                                                                
GCTVAR   DC    AL1(GVARLQ,GCVAR,L'MX@VAR,L'VARIANCE)     VARIANCE               
         DC    AL2(MX@VAR-OVERWRKD,VARIANCE-OVERWRKD)                           
         DC    AL1(GCTIOVER+GCTITOT,GCTISPDA,GCTFNUM+GCTFRGHT,GCTISPTO)         
         DC    AL1(0,L'VARIANCE),AL2(VARIANCE-OVERWRKD)                         
GVARLQ   EQU   *-GCTVAR                                                         
                                                                                
         DC    AL1(EOT)                                                         
*-----------------------------------                                            
* GRID COLUMN EQUATES                                                           
*-----------------------------------                                            
GCCONTRA EQU   91       CONTRA                                                  
GCNAME   EQU   2        CONTRA ACCOUNT NAME                                     
GCOFFC   EQU   3        OFFICE CODE                                             
GCSTDT   EQU   4        START DATE                                              
GCEND    EQU   5        END DATE                                                
GCBUD    EQU   6        BUDGET                                                  
GCACTU   EQU   7        ACTUAL                                                  
GCBAL    EQU   8        BALANCE                                                 
GCVAR    EQU   9        VARIANCE                                                
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                                         
***********************************************************************         
OVERWRKD DSECT                                                                  
ORELO    DS    A                   A(RELOCATABLE VALUE)                         
AGCTBL   DS    A                   A(GRID TABLE)                                
TMPNBLK  DS    0CL42               TEMP NAME BLOCK                              
TMPNB1   DS    CL14                                                             
TMPNB2   DS    CL14                                                             
TMPNB3   DS    CL14                                                             
BUDCODE  DS    CL(L'BUDKCOD)       BUDGET TYPE CODE                             
BUDSTART DS    PL(L'BAMMNTH)       BUDGET START MONTH                           
BUDENDMN DS    PL(L'BAMMNTH)       BUDGET END MONTH                             
BUDAMT   DS    PL(L'BAMBUDG)       BUDGET AMOUNT                                
BUDNAME  DS    CL(L'NAMEREC)       BUDGET NAME                                  
CONTCODE DS    CL(L'CHDKULA)       CONTRA CODE                                  
CONTNAME DS    CL(L'NAMEREC)       CONTRA NAME                                  
ACCNAME  DS    CL(L'NAMEREC)       ACCOUNT NAME                                 
PL16     DS    PL16                                                             
ACTAMT   DS    PL8                 ACTUAL AMOUT                                 
BALANCE  DS    PL8                 BALANCE (BUDGET-ACTUALS)                     
VARIANCE DS    PL8                 VARIANCE                                     
***********************************************************************         
DSMIX    DS    0C                                                               
MX@ENH28 DS    CL78                                                             
MX@ENH29 DS    CL78                                                             
MX@ACC   DS    CL9                                                              
MX@CTR   DS    CL9                                                              
MX@TOTAL DS    CL9                                                              
MX@TYPE  DS    CL9                                                              
MX@CTRAN DS    CL19                CONTRA ACCOUNT NAME                          
MX@STRMN DS    CL14                STARTING MONTH                               
MX@ENDMN DS    CL12                ENDING MONTH                                 
MX@BGT   DS    CL6                 BUDGET                                       
MX@ACL   DS    CL6                 ACTUAL                                       
MX@BAL   DS    CL7                 BALANCE                                      
MX@VAR   DS    CL8                 VARIANCE                                     
MX@OFFC  DS    CL2                 OF(FICE)                                     
         EJECT                                                                  
***********************************************************************         
SCRLIN1D DSECT                     COVER SCREEN ITEM LINE1                      
SCR1CONT DS    CL(L'CHDKULA)       CONTRA ACCOUNT CODE                          
         DS    CL1                                                              
SCR1NAME DS    CL11                BUDGET/CONTRA ACCOUNT NAME                   
         DS    CL1                                                              
SCR1OFFC DS    CL2                 OFFICE                                       
         DS    CL1                                                              
SCR1STRT DS    CL6                 START DATE                                   
         DS    CL1                                                              
SCR1END  DS    CL6                 END DATE                                     
         DS    CL1                                                              
SCR1BAMT DS    CL8                 BUDGET AMOUNT                                
         DS    CL1                                                              
SCR1AAMT DS    CL8                 ACTUAL AMOUNT                                
         DS    CL1                                                              
SCR1BAL  DS    CL8                 BALANCE AMOUNT                               
         DS    CL1                                                              
SCR1VAR  DS    CL7                 VARIANCE                                     
***********************************************************************         
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSITEM1  EQU   1                   ITEM 1 FORMAT                                
TSDCONT  DS    CL(L'CHDKULA)       CONTRA CODE                                  
TSDNAME  DS    CL(L'NAMEREC)       CONTRA NAME                                  
TSDOFFC  DS    CL2                 OFFICE                                       
TSDSTART DS    PL2                 START DATE                                   
TSDEND   DS    PL2                 END DATE                                     
TSDBAMT  DS    PL8                 BUDGET AMOUNT                                
TSDAAMT  DS    PL8                 ACTIAL AMOUNT                                
TSARDLNQ EQU   *-TSARDATD                                                       
                                                                                
TSARTOTD DSECT                     TSAR TOTAL LINE                              
TSTLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSTFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSTOTITM EQU   2                   TOTAL FORMAT                                 
TSTBTOT  DS    PL8                 BUDGET TOTAL                                 
TSTATOT  DS    PL8                 ACTUAL TOTAL                                 
TSTLNQ   EQU   *-TSARTOTD                                                       
***********************************************************************         
                                                                                
MYUNSCND DSECT                                                                  
MYUNSCLH DS    CL10                                                             
MYUNSCRH DS    CL(L'NAMEREC)                                                    
                                                                                
***********************************************************************         
BVATABD  DSECT                     BUDGET V ACTUALS TABLE                       
BVOFFC   DS    CL2                                                              
BVAULACT DS    0CL(L'CHDKULA)      CONTRA CODE                                  
BVAUNT   DS    CL(L'CHDKUNT)                                                    
BVALDG   DS    CL(L'CHDKLDG)                                                    
BVAACT   DS    CL(L'CHDKACT)                                                    
BVAULALN DS    X                   LENGTH OF CONTRA CODE                        
BVANAME  DS    CL(L'NAMEREC)       BUDGET NAME (IF PRESENT) ELSE CONTRA         
BVASTART DS    PL2                 START MONTH                                  
BVAEND   DS    PL2                 END MONTH                                    
BVABAMT  DS    PL8                 BUDGET AMOUNT                                
BVAAAMT  DS    PL8                 ACTUAL AMOUNT                                
BVALNQ   EQU   *-BVATABD                                                        
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE ACENQWORK                                                      
         EJECT                                                                  
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
BUDNUM   DS    XL(L'BUDKNO1)       BUDGET TYPE NUMBER                           
BUVALS   DS    0PL8                CREDITOR VALUES                              
ACTTOT   DS    PL8                 ACTUAL CHARGES TOTAL                         
BUDTOT   DS    PL8                 BUDGET TOTAL                                 
BUVALLNQ EQU   *-BUVALS                                                         
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACENQ0F   05/15/08'                                      
         END                                                                    
