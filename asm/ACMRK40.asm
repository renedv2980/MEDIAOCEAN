*          DATA SET ACMRK40    AT LEVEL 092 AS OF 03/18/15                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 045076.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE T61640A                                                                  
ACMRK40 TITLE 'ROOT ROUTINES - 1'                                               
* JFOX 077 OVERLAY CREATED FROM "ROUT" NMOD IN ROOT                             
* JFOX 078 RESTRICTIONS FOR SECONDARY CURRENCY DISCOUNT COLUMN                  
* JFOX 079 READ TRANSACTION/CALL TOBACCO FOR 2ND CURRENCY COLUMNS               
* JFOX 080 USED (CONTRA'D) DATE FILTER                                          
* ???? 081 DON'T DISPLAY DATE IF MEDIA MONTH DATE NOT IN TSAR RECORD            
* JFOX 082 US/UK COMPATIBILITY                                                  
* JFOX 083 REV OPTION FOR WIP/HOLD IS FOR FILE READ ONLY                        
ACMRK40  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MK40**,RA,R9,R8                                              
         L     RC,4(RD)                                                         
         L     RC,68(RC)                                                        
         USING WORKD,RC                                                         
         USING TWAD,R6                                                          
         USING SAVED,R7                                                         
         L     R5,AIOCNTL                                                       
         USING IOCNTLD,R5                                                       
         SRL   RF,32-8             BRANCH INDEX HELD IN HOB RF                  
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     ACCELS              GET ACCOUNT ELEMENTS                         
         B     BLDCURT             BUILD CURRENCY TABLE ENTRIES                 
         B     BLDCURR             BUILD CURRENCY TABLE ENTRIES                 
         B     BLDDIS              BUILD DISPLAY COLUMN DISPLACEMENTS           
         B     BLDLIN              BUILD A DISPLAY LINE                         
         B     BLDPFK              BUILD PFKEY DISPLAY LINE                     
         B     BLDSRC              BUILD SOURCE A/C (CPJEL/MXPEL/OTHEL)         
         B     BLDTOT              BUILD TOTALS HEADINGS AND AMOUNTS            
         B     DISPLAY             BUILD & DISPLAY DETAIL SCREEN                
         B     EXTRSL              EXTEND TRANSACTION STATUS ELEMENT            
         B     FILTER              VALIDATE FILTERS                             
         B     GENFILT             GENERAL FILTERS                              
         B     BLDINV              GET LONG INVOICE NUMBER                      
         B     BLDMOS              GET MEDIA MOS                                
         B     BLDWRKC             GET WORKCODE                                 
                                                                                
ROUTL    MVI   DUB,0               SET CC LOW                                   
         B     ROUTCC                                                           
ROUTH    MVI   DUB,2               SET CC HIGH                                  
         B     ROUTCC                                                           
ROUTE    MVI   DUB,1               SET CC EQUAL                                 
ROUTCC   CLI   DUB,1                                                            
                                                                                
ROUTX    XIT1  ,                                                                
         EJECT                                                                  
ACCELS   DS    0H                                                               
         XC    RECVALS(RECVALSL),RECVALS                                        
         XR    RF,RF                                                            
         L     R1,AIOBUFF                                                       
         LA    R1,ACCRFST-ACCRECD(R1)                                           
                                                                                
         USING CPYELD,R1                                                        
ACCEL02  CLI   CPYEL,CPYELQ        TEST COMPANY ELEMENT                         
         BNE   ACCEL04                                                          
         STCM  R1,15,RECCPYEL                                                   
         B     ACCEL30                                                          
                                                                                
         USING NAMELD,R1                                                        
ACCEL04  CLI   NAMEL,NAMELQ        TEST NAME ELEMENT                            
         BNE   ACCEL06                                                          
         MVC   RECNAME,SPACES                                                   
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+8                                                           
         B     ACCEL30                                                          
         MVC   RECNAME(0),NAMEREC                                               
                                                                                
         USING RSTELD,R1                                                        
ACCEL06  CLI   RSTEL,RSTELQ        TEST RECORD STATUS ELEMENT                   
         BNE   ACCEL08                                                          
         STCM  R1,15,RECRSTEL                                                   
*&&UK                                                                           
         TM    GETIND,GETINOSV     TEST DON'T SET SAVED VALUES                  
         BO    ACCEL30                                                          
         NI    ACCIND2,X'FF'-ACCISUND                                           
         CLI   RSTLN,RSTLN3Q       TEST LONG ELEMENT                            
         BL    ACCEL30                                                          
         TM    RSTSTAT5,RSTSSUND   TEST SUNDRY CREDITOR                         
         BZ    *+8                                                              
         OI    ACCIND2,ACCISUND                                                 
*&&                                                                             
         B     ACCEL30                                                          
                                                                                
         USING ABLELD,R1                                                        
ACCEL08  CLI   ABLEL,ABLELQ        TEST ACCOUNT BALANCE ELEMENT                 
         BNE   ACCEL10                                                          
         STCM  R1,15,RECABLEL                                                   
         ZAP   RECBAL,ABLDR        DEBITS                                       
         SP    RECBAL,ABLCR        -CREDITS                                     
         AP    RECBAL,ABLFRWD      +BBF                                         
         B     ACCEL30                                                          
                                                                                
         USING PPRELD,R1                                                        
ACCEL10  CLI   PPREL,PPRELQ        TEST PRODUCTION PROFILE ELEMENT              
         BNE   ACCEL12                                                          
         STCM  R1,15,RECPPREL                                                   
         B     ACCEL30                                                          
                                                                                
         USING RATELD,R1                                                        
ACCEL12  TM    GETIND,GETINOSV     TEST DON'T SET SAVED VALUES                  
         BO    ACCEL14                                                          
         TM    GETIND,GETIRTXQ     TEST SEEKING FREELANCE TAX ELEMENT           
         BZ    ACCEL14                                                          
         CLI   RATEL,RATETAXQ      TEST FREELANCE TAX ELEMENT                   
         BNE   ACCEL14                                                          
         OI    ACCIND2,ACCIFRTX    SET FREELANCE TAX RATE ON SUPPLIER           
         B     ACCEL30                                                          
                                                                                
         USING ASTELD,R1                                                        
ACCEL14  CLI   ASTEL,ASTELQ        TEST ACCOUNT STATUS ELEMENT                  
         BNE   ACCEL16                                                          
         MVC   RECASTS1,ASTSTAT1                                                
         CLC   ASTCUR,SPACES                                                    
         BNH   *+10                                                             
         MVC   RECCURR,ASTCUR                                                   
         B     ACCEL30                                                          
                                                                                
         USING SPAELD,R1                                                        
ACCEL16  CLI   SPAEL,SPAELQ        TEST SPECIAL POSTING A/C ELEMENT             
         BNE   ACCEL18                                                          
         CLI   SPATYPE,SPATANAL                                                 
         BNE   *+14                                                             
         MVC   RECANAL,SPAAANAL    EXTRACT 12 BYTE ANALYSIS ACCOUNT             
         B     ACCEL30                                                          
         CLI   SPATYPE,SPATEXDF                                                 
         BNE   *+14                                                             
         MVC   RECEXDF,SPAAULA     EXTRACT EXCHANGE DIFFERENCE ACCOUNT          
         B     ACCEL30                                                          
         CLI   SPATYPE,SPATCSHD                                                 
         BNE   *+14                                                             
         MVC   RECCDSC,SPAAULA     EXTRACT EXCHANGE DIFFERENCE ACCOUNT          
         B     ACCEL30                                                          
         CLI   SPATYPE,SPATBCHA                                                 
         BNE   *+14                                                             
         MVC   RECBCHA,SPAAULA     EXTRACT BANK CHARGES ACCOUNT                 
         B     ACCEL30                                                          
         DS    0H                  NEXT SPATYPE                                 
         B     ACCEL30                                                          
                                                                                
ACCEL18  DS    0H                  NEXT ACCOUNT ELEMENT OF INTEREST             
                                                                                
ACCEL20  DS    0H                  NEXT ACCOUNT ELEMENT OF INTEREST             
                                                                                
ACCEL22  DS    0H                  NEXT ACCOUNT ELEMENT OF INTEREST             
                                                                                
ACCEL24  DS    0H                  NEXT ACCOUNT ELEMENT OF INTEREST             
                                                                                
ACCEL30  IC    RF,1(R1)            BUMP TO NEXT ACCOUNT ELEMENT                 
         AR    R1,RF                                                            
         CLI   0(R1),0                                                          
         BNE   ACCEL02                                                          
                                                                                
ACCELSX  B     ROUTE                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD CURRENCY TABLE ENTRIES                                        *         
* NTRY  P1 - 'ENTRY_TYPE',A(CURRENCY CODE)                            *         
***********************************************************************         
                                                                                
*&&UK                                                                           
BLDCURT  L     RE,ACURRTAB                                                      
         USING CURTABD,RE          SEARCH TABLE FOR SAME CURRENCY               
         L     R2,0(R1)                                                         
         PUSH  USING                                                            
         USING FILTD,FILTVAL                                                    
COMP     USING CURTABD,COMPCURT                                                 
         CLC   COMP.CURTCUR,0(R2)  ADD NON-AGENCY CURRENCIES ONLY               
         BNE   *+12                                                             
         NI    FILT1,FF-FILTAC     DON'T FILTER AGENCY CURRENCY                 
         B     BLDCURTX                                                         
         POP   USING                                                            
         LA    RF,CURRTABN                                                      
BLDC002  CLI   CURTCUR,EOT         TEST EMPTY ENTRY                             
         BE    BLDC004                                                          
         CLC   CURTCUR,0(R2)       TEST CURRENCY ALREADY IN TABLE               
         BE    BLDCURTX                                                         
         LA    RE,L'CURRTAB(RE)                                                 
         BCT   RF,BLDC002                                                       
         DC    H'0'                CURRENCY TABLE FULL                          
BLDC004  MVC   CURTCUR,0(R2)       ADD CURRENCY CODE TO TABLE                   
         MVI   CURTDECP,FF         SET UNRESOLVED                               
         MVC   CURTIND1,0(R1)      SET TYPE OF CURRENCY ENTRY                   
         DROP  RE                                                               
BLDCURTX B     ROUTE                                                            
*&&                                                                             
*&&US                                                                           
BLDCURT  B     ROUTE                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD CURRENCIES                                                    *         
***********************************************************************         
                                                                                
*&&UK                                                                           
BLDCURR  L     R2,ACURRTAB                                                      
         USING CURTABD,R2                                                       
         LA    R0,CURRTABN                                                      
BLDR002  CLI   CURTCUR,EOT                                                      
         BE    BLDCURRX                                                         
         CLI   CURTDECP,FF         TEST UNRESOLVED ENTRY                        
         BNE   BLDR004                                                          
         GOTO1 VBLDCUR,DMCB,CURTCUR,CURTABD,ACOM                                
         CLI   0(R1),0             TEST PROBLEM RESOLVING CURRENCY              
         BNE   ROUTH                                                            
         CLC   CURTCUR,PRSTCURT+(CURTCUR-CURTABD)                               
         BNE   BLDR004                                                          
         MVC   PRSTCURT,CURTABD                                                 
BLDR004  LA    R2,L'CURRTAB(R2)                                                 
         BCT   R0,BLDR002                                                       
BLDCURRX B     ROUTE                                                            
         DROP  R2                                                               
*&&                                                                             
*&&US                                                                           
BLDCURR  B     ROUTE                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD HEADINGS AND DISPLAY DISPLACEMENTS                            *         
***********************************************************************         
                                                                                
BLDDIS   MVC   DISHEAD(2*L'DISHEAD),SPACES                                      
         XC    DISDISP(DISDISPL),DISDISP                                        
         XC    REPDISP(DISDISPL),REPDISP                                        
         XC    TEMP,TEMP                                                        
         MVI   FLAG,0              SET FIRST TIME THROUGH                       
*                                                                               
         TM    PCDRIVEN,PCGRIDQ     ARE WE RUNNING UNDER GRIDS                  
         BO    BLDD00               DON'T HONOR CONTROL PRFILES IF YES          
*                                                                               
         LA    R3,PROFDIS                                                       
         OC    PROFDIS,PROFDIS      DISPLAY SET BY PROFILE                      
         BNZ   BLDD02                                                           
BLDD00   L     R3,APROFDFT          DISPLAY SET BY DEFAULTS                     
         USING PROFDFTD,R3                                                      
         B     *+8                                                              
BLDD01   LA    R3,PROFDFTL(R3)                                                  
         CLI   PROFALFA,EOT                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   PROFALFA,XACTALFA                                                
         BNE   BLDD01                                                           
         LA    R3,PROFDFDS                                                      
         DROP  R3                                                               
                                                                                
BLDD02   LA    R0,L'PROFDIS                                                     
         LA    R2,OPTDIS           DISPLAY SET BY OPTION                        
         LA    R1,TEMP                                                          
         OC    OPTDIS,OPTDIS       TEST OPTION                                  
         BZ    *+12                                                             
BLDD04   CLI   0(R2),C'+'          TEST ADD PROFILE HERE                        
         BNE   BLDD08                                                           
         LA    R2,1(R2)                                                         
         LTR   R3,R3               R3 - A(PROFILE/DEFAULT)                      
         BZ    BLDD04                                                           
         MVC   0(L'PROFDIS,R1),0(R3)                                            
         LA    R1,L'PROFDIS(R1)                                                 
         BCTR  R1,0                                                             
         CLI   0(R1),0             SEEK LAST DISPLAY COLUMN                     
         BE    *-6                                                              
         LA    R1,1(R1)                                                         
*&&UK                                                                           
         TM    SAFCIND1,SAFCI1SC   TEST FORCE COLUMNS                           
         BNO   BLDD06                                                           
                                                                                
         CLI   XACTOLAY,BVOLAY                                                  
         BE    *+20                                                             
         TM    SAFCIND1,SAFCI1BC   TEST BANK IN CURRENCY                        
         BNO   *+24                                                             
         CLI   XACTOLAY,CMOLAY     CREDITOR/MANUAL OR BANK/VOID ONLY            
         BNE   *+16                                                             
         MVI   0(R1),COLAFCQ                                                    
         MVI   1(R1),COLEXDQ                                                    
         LA    R1,2(R1)                                                         
                                                                                
         CLI   XACTOLAY,BROLAY     CREDITOR/CONTRA OR BANK/REC. ONLY            
         BE    *+12                                                             
         CLI   XACTOLAY,COOLAY                                                  
         BNE   *+12                                                             
         MVI   0(R1),COLAFCQ                                                    
         LA    R1,1(R1)                                                         
*&&                                                                             
BLDD06   CLI   0(R2),C'+'          TEST DOUBLE +                                
         BNE   BLDD08                                                           
         LA    R2,1(R2)                                                         
         L     R3,ADISTAB                                                       
         B     *+12                                                             
         LA    R3,DISTABL(R3)                                                   
         LA    R1,1(R1)                                                         
         OC    0(1,R1),DISCOL-DISTABD(R3)                                       
         BNZ   *-14                                                             
         XR    R3,R3               PROFILE NOT ALLOWED AGAIN                    
                                                                                
BLDD08   LA    RE,TEMP             REMOVE EARLIER DUPLICATES                    
         B     *+8                                                              
         LA    RE,1(RE)                                                         
         CR    RE,R1                                                            
         BNL   *+22                                                             
         CLC   0(1,RE),0(R2)                                                    
         BNE   *-16                                                             
         MVI   0(RE),FF                                                         
         B     *-24                                                             
         OC    0(1,R1),0(R2)       INSERT NEW COLUMN                            
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,BLDD04                                                        
*&&UK                                                                           
         OC    SCNDCURT,SCNDCURT   TEST SECONDARY CURRENCY IN USE               
         BZ    BLDD09                                                           
         TM    OPTDIND,OPTDISIQ+OPTDISRQ                                        
         BZ    BLDD09                                                           
         MVC   TEMP+128(128),TEMP  MOVE DISPLAY COLUMNS TEMPORARILY             
         XC    TEMP(128),TEMP      AND CLEAR LIST                               
         LA    R0,128              ALLOWS UP TO 128 COLUMNS                     
         LA    RF,TEMP             IF THAT'S NOT ENOUGH                         
         LA    RE,TEMP+128         FIND ANOTHER TEMPORARY AREA                  
BLDD08A  MVC   0(1,RF),0(RE)       REBUILD DISPLAY COLUMN LIST                  
         LA    R1,COLSAMQ                                                       
         CLI   0(RE),COLAMTQ                                                    
         BE    BLDD08D                                                          
         LA    R1,COLSDRQ                                                       
         CLI   0(RE),COLDRSQ                                                    
         BE    BLDD08D                                                          
         LA    R1,COLSCRQ                                                       
         CLI   0(RE),COLCRSQ                                                    
         BE    BLDD08D                                                          
         LA    R1,COLSDSQ                                                       
         CLI   0(RE),COLDSCQ                                                    
         BE    BLDD08D                                                          
         LA    R1,COLSDIQ                                                       
         CLI   0(RE),COLDIFQ                                                    
         BE    BLDD08D                                                          
         LA    R1,COLSADQ                                                       
         CLI   0(RE),COLADVQ                                                    
         BNE   BLDD08F                                                          
BLDD08D  TM    OPTDIND,OPTDISIQ    TEST INSERT SECOND CURRENCY COLUMNS          
         BZ    *+8                 NO - REPLACE ORIGINAL COLUMN                 
         LA    RF,1(RF)            YES - ADD TO ORIGINAL COLUMN                 
         STC   R1,0(RF)            SET COLUMN VALUE                             
BLDD08F  LA    RF,1(RF)            NEXT DESTINATION COLUMN                      
         LA    RE,1(RE)            NEXT SOURCE COLUMN                           
         BCT   R0,BLDD08A                                                       
         XC    TEMP+128(128),TEMP+128                                           
*&&                                                                             
BLDD09   LA    R1,TEMP             VERIFY COLUMN ENTRIES                        
BLDD10   XR    R2,R2                                                            
         ICM   R2,1,0(R1)                                                       
         BZ    BLDD14                                                           
         CLI   0(R1),DISDISPL      TEST WITHIN DISPLAY RANGE                    
         BH    BLDD12                                                           
         BCTR  R2,0                                                             
         MH    R2,=Y(DISTABL)      DISNUM-1*4                                   
         A     R2,ADISTAB                                                       
         LA    R2,DISTABL(R2)                                                   
         USING DISTABD,R2          R2=A(DISPLAY ELEMENT TABLE ENTRY)            
         CLI   DISLEN,0                                                         
         BNE   *+8                                                              
         L     R2,ASUDTAB          SUBSTITUTE DISTAB (SAME DSECT)               
         CLC   DISCOL,0(R1)        TEST CORRECT COLUMN                          
         BE    *+12                                                             
         LA    R2,DISTABL(R2)                                                   
         B     *-14                                                             
         MVC   THREE,DISVACM       BUT TEST VALIDITY                            
         NC    THREE,XACTOPEV      TEST ELEMENT VALID FOR THIS ACTION           
         CLC   THREE,XACTOPEV                                                   
         BNE   BLDD12                                                           
         TM    DISCIND,DISCI2CQ    TEST SECONDARY CURRENCY COLUMN               
         BZ    BLDD13                                                           
*&&UK                                                                           
         OC    SCNDCURT,SCNDCURT   TEST SECONDARY CURRENCY SET                  
         BNZ   BLDD13                                                           
*&&                                                                             
BLDD12   MVI   0(R1),FF                                                         
BLDD13   LA    R1,1(R1)                                                         
         B     BLDD10                                                           
         DROP  R2                                                               
                                                                                
BLDD14   LA    R2,TEMP             REPLACE REPETITIONS WITH X'FF'               
         BCTR  R1,0                                                             
         B     *+8                                                              
         LA    R2,1(R2)                                                         
         CR    R2,R1                                                            
         BNL   *+18                                                             
         CLC   0(1,R2),0(R1)                                                    
         BNE   *-16                                                             
         MVI   0(R1),FF                                                         
         LA    R2,TEMP                                                          
         CR    R2,R1                                                            
         BL    BLDD14                                                           
                                                                                
         LR    R1,R2               DETERMINE NUMBER OF COLUMNS                  
BLDD16   CLI   0(R1),EOT                                                        
         BE    BLDD18                                                           
         CLI   0(R1),FF            REMOVE X'FF'S                                
         BE    *+20                                                             
         MVC   0(1,R2),0(R1)                                                    
         CR    R2,R1                                                            
         LA    R2,1(R2)                                                         
         BE    *+8                                                              
         MVI   0(R1),0             CLEAR FF'S AND USED CODES                    
         LA    R1,1(R1)                                                         
         B     BLDD16                                                           
                                                                                
BLDD18   LA    R1,TEMP                                                          
         MVI   0(R2),EOT           MARK END OF COLUMNS                          
         SR    R2,R1                                                            
         BNP   BLDDISX             NOTHING TO DISPLAY                           
         STC   R2,CHAR                                                          
                                                                                
         XR    R0,R0                                                            
         ICM   R0,1,SSCRCOLL       VALIDATE SCROLL VALUES                       
         BP    *+8                                                              
         LA    R0,1                                                             
         CR    R0,R2                                                            
         BNH   *+6                                                              
         LR    R0,R2                                                            
         STC   R0,SSCRCOLL                                                      
         ICM   R0,1,SSCRCOLL+1                                                  
         BP    *+8                                                              
         LA    R0,1                                                             
         CR    R0,R2                                                            
         BNH   *+6                                                              
         LR    R0,R2                                                            
         STC   R0,SSCRCOLL+1                                                    
                                                                                
BLDD20   XC    COLSUBS(COLSUBSL),COLSUBS  COLUMN SUBSTITUTION LIST              
         OC    FILTCHQ,FILTCHQ     TEST INTERNAL CHEQUE/INVOICE FILTER          
         BZ    BLDD24                                                           
         CLI   XACTOLAY,MROLAY     NOT INTERESTED FOR MEDIA/RECONCILE           
         BE    BLDD24                                                           
         CLI   FILTCHQ,INCLUDE     TEST CHEQUE MODE                             
         BE    *+12                                                             
         CLI   FILTCHQ,EXCLUDE     TEST INVOICE MODE                            
         BNE   BLDD24                                                           
*&&UK                                                                           
         TM    VOIDIND1,VOIDANBP   TEST ANALYSED BANK POSTINGS                  
         BZ    *+12                                                             
         MVI   COLFR1,COLDSCQ      REPLACE DISCOUNT AMOUNT                      
         MVI   COLTO1,COLFXRQ      WITH FIXED REFERENCE                         
*&&                                                                             
         MVI   COLFR2,COLSRCQ      SUBSTITUTE SOURCE ACCOUNT                    
         MVI   COLTO2,COLCONQ      WITH CONTRA ACCOUNT                          
         CLI   XACTOLAY,BVOLAY     TEST BANK/VOID                               
         BNE   BLDD22                                                           
         MVI   COLFR3,COLUSEQ      SUPPRESS USED DATE                           
*&&US                                                                           
         CLI   FILTCHQ,INCLUDE     TEST CHEQUE MODE                             
         BNE   *+12                                                             
         MVI   COLFR1,COLTAXQ      SUPPRESS TAX/BASIS                           
         MVI   COLFR5,COLMMDQ      SUPPRESS MEDIA MONTH/DATE                    
*&&                                                                             
BLDD22   CLI   FILTCHQ,INCLUDE     TEST CHEQUE MODE                             
         BE    BLDD24                                                           
*&&UK                                                                           
         TM    VOIDIND1,VOIDANBP   INVOICES - TEST ANALYSED POSTINGS            
         BZ    *+12                                                             
         MVI   COLFR1,COLFXRQ      REPLACE FIXED REFERENCE                      
         MVI   COLTO1,COLDSCQ      WITH DISCOUNT AMOUNT                         
*&&                                                                             
         MVI   COLFR2,COLCHQQ      SUBSTITUTE CHEQUE NUMBER                     
         MVI   COLTO2,COLREFQ      WITH REFERENCE NUMBER                        
         MVI   COLFR3,0            RETAIN USED DATE                             
         CLI   XACTOLAY,BVOLAY     TEST BANK/VOID                               
         BNE   *+12                                                             
         MVI   COLFR4,COLCRSQ      SUBSTITUTE CREDITS                           
         MVI   COLTO4,COLDRSQ      WITH DEBITS                                  
                                                                                
BLDD24   OC    COLSUBS(COLSUBSL),COLSUBS  TEST ANY SUBSTITUTION                 
         BZ    BLDD40                                                           
         LA    R1,TEMP             R1=A(DISPLAY COLUMN PROFILE)                 
         LA    R0,L'PROFDIS        R0=NUMBER OF COLUMNS                         
BLDD26   CLC   0(1,R1),COLFR1      TEST MATCH ON FROM COLUMN                    
         BNE   BLDD28                                                           
         MVC   0(1,R1),COLTO1      SUBSTITUTE TO COLUMN                         
         MVI   COLFR1,0            CLEAR TO STOP FURTHER SUBSTITUTION           
         MVI   COLTO1,0                                                         
         B     BLDD38                                                           
BLDD28   CLC   0(1,R1),COLFR2                                                   
         BNE   BLDD30                                                           
         MVC   0(1,R1),COLTO2                                                   
         MVI   COLFR2,0                                                         
         MVI   COLTO2,0                                                         
         B     BLDD38                                                           
BLDD30   CLC   0(1,R1),COLFR3                                                   
         BNE   BLDD32                                                           
         MVC   0(1,R1),COLTO3                                                   
         MVI   COLFR3,0                                                         
         MVI   COLTO3,0                                                         
         B     BLDD38                                                           
BLDD32   CLC   0(1,R1),COLFR4                                                   
         BNE   BLDD34                                                           
         MVC   0(1,R1),COLTO4                                                   
         MVI   COLFR4,0                                                         
         MVI   COLTO4,0                                                         
         B     BLDD38                                                           
BLDD34   CLC   0(1,R1),COLFR5                                                   
         BNE   BLDD36                                                           
         MVC   0(1,R1),COLTO5                                                   
         MVI   COLFR5,0                                                         
         MVI   COLTO5,0                                                         
         B     BLDD38                                                           
BLDD36   DS    0H                  NEXT SUBSTITUTION PAIR                       
                                                                                
BLDD38   BCTR  R1,0                                                             
         TM    SSCROLL,SSCROLLR    TEST RIGHT BUILD                             
         BO    *+8                                                              
         LA    R1,2(R1)                                                         
         BCT   R0,BLDD26                                                        
                                                                                
BLDD40   XR    R0,R0                                                            
         XR    RE,RE               RE=DISPLACEMENT TO DISPLAY VALUE             
         LA    R1,TEMP                                                          
                                                                                
         TM    SSCROLL,SSCROLLR    START COLUMN                                 
         BNO   *+14                                                             
         IC    R0,SSCRCOLL+1       R0=MAXIMUM COLUMNS (RTL BUILD)               
         AR    R1,R0               R1=A(START DISPLAY COLUMN)                   
         B     *+16                                                             
         IC    R0,SSCRCOLL                                                      
         AR    R1,R0               R1=A(START DISPLAY COLUMN)                   
         BCTR  R1,0                                                             
         IC    R0,CHAR             R0=MAXIMUM COLUMNS (LTR BUILD)               
                                                                                
         MVI   CHAR,0                                                           
BLDD42   XR    R2,R2                                                            
         ICM   R2,1,0(R1)          R2=DISPLAY LINE ELEMENT NUMBER               
         BZ    BLDD56              ELEMENT NOT REQUIRED                         
         CLI   FLAG,0              TEST FIRST TIME THROUGH                      
         BE    *+6                 YES - NOT SETTING DISPLACEMENTS              
         LR    R4,R2               TAKE ELEMENT NUMBER                          
         BCTR  R2,0                ESTABLISH DISTAB ENTRY                       
         MH    R2,=Y(DISTABL)      DISNUM-1*4                                   
         A     R2,ADISTAB                                                       
         LA    R2,DISTABL(R2)                                                   
         USING DISTABD,R2          R2=A(DISPLAY ELEMENT TABLE ENTRY)            
         CLI   DISLEN,0                                                         
         BNE   BLDD46                                                           
         L     R2,ASUDTAB          SUBSTITUTE DISTAB (SAME DSECT)               
BLDD44   CLC   DISCOL,0(R1)        TEST CORRECT COLUMN                          
         BE    BLDD46                                                           
         LA    R2,DISTABL(R2)                                                   
         B     BLDD44                                                           
                                                                                
BLDD46   XR    R3,R3                                                            
         IC    R3,DISLEN                                                        
         CLC   DISLEN,DISLEN2      TAKE LARGEST ELEMENT LENGTH                  
         BNL   *+8                                                              
         IC    R3,DISLEN2                                                       
         AR    R3,RE               ADD DISPLACEMENT SO FAR                      
*MN                                                                             
         CLI   XACTOLAY,BROLAY                                                  
         BNE   BLDD46A                                                          
         CLM   R3,1,=AL1(L'D2SLLINE-1)                                          
         BNL   BLDD58              OVERFLOW - NO MORE COLUMNS                   
         B     BLDD46B                                                          
*MN                                                                             
BLDD46A  CLM   R3,1,=AL1(L'DISLLINE-1)                                          
         BNL   BLDD58              OVERFLOW - NO MORE COLUMNS                   
BLDD46B  CLI   FLAG,0              TEST FIRST TIME THROUGH                      
         BE    BLDD54              YES - DON'T SET DISPLACEMENTS                
         LA    R3,L'FVIHDR(RE)     ADD L'FLDHDR TO DISPLACEMENT SO FAR          
         STC   R3,DISDISP-1(R4)    SCREEN LINE DISPLACEMENT                     
         LA    R3,1(RE)            ADD ONE TO DISPLACEMENT SO FAR               
         STC   R3,REPDISP-1(R4)    REPORT LINE DISPLACEMENT                     
         IC    R3,DISLEN           LENGTH OF 1ST LINE                           
         BCTR  R3,0                                                             
         MVC   LARFADDR,DISADDR                                                 
         LA    RE,DISHEAD(RE)                                                   
         EX    0,LARF                                                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         ICM   R3,1,DISLEN2        TEST NO 2ND LINE                             
         BZ    BLDD47                                                           
         MVC   LARFADDR,DISADD2                                                 
         EX    0,LARF                                                           
         B     BLDD48                                                           
                                                                                
BLDD47   ICM   R3,1,DISLEN         UNDERLINE FIRST LINE                         
         BZ    BLDD50                                                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK,UNDERLN                                                     
         LA    RF,WORK                                                          
         STCM  RE,15,WORK+L'WORK-4                                              
                                                                                
         CLI   0(RE),C' '          INSERT SPACES AT START                       
         BNE   *+22                                                             
         MVI   0(RF),C' '                                                       
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R3,*-20                                                          
         DC    H'0'                                                             
                                                                                
         LA    RE,0(R3,RE)         INSERT SPACES AT END                         
         LA    RF,0(R3,RF)                                                      
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BNE   *+14                                                             
         MVI   0(RF),C' '                                                       
         BCT   R3,*-16                                                          
         DC    H'0'                                                             
                                                                                
         ICM   RE,15,WORK+L'WORK-4                                              
         IC    R3,DISLEN                                                        
         LA    RF,WORK                                                          
                                                                                
BLDD48   BCTR  R3,0                                                             
*&&UK                                                                           
         TM    SAFCIND1,SAFCI1SC   TEST SINGLE FOREIGN CURRENCY                 
         BNO   BLDD49                                                           
         TM    DISCIND,DISCIFCQ    TEST MAY BE FOREIGN CURRENCY                 
         BZ    BLDD49                                                           
         TM    DISCIND,DISCIPCQ    TEST USUALLY PRIMARY CURRENCY                
         BZ    *+12                                                             
         TM    SAFCIND1,SAFCI1ON   TEST CURRENCY TOTALS                         
         BNO   BLDD49                                                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SPACES                                                   
         LA    RF,WORK-L'CURTCUR-1(R3) RIGHT ALIGN CURRENCY CODE                
         MVI   0(RF),C'('                                                       
         MVC   1(L'CURTCUR,RF),FORECURT+(CURTCUR-CURTABD)                       
         MVI   L'CURTCUR+1(RF),C')'                                             
         LA    RF,WORK                                                          
                                                                                
BLDD49   OC    SCNDCURT,SCNDCURT   TEST SECONDARY CURRENCY IN USE               
         BZ    BLDD50                                                           
         TM    DISCIND,DISCIPCQ    TEST USUALLY PRIMARY CURRENCY                
         BO    BLDD49A                                                          
         TM    DISCIND,DISCI2CQ    TEST SECONDARY CURRENCY COLUMN               
         BZ    BLDD50                                                           
         EX    R3,*+8              SECONDARY CURRENCY COLUMN                    
         B     *+10                                                             
         MVC   WORK(0),SPACES                                                   
         LA    RF,WORK-L'CURTCUR-1(R3) RIGHT ALIGN CURRENCY CODE                
         MVI   0(RF),C'('                                                       
         MVC   1(L'CURTCUR,RF),SCNDCURT+(CURTCUR-CURTABD)                       
         MVI   L'CURTCUR+1(RF),C')'                                             
         LA    RF,WORK                                                          
         B     BLDD49B                                                          
                                                                                
BLDD49A  TM    SAFCIND1,SAFCI1SC   TEST SINGLE FOREIGN CURRENCY                 
         BO    BLDD49B                                                          
         TM    SAFCIND1,SAFCI1ON   TEST CURRENCY TOTALS                         
         BO    BLDD49B                                                          
         EX    R3,*+8              SHOW PRIMARY CURRENCY IN HEADING             
         B     *+10                                                             
         MVC   WORK(0),SPACES                                                   
         LA    RF,WORK-L'CURTCUR-1(R3) RIGHT ALIGN CURRENCY CODE                
         MVI   0(RF),C'('                                                       
         MVC   1(L'CURTCUR,RF),COMPCURT+(CURTCUR-CURTABD)                       
         MVI   L'CURTCUR+1(RF),C')'                                             
         LA    RF,WORK                                                          
                                                                                
BLDD49B  DS    0H                                                               
*&&                                                                             
BLDD50   DS    0H                                                               
         LTR   R3,R3                                                            
         BM    BLDD50C                                                          
*MN                                                                             
         CLI   XACTOLAY,BROLAY                                                  
         BE    BLDD50A                                                          
         EX    R3,*+8                                                           
         B     BLDD50C                                                          
         MVC   L'DISHEAD(0,RE),0(RF)                                            
                                                                                
BLDD50A  EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   L'D2SLLINE(0,RE),0(RF)                                           
*MN                                                                             
BLDD50C  LA    R3,1(R3)                                                         
         CLC   DISLEN,DISLEN2      TEST FIRST LINE LONGEST                      
         BNH   BLDD52                                                           
*        XR    RF,RF               RIGHT JUSTIFY 2ND LINE                       
*        IC    RF,DISLEN                                                        
*        SR    RF,R3                                                            
*        BNP   BLDD52                                                           
*        EX    RF,*+4                                                           
*        MVC   WORK(0),SPACES                                                   
*        LA    RF,WORK(RF)                                                      
*MN                                                                             
         CLI   XACTOLAY,BROLAY                                                  
         BE    BLDD51                                                           
                                                                                
*        EX    R3,*+4                                                           
*        MVC   0(0,RF),L'DISHEAD(RE)                                            
*        IC    R3,DISLEN                                                        
*        BCTR  R3,0                                                             
*        EX    R3,*+4                                                           
*        MVC   L'DISHEAD(0,RE),WORK                                             
         B     BLDD51A                                                          
                                                                                
BLDD51   EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),L'D2SLLINE(RE)                                           
         IC    R3,DISLEN                                                        
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   L'D2SLLINE(0,RE),WORK                                            
*MN                                                                             
BLDD51A  IC    R3,DISLEN                                                        
         CLC   DISLEN,DISLEN2                                                   
         BNL   *+8                                                              
         IC    R3,DISLEN2                                                       
BLDD52   LA    RF,DISHEAD                                                       
         SR    RE,RF                                                            
         LA    RE,1(RE,R3)         RE=DISPLACEMENT OF NEXT ELEMENT              
         A     RE,FULL2            ADD CONSTANT FACTOR FOR SPARE                
         B     BLDD56                                                           
                                                                                
BLDD54   LA    RE,1(R3)            RE=DISPLACEMENT OF NEXT ELEMENT              
         IC    RF,CHAR             TAKE NO. OF COLUMNS TO BE DISPLAYED          
         LA    RF,1(RF)                                                         
         STC   RF,CHAR             BUMP NO. OF COLUMNS TO BE DISPLAYED          
                                                                                
BLDD56   BCTR  R1,0                BUMP TO NEXT FIELD                           
         TM    SSCROLL,SSCROLLR    TEST RIGHT BUILD                             
         BO    *+8                                                              
         LA    R1,2(R1)                                                         
         BCT   R0,BLDD42           PERFORM FOR NUMBER OF PROFILES               
                                                                                
BLDD58   CLI   FLAG,0              TEST FIRST TIME THROUGH                      
         BNE   BLDDISX             NO - WE HAVE FINISHED                        
         MVI   FLAG,X'FF'          SET NOT FIRST TIME                           
                                                                                
         XR    RF,RF                                                            
         ICM   RF,1,CHAR           NUMBER OF COLUMNS DISPLAYABLE                
         BCTR  RF,0                                                             
         BNP   BLDDISX                                                          
                                                                                
BLDD60   XR    R0,R0                                                            
         TM    SSCROLL,SSCROLLR    TEST RIGHT DETERMINES LEFT                   
         BNO   BLDD61                                                           
         IC    R0,SSCRCOLL+1       DETERMINE NEW LEFT START                     
         SR    R0,RF                                                            
         BNM   *+6                                                              
         XR    R0,R0                                                            
         STC   R0,SSCRCOLL                                                      
                                                                                
BLDD61   IC    R0,SSCRCOLL         DETERMINE NEW RIGHT START                    
         AR    R0,RF                                                            
         STC   R0,SSCRCOLL+1                                                    
                                                                                
         TM    SSCROLL,SSCROLLR    TEST RIGHT DETERMINES LEFT                   
         BNO   BLDD66                                                           
         LA    R1,TEMP             DETERMINE LENGTH OF NEXT COLUMN              
         AR    R1,R0                                                            
         XR    R2,R2                                                            
         ICM   R2,1,1(R1)                                                       
         BZ    BLDD66                                                           
         BCTR  R2,0                                                             
         MH    R2,=Y(DISTABL)                                                   
         A     R2,ADISTAB                                                       
         LA    R2,DISTABL(R2)                                                   
         USING DISTABD,R2                                                       
         CLI   DISLEN,0                                                         
         BNE   BLDD64                                                           
         L     R2,ASUDTAB          USE SUBSTITUTE TABLE FOR LENGTH              
         B     *+8                                                              
BLDD62   LA    R2,DISTABL(R2)                                                   
         CLC   DISCOL,1(R1)                                                     
         BNE   BLDD62                                                           
BLDD64   IC    R0,DISLEN                                                        
         CLC   DISLEN,DISLEN2      TAKE LARGEST LENGTH                          
         BNL   *+8                                                              
         IC    R0,DISLEN2                                                       
         DROP  R2                                                               
                                                                                
         AR    R0,RE               TEST ROOM FOR ANOTHER COLUMN                 
*MN                                                                             
         CLI   XACTOLAY,BROLAY                                                  
         BNE   BLDD65                                                           
         CLM   R0,1,=AL1(L'D2SLLINE-1)                                          
         BNL   BLDD66                                                           
         B     BLDD65A                                                          
*MN                                                                             
BLDD65   CLM   R0,1,=AL1(L'DISLLINE-1)                                          
         BNL   BLDD66                                                           
BLDD65A  LR    RE,R0                                                            
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,CHAR                                                          
         B     BLDD60                                                           
                                                                                
BLDD66   XC    SSCROLL,SSCROLL                                                  
         XC    FULL2,FULL2                                                      
         CLI   CHAR,2              TEST TWO COLUMNS OR MORE                     
         BL    BLDD40                                                           
         LA    R1,L'DISLLINE+1     CALCULATE REMAINING SPACE                    
*MN                                                                             
         CLI   XACTOLAY,BROLAY                                                  
         BNE   *+8                                                              
         LA    R1,L'D2SLLINE+1     CALCULATE REMAINING SPACE                    
*MN                                                                             
         SR    R1,RE                                                            
         BNP   BLDD40                                                           
         XR    R0,R0               CALCULATE SPACE PER COLUMN                   
         IC    RF,CHAR                                                          
         DR    R0,RF                                                            
         ST    R1,FULL2                                                         
         B     BLDD40              SET DISPLACEMENTS                            
                                                                                
BLDDISX  B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD DISLINE FOR DISPLAY OR PRINT                                  *         
***********************************************************************         
                                                                                
         USING DISLINED,R2                                                      
BLDLIN   LR    R2,R1                                                            
         CLM   R2,8,=C'R'                 TEST USING REPDISP                    
         BNE   BLDL01                                                           
         XC    DISDISP(DISDISPL),REPDISP  SWAP REPDISP WITH DISDISP             
         XC    REPDISP(DISDISPL),DISDISP                                        
         XC    DISDISP(DISDISPL),REPDISP                                        
         B     BLDL05                                                           
                                                                                
BLDL01   DS    0H                                                               
*MN                                                                             
         CLI   XACTOLAY,BROLAY                                                  
         BNE   BLDL01H                                                          
         XC    D2SLLINE,D2SLLINE                                                
         OI    D2SLHDR1+(FVOIND-FVIHDR),FVOXMT                                  
         XC    D2SLMARK,D2SLMARK                                                
         OI    D2SLHDR2+(FVOIND-FVIHDR),FVOXMT                                  
         NI    D2SLHDR1+(FVATRB-FVIHDR),255-FVAHIGH                             
         NI    D2SLHDR2+(FVATRB-FVIHDR),255-FVAHIGH-FVAPROT                     
         TM    TSARINDS,TSARMKQ                                                 
         BNO   BLDL01A                                                          
         OI    D2SLHDR1+(FVATRB-FVIHDR),FVAHIGH                                 
         OI    D2SLHDR2+(FVATRB-FVIHDR),FVAHIGH                                 
         MVC   D2SLMARK,AC@YES     PRESET 'Y' IN MARK FIELD                     
         TM    TSARIND2,TSARGRSS   TEST TAKEN GROSS OF DISCOUNT                 
         BZ    *+10                                                             
         MVC   D2SLMARK,AC4GROSS   PRESET 'G' IN MARK FIELD                     
                                                                                
BLDL01A  CLI   TSARCHA,C' '        TEST OVERLAY SET DISPLAY CHARACTER           
         BNH   *+14                                                             
         MVC   D2SLMARK,TSARCHA    PLACE IT IN MARK FIELD                       
         OI    D2SLHDR2+(FVIIND-FVIHDR),FVIVAL  SET SYSTEM-GENERATED            
         TM    TSARIND2,TSARMMPI   TEST MULTIPLE MPYEL INVOICE                  
         BNO   *+8                                                              
         OI    TWAMODE2,TWAM2MUL   SET MULTIPLE MPYEL INVOICE BIT ON            
         TM    TSARINDS,TSARDISQ   TEST DISPLAY-ONLY TRANSACTION                
         BZ    *+8                                                              
         OI    D2SLHDR2+(FVATRB-FVIHDR),FVAPROT                                 
         TM    TWAMODE2,TWAM2DIS   TEST ALL RECORDS ARE DISPLAY ONLY            
         BNO   BLDL05                                                           
         OI    D2SLHDR2+(FVATRB-FVIHDR),FVAPROT                                 
         TM    TWAMODE2,TWAM2INV   TEST INVOICE-STYLE CHEQUE OVERLAY            
         BNO   BLDL05                                                           
         B     BLDL05                                                           
*MN                                                                             
BLDL01H  XC    DISLLINE,DISLLINE                                                
         OI    DISLHDR1+(FVOIND-FVIHDR),FVOXMT                                  
         XC    DISLMARK,DISLMARK                                                
         OI    DISLHDR2+(FVOIND-FVIHDR),FVOXMT                                  
         NI    DISLHDR1+(FVATRB-FVIHDR),255-FVAHIGH                             
         NI    DISLHDR2+(FVATRB-FVIHDR),255-FVAHIGH-FVAPROT                     
         TM    TSARINDS,TSARMKQ                                                 
         BNO   BLDL02                                                           
         OI    DISLHDR1+(FVATRB-FVIHDR),FVAHIGH                                 
         OI    DISLHDR2+(FVATRB-FVIHDR),FVAHIGH                                 
*&&UK                                                                           
         CLI   XACTOLAY,MROLAY     TEST MEDIA/RECONCILE                         
         BNE   *+12                                                             
         CLI   TSARXTYP,0          NO "Y" FOR SPECIALS                          
         BNE   *+10                                                             
*&&                                                                             
         MVC   DISLMARK,AC@YES     PRESET 'Y' IN MARK FIELD                     
         TM    TSARIND2,TSARGRSS   TEST TAKEN GROSS OF DISCOUNT                 
         BZ    *+10                                                             
         MVC   DISLMARK,AC4GROSS   PRESET 'G' IN MARK FIELD                     
                                                                                
BLDL02   CLI   TSARCHA,C' '        TEST OVERLAY SET DISPLAY CHARACTER           
         BNH   *+14                                                             
         MVC   DISLMARK,TSARCHA    PLACE IT IN MARK FIELD                       
         OI    DISLHDR2+(FVIIND-FVIHDR),FVIVAL  SET SYSTEM-GENERATED            
         TM    TSARIND2,TSARMMPI   TEST MULTIPLE MPYEL INVOICE                  
         BNO   *+8                                                              
         OI    TWAMODE2,TWAM2MUL   SET MULTIPLE MPYEL INVOICE BIT ON            
         TM    TSARINDS,TSARDISQ   TEST DISPLAY-ONLY TRANSACTION                
         BZ    *+8                                                              
         OI    DISLHDR2+(FVATRB-FVIHDR),FVAPROT                                 
         TM    TWAMODE2,TWAM2DIS   TEST ALL RECORDS ARE DISPLAY ONLY            
         BNO   BLDL05                                                           
         OI    DISLHDR2+(FVATRB-FVIHDR),FVAPROT                                 
         TM    TWAMODE2,TWAM2INV   TEST INVOICE-STYLE CHEQUE OVERLAY            
         BNO   BLDL05                                                           
*&&UK                                                                           
         CP    TSARFREM,PZERO      TEST FULLY MATCHED                           
         BE    BLDL05              YES                                          
         CP    TSARAMNT,TSARFREM   TEST PART MATCHED                            
         BE    BLDL05              NO (UNMATCHED)                               
         BH    *+6                 YES                                          
         DC    H'0'                REMAINDER CANNOT EXCEED AMOUNT               
         MVC   DISLMARK,AC@NO      SET "NO" FOR UNMATCHING                      
*&&                                                                             
BLDL05   MVI   FLAG,0              SET RECORD NOT READ                          
         XR    RE,RE               DISPLAY OFFICE                               
         ICM   RE,1,DISOFFD                                                     
         BZ    BLDL06                                                           
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TSAROFF,RF),TSAROFF                                          
                                                                                
BLDL06   XR    RE,RE               DISPLAY WORKCODE (PRODUCTION)                
         ICM   RE,1,DISWRKD                                                     
         BZ    BLDL07                                                           
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TSAROFF,RF),TSAROFF                                          
                                                                                
BLDL07   XR    RE,RE               DISPLAY CONTRA                               
         ICM   RE,1,DISCOND                                                     
         BZ    BLDL12                                                           
*&&UK                                                                           
         CLI   XACTOLAY,MROLAY     TEST MEDIA/RECONCILE                         
         BNE   *+14                                                             
         CLC   TSARSUPN,SPACES                                                  
         BH    *+14                                                             
*&&                                                                             
         CLC   TSARCON,SPACES                                                   
         BNH   BLDL12                                                           
         LA    RF,DISLINED(RE)                                                  
*&&UK                                                                           
         CLI   XACTOLAY,MROLAY     TEST MEDIA/RECONCILE                         
         BNE   BLDL08                                                           
         CLC   TSARSUPN,SPACES     TEST SUPPLIER SHORT NAME                     
         BNH   BLDL08                                                           
         MVC   0(L'TSARSUPN,RF),TSARSUPN                                        
         MVC   L'TSARSUPN+1(L'TSARCCLI,RF),TSARCCLI                             
         B     BLDL12                                                           
*&&                                                                             
BLDL08   LA    RE,L'TSARCON-2                                                   
         LA    R1,TSARCON+1                                                     
         CLI   0(R1),C' '          SCAN FOR FIRST NON-BLANK                     
         BH    *+14                GOT IT                                       
         LA    R1,1(R1)                                                         
         BCT   RE,*-12                                                          
         DC    H'0'                TSARCON IS BAD                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R1)                                                    
                                                                                
BLDL12   XR    RE,RE               DISPLAY REFERENCE                            
         ICM   RE,1,DISREFD                                                     
         BZ    BLDL14                                                           
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TSARREF,RF),TSARREF                                          
                                                                                
BLDL14   XR    RE,RE               DISPLAY MOS                                  
         ICM   RE,1,DISMOSD                                                     
         BZ    BLDL16                                                           
         OC    TSARMOS,TSARMOS                                                  
         BZ    BLDL16                                                           
         LA    RF,DISLINED(RE)                                                  
         MVC   WORK(L'TSARMOS),TSARMOS                                          
         MVI   WORK+L'TSARMOS,X'01'                                             
         GOTO1 VDATCON,DMCB,(1,WORK),(9,(RF))                                   
                                                                                
BLDL16   XR    RE,RE               DISPLAY BATCH REF                            
         ICM   RE,1,DISBATD                                                     
         BZ    BLDL18                                                           
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TSARBAT,RF),TSARBAT                                          
                                                                                
BLDL18   XR    RE,RE               DISPLAY SUPPLIER                             
         ICM   RE,1,DISSUPD                                                     
         BZ    BLDL20                                                           
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TSARCON-1,RF),TSARCON+1                                      
                                                                                
BLDL20   XR    RE,RE               DISPLAY JOB/ESTIMATE                         
         ICM   RE,1,DISJOBD                                                     
         BZ    BLDL22                                                           
         LA    RF,DISLINED(RE)                                                  
*&&US                                                                           
         TM    TSARIND2,TSARJEST   TEST SOURCE IS GENUINE JOB/ESTIMATE          
         BZ    BLDL22                                                           
         TM    ACCIND1,ACCISPOT+ACCIPRNT                                        
         BZ    *+14                                                             
         MVC   0(L'TSARFSAC,RF),TSARFSAC                                        
         B     BLDL22                                                           
         TM    ACCIND1,ACCIPROD                                                 
         BZ    BLDL22                                                           
*&&                                                                             
         CLC   PRODUL,TSARFSAC     TEST PRODUCTION SOURCE A/C                   
         BNE   BLDL22                                                           
         MVC   0(L'TSARFSAC-L'PRODUL,RF),TSARFSAC+L'PRODUL                      
                                                                                
BLDL22   XR    RE,RE               DISPLAY SOURCE ACCOUNT                       
         ICM   RE,1,DISSRCD                                                     
         BZ    BLDL24                                                           
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TSARFSAC,RF),TSARFSAC                                        
                                                                                
BLDL24   XR    RE,RE               DISPLAY CHEQUE NUMBER                        
         ICM   RE,1,DISCHQD                                                     
         BZ    BLDL26                                                           
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TSARREF,RF),TSARREF    USE REFERENCE                         
         CLI   XACTOLAY,CCOLAY            TEST CREDITOR/CHEQUE                  
         BNE   BLDL26                                                           
*&&UK                                                                           
         CLC   TSARFCHN,SPACES            TEST MPYEL CHEQUE NO. PRESENT         
         BNH   BLDL26                                                           
         MVC   0(L'TSARFCHN,RF),TSARFCHN  YES - USE THAT INSTEAD                
*&&                                                                             
BLDL26   XR    RE,RE               DISPLAY TRANSACTION DATE                     
         ICM   RE,1,DISDATD                                                     
         BZ    BLDL28                                                           
         OC    TSARDAT,TSARDAT                                                  
         BZ    BLDL28                                                           
         LA    RF,DISLINED(RE)                                                  
         GOTO1 VDATCON,DMCB,(1,TSARDAT),(17,(RF))                               
                                                                                
BLDL28   XR    RE,RE               DISPLAY TRANSACTION TYPE (DR/CR)             
         ICM   RE,1,DISTYPD                                                     
         BZ    BLDL30                                                           
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'AC@DR,RF),AC@DR                                              
         TM    TSARINDS,TSARDRQ                                                 
         BO    BLDL30                                                           
         MVC   0(L'AC@DR,RF),SPACES                                             
         MVC   0(L'AC@CR,RF),AC@CR                                              
                                                                                
BLDL30   XR    RE,RE               DISPLAY DUE DATE                             
         ICM   RE,1,DISDUED                                                     
         BZ    BLDL32                                                           
         MVC   HALF,TSARFDUE                                                    
*        CLI   TWASTYP,TYPCRD      IF CREDITOR CONTRA USE TARFDU2               
*        BNE   BLDL31                                                           
*        CLI   TWASACT,ACTOFFS     BECAUSE TSARFDUE FILLED WITH USED DT         
*        BNE   BLDL31                                                           
*        MVC   HALF,TSARFDU2                                                    
BLDL31   OC    HALF,HALF           DUE DATE FOUND?                              
         BZ    BLDL32                                                           
         LA    RF,DISLINED(RE)                                                  
         GOTO1 VDATCON,DMCB,(2,HALF),(17,(RF))                                  
                                                                                
BLDL32   XR    RE,RE               DISPLAY SYSTEM SEQUENCE NUMBER               
         ICM   RE,1,DISSEQD                                                     
         BZ    BLDL34                                                           
         LA    RF,DISLINED(RE)                                                  
         IC    RE,TSARSBR                                                       
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(L'LC3SUBR,RF),DUB                                              
                                                                                
BLDL34   XR    RE,RE               DISPLAY USED DATE                            
         ICM   RE,1,DISUSED                                                     
         BZ    BLDL36                                                           
         OC    TSARUSDT,TSARUSDT                                                
         BZ    BLDL36                                                           
         LA    RF,DISLINED(RE)                                                  
         GOTO1 VDATCON,DMCB,(2,TSARUSDT),(17,(RF))                              
                                                                                
BLDL36   XR    RE,RE               DISPLAY TSARFREE W/C (CPJEL)                 
         ICM   RE,1,DISFWKD                                                     
         BZ    BLDL38                                                           
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TSARFWRK,RF),TSARFWRK                                        
                                                                                
BLDL38   XR    RE,RE               DISPLAY TSARFREE NUMBER (OTHEL)              
         ICM   RE,1,DISFOTD                                                     
         BZ    BLDL40                                                           
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TSARFOTH,RF),TSARFOTH                                        
                                                                                
BLDL40   XR    RE,RE               DISPLAY TSAR RECORD NUMBER                   
         ICM   RE,1,DISNUMD                                                     
         BZ    BLDL42                                                           
         LA    RF,DISLINED(RE)                                                  
         L     RE,ATSARBLK                                                      
         MVC   HALF,TSRNUM-TSARD(RE)                                            
         CURED HALF,(L'LC@COUNT,(RF)),0,ALIGN=LEFT                              
                                                                                
BLDL42   XR    RE,RE               DISPLAY DISCOUNT AMOUNT                      
         ICM   RE,1,DISDSCD                                                     
         BZ    BLDL44                                                           
         OC    TSARFDIS,TSARFDIS                                                
         BZ    BLDL44                                                           
         CLI   XACTION,ACTVOID     TEST VOID                                    
         BNE   *+12                                                             
         CLI   FILTCHQ,INCLUDE     CHEQUE MODE DISCOUNT NOT AVAILABLE           
         BE    BLDL44                                                           
         LA    RF,DISLINED(RE)                                                  
*&&UK                                                                           
         TM    SAFCIND1,SAFCI1SC+SAFCI1ON                                       
         BNO   BLDL43                                                           
         OC    TSARFDIC,TSARFDIC                                                
         BZ    BLDL43                                                           
         CURED TSARFDIC,(L'LC@DSCR,(RF)),FORECURT,MINUS=YES                     
         B     BLDL44                                                           
BLDL43   CURED TSARFDIS,(L'LC@DSCR,(RF)),COMPCURT,MINUS=YES                     
         CLM   R2,8,=C'R'          TEST THIS IS A REPORT BEING PRINTED          
         BE    BLDL44              DON'T FIX SCREEN HEADLINE TWO                
         XR    RE,RE                                                            
         ICM   RE,1,DISDSCD        MAKE SURE NO CURRENCY SYMBOLS                
         L     RF,ADISHEA2                                                      
         LA    RF,0(RE,RF)                                                      
         MVC   0(L'LC@DSCR,RF),UNDERLN                                          
         L     R1,ADISHEAD                                                      
         LA    R1,0(RE,R1)                                                      
BLDL43A  CLC   0(1,RF),UNDERLN                                                  
         BNE   BLDL44                                                           
         CLI   0(R1),C' '                                                       
         BNE   BLDL44                                                           
         MVI   0(RF),C' '                                                       
         LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         B     BLDL43A                                                          
*&&                                                                             
*&&US                                                                           
BLDL43   CURED TSARFDIS,(L'LC@DSCR,(RF)),2,MINUS=YES                            
*&&                                                                             
                                                                                
BLDL44   XR    RE,RE               DISPLAY OFFSET MOS                           
         ICM   RE,1,DISOMOD                                                     
         BZ    BLDL46                                                           
         OC    TSARFUSE,TSARFUSE                                                
         BZ    BLDL46                                                           
         LA    RF,DISLINED(RE)                                                  
         MVC   WORK(L'TSARFUSE),TSARFUSE                                        
         MVI   WORK+L'TSARFUSE,X'01'                                            
         GOTO1 VDATCON,DMCB,(1,WORK),(9,(RF))                                   
                                                                                
BLDL46   XR    RE,RE               DISPLAY MARKED STATUS                        
         ICM   RE,1,DISSTAD                                                     
         BZ    BLDL50                                                           
         LA    RF,DISLINED(RE)                                                  
         TM    TSARINDS,TSARASTQ   TEST ALTERNATIVE STATUS CHANGE BASIS         
         BNO   BLDL48                                                           
         MVC   0(1,RF),TSARAST                                                  
         MVC   1(1,RF),TSARCHA                                                  
         B     BLDL50                                                           
                                                                                
BLDL48   MVC   0(1,RF),AC@NO                                                    
         TM    TSARINDS,TSARINMQ   TEST INCOMING RECORD MARKED                  
         BNO   *+10                                                             
         MVC   0(1,RF),AC@YES                                                   
         MVC   1(1,RF),AC@NO                                                    
         TM    TSARINDS,TSARMKQ    TEST CURRENTLY MARKED                        
         BNO   *+10                                                             
         MVC   1(1,RF),AC@YES                                                   
         TM    TWAMODE2,TWAM2INV   TEST INVOICE-STYLE CHEQUE OVERLAY            
         BNO   BLDL50                                                           
         TM    TSARINDS,TSARMCHQ   TEST CHEQUE                                  
         BNO   BLDL50                                                           
*&&UK                                                                           
         CP    TSARFREM,PZERO      TEST FULLY MATCHED                           
         BE    BLDL50                                                           
         CP    TSARAMNT,TSARFREM   TEST PART MATCHED                            
         BE    BLDL50              NO (UNMATCHED)                               
         BH    *+6                 YES                                          
         DC    H'0'                REMAINDER CANNOT EXCEED AMOUNT               
         MVC   1(1,RF),AC@PENDG                                                 
*&&                                                                             
BLDL50   XR    RE,RE               DISPLAY DISK ADDRESS                         
         ICM   RE,1,DISDADD                                                     
         BZ    BLDL52                                                           
         LA    RF,DISLINED(RE)                                                  
         GOTO1 VHEXOUT,DMCB,TSARDADR,(RF),L'TSARDADR,0                          
                                                                                
BLDL52   XR    RE,RE               DISPLAY ACTIVITY DATE                        
         ICM   RE,1,DISADAD                                                     
         BZ    BLDL54                                                           
         OC    TSARADAT,TSARADAT                                                
         BZ    BLDL54                                                           
         LA    RF,DISLINED(RE)                                                  
         GOTO1 VDATCON,DMCB,(2,TSARADAT),(17,(RF))                              
                                                                                
BLDL54   XR    RE,RE               DISPLAY TRANS STATUS IN HEX                  
         ICM   RE,1,DISHSTD                                                     
         BZ    BLDL56                                                           
         LA    RF,DISLINED(RE)                                                  
         GOTO1 VHEXOUT,DMCB,TSARSTA,(RF),L'TSARSTA,0                            
                                                                                
BLDL56   XR    RE,RE               DISPLAY BATCH TYPE                           
         ICM   RE,1,DISBTYD                                                     
         BZ    BLDL58                                                           
         LA    RF,DISLINED(RE)                                                  
         MVI   HALF,0                                                           
         MVC   HALF+1(1),TSARBTY                                                
         CURED HALF,(L'LC3TYPE,(RF)),0,ALIGN=LEFT                               
                                                                                
BLDL58   XR    RE,RE               DISPLAY REVERSED MOS                         
         ICM   RE,1,DISRMOD                                                     
         BZ    BLDL60                                                           
         OC    TSARFREV,TSARFREV                                                
         BZ    BLDL60                                                           
         LA    RF,DISLINED(RE)                                                  
         MVC   WORK(L'TSARFREV),TSARFREV                                        
         MVI   WORK+L'TSARFREV,X'01'                                            
         GOTO1 VDATCON,DMCB,(1,WORK),(9,(RF))                                   
                                                                                
BLDL60   XR    RE,RE               DISPLAY AMOUNT                               
         ICM   RE,1,DISAMTD                                                     
         BZ    BLDL70                                                           
         LA    R3,DISLINED(RE)     R3=A(DESTINATION OF AMOUNT)                  
         ZAP   DUB,TSARAMNT                                                     
         TM    TWAMODE2,TWAM2INV   TEST INVOICE-STYLE CHEQUE OVERLAY            
         BNO   BLDL68                                                           
         TM    TSARINDS,TSARMCHQ   TEST CHEQUE                                  
         BNO   BLDL68                                                           
*&&UK                                                                           
         ZAP   DUB,TSARFREM        USE REMAINDER                                
         CLM   R2,8,=C'R'          TEST THIS IS A REPORT BEING PRINTED          
         BNE   BLDL62                                                           
         OC    FILTCH1,FILTCH1     TEST MATCHING AN INVOICE                     
         BNZ   BLDL64                                                           
         B     BLDL68                                                           
BLDL62   TM    TWAMODE2,TWAM2DIS   TEST UNMATCHING AN INVOICE                   
         BZ    BLDL68                                                           
BLDL64   LA    R0,CHQMAX                                                        
         LA    R1,FILTAM1                                                       
         LA    RE,FILTCH1                                                       
         L     RF,ATSARBLK                                                      
BLDL66   CLC   0(L'FILTCH1,RE),TSRNUM-TSARD(RF)                                 
         BNE   *+14                                                             
         ZAP   DUB,0(L'FILTAM1,R1)                                              
         B     BLDL68                                                           
         LA    RE,L'FILTCH1(RE)                                                 
         LA    R1,L'FILTAM1(R1)                                                 
         BCT   R0,BLDL66                                                        
         DC    H'0'                                                             
*&&                                                                             
BLDL68   CURED DUB,(L'AC@AMT,(R3)),2,MINUS=YES                                  
         TM    TWAMODE2,TWAM2INV   TEST INVOICE-STYLE CHEQUE OVERLAY            
         BNO   BLDL70                                                           
         TM    TSARINDS,TSARMCHQ   TEST CHEQUE                                  
         BNO   BLDL70                                                           
*&&UK                                                                           
         CP    TSARFREM,PZERO      TEST FULLY MATCHED                           
         BE    BLDL70              YES                                          
         CP    TSARAMNT,TSARFREM   TEST PART MATCHED                            
         BE    BLDL70              NO (UNMATCHED)                               
         BH    *+6                 YES                                          
         DC    H'0'                REMAINDER CANNOT EXCEED AMOUNT               
         MVI   L'AC@AMT-1(R3),C'*' USE SIGN POSITION, AS AMOUNT IS +IVE         
*&&                                                                             
BLDL70   XR    RE,RE               DISPLAY DEBIT                                
         ICM   RE,1,DISDRSD                                                     
         BZ    BLDL80                                                           
         TM    TSARINDS,TSARDRQ    TEST DEBIT                                   
         BZ    BLDL80                                                           
         TM    TSARIND3,TSARPWOI+TSARDIFF                                       
         BNZ   BLDL80              NOT NEW POSTINGS                             
         LA    R3,DISLINED(RE)     R3=A(DESTINATION OF AMOUNT)                  
         ZAP   DUB,TSARAMNT                                                     
         TM    TWAMODE2,TWAM2INV   TEST INVOICE-STYLE CHEQUE OVERLAY            
         BNO   BLDL78                                                           
         TM    TSARINDS,TSARMCHQ   TEST CHEQUE                                  
         BNO   BLDL78                                                           
*&&UK                                                                           
         ZAP   DUB,TSARFREM        USE REMAINDER                                
         CLM   R2,8,=C'R'          TEST THIS IS A REPORT BEING PRINTED          
         BNE   BLDL72                                                           
         OC    FILTCH1,FILTCH1     TEST MATCHING AN INVOICE                     
         BNZ   BLDL74                                                           
         B     BLDL78                                                           
BLDL72   TM    TWAMODE2,TWAM2DIS   TEST UNMATCHING AN INVOICE                   
         BZ    BLDL78                                                           
BLDL74   LA    R0,CHQMAX                                                        
         LA    R1,FILTAM1                                                       
         LA    RE,FILTCH1                                                       
         L     RF,ATSARBLK                                                      
BLDL76   CLC   0(L'FILTCH1,RE),TSRNUM-TSARD(RF)                                 
         BNE   *+14                                                             
         ZAP   DUB,0(L'FILTAM1,R1)                                              
         B     BLDL78                                                           
         LA    RE,L'FILTCH1(RE)                                                 
         LA    R1,L'FILTAM1(R1)                                                 
         BCT   R0,BLDL76                                                        
         DC    H'0'                                                             
*&&                                                                             
BLDL78   CURED DUB,(L'AC@DRS,(R3)),2,MINUS=YES                                  
         TM    TWAMODE2,TWAM2INV   TEST INVOICE-STYLE CHEQUE OVERLAY            
         BNO   BLDL80                                                           
         TM    TSARINDS,TSARMCHQ   TEST CHEQUE                                  
         BNO   BLDL80                                                           
*&&UK                                                                           
         CP    TSARFREM,PZERO      TEST FULLY MATCHED                           
         BE    BLDL80              YES                                          
         CP    TSARAMNT,TSARFREM   TEST PART MATCHED                            
         BE    BLDL80              NO (UNMATCHED)                               
         BH    *+6                 YES                                          
         DC    H'0'                REMAINDER CANNOT EXCEED AMOUNT               
         MVI   L'AC@DRS-1(R3),C'*' USE SIGN POSITION, AS AMOUNT IS +IVE         
*&&                                                                             
BLDL80   XR    RE,RE               DISPLAY CREDIT                               
         ICM   RE,1,DISCRSD                                                     
         BZ    BLDL90                                                           
         TM    TSARINDS,TSARDRQ    TEST CREDIT                                  
         BO    BLDL90                                                           
         TM    TSARIND3,TSARPWOI+TSARDIFF                                       
         BNZ   BLDL90              NOT NEW POSTINGS                             
         LA    R3,DISLINED(RE)     R3=A(DESTINATION OF AMOUNT)                  
         ZAP   DUB,TSARAMNT                                                     
         TM    TWAMODE2,TWAM2INV   TEST INVOICE-STYLE CHEQUE OVERLAY            
         BNO   BLDL88                                                           
         TM    TSARINDS,TSARMCHQ   TEST CHEQUE                                  
         BNO   BLDL88                                                           
*&&UK                                                                           
         ZAP   DUB,TSARFREM        USE REMAINDER                                
         CLM   R2,8,=C'R'          TEST THIS IS A REPORT BEING PRINTED          
         BNE   BLDL82                                                           
         OC    FILTCH1,FILTCH1     TEST UNMATCHING AN INVOICE                   
         BNZ   BLDL84                                                           
         B     BLDL88                                                           
BLDL82   TM    TWAMODE2,TWAM2DIS   TEST UNMATCHING AN INVOICE                   
         BZ    BLDL88                                                           
BLDL84   LA    R0,CHQMAX                                                        
         LA    R1,FILTAM1                                                       
         LA    RE,FILTCH1                                                       
         L     RF,ATSARBLK                                                      
BLDL86   CLC   0(L'FILTCH1,RE),TSRNUM-TSARD(RF)                                 
         BNE   *+14                                                             
         ZAP   DUB,0(L'FILTAM1,R1)                                              
         B     BLDL88                                                           
         LA    RE,L'FILTCH1(RE)                                                 
         LA    R1,L'FILTAM1(R1)                                                 
         BCT   R0,BLDL86                                                        
         DC    H'0'                                                             
*&&                                                                             
BLDL88   CURED DUB,(L'AC@CRS,(R3)),2,MINUS=YES                                  
         TM    TWAMODE2,TWAM2INV   TEST INVOICE-STYLE CHEQUE OVERLAY            
         BNO   BLDL90                                                           
         TM    TSARINDS,TSARMCHQ   TEST CHEQUE                                  
         BNO   BLDL90                                                           
*&&UK                                                                           
         CP    TSARFREM,PZERO      TEST FULLY MATCHED                           
         BE    BLDL90              YES                                          
         CP    TSARAMNT,TSARFREM   TEST PART MATCHED                            
         BE    BLDL90              NO (UNMATCHED)                               
         BH    *+6                 YES                                          
         DC    H'0'                REMAINDER CANNOT EXCEED AMOUNT               
         MVI   L'AC@DRS-1(R3),C'*' USE SIGN POSITION, AS AMOUNT IS +IVE         
*&&                                                                             
BLDL90   XR    RE,RE                                                            
*&&UK                                                                           
         ICM   RE,1,DISPRFD        DISPLAY PAYMENT REFERENCE                    
         BZ    BLDL92                                                           
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TSARFDIS,RF),TSARFDIS                                        
*&&                                                                             
         ICM   RE,1,DISINVD        DISPLAY MEDIA/LONG INVOICE NUMBER            
         BZ    BLDL92                                                           
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TSARFINV,RF),TSARFINV                                        
         CLC   0(L'TSARFINV,RF),SPACES                                          
         BH    *+10                                                             
         MVC   0(L'TSARREF,RF),TSARREF  USE REFERENCE IF NO INVOICE#            
                                                                                
BLDL92   XR    RE,RE               DISPLAY EXTRA STATUS DETAIL                  
         ICM   RE,1,DISXSTD                                                     
         BZ    BLDL98                                                           
         LA    RF,DISLINED(RE)                                                  
         MVI   0(RF),C'.'                                                       
         MVC   1(L'LC4STAT-1,RF),0(RF)                                          
*&&UK                                                                           
         CLC   TSARAFCC,SPACES     TEST ASSOCIATED FOREIGN CURRENCY             
         BNH   *+10                                                             
         MVC   0(1,RF),AC3INTNL    INTERNATIONAL CURRENCY TRANSACTION           
         TM    TSARSTA,TRNSAUTH    TEST AUTHORISED                              
         BZ    *+10                                                             
         MVC   1(1,RF),AC3AUTH     AUTH'D                                       
*&&                                                                             
         TM    TSARSTA,TRNSHOLD                                                 
         BZ    *+10                                                             
         MVC   1(1,RF),AC3HOLD     HELD                                         
         TM    TSARSTA,TRNSAPPR    SELECTED/RECONCILED/MATCHED                  
         BZ    BLDL96                                                           
         MVC   1(1,RF),AC3RECN     RECONCILED                                   
         CLI   XTYPE,TYPBNK        TEST BANK FUNCTION                           
         BNE   BLDL94                                                           
         CLI   XACTION,ACTVOID     BUT IF VOID                                  
         BNE   BLDL96                                                           
         CLI   FILTCHQ,INCLUDE     ENSURE CHEQUE MODE                           
         BE    BLDL96                                                           
         MVC   1(1,RF),OP3SEL      ELSE SELECTED                                
         B     BLDL96                                                           
BLDL94   CLI   XTYPE,TYPGEN        TEST GENERAL FUNCTION ON BANK A/C            
         BNE   *+14                                                             
         CLC   BANKUL,ACCOUNT+(ACTKUNT-ACTRECD)                                 
         BE    BLDL96              RECONCILED IS CORRECT STATUS                 
         MVC   1(1,RF),OP3SEL                                                   
         CLI   XTYPE,TYPCRD        TEST CREDITOR FUNCTION                       
         BE    BLDL96                                                           
         TM    ACCINDS,ACCISVLS    TEST VENDOR ACCOUNT                          
         BNZ   BLDL96                                                           
         MVC   1(1,RF),OP3MAT      ELSE MATCHED                                 
BLDL96   TM    TSARSSTA,TRSSVOID                                                
         BNO   *+10                                                             
         MVC   2(1,RF),AC3VOID     VOID                                         
         TM    TSARSTA,TRNSNOCM                                                 
         BZ    *+10                                                             
         MVC   2(1,RF),AC@NO       NON-COMMISSIONABLE                           
         TM    TSARIND2,TSARMEMO   MEMO ITEMS ATTACHED                          
         BZ    *+10                                                             
         MVC   3(1,RF),AC3MEMO     MEMO                                         
                                                                                
BLDL98   XR    RE,RE               DISPLAY BANK STATEMENT DATE                  
         ICM   RE,1,DISBSDD                                                     
*MN      BZ    BLDL100                                                          
         BZ    BLDL99                                                           
         LA    RF,DISLINED(RE)                                                  
         GOTO1 VDATCON,DMCB,(2,TSARBSDT),(17,(RF))                              
*MN                                                                             
BLDL99   XR    RE,RE               DISPLAY BANK RECONCILED DATE                 
         ICM   RE,1,DISRCDT                                                     
         BZ    BLDL99A                                                          
         LA    RF,DISLINED(RE)                                                  
         CLC   TSARRCDT,SPACES                                                  
         BE    BLDL99A                                                          
         OC    TSARRCDT,TSARRCDT                                                
         BZ    BLDL99A                                                          
         GOTO1 VDATCON,DMCB,(0,TSARRCDT),(17,(RF))                              
                                                                                
BLDL99A  XR    RE,RE               DISPLAY BANK RECONCILED DATE                 
         ICM   RE,1,DISCLDT                                                     
         BZ    BLDL100                                                          
         LA    RF,DISLINED(RE)                                                  
         CLC   TSARCLDT,SPACES                                                  
         BE    BLDL100                                                          
         OC    TSARCLDT,TSARCLDT                                                
         BZ    BLDL100                                                          
         GOTO1 VDATCON,DMCB,(0,TSARCLDT),(17,(RF))                              
*MN                                                                             
BLDL100  XR    RE,RE                                                            
*&&US                                                                           
         ICM   RE,1,DISMMDD        MEDIA MONTH/DATE                             
         BZ    BLDL102                                                          
         OC    TSARFMMD,TSARFMMD   CHECK MEDIA MONTH/DATE PRESENT               
         BZ    BLDL102                                                          
         LA    RF,DISLINED(RE)                                                  
         MVC   WORK(L'TSARFMMD),TSARFMMD                                        
         LA    R0,17               SET FOR MMMDD/YY OUTPUT                      
         CLI   WORK+L'TSARFMMD-1,X'00'                                          
         BNE   *+12                                                             
         LA    R0,9                SET FOR MMM/YY OUTPUT                        
         MVI   WORK+L'TSARFMMD-1,X'01'                                          
         GOTO1 VDATCON,DMCB,(1,WORK),((R0),(RF))                                
*&&                                                                             
*&&UK                                                                           
         ICM   RE,1,DISSERD        BUY SERIAL NUMBER                            
         BZ    BLDL102                                                          
         LA    RF,DISLINED(RE)                                                  
         OC    TSARFSER,TSARFSER   TEST NEW MISSING BUY                         
         BNZ   *+14                                                             
         MVC   0(L'AC@MSNG,RF),AC@MSNG                                          
         B     BLDL102                                                          
         MVC   0(L'MRHSERA,RF),TSARFSER                                         
         XR    R1,R1                                                            
         ICM   R1,7,TSARFSER+(MRHSERN-MRHSER)                                   
         CLI   TSARFSER,MRHPSMIS   TEST POSTED MISSING BUY                      
         BNE   *+14                                                             
         MVI   0(RF),C'#'          INDICATE MISSING                             
         ICM   R1,15,TSARFSER      TAKE THE LOT                                 
         LNR   R1,R1               REVERSE THE VALUE                            
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  L'MRHSERA(L'LC@SERNO-L'MRHSERA,RF),DUB                           
*&&                                                                             
BLDL102  XR    RE,RE                                                            
*&&US                                                                           
         ICM   RE,1,DISTAXD        TAX/BASIS                                    
         BZ    BLDL104                                                          
         LA    RF,DISLINED(RE)                                                  
         OC    TSARFTAX,TSARFTAX                                                
         BZ    BLDL104                                                          
         CURED TSARFTAX,(L'LC@TAXBS,(RF)),2,MINUS=YES                           
*&&                                                                             
*&&UK                                                                           
         ICM   RE,1,DISUNPD        UNIT PRICE                                   
         BZ    BLDL104                                                          
         OC    TSARFUNP,TSARFUNP                                                
         BZ    BLDL104                                                          
         LA    RF,DISLINED(RE)                                                  
         CURED TSARFUNP,(L'LC@UNPR,(RF)),2,MINUS=YES                            
*&&                                                                             
BLDL104  OC    DISNARD,DISNARD     TRANSACTION NARRATIVE                        
         BZ    BLDL120                                                          
*        CLI   FILEFORM,VLISQ      TEST OLD FILE                                
*        BE    BLDL120                                                          
         OC    TSARDADR,TSARDADR                                                
         BZ    BLDL114                                                          
         MVC   IODA,TSARDADR       SET DISK ADDRESS                             
         LA    R1,IOGET+IOACCMST+IO1Q  GETRUP BUILDS KEY ON OLD FILE            
         TM    TSARRSTA,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOBUFF                                                       
         OI    FLAG,X'80'          SET RECORD READ                              
         LA    R1,TRNRFST-TRNRECD(R1)                                           
         USING TRNELD,R1                                                        
         XR    RE,RE                                                            
BLDL106  CLI   TRNEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   TRNEL,TRNELQ                                                     
         BE    BLDL108                                                          
         IC    RE,TRNLN                                                         
         AR    R1,RE                                                            
         B     BLDL106                                                          
                                                                                
BLDL108  XR    RF,RF                                                            
         IC    RF,DISNARD                                                       
         LA    RF,DISLINED(RF)                                                  
         MVC   0(L'LC@NRTV,RF),SPACES                                           
         IC    RE,TRNLN                                                         
         SH    RE,=Y(TRNLN1Q+1)                                                 
         BM    BLDL112                                                          
         CLM   RE,1,=AL1(L'LC@NRTV-1)                                           
         BNH   BLDL110             ALL NARRATIVE FITS                           
         LA    RE,L'LC@NRTV-1                                                   
*        LA    RF,TRNNARR+L'LC@NRTV                                             
*        CLI   0(RF),C' '          TEST STRING CONTINUES                        
*        BNH   BLDL110             BREAK FOUND IN TEXT                          
*        BCTR  RF,0                                                             
*        BCT   RE,*-10             LOOK BACKWARDS FOR A BREAK                   
*        LA    RE,L'LC@NRTV-1      NO BREAK FOUND IN NARRATIVE                  
                                                                                
BLDL110  XR    RF,RF                                                            
         IC    RF,DISNARD                                                       
         LA    RF,DISLINED(RF)                                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),TRNNARR     EXTRACT NARRATIVE                            
         DROP  R1                                                               
                                                                                
BLDL112  DS    0H                                                               
*&&UK                                                                           
         USING MRHELD,R1                                                        
         XR    RE,RE               SEEK MRHEL                                   
         IC    RE,MRHLN                                                         
         AR    R1,RE                                                            
         CLI   MRHEL,0                                                          
         BE    BLDL120                                                          
         CLI   MRHEL,MRHELQ                                                     
         BNE   BLDL112                                                          
         XR    RF,RF                                                            
         IC    RF,DISNARD                                                       
         LA    RF,DISLINED(RF)                                                  
         MVC   0(L'LC@NRTV,RF),MRHDETS                                          
         B     BLDL120                                                          
         DROP  R1                                                               
*&&                                                                             
BLDL114  TM    TSARIND3,TSARPWOI+TSARDIFF                                       
         BZ    BLDL120                                                          
         SR    RE,RE                                                            
         ICM   RE,3,TSARDNAR       DISPLACEMENT TO LENGTH/NARRATIVE             
         BZ    BLDL120                                                          
         LA    RE,SAVED(RE)        DISPLACEMENT INTO SAVED                      
         SR    RF,RF                                                            
         IC    RF,DISNARD                                                       
         LA    RF,DISLINED(RF)                                                  
         ICM   R1,1,0(RE)          R1=L'NARRATIVE                               
         BZ    BLDL120                                                          
         CLI   0(RE),L'LC@NRTV     TEST FULL NARRATIVE FITS                     
         BNH   *+8                                                              
         LA    R1,L'LC@NRTV        MAX L'NARRATIVE                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),1(RE)                                                    
                                                                                
BLDL120  XR    RE,RE               DISPLAY INSERTION/TRANSMISSION DATE          
*&&UK                                                                           
         ICM   RE,1,DISIDTD                                                     
         BZ    BLDL122                                                          
         OC    TSARFIDT,TSARFIDT                                                
         BZ    BLDL122                                                          
         LA    RF,DISLINED(RE)                                                  
         GOTO1 VDATCON,DMCB,(2,TSARFIDT),(17,(RF))                              
*&&                                                                             
BLDL122  DS    0H                                                               
*&&UK                                                                           
         OC    DISSDCD,DISSDCD     TEST DISPLAY SUNDRY CREDITOR                 
         BZ    BLDL130                                                          
*        CLI   FILEFORM,VLISQ      TEST OLD FILE                                
*        BE    BLDL130                                                          
         TM    ACCIND2,ACCISUND    TEST ACCOUNT IS A SUNDRY CREDITOR            
         BZ    BLDL130                                                          
         OC    TSARDADR,TSARDADR                                                
         BZ    BLDL130                                                          
         TM    FLAG,X'80'          TEST RECORD HAS BEEN READ                    
         BO    BLDL123                                                          
         MVC   IODA,TSARDADR       SET DISK ADDRESS                             
         LA    R1,IOGET+IOACCMST+IO1Q  GETRUP BUILDS KEY ON OLD FILE            
         TM    TSARRSTA,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    FLAG,X'80'          SET RECORD HAS BEEN READ                     
BLDL123  L     R1,AIOBUFF                                                       
         LA    R1,TRNRFST-TRNRECD(R1)                                           
         USING NAMELD,R1                                                        
         XR    RE,RE                                                            
BLDL124  CLI   NAMEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NAMEL,NAMELQ                                                     
         BE    BLDL126                                                          
         IC    RE,NAMLN                                                         
         AR    R1,RE                                                            
         B     BLDL124                                                          
BLDL126  XR    RF,RF                                                            
         IC    RF,DISSDCD                                                       
         LA    RF,DISLINED(RF)                                                  
         MVC   0(L'LC@SUNCR,RF),SPACES                                          
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         CH    RE,=Y(L'LC@SUNCR-1)                                              
         BNH   BLDL128                                                          
         LA    RE,L'LC@SUNCR-1                                                  
         LA    RF,NAMEREC+L'LC@SUNCR                                            
         CLI   0(RF),C' '          TEST STRING CONTINUES                        
         BNH   BLDL128             BREAK FOUND IN TEXT                          
         BCTR  RF,0                                                             
         BCT   RE,*-10             LOOK BACKWARDS FOR A BREAK                   
         LA    RE,L'LC@SUNCR-1     NO BREAK FOUND IN SUNDRY CREDITOR            
                                                                                
BLDL128  XR    RF,RF                                                            
         IC    RF,DISSDCD                                                       
         LA    RF,DISLINED(RF)                                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),NAMEREC     EXTRACT SUNDRY CREDITOR                      
         DROP  R1                                                               
*&&                                                                             
BLDL130  CLI   DISSNMD,0           SUPPLIER NAME                                
         BZ    BLDL138                                                          
*        CLI   FILEFORM,VLISQ      TEST OLD FILE                                
*        BE    BLDL138                                                          
         CLC   TSARCON,SPACES                                                   
         BNH   BLDL138                                                          
         MVC   KEY,SPACES                                                       
         PUSH  USING                                                            
SUP      USING ACTRECD,KEY                                                      
         MVC   SUP.ACTKCULA,TSARCON                                             
         GOTO1 AIOEXEC,IOHI+IOACCDIR+IO1Q                                       
         BNE   BLDL138                                                          
         CLC   SUP.ACTKCULA,TSARCON                                             
         BNE   BLDL138                                                          
         POP   USING                                                            
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOBUFF                                                       
         LA    R1,TRNRFST-TRNRECD(R1)                                           
         USING NAMELD,R1                                                        
         XR    RE,RE                                                            
BLDL132  CLI   NAMEL,0                                                          
         BE    BLDL138                                                          
         CLI   NAMEL,NAMELQ                                                     
         BE    BLDL134                                                          
         IC    RE,NAMLN                                                         
         AR    R1,RE                                                            
         B     BLDL132                                                          
                                                                                
BLDL134  XR    RF,RF                                                            
         IC    RF,DISSNMD                                                       
         LA    RF,DISLINED(RF)                                                  
         MVC   0(L'LC@SUPN,RF),SPACES                                           
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         BM    BLDL138                                                          
         CLM   RE,1,=AL1(L'LC@SUPN-1)                                           
         BNH   BLDL136             ALL SUPPLIER NAME FITS                       
         LA    RE,L'LC@SUPN-1                                                   
         LA    RF,NAMEREC+L'LC@SUPN                                             
         CLI   0(RF),C' '          TEST STRING CONTINUES                        
         BNH   BLDL136             BREAK FOUND IN TEXT                          
         BCTR  RF,0                                                             
         BCT   RE,*-10             LOOK BACKWARDS FOR A BREAK                   
         LA    RE,L'LC@SUPN-1      NO BREAK FOUND IN NARRATIVE                  
                                                                                
BLDL136  XR    RF,RF                                                            
         IC    RF,DISSNMD                                                       
         LA    RF,DISLINED(RF)                                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),NAMEREC     EXTRACT NARRATIVE                            
         DROP  R1                                                               
                                                                                
BLDL138  DS    0H                                                               
*&&UK                                                                           
         CLI   DISAFCD,0           FOREIGN CURRENCY DATA                        
         BE    BLDL148                                                          
         LA    R3,COMPCURT                                                      
         ZAP   DUB,TSARAMNT                                                     
         USING CURTABD,R3                                                       
         CLC   TSARAFCC,SPACES                                                  
         BNH   BLDL146                                                          
         L     R3,ACURRTAB         SEARCH CURRENCY TABLE                        
         LA    R0,CURRTABN                                                      
         CLI   CURTCUR,EOT                                                      
         BE    BLDL148                                                          
         CLC   CURTCUR,TSARAFCC                                                 
         BE    *+16                                                             
         LA    R3,L'CURRTAB(R3)                                                 
         BCT   R0,*-22                                                          
         B     BLDL148                                                          
         ZAP   DUB,TSARAFCA                                                     
                                                                                
         TM    TWAMODE2,TWAM2INV   TEST INVOICE-STYLE CHEQUE OVERLAY            
         BNO   BLDL146                                                          
         TM    TSARINDS,TSARMCHQ   TEST CHEQUE                                  
         BNO   BLDL146                                                          
         ZAP   DUB,TSARFREF        USE (AFC) REMAINDER                          
         CLM   R2,8,=C'R'          TEST THIS IS A REPORT BEING PRINTED          
         BNE   BLDL140                                                          
         OC    FILTCH1,FILTCH1     TEST UNMATCHING AN INVOICE                   
         BNZ   BLDL142                                                          
         B     BLDL146                                                          
BLDL140 TM     TWAMODE2,TWAM2DIS   TEST UNMATCHING AN INVOICE                   
         BZ    BLDL146                                                          
BLDL142  LA     R0,CHQMAX                                                       
         LA    R1,FILTAF1                                                       
         LA    RE,FILTCH1                                                       
         L     RF,ATSARBLK                                                      
BLDL144  CLC    0(L'FILTCH1,RE),TSRNUM-TSARD(RF)                                
         BNE   *+14                                                             
         ZAP   DUB,0(L'FILTAF1,R1)                                              
         B     BLDL146                                                          
         LA    RE,L'FILTCH1(RE)                                                 
         LA    R1,L'FILTAF1(R1)                                                 
         BCT   R0,BLDL144                                                       
         DC    H'0'                                                             
                                                                                
BLDL146  XR    RF,RF                                                            
         IC    RF,DISAFCD                                                       
         LA    RF,DISLINED(RF)                                                  
         CURED DUB,(L'LC@AMTR,(RF)),CURTABD,MINUS=YES,                 >        
               CURSYMB=YES                                                      
         ORG   *-2                                                              
         TM    SAFCIND1,SAFCI1SC   TEST SINGLE FOREIGN CURRENCY                 
         BZ    *+8                                                              
         OI    0(R1),B'00000100'   EXCLUDE CURRENCY SYMBOL                      
         BALR  RE,RF                                                            
         DROP  R3                                                               
         TM    TWAMODE2,TWAM2INV   TEST INVOICE-STYLE CHEQUE OVERLAY            
         BNO   BLDL148                                                          
         TM    TSARINDS,TSARMCHQ   TEST CHEQUE                                  
         BNO   BLDL148                                                          
         CP    TSARFREF,PZERO      TEST FULLY MATCHED                           
         BE    BLDL148             YES                                          
         CP    TSARAFCA,TSARFREF   TEST PART MATCHED                            
         BE    BLDL148             NO (UNMATCHED)                               
         BH    *+6                 YES                                          
         DC    H'0'                REMAINDER CANNOT EXCEED AMOUNT               
         XR    RF,RF                                                            
         IC    RF,DISAFCD          FOREIGN CURRENCY DATA                        
         LA    RF,DISLINED(RF)                                                  
         MVI   L'LC@AMTR-1(RF),C'*' USE SIGN POSITION, AS AMOUNT IS +VE         
*&&                                                                             
BLDL148  DS    0H                                                               
*&&UK                                                                           
         ZAP   PKWK16B,PZERO                                                    
         CLI   DISEXDD,0           EXCHANGE DIFFERENCE                          
         BE    BLDL150                                                          
         TM    TSARIND2,TSARDIFP   TEST NOT EXCHANGE DIFFERENCE POSTING         
         BNZ   BLDL150                                                          
         GOTO1 AEXCHDF                                                          
         BNE   BLDL150                                                          
         CURED (P6,TMPEXDF),(17,WORK),COMPCURT,MINUS=YES,ALIGN=LEFT             
         XR    RF,RF                                                            
         IC    RF,DISEXDD          EXCHANGE DIFFERENCE                          
         LA    RF,DISLINED(RF)                                                  
         LR    RE,R0                                                            
         LA    R0,L'LC@EXDF                                                     
         SR    R0,RE                                                            
         BM    *+20                                                             
         AR    RF,R0                                                            
         EX    RE,*+8                                                           
         B     BLDL150                                                          
         MVC   0(0,RF),WORK                                                     
                                                                                
         MVI   0(RF),C'+'          PREFIX OVERFLOW WITH +                       
         MVC   1(L'LC@EXDF-1,RF),WORK                                           
         CP    TMPEXDF,PZERO                                                    
         BNM   *+8                                                              
         MVI   L'LC@EXDF-1(RF),C'-'  RETAIN SIGN                                
*&&                                                                             
BLDL150  DS    0H                                                               
*&&UK                                                                           
         CLI   DISRATD,0           EXCHANGE RATE                                
         BE    BLDL152                                                          
         PUSH  USING                                                            
         USING AFCX,TSARAFCX                                                    
         OC    AFCXRATE,AFCXRATE                                                
         BZ    BLDL152                                                          
         GOTO1 AEDTRAT,DMCB,AFCXRATE,(L'LC@EXCHR,WORK)                          
         LA    RE,(L'LC@EXCHR-1)/2 ALIGN DECIMAL POINT TO CENTRE                
         LA    RF,WORK                                                          
         CLI   0(RF),C'0'                                                       
         BL    *+12                                                             
         LA    RF,1(RF)                                                         
         BCT   RE,*-12                                                          
         XR    RF,RF                                                            
         IC    RF,DISRATD                                                       
         AR    RF,RE                                                            
         LA    RF,DISLINED(RF)                                                  
         LCR   RE,RE                                                            
         LA    RE,L'LC@EXCHR-1(RE)                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),WORK                                                     
         POP   USING                                                            
*&&                                                                             
BLDL152  SR    RF,RF               DISPLAY ADVANCE AMOUNT                       
         ICM   RF,1,DISADVD                                                     
         BZ    BLDL154                                                          
         CLI   FILTCHQ,INCLUDE     CHEQUE MODE ONLY                             
         BNE   BLDL154                                                          
         LA    RF,DISLINED(RF)                                                  
*&&UK*&& CURED TSARCADV,(L'LC@AVNPA,(RF)),COMPCURT,MINUS=YES                    
*&&US*&& CURED TSARCADV,(L'LC@AVNPA,(RF)),2,MINUS=YES                           
BLDL154  SR    RF,RF               DISPLAY DIFFERENCE AMOUNT                    
         ICM   RF,1,DISDIFD                                                     
         BZ    BLDL156                                                          
         CLI   FILTCHQ,INCLUDE     CHEQUE MODE ONLY                             
         BNE   BLDL156                                                          
         LA    RF,DISLINED(RF)                                                  
*&&UK*&& CURED TSARCDIF,(L'LC@DFRNC,(RF)),COMPCURT,MINUS=YES                    
*&&US*&& CURED TSARCDIF,(L'LC@DFRNC,(RF)),2,MINUS=YES                           
                                                                                
BLDL156  DS    0H                                                               
*&&UK                                                                           
         OC    SCNDCURT,SCNDCURT   TEST SECONDARY CURRENCY IN USE               
         BZ    BLDL180                                                          
         OC    DISSCC(DISSCCN),DISSCC                                           
         BZ    BLDL180                                                          
         OC    TSARDADR,TSARDADR   TEST ON FILE                                 
         BZ    BLDL157                                                          
         TM    SCURIND1,SCURIOMT   TEST OVERLAY MAINTAINS TOTALS                
         BO    BLDL157             WE HAVE 2ND CURRENCY AMOUNTS                 
         TM    FLAG,X'80'          TEST RECORD HAS BEEN READ                    
         BO    BLDL156A                                                         
         MVC   IODA,TSARDADR       SET DISK ADDRESS                             
         LA    R1,IOGET+IOACCMST+IO1Q  GETRUP BUILDS KEY ON OLD FILE            
         TM    TSARRSTA,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    FLAG,X'80'                                                       
                                                                                
BLDL156A MVC   WORK(L'CURTCUR),COMPCURT+(CURTCUR-CURTABD)                       
         MVC   WORK+L'CURTCUR(L'CURTCUR),SCNDCURT+(CURTCUR-CURTABD)             
         LA    R0,TOBAAOUT                                                      
         TM    COMPSTA7,CPYSSCNV                                                
         BZ    *+8                                                              
         LA    R0,TOBAACVS                                                      
         GOTO1 VTOBACCO,DMCB,((R0),WORK),AIOBUFF,ACOM,0,0,0                     
***********************************************************************         
* TRNREC IS IN SECONDARY CURRENCY AFTER TOBACCO CALL                  *         
***********************************************************************         
         OI    FLAG,X'40'          SET RECORD IN SECOND CURRENCY                
         GOTO1 ASETELAD,AIOBUFF                                                 
         USING TRNELD,R1                                                        
         ICM   R1,15,ATRNEL                                                     
         ZAP   TSARSCUA,TRNAMNT                                                 
         ZAP   TSARSCUD,PZERO                                                   
         USING SCIELD,R1                                                        
         ICM   R1,15,ASCICDSC                                                   
         BZ    BLDL157                                                          
         ZAP   TSARSCUD,SCIAMNT                                                 
                                                                                
BLDL157  CLI   DISSAMD,0           DISPLAY SECOND CURRENCY AMOUNT               
         BE    BLDL160                                                          
         TM    FLAG,X'40'          TEST RECORD IN SECONDARY CURRENCY            
         BO    *+12                                                             
         TM    SCURIND1,SCURIOMT   TEST OVERLAY MAINTAINS TOTALS                
         BZ    *+14                                                             
         ZAP   DUB,TSARSCUA        USE EXTRACTED AMOUNT                         
         B     BLDL158                                                          
         ZAP   DUB,TSARAMNT        CALCULATE SECOND CURRENCY AMOUNT             
         GOTO1 VCASHVAL,DMCB,(X'80',DUB),(X'28',0),WORK,0,0,0                   
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   DUB,12(8,R1)                                                     
BLDL158  SR    RF,RF                                                            
         IC    RF,DISSAMD                                                       
         LA    RF,DISLINED(RF)                                                  
         CURED DUB,(L'AC@AMT,(RF)),SCNDCURT,MINUS=YES                           
                                                                                
BLDL160  CLI   DISSDRD,0           DISPLAY SECOND CURRENCY DEBIT                
         BE    BLDL164                                                          
         TM    TSARINDS,TSARDRQ    TEST DEBIT                                   
         BZ    BLDL164                                                          
         TM    TSARIND3,TSARPWOI+TSARDIFF                                       
         BNZ   BLDL164             NOT NEW POSTINGS                             
         TM    FLAG,X'40'          TEST RECORD IN SECONDARY CURRENCY            
         BO    *+12                                                             
         TM    SCURIND1,SCURIOMT   TEST OVERLAY MAINTAINS TOTALS                
         BZ    *+14                                                             
         ZAP   DUB,TSARSCUA        USE EXTRACTED AMOUNT                         
         B     BLDL162                                                          
         ZAP   DUB,TSARAMNT        CALCULATE SECOND CURRENCY AMOUNT             
         GOTO1 VCASHVAL,DMCB,(X'80',DUB),(X'28',0),WORK,0,0,0                   
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   DUB,12(8,R1)                                                     
BLDL162  SR    RF,RF                                                            
         IC    RF,DISSDRD                                                       
         LA    RF,DISLINED(RF)                                                  
         CURED DUB,(L'AC@DRS,(RF)),SCNDCURT,MINUS=YES                           
                                                                                
BLDL164  CLI   DISSCRD,0           DISPLAY SECOND CURRENCY CREDIT               
         BE    BLDL168                                                          
         TM    TSARINDS,TSARDRQ    TEST DEBIT                                   
         BO    BLDL168                                                          
         TM    TSARIND3,TSARPWOI+TSARDIFF                                       
         BNZ   BLDL168             NOT NEW POSTINGS                             
         TM    FLAG,X'40'          TEST RECORD IN SECONDARY CURRENCY            
         BO    *+12                                                             
         TM    SCURIND1,SCURIOMT   TEST OVERLAY MAINTAINS TOTALS                
         BZ    *+14                                                             
         ZAP   DUB,TSARSCUA        USE EXTRACTED AMOUNT                         
         B     BLDL166                                                          
         ZAP   DUB,TSARAMNT        CALCULATE SECOND CURRENCY AMOUNT             
         GOTO1 VCASHVAL,DMCB,(X'80',DUB),(X'28',0),WORK,0,0,0                   
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   DUB,12(8,R1)                                                     
BLDL166  SR    RF,RF                                                            
         IC    RF,DISSCRD                                                       
         LA    RF,DISLINED(RF)                                                  
         CURED DUB,(L'AC@CRS,(RF)),SCNDCURT,MINUS=YES                           
                                                                                
BLDL168  CLI   DISSDSD,0           DISPLAY SECOND CURRENCY DISCOUNT             
         BE    BLDL172                                                          
         CLI   XACTION,ACTVOID     TEST VOID                                    
         BNE   *+12                                                             
         CLI   FILTCHQ,INCLUDE     CHEQUE MODE DISCOUNT NOT AVAILABLE           
         BE    BLDL172                                                          
         TM    FLAG,X'40'          TEST RECORD IN SECONDARY CURRENCY            
         BO    *+12                                                             
         TM    SCURIND1,SCURIOMT   TEST OVERLAY MAINTAINS TOTALS                
         BZ    *+14                                                             
         ZAP   DUB,TSARSCUD        USE EXTRACTED AMOUNT                         
         B     BLDL170                                                          
         OC    TSARFDIS,TSARFDIS   TEST DISCOUNT VALUE KNOWN                    
         BZ    BLDL172                                                          
         ZAP   DUB,TSARFDIS        CALCULATE SECOND CURRENCY AMOUNT             
         GOTO1 VCASHVAL,DMCB,(X'80',DUB),(X'28',0),WORK,0,0,0                   
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   DUB,12(8,R1)                                                     
BLDL170  SR    RF,RF                                                            
         IC    RF,DISSDSD                                                       
         LA    RF,DISLINED(RF)                                                  
         CURED DUB,(L'LC@DSCR,(RF)),SCNDCURT,MINUS=YES                          
                                                                                
BLDL172  CLI   DISSDID,0           DISPLAY SECOND CURRENCY DIFFERENCE           
         BE    BLDL174                                                          
         CLI   FILTCHQ,INCLUDE     CHEQUE MODE ONLY                             
         BNE   BLDL174                                                          
         ZAP   DUB,TSARCDIF                                                     
         GOTO1 VCASHVAL,DMCB,(X'80',DUB),(X'28',0),WORK,0,0,0                   
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   DUB,12(8,R1)                                                     
         SR    RF,RF                                                            
         IC    RF,DISSDID                                                       
         LA    RF,DISLINED(RF)                                                  
         CURED DUB,(L'LC@DFRNC,(RF)),SCNDCURT,MINUS=YES                         
                                                                                
BLDL174  CLI   DISSADD,0           DISPLAY SECOND CURRENCY ADVANCE              
         BE    BLDL180                                                          
         CLI   FILTCHQ,INCLUDE     CHEQUE MODE ONLY                             
         BNE   BLDL180                                                          
         ZAP   DUB,TSARCADV                                                     
         GOTO1 VCASHVAL,DMCB,(X'80',DUB),(X'28',0),WORK,0,0,0                   
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   DUB,12(8,R1)                                                     
         SR    RF,RF                                                            
         IC    RF,DISSADD                                                       
         LA    RF,DISLINED(RF)                                                  
         CURED DUB,(L'LC@AVNPA,(RF)),SCNDCURT,MINUS=YES                         
*&&                                                                             
                                                                                
* BLDL180  SR    RF,RF               NEXT DISPLAY COLUMN                        
BLDL180  SR    RF,RF               NEXT DISPLAY COLUMN                          
         ICM   RF,1,DISSYSD        DISPLAY SYSTEM                               
         BZ    BLDL190                                                          
         LA    RF,DISLINED(RF)                                                  
         MVC   0(L'TSARAASY,RF),TSARAASY                                        
BLDL190  SR    RF,RF               NEXT DISPLAY COLUMN                          
         ICM   RF,1,DISMEDD        DISPLAY MEDIA                                
         BZ    BLDL200                                                          
         LA    RF,DISLINED(RF)                                                  
         MVC   0(L'TSARAAMD,RF),TSARAAMD                                        
BLDL200  SR    RF,RF                                                            
         ICM   RF,1,DISCLID        DISPLAY CLIENT                               
         BZ    BLDL210                                                          
         LA    RF,DISLINED(RF)                                                  
         MVC   0(L'TSARAACL,RF),TSARAACL                                        
BLDL210  SR    RF,RF                                                            
         ICM   RF,1,DISPROD        DISPLAY PRODUCT                              
         BZ    BLDL220                                                          
         LA    RF,DISLINED(RF)                                                  
         MVC   0(L'TSARAAPR,RF),TSARAAPR                                        
BLDL220  SR    RF,RF                                                            
         ICM   RF,1,DISESTD        DISPLAY ESTIMATE                             
         BZ    BLDL230                                                          
         LA    RF,DISLINED(RF)                                                  
         MVC   0(L'TSARAAES,RF),TSARAAES                                        
BLDL230  SR    RF,RF                                                            
         ICM   RF,1,DISMMOSD       DISPLAY MEDIA MOS                            
         BZ    BLDL240                                                          
         LA    RF,DISLINED(RF)                                                  
         MVC   0(L'TSARAALT,RF),TSARAALT                                        
BLDL240  SR    RF,RF                                                            
         ICM   RF,1,DISVEND       DISPLAY VENDOR                                
         BZ    BLDL250                                                          
         LA    RF,DISLINED(RF)                                                  
         MVC   0(L'TSARAAVN,RF),TSARAAVN                                        
BLDL250  SR    RF,RF                                                            
         ICM   RF,1,DISVIND       DISPLAY VENDOR INV#                           
         BZ    BLDL260                                                          
         LA    RF,DISLINED(RF)                                                  
         MVC   0(L'TSARAAVI,RF),TSARAAVI                                        
BLDL260  SR    RF,RF               NEXT DISPLAY COLUMN                          
*                                                                               
         CLM   R2,8,=C'R'          IF REPDISP USED, SWAP WITH DISDISP           
         BNE   ROUTE                                                            
         XC    DISDISP(DISDISPL),REPDISP                                        
         XC    REPDISP(DISDISPL),DISDISP                                        
         XC    DISDISP(DISDISPL),REPDISP                                        
         B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD PFKEY DISPLAY LINE                                            *         
***********************************************************************         
                                                                                
BLDPFK   MVI   TEMP,C' '                                                        
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
         LA    RF,TEMP             RF=A(PFKEY BUILD AREA)                       
         LR    R3,RF               SAVE A(START OF STRING)                      
         L     R1,AAPFTAB                                                       
         TM    TWAMODE,TWAMALTP                                                 
         BZ    *+8                                                              
         L     R1,ASPFTAB                                                       
         TM    TWAMODE3,TWAM3ZOO   TEST PROCESSING ZOOM SCREEN                  
         BZ    *+8                                                              
         L     R1,AZPFTAB                                                       
                                                                                
         USING APFTABD,R1                                                       
BLDPFK02 TM    TWAMODE3,TWAM3ZOO   TEST PROCESSING ZOOM SCREEN                  
         BO    *+12                                                             
         TM    TWAMODE,TWAMALTP    TEST SCROLL OR ACTION PFKEYS                 
         BNZ   BLDPFK24                                                         
         XR    R0,R0                                                            
         ICM   R0,1,APFTPFK        R0=PFKEY NUMBER                              
         BZ    BLDPFKX             EOT                                          
*&&UK                                                                           
*                                  TEST PFK DISPLAY INDICATOR                   
         TM    APFTIND1,APFTI1FC   TEST PFKEY FOR SINGLE CURRENCY               
         BZ    BLDPFK04                                                         
         TM    SAFCIND1,SAFCI1SC   TEST ONE FOREIGN CURRENCY                    
         BO    BLDPFK04                                                         
         TM    APFTIND1,APFTICOD   TEST TOGGLE TOTALS PFK                       
         BZ    BLDPFK14                                                         
         OC    SCNDCURT,SCNDCURT   TEST SECONDARY CURRENCY AVAILABLE            
         BZ    BLDPFK14                                                         
                                                                                
BLDPFK04 TM    APFTIND1,APFTILFT+APFTIRGH  TRYING TO DO LEFT/RIGHT ALSO         
         BZ    BLDPFK06                                                         
         TM    SAFCIND1,SAFCI1SC   TEST ONE FOREIGN CURRENCY                    
         BNO   BLDPFK06                                                         
         CLI   XACTOLAY,BVOLAY     DONT DISPLAY LEFT/RIGHT IN BANK VOID         
         BE    BLDPFK14                                                         
                                                                                
BLDPFK06 TM    APFTIND1,APFTIALT   TEST PFKEY FOR ALTERNATE TOTAL               
         BZ    BLDPFK08                                                         
         CLI   XACTOLAY,CMOLAY     TEST CREDITOR MANUAL                         
         BE    BLDPFK08                                                         
         CLI   XACTOLAY,BVOLAY     OR BANK VOID                                 
         BNE   BLDPFK14                                                         
*&&                                                                             
BLDPFK08 CLI   APFTMODE,0          TEST MODE RESTRICTION                        
         BE    *+14                                                             
         CLC   APFTMODE,FILTCHQ    TEST CHEQUE/INVOICE MODE                     
         BNE   BLDPFK14                                                         
                                                                                
         CLI   APFTOVR,0           TEST OVERLAY RESTRICTION                     
         BE    *+14                                                             
         CLC   APFTOVR,XACTOLAY    TEST CORRECT OVERLAY                         
         BNE   BLDPFK14                                                         
                                                                                
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING GRIDS                         
         BZ    *+12                                                             
         TM    APFTIND2,APFT2GRD   IS THIS PF KEY ALLOWED FOR GRIDS             
         BZ    BLDPFK14                                                         
         TM    APFTIND1,APFTIP26   TEST PROFILE 26 RESTRICTION                  
         BZ    BLDPFK12                                                         
         CLI   PROFDFAV,C'N'       TEST NEITHER ACTION VALID                    
         BE    BLDPFK14                                                         
         CLI   PROFDFAV,C'B'       TEST BOTH ACTIONS VALID                      
         BE    BLDPFK12                                                         
         CLI   PROFDFAV,C'D'       TEST DIFFERENCES ONLY                        
         BNE   BLDPFK10                                                         
         CLI   APFTPFK,PFK02       TEST DIFFERENCE PFK                          
         BNE   BLDPFK14                                                         
         B     BLDPFK12                                                         
BLDPFK10 CLI   PROFDFAV,C'A'       TEST ADVANCES ONLY                           
         BNE   BLDPFK14                                                         
         CLI   APFTPFK,PFK05       TEST ADVANCE PFK                             
         BNE   BLDPFK14                                                         
         B     BLDPFK12                                                         
                                                                                
BLDPFK12 DS    0H                  NEXT PROFILE RESTRICTION                     
         B     BLDPFK16                                                         
                                                                                
BLDPFK14 LA    R1,APFTABL(R1)      SET A(NEXT TABLE ENTRY)                      
         B     BLDPFK02                                                         
                                                                                
BLDPFK16 BAS   RE,SETPFK           SET PFNN=                                    
         MVC   LAREADDR,APFTLCAD   SET SCON ADDRESS                             
*&&UK                                                                           
         TM    APFTIND1,APFTICOD   TEST PFKEY FOR SINGLE CURRENCY               
         BZ    BLDPFK22                                                         
         MVC   0(L'CURTCUR,RF),COMPCURT+(CURTCUR-CURTABD)                       
         TM    SAFCIND1,SAFCI1SC   TEST ONE FOREIGN CURRENCY                    
         BZ    BLDPFK18                                                         
         TM    SAFCIND1,SAFCI1ON   TEST ALTERNATIVE TOTALS SHOWN                
         BO    BLDPFK18                                                         
         TM    SCURIND1,SCURITOT   TEST SECONDARY TOTALS SHOWN                  
         BO    BLDPFK20                                                         
         MVC   0(L'CURTCUR,RF),FORECURT+(CURTCUR-CURTABD)                       
         B     BLDPFK20                                                         
BLDPFK18 OC    SCNDCURT,SCNDCURT   TEST SECONDARY CURRENCY AVAILABLE            
         BZ    BLDPFK20                                                         
         TM    SCURIND1,SCURITOT   TEST SECONDARY TOTALS SHOWN                  
         BO    BLDPFK20                                                         
         MVC   0(L'CURTCUR,RF),SCNDCURT+(CURTCUR-CURTABD)                       
BLDPFK20 MVI   L'CURTCUR(RF),C' '                                               
         LA    RF,L'CURTCUR+1(RF)                                               
*&&                                                                             
BLDPFK22 LA    R2,L'LC@PAGE-1                                                   
         TM    APFTIND1,APFTILNG   TEST LONG PFKEY TEXT                         
         BZ    *+8                                                              
         LA    R2,L'LC@DFRNC-1                                                  
         LA    R1,APFTABL(R1)      SET A(NEXT TABLE ENTRY)                      
         B     BLDPFK26                                                         
         DROP  R1                                                               
                                                                                
         USING SPFTABD,R1                                                       
BLDPFK24 XR    R0,R0                                                            
         ICM   R0,1,SPFTPFK        R0=PFKEY NUMBER                              
         BZ    BLDPFKX             EOT                                          
         BAS   RE,SETPFK           SET PFNN=                                    
         CLI   SPFTDIR,0           SET DIRECTION IF DEFINED IN TABLE            
         BE    *+14                                                             
         MVC   0(L'SPFTDIR,RF),SPFTDIR                                          
         LA    RF,L'SPFTDIR(RF)                                                 
         MVC   LAREADDR,SPFTLCAD   SET SCON ADDRESS                             
         LA    R1,SPFTABL(R1)      SET A(NEXT TABLE ENTRY)                      
         LA    R2,L'LC@PAGE-1                                                   
         DROP  R1                                                               
                                                                                
BLDPFK26 EX    0,LARE              GET A(ACTION/SCROLL WORD)                    
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(RE)                                                    
         AR    RF,R2               RF=LAST POSSIBLE CHARACTER                   
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(RF)            POINT ONE PAST END OF WORD                   
         B     BLDPFK02                                                         
                                                                                
BLDPFKX  BAS   RE,SETPFK           SET ALTPF KEY IF ROOM                        
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* BLDPFK SUB-ROUTINE TO BUILD PFKEY KEYWORD                           *         
***********************************************************************         
                                                                                
SETPFK   LTR   R0,R0               TEST LAST TIME CALL                          
         BZ    SETPFK2                                                          
         CR    RF,R3               TEST "PF" DISPLAYED                          
         BNE   *+14                                                             
         MVC   0(2,RF),AC@PFK      NO - FORMAT PFNN= (ELSE NN=)                 
         LA    RF,2(RF)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(2,RF),DUB                                                      
         CLI   0(RF),C'0'          TEST PFKEY 1 THRU 9                          
         BNE   *+12                                                             
         MVC   0(2,RF),1(RF)       YES - SQUASH OUT ZERO                        
         BCTR  RF,0                                                             
         MVI   2(RF),C'='                                                       
         LA    RF,3(RF)                                                         
         BR    RE                                                               
                                                                                
SETPFK2  SR    RF,R3               SET PF1(3)=ALTPFS IF ENOUGH ROOM             
         BZ    *+12                                                             
*&&UK*&& CH    RF,=Y(L'MRKPFK-L'LC@ALTPF-5)  TEST PF1=ALTPFS WILL FIT           
*&&US*&& CH    RF,=Y(L'MRKPFK-L'LC@ALTPF-6)  TEST PF13=ALTPFS WILL FIT          
         BH    SETPFK4                                                          
         TM    TWAMODE3,TWAM3ZOO   TEST NOT ZOOM SCREEN                         
         BO    SETPFK4                                                          
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING GRIDS                         
         BO    SETPFK4             YES DON'T PUT OUT ALT PFKEY                  
         AR    RF,R3               POINT BACK TO OUTPUT AREA                    
         CR    RF,R3               TEST FIRST PFKEY SHOWN                       
         BNE   *+14                                                             
         MVC   0(2,RF),AC@PFK      SET PF PREFIX                                
         LA    RF,2(RF)                                                         
         MVI   0(RF),C'1'          PF1                                          
*&&US                                                                           
         MVI   1(RF),C'3'          PF13                                         
         LA    RF,1(RF)                                                         
*&&                                                                             
         MVI   1(RF),C'='                                                       
         MVC   2(L'LC@ALTPF,RF),LC@ALTPF                                        
SETPFK4  ICM   R2,15,ADISPFKS      R2=A(HEADER OF PF KEY TWA FIELD)             
         BZR   RE                  A(PFK LINE) NOT PROVIDED                     
         OI    FVOIND-FVIHDR(R2),FVOXMT                                         
         LA    RF,TEMP+L'MRKPFK                                                 
         CLI   0(RF),C' '                                                       
         BE    *+12                                                             
         MVI   0(RF),C' '                                                       
         BCT   RF,*-12                                                          
         MVC   L'FVIHDR(L'MRKPFK,R2),TEMP                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD SOURCE ACCOUNT FROM ELEMENT AND/OR TRANSACTION KEY *         
***********************************************************************         
                                                                                
BLDSRC   MVC   SRCWORK,SPACES      CLEAR SOURCE ACCOUNT WORK AREA               
         L     R1,AIOBUFF          R1=A(TRANSACTION RECORD)                     
         LA    R2,TRNRFST-TRNRECD(R1)                                           
         USING TRNELD,R2           R2=A(TRNEL)                                  
         LR    R3,R2               R3=A(TRNEL)                                  
         XR    R0,R0                                                            
*&&US                                                                           
         CLC   RECVUL,TRNKUNT-TRNRECD(R1)                                       
         BE    BLDSRC40            RECEIVABLES                                  
         TM    ACCIND1,ACCISPOT+ACCIPRNT  TEST MEDIA VENDOR                     
         BZ    BLDSRC02                                                         
         CLI   TRNEL,TRNELQ        TEST TRANSACTION ELEMENT                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDATCON,DMCB,(1,TRNDATE),(0,WORK)                                
         MVI   BYTE,X'FF'          SET DAY IS KNOWN                             
         XR    R0,R0                                                            
*&&                                                                             
BLDSRC02 IC    R0,TRNLN                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             TEST EOR                                     
         BE    BLDSRCX                                                          
*&&UK                                                                           
         CLI   0(R2),CPJELQ        CLIENT/PRODUCT/JOB ELEMENT                   
         BE    BLDSRC06                                                         
         CLI   0(R2),MXPELQ        MEDIA EXTRA PAYMENT ELEMENT                  
         BE    BLDSRC14                                                         
         CLI   0(R2),SORELQ        SOURCE ELEMENT                               
         BE    BLDSRC28                                                         
         B     BLDSRC02                                                         
*&&                                                                             
*&&US                                                                           
         CLI   0(R2),SCIELQ        SUBSIDIARY CASH ELEMENT                      
         BE    BLDSRC30                                                         
         TM    ACCIND1,ACCIPROD    TEST PRODUCTION                              
         BNO   BLDSRC04                                                         
         CLI   0(R2),OTHELQ        OTHERS ELEMENT                               
         BE    BLDSRC26                                                         
         B     BLDSRC02                                                         
                                                                                
BLDSRC04 CLI   0(R2),XPYELQ        MEDIA EXTRA PAYMENT ELEMENT                  
         BE    BLDSRC16                                                         
         B     BLDSRC02                                                         
*&&                                                                             
*&&UK                                                                           
         USING CPJELD,R2           R2=A(CLIENT/PRODUCT/JOB ELEMENT)             
BLDSRC06 CLI   CPJTYPE,CPJTEXP     TEST EXPENSE TYPE                            
         BE    BLDSRC08                                                         
         CLI   CPJTYPE,CPJTJOB     TEST JOB TYPE                                
         BE    BLDSRC10                                                         
         CLI   CPJTYPE,CPJTOTH     TEST OTHER TYPE                              
         BE    BLDSRC12                                                         
         B     ROUTH               SET CC NEQ                                   
BLDSRC08 MVC   SRCWORK(L'EXPSUL),EXPSUL                                         
         MVC   SRCWORK+L'EXPSUL(L'CPJEXP),CPJEXP                                
         B     BLDSRCX                                                          
BLDSRC10 MVC   SRCWORK(L'PRODUL),PRODUL                                         
         XR    RE,RE                                                            
         IC    RE,PRODBLEN         L'PRODUCTION LEVELS A+B                      
         LA    R1,SRCWORK+L'PRODUL(RE)                                          
         MVC   0(L'CPJJOB,R1),CPJJOB                                            
         XR    RF,RF                                                            
         IC    RF,PRODALEN         L'PRODUCTION LEVEL A                         
         LA    R1,SRCWORK+L'PRODUL(RF)                                          
         SR    RE,RF               L'PRODUCTION LEVEL B                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),CPJPRO                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SRCWORK+L'PRODUL(0),CPJCLI                                       
         B     BLDSRCX                                                          
BLDSRC12 MVC   SRCWORK(L'CPJOULA),CPJOULA                                       
         B     BLDSRCX                                                          
                                                                                
         USING MXPELD,R2           R2=A(MEDIA EXTRA PAYMENT ELEMENT)            
BLDSRC14 MVC   SRCWORK(L'MXPCLICD),MXPCLICD                                     
         B     BLDSRCX                                                          
                                                                                
         USING SORELD,R2                                                        
BLDSRC28 MVC   SRCWORK,SORAULA                                                  
         B     BLDSRCX                                                          
*&&                                                                             
*&&US                                                                           
         USING XPYELD,R2           R2=A(EXTRA PAYMENT ELEMENT)                  
BLDSRC16 TM    ACCIND1,ACCISPOT+ACCIPRNT TEST SPOT/PRINT VENDOR                 
         BZ    BLDSRC22                                                         
         OC    XPYPER,XPYPER       TEST PAYMENT PERIOD                          
         BZ    BLDSRC18                                                         
         LA    RF,XPYPER+6         FIRST TRY THE END DATE                       
         CLI   0(RF),C'-'          SEPARATED BY A DASH?                         
         BNE   *+8                                                              
         LA    RF,1(RF)            RF TO END DATE                               
         CLI   0(RF),C' '          ANY DATA IN END DATE FIELD                   
         BH    *+8                                                              
         LA    RF,XPYPER           NO, USE START DATE                           
         MVC   WORK(6),0(RF)                                                    
         CLC   WORK+4(2),=C'00'    CHECK THE DAY                                
         BNE   BLDSRC18                                                         
         MVC   WORK+4(2),=C'01'    ASSUME FIRST OF MONTH                        
         MVI   BYTE,0              SET DAY IS ASSUMED                           
                                                                                
BLDSRC18 TM    ACCIND1,ACCISPOT    TEST MEDIA SPOT VENDOR                       
         BZ    BLDSRC20                                                         
         TM    ACCIND1,ACCINETW    BUT NOT NETWORK SPOT VENDOR                  
         BO    BLDSRC20                                                         
         L     RF,ACOM                                                          
         USING COMFACSD,RF                                                      
         GOTO1 VGETBRD,DMCB,(1,WORK),WORK+6,CGETDAY,CADDAY                      
         DROP  RF                                                               
         MVC   WORK(6),WORK+12     SET END DATE OF BROADCAST MONTH              
         MVI   BYTE,X'FF'          SET DAY IS KNOWN                             
                                                                                
BLDSRC20 GOTO1 VDATCON,DMCB,(0,WORK),(1,TSARFMMD)                               
         CLI   BYTE,0              TEST DAY WAS ASSUMED                         
         BNE   BLDSRC22                                                         
         MVI   TSARFMMD+L'TSARFMMD-1,0                                          
                                                                                
BLDSRC22 L     R1,AIOBUFF                                                       
         XR    RE,RE                                                            
         IC    RE,PRODBLEN         RE=L'CLIENT/PRODUCT                          
         XR    RF,RF                                                            
         IC    RF,PRODALEN         RF=L'CLIENT                                  
         SR    RE,RF               RE=L'PRODUCT                                 
         LA    R1,(TRNKCACT+L'TRNKCACT)-TRNRECD(R1)                             
         SR    R1,RF               R1=A(CLIENT CODE IN C/A)                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SRCWORK(0),0(R1)    EXTRACT CLIENT                               
         LA    R1,SRCWORK+1(RF)                                                 
         L     RF,AIOBUFF          ASSUME PRODUCT IN KEY REFERENCE              
         LA    RF,TRNKREF-TRNRECD(RF)                                           
         TM    ACCIND1,ACCIPRNT    TEST PRINT VENDOR                            
         BNZ   BLDSRC24                                                         
         LA    RF,XPYPRO           ELSE PRODUCT IN XPYEL                        
         CLC   XPYPRO(3),=C'POL'   TEST POL=                                    
         BNE   BLDSRC24                                                         
         LA    RF,XPYPRO+4         SKIP TO PRODUCT                              
BLDSRC24 BCTR  RE,0                RE=L'PRODUCT-1                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RF)       EXTRACT PRODUCT                              
         OC    XPYEST,XPYEST       TEST ESTIMATE PRESENT                        
         BZ    BLDSRC02                                                         
         LA    RF,1(RE,R1)         RF=A(NEXT)                                   
         CURED XPYEST,(6,(RF)),0,ALIGN=LEFT                                     
         B     BLDSRC02                                                         
                                                                                
         USING OTHELD,R2           R2=A(OTHERS ELEMENT)                         
         USING TRNELD,R3           R3=A(TRANSACTION ELEMENT)                    
BLDSRC26 L     R1,AIOBUFF                                                       
         USING TRNRECD,R1          R1=A(TRANSACTION KEY)                        
         XR    RE,RE                                                            
         IC    RE,PRODBLEN         RE=L'CLIENT/PRODUCT                          
         XR    RF,RF                                                            
         IC    RF,PRODALEN         RF=L'CLIENT                                  
         SR    RE,RF               RE=L'PRODUCT                                 
         LA    RF,L'PRODUL-1(RF)   RF=L'CLIENT+UL-1                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SRCWORK(0),TRNKCUNT EXTRACT SJCLIENT                             
         DROP  R1                                                               
         LA    R1,SRCWORK+1(RF)                                                 
         IC    RF,PRODCLEN         RF=L'CLIENT/PRODUCT/JOB                      
         SR    RF,RE               RF=L'JOB                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),OTHNUM      EXTRACT PRODUCT                              
         LA    R1,1(R1,RE)                                                      
         CLM   RF,1,=AL1(6)        CHECK NOT TOO LONG FOR OTHNUM                
         BNH   *+8                                                              
         LA    RF,6                THIS IS THE MAXIMUM PERMISSABLE              
         BCTR  RF,0                                                             
         CLI   TRNTYPE,9           TEST MEDIA TRANSFER                          
         BE    *+12                                                             
         LA    RE,OTHNUM+6         NO - JOB IS AT OTHNUM+6                      
         B     *+14                                                             
         LA    RE,OTHNUM+1(RE)     YES - JOB IS AT OTHNUM+L'PRODUCT             
         MVC   TSARFMMD(L'OTHDATE),OTHDATE  SET MEDIA MONTH/DATE                
         EX    RF,*+8                                                           
         B     BLDSRC02            GO BACK FOR NEXT ELEMENT                     
         MVC   0(0,R1),0(RE)       EXTRACT JOB                                  
                                                                                
         USING SCIELD,R2           R2=A(SUBSIDIARY CASH ELEMENT)                
BLDSRC30 CLI   SCITYPE,SCITGLEV    TEST GST BASIS (IE. NET)                     
         BE    *+12                                                             
         CLI   SCITYPE,SCITTAXP    TEST GST AMOUNT                              
         BNE   BLDSRC02                                                         
         ZAP   TSARFTAX,SCIAMNT    SET GST OR GST BASIS                         
         B     BLDSRC02                                                         
                                                                                
         USING MDTELD,R2                                                        
BLDSRC40 IC    R0,MDTLN            RECEIVABLE LEDGER                            
         AR    R2,R0                                                            
         CLI   MDTEL,0             TEST EOR                                     
         BE    BLDSRCX                                                          
         CLI   MDTEL,UFSELQ        TEST USER FIELD SELECT (SPECIAL#)            
         BE    BLDSRC44                                                         
         CLI   MDTEL,MDTELQ        TEST MEDIA TRANSFER                          
         BNE   BLDSRC40                                                         
         MVC   SRCWORK,SPACES                                                   
         MVC   SRCWORK(L'PRODUL),PRODUL                                         
         LA    R1,SRCWORK+L'PRODUL                                              
         XR    RE,RE                                                            
         IC    RE,PRODBLEN         RE=L'CLIENT/PRODUCT                          
         XR    RF,RF                                                            
         IC    RF,PRODALEN         RF=L'CLIENT                                  
         SR    RE,RF               RE=L'PRODUCT                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),MDTCLI                                                   
         LA    R1,1(RF,R1)                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),MDTPRD                                                   
         LA    R1,1(RE,R1)                                                      
         IC    RE,PRODCLEN         RE=L'CLIENT/PRODUCT/JOB                      
         IC    RF,PRODBLEN         RF=L'CLIENT/PRODUCT                          
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),MDTCLI                                                   
         MVC   0(L'MDTJOB,R1),MDTJOB                                            
         MVC   TSARFMMD(L'MDTMOS),MDTMOS                                        
         MVI   TSARFMMD+L'MDTMOS,0 CLEAR DAY                                    
         B     BLDSRC40                                                         
                                                                                
         USING UFSELD,R2                                                        
BLDSRC44 MVC   TSARFOTH,SPACES                                                  
         XR    R1,R1                                                            
         IC    R1,UFSLN                                                         
         SH    R1,=Y(UFSLN1Q+1)                                                 
         CLM   R1,1,=AL1(L'TSARFOTH-1)                                          
         BNH   *+8                                                              
         LA    R1,L'TSARFOTH-1                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TSARFOTH(0),UFSDATA                                              
         B     BLDSRC40                                                         
*&&                                                                             
BLDSRCX  DS    0H                                                               
*&&US                                                                           
         CLC   SRCWORK,SPACES      TEST IF SOURCE ACCOUNT WAS EXTRACTED         
         BE    *+12                                                             
         OI    TSARIND2,TSARJEST   SOURCE A/C IS GENUINE JOB/ESTIMATE           
         B     *+14                                                             
         L     R1,AIOBUFF          IF NOT - SET CONTRA ACCOUNT                  
         MVC   SRCWORK,TRNKCUNT-TRNRECD(R1)                                     
*&&                                                                             
         B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD TOTALS HEADINGS AND AMOUNTS                                   *         
***********************************************************************         
                                                                                
         USING TOTTABD,R4                                                       
BLDTOT   TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRIDS                   
         BNZ   BLDTOTX             YES                                          
         ST    R1,FULL             FULL=A(TOTALS HEADLINE)                      
         LR    R2,R1                                                            
         MVC   DMCB+4(3),=X'D90616'  OVERLAY TOTALS/PFK SCREEN                  
         MVI   DMCB+7,TWASCRTP                                                  
         GOTO1 VCALLOV,DMCB,(0,(R2))                                            
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R3,FULL             A(TOTALS HEADLINE)                           
         XC    L'FVIHDR(TOTLINEL-L'FVIHDR,R3),L'FVIHDR(R3)                      
         LA    R3,TOTLINEL(R3)     A(TOTALS PRINTLINE)                          
         XC    L'FVIHDR(TOTLINEL-L'FVIHDR,R3),L'FVIHDR(R3)                      
         LA    R3,TOTALS                                                        
         L     R4,AACTTOT          R4=A(ACTION TOTALS)                          
*&&UK                                                                           
         USING CURTABD,R2                                                       
         LA    R2,COMPCURT         R2=A(AGENCY CURRENCY ENTRY)                  
         L     RF,FULL                                                          
         LA    RF,TOTLINEL(RF)                                                  
         NI    FVATRB-FVIHDR(RF),FF-FVAHIGH  TOTALS NOT HIGH INTENSITY          
         TM    SAFCIND1,SAFCI1ON+SAFCI1SC  TEST ON & 1 FOREIGN CURRENCY         
         BNO   BLDTOT02                                                         
         OI    FVATRB-FVIHDR(RF),FVAHIGH  FOREIGN TOTS HIGH INTENSITY           
         LA    R2,FORECURT         USE FOREIGN CURRENCY ENTRY                   
         LA    R3,CURTOTS          USE FOREIGN CURRENCY TOTALS                  
         B     BLDTOT08                                                         
BLDTOT02 TM    SCURIND1,SCURITOT   TEST SECOND CURRENCY TOTALS                  
         BZ    BLDTOT08                                                         
         OI    FVATRB-FVIHDR(RF),FVAHIGH  SECOND TOTS HIGH INTENSITY            
         TM    SCURIND1,SCURIOMT   TEST O/LAY MAINTAINS 2ND CURR TOTALS         
         BO    BLDTOT06                                                         
         L     R3,ASCUTOTS         NO - BUILD SECOND CURRENCY TOTALS            
         LA    R2,TOTALS           FROM PRIMARY CURRENCY TOTALS                 
         LA    R0,SCUTOTL/L'TOTALS                                              
         MVC   WORK(L'CURTCUR),COMPCURT+(CURTCUR-CURTABD)                       
         MVC   WORK+L'CURTCUR(L'CURTCUR),SCNDCURT+(CURTCUR-CURTABD)             
BLDTOT04 GOTO1 VCASHVAL,DMCB,(X'80',0(R2)),(X'28',0),WORK,0,0,0                 
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   0(L'TOTALS,R3),12(8,R1)                                          
         LA    R2,L'TOTALS(R2)                                                  
         LA    R3,L'TOTALS(R3)                                                  
         BCT   R0,BLDTOT04                                                      
                                                                                
BLDTOT06 LA    R2,SCNDCURT         USE SECOND CURRENCY ENTRY                    
         L     R3,ASCUTOTS         USE SECOND CURRENCY TOTALS                   
*&&                                                                             
BLDTOT08 CLI   TOTTABD,X'FF'       TEST EOT                                     
         BE    BLDTOTX                                                          
         XR    RF,RF                                                            
         ICM   RF,1,TOTSDIS        TEST SCREEN TOTAL                            
         BZ    BLDTOT14            NO - GET NEXT TOTAL                          
*&&UK                                                                           
         TM    TOTSDIS,TOTSALT     TEST ALTERNATE TOTAL                         
         BZ    BLDTOT10                                                         
         TM    SAFCIND1,SAFCIALT                                                
         BO    *+12                                                             
         LA    R4,TOTTABL(R4)      IGNORE THIS TOTAL                            
         LA    R3,L'TOTALS(R3)                                                  
         IC    RF,TOTSDIS                                                       
         N     RF,=AL4(FF-TOTSALT)                                              
BLDTOT10 DS    0H                                                               
*&&                                                                             
         A     RF,FULL                                                          
         MVC   LAREADDR,TOTSTXT    S(TOTAL TEXT IN WORKD)                       
         EX    0,LARE                                                           
         MVC   0(TOTTXTL,RF),0(RE)                                              
         LA    RF,TOTLINEL(RF)                                                  
*&&UK                                                                           
         TM    COMPSTA6,CPYSFMCR+CPYSFOCR                                       
         BZ    BLDTOT12                CURRENCY AGENCY PROBLEM                  
         TM    TOTSDIS,TOTSALT     TEST ALTERNATE TOTAL                         
         BO    BLDTOT14                                                         
         CURED (P8,0(R3)),(TOTAMTL,(RF)),CURTABD,ALIGN=LEFT,MINUS=YES           
         XR    RF,RF               REFRESH RF                                   
         IC    RF,TOTSDIS                                                       
         A     RF,FULL                                                          
         LA    RF,TOTLINEL+TOTAMTL-1(RF)                                        
         LR    RE,RF                                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RE,RF                                                            
         CH    RE,=AL2(L'CURTCUR)                                               
         BNH   *+10                                                             
         MVC   2(L'CURTCUR,RF),CURTCUR                                          
         B     BLDTOT14                                                         
*&&                                                                             
BLDTOT12 CURED (P8,0(R3)),(TOTAMTL,(RF)),2,ALIGN=LEFT,FLOAT=-                   
                                                                                
BLDTOT14 DS    0H                                                               
*&&UK                                                                           
         TM    TOTSDIS,TOTSALT     TEST ALTERNATE TOTAL                         
         BZ    BLDTOT16                                                         
         TM    SAFCIND1,SAFCIALT                                                
         BZ    BLDTOT16                                                         
         CURED (P8,0(R3)),(TOTAMTL,(RF)),CURTABD,ALIGN=LEFT,MINUS=YES           
         LA    R3,L'TOTALS(R3)     IGNORE NEXT TOTAL                            
         LA    R4,TOTTABL(R4)                                                   
*&&                                                                             
BLDTOT16 LA    R3,L'TOTALS(R3)                                                  
         LA    R4,TOTTABL(R4)                                                   
         B     BLDTOT08                                                         
*&&UK*&& DROP  R2                                                               
                                                                                
BLDTOTX  GOTO1 ABLDPFK             BUILD PFKEY LINE                             
         GOTO1 AXMTSCR             TRANSMIT ALL FIELDS                          
         B     ROUTE                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF NUMBERS OF TSAR RECORDS TO BE DISPLAYED ACCORDING TO  *         
* SCROLL AND FILTER PARAMETERS                                        *         
***********************************************************************         
                                                                                
DISPLAY  XC    TEMP,TEMP           CLEAR TEMP (FOR DISLCNT & DISLIST)           
         TM    DISIND,DISIRST      TEST RESTART FROM BEGINNING                  
         BNO   DISP02                                                           
         MVC   SCRLAMT,=AL2(SCRMAXI)                                            
         MVI   SCRLDIR,SCRUP                                                    
         B     DISP08              ALWAYS SCROLL IN THIS CASE                   
                                                                                
DISP02   TM    DISIND,DISINCOL     TEST NEW DISPLAY COLUMNS                     
         BNZ   DISP26              DON'T SCROLL, RE-BUILD DISPLAY               
         TM    DISIND,DISIFFLT     TEST FORCE RE-FILTERING OF DISLIST           
         BNO   DISP08                                                           
         LH    R0,DISLCNT          R0=COUNT OF RECORDS IN DISLIST               
         LTR   R0,R0                                                            
         BZ    DISP08                                                           
         XC    TEMP(DISLISTL),TEMP CLEAR TEMP FOR RE-FILTERED DISLIST           
         LA    R2,DISLIST          R2=A(DISLIST)                                
         XR    R3,R3               R3 COUNTS RE-FILTERED TEMP RECORDS           
DISP04   MVC   DISNUM,0(R2)        SET RECORD NUMBER AND GET RECORD             
         GOTO1 ATSARGET,DISNUM                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AFILTER             FILTER THE RECORD                            
         BNE   DISP06              FAILS - GET NEXT DISLIST RECORD              
         LA    RF,TEMP(R3)         PASSES - INDEX INTO TEMP LIST                
         MVC   0(2,RF),DISNUM      AND SET THE NUMBER                           
         LA    R3,2(R3)            ADVANCE INDEX INTO TEMP LIST                 
DISP06   LA    R2,2(R2)            R2=A(NEXT DISLIST RECORD NO.)                
         BCT   R0,DISP04           DO FOR NUMBER OF RECORDS IN DISLIST          
         SRL   R3,1                R3=NEW RECORD COUNT*2, SO HALVE IT           
         STH   R3,DISLCNT          AND SET NEW RECORD COUNT                     
         MVC   DISLIST(DISLISTL),TEMP  REFRESH DISLIST                          
                                                                                
DISP08   NI    DISIND,255-DISIBOF-DISIEOF  RESET BOF/EOF INDICATORS             
         CLC   SCRLAMT,=AL2(SCRMAXI)  TEST MAXIMUM SCROLL                       
         BNE   DISP10                                                           
         XC    DISLIST(DISLISTL),DISLIST   CLEAR LIST OF TSAR NUMBERS           
         XC    DISLCNT,DISLCNT     CLEAR COUNT OF RECORDS DISPLAYED             
         LH    R0,=Y(SCRPAGE)                                                   
         STCM  R0,3,SCRLAMT        SET SCROLL TO A PAGE                         
         LH    R2,DISMAX           HIGHEST TSAR RECORD NUMBER                   
         AH    R2,=H'1'                                                         
         CLI   SCRLDIR,SCRDOWN     TEST MAXIMUM SCROLL DOWN                     
         BNE   *+12                                                             
         MVI   SCRLDIR,SCRUP       YES - SET SCROLL UP                          
         B     DISP12                                                           
         XR    R2,R2               SET MINIMUM RECORD NUMBER-1                  
         MVI   SCRLDIR,SCRDOWN     SET SCROLL DOWN                              
         OI    DISIND,DISIRST      INDICATE FIRST PAGE                          
         B     DISP12                                                           
*                                  CLEAR DISLIST IF NO PREVIOUS RECORD          
DISP10   OC    DISLCNT,DISLCNT                                                  
         BNZ   *+10                                                             
         XC    DISLIST(DISLISTL),DISLIST                                        
         MVC   TEMP(DISLISTL),DISLIST                                           
         MVC   TEMP+DISLISTL(L'DISLCNT),DISLCNT                                 
         XR    R0,R0                                                            
         ICM   R0,3,SCRLAMT        R0=SCROLL AMOUNT (NN LINES)                  
         LH    R2,DISLIST          R2=FIRST DISPLAYED RECORD                    
         CLI   SCRLDIR,SCRUP                                                    
         BE    DISP12                                                           
         LH    R2,DISLCNT          SET R2 TO TSAR NUMBER OF LAST                
         LTR   R2,R2               DISPLAYED RECORD                             
         BZ    *+12                                                             
         SLL   R2,1                                                             
         LH    R2,DISLIST-L'DISLIST(R2)                                         
                                                                                
DISP12   CLI   SCRLDIR,SCRUP       TEST SCROLL UP (BACKWARDS)                   
         BNE   DISP14                                                           
         SH    R2,=H'1'            DECREMENT RECORD COUNT                       
         BP    DISP16              GET RECORD IF NOT AT BOF                     
         OI    DISIND,DISIBOF      SET BOF REACHED                              
         B     DISP22                                                           
                                                                                
DISP14   AH    R2,=H'1'            INCREMENT RECORD COUNTER                     
         CH    R2,DISMAX           GET RECORD IF NOT AT EOF                     
         BNH   DISP16                                                           
         OI    DISIND,DISIEOF      SET EOF REACHED                              
         B     DISP22                                                           
                                                                                
DISP16   STH   R2,DISNUM           SET RECORD NUMBER AND GET RECORD             
         GOTO1 ATSARGET,DISNUM                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AFILTER             FILTER THE RECORD                            
         BNE   DISP12              GET NEXT IF FILTERED OUT                     
                                                                                
         LTR   R0,R0               TEST HERE AFTER ACTIVE LOOP                  
         BZ    DISP24              YES - ONE RECORD FOUND IS ENOUGH             
                                                                                
         LH    RE,DISLCNT          RE=NUMBER OF ENTRIES IN DISLIST              
         CLI   SCRLDIR,SCRDOWN     TEST SCROLL UP OR DOWN                       
         BNE   DISP18                                                           
*                                  ADD NUMBER TO END OF LIST                    
         LR    RF,RE                                                            
         CH    RF,=Y(LINES)        SHIFT OFF FIRST ENTRY IF LIST FULL           
         BL    *+12                                                             
         MVC   DISLIST(DISLISTL-L'DISLIST),DISLIST+L'DISLIST                    
         BCTR  RF,0                                                             
         SLL   RF,1                                                             
         LA    RF,DISLIST(RF)      RF=A(NEW LAST ENTRY IN TABLE)                
         B     DISP20                                                           
*                                  ADD NUMBER TO BEGINNING OF LIST              
*                                  SHIFT OFF LAST ENTRY                         
DISP18   MVC   WORK(DISLISTL-L'DISLIST),DISLIST                                 
         MVC   DISLIST+L'DISLIST(DISLISTL-L'DISLIST),WORK                       
         LA    RF,DISLIST          RF=A(NEW FIRST ENTRY IN TABLE)               
                                                                                
DISP20   MVC   0(L'DISNUM,RF),DISNUM                                            
         AH    RE,=H'1'                                                         
         CH    RE,=Y(LINES)        TEST DISLIST FULL                            
         BH    *+8                                                              
         STH   RE,DISLCNT          NO - SET NEW NUMBER OF ENTRIES               
         LTR   R0,R0               TEST HERE AFTER ACTIVE LOOP                  
         BZ    DISP12              YES - DON'T LET R0 GO NEGATIVE               
         BCT   R0,DISP12                                                        
         B     DISP12              GO BACK TO LOOK FOR ONE MORE                 
                                                                                
DISP22   LTR   RE,R0               TEST ANY SCOLLING LEFT TO DO                 
         BZ    DISP24                                                           
         CLI   SCRLDIR,SCRDOWN     TEST SCROLLING DOWN (NOT FIRST)              
         BNE   DISP24                                                           
         TM    DISIND,DISIRST      TEST SCROLL FROM BEGINNING                   
         BNZ   DISP24                                                           
         LH    RF,DISLCNT                                                       
         SR    RF,RE               RF=NUMBER OF ENTRIES REMAINING               
         BNP   DISP24                                                           
         STH   RF,DISLCNT                                                       
         SLL   RE,1                                                             
         LA    RE,DISLIST(RE)      RE=A(SHIFT FROM POSITION)                    
         SLL   RF,1                                                             
         BCTR  RF,0                                                             
         XC    WORK(DISLISTL),WORK                                              
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)       SAVE REMAINDER & SET AS FIRST                
         XC    DISLIST(DISLISTL),DISLIST                                        
         MVC   DISLIST(DISLISTL),WORK                                           
                                                                                
DISP24   NI    DISIND,255-DISIRST-DISIFFLT                                      
         OC    DISLIST(DISLISTL),DISLIST                                        
         BNZ   DISP26                                                           
         MVC   DISLIST(DISLISTL),TEMP                                           
         MVC   DISLCNT,TEMP+DISLISTL                                            
         B     DISP26                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD INPUT SCREEN FROM DISTAB                                      *         
***********************************************************************         
                                                                                
DISP26   GOTO1 ABLDDIS             BUILD HEADS & DISPLAY DISPLACEMENTS          
*MN                                                                             
         LA    R2,DISHEAD+L'DISLLINE                                            
         L     R1,ADISHEAD         A(INPUT HEADLINE)                            
         LA    R3,L'DISHEAD-1                                                   
         LA    R4,L'DISHEAD-1                                                   
         CLI   XACTOLAY,BROLAY                                                  
         BNE   *+16                                                             
         LA    R2,DISHEAD+L'D2SLLINE                                            
         LA    R3,L'D2SLLINE-1                                                  
         LA    R4,L'D2SLLINE-1                                                  
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   L'FVIHDR(0,R1),DISHEAD      MOVE HEADING TO SCREEN               
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         L     R1,ADISHEA2         A(INPUT HEADLINE2)                           
         EX    R4,*+8                                                           
         B     *+10                                                             
*MN      MVC   L'FVIHDR(0,R1),DISHEAD+L'D2SLLINE                                
         MVC   L'FVIHDR(0,R1),0(R2)                                             
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
*MN                                                                             
         L     R1,ADISDET1         A(INPUT DETAIL LINE)                         
         L     RF,ADISTOTS                                                      
         BCTR  RF,0                                                             
         TWAXC (R1),(RF),PROT=Y    CLEAR SCREEN TO TOTALS LINE                  
                                                                                
         L     R3,ADISDET1                                                      
         USING DISLINED,R3         R3=A(FIRST TWA LINE)                         
         XR    R0,R0                                                            
         ICM   R0,3,DISLCNT        R0=NUMBER OF LINES IN TWA                    
         BZ    DISP32                                                           
         XC    FVADDR,FVADDR                                                    
         LA    R2,DISLIST          R2=A(RECORD NUMBERS)                         
DISP28   GOTO1 ATSARGET,(R2)       GET TSAR RECORD AND EXPLODE KEY              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ABLDLIN,DISLINED    DISPLAY THIS RECORD                          
*MN                                                                             
         CLI   XACTOLAY,BROLAY                                                  
         BNE   DISP29                                                           
         TM    D2SLHDR2+(FVATRB-FVIHDR),FVAPROT                                 
         BO    DISP30                                                           
         OC    FVADDR,FVADDR                                                    
         BNZ   DISP30                                                           
         LA    RF,D2SLHDR2                                                      
         ST    RF,FVADDR           SET CURSOR TO 1ST UNPROT MARK FIELD          
         B     DISP30                                                           
*MN                                                                             
DISP29   TM    DISLHDR2+(FVATRB-FVIHDR),FVAPROT                                 
         BO    DISP30                                                           
         OC    FVADDR,FVADDR                                                    
         BNZ   DISP30                                                           
         LA    RF,DISLHDR2                                                      
         ST    RF,FVADDR           SET CURSOR TO 1ST UNPROT MARK FIELD          
DISP30   LA    R3,DISLINEL(R3)     BUMP TO NEXT LINE AND RECORD                 
         LA    R2,2(R2)                                                         
         BCT   R0,DISP28           DO FOR N'ITEMS IN TABLE                      
         OC    FVADDR,FVADDR       IF NO UNPROT MARK FIELDS                     
         BNZ   DISP32                                                           
         LA    R3,MRKSCRH          SET CURSOR TO SCROLL FIELD                   
         ST    R3,FVADDR                                                        
                                                                                
DISP32   MVC   FVMSGNO,=AL2(IAMKTEPA)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         TM    DISIND,DISIEOF+DISIBOF                                           
         BZ    ROUTE                                                            
         MVC   FVMSGNO,=AL2(IAMKTNOM)                                           
         OC    DISLCNT,DISLCNT     TEST ANYTHING DISPLAYED                      
         BNZ   ROUTE                                                            
         MVC   FVMSGNO,=AL2(IANOTRAN)                                           
         LA    R3,MRKOPTH          SET CURSOR TO OPTION FIELD                   
         ST    R3,FVADDR                                                        
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTEND SHORT STATUS ELEMENT                              *         
***********************************************************************         
                                                                                
EXTRSL   XC    ELEMT(TRSLNQ),ELEMT                                              
         USING TRSELD,R2           R2=A(STATUS ELEMENT)                         
         IC    R1,TRSLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMT(0),TRSEL    SAVE EXISTING ELEMENT                          
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),('TRSELQ',AIOBUFF),0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         LA    R2,ELEMT                                                         
         MVI   TRSLN,TRSLNQ        SET NEW LENGTH                               
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AIOBUFF,(R2)                           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
EXTRSLX  B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER TRANSACTIONS ON OPTIONS ENTERED                              *         
***********************************************************************         
                                                                                
FILTER   CLI   XACTOLAY,MROLAY     TEST MEDIA/RECONCILE                         
         BNE   *+14                                                             
         CLC   =X'FFFF',TSARKEY    DROP SPECIAL RECORD                          
         BE    ROUTH                                                            
         XR    RF,RF                                                            
         OC    FILTCHQ,FILTCHQ     OPTION NOT SET                               
         BZ    FILTER08                                                         
         CLI   XACTOLAY,MROLAY     NOT INTERESTED FOR MEDIA/RECONCILE           
         BE    FILTER08                                                         
         LA    RF,BZQ              BZ EXIT IF NOT A CHEQUE                      
         CLI   FILTCHQ,INCLUDE     IE. CHEQUES ONLY                             
         BE    *+8                                                              
         LA    RF,BOQ              BO EXIT IF A CHEQUE                          
         TM    TSARINDS,TSARMCHQ                                                
         EX    RF,*+8                                                           
         B     *+8                                                              
         NOP   ROUTH                                                            
*&&UK                                                                           
         CLI   XACTION,ACTCHEQ     CHEQUE ACTIONS - U.K. ONLY                   
         BNE   FILTER08            NOT A TRUE CHEQUE ACTION                     
         TM    TWAMODE2,TWAM2INV   TEST USER SELECTING INVOICES (CC)            
         BO    FILTER04            YES                                          
         CLI   FILTCHQ,INCLUDE     TEST SHOWING CHEQUES                         
         BE    FILTER08            YES - CARRY ON                               
         TM    TSARIND2,TSARDIFP   TEST EXCHANGE DIFFERENCE POSTING             
         BZ    FILTER02                                                         
         CLC   TSARFCHN,SPACES     TEST CHEQUE NUMBER                           
         BNH   FILTER02                                                         
         CLC   FILTEHN,TSARFCHN    TEST CHEQUE NUMBER                           
         BNE   ROUTH                                                            
         CLC   FILTEHD,TSARFCHD    ELSE - TEST DATE                             
         BNE   ROUTH                                                            
         CLC   FILTEHB,TSARFCHB    TEST BANK U/L/ACCOUNT                        
         BNE   ROUTH                                                            
         B     FILTER08                                                         
FILTER02 CLC   FILTCHN,TSARFCHN    TEST CHEQUE NUMBER (MAY BE SPACES)           
         BNE   ROUTH                                                            
         CLC   FILTCHN,SPACES      WAS CHEQUE NUMBER SET                        
         BE    FILTER08            NO - CARRY ON                                
         CLC   FILTCHD,TSARFCHD    ELSE - TEST DATE                             
         BNE   ROUTH                                                            
         CLC   FILTCHB,TSARFCHB    TEST BANK U/L/ACCOUNT                        
         BNE   ROUTH                                                            
         CLC   FILTSBR,TSARFSBR    TEST CHEQUE SUB-REFERENCE                    
         BNE   ROUTH                                                            
         TM    COMPSTAT,CPYSOROE   TEST COMPANY ON OFFICES                      
         BNO   FILTER08                                                         
         CLC   STSAROFF,TSAROFF    TEST OFFICE                                  
         BNE   ROUTH                                                            
         B     FILTER08                                                         
                                                                                
FILTER04 CLI   FILTCHQ,EXCLUDE     TEST SHOWING INVOICES                        
         BE    FILTER08            YES - CARRY ON                               
         TM    STSARIND,TSARMKQ    TEST INVOICE BEING MARKED                    
         BZ    FILTER06            YES                                          
         USING TSARD,RF                                                         
         L     RF,ATSARBLK                                                      
         LA    R0,CHQMAX                                                        
         LA    R1,FILTCHS                                                       
         CLC   TSRNUM,0(R1)        TEST CHEQUE BELONGS TO THE INVOICE           
         BE    FILTER08            YES - CARRY ON                               
         LA    R1,2(R1)                                                         
         BCT   R0,*-14                                                          
         B     ROUTH               CHEQUE DOESN'T BELONG TO INVOICE             
         DROP  RF                                                               
                                                                                
FILTER06 CP    TSARFREM,PZERO      TEST ANYTHING AVAILABLE FOR MATCHING         
         BE    ROUTH                                                            
         TM    TSARINDS,TSARMKQ    TEST MATCHED/PART UNMATCHED                  
         BZ    FILTER08                                                         
         CLC   TSARCHA,AC@NO       TEST PART UNMATCHED                          
         BE    ROUTH                                                            
*&&                                                                             
FILTER08 OC    OPTMRK,OPTMRK       OPTION NOT SET                               
         BZ    FILTER10                                                         
         LA    RF,BZQ              BZ EXIT IF NOT MARKED CURRENTLY              
         LA    RE,BOQ              BO EXIT IF MARKED ON FILE                    
         CLI   OPTMRK,INCLUDE                                                   
         BE    *+12                                                             
         LA    RF,BOQ              BO EXIT IF MARKED CURRENTLY                  
         LA    RE,BZQ              BZ EXIT IF NOT MARKED ON FILE                
         TM    TSARINDS,TSARMKQ                                                 
         EX    RF,*+8                                                           
         B     *+8                                                              
         NOP   ROUTH                                                            
         TM    TSARINDS,TSARINMQ                                                
         EX    RE,*+8                                                           
         B     *+8                                                              
         NOP   ROUTH                                                            
                                                                                
FILTER10 OC    OPTDATR,OPTDATR                                                  
         BZ    FILTER12                                                         
         CLC   TSARDAT,OPTSDT                                                   
         BL    ROUTH                                                            
         CLC   TSARDAT,OPTEDT                                                   
         BH    ROUTH                                                            
                                                                                
FILTER12 OC    FLTMOS,FLTMOS       OVERLAY MOS FILTER (GR)                      
         BZ    FILTER14                                                         
         CLC   TSARMOS,FLTMOS                                                   
         BNE   ROUTH                                                            
         B     FILTER16            SKIP OPTION MOS FILTER                       
                                                                                
FILTER14 OC    OPTMOSR,OPTMOSR                                                  
         BZ    FILTER16                                                         
         CLC   TSARMOS,OPTSMO                                                   
         BL    ROUTH                                                            
         CLC   TSARMOS,OPTEMO                                                   
         BH    ROUTH                                                            
                                                                                
FILTER16 OC    OPTCON,OPTCON                                                    
         BZ    FILTER18                                                         
         IC    RF,OPTCONXL         RF=EXECUTE L'CONTRA FILTER                   
         LA    R1,BNEQ                                                          
         TM    OPTCONI,OPTINOT     TEST NEGATIVE FILTER                         
         BZ    *+8                                                              
         LA    R1,BEQ                                                           
         LA    R0,L'TSARCON-1      R0=MAX L'CONTRA U/L/ACCOUNT                  
         LA    RE,TSARCON+1        RE=A(CONTRA U/L/ACCOUNT)                     
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R0,*-12                                                          
         CR    R0,RF               TEST WE HAVE ENOUGH TO COMPARE               
         BNL   *+16                                                             
         TM    OPTCONI,OPTINOT     NO - TEST NEGATIVE FILTER                    
         BNZ   ROUTE               YES - INCLUDE                                
         B     ROUTH               NO - EXCLUDE                                 
         EX    RF,*+12                                                          
         EX    R1,*+4                                                           
         NOP   ROUTH                                                            
         CLC   0(0,RE),OPTCON                                                   
                                                                                
FILTER18 OC    OPTSRC,OPTSRC                                                    
         BZ    FILTER20                                                         
         IC    RF,OPTSRCXL                                                      
         LA    R1,BNEQ                                                          
         TM    OPTSRCI,OPTINOT     TEST NEGATIVE                                
         BZ    *+8                                                              
         LA    R1,BEQ                                                           
         EX    RF,*+12                                                          
         EX    R1,*+4                                                           
         NOP   ROUTH                                                            
         CLC   TSARFSAC(0),OPTSRC                                               
                                                                                
FILTER20 OC    FLTAMT,FLTAMT       OVERLAY AMOUNT FILTER (GR)                   
         BZ    FILTER22                                                         
         ZAP   DUB,TSARAMNT                                                     
         NI    DUB+L'DUB-1,X'FE'   ENSURE NOT NEGATIVE                          
         CP    DUB,FLTAMT                                                       
         BNE   ROUTH                                                            
         B     FILTER26            SKIP OPTION AMOUNT FILTER                    
                                                                                
FILTER22 OC    OPTAMT,OPTAMT                                                    
         BZ    FILTER26                                                         
         ZAP   DUB,TSARAMNT                                                     
*&&UK                                                                           
         OC    OPTAMTC,OPTAMTC                                                  
         BZ    FILTER24                                                         
         CLC   OPTAMTC,COMPCURT+(CURTCUR-CURTABD)                               
         BNE   *+14                                                             
         OC    TSARAFCC,TSARAFCC                                                
         BZ    FILTER24                                                         
         CLC   OPTAMTC,TSARAFCC                                                 
         BNE   ROUTH                                                            
         ZAP   DUB,TSARAFCA                                                     
*&&                                                                             
FILTER24 MVC   BYTE,OPTAMTM                                                     
         TM    BYTE,X'08'          TEST MODIFIER BIT FOR +/- AMOUNT             
         BNO   *+12                                                             
         NI    BYTE,X'F7'          TURN BIT OFF                                 
         NI    DUB+L'DUB-1,X'FE'   TURN OFF NEGATIVE BIT IF NECESSARY           
         IC    RF,BYTE                                                          
         CP    DUB,OPTAMT                                                       
         EX    RF,*+4                                                           
         NOP   ROUTH               BNE / BL / BH                                
                                                                                
FILTER26 OC    OPTOFF,OPTOFF                                                    
         BZ    FILTER28                                                         
         LA    R1,BNEQ                                                          
         TM    OPTOFFI,OPTINOT     TEST NEGATIVE                                
         BZ    *+8                                                              
         LA    R1,BEQ                                                           
         CLC   TSAROFF,OPTOFF                                                   
         EX    R1,*+4                                                           
         NOP   ROUTH                                                            
                                                                                
FILTER28 OC    OPTWRK,OPTWRK                                                    
         BZ    FILTER30                                                         
         LA    RF,TSARFWRK         ASSUME SOURCE WORKCODE (IN CPJEL)            
         CLC   ACCOUNT+(ACTKUNT-ACTRECD)(L'PRODUL),PRODUL                       
         BNE   *+8                                                              
         LA    RF,TSAROFF          PRODUCTION - WORKCODE IS IN KEY              
         LA    R1,BNEQ                                                          
         TM    OPTWRKI,OPTINOT     TEST NEGATIVE                                
         BZ    *+8                                                              
         LA    R1,BEQ                                                           
         CLC   0(L'OPTWRK,RF),OPTWRK                                            
         EX    R1,*+4                                                           
         NOP   ROUTH                                                            
                                                                                
FILTER30 OC    OPTCPJ,OPTCPJ                                                    
         BZ    FILTER32                                                         
         LA    R1,BNEQ                                                          
         TM    OPTCPJI,OPTINOT     TEST NEGATIVE                                
         BZ    *+8                                                              
         LA    R1,BEQ                                                           
         IC    RF,OPTCPJXL                                                      
         EX    RF,*+12                                                          
         EX    R1,*+4                                                           
         NOP   ROUTH                                                            
         CLC   TSARFSAC(0),OPTCPJ                                               
                                                                                
FILTER32 OC    OPTAUT,OPTAUT                                                    
         BZ    FILTER34                                                         
         LA    RF,BZQ              BZ EXIT IF NOT AUTH'D                        
         CLI   OPTAUT,INCLUDE                                                   
         BE    *+8                                                              
         LA    RF,BOQ              BO EXIT IF AUTH'D                            
         TM    TSARSTA,TRNSAUTH                                                 
         EX    RF,*+4                                                           
         NOP   ROUTH                                                            
         B     FILTER34                                                         
                                                                                
FILTER34 OC    OPTCHG,OPTCHG                                                    
         BZ    FILTER40                                                         
         TM    TSARINDS,TSARASTQ   TEST ALTERNATIVE STATUS CHANGE (WN)          
         BNO   FILTER36                                                         
         LA    RF,BEQ              BE EXIT IF NOT CHANGED                       
         CLI   OPTCHG,INCLUDE                                                   
         BE    *+8                                                              
         LA    RF,BNEQ             BNE EXIT IF CHANGED                          
         CLC   TSARAST,TSARCHA                                                  
         B     FILTER38                                                         
                                                                                
FILTER36 LA    RF,BNMQ             BNM EXIT IF NOT CHANGED                      
         CLI   OPTCHG,INCLUDE                                                   
         BE    *+8                                                              
         LA    RF,BMQ              BM EXIT IF CHANGED                           
         TM    TSARINDS,TSARMKQ+TSARINMQ                                        
                                                                                
FILTER38 EX    RF,*+4                                                           
         NOP   ROUTH                                                            
         B     FILTER40                                                         
                                                                                
FILTER40 OC    OPTREFR,OPTREFR                                                  
         BZ    FILTER44                                                         
         OC    OPTSRF,OPTSRF       TEST START REFERENCE                         
         BZ    FILTER42                                                         
         IC    R1,OPTSRFXL                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TSARREF(0),OPTSRF                                                
         BL    ROUTH               <START - DROP IT                             
         BE    FILTER42            =START FOR L'EX, TEST <END FOR L'EX          
         OC    OPTERF,OPTERF       TEST END REFERENCE                           
         BZ    ROUTH               NO - 'START' IS ABSOLUTE FOR L'EX            
         B     *+14                                                             
FILTER42 OC    OPTERF,OPTERF                                                    
         BZ    FILTER44                                                         
         IC    R1,OPTERFXL                                                      
         EX    R1,*+8                                                           
         BH    ROUTH                                                            
         CLC   TSARREF(0),OPTERF                                                
                                                                                
FILTER44 OC    OPTADAR,OPTADAR                                                  
         BZ    FILTER46                                                         
         CLC   TSARADAT,OPTADS                                                  
         BL    ROUTH                                                            
         CLC   TSARADAT,OPTADE                                                  
         BH    ROUTH                                                            
                                                                                
FILTER46 OC    OPTHLD,OPTHLD       TEST OPTION SET                              
         BZ    FILTER48                                                         
         LA    RF,BZQ              BZ EXIT IF NOT HELD                          
         CLI   OPTHLD,INCLUDE                                                   
         BE    *+8                                                              
         LA    RF,BOQ              BO EXIT IF HELD                              
         TM    TSARSTA,TRNSHOLD                                                 
         EX    RF,*+4                                                           
         NOP   ROUTH                                                            
                                                                                
FILTER48 OC    OPTSEL,OPTSEL       TEST OPTION SET                              
         BZ    FILTER50                                                         
         LA    RF,BZQ              BZ EXIT IF NOT SELECTED (APPROVED)           
         CLI   OPTSEL,INCLUDE                                                   
         BE    *+8                                                              
         LA    RF,BOQ              BO EXIT IF SELECTED                          
         TM    TSARSTA,TRNSAPPR                                                 
         EX    RF,*+4                                                           
         NOP   ROUTH                                                            
                                                                                
FILTER50 OC    OPTCTD,OPTCTD       TEST OPTION SET                              
         BZ    FILTER52                                                         
         LA    RF,BZQ              BZ EXIT IF NOT CONTRA'D                      
         CLI   OPTCTD,INCLUDE                                                   
         BE    *+8                                                              
         LA    RF,BOQ              BO EXIT IF CONTRA'D                          
         TM    TSARSSTA,TRSSOFFS                                                
         EX    RF,*+4                                                           
         NOP   ROUTH                                                            
                                                                                
FILTER52 OC    OPTREV,OPTREV       TEST OPTION SET                              
         BZ    FILTER54                                                         
*&&US                                                                           
         CLI   XACTOLAY,WHOLAY     TEST WIP/HOLD                                
         BE    FILTER54            REV OPTION IS FOR FILE READ ONLY             
*&&                                                                             
         LA    RF,BZQ              BZ EXIT IF NOT REVERSED                      
         CLI   OPTREV,INCLUDE                                                   
         BE    *+8                                                              
         LA    RF,BOQ              BO EXIT IF REVERSED                          
         TM    TSARSTA,TRNSREV                                                  
         EX    RF,*+4                                                           
         NOP   ROUTH                                                            
                                                                                
FILTER54 OC    OPTREC,OPTREC       TEST OPTION SET                              
         BZ    FILTER56                                                         
         LA    RF,BZQ              BZ EXIT IF NOT RECONCILED                    
         CLI   OPTREC,INCLUDE                                                   
         BE    *+8                                                              
         LA    RF,BOQ              BO EXIT IF RECONCILED                        
         TM    TSARSTA,TRNSBREC                                                 
         EX    RF,*+4                                                           
         NOP   ROUTH                                                            
                                                                                
FILTER56 OC    OPTBAT,OPTBAT       TEST OPTION SET                              
         BZ    FILTER58                                                         
         LA    R1,BNEQ                                                          
         TM    OPTBATI,OPTINOT     TEST NEGATIVE FILTER                         
         BZ    *+8                                                              
         LA    R1,BEQ                                                           
         CLC   TSARBTY,OPTBAT      COMPARE BATCH TYPE                           
         EX    R1,*+4                                                           
         NOP   ROUTH                                                            
                                                                                
FILTER58 OC    OPTSUBR,OPTSUBR                                                  
         BZ    FILTER62                                                         
         OC    OPTSSR,OPTSSR       TEST START SUBREFERENCE                      
         BZ    FILTER60                                                         
         IC    R1,OPTSSRXL                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TSARFOTH(0),OPTSSR                                               
         BL    ROUTH               <START - DROP IT                             
         BE    FILTER60            =START FOR L'EX, TEST <END FOR L'EX          
         OC    OPTESR,OPTESR       TEST END SUBREFERENCE                        
         BZ    ROUTH               NO - 'START' IS ABSOLUTE FOR L'EX            
         B     *+14                                                             
FILTER60 OC    OPTESR,OPTESR                                                    
         BZ    FILTER62                                                         
         IC    R1,OPTESRXL                                                      
         EX    R1,*+8                                                           
         BH    ROUTH                                                            
         CLC   TSARFOTH(0),OPTESR                                               
                                                                                
FILTER62 OC    OPTPND,OPTPND       TEST PENDING FILTER SET                      
         BZ    FILTER64                                                         
         LA    RF,BZQ              BZ EXIT IF NOT PENDING                       
         CLI   OPTPND,INCLUDE                                                   
         BE    *+8                                                              
         LA    RF,BOQ              BO EXIT IF PENDING                           
         TM    TSARINDS,TSARHITQ   TEST PENDING REVERSE/UNREVERSE               
         EX    RF,*+4                                                           
         NOP   ROUTH                                                            
                                                                                
FILTER64 OC    OPTMAT,OPTMAT       TEST OPTION SET                              
         BZ    FILTER68                                                         
         CLI   OPTMAT,INCLUDE      TEST INCLUDING MATCHED                       
         BE    FILTER66                                                         
         TM    TSARINDS,TSARMKQ    EXCLUDING MATCHED - TEST MARKED              
         BNZ   ROUTH               YES - DROP IT                                
         TM    TWAMODE2,TWAM2INV   TEST INVOICE-STYLE CHEQUE OVERLAY            
         BZ    FILTER68                                                         
*&&UK                                                                           
         TM    TSARINDS,TSARMCHQ   TEST MANUAL CHEQUE                           
         BZ    FILTER68                                                         
         CP    TSARFREM,TSARAMNT   TEST REMAINDER IS FULLY INTACT               
         BNE   ROUTH               NO - DROP IT                                 
         CLC   TSARCHA,AC@PENDG    TEST PART MATCHED THIS TIME                  
         BE    ROUTH               YES - DROP IT                                
         B     FILTER68                                                         
*&&                                                                             
FILTER66 TM    TSARINDS,TSARMKQ    INCLUDING MATCHED - TEST MARKED              
         BNZ   FILTER68            YES - CARRY ON                               
         TM    TWAMODE2,TWAM2INV   TEST INVOICE-STYLE CHEQUE OVERLAY            
         BZ    ROUTH                                                            
*&&UK                                                                           
         TM    TSARINDS,TSARMCHQ   TEST MANUAL CHEQUE                           
         BZ    ROUTH                                                            
         CLC   TSARCHA,AC@PENDG    TEST PART MATCHED THIS TIME                  
         BE    FILTER68                                                         
         CP    TSARFREM,TSARAMNT   TEST REMAINDER FULLY INTACT                  
         BE    ROUTH               YES - DROP IT                                
*&&                                                                             
FILTER68 OC    OPTVOI,OPTVOI       TEST OPTION SET                              
         BZ    FILTER70                                                         
         LA    RF,BZQ              BZ EXIT IF NOT VOID                          
         CLI   OPTVOI,INCLUDE                                                   
         BE    *+8                                                              
         LA    RF,BOQ              BO EXIT IF VOID                              
         TM    TSARSSTA,TRSSVOID                                                
         EX    RF,*+4                                                           
         NOP   ROUTH                                                            
                                                                                
FILTER70 OC    OPTDSC,OPTDSC       TEST OPTION SET                              
         BZ    FILTER72                                                         
         LA    RF,BZQ              BZ EXIT IF DISCOUNT TAKEN                    
         CLI   OPTDSC,INCLUDE                                                   
         BE    *+8                                                              
         LA    RF,BOQ              BO EXIT IF DISCOUNT NOT TAKEN                
         TM    TSARIND2,TSARLDSC                                                
         EX    RF,*+4                                                           
         NOP   ROUTH                                                            
                                                                                
FILTER72 OC    OPTCMM,OPTCMM       TEST OPTION SET                              
         BZ    FILTER74                                                         
         LA    RF,BOQ              BO EXIT IF NON-COMMISSIONABLE                
         CLI   OPTCMM,INCLUDE                                                   
         BE    *+8                                                              
         LA    RF,BZQ              BZ EXIT IF COMMISSIONABLE                    
         TM    TSARSTA,TRNSNOCM                                                 
         EX    RF,*+4                                                           
         NOP   ROUTH                                                            
                                                                                
FILTER74 DS    0H                                                               
*&&US                                                                           
         OC    OPTINVR,OPTINVR                                                  
         BZ    FILTER78                                                         
         OC    OPTSINV,OPTSINV     TEST START MEDIA INVOICE NUMBER              
         BZ    FILTER76                                                         
         IC    R1,OPTSINXL                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TSARFINV(0),OPTSINV                                              
         BL    ROUTH               <START - DROP IT                             
         BE    FILTER76            =START FOR L'EX, TEST <END FOR L'EX          
         OC    OPTEINV,OPTEINV     TEST END MEDIA INVOICE NUMBER                
         BZ    ROUTH               NO - 'START' IS ABSOLUTE FOR L'EX            
                                                                                
FILTER76 OC    OPTEINV,OPTEINV                                                  
         BZ    FILTER78                                                         
         IC    R1,OPTEINXL                                                      
         EX    R1,*+8                                                           
         BH    ROUTH                                                            
         CLC   TSARFINV(0),OPTEINV                                              
                                                                                
FILTER78 OC    OPTPUB,OPTPUB       TEST PUBLICATION/ZONE/EDITION                
         BZ    FILTER80                                                         
         IC    R1,OPTPUBXL                                                      
         EX    R1,*+8                                                           
         BNE   ROUTH                                                            
         CLC   TSARCON+1(0),OPTPUB                                              
                                                                                
FILTER80 OC    OPTMMDR,OPTMMDR                                                  
         BZ    FILTER82                                                         
         CLC   TSARFMMD,SPACES                                                  
         BNH   ROUTH                                                            
         LA    R1,L'TSARFMMD-1                                                  
         CLI   TSARFMMD+L'TSARFMMD-1,0                                          
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BL    ROUTH                                                            
         CLC   TSARFMMD(0),OPTSMM                                               
         EX    R1,*+8                                                           
         BH    ROUTH                                                            
         CLC   TSARFMMD(0),OPTEMM                                               
*&&                                                                             
FILTER82 OC    OPTBSDR,OPTBSDR                                                  
         BZ    FILTER83                                                         
         CLC   TSARBSDT,OPTSBS                                                  
         BL    ROUTH                                                            
         CLC   TSARBSDT,OPTEBS                                                  
         BH    ROUTH                                                            
                                                                                
*MN                                                                             
FILTER83 OC    OPTRCDR,OPTRCDR                                                  
         BZ    FILTER84                                                         
         CLC   TSARRCDT,SPACES                                                  
         BE    ROUTH                                                            
         OC    TSARRCDT,TSARRCDT                                                
         BZ    ROUTH                                                            
         GOTO1 VDATCON,DMCB,(1,OPTSREC),(0,WORK)                                
         GOTO1 VDATCON,DMCB,(1,OPTEREC),(0,WORK+6)                              
         CLC   TSARRCDT,WORK                                                    
         BL    ROUTH                                                            
         CLC   TSARRCDT,WORK+6                                                  
         BH    ROUTH                                                            
                                                                                
FILTER84 OC    OPTCLDR,OPTCLDR                                                  
         BZ    FILTER85                                                         
         CLC   TSARCLDT,SPACES                                                  
         BE    ROUTH                                                            
         OC    TSARCLDT,TSARCLDT                                                
         BZ    ROUTH                                                            
         GOTO1 VDATCON,DMCB,(1,OPTSCLR),(0,WORK)                                
         GOTO1 VDATCON,DMCB,(1,OPTECLR),(0,WORK+6)                              
         CLC   TSARCLDT,WORK                                                    
         BL    ROUTH                                                            
         CLC   TSARCLDT,WORK+6                                                  
         BH    ROUTH                                                            
*MN                                                                             
FILTER85 DS    0H                                                               
*&&UK                                                                           
         OC    OPTFXRR,OPTFXRR                                                  
         BZ    FILTER88                                                         
         OC    OPTSFR,OPTSFR       TEST START REFERENCE                         
         BZ    FILTER86                                                         
         IC    R1,OPTSFRXL                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TSARFDIS(0),OPTSFR                                               
         BL    ROUTH               <START - DROP IT                             
         BE    FILTER86            =START FOR L'EX, TEST <END FOR L'EX          
         OC    OPTEFR,OPTEFR       TEST END REFERENCE                           
         BZ    ROUTH               NO - 'START' IS ABSOLUTE FOR L'EX            
         B     *+14                                                             
FILTER86 OC    OPTEFR,OPTEFR                                                    
         BZ    FILTER88                                                         
         IC    R1,OPTEFRXL                                                      
         EX    R1,*+8                                                           
         BH    ROUTH                                                            
         CLC   TSARFDIS(0),OPTEFR                                               
*&&                                                                             
FILTER88 OC    OPTURG,OPTURG       URGENT OPTION                                
         BZ    FILTER90                                                         
         LA    RF,BZQ              BZ EXIT IF NOT URGENT                        
         CLI   OPTURG,INCLUDE                                                   
         BE    *+8                                                              
         LA    RF,BOQ              BO EXIT IF URGENT                            
         TM    TSARSTA,TRNSURG                                                  
         EX    RF,*+4                                                           
         NOP   ROUTH                                                            
                                                                                
FILTER90 OC    OPTALC,OPTALC       ALLOCATED OPTION                             
         BZ    FILTER92                                                         
         LA    RF,BZQ              BZ EXIT IF ALLOCATED                         
         CLI   OPTALC,INCLUDE                                                   
         BE    *+8                                                              
         LA    RF,BOQ              BO EXIT IF UNALLOCATED                       
         TM    TSARIND2,TSARALCR   TEST ALLOCATED RECEIVABLE                    
         EX    RF,*+4                                                           
         NOP   ROUTH                                                            
                                                                                
FILTER92 OC    OPTDUER,OPTDUER                                                  
         BZ    FILTER94                                                         
         CLI   FILTCHQ,INCLUDE     TEST 2-STEP ACTION SHOWING CHEQUES           
         BE    FILTER94            SKIP TEST                                    
         CLC   TSARFDUE,OPTSDU                                                  
         BL    ROUTH                                                            
         CLC   TSARFDUE,OPTEDU                                                  
         BH    ROUTH                                                            
                                                                                
FILTER94 DS    0H                                                               
*&&UK                                                                           
         OC    OPTCUR,OPTCUR                                                    
         BZ    FILTER96                                                         
         CLC   OPTCUR,COMPCURT+(CURTCUR-CURTABD)                                
         BNE   *+14                                                             
         OC    TSARAFCC,TSARAFCC                                                
         BZ    FILTER96                                                         
         CLI   OPTCUR,OPTCUALL     TEST ALL NON-COMPANY CURRENCIES              
         BE    *+14                                                             
         CLC   TSARAFCC,OPTCUR     TEST THIS CURRENCY MATCHES                   
         BNE   ROUTH                                                            
         OC    TSARAFCC,TSARAFCC   TEST FOREIGN CURRENCY HELD                   
         BZ    ROUTH                                                            
*&&                                                                             
FILTER96 OC    OPTCR,OPTCR         TEST OPTION SET                              
         BZ    FILTER98                                                         
         CLC   OPTCR,AC@YES        TEST INCLUDE CR                              
         BE    FILTER98                                                         
         LA    RF,BOQ              BO EXIT IF NOT CR                            
         CLC   OPTCR,AC@ONLY                                                    
         BE    *+8                                                              
         LA    RF,BZQ              BZ EXIT IF CR                                
         TM    TSARSTA,TRNSDR                                                   
         EX    RF,*+8                                                           
         B     *+8                                                              
         NOP   ROUTH                                                            
                                                                                
FILTER98 OC    OPTEPDR,OPTEPDR     INVOICE - EARLIEST PAYMENT DATE              
         BZ    FILTE100                                                         
         CLI   FILTCHQ,INCLUDE     TEST 2-STEP ACTION SHOWING CHEQUES           
         BE    FILTE100            SKIP TEST                                    
*&&UK                                                                           
         CLC   TSARERPD,OPTSEPD                                                 
         BL    ROUTH                                                            
         CLC   TSARERPD,OPTEEPD                                                 
         BH    ROUTH                                                            
*&&                                                                             
FILTE100 OC    OPTUDAR,OPTUDAR     USED (CONTRA'D) DATE                         
         BZ    FILTE102                                                         
         CLC   TSARUSDT,OPTUDS                                                  
         BL    ROUTH                                                            
         CLC   TSARUSDT,OPTUDE                                                  
         BH    ROUTH                                                            
                                                                                
FILTE102 DS    0H                                                               
         B     ROUTE                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
* GENERAL FILTERS                                                     *         
* NTRY - R1=A(RECORD)                                                 *         
***********************************************************************         
                                                                                
GENFILT  DS    0H                                                               
         GOTO1 ASETELAD,(R1)       SET UP ELEMENT ADDRESSES                     
*&&UK                                                                           
         PUSH  USING                                                            
         USING FILTD,FILTVAL                                                    
         TM    FILT1,FILTFC        FOREIGN CURRENCY FILTER                      
         BZ    GENFIL08                                                         
         ICM   R3,15,ATRXEL        ENSURE NOT DIFFERENCE POSTING                
         BZ    GENFIL02                                                         
         TM    TRXSTA1-TRXELD(R3),TRXSXDIF                                      
         BZ    GENFIL02                                                         
         ICM   R3,15,AFFTACUR      TEST ASSOCIATED CURRENCY                     
         BZ    GENFIL08            DON'T FILTER IF NON ASSOCIATED               
         MVC   FULL,FFTDATA-FFTELD(R3)                                          
         B     GENFIL04                                                         
GENFIL02 ICM   R3,15,AAFCEL        TEST AGENCY CURRENCY                         
         BZ    GENFIL08            DON'T FILTER AGENCY CURRENCY                 
         MVC   FULL,AFCCURR-AFCELD(R3)                                          
GENFIL04 L     R2,ACURRTAB         FIND CURRENCY IN TABLE                       
         USING CURTABD,R2                                                       
GENFIL06 CLI   CURTCUR,EOT                                                      
         BE    GENFILTE                                                         
         CLC   CURTCUR,FULL        TEST CURRENCY ALLOWABLE                      
         BE    GENFIL08                                                         
         LA    R2,L'CURRTAB(R2)                                                 
         B     GENFIL06            LOOP UNTIL EOT                               
         DROP  R2                                                               
                                                                                
GENFIL08 TM    FILT1,FILTAC        AGENCY CURRENCY FILTER                       
         BZ    GENFIL10                                                         
         ICM   R3,15,ATRXEL        ENSURE NOT DIFFERENCE POSTING                
         BZ    *+12                                                             
         TM    TRXSTA1-TRXELD(R3),TRXSXDIF                                      
         BO    GENFIL10                                                         
         OC    AAFCEL,AAFCEL       TEST FOREIGN CURRENCY                        
         BZ    GENFILTE            FILTER NON-FOREIGN CURRENCY                  
                                                                                
GENFIL10 TM    FILT1,FILTXD        EXCHANGE DIFFERENCE FILTER                   
         BZ    GENFIL12                                                         
         ICM   R3,15,ATRXEL        TEST DIFFERENCE POSTING                      
         BZ    *+12                                                             
         TM    TRXSTA1-TRXELD(R3),TRXSXDIF                                      
         BO    GENFILTE                                                         
         POP   USING                                                            
*&&                                                                             
GENFIL12 DS    0H                                                               
                                                                                
GENFILTX B     ROUTE                                                            
GENFILTE B     ROUTH               RETURN WITH ERROR                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT LONG INVOICE NUMBER                              *         
***********************************************************************         
                                                                                
BLDINV   DS    0H                                                               
*                                                                               
         CLC   TSARFINV,SPACES     DO WE ALREADY HAVE LONG INV#?                
         BH    BLDINX              YES EXIT                                     
         USING XPYELD,R2                                                        
         ICM   R2,15,AXPYEL                                                     
         BZ    BLDIN10                                                          
         MVC   TSARFINV(L'XPYINV),XPYINV    SAVE TO TSAR                        
         B     BLDINX                                                           
         USING FFTELD,R2                                                        
BLDIN10  ICM   R2,15,AFFTLEL                                                    
         BZ    BLDIN20                                                          
         XR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         C     RE,=F'20'                                                        
         BNH   *+8                                                              
         LA    RE,20                                                            
         BCTR  RE,0                TAKE MAX OF 20 CHAR                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TSARFINV(0),FFTDATA                                              
         B     BLDINX                                                           
BLDIN20  CLC   TSARFINV,SPACES                                                  
         BH    BLDINX                                                           
         L     RF,AIOBUFF                                                       
         LA    RF,TRNKREF-TRNRECD(RF)                                           
         MVC   TSARFINV(L'TSARREF),0(RF)  MOVE TRAN REF AS LONG INV             
BLDINX   B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT MEDIA MOS                                        *         
***********************************************************************         
                                                                                
BLDMOS   DS    0H                                                               
         CLC   TSARFMMD,SPACES     DO WE ALREADY HAVE MOS?                      
         BH    BLDMOSX                                                          
*                                                                               
         USING GDAELD,R2           EXTRACT MEDIA MOS                            
         ICM   R2,15,AGDAMMOS      MEDIA MONTH OF SERVICE                       
         BZ    *+14                                                             
         MVC   TSARFMMD,GDAYYMM                                                 
         B     BLDMOSX                                                          
*                                                                               
         USING MBIELD,R2           EXTRACT MEDIA MOS                            
         ICM   R2,15,AMBIEL                                                     
         BZ    *+14                                                             
         MVC   TSARFMMD(L'MBIMOS),MBIMOS                                        
         B     BLDMOSX                                                          
*                                                                               
*        USING OTHELD,R2           EXTRACT MEDIA MOS                            
*        ICM   R2,15,AOTHEL                                                     
*        BZ    BLDMOSX                                                          
*        MVC   TSARFMMD(L'OTHDATE),OTHDATE                                      
*                                                                               
BLDMOSX  B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT WORKCODE                                         *         
***********************************************************************         
                                                                                
BLDWRKC  DS    0H                                                               
         USING FFTELD,R2                                                        
         ICM   R2,15,AFFTKWRK                                                   
         BZ    BLDWRKX                                                          
         MVC   TSARFWRK,FFTWORK                                                 
BLDWRKX  B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE ACMRKWRK                                                       
       ++INCLUDE ACMRKCCD          CREDITOR/CHEQUE DSECT REQUIRED               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* ACGOBLOCK                                                                     
         PRINT OFF                                                              
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'092ACMRK40   03/18/15'                                      
         END                                                                    
