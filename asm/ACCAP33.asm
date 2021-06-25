*          DATA SET ACCAP33    AT LEVEL 015 AS OF 07/26/17                      
*PHASE T61D33C                                                                  
         TITLE 'TIMESHEET ROUTINES - #3'                                        
* NSHE 025 140300 GET COMPANY STATUS BYTE 9 FOR ADDTRN (EUROPE ONLY)            
* NSHE 029 090704 MERGE US AND UK VERSIONS                                      
* NSHE 030 090804 CHECK WHETHER DAILY TIME IS ON AT VARIOUS 1R LEVELS           
* **** 031 130904 'A' PHASE CARD                                                
* JFOS 032 110705 GET COMPANY STATUS BYTE B                                     
* JFOS 033 100506 SET STATUS FOR GDATMCST DATE ON/OFF                           
* SMAN 034 190309 BR23834L BRING BACK PERIOD CORRECTLY                          
* MPEN 036 170717 <DSRD-15589> EXTEND TIMELINE NARRATIVE TO 200 CHARS           
T61D33   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
***********************************************************************         
* BRANCH INDEX HELD IN HIGH ORDER BYTE OF RF                          *         
***********************************************************************         
         SPACE 1                                                                
ROUT     NMOD1 250,**ROU3**,R7,CLEAR=YES                                        
         USING TIMEGWSD,R9         R9=A(GLOBAL WORKING STORAGE)                 
         L     RC,BCSVRC                                                        
         USING GEND,RC             RC=A(GEND)                                   
         L     R8,ASYSD            R8=A(SYSD)                                   
         USING SYSD,R8                                                          
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     GETPCP               1 - GET PROJECT CONTROL CLIENT POS          
         B     GETINC               2 - GET DEFAULT INCOME ACCOUNT              
         B     GETCAL               3 - GET CALENDAR RECORD                     
         B     GETACT               4 - GET A RECORD                            
         B     GETELS               5 - GET ELEMENT DATA                        
         B     GETLNQS              6 - GET COMBINED LEDGR LEVEL LNGTHS         
         B     GETTAX               7 - GET DEFAULT TAX INFORMATION             
         B     GETERR               8 - GET ERROR MESSAGE                       
         B     CONVERT              9 - CONVERT TIMELS TO FLD TIMELS            
         B     SCROLL              10 - GET # LINES TO SCROLL                   
         B     FILTER              11 - FILTER                                  
         B     CSTCHK              12 - CHECK COSTING ACCOUNT                   
*                                                                               
ROUTL    MVI   BCDUB,0             SET CC LOW                                   
         B     ROUTCC                                                           
ROUTH    MVI   BCDUB,2             SET CC HIGH                                  
         B     ROUTCC                                                           
ROUTE    MVI   BCDUB,1             SET CC EQUAL                                 
ROUTCC   CLI   BCDUB,1                                                          
*                                                                               
ROUTX    XIT1                      EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* GET CLIENT POSITION FOR PROJECT CONTROL                             *         
*                                                                     *         
* EXIT - BCCLIPOS = DEFAULT CLIENT POSITION                           *         
***********************************************************************         
         SPACE 1                                                                
GETPCP   DS    0H                                                               
         MVI   BCCLIPOS,2          DEFAULT CLIENT POSITION FOR 1C               
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACCKEY),BCSPACES                                        
         MVC   BIGKEY(1),CMPY                                                   
         MVC   BIGKEY+1(2),=C'1C'                                               
         TM    BCCPYST3,CPYSPCSJ                                                
         BZ    *+14                                                             
         MVC   BIGKEY+1(2),=C'SJ'                                               
         MVI   BCCLIPOS,1          DEFAULT CLIENT POSITION FOR SJ               
         GOTO1 AGETACT,0                                                        
         BNE   ROUTH                                                            
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* GET DEFAULT INCOME ACCOUNT                                          *         
*                                                                     *         
* NTRY - PARM1 = A(SJ C/U/L/ACCOUNT)                                  *         
*      - PARM2 = A(WORKCODE)                                          *         
*                                                                     *         
* EXIT - CC=EQU - BCINCOME=INCOME ACCOUNT FROM OPT/MAINT OR SKIPS     *         
*      - CC=NEQ - INCOME ACCOUNT IS NOT FOUND/VALID                   *         
***********************************************************************         
         SPACE 1                                                                
GETINC   DS    0H                                                               
         L     RF,0(R1)                                                         
         MVC   BCACCODE,0(RF)                                                   
         L     RF,4(R1)                                                         
         MVC   BCWORK(2),0(RF)                                                  
*                                                                               
         L     R0,AGOBLOCK                                                      
         LH    R1,=Y(GOBLOCKX-GOBLOCKD)                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,AGOBLOCK                                                      
         AH    R0,=Y(GOBLOCKX-GOBLOCKD)                                         
         ST    R0,AGOXBLK          EXTENSION BLOCK                              
         LH    R1,=Y(GOXBLKX-GOXBLOCK)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING GOBLOCKD,R3                                                      
         L     R3,AGOBLOCK                                                      
         MVC   GOADM,DATAMGR                                                    
*&&US*&& MVC   GOAEXT,AGOXBLK      US USES 1ST EXTENSION BLOCK                  
*&&UK*&& MVC   GOABEXT,AGOXBLK     UK USES 2ND EXTENSION BLOCK                  
*                                                                               
         MVC   GOSELCUL,BCACKCPY   CPY/U/L                                      
         LA    RF,BCACKACT         SJ CLIENT CODE                               
         SR    R1,R1                                                            
         IC    R1,BCSJLEV1                                                      
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   GOSELCLI(0),0(RF)                                                
         OC    GOSELCLI,BCSPACES                                                
         LA    RF,1(R1,RF)                                                      
*                                                                               
         SR    R1,R1               SJ PRODUCT CODE                              
         IC    R1,BCSJLEV2                                                      
         SH    R1,=H'1'                                                         
         BM    GETINC10                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BCSPACES                                                 
         BNH   GETINC10                                                         
         EX    R1,*+4                                                           
         MVC   GOSELPRO(0),0(RF)                                                
         OC    GOSELPRO,BCSPACES                                                
         LA    RF,1(R1,RF)                                                      
*                                                                               
         SR    R1,R1               SJ JOB CODE                                  
         IC    R1,BCSJLEV3                                                      
         SH    R1,=H'1'                                                         
         BM    GETINC10                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BCSPACES                                                 
         BNH   GETINC10                                                         
         EX    R1,*+4                                                           
         MVC   GOSELJOB(0),0(RF)                                                
         OC    GOSELJOB,BCSPACES                                                
*                                                                               
         MVC   GOSELWC,BCWORK      WORKCODE                                     
*                                                                               
GETINC10 GOTO1 VGETOPT,DMCB,(R3)                                                
*&&UK                                                                           
         USING GOBBLKD,R3                                                       
         L     R3,AGOXBLK                                                       
         MVC   BCINCOME,GOINCAC    INCOME ACCOUNT FROM OPT/MAINT                
*&&                                                                             
*&&US                                                                           
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLK                                                       
         MVC   BCINCOME,GOICA      INCOME ACCOUNT FROM OPT/MAINT                
         CLC   BCINCOME,BCSPACES   RETURN INCOME ACCOUNT OR SKIPS               
         BH    *+16                                                             
         MVC   BCINCOME(1),CMPY                                                 
         MVC   BCINCOME+1(5),=C'SKIPS'       DEFAULT TO SKIPS IN USA            
*&&                                                                             
         MVC   BIGKEY,BCSPACES                                                  
         MVC   BIGKEY(L'BCINCOME),BCINCOME                                      
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE      INSURE ACCOUNT IS VALID            
         BNE   ROUTH                                                            
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* GET CALENDAR                                                        *         
*                                                                     *         
* NTRY - R1=A(CALENDAR BLOCK) COVERED BY CALD DSECT                   *         
*                                                                     *         
* EXIT - CC=EQU - CALENDAR BLOCK CORRECTLY FILLED IN                  *         
*      - CC=NEQ - ERROR WITH GERROR SET                               *         
***********************************************************************         
         SPACE 1                                                                
GETCAL   DS    0H                                                               
         LR    R5,R1               R5=A(CALENDAR BLOCK)                         
         USING CALD,R5                                                          
         MVI   BCBYTE1,0           USE AS A 'CALENDAR FOUND' FLAG               
*                                                                               
         MVC   BCYYMMDD,CALPYMD    IF PASSED SPECIFIC DATE USE IT               
         TM    CALSTAT,CALYMDQ                                                  
         BO    *+10                                                             
         MVC   BCYYMMDD,BCTODAY3   IF PASSED PERIOD # USE TODAY                 
*                                                                               
         USING CASRECD,R6          R6=A(CALENDAR RECORD)                        
GETC10   LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVI   CASPTYP,CASPTYPQ    X'3E'                                        
         MVI   CASPSUB,CASPSUBQ    X'0C'                                        
         MVC   CASPCPY,CMPY                                                     
         MVC   CASPEDTE,BCYYMMDD                                                
         GOTO1 HIGH                READHI FOR CORRECT YEAR                      
         B     GETC30                                                           
*                                                                               
GETC20   GOTO1 SEQ                                                              
GETC30   CLC   CASPAS(3),KEYSAVE                                                
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACENOCAL)   ERROR - CALENDAR NOT FOUND               
         B     ROUTH                                                            
         CLC   BCYYMMDD,CASPSDTE                                                
         BNL   *+14                                                             
         MVC   GERROR,=AL2(ACENOCAL)   ERROR - GOT NEXT YEARS CALENDAR          
         B     ROUTH                                                            
         CLC   BCYYMMDD,CASPEDTE                                                
         BNH   *+14                                                             
         MVC   GERROR,=AL2(ACENOCAL)   ERROR - GOT LAST YEARS CALENDAR          
         B     ROUTH                                                            
*                                                                               
         CLC   CASPOFC,BCSPACES        ARE WE AT DEFAULT CAL RECORD             
         BE    GETC40                      IF YES - CONTINUE                    
         CLC   CALOFF,BCSPACES         WAS OFFICE SPECIFIED                     
         BNH   GETC20                      NO - SEE IF THERES A DEFLT           
         CLC   CASPOFC,CALOFF          READ FOR OFFICE CALENDAR RECORD          
         BE    GETC50                                                           
         B     GETC20                                                           
*                                                                               
GETC40   GOTO1 GETREC                                                           
*                                                                               
         CLC   CALOFF,BCSPACES     WAS OFFICE SPECIFIED                         
         BNH   GETC60                                                           
         MVC   CASPOFC,CALOFF      READ FOR OFFICE CALENDAR RECORD              
         GOTO1 HIGH                                                             
         CLC   CASPAS,KEYSAVE      IF OFFICE CALENDAR SETUP USE IT              
         BNE   GETC60              ELSE USE DEFAULT CALENDAR                    
GETC50   GOTO1 GETREC                                                           
*                                                                               
GETC60   L     R6,AIO                                                           
         AH    R6,=Y(ACTRFST-ACTRECD)                                           
GETC70   CLI   0(R6),0                                                          
         BE    GETCX                                                            
         CLI   0(R6),TMRELQ        X'87' TS PERIOD RULES ELEMENT                
         BE    GETC90                                                           
         CLI   0(R6),TMPELQ        X'88' TS PERIODS ELEMENT                     
         BE    GETC100                                                          
GETC80   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     GETC70                                                           
*                                                                               
         USING TMRELD,R6                                                        
GETC90   MVC   CALCSTRT,TMRSTART   RETURN CALENDAR START DATE YMD               
         MVC   CALCEND,TMREND      RETURN CALENDAR END DATE YMD                 
         B     GETC80                                                           
*                                                                               
         USING TMPELD,R6                                                        
GETC100  TM    CALSTAT,CALYMDQ     BY PERIOD VS BY DATE                         
         BNO   GETC110                                                          
*                                                                               
         CLC   CALPYMD,TMPSTART    *** GET CALENDAR BY DATE ***                 
         BL    GETC80                                                           
         CLC   CALPYMD,TMPEND                                                   
         BH    GETC80                                                           
         B     GETC120                                                          
*                                                                               
GETC110  CLC   CALPNUM,TMPNUMB     *** GET CALENDAR BY PERIOD ***               
         BNE   GETC80                                                           
         CLC   TMPSTART,BCTODAY3                                                
         BNH   GETC120             IF < OR = TODAY THEN OK                      
         GOTO1 DATCON,DMCB,(1,BCYYMMDD),(0,BCWORK) ELSE BACK UP TO              
         GOTO1 ADDAY,DMCB,BCWORK,BCWORK+6,F'-365'  PREVIOUS YEAR                
         GOTO1 DATCON,DMCB,(0,BCWORK+6),(1,BCYYMMDD)                            
         CLI   BCBYTE1,1                                                        
         BE    GETCX                                                            
         MVI   BCBYTE1,1                                                        
         B     GETC10                                                           
*                                                                               
GETC120  MVC   CALRMTH,TMPMTH      SAVE PERIOD MONTH                            
         MVC   CALRNUM,TMPNUMB     SAVE PERIOD NUMBER                           
         MVC   CALRSTRT,TMPSTART   SAVE PERIOD START DATE                       
         MVC   CALREND,TMPEND      SAVE PERIOD END DATE                         
         MVI   BCBYTE1,X'FF'                                                    
*                                                                               
GETCX    CLI   BCBYTE1,X'FF'       CHECK FLAG IF CALENDAR WAS FOUND             
         BE    ROUTE                                                            
         MVC   GERROR,=AL2(ACENOCAL)   ERROR - CALENDAR NOT FOUND               
         B     ROUTH                                                            
         EJECT                                                                  
***********************************************************************         
* GET A RECORD                                                        *         
*                                                                     *         
* NTRY - AIO MUST BE SET TO A 2000 BYTE I/O AREA                      *         
*      - BIGKEY MUST BE SET FOR READ                                  *         
*                                                                     *         
* EXIT - CC=EQU - BCACCODE=ACCOUNT CODE READ                          *         
*               - ELEMENT EXTRACTION BLOCK FILLED IN                  *         
*      - CC=NEQ - IF RECORD NOT FOUND                                 *         
***********************************************************************         
         SPACE 1                                                                
GETACT   DS    0H                                                               
         MVI   RDUPDATE,C'N'       SET TO NO                                    
         XC    BCBYTE2,BCBYTE2                                                  
         LA    R6,BIGKEY                                                        
         USING ACTRECD,R6          R6=A(ACCOUNT RECORD KEY)                     
         MVC   BCACCODE,BIGKEY                                                  
         LTR   R1,R1                                                            
         BZ    *+12                                                             
         MVI   BCBYTE2,1                                                        
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'       SET TO NO                                    
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   ROUTH                                                            
         CLI   BCBYTE2,1           READ FOR UPDATE?                             
         BNE   *+8                                                              
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVC   BCACCODE,BIGKEY                                                  
         GOTO1 AGETELS                                                          
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* EXTRACT ELEMENT INFORMATION                                         *         
*                                                                     *         
* NTRY - EXTRACTS ELEMENT DATA FROM RECORD IN 'AIO'                   *         
*                                                                     *         
* EXIT - ELEMENT EXTRACT BLOCK FILLED IN                              *         
***********************************************************************         
         SPACE 1                                                                
GETELS   DS    0H                                                               
         LA    R0,BCAVALS          CLEAR ELEMENT EXTRACT BLOCK                  
         LA    R1,BCAVALQ                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING ACCRECD,R6                                                       
         L     R6,AIO                                                           
         MVC   BCACCODE,ACCKEY                                                  
         LA    R6,ACCRFST                                                       
GET100   CLI   0(R6),0             END OF RECORD                                
         BE    ROUTE                                                            
         CLI   0(R6),CPYELQ        X'10' - COMPANY ELEMENT                      
         BE    ELEM10                                                           
         CLI   0(R6),PMDELQ        X'11' - PRODUCTION MEDIA ELEMENT             
         BE    ELEM11                                                           
         CLI   0(R6),WCOELQ        X'12' - WORKCODE ELEMENT                     
         BE    ELEM12                                                           
         CLI   0(R6),LDGELQ        X'14' - LEDGER ELEMENT                       
         BE    ELEM14                                                           
         CLI   0(R6),NAMELQ        X'20' - ACCOUNT NAME                         
         BE    ELEM20                                                           
         CLI   0(R6),PPRELQ        X'24' - PRODUCTION PROFILE                   
         BE    ELEM24                                                           
         CLI   0(R6),JOBELQ        X'26' - PRODUCTION JOB ELEMENT               
         BE    ELEM26                                                           
         CLI   0(R6),SPAELQ        X'2C' - SPECIAL POSTING A/C                  
         BE    ELEM2C                                                           
         CLI   0(R6),RSTELQ        X'30' - RECORD STATUS                        
         BE    ELEM30                                                           
         CLI   0(R6),ABLELQ        X'32' - BALANCE ELEMENT                      
         BE    ELEM32                                                           
         CLI   0(R6),GPNELQ        X'5A' - GENERAL PURPOSE NAME                 
         BE    ELEM5A                                                           
         CLI   0(R6),GDAELQ        X'E5' - GENERAL DATE                         
         BE    ELEME5                                                           
*                                                                               
GETNEXT  SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     GET100                                                           
         EJECT                                                                  
***********************************************************************         
* X'10' COMPANY ELEMENT                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R6                                                        
ELEM10   DS    0H                                                               
         MVC   BCCPYST1,CPYSTAT1   STATUS #1                                    
         MVC   BCCPYST2,CPYSTAT2   STATUS #2                                    
         MVC   BCCPYST3,CPYSTAT3   STATUS #3                                    
         MVC   BCCPYST4,CPYSTAT4   STATUS #4                                    
         CLI   CPYLN,CPYLN2Q                                                    
         BL    ELEM10X                                                          
         MVC   BCCPYST5,CPYSTAT5   STATUS #5                                    
         MVC   BCCPYST6,CPYSTAT6   STATUS #6                                    
         MVC   BCCPYST7,CPYSTAT7   STATUS #7                                    
         MVC   BCCPYST8,CPYSTAT8   STATUS #8                                    
*&&UK*&& MVC   BCCPYCUR,CPYCURR    COMPANY PRIMARY CURRENCY CODE                
         CLI   CPYLN,CPYLN3Q                                                    
         BL    ELEM10X                                                          
         MVC   BCCPYTMS,CPYTMSSD   TMS START DATE                               
*&&UK*&& MVC   BCCPYSEC,CPYCURRS   COMPANY SECONDARY CURRENCY CODE              
         MVC   BCCPYST9,CPYSTAT9   STATUS #9                                    
         MVC   BCCPYSTA,CPYSTATA   STATUS #10                                   
         CLI   CPYLN,CPYLN4Q                                                    
         BL    ELEM10X                                                          
         MVC   BCCPYSTB,CPYSTATB   STATUS #11                                   
*&&US*&& MVC   BCCPYGLM,CPYGLMOA   COMPANY GL MOA                               
ELEM10X  B     GETNEXT                                                          
         SPACE 3                                                                
***********************************************************************         
* X'11' PRODUCTION MEDIA ELEMENT                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING PMDELD,R6                                                        
ELEM11   DS    0H                                                               
*&&US                                                                           
         CLI   PMDLN,PMDLN3Q                                                    
         BL    *+10                                                             
         MVC   BCINCOME,PMDTIN     TIME INCOME ACCOUNT                          
*&&                                                                             
         B     GETNEXT                                                          
         SPACE 3                                                                
***********************************************************************         
* X'12' WORKCODE ELEMENT                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING WCOELD,R6                                                        
ELEM12   DS    0H                                                               
*&&UK                                                                           
         TM    WCOSTAT,WCOSHCOE    TIME TYPE WORKCODE                           
         BNO   *+8                                                              
         OI    BCINDS,BCFLTIME                                                  
*&&                                                                             
*&&US                                                                           
         TM    WCOSTAT2,WCOSART    RATE ELIGIBLE FOR ADJUSTMENT                 
         BZ    *+8                                                              
         OI    BCINDS,BCFLEADJ                                                  
*&&                                                                             
         B     GETNEXT                                                          
         EJECT                                                                  
***********************************************************************         
* X'14' LEDGER ELEMENT                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGELD,R6                                                        
ELEM14   CLI   LDGLN,X'20'         EXIT IF OLD FORMAT                           
         BL    GETNEXT                                                          
         CLI   LDGCPOS,0                                                        
         BE    GETNEXT                                                          
         MVC   BCCLIPOS,LDGCPOS    SAVE DISPLACEMENT TO CLIENT                  
         B     GETNEXT                                                          
         SPACE 3                                                                
***********************************************************************         
* X'20' NAME ELEMENT                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING NAMELD,R6                                                        
ELEM20   MVC   BCACNAME,BCSPACES                                                
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    ELEM20X                                                          
         EX    R1,*+4                                                           
         MVC   BCACNAME(0),NAMEREC ACCOUNT NAME                                 
         OC    BCACNAME,BCSPACES                                                
ELEM20X  B     GETNEXT                                                          
         SPACE 3                                                                
***********************************************************************         
* X'24' PRODUCTION PROFILE ELEMENT                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING PPRELD,R6                                                        
ELEM24   MVC   BCPRCOST,PPRCOST    1C COST ACCOUNT                              
         MVC   BCPROFFC,PPRGAOFF   CLIENT OFFICE                                
         B     GETNEXT                                                          
         EJECT                                                                  
***********************************************************************         
* X'26' PRODUCTION JOB ELEMENT                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING JOBELD,R6                                                        
ELEM26   CLI   JOBLN,JOBLN3Q                                                    
         BL    ELEM26X                                                          
         TM    JOBSTA1,JOBSXJOB                                                 
         BZ    *+8                                                              
         OI    BCINDS,BCFLXJOB     JOB IS AN X-JOB                              
         TM    JOBSTA1,JOBSART                                                  
         BZ    *+8                                                              
         OI    BCINDS,BCFLEADJ     JOB ELIGLIBLE FOR RATE ADJUSTMENT            
ELEM26X  B     GETNEXT                                                          
         SPACE 3                                                                
***********************************************************************         
* X'2C' RECORD STATUS ELEMENT                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING SPAELD,R6                                                        
ELEM2C   CLI   SPATYPE,SPATINCO                                                 
         BNE   *+16                                                             
         MVC   BCSPINC(1),CMPY                                                  
         MVC   BCSPINC+1(14),SPAAULA   SPECIAL INCOME ACCOUNT (FROM 1R)         
         CLI   SPATYPE,SPATANAL                                                 
         BNE   ELEM2CX                                                          
         MVC   BCSPANAL(1),CMPY                                                 
         MVC   BCSPANAL+1(2),=C'12'                                             
         MVC   BCSPANAL+3(12),SPAAULA  SPECIAL ANAL ACCOUNT (FROM SI)           
ELEM2CX  B     GETNEXT                                                          
         SPACE 3                                                                
***********************************************************************         
* X'30' RECORD STATUS ELEMENT                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTELD,R6                                                        
ELEM30   MVC   BCASTAT1,RSTSTAT1   STATUS BYTE #1                               
         MVC   BCASTAT3,RSTSTAT3   STATUS BYTE #3                               
         MVC   BCACOST,RSTCOSTG    COSTING BYTE                                 
         MVC   BCACOSTP,RSTCCTRR   COSTING REPLACE POSITION                     
         MVC   BCACCTR,RSTCCTR     COSTING CENTER                               
         CLI   RSTLN,RSTLN2Q                                                    
         BL    ELEM30X                                                          
         MVC   BCASTAT2,RSTSTAT2   STATUS BYTE #2                               
         MVC   BCASTAT4,RSTSTAT4   STATUS BYTE #4                               
         CLI   RSTLN,RSTLN3Q                                                    
         BL    ELEM30X                                                          
         MVC   BCASTAT5,RSTSTAT5   STATUS BYTE #5                               
         MVC   BCTSNUM,RSTSNUM     TIME SHEET #/REVISION #                      
         MVC   BCDFTASK,RSTDFTSK   DEFAULT TASK CODE                            
*                                                                               
ELEM30X  B     GETNEXT                                                          
         EJECT                                                                  
***********************************************************************         
* X'32' BALANCE ELEMENT                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ABLELD,R6                                                        
ELEM32   OI    BCINDS,BCFLABAL     ACCOUNT HAS BALANCE ELEMENT                  
         B     GETNEXT             AND IS VALID FOR POSTING                     
         SPACE 3                                                                
***********************************************************************         
* X'5A' GENERAL NAME ELEMENT                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING GPNELD,R6                                                        
ELEM5A   LA    RF,BCPRNFLN                                                      
         CLI   GPNTYP,GPNTFST      FIRST OR LAST NAME                           
         BE    *+8                                                              
         LA    RF,BCPRNLLN                                                      
         MVC   1(36,RF),BCSPACES                                                
         SR    R1,R1                                                            
         IC    R1,GPNLN                                                         
         SH    R1,=Y(GPNLNQ)                                                    
         STC   R1,0(RF)            STORE LENGTH OF NAME                         
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   1(0,RF),GPNNME      STORE NAME                                   
         OC    1(36,RF),BCSPACES                                                
ELEM5AX  B     GETNEXT                                                          
         EJECT                                                                  
***********************************************************************         
* X'E5' GENERAL DATE ELEMENT                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING GDAELD,R6                                                        
ELEME5   CLI   GDATYPE,GDATDTIM                                                 
         BNE   ELEME506                                                         
         CLC   BCYYMMDD,GDADATE    IS DATE WITH DAILY TIME RANGE                
         BL    GETNEXT                                                          
         OC    GDADATE2,GDADATE2                                                
         BZ    ELEME502                                                         
         CLC   BCYYMMDD,GDADATE2                                                
         BNL   ELEME504                                                         
ELEME502 OI    BCPRIND,BCPRIDTM    SET DAILY TIME IN ON                         
         B     GETNEXT                                                          
*                                                                               
ELEME504 NI    BCPRIND,X'FF'-BCPRIDTM    TURN DAILY TIME OFF                    
         B     GETNEXT                                                          
                                                                                
ELEME506 CLI   GDATYPE,GDATMCST                                                 
         BNE   GETNEXT                                                          
         CLC   BCYYMMDD,GDADATE    IS DATE WITH MCS TIME RANGE                  
         BL    GETNEXT                                                          
         OC    GDADATE2,GDADATE2                                                
         BZ    ELEME508                                                         
         CLC   BCYYMMDD,GDADATE2                                                
         BNL   ELEME510                                                         
ELEME508 OI    BCPRIND,BCPRIMCT    SET MCS TIME IS ON                           
         B     GETNEXT                                                          
*                                                                               
ELEME510 NI    BCPRIND,X'FF'-BCPRIMCT    TURN MCS TIME OFF                      
         B     GETNEXT                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
* GET COMBINED LEGDER LEVELS (CONVERTS 1,2,2,7 -> 1,3,5,12)           *         
*                                                                     *         
* NTRY = R1=A(START OF LEDGER LEVEL BLOCK)                            *         
*                                                                     *         
* EXIT = CONVERTED LEDGER LEDGER LEVEL BLOCK                          *         
***********************************************************************         
         SPACE 1                                                                
GETLNQS  DS    0H                                                               
         LR    RF,R1               RF=START OF LEDGER LEVEL BLOCK               
         LA    R0,3                R0=# LEVELS-1                                
GETLNQ10 SR    R1,R1                                                            
         ICM   R1,1,0(RF)                                                       
         BZ    GETLNQ20                                                         
         SR    RE,RE                                                            
         ICM   RE,1,1(RF)                                                       
         BZ    GETLNQ20                                                         
         AR    RE,R1                                                            
         STC   RE,1(RF)                                                         
GETLNQ20 LA    RF,1(RF)                                                         
         BCT   R0,GETLNQ10                                                      
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* GET DEFAULT TAX INFORMATION                                         *         
*                                                                     *         
* NTRY - PARM1 = A(SJ C/U/L/ACCOUNT)                                  *         
*      - PARM2 = A(WORKCODE)                                          *         
*                                                                     *         
* EXIT - CC=EQU - BCTAXLOC = DEFAULT TAX LOCALITY (OPTIONAL)          *         
*               - BCTAXWC  = DEFAULT TAX WORKCODE (OPTIONAL)          *         
*               - BCTXREQ  = DEFAULT TAX RULE     (OPTIONAL)          *         
***********************************************************************         
         SPACE 1                                                                
GETTAX   DS    0H                                                               
*&&US                                                                           
         L     RF,0(R1)                                                         
         MVC   BCACCODE,0(RF)                                                   
         L     RF,4(R1)                                                         
         MVC   BCWORK(2),0(RF)                                                  
*                                                                               
         L     R0,AGOBLOCK                                                      
         LH    R1,=Y(GOBLOCKX-GOBLOCKD)                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,AGOBLOCK                                                      
         AH    R0,=Y(GOBLOCKX-GOBLOCKD)                                         
         ST    R0,AGOXBLK          EXTENSION BLOCK                              
         LH    R1,=Y(GOXBLKX-GOXBLOCK)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING GOBLOCKD,R3                                                      
         L     R3,AGOBLOCK                                                      
         MVC   GOADM,DATAMGR                                                    
         MVC   GOAEXT,AGOXBLK      EXTENSION BLOCK                              
*                                                                               
         MVC   GOSELCUL,BCACKCPY   CPY/U/L                                      
         LA    RF,BCACKACT         SJ CLIENT CODE                               
         SR    R1,R1                                                            
         IC    R1,BCSJLEV1                                                      
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   GOSELCLI(0),0(RF)                                                
         OC    GOSELCLI,BCSPACES                                                
         LA    RF,1(R1,RF)                                                      
*                                                                               
         SR    R1,R1               SJ PRODUCT CODE                              
         IC    R1,BCSJLEV2                                                      
         SH    R1,=H'1'                                                         
         BM    GETINC10                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BCSPACES                                                 
         BNH   GETINC10                                                         
         EX    R1,*+4                                                           
         MVC   GOSELPRO(0),0(RF)                                                
         OC    GOSELPRO,BCSPACES                                                
         LA    RF,1(R1,RF)                                                      
*                                                                               
         SR    R1,R1               SJ JOB CODE                                  
         IC    R1,BCSJLEV3                                                      
         SH    R1,=H'1'                                                         
         BM    GETINC10                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BCSPACES                                                 
         BNH   GETINC10                                                         
         EX    R1,*+4                                                           
         MVC   GOSELJOB(0),0(RF)                                                
         OC    GOSELJOB,BCSPACES                                                
*                                                                               
         MVC   GOSELWC,BCWORK      WORKCODE                                     
*                                                                               
GETTAX10 GOTO1 VGETOPT,DMCB,(R3)                                                
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLK                                                       
*                                                                               
         XC    BCTAXBLK,BCTAXBLK   CLEAR TAX BLOCK                              
         CLI   GOTAX,C'Y'          ELIGIBLE FOR TAX                             
         BNE   *+10                                                             
         MVC   BCTAXREQ,GOTAX                                                   
         CLC   GOTXLOC,BCSPACES    TAX LOCALITY                                 
         BNH   *+10                                                             
         MVC   BCTAXLOC,GOTXLOC                                                 
         CLC   GOTXWC,BCSPACES     TAX WORKCODE                                 
         BNH   *+10                                                             
         MVC   BCTAXWC,GOTXWC                                                   
*&&                                                                             
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* GET ERROR MESSAGE                                                   *         
*                                                                     *         
* NTRY - R1 = A(ERROR FLAGS)                                          *         
*                                                                     *         
* EXIT - CC=EQU - BCWORK = ERROR MESSAGE                              *         
***********************************************************************         
         SPACE 1                                                                
GETERR   DS    0H                                                               
         STCM  R1,3,BCHALF                                                      
         MVC   BCWORK,BCSPACES                                                  
*                                                                               
         LA    R0,ERRTABQ          # TABLE ENTRIES                              
         LA    R1,ERRTAB           START OF ERROR TABLE                         
GETERR5  MVC   HALF,0(R1)                                                       
         NC    HALF,BCHALF                                                      
         BNZ   GETERR10                                                         
         LA    R1,L'ERRTAB(R1)                                                  
         BCT   R0,GETERR5                                                       
         B     ROUTE                                                            
*                                                                               
GETERR10 SR    R3,R3                                                            
         ICM   R3,3,2(R1)                                                       
         BZ    ROUTE                                                            
         GOTO1 GETTXT,BCDMCB,(R3),(60,BCWORK),(C'E',DMCB),0,(X'08',0),0         
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* CONVERT INPUT DETAIL TIMELS TO FLD TIMELS                           *         
*                                                                     *         
* NTRY - R1=A(CLUSTER OF TIMELS) - TYPE #1-#3                         *         
*                                                                     *         
* EXIT - CC=EQU - BCELEM CONTAINS FLD TIMEL #4                        *         
*      - CC=NEQ - SOMETHING IS WRONG                                  *         
***********************************************************************         
         SPACE 1                                                                
CONVERT  DS    0H                                                               
         USING TIMELD,R3                                                        
         LR    R3,R1               R3=A(TIME CLUSTER)                           
*                                                                               
         LA    R6,BCELEM                                                        
         XC    BCELEM,BCELEM                                                    
         MVI   BCELEM+(TIMEL-TIMELD),TIMELQ                                     
         MVI   BCELEM+(TIMLN-TIMELD),TIMFITEM-TIMELD                            
         MVI   BCELEM+(TIMETYP-TIMELD),TIMEFLD                                  
         LA    R6,(TIMFITEM-TIMELD)(R6)                                         
*                                                                               
CONV100  ST    R3,BCADDR                                                        
         CLI   0(R3),0                                                          
         BE    CONVX                                                            
         CLI   TIMETYP,TIMEINP                                                  
         BNE   CONV200                                                          
         MVC   BCELEM+(TIMFNUM-TIMELD)(2),TIMLINE#                              
         BAS   RE,CONVRATE                                                      
         BAS   RE,APPEND                                                        
         BAS   RE,CONVINC                                                       
         BAS   RE,APPEND                                                        
         B     CONVNXT                                                          
*                                                                               
CONV200  DS    0H                                                               
*&&US                                                                           
         CLI   TIMETYP,TIMETAX                                                  
         BNE   CONV300                                                          
         BAS   RE,CONVBAS          CONVERT BASIS AMOUNT                         
         BAS   RE,APPEND                                                        
         BAS   RE,CONVLOC          CONVERT LOCALITY                             
         BAS   RE,APPEND                                                        
         BAS   RE,CONVTWC          CONVERT TAX WORKCODE                         
         BAS   RE,APPEND                                                        
         B     CONVNXT                                                          
*&&                                                                             
*                                                                               
CONV300  CLI   TIMETYP,TIMENAR                                                  
         BNE   CONVNXT                                                          
         BAS   RE,CONVNAR                                                       
         BAS   RE,APPEND                                                        
         B     CONVNXT                                                          
*                                                                               
CONVNXT  SR    R1,R1               BUMP TO NEXT CLUSTER ELEMENT                 
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     CONV100                                                          
*                                                                               
CONVX    B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* MOVE FIELD                                                          *         
***********************************************************************         
         SPACE 1                                                                
APPEND   NTR1                                                                   
         SR    RF,RF                                                            
         ICM   RF,1,BCXFLDLN                                                    
         BZ    ROUTE                                                            
         SH    RF,=H'1'                                                         
         EX    RF,*+4                                                           
         MVC   0(0,R6),BCXFLD                                                   
         LA    R6,1(RF,R6)                                                      
         SR    RE,RE                                                            
         IC    RE,BCELEM+(TIMFMINI-TIMELD)      BUMP MINI ELEM COUNT            
         LA    RE,1(RE)                                                         
         STC   RE,BCELEM+(TIMFMINI-TIMELD)                                      
         IC    RE,BCELEM+(TIMLN-TIMELD)                                         
         LA    RE,1(RF,RE)                                                      
         STC   RE,BCELEM+(TIMLN-TIMELD)                                         
         XIT1  REGS=(R6)                                                        
         EJECT                                                                  
***********************************************************************         
* CONVERT RATE                                                        *         
***********************************************************************         
         SPACE 1                                                                
CONVRATE NTR1                                                                   
         XC    BCXFLD,BCXFLD                                                    
*                                                                               
         USING TIMELD,R3                                                        
         L     R3,BCADDR           INPUT DETAILS ELEM                           
         CLI   TIMLN,TIMILN2Q      LENGTH FOR BILLABLE INPUT                    
         BNE   ROUTH                                                            
         MVC   BCWORK,BCSPACES                                                  
         MVI   BCWORK,C'*'                                                      
         TM    TIMRBSTA,TIMRBADJ   DEFAULT ADJUSTMENT RATE                      
         BNO   *+8                                                              
         MVI   BCWORK,C'A'                                                      
         CURED TIMRATE,(10,BCWORK+1),2,ALIGN=LEFT                               
         LA    R1,BCWORK                                                        
         TM    TIMRBSTA,TIMRORAT   WAS RATE OVERRIDDEN                          
         BNO   *+8                                                              
         LA    R1,1(R1)                                                         
         MVI   BCXFLDLN,13                                                      
         MVC   BCXFLD#,=Y(FLDRATE)                                              
         MVC   BCXFIELD(10),0(R1)                                               
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERT INCOME ACCOUNT                                              *         
***********************************************************************         
         SPACE 1                                                                
CONVINC  NTR1                                                                   
         XC    BCXFLD,BCXFLD                                                    
*                                                                               
         USING TIMELD,R3                                                        
         L     R3,BCADDR           INPUT DETAILS ELEM                           
         CLI   TIMLN,TIMILN2Q      LENGTH FOR BILLABLE INPUT                    
         BNE   ROUTH                                                            
         CLC   TIMINC,BCSPACES                                                  
         BNH   ROUTH                                                            
         MVC   BCWORK,BCSPACES                                                  
         LA    R1,BCWORK                                                        
         TM    TIMRBSTA,TIMROINC   INCOME ACCOUNT OVERRIDDEN                    
         BO    *+12                                                             
         MVI   0(R1),C'*'                                                       
         LA    R1,1(R1)                                                         
         MVC   0(L'TIMINC,R1),TIMINC                                            
         MVI   BCXFLDLN,18                                                      
         MVC   BCXFLD#,=Y(FLDINC)                                               
         MVC   BCXFIELD(15),0(R1)                                               
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERT TAX INFO                                                    *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
CONVBAS  NTR1                                                                   
         XC    BCXFLD,BCXFLD                                                    
*                                                                               
         USING TIMELD,R3                                                        
         L     R3,BCADDR           INPUT DETAILS ELEM                           
         MVC   BCWORK,BCSPACES                                                  
         CURED TIMTBAS,(9,BCWORK),2,FLOAT=-                                     
         MVI   BCXFLDLN,12                                                      
         MVC   BCXFLD#,=Y(FLDBASIS)                                             
         MVC   BCXFIELD(9),BCWORK                                               
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERT TAX LOCALITY                                                *         
***********************************************************************         
         SPACE 1                                                                
CONVLOC  NTR1                                                                   
         XC    BCXFLD,BCXFLD                                                    
*                                                                               
         USING TIMELD,R3                                                        
         L     R3,BCADDR                                                        
         CLC   TIMTLOC,BCSPACES                                                 
         BNH   ROUTE                                                            
         XC    BCXFLD,BCXFLD                                                    
         MVI   BCXFLDLN,11                                                      
         MVC   BCXFLD#,=Y(FLDLOCAL)                                             
         MVC   BCXFIELD(L'TIMTLOC),TIMTLOC                                      
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERT TAX WORKCODE                                                *         
***********************************************************************         
         SPACE 1                                                                
CONVTWC  NTR1                                                                   
         USING TIMELD,R3                                                        
         L     R3,BCADDR                                                        
         XC    BCXFLD,BCXFLD                                                    
         CLC   TIMTWC,BCSPACES                                                  
         BNH   ROUTE                                                            
         MVI   BCXFLDLN,5                                                       
         MVC   BCXFLD#,=Y(FLDTAXWC)                                             
         MVC   BCXFIELD(L'TIMTWC),TIMTWC                                        
         B     ROUTE                                                            
         DROP  R3                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* CONVERT NARRATIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
CONVNAR  NTR1                                                                   
         USING TIMELD,R3                                                        
         L     R3,BCADDR                                                        
         XC    BCXFLD,BCXFLD                                                    
*                                                                               
         SR    R1,R1               R1=LENGTH ELEMENT DATA                       
         IC    R1,TIMLN                                                         
         SH    R1,=Y(TIMHLNQ+1)                                                 
         BM    ROUTE                                                            
         CHI   R1,L'STCTCNAR-1     LIMIT TO 60 CHARS                            
         BNH   *+8                                                              
         LHI   R1,L'STCTCNAR-1                                                  
         MVC   BCXFIELD(0),TIMNARR                                              
         EX    R1,*-6                                                           
         LA    R1,4(R1)                                                         
         STC   R1,BCXFLDLN                                                      
         MVC   BCXFLD#,=Y(FLDNARR)                                              
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS SCROLL FIELD                                                *         
*                                                                     *         
* NTRY - R1=A(SCROLL FIELD)                                           *         
*                                                                     *         
* EXIT - CC=EQU - BCHALF HI ORDER BYTE  = SCRLPAGE                    *         
*                                       = SCRLHALF                    *         
*               - BCHALF LOW ORDER BYTE = # LINES TO SCROLL           *         
*      - CC=NEQ - INVALID INPUT                                       *         
***********************************************************************         
         SPACE 1                                                                
SCROLL   DS    0H                                                               
         XC    BCHALF,BCHALF                                                    
*                                                                               
         LR    R2,R1                                                            
         MVI   BCIFMAX,4                                                        
         GOTO1 AFVAL,(R2)                                                       
         BH    ROUTH                                                            
*                                                                               
         CLI   BCIFLDH+5,0         DEFAULT IS PAGE                              
         BNE   *+12                                                             
         OI    BCHALF,SCRLPAGE                                                  
         B     ROUTE                                                            
*                                                                               
         SR    R3,R3                                                            
         IC    R3,BCIFLDH+5                                                     
         SH    R3,=H'1'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   BCIFLD(0),AC@PAGE                                                
         BNE   *+12                                                             
         OI    BCHALF,SCRLPAGE                                                  
         B     ROUTE                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   BCIFLD(0),AC@HALF                                                
         BNE   *+12                                                             
         OI    BCHALF,SCRLHALF                                                  
         B     ROUTE                                                            
         LA    R3,1(R3)                                                         
         GOTO1 CASHVAL,BCDMCB,(C'N',BCIFLD),(R3)                                
         CLI   BCDMCB,0                                                         
         BNE   SCRLERRX                                                         
         CLC   BCDMCB+4(4),=F'50'  MAX SCROLL AMOUNT                            
         BH    SCRLERRX                                                         
         MVC   BCHALF+1(1),BCDMCB+7                                             
         B     ROUTE                                                            
*                                                                               
SCRLERRX MVC   GERROR,=AL2(ACEINV) INVALID INPUT                                
         B     ROUTH                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER ROUTINE                                                      *         
* (ACCAP42 HAS ITS OWN FILTER ROUT THAT NEEDS TO REMAIN SAME AS THIS) *         
*        P1 - A(TIMELD)                                               *         
*        P2 - A(TSAR REC)                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
         USING TSARRECD,R6                                                      
FILTER   DS    0H                                                               
         MVC   BCBYTE1,0(R1)                                                    
         TM    BCBYTE1,X'80'        ARE WE DOING A GENERAL COMPARE?             
         BO    FILT70                                                           
         L     R3,0(R1)                                                         
         L     R6,4(R1)                                                         
*                                                                               
         OC    BCFLTS,BCFLTS       CHECK IF ANYTHING BEING FILTERED?            
         BZ    *+12                                                             
         TM    TRKSTAT,TRKSSAVE+TRKSSAV#        SKIP IF SAVED                   
         BNZ   ROUTH                                                            
*                                                                               
         CLI   BCFLTTYP,0          *** FILTER BY TYPE OF TIME ***               
         BE    FILT10                                                           
         CLI   BCFLTTYP,TIMTCR                                                  
         BH    *+18                                                             
         CLC   BCFLTTYP,TIMTTYP                                                 
         BNE   ROUTH                                                            
         B     FILT10                                                           
         CLI   TIMTTYP,TIMTCN                                                   
         BL    ROUTH                                                            
*                                                                               
FILT10   CLC   BCFLTCLI,BCSPACES   *** FILTER BY CLIENT ***                     
         BNH   FILT20                                                           
         SR    R0,R0                                                            
         IC    R0,BCSJLEV1                                                      
         LA    RE,BCFLTCLI         FILTER FIELD - CLIENT                        
         LA    RF,TIMACC+2         TMS LINE FIELD - CLIENT                      
FILT15   CLI   0(RE),C'?'          CHECK FOR WILDCARDS(QUESTION MARKS)          
         BE    *+14                                                             
         CLC   0(1,RE),0(RF)                                                    
         BNE   ROUTH                                                            
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FILT15                                                        
*                                                                               
FILT20   CLC   BCFLTPRO,BCSPACES   *** FILTER BY PRODUCT ***                    
         BNH   FILT30                                                           
         LA    RF,TIMACC+2                                                      
         SR    R1,R1                                                            
         IC    R1,BCSJLNQ1                                                      
         AR    RF,R1                                                            
         SR    R0,R0                                                            
         IC    R0,BCSJLEV2                                                      
         LA    RE,BCFLTPRO         FILTER FIELD - PRODUCT                       
FILT25   CLI   0(RE),C'?'          CHECK FOR WILDCARDS(QUESTION MARKS)          
         BE    *+14                                                             
         CLC   0(1,RE),0(RF)                                                    
         BNE   ROUTH                                                            
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FILT25                                                        
*                                                                               
FILT30   CLC   BCFLTJOB,BCSPACES   *** FILTER BY JOB ***                        
         BNH   FILT40                                                           
         LA    RF,TIMACC+2                                                      
         SR    R1,R1                                                            
         IC    R1,BCSJLNQ2                                                      
         AR    RF,R1                                                            
         SR    R0,R0                                                            
         IC    R0,BCSJLEV3                                                      
         LA    RE,BCFLTJOB         FILTER FIELD - JOB                           
FILT35   CLI   0(RE),C'?'          CHECK FOR WILDCARDS(QUESTION MARKS)          
         BE    *+14                                                             
         CLC   0(1,RE),0(RF)                                                    
         BNE   ROUTH                                                            
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FILT35                                                        
*                                                                               
FILT40   CLC   BCFLTTSK,BCSPACES   *** FILTER BY TASK ***                       
         BNH   FILT50                                                           
         LA    RE,BCFLTTSK         FILTER FIELD - TASK                          
         LA    RF,TIMTSK                                                        
         SR    R0,R0                                                            
         LA    R0,L'TIMTSK                                                      
FILT45   CLI   0(RE),C'?'          CHECK FOR WILDCARDS(QUESTION MARKS)          
         BE    *+14                                                             
         CLC   0(1,RE),0(RF)                                                    
         BNE   ROUTH                                                            
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FILT45                                                        
*                                                                               
FILT50   CLC   BCFLTCA,BCSPACES    *** FILTER BY CONTRA ***                     
         BNH   FILT60                                                           
         LA    RE,BCFLTCA          ONLY COMPARE ON SIGNIFICANT CHAR             
         LA    RF,TRKCNTRA         SAME CONTRA?                                 
*                                                                               
         LA    RF,BCFLTCA+L'BCFLTCA-1                                           
         CLI   0(RF),C' '          FIND LAST SIGNIFICANT CHAR                   
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    R1,BCFLTCA          FIND ACTUAL LENGTH OF FILT FLD               
         SR    RF,R1                                                            
         AHI   RF,1                INCLUDE LAST SIGNIFICANT CHAR                
*                                                                               
         LR    R0,RF               SAVE OFF RF IN R0                            
         SR    R1,R1               FIND OUT WHAT LEVEL IT'S AT + UL             
         IC    R1,BC1CLNQ1         ACCOUNT LEVEL                                
         AHI   R1,2                U/L                                          
         CR    R0,R1                                                            
         BH    *+10                                                             
         LR    R0,R1                                                            
         B     FILT52                                                           
         IC    R1,BC1CLNQ2                                                      
         AHI   R1,2                                                             
         CR    R0,R1                                                            
         BH    *+6                                                              
         LR    R0,R1                                                            
*        SR    R0,R0                                                            
*        LA    R0,L'BCFLTCA                                                     
FILT52   LA    RE,BCFLTCA          ONLY COMPARE ON SIGNIFICANT CHAR             
         LA    RF,TRKCNTRA         SAME CONTRA?                                 
FILT55   CLI   0(RE),C'?'                                                       
         BE    FILT57                                                           
*        CLI   0(RE),C' '                                                       
*        BE    FILT57                                                           
         CLC   0(1,RE),0(RF)       SAME CONTRA?                                 
         BNE   ROUTH                                                            
FILT57   LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FILT55                                                        
*                                                                               
FILT60   OC    BCFLTMOA,BCFLTMOA   *** FILTER BY MOA ***                        
         BZ    FILTX                                                            
*        TM    TRKSTAT,TRKSSAVE+TRKSSAV#        SKIP IF SAVED                   
*        BNZ   ROUTH                                                            
         CLC   TIMMOA(L'BCFSTMOA),BCFSTMOA                                      
         BL    ROUTH                                                            
         CLC   TIMMOA(L'BCFENMOA),BCFENMOA                                      
         BH    ROUTH                                                            
         B     FILTX                                                            
*                                                                               
* GENERALIZED COMPARE ROUTINE - OVERLAY PASSES EVERYTHING                       
*                                                                               
FILT70   L     RE,0(R1)            ACTUAL FIELD                                 
         L     RF,4(R1)            RF=A(FILTER FIELD)                           
         TM    BCBYTE1,X'20'       ARE WE DOING A DATE?                         
         BNO   FILT72                                                           
         CLC   8(L'BCFSTMOA,RE),0(RF)                                           
         BL    ROUTH                                                            
         CLC   8(L'BCFENMOA,RE),2(RF)                                           
         BH    ROUTH                                                            
         B     FILTX                                                            
*                                                                               
FILT72   SR    R0,R0                                                            
         IC    R0,5(RE)            LENGTH OF INPUT FIELD                        
         LR    R3,R0                                                            
         LA    RE,8(RE)            BUMP PAST HEADER                             
FILT75   CLI   0(RF),C'?'          CHECK FOR WILDCARDS(QUESTION MARKS)          
         BE    *+14                                                             
         CLC   0(1,RF),0(RE)                                                    
         BNE   ROUTH                                                            
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FILT75                                                        
*                                                                               
FILTX    B     ROUTE                                                            
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* INSURE THAT ALL NEW CHANGES AMOUNT TO 0 HOURS FOR EACH COSTING ACCT *         
*                                                                     *         
* EXIT - CC=EQU - ALL ACCOUNT AMOUNT TO 0                             *         
*      - CC=NEQ - HOURS REMAIN ON COSTING ACCOUNT - DON'T UPDATE      *         
***********************************************************************         
         SPACE 1                                                                
CSTCHK   DS    0H                                                               
         MVC   BCFULL,AIO                                                       
         XC    LWS1CNUM,LWS1CNUM   CLEAR # TABLE ENTRIES                        
*                                                                               
         LA    RE,LWS1CTAB                                                      
         LH    RF,=Y(L'LWS1CTAB)                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING TSARRECD,R4         R4=A(TSAR ITEM)                              
         L     R4,AIO1                                                          
         LA    R3,TSARDH                                                        
CST10    GOTO1 AXAIO,AIO1                                                       
         GOTO1 ATSAR,BCDMCB,(R3),1                                              
         TM    BCTSERRS,TSEEOF                                                  
         BO    CST60                                                            
         TM    TRKSTAT,TRKSDEL+TRKSSAVE SKIP DELETED/SAVED ITEMS                
         BNZ   CST50                                                            
*                                                                               
         USING TIMELD,R6                                                        
         LA    R6,TRDATA           R6=A(CURRENT X'8B' DETAIL ITEM)              
         CLI   TIMEL,TIMELQ                                                     
         BE    *+6                 NOT X'8B' ELEMENT                            
         DC    H'0'                                                             
         CLI   TIMETYP,TIMEINP     INPUT DETAIL ITEM                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CST1CD,R2                                                        
         LA    R2,LWS1CTAB         R2=A(1C - HRS TABLE)                         
         SR    R0,R0                                                            
         ICM   R0,3,LWS1CNUM       NUMBER OF ENTRIES                            
         BZ    CST40                                                            
*                                                                               
CST20    CLC   CST1CACT,TRKCNTRA   1C/1N ACCOUNT                                
         BNE   CST30                                                            
         AP    CST1CHRS,TIMHRS     ADD IN HOURS                                 
         B     CST50                                                            
CST30    LA    R2,CST1CLNQ(R2)     NEXT TABLE ENTRY                             
         BCT   R0,CST20                                                         
*                                                                               
CST40    DS    0H                  ADD NEW TABLE ENTRY                          
         CLC   TIMMOA,CALSTDTE     MAKE SURE IT'S A NEW ENTRY                   
         BL    *+14                                                             
         CLC   TIMMOA,CALENDTE                                                  
         BNH   CST50                                                            
         MVC   CST1CACT,TRKCNTRA   1C/1N ACCOUNT                                
         ZAP   CST1CHRS,TIMHRS     HOURS                                        
         SR    R1,R1                                                            
         ICM   R1,3,LWS1CNUM                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,LWS1CNUM       BUMP TABLE COUNT                             
         CH    R1,=Y(LWS1CMAX)                                                  
         BNH   *+6                                                              
         DC    H'0'                SJ TABLE FULL - #ITEMS>300                   
*                                                                               
CST50    LA    R3,TSANXT                                                        
         B     CST10                                                            
*                                                                               
CST60    DS    0H                                                               
         USING CST1CD,R2                                                        
         LA    R2,LWS1CTAB         R2=A(SJ -HRS TABLE)                          
         SR    R0,R0                                                            
         ICM   R0,3,LWS1CNUM       NUMBER OF ENTRIES                            
         BZ    ROUTE                                                            
NEG25    CP    CST1CHRS,=P'0'                                                   
         BE    CST70                                                            
         MVC   BCWORK,BCSPACES                                                  
         MVC   BCWORK(14),CST1CACT                                              
         LA    RF,BCWORK+14                                                     
         B     ROUTH               SET FOR ERROR MSG                            
*                                                                               
CST70    LA    R2,CST1CLNQ(R2)                                                  
         BCT   R0,NEG25                                                         
         B     ROUTE                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ERROR EQUATE TABLE                                                  *         
***********************************************************************         
         SPACE 1                                                                
ERRTAB   DS    0AL4                                                             
         DC    AL2(TRKERATI),AL2(ACEMSRAT)       INVALID RATE                   
         DC    AL2(TRKERATM),AL2(ACEMSRAT)       MISSING RATE                   
         DC    AL2(TRKEANLI),AL2(ACEIANAL)       INVALID ANALYSIS               
         DC    AL2(TRKEANLM),AL2(ACEIANAL)       MISSING ANALYSIS               
         DC    AL2(TRKEINCI),AL2(ACEIINCM)       INVALID INCOME                 
         DC    AL2(TRKEINCM),AL2(ACEMSINC)       MISSING INCOME                 
         DC    AL2(TRKETAX),AL2(ACEITAX)         INVALID TAX INFO               
ERRTABQ  EQU   (*-ERRTAB)/L'ERRTAB                                              
         EJECT                                                                  
***********************************************************************         
* RATE SEARCH TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
*&&UK                                                                           
SRCHTBL  DS    0XL1                                                             
         DC    B'11111111'     W-C/PROD/CLI/OFF/D/C/B/A                         
         DC    B'01111111'         PROD/CLI/OFF/D/C/B/A                         
         DC    B'01101111'         PROD/CLI     D/C/B/A                         
         DC    B'10111111'     W-C      CLI/OFF/D/C/B/A                         
         DC    B'10101111'     W-C      CLI     D/C/B A                         
         DC    B'00111111'              CLI/OFF/D/C/B/A                         
         DC    B'00101111'              CLI     D/C/B/A                         
         DC    B'10011111'     W-C          OFF/D/C/B/A                         
         DC    B'00011111'                  OFF/D/C/B/A                         
         DC    B'10001111'     W-C              D/C/B/A                         
         DC    B'00001111'                      D/C/B/A                         
         DC    B'11110111'     W-C/PROD/CLI/OFF   C/B/A                         
         DC    B'01110111'         PROD/CLI/OFF   C/B/A                         
         DC    B'01100111'         PROD/CLI       C/B/A                         
         DC    B'10110111'     W-C      CLI/OFF   C/B/A                         
         DC    B'10100111'     W-C      CLI       C/B A                         
         DC    B'00110111'              CLI/OFF   C/B/A                         
         DC    B'00100111'              CLI       C/B/A                         
         DC    B'10010111'     W-C          OFF   C/B/A                         
         DC    B'00010111'                  OFF   C/B/A                         
         DC    B'10000111'     W-C                C/B/A                         
         DC    B'00000111'                        C/B/A                         
         DC    B'11110011'     W-C/PROD/CLI/OFF     B/A                         
         DC    B'01110011'         PROD/CLI/OFF     B/A                         
         DC    B'01100011'         PROD/CLI         B/A                         
         DC    B'10110011'     W-C      CLI/OFF     B/A                         
         DC    B'10100011'     W-C      CLI         B/A                         
         DC    B'00110011'              CLI/OFF     B/A                         
         DC    B'00100011'              CLI         B/A                         
         DC    B'10010011'     W-C          OFF     B/A                         
         DC    B'00010011'                  OFF     B/A                         
         DC    B'10000011'     W-C                  B/A                         
         DC    B'00000011'                          B/A                         
         DC    B'11110001'     W-C/PROD/CLI/OFF       A                         
         DC    B'01110001'         PROD/CLI/OFF       A                         
         DC    B'10110001'     W-C      CLI/OFF       A                         
         DC    B'10100001'     W-C      CLI           A                         
         DC    B'00110001'              CLI/OFF       A                         
         DC    B'00100001'              CLI           A                         
         DC    B'10010001'     W-C          OFF       A                         
         DC    B'00010001'                  OFF       A                         
         DC    B'10000001'     W-C                    A                         
         DC    B'00000001'                            A                         
         DC    B'11110000'     W-C/PROD/CLI/OFF                                 
         DC    B'01110000'         PROD/CLI/OFF                                 
         DC    B'10110000'     W-C      CLI/OFF                                 
         DC    B'10100000'     W-C      CLI                                     
         DC    B'00100000'              CLI                                     
         DC    B'00110000'              CLI/OFF                                 
         DC    B'10010000'     W-C          OFF                                 
         DC    B'00010000'                  OFF                                 
         DC    B'10000000'     W-C                                              
         DC    X'00'           E-O-T POSSIBLE SEARCHES                          
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* RATE SEARCH TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
SRCHTBL1 DS    0XL1                                                             
         DC    B'11111111'     PRO/CLI/OFF/TASK/STF/SUB/DEP/OFF                 
         DC    B'11101111'     PRO/CLI/OFF      STF/SUB/DEP/OFF                 
         DC    B'01111111'         CLI/OFF/TASK/STF/SUB/DEP/OFF                 
         DC    B'01101111'         CLI/OFF      STF/SUB/DEP/OFF                 
         DC    B'00111111'             OFF/TASK/STF/SUB/DEP/OFF                 
         DC    B'00101111'             OFF      STF/SUB/DEP/OFF                 
         DC    B'00011111'                 TASK/STF/SUB/DEP/OFF                 
         DC    B'00001111'                      STF/SUB/DEP/OFF                 
         DC    B'11110111'     PRO/CLI/OFF/TASK     SUB/DEP/OFF                 
         DC    B'11100111'     PRO/CLI/OFF          SUB/DEP/OFF                 
         DC    B'01110111'         CLI/OFF/TASK     SUB/DEP/OFF                 
         DC    B'01100111'         CLI/OFF          SUB/DEP/OFF                 
         DC    B'00110111'             OFF/TASK     SUB/DEP/OFF                 
         DC    B'00100111'             OFF          SUB/DEP/OFF                 
         DC    B'00010111'                 TASK     SUB/DEP/OFF                 
         DC    B'00000111'                          SUB/DEP/OFF                 
         DC    B'11110011'     PRO/CLI/OFF/TASK         DEP/OFF                 
         DC    B'11100011'     PRO/CLI/OFF              DEP/OFF                 
         DC    B'01110011'         CLI/OFF/TASK         DEP/OFF                 
         DC    B'01100011'         CLI/OFF              DEP/OFF                 
         DC    B'00110011'             OFF/TASK         DEP/OFF                 
         DC    B'00100011'             OFF              DEP/OFF                 
         DC    B'00010011'                 TASK         DEP/OFF                 
         DC    B'00000011'                              DEP/OFF                 
         DC    B'11110001'     PRO/CLI/OFF/TASK             OFF                 
         DC    B'11100001'     PRO/CLI/OFF                  OFF                 
         DC    B'01110001'         CLI/OFF/TASK             OFF                 
         DC    B'01100001'         CLI/OFF                  OFF                 
         DC    B'00110001'             OFF/TASK             OFF                 
         DC    B'00100001'             OFF                  OFF                 
         DC    B'00010001'                 TASK             OFF                 
         DC    B'00000001'                                  OFF                 
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* ADJUSTMENT RATE SEARCH TABLE                                        *         
***********************************************************************         
         SPACE 1                                                                
SRCHTBL2 DS    0XL1                                                             
         DC    B'11111111'     TASK/STF/SUB/DEP/JOB/PRO/CLI/OFF                 
         DC    B'01111111'          STF/SUB/DEP/JOB/PRO/CLI/OFF                 
         DC    B'10111111'     TASK     SUB/DEP/JOB/PRO/CLI/OFF                 
         DC    B'00111111'              SUB/DEP/JOB/PRO/CLI/OFF                 
         DC    B'10011111'     TASK         DEP/JOB/PRO/CLI/OFF                 
         DC    B'00011111'                  DEP/JOB/PRO/CLI/OFF                 
         DC    B'10001111'     TASK             JOB/PRO/CLI/OFF                 
         DC    B'00001111'                      JOB/PRO/CLI/OFF                 
         DC    B'11110111'     TASK/STF/SUB/DEP     PRO/CLI/OFF                 
         DC    B'01110111'          STF/SUB/DEP     PRO/CLI/OFF                 
         DC    B'10110111'     TASK     SUB/DEP     PRO/CLI/OFF                 
         DC    B'00110111'              SUB/DEP     PRO/CLI/OFF                 
         DC    B'10010111'     TASK         DEP     PRO/CLI/OFF                 
         DC    B'00010111'                  DEP     PRO/CLI/OFF                 
         DC    B'10000111'     TASK                 PRO/CLI/OFF                 
         DC    B'00000111'                          PRO/CLI/OFF                 
         DC    B'11110011'     TASK/STF/SUB/DEP         CLI/OFF                 
         DC    B'01110011'          STF/SUB/DEP         CLI/OFF                 
         DC    B'10110011'     TASK     SUB/DEP         CLI/OFF                 
         DC    B'00110011'              SUB/DEP         CLI/OFF                 
         DC    B'10010011'     TASK         DEP         CLI/OFF                 
         DC    B'00010011'                  DEP         CLI/OFF                 
         DC    B'10000011'     TASK                     CLI/OFF                 
         DC    B'00000011'                              CLI/OFF                 
         DC    B'11110001'     TASK/STF/SUB/DEP             OFF                 
         DC    B'01110001'          STF/SUB/DEP             OFF                 
         DC    B'10110001'     TASK     SUB/DEP             OFF                 
         DC    B'00110001'              SUB/DEP             OFF                 
         DC    B'10010001'     TASK         DEP             OFF                 
         DC    B'00010001'                  DEP             OFF                 
         DC    B'10000001'     TASK                         OFF                 
         DC    B'00000001'                                  OFF                 
         DC    B'11110000'     TASK/STF/SUB/DEP                                 
         DC    B'01110000'          STF/SUB/DEP                                 
         DC    B'10110000'     TASK     SUB/DEP                                 
         DC    B'00110000'              SUB/DEP                                 
         DC    B'10010000'     TASK         DEP                                 
         DC    B'00010000'                  DEP                                 
         DC    B'10000000'     TASK                                             
         DC    B'00000000'                                     AGY              
SRCH2N   EQU   *-SRCHTBL2                                                       
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* 1C COSTING ACOUNT HOURS DSECT                                       *         
***********************************************************************         
         SPACE 1                                                                
CST1CD   DSECT                                                                  
CST1CACT DS    CL14                1C ACCOUNT                                   
CST1CHRS DS    PL6                 HOURS ACCUMULATOR                            
CST1CLNQ EQU   *-CST1CD                                                         
         EJECT                                                                  
***********************************************************************         
* INCLUDES                                                            *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE DDCTRYEQUS                                                     
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACCAP30GW                                                      
       ++INCLUDE ACCAP30DST                                                     
       ++INCLUDE ACTOBACCOD                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDTSARD                                                        
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
GOXBLKD  DSECT                                                                  
       ++INCLUDE ACGOXBLOCK                                                     
GOBBLKD  DSECT                                                                  
       ++INCLUDE ACGOBBLOCK                                                     
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015ACCAP33   07/26/17'                                      
         END                                                                    
