*          DATA SET ACREPCU02  AT LEVEL 021 AS OF 05/01/02                      
*PHASE ACCU02A,+0                                                               
*INCLUDE SORTER                                                                 
*INCLUDE ACCEDIT                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE CHOPCON                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE ACSLRY                                                                 
         TITLE 'SALARY HISTORY REPORT'                                          
ACCU02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACCU**,R9,R7    R9=2ND BASE REGISTER, R7=3RD BASE            
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING ACCU02D,RC          RC=A(SAVE W/S)                               
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8         R8=ADDRESSES WIDE PRINT                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
         CLI   MODE,LEDGFRST       LEDGER FIRST                                 
         BE    LDGF                                                             
         CLI   MODE,PROCACC        ACCOUNT FIRST                                
         BE    PACC                                                             
         CLI   MODE,REQLAST        REQUEST LAST                                 
         BE    REQL                                                             
         CLI   MODE,RUNLAST        RUN LAST                                     
         BE    RUNL                                                             
EXIT     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS                                           
*                                                                               
         L     R2,=A(BOXRC)        SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAYP)                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RQSW,0                                                           
         MVI   SRTSW,0                                                          
         XC    SVSRTKEY,SVSRTKEY                                                
         XC    SVACT,SVACT                                                      
         XC    SVPERS,SVPERS                                                    
         XC    SVKEY,SVKEY                                                      
         MVI   RCSUBPRG,0          RESET 'CUZ QOPT4=O/R SCREWS NEXT ONE         
         XC    LEVELS(4),LEVELS                                                 
         XC    LEVELNM(60),LEVELNM                                              
         MVI   GOTOVHD,C'N'                                                     
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         XC    STDATE,STDATE                                                    
         MVC   ENDATE,EFFS                                                      
         XC    START,START                                                      
         MVC   END,EFFS                                                         
         CLI   QOPT4,C'R'                                                       
         BNE   REQF2                                                            
         MVI   RCSUBPRG,10         PAYROLL RATE LISTING                         
         MVI   RCCOLUMN,SAL                                                     
         B     REQF5                                                            
*                                                                               
REQF2    CLI   QOPT4,C'O'          OVERHEAD ONLY                                
         BNE   REQF5                                                            
         MVI   RCSUBPRG,13         PAYROLL COST LISTING                         
         MVI   RCCOLUMN,SAL+PEN+BEN+TOT                                         
         MVC   STDATE,TODAYP       DATE IS TODAY'S DATE                         
         MVC   ENDATE,TODAYP       THRU DATE IS TODAY                           
*                                                                               
REQF5    CLC   QSTART,SPACES                                                    
         BNH   REQF10                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,STDATE)                                
         CLC   QSTART+4(2),SPACES                                               
         BH    *+8                                                              
         MVI   STDATE+2,X'01'                                                   
*                                                                               
REQF10   CLC   QEND,SPACES                                                      
         BNH   REQF20                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,ENDATE)                                  
         CLC   QEND+4(2),SPACES    NO DAY IN END DATE                           
         BH    *+8                                                              
         MVI   ENDATE+2,X'FF'      MAKE IT THE LAST DAY OF MONTH                
*                                                                               
         USING ACCOMPD,R4                                                       
REQF20   MVC   START,STDATE                                                     
         MVC   END,ENDATE                                                       
         L     R4,ADCMPEL                                                       
         MVC   CMPABBR,ACMPABBR    COMPANY ABBREVIATION                         
         MVC   CMPNAME,SPACES                                                   
         L     R4,ADCMPNAM                                                      
         SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   CMPNAME(0),2(R4)    COMPANY NAME                                 
*                                                                               
*              READ PAYROLL CODE RECORD / BUILD PAYROLL CODE TABLE              
*                                                                               
         USING PYCODED,R5                                                       
         L     R5,APYTABLE                                                      
*                                                                               
         USING PAYRECD,R4                                                       
         L     R4,AIO1                                                          
         MVC   PAYKEY,SPACES                                                    
         MVI   PAYKTYP,PAYKTYPQ    X'3E'                                        
         MVI   PAYKSUB,PAYKSUBQ    X'03'                                        
         MVC   PAYKCPY,QCOMPANY                                                 
         MVI   PAYKSEQ,0                                                        
*                                                                               
REQF100  L     R4,AIO1                                                          
         MVC   SVKEY,0(R4)                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,(R4),(R4)                             
         TM    DMCB+8,X'10'        TEST RECORD NOT FOUND                        
         BO    REQF175                                                          
         CLC   PAYKEY(PAYKSEQ-PAYKEY),SVKEY                                     
         BNE   REQF175                                                          
         MVI   ELCODE,PAYELQ       X'84' ELEMENT                                
         BAS   RE,GETEL                                                         
         BNE   REQF150                                                          
*                                                                               
         USING PAYELD,R4                                                        
REQF125  MVC   PYNUM,PAYNUM        NUMBER (SYSTEM ASSIGNED)                     
         MVC   PYCODE,PAYCODE      PAYROLL CODE                                 
         MVI   PYTYPE,PATTOTH      DEFAULT TO 'OTHER' CATEGORY                  
         MVC   PYDESC,PAYDESC      DESCRIPTION                                  
         MVC   PYREV,PAYREV        REVERSAL TYPE                                
         LA    R5,PYLNQ(R5)        NEXT TABLE ENTRY                             
         BAS   RE,NEXTEL                                                        
         BE    REQF125                                                          
*                                                                               
         USING PAYRECD,R4                                                       
REQF150  L     R4,AIO1             CHECK FOR LINKED RECORDS                     
         MVC   PAYKEY,SVKEY                                                     
         SR    R1,R1                                                            
         IC    R1,PAYKSEQ                                                       
         LA    R1,1(R1)                                                         
         STC   R1,PAYKSEQ                                                       
         B     REQF100                                                          
*                                                                               
REQF175  MVI   0(R5),X'FF'         MARK END OF PAYCODE TABLE                    
         L     RF,APYTABLE         CHECK FOR ANY PAYCODES                       
         CR    R5,RF                                                            
         BH    REQF200                                                          
         MVC   XP(28),=C'ERROR - NO EXISTING PAYCODES'                          
         GOTO1 ACREPORT                                                         
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVI   ACMMODE,RUNLAST     FORCE RUN LAST                               
         B     REQF400                                                          
*                                                                               
*              READ METHOD RECORD                                               
*                                                                               
         USING CAHRECD,R4          R4 --> A(COST ALLOCATION HISTORY)            
REQF200  L     R4,AIO1                                                          
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    X'3E'                                        
         MVI   CAHKSUB,CAHKSUBQ    X'01'                                        
         MVC   CAHKCPY,QCOMPANY    COMPANY                                      
         MVI   CAHKMTHD,C'1'       DEFAULT TO METHOD 1                          
         CLC   QMTHD,SPACES                                                     
         BNH   *+10                                                             
         MVC   CAHKMTHD,QMTHD      METHOD CODE                                  
         XC    CAHKOFC,CAHKOFC                                                  
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,(R4),(R4)                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                MISSING METHOD CODE RECORD                   
*                                                                               
         L     R4,AIO1                                                          
         AH    R4,DATADISP                                                      
REQF210  CLI   0(R4),0                                                          
         BE    REQF300                                                          
         CLI   0(R4),PATELQ        X'85' ELEMENT                                
         BE    REQF230                                                          
         CLI   0(R4),METELQ        X'82' ELEMENT (MTHD ALLOC ELEMENT)           
         BE    REQF250                                                          
         CLI   0(R4),NAMELQ        X'20' ELEMENT (GENERAL NAME)                 
         BE    REQF260                                                          
REQF220  SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     REQF210                                                          
*                                                                               
         USING PATELD,R4                                                        
         USING PYCODED,R5                                                       
REQF230  L     R5,APYTABLE                                                      
REQF240  CLI   0(R5),X'FF'                                                      
         BE    REQF220                                                          
         CLC   PYNUM,PATNUM        PAYROLL CODE NUMBER                          
         BNE   *+14                                                             
         MVC   PYTYPE,PATTYPE                                                   
         B     REQF220                                                          
         LA    R5,PYLNQ(R5)                                                     
         B     REQF240                                                          
*                                                                               
         USING METELD,R4                                                        
REQF250  MVC   MTHD3,METCODE       GET ALLOCATION METHOD CODE                   
         B     REQF220                                                          
*                                                                               
         USING NAMELD,R4                                                        
REQF260  MVC   MTHDNAME,SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   MTHDNAME(0),NAMEREC                                              
         B     REQF220                                                          
*                                                                               
REQF300  MVC   EXPOPTN,SPACES      EXPAND OPTION 1, DEFAULT BOTH                
         CLI   QOPT1,C'T'                                                       
         BNE   *+10                                                             
         MVC   EXPOPTN,=CL12'TAPE INPUT'                                        
         CLI   QOPT1,C'M'                                                       
         BNE   *+10                                                             
         MVC   EXPOPTN,=CL12'MANUAL INPUT'                                      
*                                                                               
REQF400  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LEDGER FIRST                                                        *         
***********************************************************************         
         SPACE 1                                                                
LDGF     DS    0H                                                               
         USING ACHEIRD,R4                                                       
         L     R4,ADLDGHIR         HEIRARCHY ELEMENT                            
         MVC   LEVELA,ACHRLEVA     LEVEL LENGTHS                                
         MVC   LEVELB,ACHRLEVB                                                  
         MVC   LEVELC,ACHRLEVC                                                  
         MVC   LEVELD,ACHRLEVD                                                  
         MVC   LEVELANM,ACHRDESA   LEVEL NAMES                                  
         MVC   LEVELBNM,ACHRDESB                                                
         MVC   LEVELCNM,ACHRDESC                                                
         MVC   LEVELDNM,ACHRDESD                                                
         MVC   LVLALEN,LEVELA                                                   
         ZIC   R1,LEVELD           CALCULATE EACH LEVEL LEN                     
         ZIC   RE,LEVELC                                                        
         SR    R1,RE                                                            
         STC   R1,LVLDLEN                                                       
         ZIC   R1,LEVELB                                                        
         SR    RE,R1                                                            
         STC   RE,LVLCLEN                                                       
         ZIC   RE,LEVELA                                                        
         SR    R1,RE                                                            
         STC   R1,LVLBLEN                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCACC                                                             *         
***********************************************************************         
         SPACE 1                                                                
PACC     DS    0H                                                               
         USING SRTRECD,R3                                                       
         L     R3,ASRTREC                                                       
*                                                                               
         L     R0,ASRTREC          CLEAR SORT RECORD AREA                       
         LA    R1,SRTLNQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,ASRTSAV          CLEAR SORT SAVE AREA                         
         LA    R1,SRTLNQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,ADLVANAM                                                      
         BAS   RE,GETNAME                                                       
         MVC   SRTNOFFC,WORK       OFFICE NAME                                  
*                                                                               
         L     R4,ADLVBNAM                                                      
         BAS   RE,GETNAME                                                       
         MVC   SRTNDEPT,WORK       DEPARTMENT NAME                              
*                                                                               
         L     R4,ADLVCNAM                                                      
         BAS   RE,GETNAME                                                       
         MVC   SRTNSDPT,WORK       SUB DEPARTMENT NAME                          
*                                                                               
         L     R4,ADACCNAM                                                      
         BAS   RE,GETNAME                                                       
         MVC   SRTNPERS,WORK       PERSON'S NAME (LAST, FIRST M.I.)             
*                                                                               
         L     R4,ADACC                                                         
         MVI   SRTKREPT,0                                                       
         MVC   SRTKACT,ACTKACT-ACTRECD(R4) ACCOUNT CODE                         
         GOTO1 BCLEVELS,DMCB,(R3)      BREAK OUT LEVELS OF ACCT                 
*                                                                               
         CLI   RCSUBPRG,10         PAYROLL PART OF CU                           
         BL    PACC100                                                          
         BAS   RE,OVRHEAD          GET OVERHEAD ACCOUNTS                        
         B     PROCX                                                            
*                                                                               
PACC100  BAS   RE,HISTORY          GET PERSON'S SALARY INFO                     
         CLI   QOPT4,C'I'          INCLUDE OVERHEAD?                            
         BNE   *+8                                                              
         BAS   RE,OVRHEAD                                                       
*                                                                               
PROCX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PAYROLL HISTORY RECORD                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING PHIRECD,R4                                                       
HISTORY  NTR1                                                                   
         L     R4,AIO1                                                          
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ    X'3E'                                        
         MVI   PHIKSUB,PHIKSUBQ    X'05'                                        
         MVC   PHIKCPY,QCOMPANY                                                 
         MVC   PHIKOFC,SRTCOFFC                                                 
         MVC   PHIKDPT,SRTCDEPT                                                 
         MVC   PHIKSBD,SRTCSDPT                                                 
         MVC   PHIKPER,SRTCPERS                                                 
         SR    R1,R1                                                            
         ICM   R1,3,ENDATE         HIGHEST DATES COME FIRST ON FILE             
         LNR   R1,R1               DATE STORED AS X'FFFF'-YYMM                  
         STCM  R1,3,PHIKMOA                                                     
         MVI   PHIKSEQ,0                                                        
         MVC   SVKEY,PHIKEY                                                     
         MVC   COMMAND,DMRDHI                                                   
*                                                                               
HIST100  L     R4,AIO1                                                          
         GOTO1 DATAMGR,DMCB,COMMAND,ACCFIL,(R4),(R4)                            
         CLC   PHIKEY(PHIKMOA-PHIKEY),SVKEY                                     
         BNE   HIST300                                                          
         MVC   COMMAND,DMRSEQ                                                   
         SR    R1,R1                                                            
         ICM   R1,3,PHIKMOA                                                     
         LNR   R1,R1                                                            
         STCM  R1,3,HALF                                                        
         CLC   HALF(2),STDATE      STOP READING WHEN < START DATE               
         BL    HIST300                                                          
*                                                                               
         USING PDEELD,R4                                                        
         L     R4,AIO1                                                          
         AH    R4,DATADISP                                                      
HIST200  CLI   0(R4),0                                                          
         BE    HIST100                                                          
         CLI   0(R4),PDEELQ        X'86' ELEMENT                                
         BE    HIST220                                                          
HIST210  SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     HIST200                                                          
*                                                                               
HIST220  CLC   PDEDTE,STDATE                                                    
         BL    HIST210                                                          
         CLC   PDEDTE,ENDATE                                                    
         BH    HIST210                                                          
*                                                                               
         MVC   SRTKCKDT,PDEDTE     CHECK DATE INTO SORT KEY                     
         MVC   SRTKNUM,SPACES                                                   
         MVC   SRTCODE,SPACES                                                   
         MVC   SRTCKDT,PDEDTE                                                   
*                                                                               
         USING PYCODED,R5          R5 --> A(PAYROLL TYPE TABLE)                 
         L     R5,APYTABLE         - 1. SALARY                                  
         MVI   TYPE,PATTOTH        - 2. BENEFIT                                 
HIST230  CLI   0(R5),X'FF'         - 3. PENSION                                 
         BE    HIST240             - 4. INDIRECT                                
         CLC   PYNUM,PDENUM        - 5. OTHER                                   
         BE    *+12                                                             
         LA    R5,PYLNQ(R5)                                                     
         B     HIST230                                                          
         MVC   TYPE,PYTYPE                                                      
         CLI   TYPE,0              NO PAYTYPE ASSIGNED                          
         BNE   *+8                                                              
         MVI   TYPE,PATTOTH        DEFAULT TO OTHER                             
*                                                                               
         MVC   SRTKNUM,PYNUM                                                    
         MVC   SRTCODE,PYCODE                                                   
         MVC   SRTDESC,PYDESC                                                   
*                                                                               
HIST240  CLC   QSELECT(5),SPACES                                                
         BNH   *+14                                                             
         CLC   SRTCODE,QSELECT     FILTER ON PAYCODE TYPE                       
         BNE   HIST210                                                          
*                                                                               
         LA    R0,SRT#ACCM         CLEAR SORT ACCUMULATORS                      
         LA    R1,SRTACCMS                                                      
         ZAP   0(L'SRTACCMS,R1),=P'0'                                           
         LA    R1,L'SRTACCMS(R1)                                                
         BCT   R0,*-10                                                          
*                                                                               
*              ADD AMOUNT/ADJUSTMENT INTO CORRECT ACCUMULATOR                   
*                                                                               
         LA    RF,OFFSET                                                        
         CLC   TYPE,0(RF)                                                       
         BE    *+12                                                             
         LA    RF,L'OFFSET(RF)                                                  
         B     *-14                                                             
         SR    R1,R1                                                            
         IC    R1,1(RF)                                                         
         LA    RF,SRTACCMS(R1)                                                  
         CLI   QOPT1,C'M'          M = ONLY SHOW MANUAL ENTRIES                 
         BE    *+10                                                             
         AP    0(L'SRTACCMS,RF),PDEAMT                                          
         CLI   QOPT1,C'T'          T = ONLY SHOW TAPE ENTRIES                   
         BE    *+10                                                             
         AP    0(L'SRTACCMS,RF),PDEADJ                                          
         ZAP   SRTCOUNT,=P'1'                                                   
         BAS   RE,PUTSRT                                                        
         B     HIST210                                                          
*                                                                               
HIST300  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PUT RECORD TO SORTER                                                *         
***********************************************************************         
         SPACE 1                                                                
PUTSRT   NTR1                                                                   
         L     R0,ASRTREC          COPY SORT RECORD SO ORIG ONE DOESNT          
         LA    R1,SRTLNQ           GET MESSED UP BELOW WHEN DOING TOTS          
         LR    RF,R1                                                            
         L     RE,AIO2                                                          
         MVCL  RE,R0                                                            
*                                                                               
         L     R3,AIO2                                                          
         USING SRTRECD,R3                                                       
*                                                                               
         LA    R0,OPT2TB#                                                       
         LA    RF,OPT2TB                                                        
         CLC   QOPT2,0(RF)                                                      
         BE    *+14                                                             
         LA    RF,L'OPT2TB(RF)                                                  
         BCT   R0,*-14                                                          
         DC    H'0'                QOPT2 INVALID                                
         MVC   RCSUBPRG,1(RF)      SET CORRECT HEADINGS                         
         MVC   RCCOLUMN,2(RF)      SAVE COLUMN PRINT INDICATOR                  
*                                                                               
         CLI   QOPT3,C'C'                                                       
         BNE   *+10                                                             
         MVC   SRTKCKDT,EFFS       SUMMARY BY PAYROLL CODE                      
*                                                                               
         CLI   QOPT3,C'P'                                                       
         BNE   *+10                                                             
         MVC   SRTKNUM,EFFS        SUMMARY BY PERIOD                            
*                                                                               
PUT5     CLI   QOPT3,C'O'          BUMP HEADLINES # FOR SUMMARY VERSION         
         BNE   PUT10                                                            
         SR    R1,R1                                                            
         IC    R1,RCSUBPRG                                                      
         LA    R1,1(R1)                                                         
         STC   R1,RCSUBPRG                                                      
*                                                                               
PUT10    ZAP   TOTAL,=P'0'                                                      
         TM    RCCOLUMN,SAL                                                     
         BZ    *+10                                                             
         AP    TOTAL,SRTSALRY                                                   
         TM    RCCOLUMN,PEN                                                     
         BZ    *+10                                                             
         AP    TOTAL,SRTPENSN                                                   
         TM    RCCOLUMN,BEN                                                     
         BZ    *+10                                                             
         AP    TOTAL,SRTBENFT                                                   
         TM    RCCOLUMN,IND                                                     
         BZ    *+10                                                             
         AP    TOTAL,SRTINDIR                                                   
         TM    RCCOLUMN,OTH                                                     
         BZ    *+10                                                             
         AP    TOTAL,SRTOTHER                                                   
         CP    TOTAL,=P'0'         DONT SHOW 0$ ITEMS ON REPORT                 
         BE    EXIT                                                             
*                                                                               
         CLI   QOPT3,C'O'          SKIP THIS SORT PUT FOR OFFICE SUM            
         BE    PUT100                                                           
         CLI   QOPT3,C' '          SKIP THIS SORT PUT FOR PERSON SUM            
         BE    PUT100                                                           
         OI    SRTSW,SRTPUT                                                     
         GOTO1 SORTER,DMCB,=C'PUT',AIO2                                         
*                                                                               
PUT100   MVC   SRTKCKDT,EFFS        PERSON TOTAL RECORD                         
         MVC   SRTKNUM,EFFS                                                     
         MVI   SRTSTAT,SRTSTPER                                                 
         CLI   QOPT3,C'O'          SKIP THIS SORT PUT FOR OFFICE SUM            
         BE    PUT200                                                           
         OI    SRTSW,SRTPUT                                                     
         GOTO1 SORTER,DMCB,=C'PUT',AIO2                                         
*                                                                               
PUT200   LA    RF,SRTKACT          DEPT TOTAL                                   
         SR    R0,R0                                                            
         IC    R0,LEVELB                                                        
         AR    RF,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LEVELD                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   0(0,RF),EFFS                                                     
         MVI   SRTSTAT,SRTSTDPT                                                 
         OI    SRTSW,SRTPUT                                                     
         GOTO1 SORTER,DMCB,=C'PUT',AIO2                                         
*                                                                               
         LA    RF,SRTKACT          OFFICE TOTAL                                 
         SR    R0,R0                                                            
         IC    R0,LEVELA                                                        
         AR    RF,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LEVELB                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   0(0,RF),EFFS                                                     
         MVI   SRTSTAT,SRTSTOFF                                                 
         OI    SRTSW,SRTPUT                                                     
         GOTO1 SORTER,DMCB,=C'PUT',AIO2                                         
*                                                                               
         MVC   SRTKACT,EFFS                                                     
         MVI   SRTSTAT,SRTSTREQ                                                 
         OI    SRTSW,SRTPUT                                                     
         GOTO1 SORTER,DMCB,=C'PUT',AIO2                                         
*                                                                               
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GET OVERHEAD ACCOUNTS                                               *         
***********************************************************************         
         SPACE 1                                                                
OVRHEAD  NTR1                                                                   
         L     R2,ADACC                                                         
         LA    RE,3(R2)            POINT TO ACCOUNT                             
         LA    R5,LEVELS           POINT TO LEVEL LENGTHS                       
OVR010   ZIC   R1,1(R5)            GET LENGTH OF LEVEL                          
         BCTR  R1,0                                                             
         CLI   0(R5),MAXLEN        IS THIS LAST LEVEL?                          
         BNE   OVR020                                                           
         CLC   0(MIN9Q,RE),NINES   ARE THE MIN # 9'S HERE FOR OV.ACT?           
         BE    OVR030              YES, OVERHEAD ACCOUNT                        
         BNE   EXIT                NO MATCH, DON'T PROCESS                      
*                                                                               
OVR020   EXCLC R1,0(RE),NINES      DOES ENTIRE FIELD HAVE 9'S?                  
         BE    OVR030                                                           
         LA    RE,1(R1,RE)         BUMP TO NEXT FIELD IN ACCT                   
         LA    R5,2(R5)            BUMP TO NEXT LEVEL LENGTHS                   
         B     OVR010                                                           
*                                                                               
OVR030   L     R2,ADACC                                                         
         CLI   QOPT4,C'I'          INCLUDE OVERHEAD?                            
         BE    *+8                                                              
         CLI   RCSUBPRG,13         OVERHEAD ONLY                                
         BNE   OVR040                                                           
         LA    R6,SALAREA          SALARY AREA                                  
*MN      GOTO1 ACSLRY,DMCB,(R2),START,(R6)                                      
         GOTO1 ACSLRY,DMCB,(X'80',(R2)),START,(R6),ADCOMFAC                     
         BAS   RE,SAL2SRT                                                       
         B     EXIT                                                             
*                                                                               
OVR040   L     R4,ADACC                                                         
         MVI   ELCODE,MSAELQ                                                    
         BAS   RE,GETEL                                                         
         DROP  R2                                                               
*                                                                               
OVR050   BNE   EXIT                                                             
         USING MSAELD,R4                                                        
         CLC   MSABEG,ENDATE                                                    
         BH    OVR059                                                           
         OC    MSAEND,MSAEND                                                    
         BZ    OVR055                                                           
         CLC   MSAEND,STDATE       ENDS BEFORE START?                           
         BL    OVR059                                                           
OVR055   BAS   RE,MSA2SRT                                                       
OVR059   BAS   RE,NEXTEL                                                        
         B     OVR050                                                           
         EJECT                                                                  
***********************************************************************         
* SALARY INFO TO SORTER                                               *         
***********************************************************************         
         SPACE 1                                                                
SAL2SRT  NTR1                                                                   
         L     R0,ASRTREC          COPY SORT RECORD SO ORIG ONE DOESNT          
         LA    R1,SRTLNQ           GET MESSED UP BELOW WHEN DOING TOTS          
         LR    RF,R1                                                            
         L     RE,AIO2                                                          
         MVCL  RE,R0                                                            
*                                                                               
         L     R3,AIO2                                                          
         USING SRTRECD,R3                                                       
*                                                                               
         LA    R0,SRT#ACCM         CLEAR SORT ACCUMS                            
         LA    R1,SRTACCMS                                                      
         ZAP   0(L'SRTACCMS,R1),=P'0'                                           
         LA    R1,L'SRTACCMS(R1)                                                
         BCT   R0,*-10                                                          
*                                                                               
         LA    R6,SALAREA          PT TO SALARY INFO                            
         USING SLRD,R6             USE ITS DSECT                                
         CP    SLRTOT,=P'0'        DON'T SHOW $0                                
         BE    EXIT                                                             
         MVI   GOTOVHD,C'Y'        GOT AN OVERHEAD                              
         ZAP   SRTSALRY,SLRSAL     MOVE SALARY OVER                             
         ZAP   SRTPENSN,SLRPEN     MOVE PENSION OVER                            
         ZAP   SRTBENFT,SLRBEN     MOVE BENIFIT OVER                            
         OI    SRTSW,SRTPUT                                                     
         MVI   SRTKREPT,X'FF'                                                   
         MVC   SRTCODE,=C'OVRHD'   OVERHEAD SORT RECORD                         
         GOTO1 SORTER,DMCB,=C'PUT',AIO2                                         
*                                                                               
         LA    RF,SRTKACT          DEPT TOTAL                                   
         SR    R0,R0                                                            
         IC    R0,LEVELB                                                        
         AR    RF,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LEVELD                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   0(0,RF),EFFS                                                     
         MVI   SRTSTAT,SRTSTDPT                                                 
         OI    SRTSW,SRTPUT                                                     
         GOTO1 SORTER,DMCB,=C'PUT',AIO2                                         
*                                                                               
         LA    RF,SRTKACT          OFFICE TOTAL                                 
         SR    R0,R0                                                            
         IC    R0,LEVELA                                                        
         AR    RF,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LEVELB                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   0(0,RF),EFFS                                                     
         MVI   SRTSTAT,SRTSTOFF                                                 
         OI    SRTSW,SRTPUT                                                     
         GOTO1 SORTER,DMCB,=C'PUT',AIO2                                         
*                                                                               
         MVC   SRTKEY,EFFS                                                      
         MVI   SRTSTAT,SRTSTREQ                                                 
         OI    SRTSW,SRTPUT                                                     
         GOTO1 SORTER,DMCB,=C'PUT',AIO2                                         
         B     EXIT                                                             
         DROP  R6                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* MSAELD INFO TO SORTER                                               *         
***********************************************************************         
         SPACE 1                                                                
MSA2SRT  NTR1                                                                   
         L     R0,ASRTREC          COPY SORT RECORD SO ORIG ONE DOESNT          
         LA    R1,SRTLNQ           GET MESSED UP BELOW WHEN DOING TOTS          
         LR    RF,R1                                                            
         L     RE,AIO2                                                          
         MVCL  RE,R0                                                            
*                                                                               
         L     R3,AIO2                                                          
         USING SRTRECD,R3                                                       
*                                                                               
         LA    R0,SRT#ACCM         CLEAR SORT ACCUMS                            
         LA    R1,SRTACCMS                                                      
         ZAP   0(L'SRTACCMS,R1),=P'0'                                           
         LA    R1,L'SRTACCMS(R1)                                                
         BCT   R0,*-10                                                          
*                                                                               
         DROP  R4                                                               
         LR    R6,R4               PT TO SALARY INFO                            
         USING MSAELD,R6           USE ITS DSECT                                
         CP    MSALARY,=P'0'       DON'T SHOW $0                                
         BE    EXIT                                                             
         MVC   SRTKSDTC,MSABEG     COMPLEMENT DATES FOR SORTING                 
         MVC   SRTKNDTC,MSAEND                                                  
         XC    SRTKSDTC(4),EFFS                                                 
*                                                                               
         MVC   SRTMSATP,MSATYPE    MOVE TYPE OVER                               
         MVC   SRTMSAST,MSABEG     MOVE START DATE                              
         MVC   SRTMSAND,MSAEND     MOVE END DATE                                
         MVC   SRTMSABS,MSABASIS   MOVE BASIS OVER                              
         ZAP   SRTSALRY,MSALARY    MOVE SALARY OVER                             
         OI    SRTSW,SRTPUT                                                     
         GOTO1 SORTER,DMCB,=C'PUT',AIO2                                         
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* EXTRACT NAME FROM X'20' ELEMENT                                     *         
***********************************************************************         
         SPACE 1                                                                
GETNAME  NTR1                                                                   
         MVC   WORK,SPACES                                                      
         OR    R4,R4                                                            
         BZ    EXIT                                                             
*                                                                               
         USING NAMELD,R4                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    EXIT                                                             
         EX    R1,*+4                                                           
         MVC   WORK(0),NAMEREC                                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* BREAK O/D/S/P INTO COMPONENTS                                       *         
***********************************************************************         
         SPACE 1                                                                
BCLEVELS NTR1                                                                   
         L     R3,0(R1)            R3 -> START OF ACCOUNT CODE                  
         USING SRTRECD,R3                                                       
*                                                                               
         MVC   SRTCOFFC,SPACES     OFFICE CODE                                  
         MVC   SRTCDEPT,SPACES     DEPARTMENT                                   
         MVC   SRTCSDPT,SPACES     SUBDEPARTMENT                                
         MVC   SRTCPERS,SPACES     PERSON                                       
*                                                                               
         LA    RF,SRTKACT                                                       
         SR    R1,R1                                                            
         IC    R1,LEVELA                                                        
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   SRTCOFFC(0),0(RF)   ISOLATE OFFICE CODE                          
*                                                                               
         LA    RF,SRTKACT                                                       
         SR    R0,R0                                                            
         IC    R0,LEVELA                                                        
         AR    RF,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LEVELB                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         BM    BCLEVELX                                                         
         EX    R1,*+4                                                           
         MVC   SRTCDEPT(0),0(RF)   ISOLATE DEPT CODE                            
*                                                                               
         LA    RF,SRTKACT                                                       
         SR    R0,R0                                                            
         IC    R0,LEVELB                                                        
         AR    RF,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LEVELC                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         BM    BCLEVELX                                                         
         EX    R1,*+4                                                           
         MVC   SRTCSDPT(0),0(RF)   ISOLATE SDPT CODE                            
*                                                                               
         LA    RF,SRTKACT                                                       
         SR    R0,R0                                                            
         IC    R0,LEVELC                                                        
         AR    RF,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LEVELD                                                        
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         BM    BCLEVELX                                                         
         EX    R1,*+4                                                           
         MVC   SRTCPERS(0),0(RF)   ISOLATE PERSON CODE                          
*                                                                               
BCLEVELX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              REQUEST LAST                                           *         
***********************************************************************         
*                                                                               
REQL     DS    0H                                                               
         BAS   RE,DETAIL           PRINT DETAIL REPORT                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT DETAIL ITEM REPORT                                            *         
***********************************************************************         
         SPACE 1                                                                
DETAIL   NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         XC    SVACT,SVACT         CLEAR HEADLINE SAVE ACCOUNT                  
         XC    SVPERS,SVPERS                                                    
         XC    SVSRTKEY,SVSRTKEY                                                
         MVC   SVSUBPRG,RCSUBPRG                                                
*                                                                               
DET100   TM    RQSW,RQLAST                                                      
         BO    DETX                                                             
         GOTO1 SORTER,DMCB,=C'GET'                                              
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   DET125                                                           
         TM    SRTSW,SRTPUT        NO REPORT IF NOTHING PUT TO SORT             
         BZ    DETX                                                             
         OI    RQSW,RQLAST         IF DONE WITH SORT THEN DO 1 MORE             
         B     DET175              PASS TO FINISH RECORD IN BUFFER              
*                                                                               
DET125   L     R0,DMCB+4           MOVE RECORD TO AREA WHERE IT CAN             
         LA    R1,SRTLNQ           BE SEEN IN A DUMP                            
         LR    RF,R1                                                            
         L     RE,ASRTREC                                                       
         MVCL  RE,R0                                                            
*                                                                               
         CLI   QOPT7,C'D'          D = DUMP SORT RECORDS                        
         BNE   *+8                                                              
         BAS   RE,DMPSRT                                                        
*                                                                               
         TM    RQSW,RQFIRST        SAVE OFF 1ST SORT RECORD                     
         BZ    DETNEXT                                                          
*                                                                               
DET150   DS    0H                                                               
         L     R3,ASRTSAV          SUM RECORDS THAT ARE SAME                    
         USING SRTRECD,R3                                                       
         L     RF,ASRTREC                                                       
         CLI   RCSUBPRG,10                                                      
         BNE   *+14                                                             
         CLC   SRTKEY(SRTCOFFC-SRTKEY),0(RF)                                    
         B     *+10                                                             
         CLC   SRTKEY,0(RF)                                                     
         BNE   DET175                                                           
         LA    R0,SRT#ACCM                                                      
         LA    R1,SRTACCMS                                                      
         LA    RF,SRTACCMS-SRTRECD(RF)                                          
         AP    0(L'SRTACCMS,R1),0(L'SRTACCMS,RF)                                
         LA    R1,L'SRTACCMS(R1)                                                
         LA    RF,L'SRTACCMS(RF)                                                
         BCT   R0,*-14                                                          
*                                                                               
         CLI   QOPT3,C'C'          FOR PAYROLL CODE SUMMARY                     
         BNE   DET100              SAVE HIGHEST DATE                            
         CLC   SRTCKDT,SRTCKDT-SRTRECD(RF)                                      
         BH    DET100                                                           
         MVC   SRTCKDT,SRTCKDT-SRTRECD(RF)                                      
         B     DET100                                                           
*                                                                               
DET175   MVC   RCSUBPRG,SVSUBPRG                                                
         SR    RF,RF                                                            
         IC    RF,SRTSTAT                TYPE OF RECORD                         
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     DETITEM             PRINT DETAIL ITEM                            
         B     DETPERS             PRINT PERSON TOTAL                           
         B     DETDEPT             PRINT DEPT   TOTAL                           
         B     DETOFFC             PRINT OFFICE TOTAL                           
         B     DETRQST             PRINT REQST  TOTAL                           
*                                                                               
*              PRINT ITEM LINE                                                  
*                                                                               
         USING PLINED,R6                                                        
DETITEM  LA    R6,XP                                                            
         CLC   SVPERS,SRTCPERS                                                  
         BE    DETIT10                                                          
         MVC   SVPERS,SRTCPERS                                                  
         MVC   PLINPERS,SRTCPERS   EMPLOYEE NUMBER                              
         MVC   PLINNAME,SRTNPERS   EMPLOYEE NAME                                
         CLC   SRTCODE,=C'OVRHD'                                                
         BNE   DETIT05                                                          
         CLI   QOPT4,C'I'                                                       
         BNE   DETIT05                                                          
         ZIC   RE,RCSUBPRG                                                      
         SRL   RE,1                                                             
         LA    RE,11(RE)                                                        
         STC   RE,RCSUBPRG                                                      
         B     DETIT15                                                          
*                                                                               
DETIT05  CLI   RCSUBPRG,10         PAYROLL PROGRAMS?                            
         BNL   DETIT15                                                          
DETIT10  GOTO1 DATCON,DMCB,(1,SRTCKDT),(5,PLINCKDT)                             
         CLI   QOPT3,C'P'                                                       
         BE    *+10                NO CODE IF SUMMARY BY PERIOD                 
         MVC   PLINCODE,SRTCODE                                                 
DETIT15  BAS   RE,FORMAT                                                        
         BAS   RE,PRNTXP                                                        
         B     DETNEXT                                                          
*                                                                               
*              PRINT PERSON TOTAL                                               
*                                                                               
         USING PLINED,R6                                                        
DETPERS  LA    R6,XP                                                            
         XC    SVPERS,SVPERS                                                    
         CLI   QOPT3,C' '          PERSON SUMMARY                               
         BNE   DETPER10                                                         
         MVC   PLINPERS,SRTCPERS   EMPLOYEE NUMBER                              
         MVC   PLINNAME,SRTNPERS   EMPLOYEE NAME                                
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTXP                                                        
         B     DETNEXT                                                          
*                                                                               
DETPER10 CP    SRTCOUNT,=P'1'                                                   
         BE    *+18                                                             
         MVC   PLINNAME(13),=CL13'*** TOTAL ***'                                
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTXP                                                        
         BAS   RE,PRNTXP                                                        
         B     DETNEXT                                                          
*                                                                               
*              PRINT DEPARTMENT TOTAL                                           
*                                                                               
         USING PLINED,R6                                                        
DETDEPT  LA    R6,XP                                                            
         XC    SVPERS,SVPERS                                                    
         CLI   QOPT3,C' '          SKIP LINE BEFORE TOTAL IF PERSON SUM         
         BNE   *+8                                                              
         BAS   RE,PRNTXP                                                        
*                                                                               
         CLI   QOPT3,C'O'          OFFICE SUMMARY                               
         BNE   DETDEP10                                                         
         MVC   PSUMDPT,SRTCDEPT                                                 
         MVC   PSUMDPTN,SRTNDEPT                                                
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTXP                                                        
         B     DETNEXT                                                          
*                                                                               
DETDEP10 MVC   WORK,SPACES                                                      
         MVC   WORK(10),=C'TOTAL FOR '                                          
         MVC   WORK+10(L'SRTCDEPT),SRTCDEPT                                     
         MVC   WORK+20(L'SRTNDEPT),SRTNDEPT                                     
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         CP    SRTCOUNT,=P'1'                                                   
         BE    DETNEXT                                                          
         MVC   PLINTOT,WORK                                                     
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTXP                                                        
         BAS   RE,PRNTXP                                                        
         B     DETNEXT                                                          
*                                                                               
*              PRINT OFFICE TOTAL                                               
*                                                                               
         USING PLINED,R6                                                        
DETOFFC  LA    R6,XP                                                            
         XC    SVPERS,SVPERS                                                    
         CLI   QOPT3,C'O'          OFFICE SUMMARY                               
         BNE   *+8                                                              
         BAS   RE,PRNTXP                                                        
         MVC   WORK,SPACES                                                      
         MVC   WORK(10),=C'TOTAL FOR '                                          
         MVC   WORK+10(L'SRTCOFFC),SRTCOFFC                                     
         MVC   WORK+20(L'SRTNOFFC),SRTNOFFC                                     
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PLINTOT,WORK                                                     
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTXP                                                        
         BAS   RE,PRNTXP                                                        
         B     DETNEXT                                                          
*                                                                               
*              REQUEST TOTAL                                                    
*                                                                               
         USING PLINED,R6                                                        
DETRQST  BAS   RE,PRNTXP                                                        
         LA    R6,XP                                                            
         MVC   PLINTOT(20),=CL20'TOTAL FOR REQUEST'                             
         CLI   QOPT4,C'I'                                                       
         BNE   DETRQST5                                                         
         CLI   GOTOVHD,C'Y'                                                     
         BNE   DETRQST6                                                         
         MVC   PLINTOT(22),=CL22'TOTAL FOR OVERHEAD'                            
         CLI   SRTKEY,X'FF'                                                     
         BE    DETRQST5                                                         
         MVC   PLINTOT(22),=CL22'TOTAL FOR NON-OVERHEAD'                        
         GOTO1 COPYSRT,DMCB,ASRTSAV,NOVHDSAV  COPY SORT AREA                    
DETRQST5 BAS   RE,FORMAT                                                        
         BAS   RE,PRNTXP                                                        
*                                                                               
         TM    RQSW,RQLAST         ARE WE DONE?                                 
         BZ    DETNEXT                                                          
         CLI   QOPT4,C'I'                                                       
         BNE   DETNEXT                                                          
         GOTO1 COPYSRT,DMCB,ASRTSAV,ASRTREC   COPY FOR LATER                    
         GOTO1 COPYSRT,DMCB,NOVHDSAV,ASRTSAV                                    
         BAS   RE,PRNTXP                                                        
         MVC   PLINTOT(22),=CL22'TOTAL FOR NON-OVERHEAD'                        
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTXP                                                        
*                                                                               
         L     RF,ASRTREC                                                       
TMP      USING SRTRECD,RF                                                       
         AP    SRTSALRY,TMP.SRTSALRY                                            
         AP    SRTPENSN,TMP.SRTPENSN                                            
         AP    SRTBENFT,TMP.SRTBENFT                                            
         AP    SRTINDIR,TMP.SRTINDIR                                            
         AP    SRTOTHER,TMP.SRTOTHER                                            
         AP    SRTCOUNT,TMP.SRTCOUNT                                            
         DROP  TMP                                                              
*                                                                               
         BAS   RE,PRNTXP                                                        
         MVC   PLINTOT(22),=CL22'TOTAL FOR REQUEST'                             
DETRQST6 BAS   RE,FORMAT                                                        
         BAS   RE,PRNTXP                                                        
         B     DETNEXT                                                          
*                                                                               
DETNEXT  L     R0,ASRTREC                                                       
         LA    R1,SRTLNQ                                                        
         LR    RF,R1                                                            
         L     RE,ASRTSAV                                                       
         MVCL  RE,R0                                                            
         OI    RQSW,RQFIRST                                                     
         B     DET100                                                           
*                                                                               
DETX     DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FORMAT ACCUMULATORS                                                 *         
***********************************************************************         
         SPACE 1                                                                
FORMAT   NTR1                                                                   
         L     R3,ASRTSAV                                                       
         USING SRTRECD,R3                                                       
*                                                                               
         USING PLINED,R6                                                        
         LA    R6,XP+(PLIN$-PLINED)                                             
         ZAP   TOTAL,=P'0'                                                      
*                                                                               
         CLI   RCSUBPRG,10                                                      
         BNE   FORM09                                                           
*                                                                               
         LA    R6,XP                                                            
         LA    RF,TYPETBL                                                       
FORM01   CLI   0(RF),X'FF'         EOT?                                         
         BE    FORM02                                                           
         CLC   SRTMSATP,0(RF)                                                   
         BE    *+12                                                             
         LA    RF,5(RF)                                                         
         B     FORM01                                                           
         MVC   PLINTYPE,1(RF)                                                   
*                                                                               
         LA    RF,BASETBL                                                       
FORM02   CLI   0(RF),X'FF'         EOT?                                         
         BE    FORM03                                                           
         CLC   SRTMSABS,0(RF)                                                   
         BE    *+12                                                             
         LA    RF,3(RF)                                                         
         B     FORM02                                                           
         MVC   PLINBASE,0(RF)                                                   
*                                                                               
FORM03   MVC   WORK(2),SRTMSAST                                                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(9,PLINDBEG)                                
         MVI   PLINDBEG+6,C','                                                  
         MVC   WORK(2),SRTMSAND                                                 
         GOTO1 DATCON,DMCB,(1,WORK),(9,PLINDEND)                                
         EDIT  (P8,SRTSALRY),(17,PLINRATE),2,ZERO=BLANK,MINUS=YES               
         B     EXIT                                                             
*                                                                               
FORM09   TM    RCCOLUMN,SAL                                                     
         BZ    FORM10                                                           
         EDIT  (P8,SRTSALRY),(17,(R6)),2,ZERO=BLANK,MINUS=YES                   
         AP    TOTAL,SRTSALRY                                                   
         LA    R6,L'PLIN$(R6)                                                   
*                                                                               
FORM10   TM    RCCOLUMN,PEN                                                     
         BZ    FORM20                                                           
         EDIT  (P8,SRTPENSN),(17,(R6)),2,ZERO=BLANK,MINUS=YES                   
         AP    TOTAL,SRTPENSN                                                   
         LA    R6,L'PLIN$(R6)                                                   
*                                                                               
FORM20   TM    RCCOLUMN,BEN                                                     
         BZ    FORM30                                                           
         EDIT  (P8,SRTBENFT),(17,(R6)),2,ZERO=BLANK,MINUS=YES                   
         AP    TOTAL,SRTBENFT                                                   
         LA    R6,L'PLIN$(R6)                                                   
*                                                                               
FORM30   TM    RCCOLUMN,IND                                                     
         BZ    FORM40                                                           
         EDIT  (P8,SRTINDIR),(17,(R6)),2,ZERO=BLANK,MINUS=YES                   
         AP    TOTAL,SRTINDIR                                                   
         LA    R6,L'PLIN$(R6)                                                   
*                                                                               
FORM40   TM    RCCOLUMN,OTH                                                     
         BZ    FORM50                                                           
         EDIT  (P8,SRTOTHER),(17,(R6)),2,ZERO=BLANK,MINUS=YES                   
         AP    TOTAL,SRTOTHER                                                   
         LA    R6,L'PLIN$(R6)                                                   
*                                                                               
FORM50   TM    RCCOLUMN,TOT                                                     
         BZ    FORM60                                                           
         EDIT  (P8,TOTAL),(17,(R6)),2,ZERO=BLANK,MINUS=YES                      
*                                                                               
FORM60   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              COPY SORT AREA                                         *         
*                  CALLED W/ GOTO1, PARM1=SOURCE, PARM2=DEST.         *         
***********************************************************************         
*                                                                               
COPYSRT  NTR1                                                                   
         L     R0,0(R1)                                                         
         L     RE,4(R1)                                                         
         LA    R1,SRTLNQ                                                        
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PRINT A LINE                                           *         
***********************************************************************         
*                                                                               
PRNTXP   NTR1                                                                   
         L     R3,ASRTSAV                                                       
         USING SRTRECD,R3                                                       
*                                                                               
         MVC   XHEAD2+12(L'CMPABBR),CMPABBR                                     
         MVC   XHEAD2+21(L'CMPNAME),CMPNAME                                     
*                                                                               
         CLC   SRTKACT,EFFS                                                     
         BE    PRNTX                                                            
*                                                                               
PRNTXP5  LA    R1,L'SRTCOFFC       NEW PAGE ON CHANGE OF OFFICE                 
         CLI   QOPT3,C'O'          FOR SUMMARY VERSION OF REPORT                
         BE    *+8                 OTHERWISE                                    
         LA    R1,L'SRTCOFFC+L'SRTCDEPT+L'SRTCSDPT                              
         SH    R1,=H'1'                                                         
         EXCLC R1,SVACT,SRTCOFFC                                                
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   SVACT,SRTCOFFC                                                   
*                                                                               
         CLI   QOPT3,C'O'                                                       
         BNE   PRNT100                                                          
         MVC   XHEAD8(60),XSPACES                                               
         MVC   XHEAD9(60),XSPACES                                               
         MVC   XHEAD9+2(L'LEVELBNM),LEVELBNM                                    
*                                                                               
PRNT100  MVC   XHEAD4(L'LEVELANM),LEVELANM                                      
         MVC   XHEAD4+12(6),SRTCOFFC                                            
         MVC   XHEAD4+21(L'SRTNOFFC),SRTNOFFC                                   
         MVC   XHEAD4+114(12),EXPOPTN         DISPLAY OPTION USED               
         MVC   XHEAD5+114(3),MTHD3            DISPLAY ALLOC METHOD CODE         
         MVC   XHEAD5+129(36),MTHDNAME        DISPLAY FULL METHOD NAME          
*                                                                               
         CLI   QOPT3,C'O'          DONT PRINT LOW LEVEL NAMES IN HEAD           
         BE    PRNTX               FOR SUMMARY VERSION OF REPORT                
         MVC   XHEAD5(L'LEVELBNM),LEVELBNM                                      
         MVC   XHEAD5+12(6),SRTCDEPT                                            
         MVC   XHEAD5+21(L'SRTNDEPT),SRTNDEPT                                   
         MVC   XHEAD6(L'LEVELCNM),LEVELCNM                                      
         MVC   XHEAD6+12(6),SRTCSDPT                                            
         MVC   XHEAD6+21(L'SRTNSDPT),SRTNSDPT                                   
*                                                                               
PRNTX    GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DUMP SORTER RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
DMPSRT   NTR1                       DUMP A SORT RECORD                          
         LA    R2,=CL15'SORT RECORD'                                            
         L     R4,ASRTREC                                                       
         LA    RF,SRTLNQ                                                        
         GOTO1 VPRNTBL,DMCB,(15,(R2)),(R4),C'DUMP',(RF),=C'2D'                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LAST FOR RUN                                                        *         
***********************************************************************         
*                                                                               
RUNL     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
***********************************************************************         
* EXTERNAL ADDRESS LIST                                               *         
***********************************************************************         
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    V(SORTER)                                                        
         DC    V(PRNTBL)                                                        
         DC    V(SQUASHER)                                                      
         DC    V(CHOPCON)                                                       
         DC    V(ACSLRY)                                                        
         DC    A(IO1)                                                           
         DC    A(IO2)                                                           
         DC    A(IOSORT)                                                        
         DC    A(IOSORTSV)                                                      
         DC    A(PYTABLE)                                                       
         EJECT                                                                  
***********************************************************************         
*              LITERALS                                               *         
***********************************************************************         
*                                                                               
EFFS     DC    20X'FF'                                                          
NINES    DC    12C'9'                                                           
ACCFIL   DC    CL8'ACCFIL'                                                      
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
RECCARD  DC    C'RECORD TYPE=F,LENGTH=281 '                                     
SORTCARD DC    C'SORT FIELDS=(1,17,A),FORMAT=BI,WORK=1 '                        
*                                                                               
TYPETBL  DC    AL1(MSATSLRY),C'SAL '                                            
         DC    AL1(MSATOVER),C'OT  '                                            
         DC    AL1(MSATTEMP),C'TEMP'                                            
         DC    AL1(MSATBONU),C'BON '                                            
         DC    AL1(MSATPENS),C'PEN '                                            
         DC    AL1(MSATBENE),C'BEN '                                            
         DC    AL1(MSATADMN),C'ADM '                                            
         DC    AL1(MSATBUDG),C'BUD '                                            
         DC    AL1(MSATRATE),C'RTE '                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
BASETBL  DC    C'MON'                                                           
         DC    C'QTR'                                                           
         DC    C'YTD'                                                           
         DC    C'AN '                                                           
         DC    C'HR '                                                           
         DC    X'FF'                                                            
         SPACE 1                                                                
MAXLEN   EQU   12                  MAX LENGTH OF ACCOUNT                        
MIN9Q    EQU   3                   MIN # 9'S PRESENT FOR OVERHEAD ACCT          
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              COLUMN PRINTING CONTROL TABLE                          *         
***********************************************************************         
*                                                                               
*              CL1 - QOPT2 VALUE                                                
*              AL1 - HEADINGS TO USE                                            
*              AL1 - COLUMNS TO PRINT                                           
*                                                                               
OPT2TB   DS    0XL3                                                             
         DC    C' ',AL1(0),AL1(SAL+PEN+BEN+IND+TOT)                             
         DC    C'1',AL1(2),AL1(SAL+PEN+BEN+IND+OTH+TOT)                         
         DC    C'2',AL1(4),AL1(SAL+PEN+BEN+TOT)                                 
         DC    C'3',AL1(6),AL1(SAL+PEN+TOT)                                     
         DC    C'4',AL1(8),AL1(SAL)                                             
OPT2TB#  EQU   (*-OPT2TB)/L'OPT2TB                                              
*                                                                               
SAL      EQU   X'01'               SALARY COLUMN                                
PEN      EQU   X'02'               PENSION COLUMN                               
BEN      EQU   X'04'               BENEFIT COLUMN                               
IND      EQU   X'08'               INDIRECT COLUMN                              
OTH      EQU   X'10'               OTHER COLUMN                                 
TOT      EQU   X'20'               TOTAL COLUMN                                 
         EJECT                                                                  
***********************************************************************         
*              ACCUMULATOR OFFSET TABLE                               *         
***********************************************************************         
*                                                                               
OFFSET   DS    0XL2                                                             
         DC    AL1(PATTSAL),AL1(SRTSALRY-SRTACCMS)                              
         DC    AL1(PATTPEN),AL1(SRTPENSN-SRTACCMS)                              
         DC    AL1(PATTBEN),AL1(SRTBENFT-SRTACCMS)                              
         DC    AL1(PATTIND),AL1(SRTINDIR-SRTACCMS)                              
         DC    AL1(PATTOTH),AL1(SRTOTHER-SRTACCMS)                              
         EJECT                                                                  
***********************************************************************         
*              BOX HOOK                                               *         
***********************************************************************         
*                                                                               
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
*                                                                               
         USING BOXD,R4                                                          
         L     R4,ADBOX                                                         
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXCOLS,C'L'                                                     
         CLI   RCSUBPRG,9          SKIP ALL THIS IF OVERHEAD                    
         BH    BX500                                                            
         CLI   QOPT3,C'O'                                                       
         BE    *+12                                                             
         MVI   BOXCOLS+40,C'C'                                                  
         MVI   BOXCOLS+51,C'C'                                                  
*                                                                               
         CLI   RCSUBPRG,1          SAL/BEN/PEN/IND/TOT                          
         BH    BX100                                                            
BX010    MVI   BOXCOLS+62,C'C'                                                  
         MVI   BOXCOLS+79,C'C'                                                  
         MVI   BOXCOLS+96,C'C'                                                  
         MVI   BOXCOLS+113,C'C'                                                 
         MVI   BOXCOLS+130,C'C'                                                 
         MVI   BOXCOLS+147,C'R'                                                 
         B     BXIT                                                             
*                                                                               
BX100    CLI   RCSUBPRG,3          SAL/BEN/PEN/IND/OTH/TOT                      
         BH    BX200                                                            
BX110    MVI   BOXCOLS+62,C'C'                                                  
         MVI   BOXCOLS+79,C'C'                                                  
         MVI   BOXCOLS+96,C'C'                                                  
         MVI   BOXCOLS+113,C'C'                                                 
         MVI   BOXCOLS+130,C'C'                                                 
         MVI   BOXCOLS+147,C'C'                                                 
         MVI   BOXCOLS+164,C'R'                                                 
         B     BXIT                                                             
*                                                                               
BX200    CLI   RCSUBPRG,5          SAL/BEN/PEN/TOT                              
         BH    BX300                                                            
BX210    MVI   BOXCOLS+62,C'C'                                                  
         MVI   BOXCOLS+79,C'C'                                                  
         MVI   BOXCOLS+96,C'C'                                                  
         MVI   BOXCOLS+113,C'C'                                                 
         MVI   BOXCOLS+130,C'R'                                                 
         B     BXIT                                                             
*                                                                               
BX300    CLI   RCSUBPRG,7          SAL/BEN/TOT                                  
         BH    BX400                                                            
BX310    MVI   BOXCOLS+62,C'C'                                                  
         MVI   BOXCOLS+79,C'C'                                                  
         MVI   BOXCOLS+96,C'C'                                                  
         MVI   BOXCOLS+113,C'R'                                                 
         B     BXIT                                                             
*                                                                               
BX400    CLI   RCSUBPRG,9          SAL                                          
         BH    BX500                                                            
BX410    MVI   BOXCOLS+62,C'C'                                                  
         MVI   BOXCOLS+79,C'R'                                                  
         B     BXIT                                                             
*                                                                               
BX500    CLI   RCSUBPRG,10         SAL/BEN/PEN/TOT                              
         BH    BX600                                                            
         MVI   BOXCOLS+41,C'C'                                                  
         MVI   BOXCOLS+48,C'C'                                                  
         MVI   BOXCOLS+66,C'C'                                                  
         MVI   BOXCOLS+76,C'C'                                                  
         MVI   BOXCOLS+96,C'R'                                                  
         B     BXIT                                                             
*                                                                               
BX600    CLI   RCSUBPRG,15         SAL/BEN/PEN/TOT                              
         BH    BXIT                                                             
         ZIC   RF,RCSUBPRG                                                      
         SH    RF,=H'11'                                                        
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     BX010                                                            
         B     BX110                                                            
         B     BX210                                                            
         B     BX310                                                            
         B     BX410                                                            
*                                                                               
BXIT     MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
         XMOD1 1                                                                
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              BUFFERS                                                *         
***********************************************************************         
*                                                                               
IO1      DS    0D                  IOAREA #1                                    
         DS    2000C                                                            
MAXACC   EQU   *-IO1                                                            
*                                                                               
IO2      DS    0D                  IOAREA #2                                    
         DS    2000C                                                            
*                                                                               
IOSORT   DS    0D                  SORT RECORD AREA                             
         DS    500C                                                             
*                                                                               
IOSORTSV DS    0D                  SORT RECORD SAVE AREA                        
         DS    500C                                                             
*                                                                               
PYTABLE  DS    0D                  PAYCODE TABLE                                
         DS    4000C                                                            
         EJECT                                                                  
***********************************************************************         
*              ELEMENT TABLE DSECT                                    *         
***********************************************************************         
*                                                                               
PYCODED  DSECT                                                                  
PYNUM    DS    CL1                 PAYROLL NUMBER                               
PYCODE   DS    CL5                 PAYROLL CODE                                 
PYTYPE   DS    XL1                 PAYROLL TYPE (SAL,PEN,BEN,IND)               
PYDESC   DS    CL15                DESCRIPTION                                  
PYREV    DS    CL5                 REVERSAL TYPE                                
PYLNQ    EQU   *-PYCODED                                                        
         EJECT                                                                  
***********************************************************************         
*              PRINT LINE DSECT                                       *         
***********************************************************************         
*                                                                               
PLINED   DSECT                                                                  
PLIN     DS    0C                                                               
         DS    CL2                                                              
PSUMDPT  DS    0CL5                DEPARTMENT CODE                              
PLINPERS DS    CL7                 EMPLOYEE NUMBER                              
         DS    CL1                                                              
PSUMDPTN DS    0CL36               DEPARTMENT NAME                              
PLINNAME DS    CL29                NAME (LAST, FIRST M.)                        
         DS    CL3                                                              
PLINCKDT DS    CL8                 CHECK DATE YYMMDD                            
         DS    CL3                                                              
PLINCODE DS    CL5                 PAY CODE                                     
         DS    CL3                                                              
PLIN$    DS    0CL17               DOLLAR COLUMNS                               
         ORG PLINPERS                                                           
PLINTOT  DS    CL38                TOTAL LINE                                   
         ORG   PLINCKDT                                                         
         DS    CL1                                                              
PLINTYPE DS    CL4                 PAYROLL TYPE                                 
         DS    CL4                                                              
PLINDBEG DS    CL6                 PAYROLL DATE BEG                             
         DS    CL1                                                              
PLINDEND DS    CL6                 PAYROLL DATE END                             
         DS    CL6                                                              
PLINBASE DS    CL3                 PAYROLL BASIS                                
         DS    CL6                                                              
PLINRATE DS    CL17                PAYROLL RATE                                 
         EJECT                                                                  
***********************************************************************         
*              WORKING STORAGE                                        *         
***********************************************************************         
*                                                                               
ACCU02D  DSECT                                                                  
VTYPES   DS    0A                  EXTERNAL ADDRESSES                           
SORTER   DS    A                   SORTER                                       
VPRNTBL  DS    A                   PRNTBL                                       
SQUASHER DS    A                   SQUASHER                                     
CHOPCON  DS    A                   CHOPPER                                      
ACSLRY   DS    A                   SALARY                                       
AIO1     DS    A                   IO AREA #1 (2000 BYTES)                      
AIO2     DS    A                   IO AREA #2 (2000 BYTES)                      
ASRTREC  DS    A                   A(SORT AREA)                                 
ASRTSAV  DS    A                   A(SORT SAVE AREA)                            
APYTABLE DS    A                   PAYCODE TABLE                                
VTYPLNQ  EQU   *-VTYPES                                                         
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
*                                                                               
SRTSW    DS    XL1                 SORTER SWITCH                                
SRTPUT   EQU   X'01'               SOMETHING WAS PUT TO SORTER                  
*                                                                               
RQSW     DS    XL1                 REQUEST SWITCH                               
RQFIRST  EQU   X'01'               FIRST TIME FOR REQUEST                       
RQLAST   EQU   X'02'               LAST TIME FOR REQUEST                        
*                                                                               
INDICS   DS    XL1                 RUN INDICATOR                                
INDIEMUD EQU   X'01'               EMULATED FILE                                
INDISORT EQU   X'02'               SORTER WAS INITIATED                         
*                                                                               
STDATE   DS    PL3                 START DATE                                   
ENDATE   DS    PL3                 END DATE                                     
START    DS    CL2                                                              
END      DS    CL2                                                              
CMPABBR  DS    CL7                 COMPANY ABBREVIATION                         
CMPNAME  DS    CL36                COMPANY NAME                                 
*                                                                               
TYPE     DS    XL1                                                              
RCCOLUMN DS    XL1                                                              
ELCODE   DS    XL1                                                              
COMMAND  DS    CL8                 USED IN DATAMGR CALL                         
TODAYP   DS    PL3                 TODAY'S DATE PACKED                          
WRK2     DS    CL120                                                            
*                                                                               
SVSRTKEY DS    CL52                SAVED SORT KEY                               
SVACT    DS    CL48                                                             
SVPERS   DS    CL12                                                             
SVKEY    DS    CL42                                                             
SVSUBPRG DS    XL1                 SAVE SUB PROGRAM #                           
*                                                                               
LASTTOTS DS    CL48                                                             
TOTAL    DS    PL8                                                              
*                                                                               
LEVELS   DS    0H                                                               
LEVELA   DS    CL1                 LENGTH OF LEVEL A                            
LVLALEN  DS    CL1                 LENGTH OF LEVEL A                            
LEVELB   DS    CL1                 LENGTH OF LEVEL A+B                          
LVLBLEN  DS    CL1                 LENGTH OF LEVEL B                            
LEVELC   DS    CL1                 LENGTH OF LEVEL A+B+C                        
LVLCLEN  DS    CL1                 LENGTH OF LEVEL C                            
LEVELD   DS    CL1                 LENGTH OF LEVEL A+B+C+D                      
LVLDLEN  DS    CL1                 LENGTH OF LEVEL D                            
*                                                                               
LEVELNM  DS    0CL15                                                            
LEVELANM DS    CL15                LEDGER LEVEL NAMES (HIERARCHY)               
LEVELBNM DS    CL15                                                             
LEVELCNM DS    CL15                                                             
LEVELDNM DS    CL15                                                             
*                                                                               
EXPOPTN  DS    CL12                EXPANDED OPTION1 TAPE/MANUAL                 
*                                                                               
MTHD3    DS    CL3                 METHOD 3 CHAR CODE                           
MTHDNAME DS    CL36                EXPANDED METHOD NAME                         
*                                                                               
ITEM     DS    PL2                 ITEM COUNTER                                 
GOTOVHD  DS    CL1                 GOT AN OVERHEAD                              
SALAREA  DS    (SLRLEN)C           SALARY AREA                                  
NOVHDSAV DS    (SRTLNQ)C           SAVE NON-OVERHEAD SORT BUFFER                
         EJECT                                                                  
***********************************************************************         
*              SORT RECORD DSECT                                      *         
***********************************************************************         
*                                                                               
SRTRECD  DSECT                                                                  
SRTKEY   DS    0CL17                                                            
SRTKREPT DS    XL1                 REPORT NUMBER, TO HELP SORT                  
SRTKACT  DS    CL12                ACCOUNT CODE                                 
SRTKCKDT DS    PL3                 CHECK DATE YYMMDD                            
SRTKNUM  DS    XL1                 PAYROLL CODE #                               
*                                                                               
SRTSTAT  DS    XL1                                                              
SRTSTITM EQU   0                   ITEM RECORD                                  
SRTSTPER EQU   1                   PERSON  TOTAL                                
SRTSTDPT EQU   2                   DEPT    TOTAL                                
SRTSTOFF EQU   3                   OFFICE  TOTAL                                
SRTSTREQ EQU   4                   REQUEST TOTAL                                
*                                                                               
SRTCKDT  DS    PL3                 CHECK DATE YYMMDD                            
SRTNPERS DS    CL36                NAME (LAST, FIRST M.I.)                      
SRTCODE  DS    CL5                 PAYROLL CODE                                 
SRTDESC  DS    CL15                PAYROLL CODE DESCRIPTION                     
SRTCOFFC DS    CL12                OFFICE CODE                                  
SRTCDEPT DS    CL12                DEPT   CODE                                  
SRTCSDPT DS    CL12                SDPT   CODE                                  
SRTCPERS DS    CL12                PERSON CODE                                  
SRTNOFFC DS    CL36                OFFICE NAME                                  
SRTNDEPT DS    CL36                DEPARTMENT NAME                              
SRTNSDPT DS    CL36                SUBDEPARTMENT NAME                           
*                                                                               
SRTACCMS DS    0PL8                *** ACCUMS ***                               
SRTSALRY DS    PL8                 - SALARY                                     
SRTPENSN DS    PL8                 - PENSION                                    
SRTBENFT DS    PL8                 - BENEFIT                                    
SRTINDIR DS    PL8                 - INDIRECT                                   
SRTOTHER DS    PL8                 - OTHER                                      
SRTCOUNT DS    PL8                 COUNTER                                      
SRT#ACCM EQU   (*-SRTACCMS)/L'SRTACCMS                                          
SRTLNQ   EQU   *-SRTRECD                                                        
*                                                                               
* MSA ELEMENT FOR RCSUBPRG=10, KEY PART FOR SORTING DATES                       
*                                                                               
         ORG   SRTKCKDT                                                         
SRTKSDTC DS    XL2                 START DATE COMPLEMENT                        
SRTKNDTC DS    XL2                 END DATE COMPLEMENT                          
         ORG   SRTDESC                                                          
SRTMSATP DS    CL1                 SALARY TYPE                                  
SRTMSAST DS    PL2                 START DATE                                   
SRTMSAND DS    PL2                 END DATE                                     
SRTMSABS DS    CL1                 BASIS                                        
         EJECT                                                                  
***********************************************************************         
*              OTHER INCLUDES                                         *         
***********************************************************************         
*                                                                               
       ++INCLUDE DDSLRD                                                         
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021ACREPCU02 05/01/02'                                      
         END                                                                    
