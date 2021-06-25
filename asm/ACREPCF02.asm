*          DATA SET ACREPCF02  AT LEVEL 037 AS OF 01/20/03                      
*PHASE ACCF02A,+0                                                               
*INCLUDE DATVAL                                                                 
*INCLUDE ACCEDIT                                                                
*INCLUDE ACLIST                                                                 
*INCLUDE CHOPCON                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE UNDERLIN                                                               
*INCLUDE PERVERT                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'COST OF FINANCING REPORT'                                       
******************************************************************              
*        REGISTER USAGE                                          *              
*        R0 - WORK                                               *              
*        R1 - WORK                                               *              
*        R2 - WORK                                               *              
*        R3 - WORK/USED FOR ACCUMULATOR LOGIC                    *              
*        R4 - WORK/USED FOR PRINT LINE ADDRESSING                *              
*        R5 - WORK/USED FOR ACCOUNT TABLE PROCESSING             *              
*        R6 - WORK/USED FOR ACCOUNT TABLE PROCESSING             *              
*        R7 - 3RD BASE REGISTER                                  *              
*        R8 - COVERS WIDE PRINT DSECT                            *              
*        R9 - 2ND BASE REGISTER                                  *              
*        RA - GLOBAL WORKING STORAGE                             *              
*        RB - 1ST BASE REGISTER                                  *              
*        RC - LOCAL WORKING STORAGE                              *              
*        RD - N/A                                                *              
*        RE - WORK                                               *              
*        RF - WORK                                               *              
*                                                                *              
*        OPTIONS:                                                *              
*        1.   LEVEL OF DETAIL - (1-4) - THE DETAIL REPORT        *              
*             ALWAYS PRINTS THE LOWEST LEVEL.  THIS OPTION       *              
*             CONTROLS THE LEVEL OF DETAIL ON THE SUMMARY        *              
*             REPORT.  DEFAULT IS LOWEST LEVEL OF ACCT           *              
*                                                                *              
*        2.   COMBINE HIGHER LEVELS - (C) - THIS WILL COMBINE    *              
*             ACCT LEVELS.  EXAMPLE/ ACCT X111FORD  AND Y222FORD *              
*             WOULD NORMALLY PRODUCE 2 ACCOUNT TOTALS.  IF A     *              
*             'C' IS ENTERED A ONE LINE TOTAL WILL BE PRODUCED   *              
*             FOR FORD.  (OPT 1 WOULD NEED TO BE A 3 IN THIS EX) *              
*             DEFAULT IS NOT TO COMBINE LEVELS                   *              
*                                                                *              
*        3.   PRINT DETAIL REPORT - (Y/N) - DEFAULT IS NO        *              
*                                                                *              
*        4.   SUPPRESS AGING COLS - (S) - DEFAULT IS TO PRINT    *              
*                                                                *              
*        7.   DOWNLOAD SUMMARY REPORT  OPT7 = Y                  *              
*                                                                *              
*        PROFILES:                                               *              
*        1.   SIMPLE OR COMPOUNDED INTEREST - (S/C)              *              
*             DEFAULT IS COMPOUNDED.                             *              
******************************************************************              
         EJECT ,                                                                
ACCF02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACCF**,R9,R7    R9=2ND BASE REGISTER, R7=3RD BASE            
         L     RA,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
*                                                                               
         LA    RC,SPACEND                                                       
         L     R8,VBIGPRNT                                                      
*                                                                               
         USING ACCF02D,RC          RC=A(SAVE W/S)                               
         USING BIGPRNTD,R8         R8=ADDRESSES WIDE PRINT                      
*                                                                               
         EJECT ,                                                                
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
         CLI   MODE,LEDGFRST       LEDGER FIRST                                 
         BE    LEDGEF                                                           
         CLI   MODE,LEVAFRST       GET HEADINGS FOR LEVELS                      
         BE    LEVAF                                                            
         CLI   MODE,LEVBFRST                                                    
         BE    LEVBF                                                            
         CLI   MODE,LEVCFRST                                                    
         BE    LEVCF                                                            
         CLI   MODE,ACCFRST        LOWEST LEVEL ACCNT                           
         BE    ACCF                                                             
         CLI   MODE,PROCACC        FIRST FOR ACCOUNT                            
         BE    PRACC                                                            
         CLI   MODE,PROCTRNS       BUILD BIN TABLE                              
         BE    PROCTR                                                           
         CLI   MODE,SBACLAST       CATCH ANY REMAINING TABLE ENTRIES            
         BE    SBACL                                                            
         CLI   MODE,ACCLAST        PRINT TOTALS FOR LEVELS                      
         BE    LEVDL                                                            
         CLI   MODE,LEVCLAST       PRINT LEVEL TOTALS                           
         BE    LEVCL                                                            
         CLI   MODE,LEVBLAST                                                    
         BE    LEVBL                                                            
         CLI   MODE,LEVALAST                                                    
         BE    LEVAL                                                            
         CLI   MODE,REQLAST        REQUEST LAST - PRINT SUMMARY                 
         BE    REQL                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT ,                                                                
******************************************************************              
*        RUN FIRST                                               *              
******************************************************************              
*                                                                               
RUNF     DS    0H                                                               
         LA    RE,VTYPES           RELOCATE VTYPES                              
         LA    R0,ADCONS                                                        
         LA    RF,VTYPLNQ                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,=A(BOXRC)        SET UP BOX ROUTINE                           
         ST    RC,0(,R2)                                                        
         L     R2,VEXTRAS                                                       
*                                                                               
         USING RUNXTRAD,R2                                                      
*                                                                               
         L     R2,ADMASTD                                                       
*                                                                               
         USING MASTD,R2                                                         
*                                                                               
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
*                                                                               
         USING ACKEYD,R5                                                        
*                                                                               
         LA    R5,RKEY             SEARCH FOR COMPANY RATE RECORD               
         MVC   RKEY,SPACES                                                      
         MVI   ACRTCODE,ACRTEQU    X'2D'                                        
         MVI   ACRTSREC,ACRTSEQU   X'05'                                        
         MVC   ACRTCOMP(1),RCCOMPFL                                             
         L     R5,AIOAREA1                                                      
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',RKEY,(R5)                    
         CLI   8(R1),0                                                          
         BE    RUNF5                                                            
         LA    R1,RATETBDF         DEFAULT TABLE                                
         ST    R1,ARATETB                                                       
         B     EXIT                                                             
*                                                                               
         USING ACRATED,R5                                                       
*                                                                               
RUNF5    DS    0H                  SEARCH FOR RATE ELEMENTS                     
         L     R6,ARATETB                                                       
         AH    R5,DATADISP                                                      
*                                                                               
RUNF10   CLI   0(R5),ACRIELQ       X'6E'                                        
         BE    RUNF30                                                           
         CLI   0(R5),0                                                          
         BE    RUNF60                                                           
RUNF20   ZIC   R1,1(,R5)                                                        
         AR    R5,R1                                                            
         B     RUNF10                                                           
*                                                                               
         USING RATED,R6                                                         
*                                                                               
RUNF30   GOTO1 DATCON,DMCB,(2,ACRIDAT),(1,RTDATE)                               
         ICM   R1,7,ACRIRAT                                                     
         CVD   R1,DUB                                                           
         ZAP   RTTRUE,DUB                                                       
         ZAP   PACK16,DUB                                                       
         MP    PACK16,=PL8'1000'                                                
         DP    PACK16,=PL8'365'                                                 
         ZAP   RTRATE(8),PACK16(8)                                              
         LA    R6,RTLNQ(,R6)       BUMP TO NEXT AVAILABLE TABLE AREA            
         B     RUNF20                                                           
*                                                                               
RUNF60   MVI   RTDATE,X'FF'        MARK END OF TABLE                            
         B     EXIT                                                             
*                                                                               
         DROP  R2,R5,R6            KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        REQUEST FIRST                                           *              
******************************************************************              
*                                                                               
REQF     GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         GOTO1 DATCON,DMCB,(5,TODAYP),(1,TODAYP)                                
         BAS   RE,CLRACCUM                                                      
         ZAP   AGEDAY1,=P'10'      AGING COLUMN DEFAULTS                        
         ZAP   AGEDAY2,=P'30'                                                   
         ZAP   AGEDAY3,=P'60'                                                   
*                                                                               
         CLI   QOPT7,C'Y'                                                       
         BNE   REQF20                                                           
         MVI   QOPT4,C' '          DON'T SUPRESS AGING COL IF DWNLDNG           
         MVI   QOPT3,C' '          NO DETAIL REPORT IF DOWNLOADING              
         MVI   RCSUBPRG,9          SET PRINT RTE AS INVALID                     
         MVC   MYSUBPRG,RCSUBPRG                                                
         GOTO1 ADWNL,DMCB,(RC),DWNINIT      INITIALIZE DOWNLOAD RTE             
         B     *+8                                                              
*                                                                               
REQF20   MVI   RCSUBPRG,0          SET CORRECT HEADINGS                         
         MVC   MYSUBPRG,RCSUBPRG                                                
         L     R2,AMONACC          GET START OF FINANCIAL YEAR                  
*                                                                               
         USING ACMD,R2                                                          
*                                                                               
         XC    FINYEAR,FINYEAR                                                  
         OC    ACMFDTE,ACMFDTE                                                  
         BZ    *+14                                                             
         MVC   FINYEAR(2),ACMFDTE                                               
         MVI   FINYEAR+2,X'01'                                                  
*                                                                               
         MVC   ENDDATE,TODAYP      ENDDATE DEFAULT IS TO TODAY                  
         CLC   QEND,XSPACES                                                     
         BE    REQF1                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(1,ENDDATE)                                 
*                                                                               
REQF1    MVC   STDATE,ENDDATE                                                   
         MVI   STDATE+2,X'01'      DEFAULT IS TO FIRST OF MONTH                 
         CLC   QSTART,XSPACES                                                   
         BE    REQF2                                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(1,STDATE)                                
*                                                                               
         USING ACMD,R2                                                          
*                                                                               
REQF2    L     R2,AMONACC                CLEAR MOA RANGE DATES SO               
*                                    MONACC WILL READ FROM                      
         XC    ACMMSTR,ACMMSTR             BEGINING OF TIME                     
*                                          START DATE TO PRINT                  
         GOTO1 DATCON,DMCB,(1,STDATE),(X'20',QSTART)                            
*                                          END   DATE TO PRINT                  
         GOTO1 DATCON,DMCB,(1,ENDDATE),(X'20',QEND)                             
         OC    FINYEAR,FINYEAR                                                  
         BNZ   REQF3                                                            
         MVC   FINYEAR,STDATE      IF NO START OF FIN YEAR ON COMP REC          
         MVC   FINYEAR+1(2),=X'0101'  THEN DEFAULT IS JAN01 OF SAME YR          
*                                                                               
         USING ACCOMPD,R5                                                       
*                                                                               
REQF3    L     R5,ADCMPEL                                                       
         MVC   CMPABBR,ACMPABBR    COMPANY ABBREVIATION                         
         L     R5,ADCMPNAM                                                      
         BAS   RE,GETNAME                                                       
         MVC   CMPNAME,WORK        COMPANY NAME                                 
*                                                                               
* BUILD ACCRUAL DATES TO FILTER ON                                              
*                                                                               
         GOTO1 DATCON,DMCB,(1,ENDDATE),(3,BENDATE)                              
         MVI   BAT55MOS,X'FF'                                                   
         CLC   QMOSEND,SPACES                                                   
         BE    REQF10                                                           
         MVC   WORK(4),QMOSEND                                                  
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   BAT55MOS,WORK+6                                                  
*                                                                               
*EQF10   MVI   BAT55DTE,X'FF'      ***IF ENDDATE NO REQUIRED BRANCH             
REQF10   GOTO1 DATCON,DMCB,(3,BENDATE),(0,TEMPDT)                               
         GOTO1 ADDAY,DMCB,(C'M',TEMPDT),TEMPDT,F'-1'                            
         GOTO1 DATCON,DMCB,(0,TEMPDT),(1,BAT55DTE)                              
         B     EXIT                                                             
         DROP  R5                                                               
         DROP  R2                                                               
         EJECT ,                                                                
******************************************************************              
*        LEDGER FIRST                                            *              
******************************************************************              
*                                                                               
LEDGEF   DS    0H                                                               
         L     R5,ADLDGEL          LEDGER ELEMENT X'14'                         
*                                                                               
         USING ACLEDGD,R5                                                       
*                                                                               
*        TEMPORARILY SAVE THE DISPLACEMENT TO CLIENT TO LATER                   
*        EQUATE IT TO AN ACCOUNT LEVEL                                          
         MVC   CLIPOS,ACLTCLI                                                   
*                                                                               
         USING ACHEIRD,R5                                                       
         L     R5,ADLDGHIR         HEIRARCHY ELEMENT                            
         MVC   LEVELA,ACHRLEVA     LENGTH OF LEVEL A                            
         MVC   LEVELB,ACHRLEVB                     B                            
         MVC   LEVELC,ACHRLEVC                     C                            
         MVC   LEVELD,ACHRLEVD                     D                            
         MVC   LEVELANM,ACHRDESA   LEVEL NAMES                                  
         MVC   LEVELBNM,ACHRDESB                                                
         MVC   LEVELCNM,ACHRDESC                                                
         MVC   LEVELDNM,ACHRDESD                                                
*                                                                               
*        EQUATE CLIENT POSTITION TO AN ACCOUNT LEVEL                            
         MVI   CLILEV,2                  IF NO CLI= SET ASSUME                  
         CLI   CLIPOS,0                  LEVEL 2                                
         BE    LED1                                                             
         MVI   CLILEV,1                                                         
         CLC   LEVELA,CLIPOS                                                    
         BH    LED1                                                             
         MVI   CLILEV,2                                                         
         CLC   LEVELB,CLIPOS                                                    
         BH    LED1                                                             
         MVI   CLILEV,3                                                         
         CLC   LEVELC,CLIPOS                                                    
         BH    LED1                                                             
         MVI   CLILEV,4                                                         
*                                                                               
LED1     LA    R0,4                                                             
         LA    R1,LEVELS                                                        
         LA    R2,1                                                             
*                                                                               
LED2     CLI   0(R1),X'0C'         FIND LOWEST LEVEL                            
         BE    LED4                                                             
         LA    R1,1(,R1)                                                        
         LA    R2,1(,R2)           BUMP LEVEL COUNTER                           
         BCT   R0,LED2                                                          
         DC    H'0'                LEVEL NOT FOUND                              
*                                                                               
LED4     STC   R2,LOWLEV           SAVE LOWEST LEVEL ACCT                       
*                                                                               
         CLI   QOPT1,C' '          DEFAULT TO LOWEST LEVEL DETAIL               
         BNE   LED10                                                            
         LA    R1,LEVELD                                                        
         LA    R0,4                                                             
*                                                                               
LED5     CLI   0(R1),0                                                          
         BNE   LED8                                                             
         BCTR  R1,0                SUBTRACT 1                                   
         SH    R0,=H'1'                                                         
         B     LED5                                                             
*                                                                               
LED8     STC   R0,QOPT1                                                         
         OI    QOPT1,X'F0'         X'01' --> C'1'                               
         B     EXIT                                                             
*                                                                               
LED10    LA    R1,LEVELS           VALIDATE LEVEL ENTERED                       
         NI    QOPT1,X'0F'         CONVERT X'F1' -> X'01'                       
         ZIC   R2,QOPT1                                                         
         BCTR  R2,0                                                             
         AR    R1,R2                                                            
*                                                                               
LED15    CLI   0(R1),0             INSURE LEVEL ENTERED IS IN STRUCTURE         
         BNE   LED20                                                            
         BCTR  R1,0                                                             
         ZIC   R0,QOPT1                                                         
         SH    R0,=H'1'                                                         
         STC   R0,QOPT1                                                         
         B     LED15                                                            
*                                                                               
LED20    OI    QOPT1,X'F0'                                                      
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT ,                                                                
******************************************************************              
*        LEVEL A FIRST                                           *              
******************************************************************              
*                                                                               
LEVAF    DS    0H                                                               
         LA    R4,PRNTBLK1         CLEAR ENTIRE PRINT BLOCK                     
         BAS   RE,CLRBLK                                                        
         L     R5,ADLVANAM         GET ACCOUNT NAME                             
         BAS   RE,GETNAME                                                       
         MVC   LVANAME(L'LVANAME),WORK                                          
         MVC   LVAACCT,XSPACES                                                  
         L     R1,ADACC            GET ISOLATED ACCOUNT CODE                    
         LA    R1,3(,R1)                                                        
         ZIC   RF,LEVELA                                                        
         SH    RF,=H'1'                                                         
         BM    LEVAF10                                                          
         EXMVC RF,LVAACCT,0(R1)                                                 
*                                                                               
LEVAF10  MVC   HED1,XSPACES                                                     
         MVC   HED1(L'LEVELNM),LEVELANM                                         
         MVC   HED1+16(L'LVACCT),LVAACCT                                        
         MVC   HED1+30(L'LVNAME),LVANAME                                        
         MVI   HDFLAG,C'Y'                                                      
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        LEVEL B FIRST                                           *              
******************************************************************              
*                                                                               
LEVBF    DS    0H                                                               
         CLI   LEVELB,0                                                         
         BE    EXIT                                                             
         L     R5,ADLVBNAM         GET ACCOUNT NAME                             
         BAS   RE,GETNAME                                                       
         MVC   LVBNAME(L'LVBNAME),WORK                                          
*                                                                               
         MVC   LVBACCT,XSPACES                                                  
         L     R1,ADACC                                                         
         LA    R1,3(,R1)           BUMP PAST CO/U/L                             
         ZIC   R0,LEVELA           AND LEVEL A CODE                             
         AR    R1,R0                                                            
         ZIC   RF,LEVELB           GET ACCOUNT CODE                             
         ZIC   R0,LEVELA                                                        
         SR    RF,R0                                                            
         SH    RF,=H'1'                                                         
         BM    LEVBF5                                                           
         EXMVC RF,LVBACCT,0(R1)                                                 
*                                                                               
LEVBF5   MVC   HED2,XSPACES                                                     
         CLI   CLILEV,2                                                         
         BNH   LEVBF10                                                          
         MVC   HED2(L'LEVELNM),LEVELBNM                                         
         MVC   HED2+16(L'LVACCT),LVBACCT                                        
         MVC   HED2+30(L'LVNAME),LVBNAME                                        
         MVI   HDFLAG,C'Y'                                                      
         B     EXIT                                                             
*                                                                               
         USING PLINED,R4                                                        
*                                                                               
LEVBF10  DS    0H                                                               
         LA    R4,PRNTBLK1                                                      
         BAS   RE,CLRBLK                                                        
         MVC   WRK2,XSPACES                                                     
         MVC   WRK2(L'LEVELNM),LEVELBNM                                         
         MVC   WRK2+16(L'LVACCT),LVBACCT                                        
         MVC   WRK2+30(L'LVNAME),LVBNAME                                        
         GOTO1 SQUASHER,DMCB,WRK2,L'WRK2                                        
         MVC   PNAME,WRK2                                                       
         CLI   QOPT7,C'Y'                                                       
         BE    EXIT                                                             
         GOTO1 UNDERLIN,DMCB,(L'PNAME,PNAME),(X'BF',PNAME+L'XP)                 
         B     EXIT                                                             
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        LEVEL C FIRST                                           *              
******************************************************************              
*                                                                               
LEVCF    DS    0H                                                               
         CLI   LEVELC,0                                                         
         BE    EXIT                                                             
         L     R5,ADLVCNAM         GET ACCOUNT NAME                             
         BAS   RE,GETNAME                                                       
         MVC   LVCNAME(L'LVCNAME),WORK                                          
*                                                                               
         MVC   LVCACCT,XSPACES                                                  
         L     R1,ADACC                                                         
         LA    R1,3(,R1)           BUMP PAST CO/U/L                             
         ZIC   R0,LEVELB           AND LEVEL B CODE                             
         AR    R1,R0                                                            
         ZIC   RF,LEVELC           GET ACCOUNT CODE                             
         ZIC   R0,LEVELB                                                        
         SR    RF,R0                                                            
         SH    RF,=H'1'                                                         
         BM    LEVCF5                                                           
         EXMVC RF,LVCACCT,0(R1)                                                 
*                                                                               
LEVCF5   MVC   HED3,XSPACES                                                     
         CLI   CLILEV,3                                                         
         BNH   LEVCF10                                                          
         MVC   HED3(L'LEVELNM),LEVELCNM                                         
         MVC   HED3+16(L'LVACCT),LVCACCT                                        
         MVC   HED3+30(L'LVNAME),LVCNAME                                        
         MVI   HDFLAG,C'Y'                                                      
         B     EXIT                                                             
*                                                                               
         USING PLINED,R4                                                        
*                                                                               
LEVCF10  DS    0H                                                               
         LA    R4,PRNTBLK2                                                      
         BAS   RE,CLRBLK                                                        
         MVC   WRK2,XSPACES                                                     
         MVC   WRK2(L'LEVELNM),LEVELCNM                                         
         MVC   WRK2+16(L'LVACCT),LVCACCT                                        
         MVC   WRK2+30(L'LVNAME),LVCNAME                                        
         GOTO1 SQUASHER,DMCB,WRK2,L'WRK2                                        
         MVC   PNAME,WRK2                                                       
         CLI   QOPT7,C'Y'                                                       
         BE    EXIT                                                             
         GOTO1 UNDERLIN,DMCB,(L'PNAME,PNAME),(X'BF',PNAME+L'XP)                 
         B     EXIT                                                             
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        ACCOUNT LEVEL FIRST                                     *              
******************************************************************              
*                                                                               
ACCF     DS    0H                                                               
         MVC   WRK2,XSPACES                                                     
         L     R5,ADACCNAM         GET ACCOUNT NAME                             
         BAS   RE,GETNAME                                                       
         LA    R3,LVNAME                                                        
         ZIC   R2,LOWLEV                                                        
         BCTR  R2,0                                                             
         MH    R2,=Y(L'LVNAME)                                                  
         AR    R3,R2                                                            
         MVC   0(L'LVNAME,R3),WORK                                              
         MVC   WRK2+30(L'LVNAME),0(R3)                                          
*                                                                               
         LA    R3,LVACCT           CLEAR ACCT LEVEL                             
         ZIC   R2,LOWLEV                                                        
         BCTR  R2,0                                                             
         MH    R2,=Y(L'LVACCT)                                                  
         AR    R3,R2                                                            
         MVC   0(L'LVACCT,R3),XSPACES                                           
*                                                                               
         LA    R1,LEVELS                                                        
         ZIC   R2,LOWLEV                                                        
         BCTR  R2,0                                                             
         AR    R1,R2                                                            
         BCTR  R1,0                DROP BACK TO PREVIOUS LEVEL                  
         ZIC   R2,0(,R1)           GET LENGTH OF LEVEL                          
         L     R1,ADACC                                                         
         LA    R1,3(,R1)           BUMP PAST CO/U/L                             
         AR    R1,R2                                                            
         LA    RF,12                                                            
         SR    RF,R2                                                            
         SH    RF,=H'1'                                                         
         BM    ACCF10                                                           
         EXMVC RF,0(R3),0(R1)      SAVE OFF ISOLATED PART OF ACCT               
ACCF10   MVC   WRK2+16(L'LVACCT),0(R3)                                          
*                                                                               
         LA    R1,LEVELNM                                                       
         ZIC   R2,LOWLEV                                                        
         BCTR  R2,0                                                             
         MH    R2,=Y(L'LEVELNM)                                                 
         AR    R1,R2                                                            
         MVC   WRK2(L'LEVELNM),0(R1)                                            
         GOTO1 SQUASHER,DMCB,WRK2,L'WRK2                                        
*                                                                               
         USING PLINED,R4                                                        
*                                                                               
         LA    R4,PRNTBLK                                                       
         ZIC   R2,LOWLEV           GET LEVEL NAME                               
         SH    R2,=H'2'                                                         
         MH    R2,=Y(2*L'XP)                                                    
         AR    R4,R2                                                            
         BAS   RE,CLRBLK                                                        
         MVC   PNAME,WRK2                                                       
         CLI   QOPT7,C'Y'                                                       
         BE    EXIT                                                             
         GOTO1 UNDERLIN,DMCB,(L'PNAME,PNAME),(X'BF',PNAME+L'XP)                 
         B     EXIT                                                             
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        PROCESS ACCOUNT                                         *              
******************************************************************              
*                                                                               
PRACC    DS    0H                                                               
         MVI   NEWACC,C'Y'         SET FLAG TO PROCESS NEW ACCNT                
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        PROCESS TRANSACTION - BUILD BIN TABLE                   *              
******************************************************************              
*                                                                               
         USING ACKEYD,R5                                                        
*                                                                               
PROCTR   DS    0H                                                               
         L     R5,ADTRANS                                                       
         SH    R5,DATADISP                                                      
         OC    ACDTPEEL,ACDTPEEL                                                
         BNZ   EXIT                SKIP PEELED ITEMS                            
         CLI   NEWACC,C'Y'                                                      
         BNE   PROC5                                                            
         MVC   SAVEACC,ACKEYACC    SAVE FIRST ACCNT                             
         MVI   NEWACC,C'N'                                                      
         B     PROC8                                                            
*                                                                               
PROC5    CLC   SAVEACC(ACKEYSBR-ACKEYD),ACKEYACC                                
         BE    PROC8                                                            
         BAS   RE,REFL             PROCESS TABLE IN CHANGE OF ACCT              
         MVC   SAVEACC,ACKEYACC                                                 
*                                                                               
         USING TRANSD,R5                                                        
*                                                                               
PROC8    L     R5,ADTRANS                                                       
         CLI   TRNSTYPE,56         IGNORE REVERSALS                             
         BE    EXIT                                                             
         CLI   TRNSTYPE,55         FILTER TYPE 55 BY BAT55DTE                   
         BNE   PROC9                                                            
         CLI   BAT55MOS,X'FF'      ANY MOS END DATE?                            
         BE    PROC8A                                                           
         L     R2,AMONACC          GET START OF FINANCIAL YEAR                  
*                                                                               
         USING ACMD,R2                                                          
*                                                                               
         CLC   ACMMDTE,BAT55MOS                                                 
         BNE   EXIT                                                             
         CLI   BAT55DTE,X'FF'      IF MOS AND NO END DATE                       
         BE    PROC9                                                            
         B     PROC8B                                                           
*                                                                               
PROC8A   CLI   BAT55DTE,X'FF'      IGNORE ALL ACCRUALS                          
         BE    EXIT                                                             
*                                                                               
PROC8B   CLC   TRNSDATE,BAT55DTE                                                
         BNH   EXIT                                                             
*                                                                               
PROC9    CLI   TRNSEL,TRNSELQ      ONLY TAKE TRANSACTIONS - X'44'               
         BNE   EXIT                                                             
         CLC   TRNSDATE,ENDDATE    ELIMINATE TRANS. AFTER ENDDATE               
         BH    EXIT                                                             
*                                                                               
         USING ACKEYD,R5                                                        
*                                                                               
         L     R5,ADTRANS                                                       
         SH    R5,DATADISP                                                      
*                                                                               
         USING RUNXTRAD,R4         FILTER BY CONTRA ACCNT LIST RECORD           
*                                                                               
         L     R4,VEXTRAS                                                       
         OC    VLISTREC,VLISTREC                                                
         BZ    PROC10                                                           
         GOTO1 ACLIST,DMCB,VLISTREC,ACKEYCON+3                                  
         CLI   DMCB,0                                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   DMCB,C'E'           I=INCLUDE E=EXCLUDE                          
         BE    EXIT                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         USING TABLED,R6           BUILD REC AND ADD TO TABLE                   
*                                                                               
PROC10   L     R6,AWKAREA                                                       
         MVC   TBSTAT1,XSPACES                                                  
         MVC   TBACC,ACKEYACC      ACCOUNT                                      
         MVC   TBCON,ACKEYCON      CONTRA ACCOUNT                               
         MVC   TBTRNDT,ACKEYDTE    TRANSACTION DATE                             
         MVC   TBBNUM,ACKEYREF     BILL NUMBER                                  
         MVC   TBCKDT,ACKEYDTE                                                  
         MVC   TBCKNUM,XSPACES                                                  
         ZAP   TBCKAMT,=P'0'                                                    
         ZAP   TBCKAPPL,=P'0'                                                   
         ZAP   TBCKOVER,=P'0'                                                   
         XC    TBOVERST,TBOVERST                                                
         XC    TBDUEDT,TBDUEDT                                                  
         ZAP   TBBAMT,=P'0'                                                     
         ZAP   TBAR,=P'0'                                                       
         ZAP   TBOUT,=P'0'                                                      
         ZAP   TBCOF,=P'0'                                                      
         ZAP   TBYTD,=P'0'         YEAR TO DATE AMOUNT                          
         XC    TBDET,TBDET         A(DETAIL TABLE ENTRY)                        
         XC    TBDETNUM,TBDETNUM   NUMBER OF DETAIL ENTRIES                     
*                                                                               
         USING TRANSD,R5                                                        
*                                                                               
         L     R5,ADTRANS                                                       
         TM    TRNSSTAT,X'80'      DEBIT OR CREDIT?                             
         BZ    PROC60                                                           
         EJECT ,                                                                
*                                                                               
*        PROCESS DEBITS                                                         
*                                                                               
         USING TRANSD,R5                                                        
*                                                                               
         DS    0H                                                               
         MVI   TBTYPE,BILL                                                      
         MVI   TBSTAT2,DEBIT       DEBIT                                        
         ZAP   TBBAMT,TRNSAMNT     BILL AMOUNT                                  
         ZAP   TBAR,TRNSAMNT       A/R AMOUNT                                   
         MVC   TBDUEDT,TRNSDATE                                                 
*                                                                               
         SH    R5,DATADISP         GET BILL DUE DATE                            
         MVI   ELCODE,X'61'        DEFAULT IS TRANSACTION DATE                  
         BAS   RE,GETEL                                                         
         BNE   PROC600                                                          
*                                                                               
         USING TRDUED,R5                                                        
*                                                                               
         OC    TRDUDATE,TRDUDATE                                                
         BZ    PROC600                                                          
         GOTO1 DATCON,DMCB,(2,TRDUDATE),(1,TBDUEDT)                             
         B     PROC600                                                          
*                                                                               
*        PROCESS CREDITS                                                        
*                                                                               
         USING TRANSD,R5                                                        
*                                                                               
PROC60   DS    0H                  DEFAULT FOR CREDITS IS TRNSDATE              
         L     R5,ADTRANS                                                       
         MVC   TBDUEDT,TRNSDATE                                                 
         MVC   TBCKDT,TRNSDATE                                                  
         MVI   TBSTAT2,CREDIT                                                   
         CLC   TRNSNARR(5),=C'CHECK'                                            
         BNE   PROC70                                                           
         MVI   TBTYPE,CHECK                                                     
         ZAP   TBCKAMT,TRNSAMNT    CHECK AMOUNT                                 
         MVC   TBCKNUM,TRNSNARR+13 CHECK NUMBER                                 
         B     PROC200                                                          
*                                                                               
PROC70   CLC   TRNSNARR(6),=C'OFFSET'                                           
         BNE   PROC80                                                           
         MVI   TBTYPE,OFFSET                                                    
         ZAP   TBCKAMT,TRNSAMNT                                                 
         MVC   TBCKNUM,XSPACES                                                  
         B     PROC200                                                          
*                                                                               
PROC80   CLC   TRNSNARR(9),=C'WRITE-OFF'                                        
         BNE   PROC90                                                           
         MVI   TBTYPE,WRITEOFF                                                  
         ZAP   TBCKAMT,TRNSAMNT                                                 
         MVC   TBCKNUM,TRNSNARR+17                                              
         B     PROC200                                                          
*                                                                               
PROC90   MVI   TBTYPE,UNAPPLD                                                   
         ZAP   TBCKAMT,TRNSAMNT                                                 
         MVC   TBCKNUM,SPACES                                                   
*                                                                               
PROC200  DS    0H                  SCAN NARRATIVE FOR DATE                      
         LA    R2,TRNSNARR         POINT TO START OF NARRATIVE                  
         ZIC   R1,TRNSLEN                                                       
         SH    R1,=Y(TRNSLNQ+8)    COMPENSATE FOR L'DATE(MMMDD/YY)              
         BNP   PROC600                                                          
         LA    R2,TRNSNARR(R1)     BUMP TO END OF ELEMENT-8                     
         LR    R3,R1               GET NUMBER OF TIMES TO LOOP                  
*                                                                               
PROC300  DS    0H                                                               
         CLI   0(R2),C'S'          S/B AN ALPHA MONTH                           
         BH    PROC350                                                          
         CLI   0(R2),C'A'          LIKE SEPTEMBER OR APRIL                      
         BL    PROC350                                                          
         GOTO1 DATVAL,DMCB,(0,0(R2)),WORK                                       
         OC    0(4,R1),0(R1)       STOP IF FOUND A VALID DATE                   
         BNZ   PROC400                                                          
PROC350  BCTR  R2,0                BUMP BACK TO PREV CHAR                       
         BCT   R3,PROC300                                                       
         B     PROC600             DEFAULT TO TRNS DATE IF NO VALID DTE         
*                                                                               
PROC400  GOTO1 DATCON,DMCB,(0,WORK),(1,TBCKDT)                                  
*                                                                               
PROC600  CLC   TBCKDT,ENDDATE      ELIMINATE CR'S AFTER ENDDATE                 
         BH    EXIT                                                             
         ZAP   TBDAYS,=P'0'                                                     
         BAS   RE,BINADD           (R6)=A(ITEM)                                 
         B     EXIT                                                             
*                                                                               
         DROP  R2,R5,R6            KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        PROCESS BIN TABLE                                       *              
******************************************************************              
*                                                                               
REFL     NTR1                      PROCESS TABLE AT CHANGE OF REF #             
SBACL    DS    0H                                                               
         OC    ACCNUM,ACCNUM       POINT TO START OF BIN TABLE                  
         BZ    EXIT                                                             
         MVI   UNAPPSW,C'N'        SET UNAPPLIED SWITCH                         
*                                                                               
         USING TABLED,R6                                                        
*                                                                               
         L     R6,AACCTAB                                                       
         CLI   TBSTAT2,CREDIT      AN UNMATCHED CREDIT?                         
         BNE   REFL20                                                           
         MVI   TBSTAT2,DEBIT       FORCE FIRST ITEM TO ACT LIKE BILL            
         MVI   UNAPPSW,C'Y'                                                     
         MVC   TBDUEDT,TBCKDT                                                   
*                                                                               
REFL20   BAS   RE,PRECALC          PROCESS OTHER COLUMNS IN TABLE               
         BAS   RE,CHECKS           HANDLE CHECK COLUMNS                         
         BAS   RE,OVERPAY          PUT OUT OVERPAYMENT TABLE ENTRIES            
         BAS   RE,CALC             CALCULATE COF                                
         BAS   RE,DUMPBUF                                                       
         BAS   RE,REPORT           PRINT REPORT FOR CURRENT BILL                
*                                                                               
         BAS   RE,PUTBUFF                                                       
         XC    ACCNUM,ACCNUM       CLEAR OUT TABLE                              
         B     EXIT                                                             
*                                                                               
         DROP  R6                  KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        PROCESS A/R BALANCE AND PAST DUE COLUMNS                *              
******************************************************************              
*                                                                               
         USING TABLED,R6                                                        
*                                                                               
PRECALC  NTR1                                                                   
         L     R6,AACCTAB          POINT TO START OF BIN TABLE                  
         BAS   RE,ENDREC           PUT OUT DUMMY END RECORD                     
         SR    R3,R3               NUMBER OF ENTRIES IN TABLE                   
         ICM   R3,15,ACCNUM                                                     
         ZAP   TOTBAMT,TBBAMT      TOTAL AMOUNT OF BILL                         
         ZAP   TOTPRCK,=P'0'       TOTAL CHECKS PREVIOUS TO START               
         ZAP   TOTCK,=P'0'         TOTAL CHECKS                                 
         ZAP   TOTAR,TBBAMT        A/R AMOUNT                                   
         ZAP   TOTOUT,=P'0'        TOTAL OUTSTANDING BALANCE                    
         ZAP   TOTAGE1,=P'0'       AGING COLUMNS                                
         ZAP   TOTAGE2,=P'0'                                                    
         ZAP   TOTAGE3,=P'0'                                                    
         ZAP   TOTAGE4,=P'0'                                                    
         ZAP   TOTCOF,=P'0'        COST OF FINANCING                            
         ZAP   TOTYTD,=P'0'        YEAR TO DATE                                 
         ZAP   TBCOF,=P'0'                                                      
         ZAP   TBYTD,=P'0'                                                      
         L     R5,AACCTAB          POINT TO FIRST ENTRY                         
         MVI   BILLLATE,C'N'       DEFAULT TO BILL NOT BEING LATE               
         CLC   TBDUEDT,ENDDATE                                                  
         BNL   *+8                                                              
         MVI   BILLLATE,C'Y'       SET FLAG TO INDICATE BILL IS LATE            
*                                                                               
PRE100   CLI   TBSTAT2,CREDIT      PROCESS THIS ROW?                            
         BNE   PRE200                                                           
         CLC   TBCKDT(L'TBCKDT),TBDUEDT-TABLED(R5)                              
         BH    *+10                                                             
         AP    TOTPRCK,TBCKAMT     ADD PREVIOUS AMNTS                           
         AP    TOTCK,TBCKAMT       ADD TOTAL AMOUNT OF CHECKS                   
         SP    TOTAR,TBCKAMT                                                    
         ZAP   TBAR,TOTAR                                                       
*                                                                               
PRE200   LA    R6,TBLNQ(,R6)       BUMP TO NEXT TABLE ENTRY                     
         BCT   R3,PRE100                                                        
         ZAP   TBCKAMT-TABLED(8,R5),TOTPRCK          PREVIOUS CHECKS            
*                                                                               
         CLI   BILLLATE,C'Y'                                                    
         BNE   PRE500                                                           
         ZAP   TBOUT-TABLED(8,R5),TOTBAMT            INCOMING OUTSTND           
         SP    TBOUT-TABLED(8,R5),TOTPRCK                                       
         ZAP   TOTOUT,TOTAR                                                     
*                                                                               
*        MAKE RECEIVABLE AMOUNT PAST DUE                                        
*                                                                               
         L     R6,AACCTAB          POINT TO START OF BIN TABLE                  
         SR    R3,R3                                                            
         ICM   R3,15,ACCNUM                                                     
PRE300   CLI   TBSTAT2,CREDIT                                                   
         BNE   PRE400                                                           
         ZAP   TBOUT,TBAR          MOVE AMOUNT INTO PAST DUE COL                
PRE400   LA    R6,TBLNQ(,R6)       BUMP TO NEXT TABLE ENTRY                     
         BCT   R3,PRE300                                                        
*                                                                               
*        CALCULATE CORRECT AGING COLUMN                                         
*                                                                               
         ZAP   NUMDAYS,=P'0'                                                    
         MVC   WKDT,TBDUEDT-TABLED(R5)                                          
         GOTO1 DATCON,DMCB,(1,WKDT),(0,TEMPDT)                                  
         GOTO1 DATCON,DMCB,(1,ENDDATE),(0,TEMPDT2)                              
         GOTO1 PERVERT,DMCB,TEMPDT,TEMPDT2                                      
         SR    R2,R2                                                            
         ICM   R2,3,8(R1)          NUMBER DAYS INCLUSIVE                        
         CVD   R2,DUB                                                           
         ZAP   NUMDAYS,DUB                                                      
         CP    NUMDAYS,AGEDAY1     1-X AGING RANGE                              
         BH    *+14                                                             
         AP    TOTAGE1,TOTOUT                                                   
         B     PRE500                                                           
         CP    NUMDAYS,AGEDAY2     X-Y AGING RANGE                              
         BH    *+14                                                             
         AP    TOTAGE2,TOTOUT                                                   
         B     PRE500                                                           
         CP    NUMDAYS,AGEDAY3     Y-Z AGING RANGE                              
         BH    *+14                                                             
         AP    TOTAGE3,TOTOUT                                                   
         B     PRE500                                                           
         AP    TOTAGE4,TOTOUT      OVER Z AGING RANGE                           
*                                                                               
*        COPY ORIGINAL BILL DUE DATE TO ALL CREDITS                             
*                                                                               
PRE500   L     R6,AACCTAB          POINT TO START OF BIN TABLE                  
         SR    R3,R3                                                            
         ICM   R3,15,ACCNUM                                                     
         MVC   TBDUEDT,TBDUEDT-TABLED(R5)                                       
         LA    R6,TBLNQ(,R6)       BUMP TO NEXT TABLE ENTRY                     
         BCT   R3,*-10                                                          
         B     EXIT                                                             
*                                                                               
         DROP  R6                  KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        PROCESS CHECKS/OVERPAYMENTS                                            
******************************************************************              
*                                                                               
*        CALCULATE AMOUNT OF OVERPAYMENTS                                       
*                                                                               
         USING TABLED,R6                                                        
*                                                                               
CHECKS   NTR1                                                                   
         MVI   OVERSW,C'N'                                                      
         CLI   UNAPPSW,C'Y'        DONT CONSIDER UNAPPLIED AS OVRPYMT           
         BE    CHK250                                                           
         L     R6,AACCTAB          POINT TO START OF BIN TABLE                  
         SR    R3,R3               NUMBER OF ENTRIES IN TABLE                   
         ICM   R3,15,ACCNUM                                                     
         ZAP   WRKAMT1,TBBAMT      GET ORIGINAL BILL AMOUNT                     
*                                                                               
CHK100   CLI   TBSTAT2,CREDIT      PROCESS THIS ROW?                            
         BNE   CHK200                                                           
         ZAP   WRKAMT2,TBCKAMT     CALCULATE AMOUNT OF OVERPAYMENT              
         SP    WRKAMT2,WRKAMT1                                                  
         BNP   CHK150              IF DIFFERENCE>0 THEN OVERPAYMENT             
         ZAP   TBCKOVER,WRKAMT2    THEN SAVE AMOUNT OF OVERPAYMENT              
         ZAP   WRKAMT1,=P'0'                                                    
         MVI   OVERSW,C'Y'                                                      
         B     CHK200                                                           
*                                                                               
CHK150   MP    WRKAMT2,=P'-1'                                                   
         ZAP   WRKAMT1,WRKAMT2                                                  
*                                                                               
CHK200   LA    R6,TBLNQ(,R6)       BUMP TO NEXT TABLE ENTRY                     
         BCT   R3,CHK100                                                        
         EJECT ,                                                                
*                                                                               
*        FIND AMOUNT OF ACTUAL CHECKS                                           
*                                                                               
CHK250   L     R6,AACCTAB          CALCULATE APPLICABLE PORTION                 
         SR    R3,R3               OF CHECK WHICH CAN BE APPLIED                
         ICM   R3,15,ACCNUM        TO THE BILL (NOT AN OVERPAYMENT)             
*                                                                               
CHK300   CLI   TBSTAT2,CREDIT                                                   
         BNE   CHK400                                                           
         ZAP   WRKAMT2,TBCKAMT                                                  
         SP    WRKAMT2,TBCKOVER                                                 
         ZAP   TBCKAPPL,WRKAMT2                                                 
*                                                                               
CHK400   LA    R6,TBLNQ(,R6)       BUMP TO NEXT TABLE ENTRY                     
         BCT   R3,CHK300                                                        
         B     EXIT                                                             
*                                                                               
         DROP  R6                  KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        CALCULATE COST OF FINANCING FOR ROW                     *              
******************************************************************              
*                                                                               
         USING TABLED,R6                                                        
*                                                                               
CALC     NTR1                                                                   
         BAS   RE,CLRDET           CLEAR OUT DETAIL TABLE                       
         L     R5,AACCTAB          POINT TO FIRST ENTRY                         
         L     R6,AACCTAB          POINT TO START OF BIN TABLE                  
         SR    R3,R3               NUMBER OF ENTRIES IN TABLE                   
         ICM   R3,15,ACCNUM                                                     
         ZAP   TBCOF,=P'0'                                                      
         ZAP   TBYTD,=P'0'                                                      
         L     R2,ADETTBLE         FIRST AVAILBLE ENTRY POSTION                 
*                                                                               
         USING DETAILD,R2                                                       
*                                                                               
CALC10   CLI   TBSTAT2,CREDIT      PROCESS THIS ROW?                            
         BNE   CALC90                                                           
         STCM  R2,15,TBDET         SAVE ADDRESS OF FIRST ITEM ENTRY             
         MVI   TBDETNUM,0          ZERO ENTRIES IN TABLE                        
         MVI   FIRSTFLG,C'Y'       FIRST TIME THROUGH FOR THIS LOOP             
         ZAP   DETRATE,=P'0'                                                    
         ZAP   DETCOF,=P'0'                                                     
         ZAP   DETDAYS,=P'0'                                                    
*                                                                               
         ZAP   NUMDAYS,=P'0'                                                    
         ZAP   WRKAMT2,=P'0'                                                    
         ZAP   COFBEF,=P'0'                                                     
         ZAP   COFAFT,=P'0'                                                     
         L     R5,AACCTAB          POINT TO FIRST ENTRY                         
         CLC   TBCKDT(L'TBCKDT),TBDUEDT-TABLED(R5)                              
         BNH   CALC90                                                           
         MVC   WKDT,TBDUEDT                                                     
         CLC   WKDT,FINYEAR        IF DUE FROM PREVIOUS YEARS THEN              
         BNL   CALC13              ONLY CALCULATE COF ON THIS YEAR              
         MVC   WKDT,FINYEAR                                                     
         B     CALC15                                                           
*                                                                               
CALC13   GOTO1 DATCON,DMCB,(1,WKDT),(0,TEMPDT)                                  
         GOTO1 ADDAY,DMCB,TEMPDT,TEMPDT2,F'1'                                   
         GOTO1 DATCON,DMCB,(0,TEMPDT2),(1,WKDT)                                 
*                                                                               
CALC15   MVC   WKSTDT,WKDT                                                      
         MVC   WKENDDT,TBCKDT                                                   
*                                                                               
CALC20   BAS   RE,GETRT            GET RATE FOR CURRENT DAY                     
         ZAP   WRKAMT1,TBCKAPPL    CALCULATE COF ON CHECK AMOUNT                
*                                                                               
         CLI   TBOVERST,X'FF'                                                   
         BNE   CALC21                                                           
         ZAP   WRKAMT1,TBCKOVER                                                 
         MP    WRKAMT1,=P'-1'                                                   
         CLI   PROGPROF+1,C'N'     SHOW UNAPPLIED/OVERPAYMENTS                  
         BNE   CALC22                                                           
         ZAP   WRKAMT1,=P'0'                                                    
         B     CALC22                                                           
*                                                                               
CALC21   DS    0H                                                               
         CLI   OVERSW,C'Y'         DONT CALCULATE COF IF AN OVERPAYMENT         
         BE    CALC22                                                           
         CLI   TBSTAT1,DUMMY                                                    
         BNE   CALC22              UNLESS STILL OPEN ITEM                       
         ZAP   WRKAMT1,TOTAR       THEN USE AMOUNT RECEIVABLE                   
         CLI   PROGPROF+1,C'N'     SHOW UNAPPLIED/OVERPAYMENTS?                 
         BNE   CALC22                                                           
         CP    WRKAMT1,=P'0'                                                    
         BNL   CALC22                                                           
         ZAP   WRKAMT1,=P'0'                                                    
*                                                                               
CALC22   CLI   PROGPROF,C'S'       SIMPLE INTEREST                              
         BE    *+10                                                             
         AP    WRKAMT1,WRKAMT2     COMPOUNDED INTEREST                          
*                                                                               
         SRP   WRKAMT1,4,5                                                      
         SRP   CURRATE,4,5                                                      
         ZAP   PACK16,WRKAMT1                                                   
         MP    PACK16,CURRATE                                                   
         SRP   PACK16,64-17,5                                                   
         ZAP   WRKAMT1,PACK16                                                   
         ZAP   WRKAMT2,WRKAMT1     SAVE AMOUNT OF COST OF FINANCING             
         CLC   WKDT,STDATE                                                      
         BNL   CALC23                                                           
         CLC   TBCKDT,FINYEAR      IF CHECK IS FROM LAST FIN YEAR               
         BL    CALC28              THEN LEAVE OUT OF YTD CALC                   
         AP    COFBEF,WRKAMT1      ADD TO 'BEFORE START DATE' ACCUM             
         B     CALC28                                                           
*                                                                               
CALC23   DS    0H                  IF SAME RATE THEN ADD TOGETHER               
         CP    TRUERATE,DETRATE                                                 
         BNE   CALC24                                                           
         AP    DETDAYS,=P'1'                                                    
         AP    DETCOF,WRKAMT1                                                   
         B     CALC25                                                           
*                                                                               
CALC24   CLI   FIRSTFLG,C'Y'       DONT BUMP TABLE POS IF FIRST TIME            
         BE    *+8                                                              
         LA    R2,DETLNQ(,R2)      BUMP TO NEXT TABLE ENTRY POSITION            
         MVI   FIRSTFLG,C'N'                                                    
         ZIC   R1,TBDETNUM         NUMBER OF DETAIL LINES                       
         LA    R1,1(,R1)                                                        
         STC   R1,TBDETNUM         STORE DETAIL IN SEPARATE TABLE               
         ZAP   DETDAYS,=P'1'                                                    
         ZAP   DETRATE,TRUERATE                                                 
         ZAP   DETCOF,WRKAMT1                                                   
*                                                                               
CALC25   AP    COFAFT,WRKAMT1      ADD TO 'AFTER START DATE' ACCUM              
         AP    NUMDAYS,=P'1'                                                    
*                                                                               
CALC28   GOTO1 DATCON,DMCB,(1,WKDT),(0,TEMPDT)                                  
         GOTO1 ADDAY,DMCB,TEMPDT,TEMPDT2,F'1'                                   
         GOTO1 DATCON,DMCB,(0,TEMPDT2),(1,WKDT)                                 
         CLC   WKDT,WKENDDT                                                     
         BNH   CALC20                                                           
         LA    R2,DETLNQ(,R2)      NEXT AVAILABLE SPOT IN DETAIL TBLE           
*                                                                               
CALC30   DS    0H                                                               
         ZAP   TBCOF,COFAFT        SAVE AMOUNT OF COF FOR SPECIFIC ITEM         
         ZAP   TBDAYS,NUMDAYS      SAVE NUMBER OF DAYS LATE                     
         AP    TBYTD-TABLED(8,R5),COFBEF                                        
         CLC   TBCKDT,STDATE                                                    
         BL    *+10                                                             
         AP    TOTCOF,COFAFT                                                    
*                                                                               
CALC90   LA    R6,TBLNQ(,R6)       BUMP TO NEXT TABLE ENTRY                     
         BCT   R3,CALC10                                                        
*                                                                               
         L     R6,AACCTAB          CALCULATE YEAR TO DATE AMOUNTS               
         SR    R3,R3                                                            
         ICM   R3,15,ACCNUM                                                     
         ZAP   TOTYTD,TBYTD-TABLED(8,R5)  START W/INCOMING AMNT                 
*                                                                               
CALC220  CLI   TBSTAT2,CREDIT                                                   
         BNE   CALC300                                                          
         CLC   TBCKDT,FINYEAR                                                   
         BL    CALC300                                                          
         AP    TOTYTD,TBCOF                                                     
         ZAP   TBYTD,TOTYTD                                                     
*                                                                               
CALC300  LA    R6,TBLNQ(,R6)                                                    
         BCT   R3,CALC220                                                       
         B     EXIT                                                             
*                                                                               
         DROP  R2,R6               KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        PUT OUT A DUMMY END RECORD                              *              
******************************************************************              
*                                                                               
         USING TABLED,R6                                                        
*                                                                               
ENDREC   NTR1                                                                   
         L     R5,AACCTAB          COPY INFO IN FIRST RECORD                    
         L     R6,AWKAREA                                                       
         MVC   0(TBLNQ,R6),0(R5)                                                
         MVI   TBSTAT1,DUMMY       FORCE TO BE LAST RECORD                      
         MVI   TBSTAT2,CREDIT      MARK FOR COF CALCULATION                     
         MVC   TBCKDT,ENDDATE                                                   
*                                                                               
ENDREC3  MVI   TBTYPE,DUMMY                                                     
         MVC   TBCON,XSPACES                                                    
         MVC   TBCKNUM,XSPACES                                                  
         XC    TBDUEDT,TBDUEDT                                                  
         ZAP   TBBAMT,=P'0'                                                     
         ZAP   TBAR,=P'0'                                                       
         ZAP   TBOUT,=P'0'                                                      
         ZAP   TBCOF,=P'0'                                                      
         ZAP   TBYTD,=P'0'                                                      
         ZAP   TBDAYS,=P'0'                                                     
         XC    TBDETNUM,TBDETNUM                                                
         BAS   RE,BINADD           R6=A(ITEM)                                   
         B     EXIT                                                             
*                                                                               
         DROP  R6                  KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        PUT OUT AN OVERPAYMENT RECORD                           *              
******************************************************************              
*                                                                               
*        PUT OUT AN OVERPAYMENT RECORD FOR EACH OVERPAYMENT MADE                
*                                                                               
         USING TABLED,R6                                                        
*                                                                               
OVERPAY  NTR1                                                                   
         CLI   OVERSW,C'Y'         LEAVE IF NO OVERPAYMENTS                     
         BNE   EXIT                                                             
         L     R5,AACCTAB          CALCULATE APPLICABLE PORTION                 
         SR    R3,R3               OF CHECK WHICH CAN BE APPLIED                
         ICM   R3,15,ACCNUM        TO THE BILL (NOT AN OVERPAYMENT)             
*                                                                               
OVR100   CP    TBCKOVER-TABLED(8,R5),=P'0'                                      
         BE    OVR400                                                           
         CLI   TBSTAT1-TABLED(R5),DUMMY                                         
         BE    OVR400                                                           
         CLI   TBSTAT2-TABLED(R5),CREDIT                                        
         BNE   OVR400                                                           
         L     R6,AWKAREA                                                       
         MVC   0(TBLNQ,R6),0(R5)                                                
         MVI   TBSTAT1,OVRPY1                                                   
         MVI   TBSTAT2,CREDIT                                                   
         MVC   TBDUEDT,TBCKDT                                                   
         MVC   TBCKDT,ENDDATE                                                   
         MVI   TBTYPE,OVRPAY                                                    
         MVC   TBCON,XSPACES                                                    
         MVC   TBCKNUM,XSPACES                                                  
         ZAP   TBCKAMT,=P'0'                                                    
         ZAP   TBCKAPPL,=P'0'                                                   
         ZAP   TBBAMT,=P'0'                                                     
         ZAP   TBAR,=P'0'                                                       
         ZAP   TBOUT,=P'0'                                                      
         ZAP   TBCOF,=P'0'                                                      
         ZAP   TBYTD,=P'0'                                                      
         ZAP   TBDAYS,=P'0'                                                     
         XC    TBDETNUM,TBDETNUM                                                
         MVI   TBOVERST,X'FF'      FLAG THAT THIS IS AN OVERPAYMENT             
         BAS   RE,BINADD           R6=A(ITEM)                                   
*                                                                               
OVR400   LA    R5,TBLNQ(,R5)       BUMP TO NEXT TABLE ENTRY                     
         BCT   R3,OVR100                                                        
*                                                                               
OVR500   L     R6,AACCTAB          ASSIGN NEW VALUE TO ALLOW FOR RESORT         
         SR    R3,R3               TABLE                                        
         ICM   R3,15,ACCNUM                                                     
*                                                                               
OVR600   CLI   TBSTAT1,OVRPY1      CHANGE HEX VALUE FROM 'FF' TO X'DD'          
         BNE   *+8                 SO RECORD IS SORTED BEFORE DUMMY             
         MVI   TBSTAT1,OVRPY2      RECORD WHICH HAS VALUE OF X'EE'              
         LA    R6,TBLNQ(,R6)                                                    
         BCT   R3,OVR600                                                        
         ICM   R3,15,ACCNUM                                                     
         GOTO1 XSORT,DMCB,(0,AACCTAB),(R3),TBLNQ,TBKEYL,0                       
         B     EXIT                                                             
*                                                                               
         DROP  R6                  KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        GET RATE FOR INTEREST CALCULATION                       *              
******************************************************************              
*                                                                               
         USING RATED,R6                                                         
*                                                                               
GETRT    NTR1                                                                   
         L     R6,ARATETB                                                       
         ZAP   CURRATE,RTRATE      DEFAULT RATE IS EARLIEST RATE                
         ZAP   TRUERATE,RTTRUE                                                  
*                                                                               
GETRT1   CLI   RTDATE,X'FF'        STOP IF END OF TABLE REACHED                 
         BE    EXIT                                                             
         CLC   WKDT,RTDATE         ELSE FIND APPLICABLE RATE                    
         BL    EXIT                                                             
         ZAP   CURRATE,RTRATE      SAVE RATE IF MATCH IN DATE RANGE             
         ZAP   TRUERATE,RTTRUE                                                  
         LA    R6,RTLNQ(,R6)       BUMP TO NEXT ENTRY                           
         B     GETRT1                                                           
*                                                                               
         DROP  R6                  KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        PRINT DETAIL REPORT                                     *              
******************************************************************              
*                                                                               
         USING TABLED,R6                                                        
         USING PLINED,R4                                                        
*                                                                               
REPORT   NTR1                                                                   
         L     R5,AACCTAB          POINT TO FIRST ITEM                          
         L     R6,AACCTAB          USE R6 TO BUMP THROUGH TABLE                 
         LA    R4,XP                                                            
         MVC   XP,XSPACES                                                       
         SR    R3,R3               GET NUMBER OF TABLE ENTRIES                  
         ICM   R3,15,ACCNUM                                                     
*                                                                               
*        HANDLE DEBITS                                                          
*                                                                               
REP10    CLI   TBTYPE,BILL         BILL?                                        
         BNE   REP30                                                            
         CP    TOTCOF,=P'0'                                                     
         BE    EXIT                PRINT ONLY ITEMS THAT HAVE COF $             
*                                                                               
         BAS   RE,HEADUP                                                        
         BAS   RE,DUMPBUF          PRINT OUT HEADER BUFFER                      
         ZIC   R1,LINE                                                          
         ZIC   R0,MAXLINES                                                      
         LA    R1,6(,R1)                                                        
         CR    R1,R0                                                            
         BL    *+8                                                              
         BAS   RE,HEADUP                                                        
         MVC   PCON+2(L'PCON-2),TBCON+3                                         
         MVC   PDATA(L'BILLLN),BILLLN                                           
         MVC   PNUM(6),TBBNUM                                                   
         GOTO1 DATCON,DMCB,(1,TBDUEDT),(5,PDATE)                                
         GOTO1 DATCON,DMCB,(1,TBTRNDT),(5,PTRNDT)                               
         EDIT  (P8,TBBAMT),(12,PBILL),2,FLOAT=-                                 
*                                                                               
         CP    TBOUT,=P'0'         OUTSTANDING BALANCE BROUGHT FORWARD          
         BE    REP12                                                            
         MVI   PCHAR1,C'*'                                                      
         EDIT  (P8,TBOUT),(12,POUT),2,FLOAT=-                                   
         LA    R1,POUT                                                          
         CLI   1(R1),C' '                                                       
         BNE   *+12                                                             
         LA    R1,1(,R1)                                                        
         B     *-12                                                             
         MVI   0(R1),C'*'          FLOAT IN LEFT *                              
*                                                                               
REP12    CP    TBCKAMT,=P'0'       CHECK AMOUNT BROUGHT FORWARD                 
         BE    REP14                                                            
         MVI   PCHAR2,C'*'                                                      
         EDIT  (P8,TBCKAMT),(12,PCKAMT),2,FLOAT=-                               
         LA    R1,PCKAMT                                                        
         CLI   1(R1),C' '                                                       
         BNE   *+12                                                             
         LA    R1,1(,R1)                                                        
         B     *-12                                                             
         MVI   0(R1),C'*'          FLOAT IN LEFT *                              
*                                                                               
REP14    CP    TBYTD,=P'0'         YEAR TO DATE BROUGHT FORWARD                 
         BE    REP85                                                            
         MVI   PCHAR3,C'*'                                                      
         EDIT  (P8,TBYTD),(12,PYTD),2,FLOAT=-                                   
         LA    R1,PYTD                                                          
         CLI   1(R1),C' '                                                       
         BNE   *+12                                                             
         LA    R1,1(,R1)                                                        
         B     *-12                                                             
         MVI   0(R1),C'*'          FLOAT IN LEFT *                              
         B     REP85                                                            
*                                                                               
*        HANDLE CREDITS                                                         
*                                                                               
REP30    CLC   TBCKDT,STDATE       ONLY PRINT DETAIL FOR CR'S                   
         BL    REP90               WITHIN THE CURRENT PERIOD                    
         CLI   TBSTAT2,DEBIT                                                    
         BE    *+14                                                             
         CLC   TBCKDT,TBDUEDT-TABLED(R5)                                        
         BNH   REP90               DONT PRINT CREDITS BEFORE DUEDATE            
         CLI   TBTYPE,CHECK        CHECK?                                       
         BNE   REP40                                                            
         MVC   PDATA(L'CHCKLN),CHCKLN                                           
         MVC   PNUM(6),TBCKNUM                                                  
         CLI   TBSTAT2,DEBIT       IS IT AN UNAPPLIED?                          
         BNE   *+10                YES, THEN PRINT BILL SOURCE                  
         MVC   PCON+2(L'PCON-2),TBCON+3                                         
         GOTO1 DATCON,DMCB,(1,TBCKDT),(5,PDATE)                                 
         GOTO1 DATCON,DMCB,(1,TBTRNDT),(5,PTRNDT)                               
         EDIT  (P8,TBCKAMT),(12,PCKAMT),2,FLOAT=-                               
         ZAP   DUB,TBOUT                                                        
         AP    DUB,TBCKAMT                                                      
         EDIT  (P8,DUB),(12,POUT),2,FLOAT=-                                     
         B     REP80                                                            
*                                                                               
REP40    CLI   TBTYPE,DUMMY        TRAILER RECORD?                              
         BNE   REP50                                                            
         CLI   TBSTAT2,DEBIT                                                    
         BE    *+14                                                             
         CP    TBOUT,=P'0'                                                      
         BE    REP90                                                            
         CP    TBCOF,=P'0'                                                      
         BE    REP90                                                            
         MVC   PCON,XSPACES                                                     
         MVC   PDATA,XSPACES                                                    
         MVC   PDATA(10),=C'STILL OPEN'                                         
         GOTO1 DATCON,DMCB,(1,ENDDATE),(5,PDATE)                                
         ZAP   DUB,TBOUT                                                        
         AP    DUB,TBCKAMT                                                      
         EDIT  (P8,DUB),(12,POUT),2,FLOAT=-                                     
         B     REP80                                                            
*                                                                               
REP50    DS    0H                                                               
         CLI   TBTYPE,OFFSET      OFFSET?                                       
         BNE   REP60                                                            
         MVC   PCON,XSPACES                                                     
         MVC   PDATA(L'OFFSETLN),OFFSETLN                                       
         GOTO1 DATCON,DMCB,(1,TBCKDT),(5,PDATE)                                 
         EDIT  (P8,TBCKAMT),(12,PCKAMT),2,FLOAT=-                               
         ZAP   DUB,TBOUT                                                        
         AP    DUB,TBCKAMT                                                      
         EDIT  (P8,DUB),(12,POUT),2,FLOAT=-                                     
         B     REP80                                                            
*                                                                               
REP60    CLI   TBTYPE,WRITEOFF     WRITEOFF?                                    
         BNE   REP70                                                            
         MVC   PCON,XSPACES                                                     
         MVC   PDATA(L'WOLN),WOLN                                               
         MVC   PNUM,TBBNUM                                                      
         GOTO1 DATCON,DMCB,(1,TBCKDT),(5,PDATE)                                 
         EDIT  (P8,TBCKAMT),(12,PCKAMT),2,FLOAT=-                               
         ZAP   DUB,TBOUT                                                        
         AP    DUB,TBCKAMT                                                      
         EDIT  (P8,DUB),(12,POUT),2,FLOAT=-                                     
         B     REP80                                                            
*                                                                               
REP70    CLI   TBTYPE,UNAPPLD      UNKNOWN CREDIT                               
         BNE   REP75                                                            
         MVC   PDATA(L'CHCKLN),CHCKLN                                           
         MVC   PNUM(6),SPACES                                                   
         GOTO1 DATCON,DMCB,(1,TBCKDT),(5,PDATE)                                 
         GOTO1 DATCON,DMCB,(1,TBTRNDT),(5,PTRNDT)                               
         EDIT  (P8,TBCKAMT),(12,PCKAMT),2,FLOAT=-                               
         ZAP   DUB,TBOUT                                                        
         AP    DUB,TBCKAMT                                                      
         EDIT  (P8,DUB),(12,POUT),2,FLOAT=-                                     
         B     REP80                                                            
*                                                                               
REP75    CLI   TBTYPE,OVRPAY       OVER PAYMENT                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PDATA(L'OVERLN),OVERLN                                           
         GOTO1 DATCON,DMCB,(1,TBDUEDT),(5,PDATE)                                
         MVI   PCHAR2,C'*'                                                      
         EDIT  (P8,TBCKOVER),(12,PCKAMT),2,FLOAT=-                              
         LA    R1,PCKAMT                                                        
         CLI   1(R1),C' '                                                       
         BNE   *+12                                                             
         LA    R1,1(,R1)                                                        
         B     *-12                                                             
         MVI   0(R1),C'*'          FLOAT IN LEFT *                              
         B     REP80                                                            
*                                                                               
         USING DETAILD,R2                                                       
*                                                                               
REP80    DS    0H                                                               
         ICM   R2,15,TBDET                                                      
         ZIC   R0,TBDETNUM                                                      
         OR    R0,R0                                                            
         BZ    REP85               NO DETAILS IN TABLE                          
*                                                                               
REP82    MVC   WRK2,XSPACES                                                     
         EDIT  (P8,DETDAYS),(3,WRK2)                                            
         LA    R1,3                                                             
         CP    DETDAYS,=P'1'       IF ONLY 1 DAY THEN MOVE ONLY                 
         BNE   *+6                 FIRST 3 CHARS                                
         BCTR  R1,0                                                             
         EXMVC R1,WRK2+5,=C'DAYS'                                               
         MVI   WRK2+10,C'@'                                                     
         EDIT  (P8,DETRATE),(12,WRK2+12),4,TRAIL=C'%'                           
         GOTO1 SQUASHER,DMCB,WRK2+10,30                                         
         MVC   PDETAIL,WRK2                                                     
         EDIT  (P8,DETCOF),(12,PCOF),2,FLOAT=-                                  
         BAS   RE,OUTREC                                                        
         LA    R2,DETLNQ(R2)       BUMP TO NEXT DETAIL ENTRY                    
         BCT   R0,REP82                                                         
*                                                                               
         CLI   TBDETNUM,1          NO SUBTOTAL IF ONLY 1 ITEM                   
         BE    REP90                                                            
         MVC   WRK2,XSPACES                                                     
         MVC   WRK2(3),=C'***'                                                  
         EDIT  (P8,TBDAYS),(12,WRK2+5)                                          
         MVC   WRK2+20(8),=C'DAYS ***'                                          
         GOTO1 SQUASHER,DMCB,WRK2,L'WRK2                                        
         MVC   PDETAIL+3(L'PDETAIL-3),WRK2                                      
         EDIT  (P8,TBCOF),(12,PCOF),2,FLOAT=-                                   
         EDIT  (P8,TBYTD),(12,PYTD),2,FLOAT=-                                   
*                                                                               
REP85    DS    0H                                                               
         BAS   RE,OUTREC                                                        
*                                                                               
REP90    LA    R6,TBLNQ(,R6)       BUMP TO NEXT TABLE ENTRY                     
         BCT   R3,REP10                                                         
         BAS   RE,PUTTOTS                                                       
         B     EXIT                                                             
*                                                                               
         DROP  R2,R4,R6            KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        ADD AMOUNT INTO TOTALS                                  *              
******************************************************************              
*                                                                               
         USING ACCUMSD,R3                                                       
*                                                                               
PUTTOTS  NTR1                                                                   
         LA    R3,ACCUMTMP                                                      
         OC    ACCNUM,ACCNUM                                                    
         BZ    EXIT                                                             
         ZAP   ACCBAMT,TOTBAMT     ORIGINAL BILL AMOUNT                         
         ZAP   ACCAR,TOTAR         A/R AMOUNT                                   
         ZAP   ACCOUT,TOTOUT                                                    
         ZAP   DUB,ACCBAMT                                                      
         SP    DUB,ACCAR                                                        
         ZAP   ACCCKAMT,DUB        TOTAL PAYMENTS                               
         ZAP   ACCCOF,TOTCOF       COST OF FINANCE                              
         ZAP   ACCYTD,TOTYTD       YEAR TO DATE                                 
         ZAP   ACCAGE1,TOTAGE1                                                  
         ZAP   ACCAGE2,TOTAGE2                                                  
         ZAP   ACCAGE3,TOTAGE3                                                  
         ZAP   ACCAGE4,TOTAGE4                                                  
*                                                                               
         LA    R1,ACCUMS           CASCADE TOTALS TO ALL LEVELS                 
         LA    R0,LEVCOUNT                                                      
PUTTOT10 AP    ACCBAMT-ACCUMSD(8,R1),ACCBAMT                                    
         AP    ACCAR-ACCUMSD(8,R1),ACCAR                                        
         AP    ACCOUT-ACCUMSD(8,R1),ACCOUT                                      
         AP    ACCAGE1-ACCUMSD(8,R1),ACCAGE1                                    
         AP    ACCAGE2-ACCUMSD(8,R1),ACCAGE2                                    
         AP    ACCAGE3-ACCUMSD(8,R1),ACCAGE3                                    
         AP    ACCAGE4-ACCUMSD(8,R1),ACCAGE4                                    
         AP    ACCCKAMT-ACCUMSD(8,R1),ACCCKAMT                                  
         AP    ACCCOF-ACCUMSD(8,R1),ACCCOF                                      
         AP    ACCYTD-ACCUMSD(8,R1),ACCYTD                                      
         LA    R1,ACCUMLNQ(,R1)    BUMP TO NEXT LEVEL                           
         BCT   R0,PUTTOT10                                                      
*                                                                               
         LA    R3,ACCUMTMP         PRINT MINI TOTALS                            
         MVI   PRNTLEV,0                                                        
         BAS   RE,FORMAT                                                        
         B     EXIT                                                             
*                                                                               
         DROP  R3                  KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        LAST FOR LEVELS                                         *              
******************************************************************              
*                                                                               
LEVDL    DS    0H                  PRINT LOWEST LEVEL TOTALS                    
         LA    R3,ACCUMS                                                        
         ZIC   R2,LOWLEV                                                        
         MVC   PRNTLEV,LOWLEV                                                   
         BCTR  R2,0                                                             
         MH    R2,=Y(L'ACCUMS)                                                  
         AR    R3,R2                                                            
         B     LEVX                                                             
*                                                                               
LEVCL    DS    0H                  PRINT LEVEL C TOTALS                         
         LA    R3,ACCUM3                                                        
         MVI   PRNTLEV,3                                                        
         B     LEVX                                                             
*                                                                               
LEVBL    DS    0H                  PRINT LEVEL B TOTALS                         
         LA    R3,ACCUM2                                                        
         MVI   PRNTLEV,2                                                        
         B     LEVX                                                             
*                                                                               
LEVAL    DS    0H                  PRINT LEVEL A TOTALS                         
         LA    R3,ACCUM1                                                        
         MVI   PRNTLEV,1                                                        
*                                                                               
LEVX     DS    0H                                                               
         BAS   RE,FORMAT                                                        
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        FORMAT ACCUMULATORS - R3 POINTING AT ACCUM ROW          *              
******************************************************************              
*                                                                               
         USING ACCUMSD,R3                                                       
         USING PLINED,R4                                                        
*                                                                               
FORMAT   NTR1                                                                   
         LA    R4,XP                                                            
         CP    ACCCOF,=P'0'        NO TOTALS FOR ITEMS WITHOUT COF $            
         BNE   FORMAT5                                                          
         MVI   PRNTLEV,X'FF'       OR IF NO TOTAL FLAG IS SET                   
         B     FORMAT8                                                          
*                                                                               
FORMAT5  EDIT  (P8,ACCOUT),(12,POUT),2,,FLOAT=-                                 
         EDIT  (P8,ACCCOF),(12,PCOF),2,FLOAT=-                                  
         EDIT  (P8,ACCYTD),(12,PYTD),2,FLOAT=-                                  
         CLI   PRNTLEV,0                                                        
         BNE   FORMAT8                                                          
         MVI   PCHAR1,C'*'                                                      
         LA    R1,POUT                                                          
         CLI   1(R1),C' '                                                       
         BNE   *+12                                                             
         LA    R1,1(,R1)                                                        
         B     *-12                                                             
         MVI   0(R1),C'*'          FLOAT IN LEFT *                              
*                                                                               
         MVC   PNAME+24(13),=C'*** TOTAL ***'                                   
         BAS   RE,OUTREC                                                        
         BAS   RE,BXMID            MIDLINE AFTER BILL INFO                      
         B     EXIT                DONT CLEAR TMP ACCUM                         
*                                                                               
FORMAT8  LA    R0,ACCNUMQ          CLEAR LEVEL OF ACCUMS                        
         ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(,R3)                                                        
         BCT   R0,*-10                                                          
         CLI   PRNTLEV,X'FF'       IF 'NO TOTAL' FLAG SET THEN EXIT             
         BE    EXIT                AFTER CLEARING THE ACCUMS                    
*                                                                               
FORMAT10 MVC   WRK2,XSPACES        BUILD TOTAL LINE - LEVEL/ACCT/NAME           
         MVC   WRK2(9),=C'TOTAL FOR'                                            
*                                                                               
         CLI   PRNTLEV,5           PRINT REPORT TOTAL?                          
         BNE   FORMAT30                                                         
         MVC   WRK2+10(6),=C'REPORT'                                            
         B     FORMAT60                                                         
*                                                                               
FORMAT30 LA    R1,LEVELNM                                                       
         ZIC   R2,PRNTLEV                                                       
         BCTR  R2,0                                                             
         MH    R2,=Y(L'LEVELNM)    GET CORRECT LEVEL NAME                       
         AR    R1,R2                                                            
         MVC   WRK2+10(L'LEVELNM),0(R1)                                         
*                                                                               
         LA    R1,LVACCT                                                        
         ZIC   R2,PRNTLEV                                                       
         BCTR  R2,0                                                             
         MH    R2,=Y(L'LVACCT)     GET CORRECT ACCOUNT CODE                     
         AR    R1,R2                                                            
         MVC   WRK2+26(L'LVACCT),0(R1)                                          
*                                                                               
         LA    R1,LVNAME                                                        
         ZIC   R2,PRNTLEV                                                       
         BCTR  R2,0                                                             
         MH    R2,=Y(L'LVNAME)     GET CORRECT ACCOUNT NAME                     
         AR    R1,R2                                                            
         MVC   WRK2+40(L'LVNAME),0(R1)                                          
         GOTO1 SQUASHER,DMCB,WRK2,L'WRK2                                        
*                                                                               
FORMAT60 MVC   PNAME,WRK2                                                       
         BAS   RE,OUTREC                                                        
         BAS   RE,BXMID            MIDLINE AFTER BILL INFO                      
         B     EXIT                                                             
*                                                                               
         DROP  R3,R4               KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        PRINT A LINE                                            *              
******************************************************************              
*                                                                               
PRNTXP   NTR1                                                                   
         CLI   QOPT3,C'Y'          NO DETAIL REPORT                             
         BNE   EXIT                                                             
         MVC   XHEAD2+16(L'CMPABBR),CMPABBR                                     
         MVC   XHEAD2+30(L'CMPNAME),CMPNAME                                     
         MVC   XHEAD3(L'HED1),HED1                                              
         MVC   XHEAD4(L'HED2),HED2                                              
         MVC   XHEAD5(L'HED3),HED3                                              
         MVC   XHEAD6(L'HED4),HED4                                              
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT ,                                                                
**********************************************************************          
* PRINT/DOWNLOAD INTERFACE                                           *          
**********************************************************************          
         SPACE 1                                                                
OUTREC   NTR1                                                                   
         CLI   QOPT7,C'Y'          WAS DOWNLOADING SELECTED?                    
         BNE   OUTR10                                                           
         CLI   QOPT3,C'Y'                                                       
         BNE   OUTRX                                                            
*                                                                               
         MVI   RCSUBPRG,9          INVALID REPORT NUM FOR DOWNLOAD              
         MVC   MYSUBPRG,RCSUBPRG                                                
*                                                                               
         CLC   XP,XSPACES          DON'T DOWNLOAD BLANK LINE                    
         BE    OUTRX                                                            
*                                                                               
         MVC   SVPRNT,XP           SAVE DATA TO BE DOWNLOADED                   
         MVC   XP,XSPACES          CLEAR 1ST PRINT LINE                         
         MVC   XPSECOND,XSPACES    CLEAR 2ND PRINT LINE                         
         MVC   XPTHIRD,XSPACES     CLEAR 3RD PRINT LINE                         
         MVC   XPFOURTH,XSPACES    CLEAR 4TH PRINT LINE                         
*                                                                               
         GOTO1 ADWNRTE,DMCB,(RC)   IF SO POINT TO DOWNLOAD                      
         B     OUTRX               SKIP HEAD UP ROUTINE                         
*                                                                               
OUTR10   BAS   RE,PRNTXP           PRINT AS NORMAL                              
*                                                                               
OUTRX    B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*        PUT OUT A BOX MIDLINE                                   *              
******************************************************************              
*                                                                               
         USING BOXD,R4                                                          
*                                                                               
BXMID    NTR1                                                                   
         CLI   QOPT7,C'Y'          ARE WE DOWNLOADING                           
         BE    BXMIDX              IF YES, NO BOX                               
         L     R4,ADBOX                                                         
         MVC   BOXROWS,XSPACES                                                  
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'                                                       
         MVI   BOXINIT,0                                                        
         BAS   RE,OUTREC                                                        
BXMIDX   B     EXIT                                                             
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        PRINT OUT HEADER BUFFER                                 *              
******************************************************************              
*                                                                               
         USING TABLED,R6                                                        
*                                                                               
DUMPBUF  NTR1                                                                   
         L     R6,AACCTAB                                                       
         BAS   RE,HEADUP           MIGHT HAVE TO HEADUP IF NEW HIGH LEV         
*                                                                               
         LA    R4,PRNTBLK          PRINT OUT HEADER BUFFER                      
         LA    R0,6                                                             
DUMP1    CLC   0(L'XP,R4),XSPACES                                               
         BE    DUMP2               BUT DONT PRINT BLANK LINES                   
         MVC   XP,0(R4)                                                         
         BAS   RE,OUTREC                                                        
*                                                                               
DUMP2    LA    R4,L'XP(,R4)                                                     
         BCT   R0,DUMP1                                                         
         LA    R4,PRNTBLK          PRINT OUT HEADER BUFFER                      
         BAS   RE,CLRBLK           CLEAR OUT PRINT BUFFER                       
         B     EXIT                                                             
*                                                                               
         DROP  R6                  KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        CLEAR REMAINING SECTION OF HEADER BUFFER                *              
******************************************************************              
*                                                                               
*        R4 POINTS TO SOME OFFSET INTO THE PRINTBLK BUFFER.  THIS RTN           
*        THEN CLEARS THE FOLLOWING BYTES TO SPACES.                             
*                                                                               
CLRBLK   NTR1                      R4 POINTS TO START OF WHERE TO CLEAR         
         LA    R0,PRNTBLK                                                       
         LR    R1,R4                                                            
         SR    R1,R0               GET NUMBER PAST START OF BLOCK               
         LA    R0,PRNTBLKQ                                                      
         SR    R0,R1               NUMBER OF BYTES LEFT TO CLEAR                
*                                                                               
         MVI   0(R4),C' '                                                       
         LA    R4,1(,R4)                                                        
         BCT   R0,*-8                                                           
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        FIND NAME ELEMENT                                       *              
******************************************************************              
*                                                                               
*        WITH R5 POINTING TO FIRST ELEMENT, THIS ROUTINE                        
*        WILL LOOP THROUGH THE ELEMENTS, FIND THE NAME ELEMENT                  
*        AND RETURN THE NAME IN 'WORK'.                                         
*                                                                               
         USING ACNAMED,R5                                                       
*                                                                               
GETNAME  NTR1                                                                   
         MVC   WORK,XSPACES                                                     
         CLI   0(R5),X'20'                                                      
         BNE   EXIT                                                             
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'            LENGTH=ACNMLEN-2 (-1 FOR EX)                 
         BM    EXIT                                                             
         EXMVC R1,WORK,ACNMNAME                                                 
         B     EXIT                                                             
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        ADD ITEM TO TABLE                                       *              
******************************************************************              
*                                                                               
*        R6=A(ITEM TO BE ADDED)                                                 
*        THIS ROUTINE WILL ADD AN ITEM TO THE TABLE IN CORE.                    
*        IF ITEM IS A DR THEN IT WILL BE ADDED IF IT IS THE FIRST               
*        DR OR IT WILL BE SUMMED WITH THE PREVIOSLY EXISTING DR.                
*                                                                               
         USING TABLED,R6                                                        
*                                                                               
BINADD   NTR1                                                                   
         L     R5,AACCTAB                                                       
         CLI   TBSTAT2,DEBIT       IS IT A DEBIT?                               
         BNE   BIN100                                                           
         OC    ACCNUM,ACCNUM       IS IT THE FIRST DEBIT IN TABLE?              
         BZ    BIN100                                                           
         AP    TBBAMT-TABLED(8,R5),TBBAMT    ADD UP MULTIPLE DR'S               
         B     EXIT                                                             
*                                                                               
BIN100   SR    R3,R3                                                            
         ICM   R3,15,ACCNUM        NUMBER OF ENTRIES IN TABLE ALREADY           
         MH    R3,=Y(TBLNQ)        FIND NEXT AVAILABLE SPOT                     
*                                                                               
         AR    R5,R3                                                            
         MVC   0(TBLNQ,R5),0(R6)   COPY ENTRY INTO TABLE                        
*                                                                               
         SR    R3,R3               BUMP ENTRY COUNT                             
         ICM   R3,15,ACCNUM                                                     
         LA    R3,1(,R3)                                                        
         STCM  R3,15,ACCNUM                                                     
         GOTO1 XSORT,DMCB,(0,AACCTAB),(R3),TBLNQ,TBKEYL,0                       
         CLC   ACCNUM,=F'800'      MAX NUMBER TABLE ENTRIES                     
         BL    EXIT                                                             
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         DROP  R6                  KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        CLEAR ACCUMS                                            *              
******************************************************************              
*                                                                               
*        THIS ROUTINE CLEARS THE ENTIRE BLOCK OF ACCUMS                         
*        TO PL8'0'                                                              
*                                                                               
         USING ACCUMSD,R3                                                       
*                                                                               
CLRACCUM NTR1                                                                   
         LA    R3,ACCUMS                                                        
         LA    R0,LEVCOUNT                                                      
*                                                                               
CLRACC1  LA    R1,ACCNUMQ          CLEAR LEVEL OF ACCUMS                        
         ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(,R3)                                                        
         BCT   R1,*-10                                                          
         BCT   R0,CLRACC1                                                       
         B     EXIT                                                             
*                                                                               
         DROP  R3                  KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        PUT RECORD TO BUFFALO                                   *              
******************************************************************              
*                                                                               
*        FOR EACH BILL 1 BUFFALO RECORD IS PUT OUT FOR THE                      
*        SUMMARY REPORT TO PROCESS AT REQLAST.                                  
*                                                                               
         USING TABLED,R6                                                        
         USING ACKEYD,R5                                                        
*                                                                               
PUTBUFF  NTR1                                                                   
         L     R6,AACCTAB                                                       
         L     R5,ADACC                                                         
         LA    RF,BUFLNQ                                                        
         XCEF  BUFREC,(RF)                                                      
*                                                                               
         CLC   TBTRNDT,STDATE      IF TRANSACTION IS BEFORE START THEN          
         BNL   *+10                ELIMINATE BILLING COLUMN                     
         ZAP   TOTBAMT,=P'0'                                                    
         ZAP   BUFBILL,TOTBAMT     BILLED $                                     
*                                                                               
PUT10    ZAP   BUFAR,TOTAR         A/R AMOUNT                                   
         ZAP   BUFOUT,TOTOUT       OUTSTANDING AMOUNT                           
         ZAP   BUFAGE1,TOTAGE1     FIRST AGING COLUMN                           
         ZAP   BUFAGE2,TOTAGE2     SECOND AGING COLUMN                          
         ZAP   BUFAGE3,TOTAGE3     THIRD AGING COLUMN                           
         ZAP   BUFAGE4,TOTAGE4     FOURTH AGING COLUMN                          
         ZAP   BUFCOF,TOTCOF       COST OF FINANCING                            
         ZAP   BUFYTD,TOTYTD       YEAR TO DATE                                 
         SRP   BUFBILL,64-2,5      ROUND ALL AMOUNTS TO $                       
         SRP   BUFAR,64-2,5                                                     
         SRP   BUFOUT,64-2,5                                                    
         SRP   BUFAGE1,64-2,5                                                   
         SRP   BUFAGE2,64-2,5                                                   
         SRP   BUFAGE3,64-2,5                                                   
         SRP   BUFAGE4,64-2,5                                                   
         SRP   BUFCOF,64-2,5                                                    
         SRP   BUFYTD,64-2,5                                                    
         ZAP   BUFDUMMY,=P'1'      ASSURES US TO GET REC PASSED BACK            
         MVC   DETLEV,QOPT1        LEVEL OF DETAIL                              
         NI    DETLEV,X'0F'        CONVERT X'F1' -> X'01'                       
*                                                                               
         CLC   CLILEV,DETLEV       IF DETAIL LEVEL IS BELOW CLIENT              
         BNH   PUT100              THEN PUT DETAIL LEVEL IN BODY OF RPT         
         MVC   CLILEV,DETLEV                                                    
*                                                                               
PUT100   LA    R1,LEVELS           MOVE IN ACCOUNT CODE                         
         ZIC   R2,DETLEV           WHAT LEVEL OF DETAIL?                        
         BCTR  R2,0                                                             
         AR    R1,R2                                                            
         ZIC   R2,0(,R1)                                                        
         MVC   BUFACC(L'BUFACC),XSPACES                                         
         BCTR  R2,0                                                             
         EXMVC R2,BUFACC,ACKEYACC+3                                             
         LA    R1,LVNAME           ACCOUNT LEVEL NAME                           
         ZIC   R2,DETLEV                                                        
         BCTR  R2,0                                                             
         MH    R2,=Y(L'LVNAME)                                                  
         AR    R1,R2                                                            
         MVC   BUFNAME,0(R1)                                                    
         CLI   QOPT2,C'C'          DISPLAY HIGHER LEVELS?                       
         BE    PUT200                                                           
         MVC   BUFLEV,DETLEV                                                    
         MVI   BUFTYPE,C'1'        SUMMARY TYPE                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
         ZIC   R1,DETLEV                                                        
         BCTR  R1,0                                                             
         STC   R1,DETLEV                                                        
         CLI   DETLEV,0                                                         
         BH    PUT100              LOOP BACK AND PUT OUT ALL                    
*                                                                               
         MVC   DETLEV,QOPT1        PUT OUT BUFFALO TOTAL RECORDS                
         NI    DETLEV,X'0F'        CONVERT X'F1' -> X'01'                       
         ZIC   R1,DETLEV                                                        
         BCTR  R1,0                                                             
         STC   R1,DETLEV           NO TOTAL FOR LOWEST LEVEL OF RPT             
         CLI   DETLEV,0                                                         
         BNH   PUTX                                                             
*                                                                               
PUT150   LA    R1,LEVELS           MOVE IN ACCOUNT CODE                         
         ZIC   R2,DETLEV           WHAT LEVEL OF DETAIL?                        
         BCTR  R2,0                                                             
         AR    R1,R2                                                            
         ZIC   R2,0(,R1)                                                        
         MVI   BUFACC,X'FF'                                                     
         MVC   BUFACC+1(L'BUFACC-1),BUFACC                                      
         BCTR  R2,0                                                             
         EXMVC R2,BUFACC,ACKEYACC+3                                             
         MVC   BUF2ACC,XSPACES                                                  
         EXMVC R2,BUF2ACC,ACKEYACC+3                                            
         CLI   DETLEV,1            ISOLATE SPECIFIED LEVEL                      
         BE    PUT175                                                           
         LA    R1,LEVELS                                                        
         ZIC   R2,DETLEV                                                        
         SH    R2,=H'2'                                                         
         AR    R1,R2                                                            
         ZIC   R2,0(,R1)                                                        
         BCTR  R2,0                                                             
         EXMVC R2,BUF2ACC,XSPACES                                               
         GOTO1 SQUASHER,DMCB,BUF2ACC,L'BUF2ACC                                  
*                                                                               
PUT175   LA    R1,LVNAME           ACCOUNT LEVEL NAME                           
         ZIC   R2,DETLEV                                                        
         BCTR  R2,0                                                             
         MH    R2,=Y(L'LVNAME)                                                  
         AR    R1,R2                                                            
         MVC   BUFNAME,0(R1)                                                    
*                                                                               
         MVI   BUFLEV,X'FF'        TOTAL RECORD                                 
         MVI   BUFTYPE,C'1'        SUMMARY TYPE                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
         ZIC   R1,DETLEV                                                        
         BCTR  R1,0                                                             
         STC   R1,DETLEV                                                        
         CLI   DETLEV,0                                                         
         BH    PUT150              LOOP BACK AND PUT OUT ALL                    
         B     PUTX                HIGHER LEVEL ACCOUNTS                        
*                                                                               
PUT200   CLI   DETLEV,1            ELSE ISOLATE SPECIFIED LEVEL                 
         BE    PUT250                                                           
         LA    R1,LEVELS                                                        
         ZIC   R2,DETLEV                                                        
         SH    R2,=H'2'                                                         
         AR    R1,R2                                                            
         ZIC   R2,0(,R1)                                                        
         BCTR  R2,0                                                             
         EXMVC R2,BUFACC,XSPACES                                                
*                                                                               
PUT250   MVC   BUF2ACC,BUFACC                                                   
         MVC   BUFLEV,DETLEV       PUT RECORD TO BUFFALO                        
         MVI   BUFTYPE,C'1'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
PUTX     MVI   BUFACC,X'FF'                                                     
         MVC   BUFACC+1(L'BUFACC-1),BUFACC                                      
         MVC   BUFNAME,XSPACES                                                  
         MVC   BUF2ACC,XSPACES                                                  
         MVC   BUFNAME(6),=C'REPORT'                                            
         MVI   BUFLEV,X'FF'        TOTAL RECORD                                 
         MVI   BUFTYPE,C'1'        SUMMARY TYPE                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
         B     EXIT                                                             
*                                                                               
         DROP  R5,R6               KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*        PRINT SUMMARY                                           *              
******************************************************************              
*                                                                               
REQL     DS    0H                                                               
*                                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   REQL1                                                            
         LA    R3,ACCUM5                                                        
         MVI   PRNTLEV,5           PRINT REPORT TOTALS FOR DETAIL REPRT         
         BAS   RE,FORMAT                                                        
*                                                                               
REQL1    MVI   QOPT3,C'Y'          FORCE PRINT ROUTINE                          
         LA    R4,PRNTBLK                                                       
         BAS   RE,CLRBLK           CLEAR UP ANY LEFTOVERS FROM DETAIL           
         MVC   XP,XSPACES          REPORT THAT DIDNT PRINT                      
         MVC   XPSECOND,XSPACES                                                 
         MVC   XPTHIRD,XSPACES                                                  
         MVC   XPFOURTH,XSPACES                                                 
*                                                                               
         LA    R1,OPTION4          FIND CORRECT COLUMN FORMAT                   
         LA    R0,OPT4LOOP         NUMBER TIMES TO LOOP                         
*                                                                               
REQL2    CLC   QOPT4,0(R1)         FIND MATCH FOR QOPT4                         
         BE    REQL4                                                            
         LA    R1,OPT4LNQ(,R1)                                                  
         BCT   R0,REQL2                                                         
         DC    H'0'                INVALID INPUT INTO OPTION 4                  
*                                                                               
REQL4    ZIC   R0,1(,R1)           GET SPROG                                    
         STC   R0,RCSUBPRG                                                      
         STC   R0,MYSUBPRG                                                      
         LA    R1,2(,R1)                                                        
         STCM  R1,15,AOPT4         SAVE A(OPT4 TBLE ENTRY)                      
         MVC   PAGE,=H'1'          RESET PAGE NUMBER                            
         MVC   HED1,XSPACES                                                     
         MVC   HED2,XSPACES                                                     
         MVC   HED3,XSPACES                                                     
         MVC   HED4,XSPACES                                                     
         BAS   RE,CLRACCUM         CLEAR ACCUMULATORS                           
         LA    R4,PRNTBLK                                                       
         BAS   RE,CLRBLK                                                        
*                                                                               
REQL5    LA    RF,BUFLNQ                                                        
         XCEF  BUFREC,(RF)                                                      
         MVI   BUFTYPE,C'1'        ONLY TAKE SUMMARY RECORDS                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',(0,ABUFF),BUFREC,1                         
         TM    DMCB+8,X'80'        TEST E-O-F                                   
         BO    EXIT                                                             
*                                                                               
         USING PLINE2D,R4                                                       
*                                                                               
         LA    R4,PRNTBLK                                                       
         CLI   QOPT2,C'C'                                                       
         BNE   REQL20                                                           
         MVI   HDFLAG,C'Y'                                                      
         BAS   RE,HEADUP           HEADUP FOR COMBINED ACCTS ONLY               
*                                                                               
REQL20   DS    0H                                                               
         CLI   BUFLEV,X'FF'        TOTAL RECORD?                                
         BE    REQL75                                                           
         CLI   QOPT2,C'C'          PRINT COMBINED LEVELS                        
         BE    REQL90                                                           
*                                                                               
         CLI   BUFLEV,1            IF FIRST LEVEL THEN ALREADY ISOLATED         
         BE    REQL45                                                           
         LA    R1,LEVELS           ELSE ISOLATE DESIRED LEVEL OF ACCT           
         ZIC   R2,BUFLEV                                                        
         SH    R2,=H'2'                                                         
         AR    R1,R2                                                            
         ZIC   R2,0(,R1)                                                        
         BCTR  R2,0                                                             
         EXMVC R2,BUFACC,XSPACES                                                
         GOTO1 SQUASHER,DMCB,BUFACC,L'BUFACC                                    
*                                                                               
REQL45   CLC   BUFLEV,CLILEV       PUT IN HEADLINES OR BODY OF REPORT           
         BL    REQL55                                                           
         BAS   RE,HEADUP           MAY HAVE TO PRINT SAVED UP HEADLINES         
         B     REQL90                                                           
*                                                                               
REQL55   MVC   WRK2,XSPACES                                                     
         LA    R1,LEVELNM          GET HIERARCHY NAME FOR LEVEL                 
         ZIC   R2,BUFLEV                                                        
         BCTR  R2,0                                                             
         MH    R2,=Y(L'LEVELNM)                                                 
         AR    R1,R2                                                            
         MVC   WRK2(L'LEVELNM),0(R1)                                            
         MVC   WRK2+16(L'BUFACC),BUFACC                                         
         MVC   WRK2+30(L'BUFNAME),BUFNAME                                       
*                                                                               
         LA    R1,HEDS             DETERMINE WHICH HEADLINE TO PUT IN           
         ZIC   R2,BUFLEV                                                        
         BCTR  R2,0                                                             
         MH    R2,=Y(L'HEDS)                                                    
         AR    R1,R2                                                            
         MVC   0(L'HEDS,R1),XSPACES                                             
         MVC   0(L'WRK2,R1),WRK2                                                
         MVI   HDFLAG,C'Y'                                                      
         B     REQL100                                                          
*                                                                               
REQL75   DS    0H                  PRINT TOTALS                                 
*        CLI   QOPT3,C'Y'                                                       
*        BE    *+8                                                              
         BAS   RE,OUTREC                                                        
         MVC   WRK2,XSPACES                                                     
         MVC   WRK2(9),=C'TOTAL FOR'                                            
         MVC   WRK2+10(L'BUF2ACC),BUF2ACC                                       
         MVC   WRK2+25(L'BUFNAME),BUFNAME                                       
         GOTO1 SQUASHER,DMCB,WRK2,L'WRK2                                        
         MVC   PACC2(L'WRK2),WRK2                                               
         B     REQL95                                                           
*                                                                               
REQL90   MVC   PACC2,BUFACC        STAGGER DIFFERNET LEVELS                     
         MVC   PNAME2,BUFNAME                                                   
         CLI   QOPT2,C'C'                                                       
         BE    REQL95                                                           
         MVC   WRK2,XSPACES                                                     
         MVC   WRK2(L'PDATA2),PDATA2                                            
         MVC   PDATA2,XSPACES                                                   
         ZIC   R2,BUFLEV                                                        
         MH    R2,=H'2'                                                         
         LA    R1,PDATA2                                                        
         AR    R1,R2                                                            
         LA    RF,L'PDATA2         SUBTRACT FROM LENGTH                         
         BCTR  RF,0                                                             
         EXMVC RF,0(R1),WRK2                                                    
*                                                                               
REQL95   OI    BUFLEV,X'F0'        CONVERT TO CHARS                             
         CLC   BUFLEV,QOPT1                                                     
         BL    REQL98              NO AMOUNTS FOR HIGHEST LEVEL                 
         EDIT  (P8,BUFBILL),(10,PBILL2),FLOAT=-                                 
         EDIT  (P8,BUFAR),(10,PAR2),FLOAT=-                                     
         EDIT  (P8,BUFOUT),(10,POUT2),FLOAT=-                                   
         EDIT  (P8,BUFAGE1),(10,PAGE1),FLOAT=-                                  
         EDIT  (P8,BUFAGE2),(10,PAGE2),FLOAT=-                                  
         EDIT  (P8,BUFAGE3),(10,PAGE3),FLOAT=-                                  
         EDIT  (P8,BUFAGE4),(10,PAGE4),FLOAT=-                                  
         EDIT  (P8,BUFCOF),(9,PCOF2),FLOAT=-                                    
         EDIT  (P8,BUFYTD),(9,PYTD2),FLOAT=-                                    
         ZAP   PACK16,BUFOUT                                                    
         MP    PACK16,=P'10000'                                                 
         CP    BUFAR,=P'0'         AVOID DIVISION BY ZERO                       
         BE    REQL98                                                           
         DP    PACK16,BUFAR        CALC % PAST DUE                              
         ZAP   WRKAMT1(8),PACK16(8)                                             
         SRP   WRKAMT1,64-2,5                                                   
         EDIT  (P8,WRKAMT1),(3,PPRCNT2)                                         
*                                                                               
REQL98   BAS   RE,PRNTSUM          PRINT SUMMARY REPORT LINE                    
         CLI   BUFLEV,X'FF'                                                     
         BNE   *+8                                                              
         BAS   RE,OUTREC                                                        
*                                                                               
REQL100  GOTO1 BUFFALO,DMCB,=C'SEQ',(C'1',ABUFF),BUFREC,1                       
         TM    DMCB+8,X'80'        TEST E-O-F                                   
         BZ    REQL20                                                           
*                                                                               
REQL200  DS    0H                                                               
         CLI   QOPT7,C'Y'      WAS DOWNLOAD SELECTED                            
         BNE   REQLX                                                            
         MVC   XP,XSPACES                                                       
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOR     DOWNLOAD EOR MARKER                   
REQLX    B     EXIT                                                             
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*       PRINT SUMMARY LINE                                       *              
******************************************************************              
*                                                                               
*        THIS ROUTINE DETERMINES WHICH COLS TO PRINT                            
*        DEPENDING ON THE TABLE ENTRIES IN THE OPT4 TABLE                       
*                                                                               
         USING OPT4D,R5                                                         
*                                                                               
PRNTSUM  NTR1                                                                   
         L     R5,AOPT4            POINT TO PRINT SEQUENCE LIST                 
         LA    R4,XP+2             POINT TO PRINT LINE                          
         LA    R3,OPT4NUM          NUMBER OF ENTRIES IN OPT4 TABLE              
*                                                                               
         USING COLUMND,R6                                                       
*                                                                               
PSUM5    LA    R6,COLUMN                                                        
         LA    R0,COLLNQ           NUMBER ENTRIES IN TABLE                      
*                                                                               
PSUM10   CLI   OPTENTRY,NO         DONT PRINT THIS COLUMN                       
         BE    PSUM100                                                          
PSUM15   CLC   OPTENTRY,CCOL       FIND MATCH IN TABLE                          
         BE    PSUM20                                                           
         LA    R6,CLNQ(,R6)        BUMP TO NEXT ENTRY                           
         BCT   R0,PSUM15                                                        
         DC    H'0'                                                             
*                                                                               
PSUM20   LA    R1,PRNTBLK                                                       
         ZIC   R0,COFFSET                                                       
         AR    R1,R0               BUMP TO CORRECT OFFSET                       
         ZIC   RF,COUTLEN                                                       
         BCTR  RF,0                PREPARE FOR EXMVC                            
         EXMVC RF,0(R4),0(R1)                                                   
         LA    R4,3(RF,R4)         BUMP TO NEXT AVAIL POS IN PRNT LINE          
         LA    R5,1(,R5)                                                        
         BCT   R3,PSUM5                                                         
*                                                                               
PSUM100  DS    0H                                                               
         BAS   RE,OUTREC                                                        
         MVC   PRNTBLK(L'XP),XSPACES                                            
         B     EXIT                                                             
*                                                                               
         DROP  R5,R6               KEEP IT CLEAN                                
         EJECT ,                                                                
******************************************************************              
*       PRINT HEADLINES                                          *              
******************************************************************              
*                                                                               
HEADUP   NTR1                                                                   
         CLI   HDFLAG,C'Y'                                                      
         BNE   EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,OUTREC                                                        
         MVI   HDFLAG,C'N'         RESET HEAD FLAG                              
         MVI   FORCEHED,C'N'                                                    
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        CLEAR DETAIL TABLE                                      *              
******************************************************************              
*                                                                               
CLRDET   NTR1                                                                   
         LA    R0,SPACES           SOURCE ADDRESS                               
         SR    R1,R1                                                            
         ICM   R1,8,SPACES         PAD CHARACTER                                
         L     RE,ADETTBLE         A(DESTINATION)                               
         LA    RF,DETTBLEQ                                                      
         MVCL  RE,R0                                                            
         B     EXIT                                                             
         EJECT ,                                                                
******************************************************************              
*        EQUATES                                                 *              
******************************************************************              
*                                                                               
REPWIDTH EQU   165                 WIDTH OF REPORT                              
         EJECT ,                                                                
******************************************************************              
*        EXTERNAL ADDRESS LIST                                   *              
******************************************************************              
*                                                                               
ADCONS   DS    0F                                                               
         DC    V(DATVAL)                                                        
         DC    V(SQUASHER)                                                      
         DC    V(UNDERLIN)                                                      
         DC    V(CHOPCON)                                                       
         DC    A(IOAREA1)                                                       
         DC    A(IOAREA2)                                                       
         DC    A(WKAREA)                                                        
         DC    A(ACCTAB)                                                        
         DC    A(BUFFALOC)                                                      
         DC    V(ACLIST)                                                        
         DC    A(DETTBLE)                                                       
         DC    V(PERVERT)                                                       
         DC    V(DLFLD)                                                         
         DC    A(DWNL)             DOWNLOAD                                     
         DC    A(DWNRTE)           DOWNLOAD ROUTINE                             
         DC    A(DWNBUF)           DOWNLOAD BUFFER                              
         EJECT ,                                                                
******************************************************************              
*        PROGRAM USED EQUATES                                    *              
******************************************************************              
*                                                                               
TRANSFER EQU   0                                                                
OFFSET   EQU   1                                                                
WRITEOFF EQU   2                                                                
CHECK    EQU   3                                                                
BILL     EQU   4                                                                
UNAPPLD  EQU   5                                                                
OVRPAY   EQU   6                                                                
OVRPY1   EQU   X'FF'                                                            
OVRPY2   EQU   X'DD'                                                            
DUMMY    EQU   X'EE'                                                            
DEBIT    EQU   0                                                                
CREDIT   EQU   1                                                                
         EJECT ,                                                                
******************************************************************              
*        PRINT COLUMN EQUATES                                    *              
******************************************************************              
*                                                                               
NO       EQU   0                   NO COLUMN TO PRINT                           
DATA     EQU   1                   ACCOUNT CODE / NAME                          
BIL      EQU   2                   BILL AMOUNT                                  
AR       EQU   3                   AR BALANCE                                   
PCT      EQU   4                   PERCENT                                      
OUT      EQU   5                   OUTSTANDING AMOUNT                           
COF      EQU   6                   COST OF FINANCE                              
AGE1     EQU   7                   1ST AGING COLUMN                             
AGE2     EQU   8                   2ND                                          
AGE3     EQU   9                   3RD                                          
AGE4     EQU   10                  4TH                                          
YTD      EQU   11                  YEAR TO DATE                                 
         EJECT ,                                                                
******************************************************************              
*        PRINT COLUMN TABLE                                      *              
******************************************************************              
*                                                                               
OPTION4  DC    C' '                DEFAULT                                      
         DC    AL1(1)              SPROG 1                                      
         DC    AL1(DATA,BIL,AR,OUT,PCT,AGE1,AGE2,AGE3,AGE4,COF,YTD)             
*                                                                               
         DC    C'S'                SUPPRESS AGING COLUMNS                       
         DC    AL1(2)              SPROG 2                                      
         DC    AL1(DATA,BIL,AR,OUT,PCT,COF,YTD,NO,NO,NO,NO)                     
*                                                                               
OPT4LOOP EQU   2                   NUMBER ITEMS IN TABLE                        
OPT4LNQ  EQU   (*-OPTION4)/2       LENGTH OF ENTIRE LINE                        
OPT4NUM  EQU   11                  NUMBER ENTRIES IN A LINE                     
         EJECT ,                                                                
******************************************************************              
*        COLUMN TABLE                                            *              
******************************************************************              
*                                                                               
*        AL1  COLUMN TO PROCESS                                                 
*        AL1  LENGTH OF OUTPUT FIELD                                            
*        AL1  OFFSET TO FIELD DATA                                              
*                                                                               
COLUMN   DC    AL1(DATA),AL1(48),AL1(PDATA2-PLINE2D)                            
         DC    AL1(BIL),AL1(10),AL1(PBILL2-PLINE2D)                             
         DC    AL1(OUT),AL1(10),AL1(POUT2-PLINE2D)                              
         DC    AL1(AR),AL1(10),AL1(PAR2-PLINE2D)                                
         DC    AL1(PCT),AL1(3),AL1(PPRCNT2-PLINE2D)                             
         DC    AL1(AGE1),AL1(10),AL1(PAGE1-PLINE2D)                             
         DC    AL1(AGE2),AL1(10),AL1(PAGE2-PLINE2D)                             
         DC    AL1(AGE3),AL1(10),AL1(PAGE3-PLINE2D)                             
         DC    AL1(AGE4),AL1(10),AL1(PAGE4-PLINE2D)                             
         DC    AL1(COF),AL1(9),AL1(PCOF2-PLINE2D)                               
         DC    AL1(YTD),AL1(9),AL1(PYTD2-PLINE2D)                               
COLLNQ   EQU   (*-COLUMN)/3                                                     
         EJECT ,                                                                
******************************************************************              
*        RATE TABLE - DEFAULT IS 10%                             *              
******************************************************************              
*                                                                               
RATETBDF DS    0H                          DEFAULT RATE TABLE                   
         DC    X'000000',PL8'273972',PL8'100000'                                
         DC    X'FFFFFF',PL8'273972',PL8'100000'                                
         EJECT ,                                                                
******************************************************************              
*        LITERAL DECLARATIONS                                    *              
******************************************************************              
*                                                                               
         LTORG                                                                  
*                                                                               
         GETEL R5,DATADISP,ELCODE                                               
*                                                                               
BILLLN   DC    CL36'BILL  XXXXXX (XXX00/00) DUE XXX00/00'                       
CHCKLN   DC    CL36'CHECK XXXXXX (XXX00/00) DEP XXX00/00'                       
OFFSETLN DC    CL36'OFFSET                      XXX00/00'                       
WOLN     DC    CL36'W/O   XXXXXX                XXX00/00'                       
OVERLN   DC    CL36'OVERPAYMENT             DEP XXX00/00'                       
*                                                                               
         BUFF  FLAVOR=PACKED,                                          X        
               KEYLIST=(14,A),                                         X        
               COMMENT=48,                                             X        
               LINES=100,                                              X        
               COLUMNS=10,                                             X        
               ROWS=1                                                           
         EJECT ,                                                                
         DS    0D                  DOWNLOAD BUFFER                              
DWNBUF   DS    CL250                                                            
******************************************************************              
*        BOX HOOK                                                *              
******************************************************************              
*                                                                               
         USING BOXD,R4                                                          
*                                                                               
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R4,ADBOX                                                         
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
*                                                                               
         CLI   MYSUBPRG,0          DETAIL REPORT                                
         BNE   BX10                SPROG - 0                                    
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+49,C'C'                                                  
         MVI   BOXCOLS+64,C'C'                                                  
         MVI   BOXCOLS+79,C'C'                                                  
         MVI   BOXCOLS+94,C'C'                                                  
         MVI   BOXCOLS+109,C'C'                                                 
         MVI   BOXCOLS+133,C'C'                                                 
         MVI   BOXCOLS+147,C'C'                                                 
         MVI   BOXCOLS+162,C'R'                                                 
         B     BXXIT                                                            
*                                                                               
BX10     DS    0H                        SUMMARY REPORT - WITH AGING            
         CLI   MYSUBPRG,1          SPROG - 1                                    
         BNE   BX20                                                             
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+50,C'C'                                                  
         MVI   BOXCOLS+63,C'C'                                                  
         MVI   BOXCOLS+75,C'C'                                                  
         MVI   BOXCOLS+87,C'C'                                                  
         MVI   BOXCOLS+92,C'C'                                                  
         MVI   BOXCOLS+140,C'C'                                                 
         MVI   BOXCOLS+151,C'C'                                                 
         MVI   BOXCOLS+162,C'R'                                                 
         B     BXXIT                                                            
*                                                                               
BX20     DS    0H                        SUMMARY REPORT - W/O AGING             
         CLI   MYSUBPRG,2          SPROG - 2                                    
         BNE   BXXIT                                                            
         MVI   BOXROWS+6,C'T'            SET ROWS - SPROG 2                     
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS,C'L'              SET LH MARGIN                          
         MVI   BOXCOLS+50,C'C'                                                  
         MVI   BOXCOLS+63,C'C'                                                  
         MVI   BOXCOLS+75,C'C'                                                  
         MVI   BOXCOLS+87,C'C'                                                  
         MVI   BOXCOLS+92,C'C'                                                  
         MVI   BOXCOLS+103,C'C'                                                 
         MVI   BOXCOLS+114,C'R'                                                 
*                                                                               
BXXIT    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
**********************************************************************          
* DOWNLOAD ROUTINE                                                   *          
*          R7 = A(PRINT LINE)                                        *          
**********************************************************************          
         SPACE 1                                                                
         USING PLINED,R7                                                        
DWNRTE   DS    0D                                                               
         NMOD1 0,**DWNR**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         USING BOXD,R3                                                          
         L     R3,ADBOX                                                         
         MVI   BOXOFF,C'Y'         NO BOXES IF DOWNLOADING                      
         DROP  R3                                                               
*                                                                               
         LA    R7,SVPRNT                                                        
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         LA    R2,HEDS                                                          
         LHI   R3,4                                                             
DWNR10   MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BUFACC),16(R2)    MOVE LEVELS CODE                      
         LA    R1,L'BUFACC                LENGTH LEVEL A CODE                   
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD CODE                         
         LA    R2,L'HEDS(R2)                                                    
         BCT   R3,DWNR10                                                        
*                                                                               
         USING PLINE2D,R7                                                       
*                                                                               
         MVC   DWNFLDX,XSPACES            SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLDX(L'PDATA2),PDATA2    ACCOUNT                              
         LA    R1,L'PDATA2                FIELD LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNREC     DOWNLOAD EXTENDED TEXT                
*                                                                               
         MVI   PRTSIZE,0                                                        
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PBILL2),PBILL2    BILL AMOUNT TO DWNLD FLD              
         LA    R1,L'PBILL2                FIELD LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PAR2),PAR2        AR AMOUNT TO DWNLD FLD                
         LA    R1,L'PAR2                  FIELD LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'POUT2),POUT2      OUTSTANDING AMOUNT                    
         LA    R1,L'POUT2                 FIELD LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PPRCNT2),PPRCNT2  FINAL CHARGE TO DWNLD FLD             
         LA    R1,L'PPRCNT2               FIELD LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PAGE1),PAGE1      AGING COLUMN                          
         LA    R1,L'PAGE1                 FIELD LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PAGE2),PAGE2      AGING COLUMN                          
         LA    R1,L'PAGE2                 FIELD LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PAGE3),PAGE3      AGING COLUMN                          
         LA    R1,L'PAGE3                 FIELD LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PAGE4),PAGE4      AGING COLUMN                          
         LA    R1,L'PAGE4                 FIELD LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PCOF2),PCOF2      COST OF FINANCING                     
         LA    R1,L'PCOF2                 FIELD LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PYTD2),PYTD2      COST OF FINANCING                     
         LA    R1,L'PYTD2                 FIELD LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
DWNLST   MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOL     DOWNLOAD EOL MARKER                   
*                                                                               
DWNXIT   XMOD1                                                                  
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD MODULE                                                    *          
*          PARM1 - RC                                                *          
*          PARM2 - ACTION                                            *          
**********************************************************************          
         SPACE 1                                                                
DWNL     DS    0D                                                               
         NMOD1 0,**DOWN**                                                       
         L     RC,0(R1)                                                         
         L     RF,4(R1)                                                         
         STC   RF,DWNMODE          SAVE CURRENT MODE                            
         USING DLCBD,R5                                                         
         L     R5,ADWNBUF                                                       
*                                                                               
         CLI   DWNMODE,DWNINIT     INITIALIZE                                   
         BE    DWNL10                                                           
         CLI   DWNMODE,DWNTEXT     DOWN-LOAD TEXT                               
         BE    DWNL20                                                           
         CLI   DWNMODE,DWNNUM      DOWN-LOAD NUMBER                             
         BE    DWNL30                                                           
         CLI   DWNMODE,DWNPACK     DOWN-LOAD NUMBER (PACKED)                    
         BE    DWNL40                                                           
         CLI   DWNMODE,DWNREC      DOWN-LOAD RECORD (EXTENDED)                  
         BE    DWNL50                                                           
         MVI   DLCBACT,DLCBEOL                                                  
         CLI   DWNMODE,DWNEOL      END OF LINE                                  
         BE    DWNL60                                                           
         MVI   DLCBACT,DLCBEOR                                                  
         CLI   DWNMODE,DWNEOR      END OF REPORT                                
         BE    DWNL60                                                           
         DC    H'0'                                                             
*                                                                               
* INITIALIZATION                                                                
*                                                                               
DWNL10   TM    DWNSTAT,DWNINTZ     HAS IT ALREADY BEEN INITIALIZED?             
         BO    DWNLX               YES - EXIT                                   
         MVI   DLCBACT,DLCBINIT    DOWN LOAD ACTION IS START                    
         LA    RE,XP               PRINT LINE                                   
         ST    RE,DLCBAPL                                                       
         LA    RE,DWNHOOK          POINT TO HOOK FOR APPLICATION                
         ST    RE,DLCBAPR                                                       
         MVC   DLCXMAXL,=Y(L'XP)                                                
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
         GOTO1 DLFLD,(R5)                                                       
         MVI   FORCEHED,C'Y'       EXCEPT FIRST TIME IN                         
         GOTO1 ACREPORT                                                         
         MVC   DLCBFLD,SPACES      MUST CLEAR FIRST TIME IN                     
*                                  TURN OFF DOWN-LOAD ROW FLDS AS C' '          
         OI    DWNSTAT,DWNINTZ     TURN ON INITIALIZED BYTE                     
         B     DWNLX               EXIT                                         
*                                                                               
* DOWNLOAD A RECORD - TEXT                                                      
*                                                                               
DWNL20   MVC   DLCBLEN,PRTSIZE     LEN OF FIELD-SET TO 0 (NO PADDING)           
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBTXT     TYPE   IS TEXT                               
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL60              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER                                                    
*                                                                               
DWNL30   MVI   DLCBLEN,0           LEN OF FIELD-SET TO 0 (NO PADDING)           
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBNUM     TYPE   IS NUMBER                             
         MVI   DLCBLEN,16          YES, USE MAXIMUM LENGTH OF NUMERICS          
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL60              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER (PACKED)                                           
*                                                                               
DWNL40   MVI   DLCBTYP,DLCBPACF    PACKED DATA                                  
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBLEN,8        L'PKFLDS YES, USE MAX LEN OF NUMERICS           
         XC    DLCBFLD,DLCBFLD     CLEAN DWNLOAD FIELD TO 0'S                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         NC    DLCBFLD,DLCBFLD     YES, MAKE SURE NUMERIC FLD NOT ZEROS         
         BNZ   DWNL60              NOT  ZERO, DOWN-LOAD FIELD                   
         MVI   DLCBLEN,1           ZERO, SET LENGTH 1 TO DOWN-LOAD A 0          
         B     DWNL60                                                           
*                                                                               
* DOWNLOAD A RECORD - FULL RECORD (160 BYTE MAX)                                
*                                                                               
DWNL50   MVC   DLCBLEN,PRTSIZE     LEN OF FIELD-SET TO 0 (NO PADDING)           
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBTXT     TYPE IS TEXT                                 
         MVC   DLCBFLX,XSPACES                                                  
         MVC   DLCBFLX,DWNFLDX                                                  
         OI    DLCBFLG1,DLCBFXFL   USING EXTENDED FIELD                         
*                                                                               
* END OF LINE/END OF RECORD                                                     
*                                                                               
DWNL60   GOTO1 DLFLD,(R5)          DOWN-LOAD FIELD                              
         NI    DLCBFLG1,X'FF'-DLCBFXFL   TURN OFF USING EXTENDED FIELD          
*                                                                               
DWNLX    XMOD1                                                                  
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD HOOK                                                      *          
**********************************************************************          
         SPACE 1                                                                
DWNHOOK  MVI   FORCEHED,C'N'       NEVER HEAD UP                                
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         L     RF,ACREPORT                                                      
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*        STORAGE                                                 *              
******************************************************************              
*                                                                               
DETTBLE  DS    0D                  DETAIL TABLE                                 
         DS    2496C               (104 POSSIBLE RATES)                         
DETTBLEQ EQU   *-DETTBLE                                                        
*                                                                               
IOAREA1  DS    0D                  IOAREA #1                                    
         DS    2000C                                                            
*                                                                               
IOAREA2  DS    0D                  IOAREA #2                                    
         DS    2000C                                                            
*                                                                               
WKAREA   DS    0D                  WORK AREA                                    
         DS    200C                                                             
*                                                                               
ACCTAB   DS    (800*TBLNQ)C        TABLE OF ACCOUNTS                            
         EJECT ,                                                                
******************************************************************              
*        DSECT TO COVER SAVE W/S                                 *              
******************************************************************              
*                                                                               
ACCF02D  DSECT                                                                  
VTYPES   DS    0A                  EXTERNAL ADDRESSES                           
DATVAL   DS    A                   DATE VALIDATION                              
SQUASHER DS    A                   SQUASHER                                     
UNDERLIN DS    A                                                                
CHOPCON  DS    A                   CHOPPER                                      
AIOAREA1 DS    A                   IO AREA #1                                   
ARATETB  DS    A                                                                
AWKAREA  DS    A                   WORK AREA                                    
AACCTAB  DS    A                   ACTUAL TABLE                                 
ABUFF    DS    A                   BUFFALO                                      
ACLIST   DS    A                   ACLIST                                       
ADETTBLE DS    A                   DETAIL TABLE                                 
PERVERT  DS    A                   PERVERT                                      
DLFLD    DS    V                   DOWNLOAD MODULE                              
ADWNL    DS    A                   DOWNLOAD                                     
ADWNRTE  DS    A                   DOWNLOAD ROUTINE                             
ADWNBUF  DS    A                   DOWNLOAD BUFFER                              
VTYPLNQ  EQU   *-VTYPES                                                         
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
AOPT4    DS    A                   ADDRESS OF OPT4 LINE IN USE                  
*                                                                               
OVERSW   DS    XL1                 OVERPAYMENT SWITCH                           
MYSUBPRG DS    XL1                                                              
FINYEAR  DS    PL3                 START OF FINANCIAL YR                        
CMPABBR  DS    CL7                 COMPANY ABBREVIATION                         
CMPNAME  DS    CL36                COMPANY NAME                                 
BAT55MOS DS    PL3                                                              
BAT55DTE DS    PL3                                                              
DWNFLD   DS    CL40            SAVED AREA FOR FIELD TO BE DOWNLOADED            
DWNFLDX  DS    CL160           EXTENDED FIELD FOR DOWNLOAD                      
PRTSIZE  DS    XL1             PRINT AREA LENGTH                                
DWNSTAT  DS    XL1             DOWNLOAD STATUS                                  
DWNINTZ  EQU   X'80'           DOWNLOAD INITIALIZED                             
DWNHDLN  EQU   X'40'           DOWNLOAD HEADLINES                               
*                                                                               
DWNMODE  DS    XL1             DOWNLOAD MODE                                    
DWNINIT  EQU   1               DOWN-LOAD INITIALIZATION                         
DWNEOL   EQU   2               MARK END OF LINE                                 
DWNEOR   EQU   3               MARK END OF REPORT                               
DWNTEXT  EQU   4               DOWN-LOAD TEXT                                   
DWNNUM   EQU   5               DOWN-LOAD NUMBER                                 
DWNPACK  EQU   6               DOWN-LOAD NUMBER (PACKED)                        
DWNREC   EQU   7               DOWN-LOAD EXTENDED FIELD                         
SVPRNT   DS    CL(L'XP)            USED FOR DOWNLOAD                            
*                                                                               
LEVELS   DS    0H                                                               
LEVELA   DS    CL1                 LENGTH OF LEVEL A                            
LEVELB   DS    CL1                 LENGTH OF LEVEL B                            
LEVELC   DS    CL1                 LENGTH OF LEVEL C                            
LEVELD   DS    CL1                 LENGTH OF LEVEL D                            
*                                                                               
LEVELNM  DS    0CL15                                                            
LEVELANM DS    CL15                LEDGER LEVEL NAMES (HIERARCHY)               
LEVELBNM DS    CL15                                                             
LEVELCNM DS    CL15                                                             
LEVELDNM DS    CL15                                                             
*                                                                               
LVACCT   DS    0CL12                                                            
LVAACCT  DS    CL12                ACCOUNT CODES                                
LVBACCT  DS    CL12                                                             
LVCACCT  DS    CL12                                                             
LVDACCT  DS    CL12                                                             
*                                                                               
LVNAME   DS    0CL36                                                            
LVANAME  DS    CL36                ACCOUNT NAMES                                
LVBNAME  DS    CL36                                                             
LVCNAME  DS    CL36                                                             
LVDNAME  DS    CL36                                                             
*                                                                               
ACCNUM   DS    XL4                 COUNTER FOR TABLE ENTRIES                    
NEWACC   DS    CL1                 FLAGS IF A NEW ACCOUNT TO PROCESS            
DETLEV   DS    XL1                 LEVEL OF DETAIL                              
FIRSTFLG DS    CL1                 FIRST TIME THROUGH CODE FLAG                 
ELCODE   DS    XL1                 USED IN GETEL ROUTINE                        
*      CLI= NOW REFLECTS DISPLACEMENT INSTEAD OF LEVEL, SO                      
*      CLILEV WILL BE CONVERTED FROM THE DISPLACEMENT                           
CLILEV   DS    XL1                 LEVEL OF ACCOUNT FOR CLIENT                  
CLIPOS   DS    XL1                                                              
*                                                                               
PRNTLEV  DS    XL1                 WHAT LEVEL TOTAL TO PRINT                    
HDFLAG   DS    XL1                                                              
BILLLATE DS    CL1                 IS A BILL LATE - APPLY PAST DUE?             
UNAPPSW  DS    CL1                 IS CURRENT TRNS AN UNAPPLIED CREDIT?         
SAVEACC  DS    CL42                                                             
RKEY     DS    CL42                                                             
*                                                                               
WKDT     DS    PL3                 TEMP WORK DATE                               
WKSTDT   DS    PL3                 START DATE FOR CALC ROUTINE                  
WKENDDT  DS    PL3                 END DATE FOR CALC ROUTINE                    
STDATE   DS    PL3                 START DATE FROM REQUEST CARD                 
ENDDATE  DS    PL3                 END DATE FROM REQUEST CARD                   
BENDATE  DS    XL3                 END DATE IN BINARY                           
TODAYP   DS    PL3                 TODAY'S DATE PACKED                          
TEMPDT   DS    CL6                 YYMMDD                                       
TEMPDT2  DS    CL6                 YYMMDD                                       
LOWLEV   DS    XL1                 LOWEST LEVEL OF STRUCTURE                    
*                                                                               
TOTBAMT  DS    PL8                 TOTAL BILL AMOUNT                            
TOTAR    DS    PL8                 TOTAL A/R AMOUNT                             
TOTPRCK  DS    PL8                 TOTAL CHECK AMOUNT PREVIOUS TO START         
TOTCK    DS    PL8                 TOTAL CHECK AMOUNT                           
TOTOUT   DS    PL8                 CURRENT PAST DUE AMOUNT                      
TOTAGE1  DS    PL8                 AGING COLUMN #1                              
TOTAGE2  DS    PL8                 AGING COLUMN #2                              
TOTAGE3  DS    PL8                 AGING COLUMN #3                              
TOTAGE4  DS    PL8                 AGING COLUMN #4                              
TOTCOF   DS    PL8                 TOTAL COF FOR BILL WITH DATE RANGE           
TOTYTD   DS    PL8                 TOTAL YTD FOR BILL                           
*                                                                               
AGEDAY1  DS    PL8                 AGING DAY BREAKDOWNS                         
AGEDAY2  DS    PL8                                                              
AGEDAY3  DS    PL8                                                              
*                                                                               
WRKAMT1  DS    PL8                 WORK ACCUMS                                  
WRKAMT2  DS    PL8                                                              
WRKAMT3  DS    PL8                                                              
*                                                                               
NUMDAYS  DS    PL8                                                              
TRUERATE DS    PL8                                                              
CURRATE  DS    PL8                 CURRENT RATE AMOUNT                          
COFBEF   DS    PL8                 COF BEFORE START DATE                        
COFAFT   DS    PL8                 COF AFTER START DATE                         
PACK16   DS    PL16                16 BYTE PACKED NUMBER                        
*                                                                               
ACCUMS   DS    0CL80               5 ROWS,10 COLS, 8 BYTE PACKED ACCUMS         
ACCUM1   DS    10PL8               LEVEL1                                       
ACCUM2   DS    10PL8               LEVEL2                                       
ACCUM3   DS    10PL8               LEVEL3                                       
ACCUM4   DS    10PL8               LEVEL4                                       
ACCUM5   DS    10PL8               REPORT TOTALS                                
LEVCOUNT EQU   5                   NUMBER OF ACCUM LEVELS                       
ACCUMLNQ EQU   (*-ACCUMS)/LEVCOUNT LENGTH OF 1 LINE OF ACCUMS                   
ACCUMTMP DS    10PL8               TEMPORARY ACCUM FOR INDIVIDUAL BILL          
*                                                                               
BUFREC   DS    0CL142              BUFFALO RECORD                               
BUFTYPE  DS    CL1                 1 = SUMMARY TYPE                             
BUFACC   DS    CL12                SR ACCOUNT                                   
BUFLEV   DS    CL1                 LEVEL OF ACCNT OR X'FF' FOR TOTALS           
BUF2ACC  DS    CL12                ACCOUNT BROKEN DOWN INTO LEVEL               
BUFNAME  DS    CL36                NAME                                         
BUFBILL  DS    PL8                                                              
BUFAR    DS    PL8                 AR BALANCE                                   
BUFOUT   DS    PL8                 OUTSTANDING AMOUNT                           
BUFAGE1  DS    PL8                 AGING COLUMNS                                
BUFAGE2  DS    PL8                                                              
BUFAGE3  DS    PL8                                                              
BUFAGE4  DS    PL8                                                              
BUFCOF   DS    PL8                 COST OF FINANCING                            
BUFYTD   DS    PL8                 YEAR TO DATE FINANCING                       
BUFDUMMY DS    PL8                 DUMMY FIELD TO PASS BACK ALL RECORDS         
BUFLNQ   EQU   *-BUFREC                                                         
*                                                                               
HEDS     DS    0CL165                                                           
HED1     DS    CL165                                                            
HED2     DS    CL165                                                            
HED3     DS    CL165                                                            
HED4     DS    CL165                                                            
WRK2     DS    CL100                                                            
PRNTBLK  DS    0CL(6*L'XP)         6 LINE PRINT BUFFER                          
PRNTBLK1 DS    2CL(L'XP)                                                        
PRNTBLK2 DS    2CL(L'XP)                                                        
PRNTBLK3 DS    2CL(L'XP)                                                        
PRNTBLKQ EQU   *-PRNTBLK                                                        
         EJECT ,                                                                
******************************************************************              
*        DETAIL TABLE DSECT                                      *              
******************************************************************              
*                                                                               
DETAILD  DSECT                                                                  
DETDAYS  DS    PL8                 NUMBER OF DAYS                               
DETRATE  DS    PL8                 RATE APPLIED TO DAYS                         
DETCOF   DS    PL8                 COST OF FINANCE FOR PERIOD                   
DETLNQ   EQU   *-DETAILD                                                        
         SPACE 3                                                                
******************************************************************              
*        RATE TABLE DSECT                                        *              
******************************************************************              
*                                                                               
RATED    DSECT                                                                  
RTDATE   DS    PL3                 DATE YYMMDD                                  
RTRATE   DS    PL8                 RATE PER DAY (/365)                          
RTTRUE   DS    PL8                 TRUE RATE                                    
RTLNQ    EQU   *-RATED                                                          
         EJECT ,                                                                
******************************************************************              
*        OPT4 TABLE DSECT                                        *              
******************************************************************              
*                                                                               
OPT4D    DSECT                                                                  
OPTENTRY DS    AL1                                                              
         SPACE 5                                                                
******************************************************************              
*        COLUMN DSECT                                            *              
******************************************************************              
*                                                                               
COLUMND  DSECT                                                                  
CCOL     DS    AL1                 COLUMN EQUATE                                
COUTLEN  DS    AL1                 OUTPUT FIELD LENGTH                          
COFFSET  DS    AL1                 DISPLACEMENT IN INPUT TABLE                  
CLNQ     EQU   *-COLUMND                                                        
         EJECT ,                                                                
******************************************************************              
*        BIN ACCNT TABLE                                         *              
******************************************************************              
*                                                                               
TABLED   DSECT                                                                  
TBSTAT1  DS    CL1                                                              
TBACC    DS    CL15                ACCOUNT                                      
TBCON    DS    CL15                CONTRA ACCOUNT                               
TBTRNDT  DS    PL3                 TRANSACTION DATE                             
TBBNUM   DS    CL6                 BILL NUMBER                                  
TBSTAT2  DS    CL1                 DEBIT=C'0' CREDIT=C'1'                       
TBCKDT   DS    PL3                 CHECK DEPOSIT DATE                           
TBKEYL   EQU   *-TABLED                                                         
*                                                                               
TBTYPE   DS    CL1                                                              
TBCKNUM  DS    CL6                 CHECK NUMBER                                 
TBCKAMT  DS    PL8                 CHECK AMOUNT REAL AMOUNT                     
TBCKAPPL DS    PL8                 CHECK AMOUNT USED IN COF CALC                
TBCKOVER DS    PL8                 AMOUNT OF OVERPAYMENT                        
TBOVERST DS    XL1                 OVER FLAG                                    
TBDUEDT  DS    PL3                 BILL DUE DATE (YYMMDD)                       
TBBAMT   DS    PL8                 BILLED AMOUNT                                
TBAR     DS    PL8                 A/R BALANCE                                  
TBOUT    DS    PL8                 OUTSTANDING AMOUNT                           
TBCOF    DS    PL8                 COST OF FINANCING                            
TBYTD    DS    PL8                 YEAR TO DATE                                 
TBDAYS   DS    PL8                 NUMBER OF DAYS LATE                          
TBDETNUM DS    XL1                 NUM ENTRIES FOR CURRENT ITEM                 
TBDET    DS    XL4                 A(FIRST DETAIL ENTRY)                        
TBLNQ    EQU   *-TABLED                                                         
         EJECT ,                                                                
******************************************************************              
*        DSECT FOR ACCUMULATORS                                  *              
******************************************************************              
*                                                                               
ACCUMSD  DSECT                                                                  
ACCBAMT  DS    PL8                 BILL AMOUNT                                  
ACCAR    DS    PL8                 A/R AMOUNT                                   
ACCOUT   DS    PL8                 OUTSTANDING AMOUNT                           
ACCAGE1  DS    PL8                 AGING COLUMNS                                
ACCAGE2  DS    PL8                                                              
ACCAGE3  DS    PL8                                                              
ACCAGE4  DS    PL8                                                              
ACCCKAMT DS    PL8                 PAYMENT AMOUNT                               
ACCCOF   DS    PL8                 COST OF FINANCING                            
ACCYTD   DS    PL8                 YEAR TO DATE                                 
ACCNUMQ  EQU   (*-ACCUMSD)/8                                                    
         EJECT ,                                                                
******************************************************************              
*        DSECT FOR PRINTLINE                                     *              
******************************************************************              
*                                                                               
PLINED   DSECT                                                                  
         DS    CL2                                                              
PNAME    DS    CL47                NAMES                                        
*                                                                               
         ORG   PLINED                                                           
         DS    CL6                                                              
PDATA    DS    CL36                BILL NUMBER NARRATIVE                        
*                                                                               
         ORG   PLINED                                                           
         DS    CL12                                                             
PNUM     DS    CL6                 CHECK/BILL NUMBER                            
*                                                                               
         ORG   PLINED                                                           
         DS    CL20                                                             
PTRNDT   DS    CL8                 TRANSACTION DATE                             
*                                                                               
         ORG   PLINED                                                           
         DS    CL34                                                             
PDATE    DS    CL8                 DUE/PAID DATE                                
*                                                                               
         ORG   PLINED                                                           
         DS    CL49                                                             
PCON     DS    CL14                BILLING SOURCE                               
*                                                                               
         ORG   PLINED                                                           
         DS    CL66                                                             
PBILL    DS    CL12                BILL AMOUNT                                  
*                                                                               
         ORG   PLINED                                                           
         DS    CL81                                                             
POUT     DS    CL12                OUTSTANDING AMOUNT                           
PCHAR1   DS    CL1                                                              
*                                                                               
         ORG   PLINED                                                           
         DS    CL96                                                             
PCKAMT   DS    CL12                CHECK AMOUNT                                 
PCHAR2   DS    CL1                                                              
*                                                                               
         ORG   PLINED                                                           
         DS    CL111                                                            
PDETAIL  DS    CL20                #DAYS @X%                                    
*                                                                               
         ORG   PLINED                                                           
         DS    CL134                                                            
PCOF     DS    CL12                COST OF FINANCING                            
*                                                                               
         ORG   PLINED                                                           
         DS    CL149                                                            
PYTD     DS    CL12                YEAR TO DATE                                 
PCHAR3   DS    CL1                                                              
PLINELNQ EQU   *-PLINED                                                         
         EJECT ,                                                                
******************************************************************              
*        DSECT FOR SUMMARY PRINTLINE                             *              
******************************************************************              
*                                                                               
PLINE2D  DSECT                                                                  
         DS    CL2                                                              
PDATA2   DS    0CL48                                                            
PACC2    DS    CL12                ACCOUNT CODE                                 
PNAME2   DS    CL36                NAMES                                        
         DS    CL2                                                              
PBILL2   DS    CL10                BILL AMOUNT                                  
         DS    CL2                                                              
PAR2     DS    CL10                AR AMOUNT                                    
         DS    CL2                                                              
POUT2    DS    CL10                OUTSTANDING AMOUNT                           
         DS    CL2                                                              
PPRCNT2  DS    CL3                 %=PAST DUE/AR BALANCE                        
         DS    CL2                                                              
PAGE1    DS    CL10                AGING COLUMNS                                
         DS    CL2                                                              
PAGE2    DS    CL10                                                             
         DS    CL2                                                              
PAGE3    DS    CL10                                                             
         DS    CL2                                                              
PAGE4    DS    CL10                                                             
         DS    CL2                                                              
PCOF2    DS    CL9                 COST OF FINANCING                            
         DS    CL2                                                              
PYTD2    DS    CL9                 YEAR TO DATE                                 
         DS    CL2                                                              
PLINE2LN EQU   *-PLINE2D                                                        
         EJECT ,                                                                
******************************************************************              
*        OTHER INCLUDES                                          *              
******************************************************************              
*                                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDCNTRL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCNTRL                                                        
         PRINT ON                                                               
* ACBIGPRNTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* ++INCLUDE DDLCB                                                               
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037ACREPCF02 01/20/03'                                      
         END                                                                    
