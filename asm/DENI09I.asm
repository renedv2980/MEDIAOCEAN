*          DATA SET DENI09I    AT LEVEL 070 AS OF 02/22/18                      
*PROCESS USING(WARN(15))                                                        
*PHASE DENI09IA                                                                 
************************************************************                    
* DIFFER FROM WEEKLY SYNDICATION IN STATION = ITN%S                             
* WHERE % = DAY OF WEEK.                                                        
* UNIVERSES ARE READ FROM FILE IN A LOOP FOR THE ENTIRE                         
* MONTH, AND ARE USED ACCORDINGLY.                                              
************************************************************                    
         TITLE 'INPUT PHASE (VARIOUS) WEEKLY SYNDICATION CONVERSION'            
* Y=RAW IMPRESSIONS Z=RAW PUTS(PROGRAM) W=RAW PUTS(TIME PERIOD)                 
* F=DISPLACEMENT FILLER B=GROSS AVERAGE AUDIENCE                                
* O=CALCULATED PUTS                                                             
VHWC18   EQU   K0521               VHWC18                                       
RHWC18   EQU   K0621               RHWC18                                       
IHWC18   EQU   K0633               IHWC18                                       
GAHOMES  EQU   K1501-K0000+4       BWW55+                                       
AAHOMES  EQU   K0533-K0000         RHOMES                                       
TPPUT    EQU   K1197-K0000         WW25                                         
PUTSTRT  EQU   K0821-K0000         FIRST O                                      
PUTEND   EQU   K1041-K0000+4       LAST O BEFORE HOMES                          
TPHUTR   EQU   K1045-K0000         OHOMES                                       
GAIMPS   EQU   K1353-K0000         BW25                                         
AAIMPS   EQU   K0193-K0000         YW25                                         
*                                                                               
GAAIMP2  EQU   K1505-K0000         V18+                                         
AAIMP2   EQU   K0349-K0000         M18+                                         
GAALN1   EQU   K0345-K0193         COPY UNTIL YHOMES                            
GAALN2   EQU   K0641-K0349         COPY AFTER YHOMES                            
*                                                                               
NUMCATS  EQU   38                                                               
         EJECT                                                                  
DENI09I  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DENI09I,RA                                                     
         USING DEMCOND,R8          R8 POINTS TO GLOBAL WORKING STORAGE          
         LH    R4,=Y(WORKD-DENI09I)                                             
         LA    R4,DENI09I(R4)                                                   
         USING WORKD,R4                                                         
*                                                                               
         L     RC,ARREC                                                         
         LA    RC,4(RC)            RC POINTS TO RATING SERVICE RECORD           
         SPACE 1                                                                
         L     R2,AIREC            R2 POINTS TO INTERIM RECORD - SORT           
         USING INTERD,R2           KEY, VALUES, AND DEMO VALUES                 
         SPACE 1                                                                
         B     *+4(R1)             ROUTINE HOOK                                 
         SPACE 1                                                                
         B     READ                PROCESS INPUT TAPE                           
         B     CNVWR               SORT RECORD HOOK                             
         B     MORET               E-O-F ON INPUT                               
         B     EXIT                                                             
         EJECT                                                                  
READ     DS    0H                                                               
         CLI   INTAPESW,1          TEST FOR OPENING INPUT TAPE                  
         BE    READ20                                                           
         MVI   NOTAVAL,0                                                        
         L     RE,=A(PRGAVG)                                                    
         ST    RE,ACOMWRK          PASS ON TO OUTPUT PHASE                      
         MVI   IPGAASW,C'N'                                                     
         MVI   BYPREAD,X'FF'       SET TO 1ST-TIME-THRU                         
         OPEN  (IN1,(INPUT))                                                    
         CLI   RELOFRST,1          TEST FOR RELOCATED DTF ADDRESS               
         BNE   READ20                                                           
         L     RE,VPUTBUFF         CLEAR THE HPT SAVE AREA                      
         L     RF,HPTSAVLN                                                      
         XCEF                                                                   
         MVI   RELOFRST,0                                                       
         SPACE 1                                                                
READ20   DS    0H                                                               
         CLI   BYPREAD,C'H'        HPT RELEASE MODE                             
         BE    HPTGEN                                                           
         L     R3,ARREC                                                         
         XC    0(4,R3),0(R3)       INSERT VARIABLE LENGTH RECORD                
         MVC   0(2,R3),=H'401'     HEADER                                       
         LA    R3,4(R3)                                                         
         CLI   BYPREAD,1           TEST FOR BYPASSING INPUT READ                
         BE    READ40                                                           
*                                                                               
         GET   IN1,(R3)            READ NEXT RECD                               
         USING MIREC,RC                                                         
         CLI   GAASW,C'P'          IGNORE ALL GAA ON -NTI-                      
         BNE   READ25                                                           
         CLC   MISEQ(2),=C'04'                                                  
         BNE   READ30                                                           
         CLI   MIESTYPE,C'2'       GAA?                                         
         BE    READ20              YES, BYPASS RECD                             
         B     READ30                                                           
*                                                                               
READ25   MVI   GAASW,C'N'          DEFAULT IS NON-GAA DATA                      
         CLC   MISEQ(2),=C'04'     CHECK GAA FOR DATA                           
         BNE   READ30                                                           
*                                                                               
         CLI   MIESTYPE,C'2'                                                    
         BNE   READ30                                                           
         MVI   GAASW,C'Y'                                                       
*                                                                               
READ30   CLI   BYPREAD,X'FF'       TEST FOR 1ST-TIME-THRU                       
         BNE   READ40                                                           
         CLC   MISEQ,=C'00'        TEST FOR REPORT RECORD                       
         BE    *+6                                                              
         DC    H'0'                WRONG TAPE                                   
         CLI   MITYPE,C' '         DESCRIPTOR                                   
         BE    B0RTN                                                            
         DC    H'0'                WRONG TAPE                                   
*                                                                               
READ40   DS    0H                                                               
*                                                                               
READ41   ICM   RF,15,ACOMFACS                                                   
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,VWTYPTTB  GET A(VIEWING TYPES DESCRIPTION)             
         ICM   R5,15,DMCB          A(TABLE) RETURNED IN P1                      
         BNZ   *+6                 TEMP                                         
         DC    H'0'                BAD TABLEID PASSED                           
         USING VWTYPTD,R5                                                       
READ41A  CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                VIEWING TYPE NOT FOUND                       
         CLC   MIVWTYP,VWTYPE                                                   
         BE    *+12                                                             
         LA    R5,VWTYPTL(R5)                                                   
         B     READ41A                                                          
         MVC   KEYSRC,VWTKSRC      SAVE KEY SOURCE HERE                         
         MVC   SVVCR,VWTVCR                                                     
*                                                                               
         CLC   SAVEPNUM,MINUM      BREAK ON PROGRAM NUMBER                      
         BE    READ45                                                           
         MVC   SAVEPNUM,MINUM                                                   
         MVI   BYPASS01,0                                                       
         MVI   VARIOUS,0                                                        
         MVC   VARS,ZEROS                                                       
         MVC   DAYS,ZEROS                                                       
         XC    SAVVAR(SAVVARLQ),SAVVAR                                          
*                                                                               
READ45   CLI   BOOKTYPE,C'F'       TEST BOOKTYPE(WGHT)                          
         BE    *+12                                                             
         CLI   BOOKTYPE,C'I'       TEST BOOKTYPE                                
         BNE   READ46                                                           
         CLI   SVVCR,C'1'          CONVERT NON-VCR (INTEGRATED (SIC))           
         B     READ47                                                           
READ46   CLI   SVVCR,C' '          CONVERT VCR (ASCRIBED)                       
READ47   BNE   READ20                                                           
         CLC   MISEQ,=C'04'        TEST FOR PROGRAM HH EST RECORDS              
         BNE   READ70                                                           
         CLI   HPTGENSW,C'Y'       FIRST CREATE HPT RECORDS                     
         BE    HPTGEN                                                           
*                                                                               
         CLC   MIDYSWKS,=C'00'                                                  
         BNE   *+8                                                              
         MVI   BYPASS01,0                                                       
         CLI   BYPASS01,1          BYPASS SINGLE DAY AVERAGES                   
         BE    READ20                                                           
         CLI   MIORIG,C'0'         ***TEMP***                                   
         BE    READ48              *        *                                   
         CLI   MIORIG,C'1'         *        *                                   
         BNE   READ20              *        *                                   
         CLC   MINET(3),=C'XYZ'    *        *                                   
         NOP   READ20              *        *                                   
READ48   EQU   *                   ***TEMP***                                   
         CLI   MITYPE,C'D'         DESCRIPTOR                                   
         BE    D4RTN                                                            
         CLI   NOTAVAL,1           DATA NOT AVAILABLE                           
         BE    READ20              READ NEXT RECORD                             
         CLI   MITYPE,C'H'         HALF HOUR DETAIL                             
         BE    H4RTN                                                            
         CLI   MITYPE,C'P'         PERSONS ESTIMATES                            
         BNE   READ20                                                           
         MVI   BYPASSH4,0          RESET H4 RECORD BYPASS SWITCH                
         CLI   NOTAVAL,2           BYPASS 1/2HR P-RECDS (DNA)                   
         BE    READ20              NO DATA ON RECD, BYPASS THEM                 
         B     P4RTN               NO, PROCESS IT                               
*                                                                               
READ70   DC    H'0'                                                             
*                                                                               
READ90   MVI   BYPREAD,0                                                        
         MVI   INTAPESW,X'40'      DROP RECORD                                  
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
*B0RTN - REPORT DESCRIPTOR RECORD                                               
*********************************************************************           
B0RTN    DS    0H                                                               
         USING B0REC,RC                                                         
         BAS   RE,DAYRTN           SET UP DAY TABLE                             
         MVC   HDAYTABS,HDAYTAB    SAVE DAY TABLE                               
         BRAS  RE,B0NTR                                                         
         B     EXIT                                                             
*                                                                               
*********************************************************************           
*HPTGEN -GENERATE TIME PERIOD RECDS                                             
*********************************************************************           
HPTGEN   DS    0H                                                               
         DC    H'0'                                                             
         CLI   HPTFRST,1                                                        
         BNE   HPTGEN1                                                          
         BAS   RE,HPTMFC           CALC REQUIRED M-F AVERAGES                   
         BAS   RE,HPTMSC           CALCULATE M-S AVERAGES                       
         MVI   HPTFRST,0                                                        
         MVC   HPTNOW,VPUTBUFF                                                  
         MVI   QHRSW,0                                                          
*                                                                               
HPTGEN1  L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
*********FOR TESTING**********************                                      
*        L     R3,HPTNOW                                                        
*        USING HPTD,R3                                                          
*        SR    RE,RE                                                            
*        IC    RE,HPTDY                                                         
*        SRL   RE,4                                                             
*        STC   RE,P                                                             
*        MVC   P+1(1),HPTDY                                                     
*        OC    P(2),=C'00'                                                      
*        EDIT  (B1,HPTQHR),(2,P+3)                                              
*        EDIT  HPTAVEI,(9,P+6)                                                  
*        EDIT  HPTAVER,(3,P+16)                                                 
*        EDIT  HPTQH1I,(9,P+20)                                                 
*        EDIT  HPTQH2I,(9,P+30)                                                 
*        GOTO1 VPRINTER                                                         
* **************************************************                            
HPTGEN2  L     R3,HPTNOW                                                        
         USING HPTD,R3                                                          
         CLC   HPTCNT,=H'431'                                                   
         BH    HPTGEND                                                          
         MVC   INTBOOK,HUTBOOK                                                  
         MVC   INTIBOOK,HUTIBOOK                                                
         MVC   INTSTA,HUTSTA                                                    
         MVC   INTDAYWK,HPTDY                                                   
         MVC   INTSQH,HPTQHR                                                    
         MVC   INTEQH,HPTQHR                                                    
         MVI   INTDTYP,0                                                        
         CLI   HPTFLAG,C'1'                                                     
         BNE   *+8                                                              
         MVI   INTDTYP,X'80'                                                    
         MVC   INTACCS(L'HPTPUT),HPTPUT                                         
         LA    RE,K0533-K0000(R2)   SET HOMES VALUES                            
         MVC   0(4,RE),HPTQH1R                                                  
         MVC   4(4,RE),HPTQH1I                                                  
         CLI   QHRSW,1                                                          
         BNE   HPTGEN3                                                          
         MVC   0(4,RE),HPTQH2R                                                  
         MVC   4(4,RE),HPTQH2I                                                  
         ZIC   RE,INTSQH                                                        
         LA    RE,1(RE)                                                         
         STC   RE,INTSQH                                                        
         STC   RE,INTEQH                                                        
         MVI   QHRSW,0             SET UP FOR FIRST QHR                         
         LH    RF,HPTCNT                                                        
         LA    RF,1(RF)                                                         
         STH   RF,HPTCNT                                                        
         LA    R3,HPTLN(R3)                                                     
         ST    R3,HPTNOW                                                        
         B     HPTGEN4                                                          
*                                                                               
HPTGEN3  MVI   QHRSW,1             SET UP FOR SECOND QTR HR                     
*                                                                               
HPTGEN4  MVI   INTDUR,1            QTR HOUR DURATION                            
         MVC   INTMRKT,=H'1'                                                    
         MVI   INTMTYP,1                                                        
         MVI   INTPNAME,C' '                                                    
         MVC   INTPNAME+1(L'INTPNAME-1),INTPNAME                                
         MVI   INTTITLE,C' '                                                    
         MVC   INTTITLE+1(L'INTTITLE-1),INTTITLE                                
         XC    INTPNTI,INTPNTI                                                  
*                                                                               
         MVI   INTRTYP,PRCODEQU    -P-                                          
         LA    R7,INTKEY                                                        
         USING PRKEY,R7                                                         
         MVI   PRCODE,PRCODEQU     BUILD PAV KEY                                
         MVI   PRMED,C'N'                                                       
         MVI   PRSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
         MVC   PRSTAT,INTSTA                                                    
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRDW,INTDAYWK                                                    
         MVC   PRSTIM,INTSQH                                                    
         MVI   INTKEY+29,255       SET LAST RECORD FLAG                         
         MVI   BYPREAD,C'H'                                                     
         MVI   HPTGENSW,C'N'                                                    
         B     EXIT                                                             
*                                                                               
HPTGEND  MVI   BYPREAD,1                                                        
         B     READ20                                                           
         DROP  R3,R7                                                            
                                                                                
         EJECT                                                                  
*********************************************************************           
*D4RTN - PROGRAM DESCRIPTOR RECORD D-P-P                                        
*********************************************************************           
*                                                                               
D4RTN    DS    0H                                                               
         USING D4REC,RC                                                         
         CLC   D4MKTBRK,=C'000'    RESET AVAILABILITY ON PRIME REC              
         BNE   *+8                                                              
         MVI   NOTAVAL,0                                                        
         CLI   NOTAVAL,1           BYPASS EXTRA DESC. RECS                      
         BE    D4RX                WHICH MAY BE MISSING IND                     
         CLI   D4NAVAL,C'1'        N/A FLAG                                     
         BNE   *+12                                                             
         MVI   NOTAVAL,1                                                        
         B     D4RX                                                             
*                                                                               
         IF (CLC,D4SYNID,NE,=C'ITN '),AND,                                      
            (CLC,D4SYNID,NE,=C'SIN ')                                           
           J  *+2                  UNEXPECTED SYNDICATOR IN MIT FILE            
         ELSEIF (CLC,D4SYNID,NE,IPHSPARM) IPHSPARM= VALUE MATCHES MIT ?         
           J  *+2                  NO: TRAP THIS                                
         ENDIF                                                                  
*                                                                               
         CLC   D4MKTBRK,=C'350'    ANY UNDER 18                                 
         BE    D350RTN                                                          
         CLC   D4MKTBRK,=C'351'    ANY UNDER 12                                 
         BE    D351RTN                                                          
         CLC   D4MKTBRK,=C'352'    ANY UNDER 6                                  
         BE    D352RTN                                                          
         XC    HWCAREA,HWCAREA                                                  
*                                                                               
         CLI   D4ORIG,C'0'         TEST CORRECTION RECORD                       
         BE    D4R01               NO                                           
         DC    H'0'                NOT YET                                      
         MVI   CORRSW,1            SET CORRECTION SWITCH                        
         MVC   INTPNO,BOOK1S       ORIGINATING BOOK FOR CORRECTIONS             
         BAS   RE,DAYRTN           SET UP DAY TABLE                             
         PACK  DUB,D4END+1(2)      ***TEMP*** CONVERT YEAR (YY)                 
         CVB   R0,DUB              ***TEMP***                                   
         STC   R0,HALF             ***TEMP***                                   
         GOTO1 VNETWEEK,DMCB,D4END+1,VGETDAY,VADDAY                             
         MVC   HALF+0(1),4(R1)     YEAR                                         
         MVC   HALF+1(1),8(R1)     WEEK (NETWORK)                               
         MVC   BOOK1,HALF          KEY BOOK ALLOWS BLACK WEEK                   
         MVC   IBOOK1,HALF         WEEK TO PREVIOUS REGULAR WEEK                
*                                                                               
D4R01    MVC   SVPROJ,ZEROS        RESET H4 RECORD'S SAVE AREAS                 
         MVC   SVRTG,ZEROS                                                      
         MVC   SVSHR,ZEROS                                                      
*                                                                               
         CLC   D4TOTDUR,=C'000000' ARE TOTAL FIELDS ON THIS RECORD              
         BE    D4R04                                                            
         MVC   SVD4,D4STA          IF YES - SAVE POS.305-363                    
         LA    RE,DBUFF                                                         
         LA    RF,DBUFFQ                                                        
         XCEF                                                                   
*                                                                               
D4R04    MVC   D4STA(L'SVD4),SVD4  IF NOT - RESTORE FROM A PREVIOUS D4          
         XC    SAVETIME(56),SAVETIME                                            
         MVI   GOSUBN,SETDB#       SET DBUFFER AND SAVETIME                     
         GOTO1 =A(SUBR2),DMCB,(RC)                                              
*                                                                               
         CLC   D4DYSWKS,=C'00'     TEST AVERAGES                                
         BE    *+6                                                              
         DC    H'0'                NO AVERAGES                                  
*        CLI   D4BREAK,C'1'        BREAKOUT                                     
*        BE    D4R10                                                            
         LA    RE,D4DAYS                                                        
         LA    RF,DAYS             SET ON POINTERS FOR INDIVIDUAL DAYS          
         LA    R0,7                (NEEDED FOR PROCESSING VARIOUS)              
         SR    R5,R5               COUNTER                                      
D4R05    CLI   0(RE),C'1'                                                       
         BNE   D4R05A                                                           
         MVI   0(RF),C'1'                                                       
         LA    R1,8                                                             
         SR    R1,R0                                                            
         STC   R1,ITNDAY           MON=1,TUE=2,WED=3,...,SUN=7                  
         LA    R5,1(R5)            UPDATE COUNTER                               
*                                                                               
D4R05A   LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,D4R05                                                         
         CHI   R5,1                                                             
         BE    *+6                                                              
         DC    H'0'                ONLY ONE DAY SHOULD BE ACTIVE                
         B     D4R10                                                            
*                                                                               
D4R06    CLC   D4DAYS,=C'1111100'  M-F                                          
         BE    D4R10                                                            
         CLC   D4DAYS,=C'1111111'  M-S                                          
         BE    D4R10                                                            
         CLC   D4NET(3),=C'SYN'    SYND HAVE NO INDIV. DAY                      
         BNE   *+12                                                             
         MVI   VARIOUS,1           SO FUDGE IT HERE                             
         B     D4R07                                                            
*                                  VARIOUS                                      
         CLI   VARIOUS,1                                                        
         BE    D4R07                                                            
         MVI   VARIOUS,1                                                        
         MVC   VARS,ZEROS          1ST-TIME-THRU                                
         XC    VARSTIME(56),VARSTIME                                            
*                                                                               
         LA    RE,DAYS             TEST NUMBER OF DAYS IN AVERAGE               
         SR    RF,RF                                                            
         LA    R0,7                                                             
         CLI   0(RE),C'1'                                                       
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-16                                                          
         CH    RF,=H'1'            BYPASS SINGLE DAY AVERAGES                   
         BH    D4R07                                                            
         MVI   VARIOUS,0                                                        
         OC    INTVAR(SAVVARLQ),INTVAR   DO WE ALREADY HAVE A VAR               
         BNZ   D4RX                                                             
         MVI   BYPASS01,1                                                       
         B     D4RX                                                             
*                                                                               
D4R07    LA    RE,D4DAYS                                                        
         LA    RF,VARS             SET ON POINTERS FOR VARIOUS DAYS             
         LA    R0,7                                                             
D4R08    CLI   0(RE),C'1'                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'1'                                                       
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,D4R08                                                         
         CLC   D4NET(3),=C'SYN'    NO INDIV. DAYS FO SYND.                      
         BNE   *+10                                                             
         MVC   DAYS,VARS           SO FORCE THIS                                
*                                                                               
D4R10    XC    PVALS,PVALS                                                      
         GOTO1 VNTIPRG,DMCB,=C'LKUP',(0,VBITMAP1),D4NUM                         
         MVC   PNUM,0(R1)          PNUM=DDS INTERNAL NUMBER                     
         MVC   PACK16(10),D4NUM                                                 
         MVI   PACK16+10,C'0'                                                   
         PACK  DUB,PACK16(11)                                                   
         MVC   INTPNTI,DUB+2       5 CHAR PWOS                                  
         MVC   PNTINUM,INTPNTI     PNTINUM= NEILSENS NTI PRG NUMBER             
*                                                                               
*        PACK  DUB,D4NUM+5(5)      PROGRAM NUMBER                               
*        CVB   R0,DUB                                                           
*        STCM  R0,3,PNUM                                                        
*                                                                               
         MVI   PDPT,0              PDPT IS ALWAYS ZERO                          
         MVC   PDPT2,D4DPT                                                      
         MVC   PNET,D4NET          SET UP R/S CALL LETTERS                      
         MVI   PNET+3,C' '                                                      
*        CLC   D4NET,=C'AGG'       NON NETWORK SOURCES                          
*        BNE   NOTAGG                                                           
*        PACK  DUB,D4NUM+5(2)                                                   
*        CVB   R0,DUB                                                           
*        STC   R0,PNET+3                                                        
*                                                                               
NOTAGG   CLC   D4NET(3),=C'SYN'       SYNDICATION                               
         BNE   *+10                                                             
         MVC   PNET(3),D4SYNID     SET SYNDICATION CALL LETTERS                 
         MVC   PNAME,D4NAME                                                     
         MVC   PTITLE,D4TITLE                                                   
         MVC   PTYP,D4ALPHA                                                     
         MVC   SUBPTYP,D4SUBPTY                                                 
         MVC   PPREM,D4PREM                                                     
         MVC   INTMRKT,=H'1'       SET NETWORK MARKET TO 1                      
         CLC   D4DYSWKS,=C'01'                                                  
         BNE   D4R18                                                            
         L     R1,=A(PRGAVG)           PUT PRG IN BUFFER                        
D4R15    OC    0(4,R1),0(R1)                                                    
         BZ    D4R16               ADD PRG NUMBER TO LIST                       
         CLC   0(2,R1),=X'FFFF'                                                 
         BNE   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         CLC   INTSTA(3),0(R1)                                                  
         BNE   *+14                                                             
         CLC   PNUM,4(R1)                                                       
         BE    D4R18               PRG ALREADY IN TABLE                         
*                                                                               
         LA    R1,L'PRGAVG(R1)                                                  
         B     D4R15                                                            
*                                                                               
D4R16    MVC   0(4,R1),INTSTA                                                   
         MVC   4(2,R1),PNUM                                                     
         MVI   6(R1),X'90'         ASSUME VARIOUS DAYS                          
         CLC   D4DAYS,=X'1111100'                                               
         BNE   *+8                                                              
         MVI   6(R1),X'00'         M-F                                          
         CLC   D4DAYS,=X'1111111'                                               
         BNE   *+8                                                              
         MVI   6(R1),X'80'         M-S                                          
         LA    R1,L'PRGAVG(R1)                                                  
         CLC   0(2,R1),=X'FFFF'                                                 
*********BE    *+14                *** BAD BRANCH REMOVED BY DEIS               
         JE    *+2                 DEIS DOESN'T KNOW WHAT ELSE TO DO            
         XC    0(2,R1),0(R1)       SET NEXT SLOT TO ZERO                        
*                                                                               
D4R18    CLC   D4NET(3),=C'SYN'                                                 
         BE    *+6                                                              
         DC    H'0'                CANNOT IDENTIFY:NET/NEW NET/NEW AGG          
         MVC   INTSTA(3),D4SYNID   IF SYN JUST MOVE IN WHAT WAS GIVEN           
*        MVI   INTSTA+3,C' '       BLANK PAD                                    
         MVC   INTSTA+3(1),ITNDAY  STATION WILL BE 'ITN(DAY)S'                  
         B     *+10                                                             
*                                                                               
D4GSTA   MVC   INTSTA,4(RE)                                                     
D4GSTAB  MVI   INTSTA+4,C'T'       FOR REG NET & AGG 'T'                        
         CLC   D4NET(3),=C'SYN'                                                 
         BNE   *+8                                                              
         MVI   INTSTA+4,C'S'       SYND IS C'S'                                 
*                                                                               
         PACK  DUB,D4PROJ          PROJECTION (XXX,XXX,XXX)                     
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         AH    R1,=H'5000'         ROUND TO TEN THOUSANDS                       
         D     R0,=F'10000'                                                     
         STCM  R1,3,PWK1AUD        REACH                                        
*                                                                               
         PACK  DUB,D4STA                                                        
         CVB   R1,DUB                                                           
         STCM  R1,3,PWK1STAC       STATION COUNT                                
*                                                                               
         PACK  DUB,D4COV                                                        
         CVB   R1,DUB                                                           
         STCM  R1,3,PWK1COV        COVERAGE                                     
*                                                                               
         TM    D4AVEHUT+(L'D4AVEHUT-1),X'F0'                                    
         BO    *+10                                                             
         MVC   D4AVEHUT,=16C'0'                                                 
         PACK  DUB,D4AVEHUT                                                     
         CVB   RF,DUB                                                           
         ST    RF,SVAVGHI          SAVE AVG HOMES IMPS                          
*                                                                               
         L     RE,=A(TYPTAB)       POINT RE AT TELECAST TYPE TABLE              
         LA    R0,TYPES            COUNTER                                      
D4R20    CLC   D4SPEC,0(RE)        0=REGULAR   1=SPECIAL                        
         BNE   *+14                                                             
         CLC   D4REPEAT,1(RE)      BLANK=ORIGINAL   Y=REPEAT                    
         BE    D4R30                                                            
*                                                                               
         LA    RE,L'TYPTAB(RE)                                                  
         BCT   R0,D4R20                                                         
         DC    H'0'                BLOW UP IF TYPE IS NOT IN TABLE              
D4R30    MVC   PWK1DTYP,2(RE)      EXTRACT CORRESPONDING BIT SETTING            
*                                                                               
         CLI   INTSTA+4,C'S'       SYNDICATOR                                   
         BNE   D4R31                                                            
         CLI   D4TTYPE,C'C'                                                     
         BNE   *+8                                                              
         OI    PWK1DTYP,X'18'      ORIG+REPEAT PROGRAMMING                      
         CLI   D4TTYPE,C'M'                                                     
         BNE   *+8                                                              
         OI    PWK1DTYP,X'20'      MULTIPLE PROGRAMS                            
*                                                                               
D4R31    CLI   D4OPT,C'1'          TEST OPTIONAL DATA                           
         BNE   D4R34                                                            
* NOTE - EARLY TAPES OF SEP/87 HAVE ERROR FOR NON-VCR (INTEGRATED)              
         CLI   SVVCR,C' '          TEST VCR                                     
         BE    D4R33                                                            
         CLC   D4DYSWKS,=C'01'     TEST AVERAGES                                
         BE    D4R34                                                            
         CLI   D4MULTI,C' '        TEST MULTI-DAYS                              
         BE    D4R34                                                            
D4R33    OI    PWK1DTYP,X'80'      SET OPTIONAL BIT IN PHTDTYP                  
*                                                                               
D4R34    MVI   PWK1RSF,0                                                        
*        CLI   D4BREAK,C'1'        TEST FOR REDUCED STATION                     
*        BNE   *+8                                                              
*        MVI   PWK1RSF,X'01'       REDUCED STATION INDICATOR                    
         MVC   INTDPT,PDPT         PDPT IS ALWAYS ZERO                          
         MVC   INTDPT2,PDPT2                                                    
         MVC   INTPREM,PPREM                                                    
         MVC   INTPNUM,PNUM                                                     
         MVC   INTPTYP4,PTYP                                                    
         MVC   INTSPTYP,SUBPTYP                                                 
         MVC   INTPNAME,PNAME                                                   
         MVC   INTTITLE,PTITLE                                                  
         MVI   INTSTYP,0           DEFAULT TO REG PROGRAM                       
*        CLI   D4BREAK,C'1'        BREAKOUT?                                    
*        BNE   *+8                                                              
*        MVI   INTSTYP,C'B'                                                     
*        CLI   D4SPEC,C'1'         SPECIAL?                                     
*        BNE   *+8                                                              
*        MVI   INTSTYP,C'S'                                                     
         MVI   INTMTYP,1           METRO A MARKET                               
*        MVC   INTBOOK,BOOK1       WEEK VALUES                                  
*        MVC   INTIBOOK,IBOOK1                                                  
         MVC   INTAUD,PWK1AUD                                                   
         MVC   INTSTAC,PWK1STAC                                                 
         MVC   INTCOV,PWK1COV                                                   
         MVC   INTDTYP,PWK1DTYP                                                 
         MVC   INTRSF,PWK1RSF                                                   
*                                                                               
         GOTO1 VNETWEEK,DMCB,D4START+1,VGETDAY,VADDAY                           
         CLC   D4START,D4END                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   HALF(1),4(R1)       YEAR                                         
         MVC   HALF+1(1),8(R1)     WEEK                                         
         MVC   INTBOOK,HALF                                                     
         MVC   INTIBOOK,HALF                                                    
*                                                                               
D4R40    LA    RE,D4DAYS           POINT TO DAY FIELDS                          
D4R42    LA    R0,7                COUNTER                                      
         LA    R5,X'40'            START AT MONDAY                              
         SR    RF,RF               CLEAR CUMULATIVE BITS                        
*                                                                               
D4R50    CLI   0(RE),C'0'          TEST FOR NO ACTIVITY                         
         BE    D4R60                                                            
         CLI   0(RE),C'1'          TEST FOR PROGRAM RAN                         
         BNE   D4R60                                                            
         OR    RF,R5               UPDATE CUMULATIVE BITS                       
*                                                                               
D4R60    LA    RE,1(RE)                                                         
         SRL   R5,1                NEXT DAY                                     
         BCT   R0,D4R50                                                         
*                                                                               
         STC   RF,INTDAYWK         SAVE BITS                                    
         MVI   BYTE,X'90'          SET INTERNAL DAY CODE TO VAR                 
         L     RE,=A(NETDAYTB)                                                  
         LA    R0,NETDAYS                                                       
         CLC   INTDAYWK,0(RE)      TEST BITS VS. TABLE                          
         BE    *+16                                                             
         LA    RE,L'NETDAYTB(RE)                                                
         BCT   R0,*-14                                                          
         B     D4R70                                                            
         MVC   BYTE,1(RE)          INTERNAL DAY CODE                            
D4R70    MVC   PDAY1,BYTE                                                       
         MVC   PDAY1BIT,INTDAYWK                                                
         GOTO1 PDATA,DMCB,SAVETIME,PDAY1BIT                                     
         MVI   INTRTYP,PMCODEQU    ***** -Q- RECD ******                        
         MVC   HALF,PWK1STIM                                                    
         MVC   INTSTIM,HALF        START TIME                                   
         MVC   INTDURM,PWK1DURM    SHORT DURATION IN MINUTES (MAX 240)          
         MVC   INTDURM2,PWK2DURM   LONG DURATION IN MINUTES                     
         GOTO1 VHRTOQH,DMCB,HALF,INTSQH       START QH                          
*                                                                               
*        DEVELOP END QH (INTEQH) AND DURATION IN QH'S (INTDUR)                  
*                           INTDUR = 1 ( 1-22 MIN.)                             
*                                  = 2 (23-37 MIN.)                             
*                                  = 3 (38-52 MIN.)...ETC.                      
         ZIC   R0,INTSQH                                                        
         SR    RF,RF                                                            
         ICM   RF,3,PWK2DURM       DURATION IN MINUTES                          
         LA    RF,8(RF)            ADD 8 BEFORE CNV TO QH                       
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         CH    RF,=H'1'            TEST FOR DURATION OF AT LEAST ONE            
         BH    *+8                                                              
         LA    RF,1                                                             
         STC   RF,INTDUR           DURATION IN QH                               
         AR    R0,RF               SQH PLUS DUR                                 
         BCTR  R0,0                LESS ONE GIVE END QH                         
         STC   R0,INTEQH                                                        
*                                                                               
*              ADD DURATION IN MINUTES TO START TIME (MILITARY) TO              
*              GET END TIME                                                     
         SR    R0,R0                                                            
         LH    R1,HALF             START TIME                                   
         D     R0,=F'100'          MINUTES IN R0, HOURS IN R1                   
         SR    RF,RF                                                            
         ICM   RF,3,INTDURM2       DURATION IN MINUTES                          
         SR    RE,RE                                                            
         D     RE,=F'60'           MINUTES IN RE, HOURS IN RF                   
         AR    R0,RE               ADD MINUTES TOGETHER                         
         CH    R0,=H'60'           TEST IF SUM GT OR EQ TO 1 HOUR               
         BL    *+12                NO                                           
         SH    R0,=H'60'           SUBTRACT 60 MINUTES                          
         LA    RF,1(RF)            AND ADD 1 TO HOURS                           
         AR    R1,RF               ADD HOURS TOGETHER                           
         MH    R1,=H'100'          HOURS X 100 FOR MILITARY TIME                
         AR    R1,R0               ADD MINUTES TO HOURS                         
         CH    R1,=H'2400'         TEST FOR PAST MIDNIGHT                       
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         STCM  R1,3,INTETIM        END TIME                                     
*                                                                               
         CLC   D4NET(3),=C'SYN'    NO INDIV. DAY FOR SYNDS.                     
         BE    *+20                                                             
         CLC   D4DYSWKS,=C'01'     AVERAGES                                     
         BE    D4R77                                                            
         MVC   VARS,ZEROS                                                       
*                                  USE INDIVIDUAL DAY(S) FOR INTVAR             
         LA    RE,D4DAYS                                                        
         LA    RF,SAVVAR           SET UP START TIMES IN VAR SAVE AREA          
         USING INTVARD,RF                                                       
         LA    R0,7                                                             
D4R74    CLI   0(RE),C'1'                                                       
         BNE   D4R76                                                            
         MVC   INTVSQ,INTSQH       START QH                                     
         MVC   INTVEQ,INTEQH       END QH                                       
         PACK  DUB,D4DUR                                                        
         CVB   R1,DUB                                                           
         STCM  R1,3,INTVLDUR       LONG DURATION IN MINUTES                     
*                                                                               
         CH    R1,=H'240'          4-HOUR MAXIMUM SUPPORTED                     
         BNH   *+8                                                              
         LH    R1,=H'240'                                                       
         STC   R1,INTVSDUR         SHORT DURATION IN MINUTES (MAX 240)          
*                                                                               
         MVC   INTVSTIM,INTSTIM    START TIME                                   
         MVC   INTVETIM,INTETIM    END TIME                                     
D4R76    LA    RE,1(RE)                                                         
         LA    RF,INTVARLQ(RF)                                                  
         BCT   R0,D4R74                                                         
         DROP  RF                                                               
*                                                                               
         CLC   D4NET(3),=C'SYN'    CAN BE VAR FOR SYND.                         
         BNE   D4R80                                                            
*                                                                               
D4R77    CLI   VARIOUS,1           VARIOUS                                      
         BNE   D4R80                                                            
         LA    RE,SAVETIME         ACCUMULATE INDIVIDUAL VAR DAY(S)             
         LA    RF,VARSTIME             INTO VARSTIME                            
         LA    R0,7                                                             
D4R78    OC    0(8,RE),0(RE)                                                    
         BZ    *+10                                                             
         MVC   0(8,RF),0(RE)                                                    
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,D4R78                                                         
*                                                                               
         CLC   VARS,DAYS           HAVE ALL VARIOUS DAYS PROCESSED              
         BNE   D4R90                                                            
D4R79    MVI   VARIOUS,0           RESET                                        
         MVC   DAYS,ZEROS          RESET                                        
         MVC   INTVAR(SAVVARLQ),SAVVAR                                          
         XC    SAVVAR(SAVVARLQ),SAVVAR                                          
         MVC   SAVETIME(56),VARSTIME                                            
         XC    VARSTIME(56),VARSTIME                                            
         LA    RE,VARS             GENERATE VAR DATA USING                      
         B     D4R42                   ACCUMULATED DAYS                         
*                                                                               
D4R80    LA    R1,CALVPH           SET CALVPH FIELDS                            
         USING CALVPHD,R1                                                       
         MVC   VPHDPT,D4DPT                                                     
         MVC   VPHPCOD,D4ALPHA                                                  
         MVI   VPHSPCL,C' '                                                     
*        CLI   D4SPEC,C'1'         1=SPECIAL                                    
*        BNE   *+8                                                              
*        MVI   VPHSPCL,C'S'                                                     
         MVC   VPHACTD,INTDYBIT                                                 
         CLI   VPHACTD,0                                                        
         BNE   *+8                                                              
         MVI   VPHACTD,X'40'                                                    
         MVI   VPHTELE,1                                                        
         MVC   VPHDUR,PWK2DURM                                                  
         DROP  R1                                                               
*                                                                               
         L     R7,ARREC                                                         
         LA    R7,4(R7)                                                         
         L     R6,AIREC                                                         
         BRAS  RE,FKPROC                                                        
*                                                                               
D4R90    MVC   INTDAYWK,PDAY1                                                   
         MVC   INTDYBIT,PDAY1BIT                                                
         MVC   SAVEINT,INTVALS     SAVE FOR H4RTN/P4RTN                         
*                                                                               
D4RX     MVI   BYPREAD,0           FOR P4REC, SKIP PROCESSING AND               
         B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
*                                                                               
D350RTN  LA    RE,HWCV18                                                        
         B     D35XRTN                                                          
*                                                                               
D351RTN  LA    RE,HWCV12                                                        
         B     D35XRTN                                                          
*                                                                               
D352RTN  LA    RE,HWCV06                                                        
         B     D35XRTN                                                          
*                                                                               
D35XRTN  TM    D4AVEHUT+(L'D4AVEHUT-1),X'F0'                                    
         BO    *+10                                                             
         MVC   D4AVEHUT,=16C'0'                                                 
         PACK  DUB,D4AVEHUT                                                     
         CVB   RF,DUB                                                           
         ST    RF,24(RE)                                                        
         TM    D4AVERTG+(L'D4AVERTG-1),X'F0'                                    
         BO    *+10                                                             
         MVC   D4AVERTG,=16C'0'                                                 
         PACK  DUB,D4AVERTG                                                     
         CVB   RF,DUB                                                           
         ST    RF,12(RE)                                                        
         SR    R0,R0                                                            
         L     R1,SVAVGHI          INDEX TO PROGRAM HOMES                       
         A     R1,=F'500'                                                       
         D     R0,=F'1000'                                                      
         ST    R1,FULL                                                          
         L     R1,24(RE)                                                        
         SR    R0,R0                                                            
         D     R0,FULL                                                          
         ST    R1,0(RE)                                                         
         B     READ20                                                           
         EJECT                                                                  
**********************************************************************          
*H4RTN - PROCESS HALF HOUR DETAIL RECORD                                        
**********************************************************************          
*                                                                               
H4RTN    DS    0H                                                               
         USING H4REC,RC                                                         
         CLI   BYPASSH4,1          BYPASS ALL BUT 1ST H4 RECORD                 
         BE    READ20                                                           
         MVI   BYPASSH4,1          FIRST H, BYPASS NEXT TIME                    
         MVI   NOTAVAL,0                                                        
         CLI   H4REPT,C'4'         WAS THE DATA PROVIDED?                       
         BNE   *+12                YES                                          
         MVI   NOTAVAL,2           NO, BYPASS 1/2HR PUTS                        
         B     READ20                                                           
*                                                                               
         MVC   INTVALS(L'SAVEINT),SAVEINT   RESTORE FROM D4RTN                  
         CLC   H4PROJ,ZEROS        ARE TOTAL FIELDS ON THIS RECORD              
         BE    *+22                                                             
         MVC   SVPROJ,H4PROJ       IF YES - SAVE 3 FIELDS                       
         MVC   SVRTG,H4RTG                                                      
         MVC   SVSHR,H4SHR                                                      
         MVC   H4PROJ,SVPROJ       IF NOT - RESTORE FROM A PREVIOUS H4          
         MVC   H4RTG,SVRTG                                                      
         MVC   H4SHR,SVSHR                                                      
*                                                                               
         MVI   INTRTYP,PRCODEQU    **** -P- RECD *****                          
         MVC   PACK16(10),H4NUM                                                 
         MVI   PACK16+10,C'0'                                                   
         PACK  DUB,PACK16(11)                                                   
         MVC   INTPNTI,DUB+2       5 CHAR PWOS                                  
*                                                                               
         PACK  DUB,H4EVDUR(4)      ACTUAL HALF HOUR DURATION                    
         CVB   RF,DUB                                                           
         STC   RF,INTADUR          DUR OF PRG W/IN THIS 1/2HR                   
         PACK  DUB,H4HALF          HALF HOUR CODE                               
         CVB   RF,DUB              CONVERT TO QHR                               
         BCTR  RF,0                DDS QHR=NTI QHR CODE -1                      
         SLL   RF,1                                                             
*                                                                               
         SR    R1,R1               CLEAR ACCUM                                  
         PACK  DUB,H4DUR1          MINUTES IN FIRST QH                          
         PACK  DUB1,H4DUR2         MINUTES IN SECOND QH                         
         CP    DUB,=P'0'           TEST IF RAN IN FIRST QH                      
         BE    *+12                NO                                           
         LA    R1,1(R1)            YES-BUMP QH DURATION                         
         B     *+8                                                              
         LA    RF,1(RF)            BUMP START QH                                
         CP    DUB1,=P'0'                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         LTR   R1,R1               TEST IF PROGRAM RAN DURING HALF HOUR         
*        BNZ   *+6                 YES                                          
*        DC    H'0'                                                             
*                                                                               
         BNZ   *+8                                                              
         LA    R1,1(R1)            JUST FORCE THE ISSUE                         
*                                                                               
         STC   RF,INTSQH           SET START QUARTER HOUR                       
         STC   R1,INTDUR                                                        
         AR    R1,RF               GET END QH                                   
         BCTR  R1,0                                                             
         STC   R1,INTEQH                                                        
         MVC   SAVEINT,INTVALS     SAVE FOR P4RTN                               
         L     R7,ARREC                                                         
         LA    R7,4(R7)                                                         
         L     R6,AIREC                                                         
         BRAS  RE,GKPROC                                                        
         MVI   BYPREAD,0           FOR P4REC, SKIP PROCESSING AND               
         B     READ20              READ NEXT REC W/O GOING TO CONTRLLER         
         EJECT                                                                  
**********************************************************************          
*P4RTN - PROCESS PERSONS ESTIMATES RECORD                                       
**********************************************************************          
P4RTN    DS    0H                                                               
         MVC   SVD4,ZEROS          RESET D4 RECORD'S SAVE AREA                  
         MVI   BYPASSH4,0          RESET H4 RECORD BYPASS SWITCH                
         USING P4REC,RC                                                         
*                                                                               
         MVC   INTVALS(L'SAVEINT),SAVEINT   RESTORE FROM D4RTN/H4RTN            
         L     R7,ARREC                                                         
         LA    R7,4(R7)                                                         
         L     R6,AIREC                                                         
         CLI   P4DEMTYP,C'1'                                                    
         BE    P4R20               GO HANDLE PRG PUTS                           
*                                                                               
         CLC   P4DEMGRP,=C'001'    NON-PUTS DEMOS                               
         BNE   P4R10                                                            
         MVI   HAVPP,0             RESET PROGRAM PUT SWITCH                     
         BRAS  RE,AKPROC           DEMOS 001-020                                
         B     P4R40               BUILD KEY                                    
P4R10    BRAS  RE,BKPROC           DEMOS 021-040                                
         B     P4RX                RELEASE RECD                                 
*                                                                               
P4R20    CLC   P4DEMGRP,=C'001'    PROGRAM PUTS                                 
         BNE   P4R30                                                            
         MVI   HAVPP,1             SET HAVE PROG. PUTS                          
         BRAS  RE,CKPROC           PRG PUTS DEMOS: 001-020                      
         B     P4R40               BUILD KEY                                    
P4R30    BRAS  RE,DKPROC           PRG PUTS DEMOS: 021-040                      
         B     P4RX                RELEASE RECD                                 
         EJECT                                                                  
**********************************************************************          
*P4R40 - BUILD KEY AND RELEASE RECD TO SORT                                     
**********************************************************************          
P4R40    CLI   INTRTYP,PMCODEQU    TEST FOR 'Q' RECORD                          
         BE    P4R70                                                            
*                                                                               
P4R60    LA    R7,INTKEY           BUILD PAV KEY                                
         USING PRKEY,R7                                                         
         MVI   PRCODE,PRCODEQU     *** -P- RECD *********                       
         MVI   PRMED,C'N'                                                       
         MVI   PRSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
         CLI   CORRSW,0            TEST CORRECTION RECORD                       
         BE    P4R64                                                            
         MVC   PRCODE(2),BOOK1     (BOOK FORCES PROPER SORT)                    
         MVI   PRSRC,PRCODEQU      -P-                                          
         MVC   INTKSRC,KEYSRC                                                   
P4R64    MVC   PRSTAT,INTSTA                                                    
         MVC   PRSTYP,INTSTYP                                                   
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRSTIM,INTSQH                                                    
         MVC   PRDW,INTDAYWK                                                    
         MVC   PRDW+1(1),INTDUR    FORCE HIGHER DURATIONS FIRST                 
         XI    PRDW+1,X'FF'                                                     
         B     P4RX                                                             
         DROP  R7                                                               
*                                                                               
P4R70    LA    R7,INTKEY           BUILD 'Q' RECORD KEY                         
         USING PMKEY,R7                                                         
         MVI   PMCODE,PMCODEQU     -Q-                                          
         MVI   PMMEDIA,C'N'                                                     
         MVI   PMSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
         CLI   CORRSW,0            TEST CORRECTION RECORD                       
         BE    P4R74                                                            
         MVC   PMCODE(2),BOOK1     (BOOK FORCES PROPER SORT)                    
         MVI   PMSRC,PMCODEQU      -Q-                                          
         MVC   INTKSRC,KEYSRC                                                   
P4R74    MVC   PMBOOK,INTBOOK                                                   
         MVC   PMSTAT,INTSTA                                                    
         MVC   PMSTYP,INTSTYP                                                   
         MVC   PMPNUM,INTPNUM                                                   
         L     RE,=A(QSORTAB)          SORT 'Q' RECORDS BY DAY...               
         LA    R0,QSORTABS         M-S, M-F, VAR, MON...SUN                     
         CLC   INTDAYWK,0(RE)                                                   
         BE    *+14                                                             
         LA    RE,L'QSORTAB(RE)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         MVC   PMRLEN(1),1(RE)     USE FIELD FOLLOWING PMPNUM                   
         DROP  R7                                                               
*                                                                               
P4RX     MVI   BYPREAD,0                                                        
         MVC   1900(4,R2),=F'1'    FORCE LONG SORT RECD TO ADD TP PUTS          
*                                   AND GAA FIGURES                             
         CLC   P4DEMGRP,=C'001'    DEMOGRAPHIC GROUP                            
         BE    READ20              FOR 2ND P4REC, SKIP PROCESSING AND           
*                                  READ NEXT REC W/O GOING TO CONTRLLER         
         CLI   HAVPP,0             HANDLE MISSING PROG. PUTS                    
         BE    *+10                 NTI MUCKS IT UP BY NOT PROVIDING            
*                                   THEM ON A RANDOM BASIS                      
         SPACE 1                                                                
         CLC   P4NET(3),=C'SYN'    NO PROG PUTS FOR SYN                         
         BE    *+12                                                             
         CLI   P4DEMTYP,C'1'       PUT OUT AFTER PROGRAM PUTS                   
         BNE   READ20                                                           
*                                                                               
         LA    R1,CALVPH           CALL CALVPH                                  
         USING CALVPHD,R1                                                       
         LA    RE,INTACCS                                                       
         ST    RE,VPHDEMS                                                       
         L     RE,=A(INTAB)                                                     
         ST    RE,VPHINT                                                        
         GOTO1 VCALVPH,CALVPH                                                   
         L     R7,AIREC                                                         
         USING K0000,R7                                                         
         MVC   VHWC18(12),HWCV18                                                
         MVC   RHWC18(12),HWCR18                                                
         MVC   IHWC18(12),HWCI18                                                
         XC    HWCAREA,HWCAREA                                                  
         DROP  R7                                                               
         DROP  R1                                                               
*                                                                               
P4RX20   MVI   INTKEY+29,255                                                    
         CLI   GAASW,C'Y'                                                       
         BNE   EXIT                                                             
         MVI   INTKEY+29,C'G'                                                   
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*********************************************************************           
* CNVWR- SORT RECORD HOOK. ALL PROCESSING DONE IN CNVWRTN NMOD.                 
*********************************************************************           
*                                                                               
CNVWR    DS    0H                                                               
         GOTO1 =A(CNVRTN),DMCB,(R8),(R4)                                        
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
*********************************************************************           
*DAYRTN- ROUTINE TO SET UP DAYS (MON THRU FRI, M-F AND M-S)                     
*        OR TO SET UP A SINGLE DAY FOR A CORRECTION RECORD                      
*********************************************************************           
*                                                                               
DAYRTN   NTR1                                                                   
         USING MIREC,RC                                                         
*                                                                               
         CLC   MISTART,MIEND       TEST SINGLE DAY CORRECTION RECORD            
         BNE   DAY10                                                            
*                                                                               
* SINGLE DAY - APPROPRIATE DAY IS ENTERED IN HDAYTAB                            
*              & REMAINDER OF HDAYTAB IS LEFT INTACT.                           
*                                                                               
         LA    R1,X'10'            MON                                          
         LA    RE,MIREC+(D4DAYS-D4REC)                                          
         LA    R0,7                                                             
DAY02    CLI   0(RE),C'1'          FIND APPROPRIATE DAY                         
         BE    DAY04                                                            
         AH    R1,=H'16'           INCREMENT TO NEXT DAY                        
         LA    RE,1(RE)                                                         
         BCT   R0,DAY02                                                         
         DC    H'0'                                                             
DAY04    STC   R1,BYTE             SAVE DAY CODE IN BYTE                        
*                                                                               
         L     RE,=A(HDAYTAB)                                                   
         LA    R0,HDAYS                                                         
DAY06    CLC   BYTE,1(RE)          TEST DAY CODE                                
         BE    DAY08                                                            
         LA    RE,L'HDAYTAB(RE)                                                 
         BCT   R0,DAY06                                                         
         DC    H'0'                                                             
DAY08    MVC   2(7,RE),MISTART     2(7,RE) = START DAY                          
         MVC   9(7,RE),MIEND       9(7,RE) = END DAY                            
         B     EXIT                                                             
*                                                                               
DAY10    MVC   HDMON+2(7),MISTART  SET UP MONDAY FROM START                     
         MVC   HDMON+9(7),MISTART                                               
         MVC   HDSUN+2(7),MIEND    SET UP SUNDAY FROM END                       
         MVC   HDSUN+9(7),MIEND                                                 
*                                                                               
         XC    HDTUE+2(14),HDTUE+2 CLEAR REAINING DAYS IN TABLE                 
         XC    HDWED+2(14),HDWED+2                                              
         XC    HDTHU+2(14),HDTHU+2                                              
         XC    HDFRI+2(14),HDFRI+2                                              
         XC    HDSAT+2(14),HDSAT+2                                              
         XC    HDM#F+2(14),HDM#F+2                                              
         XC    HDM#S+2(14),HDM#S+2                                              
*                                                                               
         LA    R5,HDMON+3          FILL IN MISSING DATES                        
         LA    R7,HDTUE+3                                                       
         LA    R2,5                                                             
DAY20    GOTO1 VADDAY,DMCB,(R5),(R7),1                                          
         BCTR  R7,0                                                             
         SR    RF,RF                                                            
         MVI   0(R7),X'F0'         CENTURY                                      
         CLI   1(R7),X'FA'         DDS YEAR '00' = X'FAF0'                      
         BL    DAY25                                                            
         IC    RF,1(R7)            ADJUST YEAR TO C'00'                         
         AHI   RF,-10                                                           
         STC   RF,1(R7)                                                         
         MVI   0(R7),C'1'          SET CENTURY BYTE                             
*                                                                               
DAY25    MVC   7(7,R7),0(R7)       FOR EQUAL START AND END                      
         LA    R7,1(R7)                                                         
         LA    R5,16(R5)           DO REST OF DAYS                              
         LA    R7,16(R7)                                                        
         BCT   R2,DAY20                                                         
         B     DAY80                                                            
*                                                                               
*                                                                               
DAY80    MVC   HDM#F+2(7),MISTART  SET UP MON-FRI                               
         MVC   HDM#F+9(7),HDFRI+9                                               
         MVC   HDM#S+2(7),MISTART  SET UP MON-SUN                               
         MVC   HDM#S+9(7),MIEND                                                 
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* ********************************************************************          
* PDATA- ROUTINE TO FIND ACTUAL RUN TIME AND AVERAGE DURATION                   
*              P1 =  A(SAVETIME)                                                
*              P2 =  A(DAY BITS FOR WEEK)                                       
* ********************************************************************          
PDATA    NTR1                                                                   
         L     R6,0(R1)            ADDRESS OF SAVETIME                          
         L     RE,4(R1)            ADDRESS OF DAY BITS                          
         MVC   BYTE,0(RE)          DAY BITS                                     
         XC    FULL,FULL           CLEAR DAY COUNT FOR DURATION                 
         LA    R3,X'40'            START AT MONDAY                              
         LA    R5,7                COUNTER                                      
         SR    R7,R7               CLEAR DURATION ACCUM                         
         XC    TIMTAB(7*L'TIMTAB),TIMTAB                                        
         XC    FULL1,FULL1         CLEAR TABLE ENTRY COUNT                      
*                                                                               
PDATA2   EX    R3,*+8              TEST IF PROGRAM RAN ON DAY                   
         B     *+8                                                              
         TM    BYTE,0                                                           
         BZ    PDATA4              NO                                           
         OC    0(8,R6),0(R6)       BYPASS IF NO TIMES                           
         BZ    PDATA4                                                           
*                                                                               
         PACK  DUB,0(2,R6)         START TIME                                   
         CVB   R1,DUB                                                           
         MH    R1,=H'100'          HOURS X 100                                  
         PACK  DUB,2(2,R6)         START MINUTE                                 
         CVB   R0,DUB              MINUTES                                      
         AR    R1,R0                                                            
         CH    R1,=H'2400'         TEST FOR PAST MIDNIGHT                       
         BNH   *+8                 BEFORE OR AT MIDNIGHT                        
         SH    R1,=H'2400'         SUBTRACT FOR MIL TIME                        
         STH   R1,HALF             SAVE MIL TIME                                
         PACK  DUB,4(4,R6)         DURATION IN MINUTES                          
         CVB   R0,DUB                                                           
         AR    R7,R0               UPDATE TOTAL DURATION                        
         L     RE,FULL                                                          
         LA    RE,1(RE)            UPDATE DAY COUNT                             
         ST    RE,FULL                                                          
         BAS   RE,UPTIME           UPDATE START TIME TABLE                      
*                                                                               
PDATA4   SRL   R3,1                NEXT DAY                                     
         LA    R6,L'SAVETIME(R6)   NEXT DAY'S TIME DATA                         
         BCT   R5,PDATA2                                                        
*                                                                               
PDATA6   SR    RE,RE               CALCULATE AVERAGE DURATION                   
         LR    RF,R7               DURATION                                     
         SLDA  RE,1                                                             
         D     RE,FULL                                                          
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         STCM  RF,3,PWK2DURM       RETURN AVE (LONG) DURATION                   
*                                                                               
         CH    RF,=H'240'          4-HOUR MAXIMUM (SHORT) DURATION              
         BNH   *+8                                                              
         LH    RF,=H'240'                                                       
         STC   RF,PWK1DURM         RETURN AVE DURATION                          
*                                  SORT TIMES IN DESCENDING ORDER               
         L     R0,FULL1            NUMBER OF ENTRIES IN TABLE                   
         GOTO1 VXSORT,DMCB,(X'FF',TIMTAB),(R0),3,1,2                            
         MVC   PWK1STIM,TIMTAB     FIRST OR MOST FREQUENT TIME                  
         B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
* ********************************************************************          
* UPTIME - ROUTINE TO UPDATE RUN TIME TABLE                                     
*              HALF CONTAINS TIME                                               
*              FULL1 CONTAINS TABLE ENTRY COUNT                                 
* ********************************************************************          
*                                                                               
UPTIME   DS    0H                                                               
         LR    R0,RE                                                            
         LA    R1,7                COUNTER                                      
         LA    RE,TIMTAB                                                        
*                                                                               
UPTIME2  OC    0(L'TIMTAB,RE),0(RE)     TEST FOR E-O-T                          
         BZ    UPTIME4                  INSERT ENTRY                            
         CLC   HALF,0(RE)               TEST IF TIME IS IN TABLE                
         BE    *+12                     YES-BUMP FREQUENCY COUNT                
         LA    RE,L'TIMTAB(RE)                                                  
         BCT   R1,UPTIME2                                                       
*                                                                               
         ZIC   RF,2(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,2(RE)                                                         
         B     UPTIMEX                                                          
*                                                                               
UPTIME4  MVC   0(2,RE),HALF        ADD NEW ENTRY                                
         MVI   2(RE),1             SET FREQUENCY TO 1                           
         L     R1,FULL1                                                         
         LA    R1,1(R1)            INCREMENT TABLE ENTRY COUNT                  
         ST    R1,FULL1                                                         
*                                                                               
UPTIMEX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* *******************************************************************           
* IOERROR - READ ERROR - SET ERROR FLAG AND CLOSE FILE                          
* *******************************************************************           
*                                                                               
IOERROR  DS    0H                                                               
         GOTO1 VLOGIO,DMCB,1,=C'READ ERR-INPUT'                                 
         B     WLR2                                                             
         SPACE 2                                                                
********************************************************************            
* WLR -  WRONG LENGTH RECORD ON INPUT                                           
********************************************************************            
*                                                                               
WLR      DS    0H                                                               
         GOTO1 VLOGIO,DMCB,1,=C'WRONG LEN REC-INPUT'                            
WLR2     CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'82'                                                   
         B     EXIT                                                             
         EJECT                                                                  
* ********************************************************************          
* HPTDAY -ROUTINE TO PERFORM A ROUTINE FOR AN ENTIRE DAYS HPT DATA              
*         CALLING TEMPLATE:                                                     
*           GOTO1 HPTDAY   DMCB,A(ROUTINE),A(SOURCE),A(DEST),WEIGHT             
* ********************************************************************          
*                                                                               
HPTDAY   NTR1                                                                   
         L     RF,0(R1)            ROUTINE TO PERFORM                           
         L     R5,4(R1)            SOURCE                                       
         L     R6,8(R1)            DESTINATION                                  
         L     R7,12(R1)           WEIGHT                                       
         L     R3,16(R1)           SEQUENTIAL HALF HOURS TO DO                  
HPTDAY1  GOTO1 (RF),DMCB,(R5),(R6),(R7)                                         
         LA    R5,HPTLN(R5)        NEXT SLOT                                    
         LA    R6,HPTLN(R6)                                                     
         BCT   R3,HPTDAY1                                                       
         B     XIT                                                              
         SPACE 2                                                                
HPTMAD   NTR1                      WEIGHTED ADD ROUTINE                         
         LA    RE,HPTAVEI-HPTFLAG  START OF ACCUMS                              
         LR    RF,RE                                                            
         A     RE,0(R1)            SOURCE                                       
         A     RF,4(R1)            DEST                                         
         L     R0,8(R1)            WEIGHT                                       
         LA    R9,HPTDEND-HPTAVEI  FIND NUMBER OF ACCUMS                        
         SRL   R9,2                                                             
HPTMAD1  L     R6,0(RE)            GET A VALUE FROM SOURCE                      
         LR    R7,R0               SWAP IN WEIGHT                               
         MR    R6,R6               DO WEIGHTING                                 
         A     R7,0(RF)            ADD IN PREVIOUS VALUES                       
         ST    R7,0(RF)            SAVE IN DEST.                                
         LA    RE,4(RE)            NEXT SLOT                                    
         LA    RF,4(RF)                                                         
         BCT   R9,HPTMAD1          LOOP UNTIL DONE                              
         B     XIT                                                              
         SPACE 2                                                                
HPTDIV   NTR1                      UNWEIGHT ACCUMS ROUTINE                      
         LA    RE,HPTAVEI-HPTFLAG  START OF ACCUMS                              
         LR    RF,RE                                                            
         A     RE,0(R1)            SOURCE                                       
         A     RF,4(R1)            DEST                                         
         L     R0,8(R1)            WEIGHT                                       
         LA    R9,HPTDEND-HPTAVEI  FIND NUMBER OF ACCUMS                        
         SRL   R9,2                                                             
HPTDIV1  L     R6,0(RE)            GET A VALUE                                  
         SR    R7,R7               GET READY FOR DIV                            
         SRDA  R6,31                                                            
         DR    R6,R0               UNWEIGHT                                     
         A     R7,=F'1'            AND ROUND                                    
         SRA   R7,1                                                             
         ST    R7,0(RF)            SAVE IN DEST.                                
         LA    RE,4(RE)            NEXT SLOT                                    
         LA    RF,4(RF)                                                         
         BCT   R9,HPTDIV1          LOOP UNTIL DONE                              
         B     XIT                                                              
         SPACE 2                                                                
SETHPT   NTR1                    INPUT  A(DAY),A(QTR HOUR)                      
         LM    RE,RF,0(R1)       OUT    A(SLOT)                                 
         ZIC   R6,0(RE)                                                         
         LR    RE,R6                                                            
         ZIC   R6,0(RF)                                                         
         LR    RF,R6                                                            
         LR    R7,RE                                                            
         LA    R6,HPTLN                                                         
         MH    R6,=H'48'                                                        
         MR    R6,R6               ADJUST FOR ONE DAY                           
         LR    RE,R7               AND SAVE RESULT                              
         LA    R7,HPTLN                                                         
         LR    R6,RF                                                            
         SRL   R6,1                CONVERT TO HALF HOUR                         
         MR    R6,R6               ADJUST FOR TIME                              
         AR    R7,RE               NOW FOR BOTH                                 
         A     R7,VPUTBUFF                                                      
         ST    R7,0(R1)                                                         
         B     XIT                                                              
         SPACE 2                                                                
HPTMFC   NTR1                                                                   
         LA    R9,1                                                             
         LA    RE,=F'0'            SEED THE QTR HRS FIRST                       
         LA    RF,=AL1(48)                                                      
         STM   RE,RF,DUB                                                        
         LA    R1,DUB                                                           
         BAS   RE,SETHPT                                                        
         LA    R1,10                                                            
         LA    R9,48                                                            
         L     RE,DUB                                                           
         USING HPTD,RE                                                          
         STC   R9,HPTQHR                                                        
         LA    RE,HPTLN(RE)                                                     
         LA    R9,2(R9)                                                         
         BCT   R1,*-12                                                          
         LA    R9,1                                                             
*                                                                               
HPTMFC2  LA    RE,=F'0'            CALC MISSING M-F DEMOS                       
         LA    RF,=AL1(48)                                                      
         STM   RE,RF,DUB                                                        
         LA    R1,DUB                                                           
         BAS   RE,SETHPT                                                        
         LA    RE,HPTMAD                                                        
         ST    RE,HPTPARA                                                       
         MVC   HPTPARA+16(4),=F'10'   NO. OF SEQ. HALF HOURS                    
         MVC   HPTPARA+8(4),DUB    OUTPUT SLOT                                  
         MVC   HPTPARA+12(4),=F'1' WEIGHT                                       
         LA    RF,=AL1(48)         SET FOR START QH                             
         ST    RF,DUB+4                                                         
         LA    RF,FULL                                                          
         STC   R9,FULL             MAD THE INDIV. DAYS FOR M-F                  
         ST    RF,DUB                                                           
         LA    R1,DUB                                                           
         BAS   RE,SETHPT                                                        
         MVC   HPTPARA+4(4),DUB    INPUT SLOTS                                  
         LA    R1,HPTPARA                                                       
         BAS   RE,HPTDAY                                                        
         LA    R9,1(R9)                                                         
         CH    R9,=H'5'                                                         
         BH    HPTMFC3                                                          
         B     HPTMFC2                                                          
         SPACE 2                                                                
HPTMFC3  LA    RE,=F'0'                                                         
         LA    RF,=AL1(48)                                                      
         STM   RE,RF,DUB                                                        
         LA    R1,DUB                                                           
         BAS   RE,SETHPT                                                        
         MVC   HPTPARA+4(4),DUB                                                 
         MVC   HPTPARA+8(4),DUB                                                 
         MVC   HPTPARA+12(4),=F'5' 5 DAYS                                       
         MVC   HPTPARA+16(4),=F'10'                                             
         LA    RE,HPTDIV                                                        
         ST    RE,HPTPARA                                                       
         LA    R1,HPTPARA                                                       
         BAS   RE,HPTDAY                                                        
         B     XIT                                                              
         SPACE 2                                                                
HPTMSC   NTR1                      CALC M-S AVERAGES                            
         LA    R1,HPTPARA                                                       
         LA    R6,HPTMAD           ROUTINE (MAD)                                
         ST    R6,HPTPARA                                                       
         MVC   HPTPARA+16(4),=F'48'   NO. OF HALF HOURS                         
         MVC   HPTPARA+4(4),VPUTBUFF SOURCE (M-F)                               
         LA    R7,HPTLN            DEST    (M-S)                                
         MH    R7,=H'48'                                                        
         MH    R7,=H'8'                                                         
         A     R7,VPUTBUFF                                                      
         ST    R7,HPTPARA+8                                                     
*                                                                               
         SR    R6,R6               SEED M-S DAY AND TIMES                       
         LR    RE,R7               ESTABLISH ADDRESSABILITY                     
HPTMSC2  MVI   HPTDY,X'80'         M-S                                          
         STC   R6,HPTQHR                                                        
         LA    R6,2(R6)                                                         
         LA    RE,HPTLN(RE)                                                     
         CH    R6,=H'96'                                                        
         BNH   HPTMSC2                                                          
*                                                                               
         LA    R9,7                ADD UP 7 DAYS                                
HPTMSC3  LA    R6,1                        (WEIGHT = 1)                         
         ST    R6,HPTPARA+12                                                    
         LR    R6,R9               SET FOR CURRENT DAY                          
         LA    R7,HPTLN                                                         
         MH    R7,=H'48'           INDEX FOR NUMBER OF SLOTS IN DAY             
         MR    R6,R6               INDEX FOR THE DAY                            
         A     R7,VPUTBUFF         POINT TO DAY START                           
         ST    R7,HPTPARA+4        AND SAVE                                     
         BAS   RE,HPTDAY           ADD EM UP                                    
         BCT   R9,HPTMSC3          DO FOR ALL DAYS                              
*                                                                               
         LA    R6,HPTDIV                                                        
         ST    R6,HPTPARA                                                       
         MVC   HPTPARA+4(4),HPTPARA+8 SOURCE = DEST                             
         LA    R6,7                (WEIGHT = 7)                                 
         ST    R6,HPTPARA+12                                                    
         BAS   RE,HPTDAY           (DIV - UNWEIGHT M-S )                        
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* EOF ON INPUT FILE                                                             
**********************************************************************          
                                                                                
MORET    DS    0H                                                               
ENDJOB   CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
                                                                                
XIT      XIT1                                                                   
                                                                                
EXIT     XMOD1 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
                                                                                
* *****************************                                                 
* LITERAL POOL - FOR MAIN NMOD                                                  
* *****************************                                                 
         LTORG                                                                  
         SPACE 4                                                                
**********************************************                                  
* HUT RECORD DAY CODE TABLE                                                     
*              BYTE 0 = NTI DAY CODE                                            
*              BYTE 1 = INTERNAL DAY CODE                                       
*              BYTES 3-9 = START DAY (CYYMMDD)                                  
*              BYTES 10-16 = END DAY (CYYMMDD)                                  
**********************************************                                  
*                                                                               
HDAYTAB  DS    0CL16                                                            
HDM#S    DC    C'0',X'80',14X'00'  M-SUN                                        
HDMON    DC    C'2',X'10',14X'00'  MON                                          
HDTUE    DC    C'3',X'20',14X'00'  TUE                                          
HDWED    DC    C'4',X'30',14X'00'  WED                                          
HDTHU    DC    C'5',X'40',14X'00'  THU                                          
HDFRI    DC    C'6',X'50',14X'00'  FRI                                          
HDSAT    DC    C'7',X'60',14X'00'  SAT                                          
HDSUN    DC    C'8',X'70',14X'00'  SUN                                          
HDM#F    DC    C'9',X'00',14X'00'  M-FRI                                        
HDAYS    EQU   (*-HDAYTAB)/L'HDAYTAB                                            
*                                                                               
HDAYTABS DS    CL(L'HDAYTAB*HDAYS) SAVE AREA                                    
         SPACE 4                                                                
**********************************************                                  
* INTAB COUNTS TABLE                                                            
*              BYTES 1-3 = DATE (YYMMDD)                                        
*              BYTE 4 = DAY (MON=X'40' - SUN=X'01')                             
*              BYTES 5-156 = COUNTS                                             
**********************************************                                  
         DC    C'**INTAB**'                                                     
         DS    0F                                                               
INTAB    DS    0XL156                                                           
         DS    XL3,XL1,38F         MON      WEEK1                               
         DS    XL3,XL1,38F         TUE                                          
         DS    XL3,XL1,38F         WED                                          
         DS    XL3,XL1,38F         THU                                          
         DS    XL3,XL1,38F         FRI                                          
         DS    XL3,XL1,38F         SAT                                          
         DS    XL3,XL1,38F         SUN                                          
*                                                                               
         DS    XL3,XL1,38F         MON      WEEK2                               
         DS    XL3,XL1,38F         TUE                                          
         DS    XL3,XL1,38F         WED                                          
         DS    XL3,XL1,38F         THU                                          
         DS    XL3,XL1,38F         FRI                                          
         DS    XL3,XL1,38F         SAT                                          
         DS    XL3,XL1,38F         SUN                                          
*                                                                               
         DS    XL3,XL1,38F         MON      WEEK3                               
         DS    XL3,XL1,38F         TUE                                          
         DS    XL3,XL1,38F         WED                                          
         DS    XL3,XL1,38F         THU                                          
         DS    XL3,XL1,38F         FRI                                          
         DS    XL3,XL1,38F         SAT                                          
         DS    XL3,XL1,38F         SUN                                          
*                                                                               
         DS    XL3,XL1,38F         MON      WEEK4                               
         DS    XL3,XL1,38F         TUE                                          
         DS    XL3,XL1,38F         WED                                          
         DS    XL3,XL1,38F         THU                                          
         DS    XL3,XL1,38F         FRI                                          
         DS    XL3,XL1,38F         SAT                                          
         DS    XL3,XL1,38F         SUN                                          
*                                                                               
         DS    XL3,XL1,38F         MON      WEEK5                               
         DS    XL3,XL1,38F         TUE      AT MOST 5 WEEKS IN A MONTH          
         DS    XL3,XL1,38F         WED                                          
         DS    XL3,XL1,38F         THU                                          
         DS    XL3,XL1,38F         FRI                                          
         DS    XL3,XL1,38F         SAT                                          
         DS    XL3,XL1,38F         SUN                                          
INTABS   EQU   (*-INTAB)/L'INTAB                                                
INTABX   DS    0H                                                               
         DC    XL3'00'                                                          
         DROP  R4                                                               
         EJECT                                                                  
B0NTR    NTR1  BASE=(*,B0NTRX),LABEL=*                                          
         USING B0REC,RC                                                         
         USING WORKD,R4                                                         
         GOTO1 VNETWEEK,DMCB,B0START+1,VGETDAY,VADDAY                           
         MVC   HALF(1),4(R1)       YEAR                                         
         MVC   HALF+1(1),8(R1)     WEEK                                         
         MVC   BOOKST,HALF                                                      
         MVC   BOOK1,HALF                                                       
         MVC   IBOOK1,HALF                                                      
         MVC   BOOK1S,HALF                                                      
         MVC   IBOOK1S,HALF                                                     
         MVC   TEMPDATE,B0START+1                                               
*                                                                               
         GOTO1 VNETWEEK,DMCB,B0END+1,VGETDAY,VADDAY                             
         MVC   HALF(1),4(R1)       YEAR                                         
         MVC   HALF+1(1),8(R1)     WEEK                                         
         MVC   BOOKEND,HALF                                                     
*                                                                               
         MVI   BYPREAD,0           SET TO READ NEXT RECORD                      
*                                                                               
         MVI   WFILE,C'N'                                                       
         CLI   BOOKTYPE,C'E'       WGHT VCR                                     
         BE    *+8                                                              
         CLI   BOOKTYPE,C'F'                                                    
         BE    *+8                                                              
         B     NOWGHT                                                           
         CLC   =C'WGT ',B0REPORT   SET UP FOR WGHT REPORT                       
         BNE   *+14                                                             
         MVI   WFILE,C'Y'                                                       
         MVC   B0REPORT(L'B0REPORT-4),B0REPORT+4                                
NOWGHT   DS    0C                                                               
*                                                                               
         CLC   =C'NSS CUSTOM MIT',B0REPORT                                      
         BE    *+6                 NSS - NO UNIVS ON TAPE, RD OUR FILE          
         DC    H'0'                UNKNOWN FILE TYPE                            
         MVI   GAASW,0             FOR NSS, IF THERE, ALLOW THEM                
         MVI   HPTGENSW,C'N'       DON'T GENERATE TIME PERD FOR SYN             
*                                                                               
         L     R1,=A(UNIVSAVE)                                                  
         ST    R1,ASLOT                                                         
         L     RE,=A(SVHHUTAB)                                                  
         ST    RE,ASVHHU                                                        
         MVC   BOOK,BOOKST                                                      
UNIVS5   XC    PAVKEY,PAVKEY       BUILD DIRECTORY KEY                          
         LA    R5,PAVKEY                                                        
         USING PRKEY,R5                                                         
         MVC   PRCODE(8),=C'PNNUUUUT'   LOOK UP UNIVERSE RECD                   
         MVC   PRBOOK,BOOK         FOR CURRENT WEEK                             
         MVC   PRBTYP,BOOKTYPE                                                  
*                                                                               
         L     RE,ASLOT                                                         
         LA    RF,UNIVSAVX                                                      
         CR    RE,RF                                                            
         BL    *+6                                                              
         DC    H'0'                OVERFLOW                                     
         MVC   0(2,RE),BOOK        SAVE BOOK (WEEK)                             
         LA    RE,4(RE)            ADVANCE TO DEMO BUFFER                       
         ST    RE,ASLOT                                                         
*                                                                               
         BAS   RE,RESLOT           READ RECD IN AND SLOT DEMOS                  
         BZ    *+6                                                              
         DC    H'0'                UNIV RECD NOT FOUND                          
*        L     R5,=A(UNIVSAVE)         SLOT HOMES UNIV PROPERLY                 
         L     R5,ASLOT                SLOT HOMES UNIV PROPERLY                 
         LA    R1,K0345-K0193      DISP TO YHOMES IN UNIVSAVE                   
         SRL   R1,2                DIVIDE BY 4                                  
         AR    R5,R1               PT TO HOMES                                  
         L     RE,ASVHHU                                                        
         MVC   0(2,RE),BOOK        BOOK                                         
         MVC   4(4,RE),0(R5)       HHLD UNIVERSES                               
*        MVC   SVHHUNIV,0(R5)                                                   
         DROP  R5                                                               
*                                                                               
         CLC   BOOK,BOOKEND                                                     
         BNL   UNIVSX                                                           
         L     RE,ASLOT            ADVANCE TO SLOTS FOR NEXT WEEK               
         LA    RE,51*4(RE)                                                      
         ST    RE,ASLOT                                                         
         L     RE,ASVHHU                                                        
         LA    RE,8(RE)                                                         
         ST    RE,ASVHHU                                                        
*                                                                               
*****************************************************************               
* ADD HERE VADDAY TO UPDATE THE WEEK FOR THE UNIVERSE SLOTTING                  
*****************************************************************               
*                                                                               
         GOTO1 VADDAY,DMCB,TEMPDATE,TEMPDATE,7                                  
         GOTO1 VNETWEEK,DMCB,TEMPDATE,VGETDAY,VADDAY                            
         MVC   BOOK(1),4(R1)                                                    
         MVC   BOOK+1(1),8(R1)                                                  
         B     UNIVS5                                                           
UNIVSX   DS    0H                                                               
************************************************************                    
*                                                                               
         L     R1,=A(INTAB)        READ INTABS RECDS AND CREATE TABLE           
         USING INTABD,R1                                                        
         MVC   DATADISP,=Y(PRFRSTEL-PRKEY)                                      
         MVI   ELCODE,X'21'        NAME ELEMENT (HAS DATE IN IT)                
*                                                                               
         MVC   BOOK,BOOKST                                                      
INTABS5  LA    R5,X'40'            READ 7-DAYS STARTING WITH MONDAY             
*                                                                               
B0RTN30  LA    RF,PAVKEY                                                        
         XC    PAVKEY,PAVKEY       READ SAMPLE COUNTS                           
         USING PRKEY,RF                                                         
         MVC   PRCODE(8),=C'PNNITABT'                                           
         MVC   PRBOOK,BOOK         FOR CURRENT WEEK                             
         MVC   PRBTYP,BOOKTYPE                                                  
*                                                                               
         STC   R5,PRDW                                                          
         ST    R1,INTABADR                                                      
         LA    RE,INTABCNT                                                      
         ST    RE,ASLOT            SLOT INTO INTABCNT                           
         DROP  RF                                                               
         BAS   RE,RESLOT           READ RECD IN AND SLOT DEMOS                  
         BNZ   B0RTN35             THIS DAY NOT FOUND--TRY NEXT ONE             
         L     R6,ASREC                                                         
         LA    R6,4(R6)                                                         
         USING PRKEY,R6                                                         
         MVC   INTABDAY,PRDW       SAVE DAY OF WEEK                             
         BAS   RE,GETEL            GET NAME ELEMENT FOR DATE                    
         MVC   INTABDAT,2(R6)      DATE                                         
         LA    R1,L'INTABTAB(R1)   R1->NEXT INTAB BUCKET AREA                   
B0RTN35  SRL   R5,1                NEXT DAY BIT                                 
         LTR   R5,R5                                                            
         BNZ   B0RTN30                                                          
*                                                                               
         CLC   BOOK,BOOKEND                                                     
         BNL   INTABSX                                                          
         L     RE,=A(INTABX)                                                    
         CR    R1,RE                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   RE,BOOK+1                                                        
         LA    RE,1(RE)           UPDATE WEEK NUMBER                            
         STC   RE,BOOK+1                                                        
         CLI   BOOK+1,52                                                        
         BNH   INTABS5                                                          
         MVI   BOOK+1,1           1ST WEEK OF NEW YEAR                          
         ZIC   RE,BOOK                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BOOK            UPDATE YEAR                                   
         B     INTABS5                                                          
INTABSX  DS    0H                                                               
                                                                                
         DROP  R6                                                               
*                                                                               
B0RTN50  MVI   INTAPESW,X'40'      DROP THIS RECORD                             
         CLI   RPRINT,0            TEST FOR PRINT OF RAT SER REC                
         BE    *+8                 NO                                           
         OI    INTAPESW,X'03'      YES-SET INDICATOR FOR CONTROLLER             
         L     R5,VBITMAP1                                                      
         ICM   R5,8,=C'N'          FOR REG PCKTPC -> NETWORK MAP                
         CLC   =C'NSS CUSTOM MIT',B0REPORT                                      
         BE    *+10                                                             
         CLC   =C'FOX SPORTS',B0REPORT     SAME AS NSS CUSTOM                   
         BE    *+14                NSS - NO UNIVS ON TAPE, RD OUR FILE          
         CLC   =C'NSS POCKETPIECE',B0REPORT                                     
         BNE   *+8                                                              
         ICM   R5,8,=C'S'          FOR NSS PCKTPC -> SYND MAP                   
         STCM  R5,8,TAPETYP                                                     
         GOTO1 VNTIPRG,DMCB,=C'BLDM',(R5),0                                     
         XIT1  ,                                                                
         EJECT                                                                  
**********************************************************************          
*RESLOT- READ RECD FROM FILE BUILT BY REG NTI AND SLOT DEMOS BACK               
*        INTO APPROPRIATE TABLE AND BUCKETS (RE-SLOT THEM)                      
**********************************************************************          
RESLOT   NTR1                                                                   
         MVC   SVKEY,PAVKEY                                                     
         L     R6,ASREC                                                         
         USING PRKEY,R6                                                         
         LA    R6,4(R6)                                                         
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI ',=C'NTIDIR',PAVKEY,(R6)                 
         CLI   DMCB+8,0            ANY READ ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PAVKEY(18),0(R6)    KEY READ IN                                  
         BNE   RESLEOF             NOT FOUND                                    
         MVC   SVDA,PRNDXDA-PRKEY(R6)                                           
         MVC   SVSTATUS,PRKSTAT-PRKEY(R6)                                       
         MVC   PAVKEY,SVKEY        RESTORE SAVED KEY                            
         MVC   0(L'SVKEY,R6),SVKEY                                              
         MVC   PRRSTAT-PRKEY(1,R6),SVSTATUS                                     
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI ',=C'NTIFIL',SVDA,(R6)                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SVKEY(18),0(R6)     SAME KEY?                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,PRFRSTEL-PRKEY(R6) PT TO 1ST ELEMENT                          
         SR    R0,R0                                                            
RESL10   CLI   0(R6),X'00'         ENDREC?                                      
         BNE   *+6                                                              
         DC    H'0'                END OF REC --NO '5E' ELEMENT???              
         CLI   0(R6),X'41'                                                      
         BE    RESL15              DEMO ELEMENT                                 
         IC    R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0               NEXT ELEMENT                                 
         B     RESL10                                                           
*                                                                               
RESL15   ZIC   R0,1(R6)                                                         
         AR    R0,R6                                                            
         ST    R0,DMCB             ADDR OF NEXT ELEM                            
         MVC   DMCB+4(1),2(R6)     SAVE PRECISION                               
         NI    DMCB+4,X'0F'                                                     
         ZIC   R0,DMCB+4           LENGTH OF EACH FLD IN ELEMT                  
         LA    R1,4                                                             
         SR    R1,R0               #BYTES TO DISP INTO UNIVSAVE                 
         L     R5,ASLOT            SLOT INTO BEGIN SLOT SPECIFIED               
         AR    R5,R1                                                            
         LA    R6,3(R6)            PT TO 1ST DEMO IN ELEMENT                    
*                                                                               
RESL20   ZIC   R1,DMCB+4           PRECISION                                    
         C     R6,DMCB             END OF THIS ELELMENT                         
         BL    RESL25                                                           
         SR    R1,R1               SET CC=ZERO ON EXIT                          
         B     RELSLOTX                                                         
*                                                                               
RESL25   BCTR  R1,0                MOVE DEMO INTO SLOT                          
         EXMVC R1,0(R5),0(R6)                                                   
         LA    R5,4(R5)            BUMP UNIV PTR                                
         AR    R6,R1               BUMP ELEMENT PTR                             
         LA    R6,1(R6)            +1 FROM BCTR                                 
         B     RESL20                                                           
*                                                                               
RESLEOF  LA    R1,1                SET CC TO NON ZERO                           
*                                                                               
RELSLOTX LTR   R1,R1                                                            
         XIT1                                                                   
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
B0NTRX   EQU   *                                                                
         DROP  R2,R4,R8,R9,RA,RB,RC,RE                                          
         EJECT                                                                  
*********************************************************************           
* CNVWR- SORT RECORD HOOK                                                       
*        NON-NETWORK M-F ROTATORS EMITTED FROM INDIVIDUAL DAY RECORDS           
*        DUPLICATE HUT RECORDS DROPPED                                          
*********************************************************************           
*                                                                               
CNVRTN   NMOD1 0,**CNVW**                                                       
         USING (CNVRTN,CNVRTNX),RB                                              
         USING DEMCOND,R8          R8 = DEMCNV GLOBAL WORK                      
         USING WORKD,R4            R4 = DENI09I WORK                            
*                                                                               
CNVWR40  L     R2,ASREC            R2 - USES INTERD DSECT                       
         USING INTERD,R2                                                        
*                                                                               
         L     R1,=A(SVHHUTAB)                                                  
CNVWR41  OC    0(2,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                SHOULD FIND BOOK IN TABLE                    
         CLC   INTBOOK,0(R1)                                                    
         BE    *+12                                                             
         LA    R1,8(R1)                                                         
         B     CNVWR41                                                          
         MVC   SVHHUNIV,4(R1)      HHLD UNIVERSES FOR CURRENT WEEK              
*                                                                               
         CLI   INTKEY+29,C'G'      HANDLE GAA AUDIENCE FIGURES                  
         BE    CNVWR100                                                         
*                                                                               
         CLC   INTSTA,=C'UUUUT'    UNIVERSE RECD                                
         BNE   CNVWR42                                                          
         LA    R1,CALVPH                                                        
         USING CALVPHD,R1                                                       
         LA    RE,INTACCS                                                       
         ST    RE,VPHDEMS                                                       
         DROP  R1                                                               
         GOTO1 VCALUNV,CALVPH    CALCULATE UNIVERSE VPH'S                       
         B     CNVWR120                                                         
*                                                                               
CNVWR42  CLC   INTSTA,=C'ITABT'                                                 
         BE    CNVWR120            INTABS   RECD                                
*                                                                               
         CLC   INTSTA,=C'HUT T'    SKIP TV USAGE RECORDS                        
         BE    CNVWR50                                                          
         CLI   INTSTA+4,C'S'       AND SYNDICATORS ALSO                         
         BE    CNVWR50                                                          
         XC    SUMWGHT,SUMWGHT     INIT FOR TP PUTS                             
         XC    SUMHPT,SUMHPT       RAW IMPS                                     
         L     RE,=A(PUTSUM)                                                    
         XC    0(L'PUTSUM,RE),0(RE)                                             
*        XC    PUTSUM,PUTSUM       CALCULATED PERCENTS                          
         CLI   INTDAYWK,X'8F'      THIS PART FOR NON-VAR ONLY                   
         BH    CNVWR45                                                          
         ZIC   RE,INTDAYWK         CONVERT DAY TO NORMAL                        
         SRL   RE,4                                                             
         STC   RE,SUMDAY                                                        
         MVC   SUMSQH,INTSQH                                                    
         MVC   SUMEQH,INTEQH                                                    
*                                                                               
         XC    SUMSTIM,SUMSTIM                                                  
         XC    SUMETIM,SUMETIM                                                  
         CLI   INTRTYP,C'Q'        PROGRAM AVERAGE                              
         BNE   *+16                                                             
         MVC   SUMSTIM,INTSTIM                                                  
         MVC   SUMETIM,INTETIM                                                  
         BAS   RE,QHSUM            SUM THE QUARTER HOURS                        
         B     CNVWR48                                                          
*                                                                               
CNVWR45  LA    R7,INTVAR-L'INTVAR  THIS PART FOR VAR                            
         LA    R9,1                                                             
CNVWR46  SR    R6,R6                                                            
         LA    R6,L'INTVAR(R6)                                                  
         STH   R6,HALF                                                          
         LR    R6,R9               SET THE DAY                                  
         MH    R6,HALF             INDEX                                        
         AR    R6,R7               THEN SLOT                                    
         OC    0(L'INTVAR,R6),0(R6)  ACTIVITY CHECK                             
         BZ    CNVWR47                                                          
         STC   R9,SUMDAY           SET DAY AND TIMES                            
*                                                                               
         USING INTVARD,R6                                                       
         MVC   SUMSQH,INTVSQ                                                    
         MVC   SUMEQH,INTVEQ                                                    
*                                                                               
         XC    SUMSTIM,SUMSTIM                                                  
         XC    SUMETIM,SUMETIM                                                  
         CLI   INTRTYP,C'Q'        PROGRAM AVERAGE                              
         BNE   *+16                                                             
         MVC   SUMSTIM,INTVSTIM                                                 
         MVC   SUMETIM,INTVETIM                                                 
         DROP  R6                                                               
*                                                                               
         BAS   RE,QHSUM            ADD UP THE HPT VALUES                        
CNVWR47  LA    R9,1(R9)                                                         
         CH    R9,=H'8'            LOOP UNTIL 7 DAYS ARE DONE                   
         BL    CNVWR46                                                          
*                                                                               
CNVWR48  SR    R6,R6                                                            
         ICM   R6,3,SUMWGHT                                                     
         GOTO1 CHPTDIV,DMCB,SUMHPT,SUMHPT,(R6) GET THE AVERAGE                  
         LA    RE,SUMHPT                                                        
         USING HPTD,RE                                                          
         MVC   TPPUT(L'HPTPUT,R2),HPTPUT SEED THE TP RAW PUTS                   
         MVC   TPHUTR(4,R2),HPTAVER      SEED HH TP HUT                         
         L     RF,=A(PUTSUM)                                                    
         MVC   PUTSAVE,0(RF)                                                    
         BAS   RE,PUTDIV                                                        
         MVC   PUTSTRT(L'PUTSUM,R2),PUTSAVE SEED THE TP PUTS                    
         L     RE,ASREC                                                         
         L     RF,AWREC                                                         
         LH    R1,0(RE)                                                         
         MOVE  ((RF),(R1)),(RE)                                                 
         B     CNVWR50                                                          
*                                                                               
CNVWR50  CLI   IPGAASW,C'Y'        MERGE IN GAA FIGURES IF THERE                
         BNE   CNVWR51                                                          
         L     RE,=A(SVGAIMP)      SRC                                          
         LA    RF,GAIMPS(R2)       DEST                                         
         LA    R1,GAALN1+GAALN2                                                 
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
CNVWR51  MVI   IPGAASW,C'N'        RESET GAA SWITCH                             
         LA    R7,INTKEY                                                        
         USING PRKEY,R7                                                         
         CLI   INTRTYP,PRCODEQU    -P-                                          
         BNE   CNVXIT                                                           
*                                                                               
         CLC   PRSTAT,=C'HUT T'    HUT OR                                       
         BNE   CNV10               AGG (NON-NETWORK)                            
*                                                                               
         B     CNVXIT              EMIT RECORD                                  
*                                                                               
CNVWR100 MVI   IPGAASW,C'Y'        TURN ON FOR MERGE                            
         MVI   BYPSORT,X'80'       DON'T RELEASE TO OP PHASE                    
         LA    RE,AAIMPS(R2)       SOURCE                                       
         L     RF,=A(SVGAIMP)      DEST  - SAVE THE GAA FIGURES                 
         LA    R1,GAALN1           LEN                                          
         MOVE  ((RF),(R1)),(RE)    MOVE DEMS FROM TOP UNTIL YHOMES              
         LA    RE,AAIMP2(R2)       DON'T MOVE HOMES--MOVE AFTER IT              
         L     RF,=A(SVGAIMP)      DEST                                         
         LA    RF,GAALN1(RF)                                                    
         LA    R1,GAALN2           LEN                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         B     CNVXIT                                                           
*                                                                               
CNVWR120 DS    0H                  UNIVERSE RECD JUST GETS RELEASED             
         USING KDSCT,R2                                                         
         MVC   K0537,SVHHUNIV      SAVE UNIV HOMES IMPRESSIONS                  
         B     CNVXIT              RELEASE UNIVERSE RECD                        
*                                                                               
* GENERATE M-F RECORDS FOR AGG (NON-NETWORK) SOURCES                            
         USING INTERD,R2           SORT RECORD                                  
CNV10    CLI   PRDW,X'05'          TEST X'01'(MON) THRU X'05'(FRI)              
         BH    CNVXIT                                                           
         CLI   PRDW,X'01'                                                       
         BL    CNVXIT                                                           
         BH    CNV20               TUE THRU FRI - ACCUMULATE TO SVSREC          
*                                                                               
         L     RF,=A(SVSREC)       MON - CLEAR SVSREC                           
         XCEF  (RF),2000                                                        
         XC    SVNOACCS,SVNOACCS                                                
         XC    SVRECLN,SVRECLN                                                  
         L     RE,ASREC            SAVE SORT RCD IN SVSREC                      
         L     R1,=F'1900'                                                      
         L     RF,=A(SVSREC)                                                    
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
CNV20    DS    0H                  MON THRU FRI                                 
         LA    R1,INTACCS-INTRECLN R1 - DISPLACEMENT TO INTACCS                 
         L     R5,=A(SVSREC)                                                    
         AR    R5,R1               R5 - INTACCS (SVSREC)                        
         LA    RE,INTACCS          RE - INTACCS (SREC)                          
         LH    RF,INTRECLN                                                      
         SR    RF,R1                                                            
         BP    CNV30               OK - WE HAVE DATA                            
*                                                                               
*                                  (NO INTACCS - ZERO DATA)                     
         CLI   PRDW,X'05'          TEST FOR FRIDAY                              
         BNE   CNV60               PURGE MON, TUE, WED & THU                    
         L     RF,SVNOACCS         FRI                                          
         LTR   RF,RF               WAS THERE ANY DATA AT ALL                    
         BNZ   CNV70               YES - AVERAGE THE DATA AND EMIT RCD          
         B     CNV90               NO - SIMPLY EMIT RECORD                      
*                                                                               
CNV30    LA    RF,3(RF)            (IN CASE OF TRUNCATED FULLWORD)              
         SRL   RF,2                RF - NUMBER OF INTACCS FULLWORDS             
         CLC   SVRECLN,INTRECLN                                                 
         BNL   CNV36                                                            
         MVC   SVRECLN,INTRECLN    SAVE MAX. RECORD LENGTH                      
         ST    RF,SVNOACCS         SAVE MAX. NO. OF INTACCS FULLWORDS           
*                                                                               
CNV36    CLI   PRDW,X'01'          TEST FOR MONDAY                              
         BE    CNV60               PURGE MON RECORD (DATA SAVED)                
*                                                                               
CNV40    L     R1,0(R5)            ACCUMULATE FIVE DAYS' DATA                   
         A     R1,0(RE)                                                         
         ST    R1,0(R5)                                                         
         LA    RE,4(RE)                                                         
         LA    R5,4(R5)                                                         
         BCT   RF,CNV40                                                         
*                                                                               
         CLI   PRDW,X'05'          TEST FOR FRIDAY                              
         BE    CNV70                                                            
CNV60    MVI   INTRTYP,0           PURGE RECORD (MON, TUE, WED & THU)           
         B     CNVXIT                                                           
*                                                                               
CNV70    L     RE,=A(SVSREC)       FRIDAY - EMIT RECORD                         
         L     R1,=F'1900'                                                      
         L     RF,ASREC                                                         
         MOVE  ((RF),(R1)),(RE)    MOVE SVSREC TO SREC                          
         OC    SVRECLN,SVRECLN                                                  
         BZ    CNV90               (ZERO DATA)                                  
         LH    RF,SVRECLN          USE MAXIMUM RECORD LENGTH                    
         LA    RF,7(RF)            ADD 4 FOR SHOMES (& 3 FOR SAFETY)            
         STH   RF,INTRECLN         SET NEW MAXIMUM RECORD LENGTH                
*                                                                               
         LA    RE,INTACCS          RE - INTACCS (SREC)                          
         L     RF,SVNOACCS         RF - MAX. NO. OF INTACCS FULLWORDS           
CNV80    L     R1,0(RE)            AVERAGE FIVE DAYS' DATA                      
         LTR   R1,R1                                                            
         BZ    CNV84                                                            
         SR    R0,R0                                                            
         A     R1,=F'2'            ROUND                                        
         D     R0,=F'5'            DIVIDE BY FIVE DAYS                          
         ST    R1,0(RE)                                                         
CNV84    LA    RE,4(RE)                                                         
         BCT   RF,CNV80                                                         
*                                                                               
         SPACE 2                                                                
         USING KDSCT,R2                                                         
CNV88    XC    K0533,K0533         CALC RHOMES USING IHOMES & UNIV              
         L     R1,K0537            IHOMES                                       
         LTR   R1,R1                                                            
         BZ    CNV90                                                            
         M     R0,=F'10'                                                        
         D     R0,SVHHUNIV         (USE UNIVERSE SAVED IN H1RTN)                
         SR    R0,R0                                                            
         A     R1,=F'5'            ROUND                                        
         D     R0,=F'10'                                                        
         ST    R1,K0533            RHOMES (XX.X)                                
*                                                                               
         SPACE 2                                                                
         XC    K0541,K0541         CALC SHOMES USING IHOMES & HUT               
         L     R1,K0537            IHOMES                                       
         ZIC   RF,PRSTIM           6A=0, 630A=2  -  6P=48, 630P=50              
         SRL   RF,1                QHR --> HHR                                  
         LA    RE,HPTLN            INDEX INTO BUFFER                            
         MR    RE,RE                                                            
         LR    RE,RF               SHIFT PRODUCT                                
         A     RE,VPUTBUFF                                                      
         USING HPTD,RE                                                          
*                                                                               
         OC    HPTAVEI,HPTAVEI                                                  
         BZ    CNV90               (ZERO HUT)                                   
         M     R0,=F'1000'                                                      
         D     R0,HPTAVEI          HUT PROJ                                     
         SR    R0,R0                                                            
         A     R1,=F'5'            ROUND                                        
         D     R0,=F'10'                                                        
         M     R0,=F'10'                                                        
         ST    R1,K0541            SHOMES (XX.0)                                
         MVC   K1045,HPTAVER       SET PUT                                      
         DROP  R2                                                               
*                                                                               
CNV90    MVI   PRDW,X'00'          SET DAY/WEEK TO M-F                          
         B     CNVXIT              EMIT RECORD                                  
*                                                                               
CNVX     XIT1                      FOR NTR1'S                                   
         DROP  R7                                                               
*                                                                               
CNVXIT   XMOD1                                                                  
         EJECT                                                                  
* ********************************************************************          
* PUTDIV - ?                                                                    
* ********************************************************************          
PUTDIV   NTR1                                                                   
         SR    R6,R6                                                            
         ICM   R6,3,SUMWGHT                                                     
         LR    R0,R6                                                            
         LA    R1,L'PUTSAVE                                                     
         SRL   R1,2                DIV BY 4                                     
         LA    RE,PUTSAVE                                                       
PUTDIV1  L     R6,0(RE)                                                         
         SR    R7,R7                                                            
         SRDA  R6,31                                                            
*                                                                               
         LTR   R0,R0               IF ZERO, BYPASS DIVIDE & CLR R7              
         BNZ   *+10                                                             
         SR    R7,R7                                                            
         B     PUTDIV4                                                          
*                                                                               
         DR    R6,R0                                                            
         A     R7,=F'1'                                                         
         SRA   R7,1                                                             
PUTDIV4  ST    R7,0(RE)                                                         
         LA    RE,4(RE)                                                         
         BCT   R1,PUTDIV1                                                       
         B     CNVX                                                             
         EJECT                                                                  
* ********************************************************************          
* QHSUM -REQUIRES SUMSQH,SUMEQH,SUMDAY TO BE SET                                
* ********************************************************************          
QHSUM    NTR1                                                                   
         DC    H'0'                                                             
         MVI   QHSFRST,1                                                        
         CLC   SUMSTIM,SUMETIM     END LESS THAN START                          
         BNH   QHSUM1                                                           
         SR    RE,RE               ADJUST FOR AFTER MIDNIGHT                    
         ICM   RE,3,SUMETIM                                                     
         LA    RE,2400(RE)                                                      
         STCM  RE,3,SUMETIM                                                     
*                                                                               
QHSUM1   LA    R6,HPTLN                                                         
         STH   R6,HALF                                                          
         MH    R6,=H'48'           48 HALF HOURS IN A DAY                       
         ZIC   R7,SUMDAY                                                        
         MR    R6,R6                                                            
         A     R7,VPUTBUFF                                                      
         OC    SUMSTIM,SUMSTIM     PROGS HAVE START AND END TIMES               
         BZ    QHSUM4                                                           
         CLI   QHSFRST,1           HANDLE ONE MINUTE PROGRAMS                   
         BE    *+14                                                             
         CLC   SUMSTIM,SUMETIM                                                  
         BNL   QHSUM8                                                           
*                                                                               
         MVI   QHSFRST,0                                                        
         GOTO1 VHRTOQH,DMCB,SUMSTIM,SUMSQH                                      
         SR    RF,RF                                                            
         ICM   RF,3,SUMSTIM        GET THE START                                
         LA    RF,1(RF)            INC. BY 1 MINUTE                             
         SR    RE,RE                                                            
         D     RE,=F'100'          GET HOURS AND MINUTES                        
         CH    RE,=H'60'           TAKE CARE OF 60 SEC BASE                     
         BNE   *+8                                                              
         LH    RE,=H'100'          SET TO BUMP THE HOUR                         
         MH    RF,=H'100'                                                       
         AR    RE,RF                                                            
         STCM  RE,3,SUMSTIM                                                     
         B     QHSUM5                                                           
*                                                                               
QHSUM4   CLC   SUMSQH,SUMEQH                                                    
         BH    QHSUM8                                                           
QHSUM5   ZIC   R6,SUMSQH                                                        
         SRL   R6,1                CONVERT TO HALF                              
         MH    R6,HALF             SET TO CORRECT DISP                          
         AR    R7,R6                                                            
         SR    R6,R6                                                            
         ICM   R6,3,SUMWGHT                                                     
         LA    R6,1(R6)                                                         
         STCM  R6,3,SUMWGHT                                                     
         ZIC   R6,SUMSQH                                                        
         LA    R6,1(R6)                                                         
         STC   R6,SUMSQH                                                        
         GOTO1 CHPTMAD,DMCB,(R7),SUMHPT,1                                       
*                                                                               
         L     R6,=A(UNIVSAVE)                                                  
         LA    R7,HPTPUT-HPTFLAG(R7) POINT TO RAW PUTS                          
         GOTO1 VCALPUT,DMCB,0,0,(R6),(R7),PUTSAVE                               
         LA    R1,PUTEND-PUTSTRT                                                
         SRL   R1,2                DIVIDE BY 4                                  
         L     RE,=A(PUTSAVE)                                                   
         L     RF,=A(PUTSUM)                                                    
QHSUM6   L     R7,0(RE)                                                         
         A     R7,0(RF)                                                         
         ST    R7,0(RF)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R1,QHSUM6                                                        
         B     QHSUM1                                                           
*                                                                               
QHSUM8   B     CNVX                                                             
         EJECT                                                                  
*                                                                               
CHPTMAD  NTR1                      WEIGHTED ADD ROUTINE                         
         LA    RE,HPTAVEI-HPTFLAG  START OF ACCUMS                              
         LR    RF,RE                                                            
         A     RE,0(R1)            SOURCE                                       
         A     RF,4(R1)            DEST                                         
         L     R0,8(R1)            WEIGHT                                       
         LA    R9,HPTDEND-HPTAVEI  FIND NUMBER OF ACCUMS                        
         SRL   R9,2                                                             
CHPTMAD1 L     R6,0(RE)            GET A VALUE FROM SOURCE                      
         LR    R7,R0               SWAP IN WEIGHT                               
         MR    R6,R6               DO WEIGHTING                                 
         A     R7,0(RF)            ADD IN PREVIOUS VALUES                       
         ST    R7,0(RF)            SAVE IN DEST.                                
         LA    RE,4(RE)            NEXT SLOT                                    
         LA    RF,4(RF)                                                         
         BCT   R9,CHPTMAD1         LOOP UNTIL DONE                              
         B     CNVX                                                             
                                                                                
*                                                                               
CHPTDIV  NTR1                      UNWEIGHT ACCUMS ROUTINE                      
         LA    RE,HPTAVEI-HPTFLAG  START OF ACCUMS                              
         LR    RF,RE                                                            
         A     RE,0(R1)            SOURCE                                       
         A     RF,4(R1)            DEST                                         
         L     R0,8(R1)            WEIGHT                                       
         LA    R9,HPTDEND-HPTAVEI  FIND NUMBER OF ACCUMS                        
         SRL   R9,2                                                             
CHPTDIV1 L     R6,0(RE)            GET A VALUE                                  
         SR    R7,R7               GET READY FOR DIV                            
         SRDA  R6,31                                                            
*                                                                               
         LTR   R0,R0               IF ZERO, BYPASS DIVIDE & CLR R7              
         BNZ   *+10                                                             
         SR    R7,R7                                                            
         B     CHPTDIV4                                                         
*                                                                               
         DR    R6,R0               UNWEIGHT                                     
         A     R7,=F'1'            AND ROUND                                    
         SRA   R7,1                                                             
CHPTDIV4 ST    R7,0(RF)            SAVE IN DEST.                                
         LA    RE,4(RE)            NEXT SLOT                                    
         LA    RF,4(RF)                                                         
         BCT   R9,CHPTDIV1          LOOP UNTIL DONE                             
         B     CNVX                                                             
         EJECT                                                                  
* *****************************                                                 
* LITERAL POOL - CNVWRTN NMOD                                                   
* *****************************                                                 
         LTORG                                                                  
*                                                                               
CNVRTNX  EQU   *                                                                
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*SUBRTN2 -EXTRA PROCESING RTNS AVOIDING ADDRESS ERRORS                          
**********************************************************************          
SUBR2    NMOD1 0,**SUB2**                                                       
         USING (SUBR2,SUBR2X),RB                                                
         USING DEMCOND,R8          R8 = DEMCNV GLOBAL WORK                      
         USING WORKD,R4            R4 = DENI09I WORK                            
         L     RC,0(R1)                                                         
         USING D4REC,RC                                                         
*                                                                               
         ZIC   R1,GOSUBN                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R02_00(R1)                                                       
*                                                                               
SETDB#   EQU   (R02_01-*)/4+1     SET DBUFFER AND SAVETIME TABLE                
GTIME#   EQU   (R02_02-*)/4+1     RTNS ACTIVE DAYS, TOT DUR AND AVG DUR         
*                                                                               
R02_00   DS    0H                                                               
R02_01   B     SETDBUF             SET DBUFF AND SAVETIME BUFFERS               
R02_02   B     GTIME               GET TIMES FROM DBUFF                         
*                                                                               
R01#     EQU   (*-R02_00)/4                                                     
         DC    H'0'                                                             
*                                                                               
XIT_2    XIT1                                                                   
         EJECT                                                                  
*                                                                               
**********************************************************************          
*SETDBUF - USING D4 RECD FIELDS, SET FIELDS IN DBUFFER FOR ACTIVE DAYS          
**********************************************************************          
SETDBUF  DS    0H                                                               
         OC    DBDAYS,D4DAYS       SAVE TOTAL DAYS PRGM RAN                     
         CLI   TAPETYP,C'S'                                                     
         BNE   SETDB8                                                           
         CLC   D4EVSHR(4),ZEROS                                                 
         BNE   SETDB8                                                           
         LA    R1,SYNSTIM          DUMMY A START TIME BASED ON DPT              
SETDB5   CLI   0(R1),X'FF'         SET UNKNOWN DPT TO 3:00PM                    
         BE    *+22                                                             
         CLC   D4DPT,0(R1)         COMPARE ON DAYPART                           
         BE    *+12                                                             
         LA    R1,L'SYNSTIM(R1)                                                 
         B     SETDB5                                                           
         MVC   D4EVSHR(4),2(R1)    SET APPX TIME ON DAYPT                       
*                                                                               
SETDB8   PACK  DUB,D4EVSHR         START TIME                                   
         CVB   R1,DUB                                                           
         MH    R1,=H'100'          HOURS X 100                                  
         PACK  DUB,D4EVSMIN        START MINUTE                                 
         CVB   R0,DUB              MINUTES                                      
         AR    R1,R0                                                            
         CH    R1,=H'2400'         TEST FOR PAST MIDNIGHT                       
         BNH   *+8                 BEFORE OR AT MIDNIGHT                        
         SH    R1,=H'2400'         SUBTRACT FOR MIL TIME                        
         STH   R1,HALF             SAVE MIL TIME                                
         GOTO1 VHRTOQH,DMCB,HALF,STQHR         START QH                         
*                                                                               
         LA    R7,DBUFF                                                         
         LA    R3,SAVETIME         SET IN SAVETIME AS WELL                      
         LA    R1,D4DAYS           PT TO DAY BITS                               
         LA    R0,7                LOOP THRU 7 DAYS                             
STDB10   CLI   0(R1),C'1'          IS BIT ON                                    
         BNE   STDB15                                                           
         BAS   RE,DTIME                                                         
*                                                                               
STDB15   LA    R1,1(R1)            NEXT DAY ON TAPE RECD                        
         LA    R7,DBUFDAYQ(R7)     NEXT DAY IN BUFFER                           
         LA    R3,L'SAVETIME(R3)                                                
         BCT   R0,STDB10                                                        
         OC    DBDAYS,D4DAYS       SAVE TOTAL DAYS PRGM RAN                     
*                                                                               
SETDBX   B     XIT_2                                                            
         DROP  RC                                                               
         EJECT                                                                  
**********************************************************************          
*DTIME - FILLS DBUFF WITH 1/2HRS AND DURTN PRG AIRED FOR DAY                    
*        INPUT:  R7     ->  DAY IN DBUFF                                        
*                R3     ->  DAY IN SAVETIME                                     
*                STSQH   =  START TIME                                          
*                D4DUR   =  DURATION                                            
*                D4EVSMIN =  START MINUTE                                       
**********************************************************************          
DTIME    NTR1                                                                   
         USING D4REC,RC                                                         
         MVC   DMCB(1),STQHR                                                    
         NI    DMCB,X'FE'          FORCE TO 1/2HR                               
         XC    DMCB+4(4),DMCB+4    ACCUM TOT DUR FOR DAY                        
*                                                                               
         OC    0(4,R7),0(R7)       WAS A START TIME SET FOR DAY?                
         BNZ   *+10                                                             
         MVC   0(4,R7),D4EVSHR     SAVE START TIME                              
         MVC   0(4,R3),0(R7)       PUT START TIME IN DBUFF IN SAVETIME          
         SR    R1,R1                                                            
         ICM   R1,3,4(R7)          PICK UP TOT DUR SO FAR FOR DAY               
         PACK  DUB,D4DUR                                                        
         CVB   R2,DUB              R2=DURATION OF PRGM                          
         AR    R1,R2               ADD THIS DUR TO TOT FOR DAY                  
         STCM  R1,3,4(R7)          SAVE TOT DUR                                 
         EDIT  (2,4(R7)),(4,4(R3))                                              
         OC    4(4,R3),ZEROS                                                    
         LA    R7,6(R7)            PT TO HLF HRS LIST                           
*                                                                               
DTIM05   CLI   0(R7),0                                                          
         BE    DTIM10                                                           
         CLC   DMCB(1),0(R7)       SAME QHR CODE?                               
         BE    DTIM10                                                           
         LA    R7,2(R7)            NEXT 1/2HR SLOT                              
         B     DTIM05                                                           
*                                                                               
DTIM10   MVC   0(1,R7),DMCB                                                     
         LA    R1,30               1ST HLF HR MAX = '30'                        
         CLC   D4EVSMIN,=C'30'     STARTS AFTER END OF 1ST HLFHR                
         BL    *+8                                                              
         LA    R1,60               MAX MIN = 60 FOR 2ND 1/2HR                   
         PACK  DUB,D4EVSMIN                                                     
         CVB   R0,DUB                                                           
         SR    R1,R0               CALC # MIN AVAIL TO RUN IN 1/2 HR            
*                                                                               
DTIM15   CR    R1,R2               AT LEAST 30 MIN LEFT IN TOTDUR               
         BNH   *+6                                                              
         LR    R1,R2               ELSE USE REMAINING DURN                      
         ZIC   R0,1(R7)                                                         
         AR    R0,R1               R1 + ANY EXISTING DUR                        
         STC   R0,1(R7)                                                         
         SR    R2,R1               REDUCE TOT DUR BY AMT USED IN 1/2HR          
         BZ    DTIMEX                       ACCTD FOR ALL MINS OF DURN          
*                                                                               
DTIM20   ZIC   R3,0(R7)                                                         
         LA    R3,2(R3)            BUMP TO NEXT 1/2HR                           
         LA    R7,2(R7)            NEXT 1/2HR SLOT FOR DAY                      
         ZIC   R0,0(R7)            SEE IF MATCH ON 1/2HR                        
         BNZ   *+12                IF EMPTY SLOT, SAVE 1/2HR CODE               
         STC   R3,0(R7)                                                         
         B     *+12                                                             
         CR    R0,R3               BUCKET NOT EMPTY --SAME 1/2 HR?              
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R1,30               30 MIN MAX IN 1/2HR                          
         B     DTIM15                                                           
*                                                                               
DTIMEX   B     XIT_2                                                            
         DROP  RC                                                               
         EJECT                                                                  
**********************************************************************          
*GTIME - GET ACCUMULATED DATA FROM DBUFF                                        
*        INPUT - STQHR  = 1/2 HOUR CODE                                         
*                DBDAYS = DAYS PROGRAM RAN                                      
*                                                                               
*        OUTPUT- HLFADUR= AVG DUR FOR 1/2HR                                     
*                HLFDAYS= DAYS ACTIVE FOR THIS 1/2HR                            
*                HTOTDUR= TOTAL DUR FOR HLFHR ACROSS ACTIVE DAYS                
**********************************************************************          
GTIME    DS    0H                                                               
         XC    HTOTDUR,HTOTDUR                                                  
         XC    HLFADUR,HLFADUR                                                  
         MVC   HLFDAYS,ZEROS                                                    
         XC    DMCB(8),DMCB                                                     
         LA    R0,7                                                             
         LA    R7,DBUFF                                                         
         LA    R3,DBDAYS                                                        
         LA    R6,HLFDAYS                                                       
*                                                                               
GTIME5   CLI   0(R3),C'1'          WAS THIS DAY ACTIVE                          
         BE    GTIME7                                                           
         MVI   0(R6),C'0'                                                       
GTIME6   LA    R7,DBUFDAYQ(R7)                                                  
         LA    R6,1(R6)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,GTIME5                                                        
         B     GTIM30                                                           
*                                                                               
GTIME7   DS    0H                                                               
         ST    R7,DMCB             SAVE DAY PTR TO DBUFF ENTY                   
         LA    R7,6(R7)            PT TO 1/2HR CODE LIST                        
         SR    R1,R1                                                            
*                                                                               
GTIM10   CLI   0(R7),0                                                          
         BE    GTIM20              DONE WITH DAY                                
         CLC   STQHR,0(R7)                                                      
         BE    *+12                                                             
         LA    R7,2(R7)                                                         
         B     GTIM10                                                           
         MVI   0(R6),C'1'          SET ACTIVE DAY                               
         ZIC   R1,1(R7)                                                         
         A     R1,HTOTDUR          SAVE HLF HOUR DUR                            
         ST    R1,HTOTDUR                                                       
         L     R1,DMCB+4           BUMP ACTIVE DAYS                             
         LA    R1,1(R1)                                                         
         ST    R1,DMCB+4                                                        
GTIM20   L     R7,DMCB                                                          
         B     GTIME6                                                           
*                                                                               
GTIM30   DS    0H                                                               
         OC    DMCB+4(4),DMCB+4    NUMBER OF ACTIVE DAYS                        
         BZ    GTIMEX                                                           
         SR    RE,RE               CALCULATE AVERAGE DURATION                   
         L     RF,HTOTDUR          DURATION                                     
         SLDA  RE,1                                                             
         D     RE,DMCB+4                                                        
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         STC   RF,HLFADUR          RETURN AVG DURATION                          
*                                                                               
GTIMEX   DS    0H                                                               
         B     XIT_2                                                            
         EJECT                                                                  
***********************************************************************         
*SYNSTIM- CONVERTS DAYPART TYPE TO AN APPX START TIME FOR SYN                   
***********************************************************************         
SYNSTIM  DS    0CL6                                                             
         DC    C'PR',C'2000'       PRIME TIME                                   
         DC    C'EF',C'1800'       EARLY FRINGE                                 
         DC    C'LF',C'2330'       LATE FRINGE                                  
         DC    C'WM',C'0600'       WEEKDAY MORNING                              
         DC    C'WD',C'1200'       WEEKDAY DAYTIME                              
         DC    C'ED',C'1200'       WEEKEND DAYTIME                              
         DC    X'FF',C'1700'       UNDEFINED -- SET TIME TO 5PM                 
*                                                                               
***********************************************************************         
* LITERAL POOL - SURB2 NMOD                                                     
***********************************************************************         
         LTORG                                                                  
*                                                                               
SUBR2X   EQU   *                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
                                                                                
WORKD    DS    0F                                                               
INTABADR DS    A                   INTAB TABLE ADDRESS                          
DATADISP DS    H                                                                
ELCODE   DS    X                                                                
PACK16   DS    PL16                                                             
TEMPDATE DS    CL6                                                              
ASLOT    DS    A                                                                
WFILE    DS    C                   Y=PROCESSING WGHTED FILE                     
GOSUBN   DS    X                                                                
ASVHHU   DS    A                                                                
SVHHUNIV DS    F                   HHLD UNIVERSES FOR CURRENT BOOK              
SVHHUTAB DS    (5*2)F              HOUSEHOLD UNIVERSE FROM H1 RECORD            
         DC    X'0000'             FOR EA WEEK. (BOOK+HHLD UNIVS)               
*                                  AT MOST 5 WEEKS                              
HPTPARA  DS    6F                                                               
HPTNOW   DS    F                                                                
HPTCNT   DC    H'0'                                                             
HPTGENSW DC    C'Y'                                                             
HPTFRST  DC    X'01'                                                            
*                                                                               
RELOFRST DC    X'01'                                                            
BYPREAD  DC    X'00'                                                            
BYPASSH4 DC    X'00'               BYPASS ALL BUT 1ST H4 RECORD                 
BYPASS01 DC    X'00'               BYPASS SINGLE DAY AVERAGES                   
CORRSW   DC    X'00'               CORRRECTION RECORD SWITCH                    
HUTSW    DC    X'00'               HUT RECORD GENERATION SWITCH                 
MTWTFSW  DC    X'00'               HUT RECORD (M-T-W-T-F USED)                  
PREVSTIM DC    X'FF'               HUT RECORD PREVIOUS START QTR HR             
*                                                                               
TAPETYP  DC    X'00'                                                            
GAASW    DC    C'N'                GROSS AVG AUD SWITCH                         
HAVPP    DC    X'00'               HAVE PROGRAM PUTS                            
QHSFRST  DS    C                                                                
QHRSW    DS    C                                                                
BYTE     DS    C                                                                
SWITCH   DS    C                                                                
CNVWRSW  DS    C                                                                
VARIOUS  DS    X                   1=VARIOUS                                    
VARS     DC    CL7'0000000'        USED FOR VARIOUS (AVERAGES)                  
DAYS     DC    CL7'0000000'        USED FOR VARIOUS (INDIVIDUAL DAYS)           
*                                                                               
KEYSRC   DS    C                                                                
SVVCR    DS    C                                                                
*                                                                               
SVDA     DS    F                                                                
SVSTATUS DS    C                                                                
SVKEY    DC    XL24'00'                                                         
PAVKEY   DC    XL24'00'                                                         
*                                                                               
BLANKS   DC    80C' '                                                           
ZEROS    DC    256C'0'             ZEROS - USE FOR A VARIETY OF THINGS          
*                                                                               
SAVETIME DS    7XL8                SAVE D4EVSHR, D4EVSMIN AND D4DUR             
VARSTIME DS    7XL8                SAVE D4EVSHR, D4EVSMIN AND D4DUR             
*                                                                               
SAVVAR   DS    7XL(INTVARLQ)       SAVE VAR INFO FROM INDIVIDUAL DAYS           
SAVVARLQ EQU   *-SAVVAR                                                         
*                                                                               
ITNDAY   DS    X                                                                
                                                                                
*-------------------------------------------------------------------            
*DBUFF - CLEARED WHEN D-RECD W/TOT DUR PRESENT.                                 
*        HOLDS START TIME OF PRGM ON DAY, TOTAL DUR,                            
*        HHR (QHR CODE) WITH DURATION RAN IN THAT HLF HR                        
*-------------------------------------------------------------------            
DBUFF    DS    0C                  7-DAYS                                       
DBUFMON  DS    CL4                 START HOUR/MIN                               
         DS    CL2                 TOTAL DUR FOR DAY                            
         DS    48CL2               HLFHR CODE/DUR IN HLFHR                      
DBUFDAYQ EQU   *-DBUFMON           DISP TO NEXT DAY IN BUFFER                   
         DS    6CL(DBUFDAYQ)       6 MORE DAYS                                  
DBUFFQ   EQU   *-DBUFF             L'DBUFFER                                    
*                                                                               
DBDAYS   DS    CL7                 DAYS ACTIVE IN DBUFF                         
*-------------------             PROCESS H-RECS USING DBUFF INFO                
HLFDAYS  DS    CL7                 DAYS THIS 1/2HR WAS ACTIVE                   
HLFADUR  DS    X                   AVERAGE 1/2HR DUR                            
HTOTDUR  DS    F                   TOTAL DUR FOR 1/2HR ACROSS DAYS              
STQHR    DS    X                                                                
*-------------------------------------------------------------------            
*                                                                               
THREE    DS    CL3                                                              
         DS    0F                                                               
CALVPH   DS    CL(VPHINT-VPHDEMS+4) CALCULATE VPH'S                             
SAVEINT  DS    CL(INTACCS-INTVALS) SAVE INTERIM RECORD                          
SAVEPNUM DS    CL10                SAVE PROGRAM NUMBER                          
*                                                                               
TIMTAB   DS    7CL3                                                             
*                                                                               
         EJECT                                                                  
* REPORT DESCRIPTOR SAVE AREA (RESET FOR CORRECTION RECORDS)                    
*                                                                               
BOOK1    DS    XL2                 KEY BOOK                                     
IBOOK1   DS    XL2                 INTERNAL BOOK                                
*                                                                               
BOOK1S   DS    XL2                 KEY BOOK (SAVE AREA)                         
IBOOK1S  DS    XL2                 INTERNAL BOOK (SAVE AREA)                    
*                                                                               
BOOKST   DS    XL2                 START WEEK OF THE MONTH                      
BOOKEND  DS    XL2                 END WEEK OF THE MONTH                        
BOOK     DS    XL2                                                              
* HOUSEHOLD USAGE SAVE AREA                                                     
*                                                                               
HUTSQH   DS    X                   NEXT QUARTER HOUR                            
HUTDUR   DS    X                                                                
HUTBOOK  DS    XL2                 KEY BOOK VALUE                               
HUTIBOOK DS    XL2                 INTERNAL BOOK VALUE                          
HUTSTA   DS    CL5                                                              
HUTDAYWK DS    X                                                                
*                                                                               
* PROGRAM DESCRIPTOR SAVE AREA                                                  
*                                                                               
PVALS    DS    0CL87                                                            
PNTINUM  DS    CL5                 NTI PRG NUMBER PWOS                          
PNUM     DS    XL2                 PROGRAM NUMBER                               
PNET     DS    CL4                 NETWORK                                      
PDPT     DS    C                   NSI DAYPART (ALWAYS ZERO)                    
PDPT2    DS    CL2                 NTI DAYPART                                  
PTYP     DS    CL4                 PROGRAM TYPE                                 
SUBPTYP  DS    CL4                 SUB PROGRAM TYPE                             
PPREM    DS    C                   PREMIERE CODE                                
PNAME    DS    CL25                PROGRAM NAME                                 
PTITLE   DS    CL32                EPISODE TITLE                                
*                                                                               
PDAY1    DS    X                   DAY CODE                                     
PDAY1BIT DS    X                   DAY BITS                                     
*                                                                               
PWK1STIM DS    XL2                 START TIME                                   
PWK1DURM DS    X                   SHORT DURATION IN MINUTES (MAX 240)          
PWK2DURM DS    XL2                 LONG DURATION IN MINUTES                     
PWK1AUD  DS    XL2                                                              
PWK1STAC DS    XL2                                                              
PWK1COV  DS    XL2                                                              
PWK1DTYP DS    X                                                                
PWK1RSF  DS    X                                                                
*                                                                               
         EJECT                                                                  
SVD4     DS    0CL83               SAVE D4 RCD'S POSITIONS 305-387              
*        THESE FIELDS ARE NOT REPORTED ON ALL D4 RECORDS                        
S4STA    DS    CL5        305-309  TOTAL PROGRAM STATION COUNT                  
         DS    CL5        310-314  TOT PROGRAM HEADEND COUNT                    
S4COV    DS    CL2        315-316  TOTAL PROGRAM COVERAGE PERCENT               
         DS    CL2        317-318  CA PROGRAM COVERAGE                          
         DS    CL9        319-327  (FILLER)                                     
S4TELE   DS    CL3        328-330  TELECASTS 01=DAY 02-07=AVERAGES              
S4TOTDUR DS    CL6        331-336  TOTAL DURATION                               
S4AVEHUT DS    CL9        337-345  PROGRAM AVERAGE PROJ. (XXX,XXX,XXX)          
S4AVERTG DS    CL3        346-348  PROGRAM AVERAGE RATING (XX.X)                
S4REPORT DS    CL1        349-349  REPORTABILITY IND. 0=REPORTABLE              
         DS    CL3        350-352  CA HH RTG                                    
         DS    CL1        353      CA REPORTABILITY                             
S4HUT    DS    CL9        354-362  PROGRAM HUT PROJ. (XXX,XXX,XXX)              
S4SHR    DS    CL2        363-364  PROGRAM SHARE (XX)                           
         DS    CL9        365-373  CA AUD PROJ                                  
         DS    CL2        374-375  CA HH SHR                                    
S4PROJ   DS    CL9        376-384  TOTAL AUDIENCE PROJ. (XXX,XXX,XXX)           
S4RTG   DS    CL3        385-387  TOTAL AUDIENCE RATING (XX.X)                  
         DS    CL13       388-400  (FILLER)                                     
*                                                                               
*        THESE FIELDS ARE NOT REPORTED ON ALL H4 RECORDS                        
SVPROJ   DS    CL9         92-100  HALF HOUR AUD. PROJ. (XXX,XXX,XXX)           
SVRTG    DS    CL3        101-103  HALF HOUR AUDIENCE RATING (XX.X)             
SVSHR    DS    CL2        114-115  HALF HOUR PROGRAM SHARE (XX)                 
*                                                                               
SVRECLN  DS    F                   SAVE INTERIM RECORD'S LENGTH                 
SVNOACCS DS    F                   SAVE NUMBER OF INTACCS FULLWORDS             
*                                                                               
* FILE DCB AND BUFFER AREA                                                      
*                                                                               
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=401,                                              X        
               MACRF=GM,                                               X        
               EODAD=MORET                                                      
*                                                                               
HPTADDR  DS    F                                                                
HPTSAVLN DC    A(HPTLN*48*9)                                                    
*                                                                               
SUMWGHT  DS    CL2                                                              
SUMDAY   DS    C                                                                
SUMSQH   DS    C                                                                
SUMEQH   DS    C                                                                
SUMSTIM  DS    CL2                                                              
SUMETIM  DS    CL2                                                              
         DS    0F                                                               
SUMHPT   DS    CL188                                                            
SVAVGHI  DS    F                   SAVE PROGRAM HH                              
HWCAREA  DS    0CL36               SAVE HH WITH CHILD DATA                      
HWCV18   DS    F                                                                
HWCV12   DS    F                                                                
HWCV06   DS    F                                                                
HWCR18   DS    F                                                                
HWCR12   DS    F                                                                
HWCR06   DS    F                                                                
HWCI18   DS    F                                                                
HWCI12   DS    F                                                                
HWCI06   DS    F                                                                
*                                                                               
*                                                                               
IPGAASW  DS    C                                                                
NOTAVAL  DS    C                   DATA NOT AVAILABLE                           
PUTSAVE  DS    CL(PUTEND-PUTSTRT)  SAVE AREA FOR CALCULATED PUTS                
PUTSUM   DS    CL(PUTEND-PUTSTRT)  SUM AREA FOR CALCULATED PUTS                 
UNIVSAVE DS    F                   SAVE AREA FOR PER. UNIVERSES                 
         DS    50F                 FOR EACH WEEK                                
         DS    F                                                                
         DS    50F                                                              
         DS    F                   WEEK                                         
         DS    50F                 UNIVERSES                                    
         DS    F                                                                
         DS    50F                                                              
         DS    F                                                                
         DS    50F                 AT MOST 5 WEEKS                              
UNIVSAVX DS    0H                                                               
*                                                                               
**SVHREC   DS    2000C               SAVE HUT RECORD                            
*                                                                               
*        THE NEXT 1500 BYTES USED FOR EITHER THE SORT OR INTERIM RECORD         
*                                                                               
SVSREC   EQU   *                   SAVE SORT RECORD                             
SVIREC   DS    2000C               SAVE INTERIM RECORD                          
SVGAIMP  DS    444C                                                             
         SPACE 1                                                                
*                            NET(4),PRG#(2),DAY(1) E.G. 7C,7F,90                
PRGAVG   DS    0XL7                PROG#'S HAVING ASSOCD AVG RECD               
         DC    1000X'00'           PROG#'S HAVING ASSOCD AVG RECD               
         DC    X'FFFF'                                                          
         EJECT                                                                  
******************************************                                      
* TABLE OF DAY BIT SETTINGS AND DAY CODES                                       
******************************************                                      
                                                                                
NETDAYTB DS    0CL2                                                             
         DC    X'4010'             MON                                          
         DC    X'2020'             TUE                                          
         DC    X'1030'             WED                                          
         DC    X'0840'             THU                                          
         DC    X'0450'             FRI                                          
         DC    X'0260'             SAT                                          
         DC    X'0170'             SUN                                          
         DC    X'7C00'             M-F                                          
         DC    X'7F80'             M-SUN                                        
NETDAYS  EQU   (*-NETDAYTB)/L'NETDAYTB                                          
*                                                                               
*******************************************************                         
* TABLE OF DAY CODES AND SORT VALUES FOR 'Q' RECORDS                            
*******************************************************                         
QSORTAB  DS    0XL2                                                             
         DC    X'8000'             M-S                                          
         DC    X'0001'             M-F                                          
         DC    X'9002'             VAR                                          
         DC    X'1003'             MON                                          
         DC    X'2004'             TUE                                          
         DC    X'3005'             WED                                          
         DC    X'4006'             THU                                          
         DC    X'5007'             FRI                                          
         DC    X'6008'             SAT                                          
         DC    X'7009'             SUN                                          
QSORTABS EQU   (*-QSORTAB)/L'QSORTAB                                            
*                                                                               
         SPACE 1                                                                
* TABLE OF TELECAST TYPES AND THEIR BIT SETTINGS USED FOR INTDTYP               
*                                                                               
TYPTAB   DS    0CL3                                                             
         DC    C'  ',X'09'         REGULAR - ORIGINAL (PROTECT)                 
         DC    C'0 ',X'09'         REGULAR - ORIGINAL                           
         DC    C' Y',X'11'         REGULAR - REPEAT   (PROTECT)                 
         DC    C'0Y',X'11'         REGULAR - REPEAT                             
         DC    C'1 ',X'0C'         SPECIAL - ORIGINAL                           
         DC    C'1Y',X'14'         SPECIAL - REPEAT                             
TYPES    EQU   (*-TYPTAB)/L'TYPTAB                                              
*                                                                               
         EJECT                                                                  
                                                                                
WORKX    EQU   *                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
* DSECTS  ******  DSECTS  ****** DSECTS ****** DSECTS ****** DSECTS             
***********************************************************************         
       ++INCLUDE DENNHPTD                                                       
         EJECT                                                                  
       ++INCLUDE DENTIMITD                                                      
         EJECT                                                                  
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTNT4DL                                                     
       ++INCLUDE DECALVPHD                                                      
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
*        PRINT OFF                                                              
         TITLE '- DEMO CONVERSION - NTI POCKETPIECE'                            
         EJECT                                                                  
DENI09I  CSECT                                                                  
*                                                                               
AKPROC   NMOD1 0,AKPROC                                                         
         USING KDSCT,R6                                                         
         USING ADSCT,R7                                                         
* YW25                                                                          
         TM    A0116+(L'A0116-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0116,=16C'0'                                                    
         PACK  DUB,A0116                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0193,FULL+(4-L'K0193)                                           
* YW68                                                                          
         TM    A0125+(L'A0125-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0125,=16C'0'                                                    
         PACK  DUB,A0125                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0197,FULL+(4-L'K0197)                                           
* YW911                                                                         
         TM    A0134+(L'A0134-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0134,=16C'0'                                                    
         PACK  DUB,A0134                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0201,FULL+(4-L'K0201)                                           
* YW1214                                                                        
         TM    A0143+(L'A0143-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0143,=16C'0'                                                    
         PACK  DUB,A0143                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0205,FULL+(4-L'K0205)                                           
* YW1517                                                                        
         TM    A0152+(L'A0152-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0152,=16C'0'                                                    
         PACK  DUB,A0152                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0209,FULL+(4-L'K0209)                                           
* YW1820                                                                        
         TM    A0161+(L'A0161-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0161,=16C'0'                                                    
         PACK  DUB,A0161                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0213,FULL+(4-L'K0213)                                           
* YW2124                                                                        
         TM    A0170+(L'A0170-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0170,=16C'0'                                                    
         PACK  DUB,A0170                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0217,FULL+(4-L'K0217)                                           
* YW2529                                                                        
         TM    A0179+(L'A0179-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0179,=16C'0'                                                    
         PACK  DUB,A0179                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0221,FULL+(4-L'K0221)                                           
* YW3034                                                                        
         TM    A0188+(L'A0188-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0188,=16C'0'                                                    
         PACK  DUB,A0188                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0225,FULL+(4-L'K0225)                                           
* YW3539                                                                        
         TM    A0197+(L'A0197-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0197,=16C'0'                                                    
         PACK  DUB,A0197                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0229,FULL+(4-L'K0229)                                           
* YW4044                                                                        
         TM    A0206+(L'A0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0206,=16C'0'                                                    
         PACK  DUB,A0206                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0233,FULL+(4-L'K0233)                                           
* YW4549                                                                        
         TM    A0215+(L'A0215-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0215,=16C'0'                                                    
         PACK  DUB,A0215                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0237,FULL+(4-L'K0237)                                           
* YW5054                                                                        
         TM    A0224+(L'A0224-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0224,=16C'0'                                                    
         PACK  DUB,A0224                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0241,FULL+(4-L'K0241)                                           
* YW5564                                                                        
         TM    A0233+(L'A0233-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0233,=16C'0'                                                    
         PACK  DUB,A0233                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0245,FULL+(4-L'K0245)                                           
* YW65+                                                                         
         TM    A0242+(L'A0242-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0242,=16C'0'                                                    
         PACK  DUB,A0242                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0249,FULL+(4-L'K0249)                                           
* YM25                                                                          
         TM    A0251+(L'A0251-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0251,=16C'0'                                                    
         PACK  DUB,A0251                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0253,FULL+(4-L'K0253)                                           
* YM68                                                                          
         TM    A0260+(L'A0260-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0260,=16C'0'                                                    
         PACK  DUB,A0260                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0257,FULL+(4-L'K0257)                                           
* YM911                                                                         
         TM    A0269+(L'A0269-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0269,=16C'0'                                                    
         PACK  DUB,A0269                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0261,FULL+(4-L'K0261)                                           
* YM1214                                                                        
         TM    A0278+(L'A0278-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0278,=16C'0'                                                    
         PACK  DUB,A0278                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0265,FULL+(4-L'K0265)                                           
* YM1517                                                                        
         TM    A0287+(L'A0287-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0287,=16C'0'                                                    
         PACK  DUB,A0287                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0269,FULL+(4-L'K0269)                                           
         XMOD1 1                                                                
         SPACE 2                                                                
PRECISAK LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MH    R0,=H'10'                                                        
         BCT   RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         BO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
BKPROC   NMOD1 0,BKPROC                                                         
         USING KDSCT,R6                                                         
         USING BDSCT,R7                                                         
* YM1820                                                                        
         TM    B0116+(L'B0116-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0116,=16C'0'                                                    
         PACK  DUB,B0116                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0273,FULL+(4-L'K0273)                                           
* YM2124                                                                        
         TM    B0125+(L'B0125-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0125,=16C'0'                                                    
         PACK  DUB,B0125                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0277,FULL+(4-L'K0277)                                           
* YM2529                                                                        
         TM    B0134+(L'B0134-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0134,=16C'0'                                                    
         PACK  DUB,B0134                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0281,FULL+(4-L'K0281)                                           
* YM3034                                                                        
         TM    B0143+(L'B0143-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0143,=16C'0'                                                    
         PACK  DUB,B0143                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0285,FULL+(4-L'K0285)                                           
* YM3539                                                                        
         TM    B0152+(L'B0152-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0152,=16C'0'                                                    
         PACK  DUB,B0152                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0289,FULL+(4-L'K0289)                                           
* YM4044                                                                        
         TM    B0161+(L'B0161-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0161,=16C'0'                                                    
         PACK  DUB,B0161                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0293,FULL+(4-L'K0293)                                           
* YM4549                                                                        
         TM    B0170+(L'B0170-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0170,=16C'0'                                                    
         PACK  DUB,B0170                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0297,FULL+(4-L'K0297)                                           
* YM5054                                                                        
         TM    B0179+(L'B0179-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0179,=16C'0'                                                    
         PACK  DUB,B0179                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0301,FULL+(4-L'K0301)                                           
* YM5564                                                                        
         TM    B0188+(L'B0188-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0188,=16C'0'                                                    
         PACK  DUB,B0188                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0305,FULL+(4-L'K0305)                                           
* YM65+                                                                         
         TM    B0197+(L'B0197-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0197,=16C'0'                                                    
         PACK  DUB,B0197                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0309,FULL+(4-L'K0309)                                           
* YWMOMS                                                                        
         TM    B0206+(L'B0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0206,=16C'0'                                                    
         PACK  DUB,B0206                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0313,FULL+(4-L'K0313)                                           
* YWW1820                                                                       
         TM    B0224+(L'B0224-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0224,=16C'0'                                                    
         PACK  DUB,B0224                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0317,FULL+(4-L'K0317)                                           
* YWW2124                                                                       
         TM    B0233+(L'B0233-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0233,=16C'0'                                                    
         PACK  DUB,B0233                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0321,FULL+(4-L'K0321)                                           
* YWW2534                                                                       
         TM    B0242+(L'B0242-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0242,=16C'0'                                                    
         PACK  DUB,B0242                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0325,FULL+(4-L'K0325)                                           
* YWW3544                                                                       
         TM    B0251+(L'B0251-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0251,=16C'0'                                                    
         PACK  DUB,B0251                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0329,FULL+(4-L'K0329)                                           
* YWW4549                                                                       
         TM    B0260+(L'B0260-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0260,=16C'0'                                                    
         PACK  DUB,B0260                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0333,FULL+(4-L'K0333)                                           
* YWW5054                                                                       
         TM    B0269+(L'B0269-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0269,=16C'0'                                                    
         PACK  DUB,B0269                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0337,FULL+(4-L'K0337)                                           
* YWW55+                                                                        
         TM    B0278+(L'B0278-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0278,=16C'0'                                                    
         PACK  DUB,B0278                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0341,FULL+(4-L'K0341)                                           
         XMOD1 1                                                                
         SPACE 2                                                                
PRECISBK LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MH    R0,=H'10'                                                        
         BCT   RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         BO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
CKPROC   NMOD1 0,CKPROC                                                         
         USING KDSCT,R6                                                         
         USING CDSCT,R7                                                         
* ZW25                                                                          
         TM    C0116+(L'C0116-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0116,=16C'0'                                                    
         PACK  DUB,C0116                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1049,FULL+(4-L'K1049)                                           
* ZW68                                                                          
         TM    C0125+(L'C0125-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0125,=16C'0'                                                    
         PACK  DUB,C0125                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1053,FULL+(4-L'K1053)                                           
* ZW911                                                                         
         TM    C0134+(L'C0134-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0134,=16C'0'                                                    
         PACK  DUB,C0134                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1057,FULL+(4-L'K1057)                                           
* ZW1214                                                                        
         TM    C0143+(L'C0143-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0143,=16C'0'                                                    
         PACK  DUB,C0143                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1061,FULL+(4-L'K1061)                                           
* ZW1517                                                                        
         TM    C0152+(L'C0152-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0152,=16C'0'                                                    
         PACK  DUB,C0152                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1065,FULL+(4-L'K1065)                                           
* ZW1820                                                                        
         TM    C0161+(L'C0161-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0161,=16C'0'                                                    
         PACK  DUB,C0161                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1069,FULL+(4-L'K1069)                                           
* ZW2124                                                                        
         TM    C0170+(L'C0170-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0170,=16C'0'                                                    
         PACK  DUB,C0170                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1073,FULL+(4-L'K1073)                                           
* ZW2529                                                                        
         TM    C0179+(L'C0179-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0179,=16C'0'                                                    
         PACK  DUB,C0179                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1077,FULL+(4-L'K1077)                                           
* ZW3034                                                                        
         TM    C0188+(L'C0188-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0188,=16C'0'                                                    
         PACK  DUB,C0188                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1081,FULL+(4-L'K1081)                                           
* ZW3539                                                                        
         TM    C0197+(L'C0197-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0197,=16C'0'                                                    
         PACK  DUB,C0197                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1085,FULL+(4-L'K1085)                                           
* ZW4044                                                                        
         TM    C0206+(L'C0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0206,=16C'0'                                                    
         PACK  DUB,C0206                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1089,FULL+(4-L'K1089)                                           
* ZW4549                                                                        
         TM    C0215+(L'C0215-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0215,=16C'0'                                                    
         PACK  DUB,C0215                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1093,FULL+(4-L'K1093)                                           
* ZW5054                                                                        
         TM    C0224+(L'C0224-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0224,=16C'0'                                                    
         PACK  DUB,C0224                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1097,FULL+(4-L'K1097)                                           
* ZW5564                                                                        
         TM    C0233+(L'C0233-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0233,=16C'0'                                                    
         PACK  DUB,C0233                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1101,FULL+(4-L'K1101)                                           
* ZW65+                                                                         
         TM    C0242+(L'C0242-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0242,=16C'0'                                                    
         PACK  DUB,C0242                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1105,FULL+(4-L'K1105)                                           
* ZM25                                                                          
         TM    C0251+(L'C0251-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0251,=16C'0'                                                    
         PACK  DUB,C0251                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1109,FULL+(4-L'K1109)                                           
* ZM68                                                                          
         TM    C0260+(L'C0260-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0260,=16C'0'                                                    
         PACK  DUB,C0260                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1113,FULL+(4-L'K1113)                                           
* ZM911                                                                         
         TM    C0269+(L'C0269-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0269,=16C'0'                                                    
         PACK  DUB,C0269                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1117,FULL+(4-L'K1117)                                           
* ZM1214                                                                        
         TM    C0278+(L'C0278-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0278,=16C'0'                                                    
         PACK  DUB,C0278                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1121,FULL+(4-L'K1121)                                           
* ZM1517                                                                        
         TM    C0287+(L'C0287-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0287,=16C'0'                                                    
         PACK  DUB,C0287                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1125,FULL+(4-L'K1125)                                           
         XMOD1 1                                                                
         SPACE 2                                                                
PRECISCK LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MH    R0,=H'10'                                                        
         BCT   RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         BO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
DKPROC   NMOD1 0,DKPROC                                                         
         USING KDSCT,R6                                                         
         USING DDSCT,R7                                                         
* ZM1820                                                                        
         TM    D0116+(L'D0116-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0116,=16C'0'                                                    
         PACK  DUB,D0116                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1129,FULL+(4-L'K1129)                                           
* ZM2124                                                                        
         TM    D0125+(L'D0125-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0125,=16C'0'                                                    
         PACK  DUB,D0125                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1133,FULL+(4-L'K1133)                                           
* ZM2529                                                                        
         TM    D0134+(L'D0134-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0134,=16C'0'                                                    
         PACK  DUB,D0134                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1137,FULL+(4-L'K1137)                                           
* ZM3034                                                                        
         TM    D0143+(L'D0143-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0143,=16C'0'                                                    
         PACK  DUB,D0143                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1141,FULL+(4-L'K1141)                                           
* ZM3539                                                                        
         TM    D0152+(L'D0152-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0152,=16C'0'                                                    
         PACK  DUB,D0152                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1145,FULL+(4-L'K1145)                                           
* ZM4044                                                                        
         TM    D0161+(L'D0161-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0161,=16C'0'                                                    
         PACK  DUB,D0161                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1149,FULL+(4-L'K1149)                                           
* ZM4549                                                                        
         TM    D0170+(L'D0170-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0170,=16C'0'                                                    
         PACK  DUB,D0170                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1153,FULL+(4-L'K1153)                                           
* ZM5054                                                                        
         TM    D0179+(L'D0179-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0179,=16C'0'                                                    
         PACK  DUB,D0179                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1157,FULL+(4-L'K1157)                                           
* ZM5564                                                                        
         TM    D0188+(L'D0188-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0188,=16C'0'                                                    
         PACK  DUB,D0188                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1161,FULL+(4-L'K1161)                                           
* ZM65+                                                                         
         TM    D0197+(L'D0197-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0197,=16C'0'                                                    
         PACK  DUB,D0197                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1165,FULL+(4-L'K1165)                                           
* ZWMOMS                                                                        
         TM    D0206+(L'D0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0206,=16C'0'                                                    
         PACK  DUB,D0206                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1169,FULL+(4-L'K1169)                                           
* ZWW1820                                                                       
         TM    D0224+(L'D0224-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0224,=16C'0'                                                    
         PACK  DUB,D0224                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1173,FULL+(4-L'K1173)                                           
* ZWW2124                                                                       
         TM    D0233+(L'D0233-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0233,=16C'0'                                                    
         PACK  DUB,D0233                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1177,FULL+(4-L'K1177)                                           
* ZWW2534                                                                       
         TM    D0242+(L'D0242-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0242,=16C'0'                                                    
         PACK  DUB,D0242                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1181,FULL+(4-L'K1181)                                           
* ZWW3544                                                                       
         TM    D0251+(L'D0251-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0251,=16C'0'                                                    
         PACK  DUB,D0251                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1185,FULL+(4-L'K1185)                                           
* ZWW4549                                                                       
         TM    D0260+(L'D0260-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0260,=16C'0'                                                    
         PACK  DUB,D0260                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1189,FULL+(4-L'K1189)                                           
* ZWW5054                                                                       
         TM    D0269+(L'D0269-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0269,=16C'0'                                                    
         PACK  DUB,D0269                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1193,FULL+(4-L'K1193)                                           
* ZWW55+                                                                        
         TM    D0278+(L'D0278-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0278,=16C'0'                                                    
         PACK  DUB,D0278                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1197,FULL+(4-L'K1197)                                           
         XMOD1 1                                                                
         SPACE 2                                                                
PRECISDK LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MH    R0,=H'10'                                                        
         BCT   RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         BO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
FKPROC   NMOD1 0,FKPROC                                                         
         USING KDSCT,R6                                                         
         USING FDSCT,R7                                                         
* RHOMES                                                                        
         TM    F0346+(L'F0346-1),X'F0'                                          
         BO    *+10                                                             
         MVC   F0346,=16C'0'                                                    
         PACK  DUB,F0346                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0533,FULL+(4-L'K0533)                                           
* IHOMES                                                                        
         TM    F0337+(L'F0337-1),X'F0'                                          
         BO    *+10                                                             
         MVC   F0337,=16C'0'                                                    
         PACK  DUB,F0337                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0537,FULL+(4-L'K0537)                                           
* SHOMES                                                                        
         TM    F0363+(L'F0363-1),X'F0'                                          
         BO    *+10                                                             
         MVC   F0363,=16C'0'                                                    
         PACK  DUB,F0363                                                        
         CVB   RF,DUB                                                           
         MVC   HALF,=H'01'                                                      
         BAS   R9,PRECISFK                                                      
         ST    RF,FULL                                                          
         MVC   K0541,FULL+(4-L'K0541)                                           
         XMOD1 1                                                                
         SPACE 2                                                                
PRECISFK LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MH    R0,=H'10'                                                        
         BCT   RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         BO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
GKPROC   NMOD1 0,GKPROC                                                         
         USING KDSCT,R6                                                         
         USING GDSCT,R7                                                         
* RHOMES                                                                        
         TM    G0189+(L'G0189-1),X'F0'                                          
         BO    *+10                                                             
         MVC   G0189,=16C'0'                                                    
         PACK  DUB,G0189                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0533,FULL+(4-L'K0533)                                           
* IHOMES                                                                        
         TM    G0180+(L'G0180-1),X'F0'                                          
         BO    *+10                                                             
         MVC   G0180,=16C'0'                                                    
         PACK  DUB,G0180                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0537,FULL+(4-L'K0537)                                           
* SHOMES                                                                        
         TM    G0206+(L'G0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   G0206,=16C'0'                                                    
         PACK  DUB,G0206                                                        
         CVB   RF,DUB                                                           
         MVC   HALF,=H'01'                                                      
         BAS   R9,PRECISGK                                                      
         ST    RF,FULL                                                          
         MVC   K0541,FULL+(4-L'K0541)                                           
         XMOD1 1                                                                
         SPACE 2                                                                
PRECISGK LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MH    R0,=H'10'                                                        
         BCT   RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         BO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
       ++INCLUDE DEDEMCNVD                                                      
         EJECT                                                                  
ADSCT    DSECT                                                                  
A0000    DS    0C                                                               
         ORG   *+0115              DISPLACEMENT TO DEMOS ON NIELSEN REC         
A0116    DS    CL0009              YW25                                         
A0125    DS    CL0009              YW68                                         
A0134    DS    CL0009              YW911                                        
A0143    DS    CL0009              YW1214                                       
A0152    DS    CL0009              YW1517                                       
A0161    DS    CL0009              YW1820                                       
A0170    DS    CL0009              YW2124                                       
A0179    DS    CL0009              YW2529                                       
A0188    DS    CL0009              YW3034                                       
A0197    DS    CL0009              YW3539                                       
A0206    DS    CL0009              YW4044                                       
A0215    DS    CL0009              YW4549                                       
A0224    DS    CL0009              YW5054                                       
A0233    DS    CL0009              YW5564                                       
A0242    DS    CL0009              YW65+                                        
A0251    DS    CL0009              YM25                                         
A0260    DS    CL0009              YM68                                         
A0269    DS    CL0009              YM911                                        
A0278    DS    CL0009              YM1214                                       
A0287    DS    CL0009              YM1517                                       
         EJECT                                                                  
BDSCT    DSECT                                                                  
B0000    DS    0C                                                               
         ORG   *+0115              DISPLACEMENT TO DEMOS ON NIELSEN REC         
B0116    DS    CL0009              YM1820                                       
B0125    DS    CL0009              YM2124                                       
B0134    DS    CL0009              YM2529                                       
B0143    DS    CL0009              YM3034                                       
B0152    DS    CL0009              YM3539                                       
B0161    DS    CL0009              YM4044                                       
B0170    DS    CL0009              YM4549                                       
B0179    DS    CL0009              YM5054                                       
B0188    DS    CL0009              YM5564                                       
B0197    DS    CL0009              YM65+                                        
B0206    DS    CL0009              YWMOMS                                       
B0215    DS    CL0009                                                           
B0224    DS    CL0009              YWW1820                                      
B0233    DS    CL0009              YWW2124                                      
B0242    DS    CL0009              YWW2534                                      
B0251    DS    CL0009              YWW3544                                      
B0260    DS    CL0009              YWW4549                                      
B0269    DS    CL0009              YWW5054                                      
B0278    DS    CL0009              YWW55+                                       
B0287    DS    CL0009                                                           
         EJECT                                                                  
CDSCT    DSECT                                                                  
C0000    DS    0C                                                               
         ORG   *+0115              DISPLACEMENT TO DEMOS ON NIELSEN REC         
C0116    DS    CL0009              ZW25                                         
C0125    DS    CL0009              ZW68                                         
C0134    DS    CL0009              ZW911                                        
C0143    DS    CL0009              ZW1214                                       
C0152    DS    CL0009              ZW1517                                       
C0161    DS    CL0009              ZW1820                                       
C0170    DS    CL0009              ZW2124                                       
C0179    DS    CL0009              ZW2529                                       
C0188    DS    CL0009              ZW3034                                       
C0197    DS    CL0009              ZW3539                                       
C0206    DS    CL0009              ZW4044                                       
C0215    DS    CL0009              ZW4549                                       
C0224    DS    CL0009              ZW5054                                       
C0233    DS    CL0009              ZW5564                                       
C0242    DS    CL0009              ZW65+                                        
C0251    DS    CL0009              ZM25                                         
C0260    DS    CL0009              ZM68                                         
C0269    DS    CL0009              ZM911                                        
C0278    DS    CL0009              ZM1214                                       
C0287    DS    CL0009              ZM1517                                       
         EJECT                                                                  
DDSCT    DSECT                                                                  
D0000    DS    0C                                                               
         ORG   *+0115              DISPLACEMENT TO DEMOS ON NIELSEN REC         
D0116    DS    CL0009              ZM1820                                       
D0125    DS    CL0009              ZM2124                                       
D0134    DS    CL0009              ZM2529                                       
D0143    DS    CL0009              ZM3034                                       
D0152    DS    CL0009              ZM3539                                       
D0161    DS    CL0009              ZM4044                                       
D0170    DS    CL0009              ZM4549                                       
D0179    DS    CL0009              ZM5054                                       
D0188    DS    CL0009              ZM5564                                       
D0197    DS    CL0009              ZM65+                                        
D0206    DS    CL0009              ZWMOMS                                       
D0215    DS    CL0009                                                           
D0224    DS    CL0009              ZWW1820                                      
D0233    DS    CL0009              ZWW2124                                      
D0242    DS    CL0009              ZWW2534                                      
D0251    DS    CL0009              ZWW3544                                      
D0260    DS    CL0009              ZWW4549                                      
D0269    DS    CL0009              ZWW5054                                      
D0278    DS    CL0009              ZWW55+                                       
D0287    DS    CL0009                                                           
         EJECT                                                                  
EDSCT    DSECT                                                                  
E0000    DS    0C                                                               
         ORG   *+0140              DISPLACEMENT TO DEMOS ON NIELSEN REC         
E0141    DS    CL0009              IHOMES                                       
E0150    DS    CL0003              RHOMES                                       
         EJECT                                                                  
FDSCT    DSECT                                                                  
F0000    DS    0C                                                               
         ORG   *+0336              DISPLACEMENT TO DEMOS ON NIELSEN REC         
F0337    DS    CL0009              IHOMES                                       
F0346    DS    CL0003              RHOMES                                       
F0349    DS    CL0014                                                           
F0363    DS    CL0002              SHOMES                                       
         EJECT                                                                  
GDSCT    DSECT                                                                  
G0000    DS    0C                                                               
         ORG   *+0179              DISPLACEMENT TO DEMOS ON NIELSEN REC         
G0180    DS    CL0009              IHOMES                                       
G0189    DS    CL0003              RHOMES                                       
G0192    DS    CL0014                                                           
G0206    DS    CL0002              SHOMES                                       
         EJECT                                                                  
HDSCT    DSECT                                                                  
H0000    DS    0C                                                               
         ORG   *+0115              DISPLACEMENT TO DEMOS ON NIELSEN REC         
H0116    DS    CL0009              YW25                                         
H0125    DS    CL0009              YW68                                         
H0134    DS    CL0009              YW911                                        
H0143    DS    CL0009              YW1214                                       
H0152    DS    CL0009              YW1517                                       
H0161    DS    CL0009              YW1820                                       
H0170    DS    CL0009              YW2124                                       
H0179    DS    CL0009              YW2529                                       
H0188    DS    CL0009              YW3034                                       
H0197    DS    CL0009              YW3539                                       
H0206    DS    CL0009              YW4044                                       
H0215    DS    CL0009              YW4549                                       
H0224    DS    CL0009              YW5054                                       
H0233    DS    CL0009              YW5564                                       
H0242    DS    CL0009              YW65+                                        
H0251    DS    CL0009              YM25                                         
H0260    DS    CL0009              YM68                                         
H0269    DS    CL0009              YM911                                        
H0278    DS    CL0009              YM1214                                       
H0287    DS    CL0009              YM1517                                       
         EJECT                                                                  
IDSCT    DSECT                                                                  
I0000    DS    0C                                                               
         ORG   *+0115              DISPLACEMENT TO DEMOS ON NIELSEN REC         
I0116    DS    CL0009              YM1820                                       
I0125    DS    CL0009              YM2124                                       
I0134    DS    CL0009              YM2529                                       
I0143    DS    CL0009              YM3034                                       
I0152    DS    CL0009              YM3539                                       
I0161    DS    CL0009              YM4044                                       
I0170    DS    CL0009              YM4549                                       
I0179    DS    CL0009              YM5054                                       
I0188    DS    CL0009              YM5564                                       
I0197    DS    CL0009              YM65+                                        
I0206    DS    CL0009              YWMOMS                                       
I0215    DS    CL0009                                                           
I0224    DS    CL0009              YWW1820                                      
I0233    DS    CL0009              YWW2124                                      
I0242    DS    CL0009              YWW2534                                      
I0251    DS    CL0009              YWW3544                                      
I0260    DS    CL0009              YWW4549                                      
I0269    DS    CL0009              YWW5054                                      
I0278    DS    CL0009              YWW55+                                       
I0287    DS    CL0009                                                           
         EJECT                                                                  
JDSCT    DSECT                                                                  
J0000    DS    0C                                                               
         ORG   *+0179              DISPLACEMENT TO DEMOS ON NIELSEN REC         
J0180    DS    CL0009              IHOMES                                       
J0189    DS    CL0003              RHOMES                                       
         EJECT                                                                  
KDSCT    DSECT                                                                  
K0000    DS    0C                                                               
         ORG   *+(INTACCS-INTRECLN)  DISPLACEMENT TO DEMOS ON INTREC            
K0193    DS    CL0004              YW25                                         
K0197    DS    CL0004              YW68                                         
K0201    DS    CL0004              YW911                                        
K0205    DS    CL0004              YW1214                                       
K0209    DS    CL0004              YW1517                                       
K0213    DS    CL0004              YW1820                                       
K0217    DS    CL0004              YW2124                                       
K0221    DS    CL0004              YW2529                                       
K0225    DS    CL0004              YW3034                                       
K0229    DS    CL0004              YW3539                                       
K0233    DS    CL0004              YW4044                                       
K0237    DS    CL0004              YW4549                                       
K0241    DS    CL0004              YW5054                                       
K0245    DS    CL0004              YW5564                                       
K0249    DS    CL0004              YW65+                                        
K0253    DS    CL0004              YM25                                         
K0257    DS    CL0004              YM68                                         
K0261    DS    CL0004              YM911                                        
K0265    DS    CL0004              YM1214                                       
K0269    DS    CL0004              YM1517                                       
K0273    DS    CL0004              YM1820                                       
K0277    DS    CL0004              YM2124                                       
K0281    DS    CL0004              YM2529                                       
K0285    DS    CL0004              YM3034                                       
K0289    DS    CL0004              YM3539                                       
K0293    DS    CL0004              YM4044                                       
K0297    DS    CL0004              YM4549                                       
K0301    DS    CL0004              YM5054                                       
K0305    DS    CL0004              YM5564                                       
K0309    DS    CL0004              YM65+                                        
K0313    DS    CL0004              YWMOMS                                       
K0317    DS    CL0004              YWW1820                                      
K0321    DS    CL0004              YWW2124                                      
K0325    DS    CL0004              YWW2534                                      
K0329    DS    CL0004              YWW3544                                      
K0333    DS    CL0004              YWW4549                                      
K0337    DS    CL0004              YWW5054                                      
K0341    DS    CL0004              YWW55+                                       
K0345    DS    CL0004              YHOMES                                       
K0349    DS    CL0004              VW18+                                        
K0353    DS    CL0004              VW1849                                       
K0357    DS    CL0004              VM18+                                        
K0361    DS    CL0004              VW2554                                       
K0365    DS    CL0004              VWWRK                                        
K0369    DS    CL0004              VW1217                                       
K0373    DS    CL0004              VM1849                                       
K0377    DS    CL0004              VM2554                                       
K0381    DS    CL0004              VM55+                                        
K0385    DS    CL0004              VW2549                                       
K0389    DS    CL0004              VW55+                                        
K0393    DS    CL0004              VWW1849                                      
K0397    DS    CL0004              VM1217                                       
K0401    DS    CL0004              VM2549                                       
K0405    DS    CL0004              VV2+                                         
K0409    DS    CL0004              VW1834                                       
K0413    DS    CL0004              VM1834                                       
K0417    DS    CL0004              VWMOMS                                       
K0421    DS    CL0004              VM2149                                       
K0425    DS    CL0004              VM3564                                       
K0429    DS    CL0004              VW21+                                        
K0433    DS    CL0004              VW3564                                       
K0437    DS    CL0004              VM2154                                       
K0441    DS    CL0004              VM21+                                        
K0445    DS    CL0004              VW2149                                       
K0449    DS    CL0004              VW2154                                       
K0453    DS    CL0004              VWW2554                                      
K0457    DS    CL0004              VV611                                        
K0461    DS    CL0004              VV1217                                       
K0465    DS    CL0004              VV211                                        
K0469    DS    CL0004              VW211                                        
K0473    DS    CL0004              VM211                                        
K0477    DS    CL0004              VW611                                        
K0481    DS    CL0004              VM611                                        
K0485    DS    CL0004              VV1214                                       
K0489    DS    CL0004              VV25                                         
K0493    DS    CL0004              VV1517                                       
K0497    DS    CL0004              VW1824                                       
K0501    DS    CL0004              VW1524                                       
K0505    DS    CL0004              VM1824                                       
K0509    DS    CL0004              VM1524                                       
K0513    DS    CL0004              VV68                                         
K0517    DS    CL0004              VV911                                        
K0521    DS    CL0004              VHWC18                                       
K0525    DS    CL0004              VHWC12                                       
K0529    DS    CL0004              VHWC6                                        
K0533    DS    CL0004              RHOMES                                       
K0537    DS    CL0004              IHOMES                                       
K0541    DS    CL0004              SHOMES                                       
K0545    DS    CL0004              FN03                                         
K0549    DS    CL0004              FN04                                         
K0553    DS    CL0004              FN05                                         
K0557    DS    CL0004              FN06                                         
K0561    DS    CL0004              FN07                                         
K0565    DS    CL0004              FN08                                         
K0569    DS    CL0004              VV18+                                        
K0573    DS    CL0004              VV1834                                       
K0577    DS    CL0004              VV1849                                       
K0581    DS    CL0004              VV2549                                       
K0585    DS    CL0004              VV2554                                       
K0589    DS    CL0004              VV3564                                       
K0593    DS    CL0004              VV5564                                       
K0597    DS    CL0004              VV55+                                        
K0601    DS    CL0004              VV1824                                       
K0605    DS    CL0004              VV2149                                       
K0609    DS    CL0004              VV2154                                       
K0613    DS    CL0004              VV21+                                        
K0617    DS    CL0004              VV1524                                       
K0621    DS    CL0004              RHWC18                                       
K0625    DS    CL0004              RHWC12                                       
K0629    DS    CL0004              RHWC6                                        
K0633    DS    CL0004              IHWC18                                       
K0637    DS    CL0004              IHWC12                                       
K0641    DS    CL0004              IHWC6                                        
K0645    DS    CL0004              FN00                                         
K0649    DS    CL0004              FN01                                         
K0653    DS    CL0004              FN02                                         
K0657    DS    CL0004              FN03                                         
K0661    DS    CL0004              FN04                                         
K0665    DS    CL0004              FN05                                         
K0669    DS    CL0004              FN06                                         
K0673    DS    CL0004              FN07                                         
K0677    DS    CL0004              FN08                                         
K0681    DS    CL0004              FN09                                         
K0685    DS    CL0004              FN0A                                         
K0689    DS    CL0004              FN0B                                         
K0693    DS    CL0004              FN0C                                         
K0697    DS    CL0004              FN0D                                         
K0701    DS    CL0004              FN0E                                         
K0705    DS    CL0004              FN0F                                         
K0709    DS    CL0004              FN10                                         
K0713    DS    CL0004              FN11                                         
K0717    DS    CL0004              FN12                                         
K0721    DS    CL0004              FN13                                         
K0725    DS    CL0004              FN14                                         
K0729    DS    CL0004              FN15                                         
K0733    DS    CL0004              FN16                                         
K0737    DS    CL0004              FN17                                         
K0741    DS    CL0004              FN18                                         
K0745    DS    CL0004              FN19                                         
K0749    DS    CL0004              FN1A                                         
K0753    DS    CL0004              FN1B                                         
K0757    DS    CL0004              FN1C                                         
K0761    DS    CL0004              FN1D                                         
K0765    DS    CL0004              FN1E                                         
K0769    DS    CL0004              FN1F                                         
K0773    DS    CL0004              FN20                                         
K0777    DS    CL0004              FN21                                         
K0781    DS    CL0004              FN22                                         
K0785    DS    CL0004              FN23                                         
K0789    DS    CL0004              FN24                                         
K0793    DS    CL0004              FN25                                         
K0797    DS    CL0004              FN26                                         
K0801    DS    CL0004              FN27                                         
K0805    DS    CL0004              FN28                                         
K0809    DS    CL0004              FN29                                         
K0813    DS    CL0004              FN2A                                         
K0817    DS    CL0004              FN2B                                         
K0821    DS    CL0004              OW18+                                        
K0825    DS    CL0004              OW1849                                       
K0829    DS    CL0004              OM18+                                        
K0833    DS    CL0004              OW2554                                       
K0837    DS    CL0004              OWWRK                                        
K0841    DS    CL0004              OW1217                                       
K0845    DS    CL0004              OM1849                                       
K0849    DS    CL0004              OM2554                                       
K0853    DS    CL0004              OM55+                                        
K0857    DS    CL0004              OW2549                                       
K0861    DS    CL0004              OW55+                                        
K0865    DS    CL0004              OWW1849                                      
K0869    DS    CL0004              OM1217                                       
K0873    DS    CL0004              OM2549                                       
K0877    DS    CL0004              OV2+                                         
K0881    DS    CL0004              OW1834                                       
K0885    DS    CL0004              OM1834                                       
K0889    DS    CL0004              OWMOMS                                       
K0893    DS    CL0004              OM2149                                       
K0897    DS    CL0004              OM3564                                       
K0901    DS    CL0004              OW21+                                        
K0905    DS    CL0004              OW3564                                       
K0909    DS    CL0004              OM2154                                       
K0913    DS    CL0004              OM21+                                        
K0917    DS    CL0004              OW2149                                       
K0921    DS    CL0004              OW2154                                       
K0925    DS    CL0004              OWW2554                                      
K0929    DS    CL0004              OV611                                        
K0933    DS    CL0004              OV1217                                       
K0937    DS    CL0004              OV211                                        
K0941    DS    CL0004              OW211                                        
K0945    DS    CL0004              OM211                                        
K0949    DS    CL0004              OW611                                        
K0953    DS    CL0004              OM611                                        
K0957    DS    CL0004              OV1214                                       
K0961    DS    CL0004              OV25                                         
K0965    DS    CL0004              OV1517                                       
K0969    DS    CL0004              OW1824                                       
K0973    DS    CL0004              OW1524                                       
K0977    DS    CL0004              OM1824                                       
K0981    DS    CL0004              OM1524                                       
K0985    DS    CL0004              OV611                                        
K0989    DS    CL0004              OV911                                        
K0993    DS    CL0004              OV18+                                        
K0997    DS    CL0004              OV1834                                       
K1001    DS    CL0004              OV1849                                       
K1005    DS    CL0004              OV2549                                       
K1009    DS    CL0004              OV2554                                       
K1013    DS    CL0004              OV3564                                       
K1017    DS    CL0004              OV5564                                       
K1021    DS    CL0004              OV55+                                        
K1025    DS    CL0004              OV1824                                       
K1029    DS    CL0004              OV2149                                       
K1033    DS    CL0004              OV2154                                       
K1037    DS    CL0004              OV21+                                        
K1041    DS    CL0004              OV1524                                       
K1045    DS    CL0004              OHOMES                                       
K1049    DS    CL0004              ZW25                                         
K1053    DS    CL0004              ZW68                                         
K1057    DS    CL0004              ZW911                                        
K1061    DS    CL0004              ZW1214                                       
K1065    DS    CL0004              ZW1517                                       
K1069    DS    CL0004              ZW1820                                       
K1073    DS    CL0004              ZW2124                                       
K1077    DS    CL0004              ZW2529                                       
K1081    DS    CL0004              ZW3034                                       
K1085    DS    CL0004              ZW3539                                       
K1089    DS    CL0004              ZW4044                                       
K1093    DS    CL0004              ZW4549                                       
K1097    DS    CL0004              ZW5054                                       
K1101    DS    CL0004              ZW5564                                       
K1105    DS    CL0004              ZW65+                                        
K1109    DS    CL0004              ZM25                                         
K1113    DS    CL0004              ZM68                                         
K1117    DS    CL0004              ZM911                                        
K1121    DS    CL0004              ZM1214                                       
K1125    DS    CL0004              ZM1517                                       
K1129    DS    CL0004              ZM1820                                       
K1133    DS    CL0004              ZM2124                                       
K1137    DS    CL0004              ZM2529                                       
K1141    DS    CL0004              ZM3034                                       
K1145    DS    CL0004              ZM3539                                       
K1149    DS    CL0004              ZM4044                                       
K1153    DS    CL0004              ZM4549                                       
K1157    DS    CL0004              ZM5054                                       
K1161    DS    CL0004              ZM5564                                       
K1165    DS    CL0004              ZM65+                                        
K1169    DS    CL0004              ZWMOMS                                       
K1173    DS    CL0004              ZWW1820                                      
K1177    DS    CL0004              ZWW2124                                      
K1181    DS    CL0004              ZWW2534                                      
K1185    DS    CL0004              ZWW3544                                      
K1189    DS    CL0004              ZWW4549                                      
K1193    DS    CL0004              ZWW5054                                      
K1197    DS    CL0004              ZWW55+                                       
K1201    DS    CL0004              WW25                                         
K1205    DS    CL0004              WW68                                         
K1209    DS    CL0004              WW911                                        
K1213    DS    CL0004              WW1214                                       
K1217    DS    CL0004              WW1517                                       
K1221    DS    CL0004              WW1820                                       
K1225    DS    CL0004              WW2124                                       
K1229    DS    CL0004              WW2529                                       
K1233    DS    CL0004              WW3034                                       
K1237    DS    CL0004              WW3539                                       
K1241    DS    CL0004              WW4044                                       
K1245    DS    CL0004              WW4549                                       
K1249    DS    CL0004              WW5054                                       
K1253    DS    CL0004              WW5564                                       
K1257    DS    CL0004              WW65+                                        
K1261    DS    CL0004              WM25                                         
K1265    DS    CL0004              WM68                                         
K1269    DS    CL0004              WM911                                        
K1273    DS    CL0004              WM1214                                       
K1277    DS    CL0004              WM1517                                       
K1281    DS    CL0004              WM1820                                       
K1285    DS    CL0004              WM2124                                       
K1289    DS    CL0004              WM2529                                       
K1293    DS    CL0004              WM3034                                       
K1297    DS    CL0004              WM3539                                       
K1301    DS    CL0004              WM4044                                       
K1305    DS    CL0004              WM4549                                       
K1309    DS    CL0004              WM5054                                       
K1313    DS    CL0004              WM5564                                       
K1317    DS    CL0004              WM65+                                        
K1321    DS    CL0004              WWMOMS                                       
K1325    DS    CL0004              WWW1820                                      
K1329    DS    CL0004              WWW2124                                      
K1333    DS    CL0004              WWW2534                                      
K1337    DS    CL0004              WWW3544                                      
K1341    DS    CL0004              WWW4549                                      
K1345    DS    CL0004              WWW5054                                      
K1349    DS    CL0004              WWW55+                                       
K1353    DS    CL0004              BW25                                         
K1357    DS    CL0004              BW68                                         
K1361    DS    CL0004              BW911                                        
K1365    DS    CL0004              BW1214                                       
K1369    DS    CL0004              BW1517                                       
K1373    DS    CL0004              BW1820                                       
K1377    DS    CL0004              BW2124                                       
K1381    DS    CL0004              BW2529                                       
K1385    DS    CL0004              BW3034                                       
K1389    DS    CL0004              BW3539                                       
K1393    DS    CL0004              BW4044                                       
K1397    DS    CL0004              BW4549                                       
K1401    DS    CL0004              BW5054                                       
K1405    DS    CL0004              BW5564                                       
K1409    DS    CL0004              BW65+                                        
K1413    DS    CL0004              BM25                                         
K1417    DS    CL0004              BM68                                         
K1421    DS    CL0004              BM911                                        
K1425    DS    CL0004              BM1214                                       
K1429    DS    CL0004              BM1517                                       
K1433    DS    CL0004              BM1820                                       
K1437    DS    CL0004              BM2124                                       
K1441    DS    CL0004              BM2529                                       
K1445    DS    CL0004              BM3034                                       
K1449    DS    CL0004              BM3539                                       
K1453    DS    CL0004              BM4044                                       
K1457    DS    CL0004              BM4549                                       
K1461    DS    CL0004              BM5054                                       
K1465    DS    CL0004              BM5564                                       
K1469    DS    CL0004              BM65+                                        
K1473    DS    CL0004              BWMOMS                                       
K1477    DS    CL0004              BWW1820                                      
K1481    DS    CL0004              BWW2124                                      
K1485    DS    CL0004              BWW2534                                      
K1489    DS    CL0004              BWW3544                                      
K1493    DS    CL0004              BWW4549                                      
K1497    DS    CL0004              BWW5054                                      
K1501    DS    CL0004              BWW55+                                       
K1505    DS    CL0004              MW18+                                        
K1509    DS    CL0004              MW1849                                       
K1513    DS    CL0004              MM18+                                        
K1517    DS    CL0004              MW2554                                       
K1521    DS    CL0004              MWWRK                                        
K1525    DS    CL0004              MW1217                                       
K1529    DS    CL0004              MM1849                                       
K1533    DS    CL0004              MM2554                                       
K1537    DS    CL0004              MM55+                                        
K1541    DS    CL0004              MW2549                                       
K1545    DS    CL0004              MW55+                                        
K1549    DS    CL0004              MWW1849                                      
K1553    DS    CL0004              MM1217                                       
K1557    DS    CL0004              MM2549                                       
K1561    DS    CL0004              MV2+                                         
K1565    DS    CL0004              MW1834                                       
K1569    DS    CL0004              MM1834                                       
K1573    DS    CL0004              MWMOMS                                       
K1577    DS    CL0004              MM2149                                       
K1581    DS    CL0004              MM3564                                       
K1585    DS    CL0004              MW21+                                        
K1589    DS    CL0004              MW3564                                       
K1593    DS    CL0004              MM2154                                       
K1597    DS    CL0004              MM21+                                        
K1601    DS    CL0004              MW2149                                       
K1605    DS    CL0004              MW2154                                       
K1609    DS    CL0004              MWW2554                                      
K1613    DS    CL0004              MV611                                        
K1617    DS    CL0004              MV1217                                       
K1621    DS    CL0004              MV211                                        
K1625    DS    CL0004              MW211                                        
K1629    DS    CL0004              MM211                                        
K1633    DS    CL0004              MW611                                        
K1637    DS    CL0004              MM611                                        
K1641    DS    CL0004              MV1214                                       
K1645    DS    CL0004              MV25                                         
K1649    DS    CL0004              MV1517                                       
K1653    DS    CL0004              MW1824                                       
K1657    DS    CL0004              MW1524                                       
K1661    DS    CL0004              MM1824                                       
K1665    DS    CL0004              MM1524                                       
K1669    DS    CL0004              MV68                                         
K1673    DS    CL0004              MV911                                        
K1677    DS    CL0004              MHWC18                                       
K1681    DS    CL0004              MHWC12                                       
K1685    DS    CL0004              MHWC6                                        
K1689    DS    CL0004              LHOMES                                       
K1693    DS    CL0004              NHOMES                                       
K1697    DS    CL0004              FN02                                         
K1701    DS    CL0004              FN03                                         
K1705    DS    CL0004              FN04                                         
K1709    DS    CL0004              FN05                                         
K1713    DS    CL0004              FN06                                         
K1717    DS    CL0004              FN07                                         
K1721    DS    CL0004              FN08                                         
K1725    DS    CL0004              MV18+                                        
K1729    DS    CL0004              MV1834                                       
K1733    DS    CL0004              MV1849                                       
K1737    DS    CL0004              MV2549                                       
K1741    DS    CL0004              MV2554                                       
K1745    DS    CL0004              MV3564                                       
K1749    DS    CL0004              MV5564                                       
K1753    DS    CL0004              MV55+                                        
K1757    DS    CL0004              MV1824                                       
K1761    DS    CL0004              MV2149                                       
K1765    DS    CL0004              MV2154                                       
K1769    DS    CL0004              MV21+                                        
K1773    DS    CL0004              MV1524                                       
K1777    DS    CL0004              LHWC18                                       
K1781    DS    CL0004              LHWC12                                       
K1785    DS    CL0004              LHWC6                                        
K1789    DS    CL0004              NHWC18                                       
K1793    DS    CL0004              NHWC12                                       
K1797    DS    CL0004              NHWC6                                        
         EJECT                                                                  
         PRINT ON                                                               
*        DEDEMTABD                                                              
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
*        DDCOMFACS                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070DENI09I   02/22/18'                                      
         END                                                                    
