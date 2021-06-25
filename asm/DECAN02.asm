*          DATA SET DECAN02    AT LEVEL 036 AS OF 02/14/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DECAN02A                                                                 
         TITLE 'DEMO CONVERSION'                                                
DECAN98  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DECAN98,R3,R5    R3 = NMOD REGISTER                            
         USING DEMCOND,R8                                                       
         L     RC,ARREC                                                         
         LA    RC,4(RC)            RC = ARREC -> TPDD                           
         USING TPDD,RC                                                          
         L     R2,AIREC            R2 = IREC                                    
         USING INTERD,R2                                                        
         B     *+4(R1)                                                          
         SPACE 2                                                                
         B     READ                GET INPUT (ARREC - INT)                      
         B     CNVWR               CONVERT TO OUTPUT (INT - WORK)               
         B     MORET               CLOSE INPUT                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
READ     CLI   INTAPESW,1                                                       
         BE    OPENOK                                                           
         OPEN  (IN1,(INPUT))                                                    
*        MVI   INTAPESW,1          MARK AS OPENED                               
         XC    STALIST,STALIST                                                  
         CLI   RELOFRST,1                                                       
         BNE   OPENOK                                                           
         MVI   RELOFRST,0                                                       
         EJECT                                                                  
OPENOK   L     R4,ARREC                                                         
         XC    0(4,R4),0(R4)                                                    
         MVC   0(2,R4),=Y(RRECL+4)                                              
         LA    R4,4(R4)                                                         
         CLI   BYPREAD,1                                                        
         BE    PROCESS             GO PROCESS RECD                              
         GET   IN1,(R4)                                                         
*                                                                               
PROCESS  DS    0H                                                               
         CLC   TPDFILE,=C'TPD'     ONLY ONE RECD TYPE ON THIS TAPE              
         BE    *+6                                                              
         DC    H'0'                WRONG TAPE                                   
         CLC   =C'441',TPDMKT      MONTREA                                      
         BNE   *+10                                                             
         MVC   TPDMKT,=C'415'      IS REALLY MONTREAL FRANCO                    
*                                                                               
         XC    DUB,DUB                                                          
         PACK  DUB,TPDMKT          CONVERT MARKET NUMBER                        
         CVB   RE,DUB                                                           
         STCM  RE,3,INTMRKT                                                     
         BAS   RE,FLTMKT                                                        
         CLI   PASSFLT,C'Y'                                                     
         BNE   OPENOK                                                           
*                                                                               
         CLC   TPDSTN(4),=C'A-CH'                                               
         BNE   CHUM10                                                           
         CLC   INTMRKT,=H'490'      A-CH IN VANCOUVER IS CIVI                   
         BNE   *+14                                                             
         MVC   TPDSTN(4),=C'CIVI'                                               
         B     CHUMXX                                                           
         CLC   INTMRKT,=H'440'      IN TORONTO IT IS CKVR                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TPDSTN(4),=C'CKVR'                                               
         B     CHUMXX                                                           
*                                                                               
CHUM10   CLC   TPDSTN(4),=C'CITY'   CITY IN CALGARY IS CKAL                     
         BNE   CHUMXX                                                           
         CLC   INTMRKT,=H'484'                                                  
         BNE   CHUMXX                                                           
         MVC   TPDSTN(4),=C'CKAL'                                               
*                                                                               
CHUMXX   CLC   =C'CH  ',TPDSTN                                                  
         BNE   CHFIXX                                                           
         CLC   INTMRKT(2),=H'440'                                               
         BNE   *+10                                                             
         MVC   TPDSTN(4),=C'CHCH'                                               
         CLC   INTMRKT,=H'490'                                                  
         BNE   *+10                                                             
         MVC   TPDSTN(4),=C'CHEK'                                               
         CLC   =C'CH  ',TPDSTN                                                  
         BNE   CHFIXX                                                           
         DC    H'0'                                                             
CHFIXX   DS    0C                                                               
         MVI   INTBTYP,0                                                        
         MVC   INTSTA(4),TPDSTN    STATION CALL LETTERS                         
         CLC   TPDSTN,=C'HT/PT'    HUT/PUT RECD?                                
         BNE   PROC10                                                           
         XC    DUB,DUB                                                          
         MVI   INTSTA,C'0'                                                      
         MVC   INTSTA+1(3),TPDMKT                                               
         MVI   INTSTYP,PARENTE     HUT INTSTYP=2                                
*                                                                               
PROC10   MVI   INTSTA+4,C'T'                                                    
         MVC   INTBOOK,NSIBOOKY                                                 
         OC    NSIBOOKY,NSIBOOKY   IF NOT DEFINED, PULL FROM TAPE               
         BNZ   PROC20                                                           
         GOTO1 VNSIWEEK,DMCB,TPDYEAR+2,(1,VGETDAY),VADDAY,VDATCON               
         MVC   NSIBOOKY(1),4(R1)    YEAR                                        
         MVC   NSIBOOKY+1(1),0(R1)  WEEK                                        
PROC20   MVC   INTBOOK,NSIBOOKY                                                 
*                                                                               
         LA    RF,DAYTAB           CONVERT DAY OF WEEK TO BIT                   
PROC25   CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                INVALID DAY FIELD ON RECD                    
         CLC   TPDDAY(10),0(RF)                                                 
         BE    *+12                                                             
         LA    RF,L'DAYTAB(RF)                                                  
         B     PROC25                                                           
         MVC   INTDAY,10(RF)                                                    
*                                                                               
         PACK  DUB,TPDSTIM(2)      START HOUR                                   
         CVB   R1,DUB                                                           
         MH    R1,=H'100'          HOURS X 100                                  
         PACK  DUB,TPDSTIM+2(2)    START MINUTE                                 
         CVB   R0,DUB              MINUTES                                      
         AR    R1,R0                                                            
         CH    R1,=H'2400'         TEST FOR PAST MIDNIGHT                       
         BNH   *+8                 BEFORE OR AT MIDNIGHT                        
         SH    R1,=H'2400'         SUBTRACT FOR MIL TIME                        
         STCM  R1,3,HALF           SAVE MILITARY START TIME                     
         GOTO1 VHRTOQH,DMCB,HALF,INTSQH       START QH                          
*                                                                               
         SR    RF,RF                                                            
         PACK  DUB,TPDDUR          DURATION IN MINUTES                          
         CVB   RF,DUB                                                           
         CLC   TPDSTN,=C'HP/TP'    SET A 30 MIN DUR ON HUT/PUT RECD             
         BNE   *+12                                                             
         LA    RF,30               GIVE HUT/PUT A 30 MIN DUR                    
         B     PROC30                                                           
         CLI   INTDAY,X'95'        M-F RECD?                                    
         BNE   PROC30                                                           
         LTR   RF,RF               MAKE SURE NO DIVIDE BY ZERO                  
         BZ    PROC30                                                           
         SR    RE,RE                                                            
         D     RE,=F'5'            DIVIDE DUR BY 5 DAYS                         
*                                                                               
PROC30   STC   RF,INTADUR          SAVE DURATION IN MINUTES                     
*                                                                               
         ZIC   R0,INTSQH           CALC EQH FROM SQH                            
         LA    RF,8(RF)            ADD 8 BEFORE CNV TO QH                       
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         CH    RF,=H'1'            TEST FOR DURATION OF AT LEAST ONE            
         BH    *+8                                                              
         LA    RF,1                                                             
         AR    R0,RF               SQH PLUS DUR                                 
         BCTR  R0,0                LESS ONE GIVE END QH                         
         STC   R0,INTEQH                                                        
*                                                                               
*CHECK FOR FIRST TIME FOR STATION                                               
         LA    R9,STALIST                                                       
CHKSTA   CLI   0(R9),0             END OF LIST                                  
         BNE   CHKSTA2              NO TRY NEXT                                 
         MVI   BYPREAD,1            YES - INSERT AND SEND 'M' RECORD            
         MVC   0(5,R9),INTSTA                                                   
         MVI   STASW,1                                                          
         BAS   RE,SETKEY                                                        
         B     EXIT                                                             
*                                                                               
CHKSTA2  CLC   0(5,R9),INTSTA      STATION IN LIST                              
         BE    *+12                 YES - DO NORMAL CONVERSION                  
         LA    R9,5(R9)                                                         
         B     CHKSTA                                                           
*                                                                               
         MVI   STASW,0             RESET CREATE 'M' RECORD SWITCHES             
         MVI   BYPREAD,0                                                        
         MVI   INTWEEKS,1          SET UP SINGLE WEEK                           
         MVC   INTPNAM,TPDPNAME                                                 
         LA    R9,1                                                             
         BAS   RE,SLOTDEM          SLOT DEMOS INTO INTACCS                      
         BAS   RE,SETKEY           SET IREC KEY                                 
         TM    INTSTA,X'F0'        FOR HUT/PUT ONLY...                          
         BNO   EXIT                DON'T DO FOR HUT RECDS                       
         CLI   INTWEEKS,X'0F'      BYPASS 4 WEEK PUTS                           
         BE    EXIT                                                             
*        GOTO1 VDEMKTAC,DMCB,(C'P',DEMCOND)       FOR HUTS/PUTS                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* M1 RECORD ROUTINES   (NOT ON THIS TAPE FORMAT)                                
***********************************************************************         
M1REC    DS    0H                                                               
         XC    STALIST,STALIST     CLEAR MARKET STATION LIST                    
*        MVC   STTNAFF,TPDSTN      COPY STATION/AFFIL LIST FOR MYSELF           
*        MVC   NPRES,M1ROUND                                                    
         MVC   NSIBOOKY(2),FILTBOOK+1                                           
         CLI   FRST,1                                                           
         BNE   EXIT                                                             
         GOTO1 VDEMKTAC,DMCB,(C'I',DEMCOND),MODLIST                             
         LA    R9,INTACCS-INTERD                                                
         GOTO1 VDEMKTAC,DMCB,(C'S',DEMCOND),=C'DISP',(R9)                       
         MVI   FRST,0                                                           
         B     OPENOK                                                           
*                                                                               
* M2 RECORD ROUTINES               (NOT ON THIS TAPE FMT)                       
M2REC    LA    R1,424                                                           
         LA    R6,M2RECA                                                        
         MOVE  ((R6),(R1)),(R4)                                                 
         MVI   INTRTYP,C'U'                                                     
         MVC   INTSTA(5),=C'UUUUT'                                              
         MVC   NSIBOOKY(2),FILTBOOK+1                                           
         MVC   INTBOOK(2),NSIBOOKY                                              
         L     R6,AIREC                                                         
*        L     R7,ARREC                                                         
*        LA    R7,4(R7)                                                         
*        BAL   RE,SLOTUNV          SLOT UNIVS INTO INTACCS                      
*        GOTO1 =V(BDPROC)                                                       
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*SLOTDEM -  SLOT DEMOS FROM TAPE TO IREC                                        
**********************************************************************          
SLOTDEM  NTR1                                                                   
         LA    R6,SLOTTAB                                                       
SLOT10   CLC   =X'FFFF',0(R6)                                                   
         BE    SLOTX                                                            
         SR    R1,R1                                                            
         ICM   R1,3,0(R6)          R1=DSP TO DEMO ON TAPE (TPD)                 
         AR    R1,RC               R1->DEMO (DSP + BASE)                        
         ZIC   RF,3(R6)            LENGTH OF INPUT FLD -1                       
*-->                                                                            
         XC    WORK(10),WORK       HANDLE DECIMAL PLACE ON TAPE                 
         EXMVC RF,WORK,0(R1)                                                    
         LA    RE,WORK-1                                                        
         AR    RE,RF                                                            
         CLI   0(RE),C'.'                                                       
         BNE   *+16                                                             
         MVC   0(1,RE),1(RE)                                                    
         MVI   1(RE),C'.'                                                       
         BCTR  RF,0                REDUCE LENGTH OF INPUT FIELD                 
         LA    R1,WORK                                                          
*-->                                                                            
         ZIC   RE,5(R6)            RE=DSP EQU TO OUTPUT BKT IN IREC             
         SLL   RE,2                EQU * 4 BYTE BKTS                            
         XC    DUB,DUB                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1)                                                      
         CVB   R0,DUB              R0=FULL WORD DEMO VALUE                      
         AR    R1,RF                                                            
*                                                                               
*        CLC   0(2,R6),=AL2(DHIL35R-TPDD)                                       
*        BE    SLOT40              DON'T ROUND                                  
*        CLC   0(2,R6),=AL2(DHI3575R-TPDD)                                      
*        BE    SLOT40              DON'T ROUND                                  
*        CLC   0(2,R6),=AL2(DHI75R-TPDD)                                        
*        BE    SLOT40              DON'T ROUND                                  
*                                                                               
*        CLI   1(R1),C'.'          WAS THIS A DECIMALED ENTRY?                  
*        BNE   SLOT40              NO, DON'T ROUND                              
*        LA    R1,5                                                             
*        AR    R1,R0               ROUND OFF                                    
*        SR    R0,R0                                                            
*        D     R0,=F'10'                                                        
*        LR    R0,R1                                                            
*                                                                               
SLOT40   LA    R1,INTACCS                                                       
         AR    R1,RE               R1=A(OUTPUT BKT)                             
         STCM  R0,15,0(R1)         STORE DEMO VALUE                             
         LA    R6,L'SLOTTAB(R6)                                                 
         B     SLOT10                                                           
                                                                                
SLOTX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*SETKEY - BUILDS KEYS ON IREC                                                   
***********************************************************************         
SETKEY   NTR1                                                                   
         L     R6,AIREC                                                         
         USING DRKEY,R6                                                         
         LA    R6,4(R6)                                                         
*                                                                               
         CLC   INTMRKT,=H'415'     MONTREAL FRANCO                              
         BE    SETKSP              IS ALWAYS SPILL                              
*&&DO                                                                           
         LA    RE,SPILLTAB         SPILL ONLY STATIONS                          
         MVI   INTSPILL,0                                                       
SETKSPL  CLC   INTSTA(4),0(RE)                                                  
         BE    SETKSP                                                           
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   SETKSPL                                                          
         B     *+8                                                              
*&&                                                                             
*          DATA SET DECAN0208  AT LEVEL 024 AS OF 05/01/02                      
         LA    RE,HOMETAB                                                       
SETKSPL  CLI   0(RE),X'FF'                                                      
         BE    SETKSP                                                           
         CLC   INTSTA(4),0(RE)                                                  
         BNE   SETKSPNX                                                         
         CLC   INTMRKT(2),4(RE)                                                 
         BNE   SETKSPNX                                                         
         B     SETKSP0                                                          
SETKSPNX LA    RE,L'HOMETAB(RE)                                                 
         B     SETKSPL                                                          
SETKSP0  MVI   INTSPILL,0          NOT A SPILL STATION                          
         B     *+8                                                              
SETKSP   MVI   INTSPILL,C'Y'       NOT IN LIST - SET AS SPILL                   
*                                                                               
         MVI   DRCODE,DRCODEQU                                                  
         MVI   INTRTYP,C'R'                                                     
         MVI   DRMEDIA,C'C'        MEDIA = CANADA                               
         MVI   DRSRC,C'N'                                                       
         MVC   DRSTAT,INTSTA                                                    
         MVC   DRSTYP,INTSTYP      SET STATYP IN KEY                            
         MVI   INTSTYP,0           SET STATYP TO ZERO FOR OUTPUT PHASE          
         MVC   DRBOOK,INTBOOK                                                   
         MVC   DRBTYP,INTBTYP      SET INTERNAL BOOK TYPE                       
         CLI   BOOKTYPE,0          ALLOW OVERRIDE                               
         BE    *+10                                                             
         MVC   DRBTYP,BOOKTYPE                                                  
         CLI   STASW,1             CREAT 'M' RECORD SWITCH                      
         BE    SETKEY3                                                          
         MVC   DRHIGHD,INTDAY                                                   
         MVC   DRHIQHR,INTSQH                                                   
         ZIC   RF,INTWEEKS                                                      
         SR    RE,RE                                                            
         SLL   RF,28                                                            
         LA    R0,4                                                             
         LA    R1,0                                                             
SETKEY2  SLDL  RE,1                                                             
         LTR   RE,RE                                                            
         BZ    *+10                                                             
         LA    R1,1(R1)                                                         
         SR    RE,RE               CLEAR WEEK INDICATOR                         
         BCT   R0,SETKEY2                                                       
         STC   R1,DRHIQHR+1                                                     
         MVC   DRHIQHR+6(L'INTWEEKS),INTWEEKS                                   
*        L     R7,AIREC                                                         
*???     MVC   DRHIQHR+2(4),D0140      D0140=DMETROB FROM ADPROC                
         B     SETKEYX                                                          
SETKEY3  MVI   DRCODE,C'M'                                                      
         MVI   INTRTYP,C'M'                                                     
         B     SETKEYX                                                          
SETKEYX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*CNVWR  - SORT IRECS TO CREATE SREC FOR OPHASE   ???????                        
***********************************************************************         
CNVWR    L     R2,ASREC            SET TO SORT RECORD                           
         L     R6,ASREC                                                         
         LA    R6,4(R6)                                                         
         USING DRKEY,R6                                                         
         CLI   0(R6),C'R'          PURGE RATINGS RECORDS ONLY                   
         BNE   EXIT                                                             
         CLC   DRSTAT(5),PREVSTAT  SAME STATION                                 
         BNE   GOODREC             NO - CANNOT BE PARENT OF S1                  
         CLC   DRSTYP,PREVSTYP                                                  
         BE    GOODREC                                                          
         CLI   PREVSTYP,1          IF PREV NOT P+S1 MUST BE OK                  
         BNE   GOODREC                                                          
         CLI   DRSTYP,2            IS IT A PARENT                               
         BNE   GOODREC             NO - CANNOT HAVE S1                          
         MVI   INTRTYP,0           YES - PARENT OF S1 -PURGE                    
         B     EXIT                                                             
GOODREC  MVC   PREVSTYP,DRSTYP                                                  
         MVC   PREVSTAT,DRSTAT                                                  
         CLI   INTWEEKS,1          WEEKLY DATA                                  
         BNE   *+8                                                              
         OI    INTWEEKS,B'00100000' SET TYPICAL BIT                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*FLTMKT - IF JCL SPECIFIED, ONLY TAKE CERTAIN MARKETS                           
***********************************************************************         
FLTMKT   SR    RF,RF                                                            
         MVI   PASSFLT,C'Y'                                                     
         ICM   RF,1,FILTMRKT                                                    
         BZR   RE                                                               
         OC    INTMRKT,INTMRKT                                                  
         BZR   RE                                                               
         LA    R1,FILTMRKT+1                                                    
         CLC   INTMRKT,0(R1)                                                    
         BER   RE                                                               
         LA    R1,L'INTMRKT(R1)                                                 
         BCT   RF,*-12                                                          
         MVI   PASSFLT,C'N'                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
*END OF TAPE HANDLING                                                           
***********************************************************************         
MORET    DS    0H'0'                                                            
ENDJOB   CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
                                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
RRECL    EQU   393                 L(RATING SERVICE'S RECORD)                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
PS1E     EQU   1                                                                
PARENTE  EQU   2                                                                
PS2E     EQU   4                                                                
S1EQU    EQU   8                                                                
S2EQU    EQU   16                                                               
OUTMARE  EQU   32                                                               
CANMARE  EQU   64                                                               
METROAE  EQU   1                                                                
METROBE  EQU   2                                                                
GLOBIND  EQU   4                                                                
MODLIST  DC    C'JW',C'FN'                                                      
         DC    X'FF'                                                            
CYCTAB   DC    X'0201'             JAN                                          
         DC    X'0302'             FEB                                          
         DC    X'0403'             MAR                                          
         DC    X'0505'             MAY                                          
         DC    X'0707'             JUL                                          
         DC    X'0A0A'             OCT                                          
         DC    X'0B0B'             NOV                                          
         DC    X'0C0C'             DEC                                          
         DC    X'00'                                                            
                                                                                
DAYTAB   DS    0CL11                                                            
         DC    C'MONDAY    ',X'10'                                              
         DC    C'TUESDAY   ',X'20'                                              
         DC    C'WEDNESDAY ',X'30'                                              
         DC    C'THURSDAY  ',X'40'                                              
         DC    C'FRIDAY    ',X'50'                                              
         DC    C'SATURDAY  ',X'60'                                              
         DC    C'SUNDAY    ',X'70'                                              
         DC    C'MONDAY-FRI',X'95'                                              
         DC    C'MONDAY-WED',X'95'                                              
         DC    X'FF'                                                            
                                                                                
HOMETAB  DC    0CL6                                                             
*CALGARY/LETHBRIDGE MARKET                                                      
         DC    CL4'CBRT',AL2(484)                                               
         DC    CL4'CFCN',AL2(484)                                               
         DC    CL4'CTVC',AL2(484) REPLACES CFCN 1/27/03                         
         DC    CL4'CKAL',AL2(484)                                               
         DC    CL4'KAYU',AL2(484)                                               
         DC    CL4'KHQ ',AL2(484)                                               
         DC    CL4'KREM',AL2(484)                                               
         DC    CL4'KSPS',AL2(484)                                               
         DC    CL4'KXLY',AL2(484)                                               
*TORONTO MARKET                                                                 
         DC    CL4'CBC ',AL2(440)                                               
         DC    CL4'CBLT',AL2(440)                                               
         DC    CL4'CFMT',AL2(440)                                               
         DC    CL4'OMN1',AL2(440) REPLACES CFMT 1/27/03                         
         DC    CL4'CFTO',AL2(440)                                               
         DC    CL4'CHCH',AL2(440)                                               
         DC    CL4'CITY',AL2(440)                                               
         DC    CL4'CKCO',AL2(440)                                               
         DC    CL4'CKVR',AL2(440)                                               
         DC    CL4'CP24',AL2(440)                                               
         DC    CL4'CTS ',AL2(440)                                               
         DC    CL4'CTV ',AL2(440)                                               
         DC    CL4'DISC',AL2(440)                                               
         DC    CL4'SPNO',AL2(440)                                               
         DC    CL4'TVO ',AL2(440)                                               
         DC    CL4'WGRZ',AL2(440)                                               
         DC    CL4'WIVB',AL2(440)                                               
         DC    CL4'WKBW',AL2(440)                                               
         DC    CL4'WNED',AL2(440)                                               
         DC    CL4'WUTV',AL2(440)                                               
*VANCOUVER MARKET                                                               
         DC    CL4'CBUT',AL2(490)                                               
         DC    CL4'CHEK',AL2(490)                                               
         DC    CL4'CIVI',AL2(490)                                               
         DC    CL4'CIVT',AL2(490)                                               
         DC    CL4'CTVV',AL2(490) REPLACES CIVT 1/27/03                         
         DC    CL4'CKNO',AL2(490)                                               
         DC    CL4'CKVU',AL2(490)                                               
         DC    CL4'KCPQ',AL2(490)                                               
         DC    CL4'KCTS',AL2(490)                                               
         DC    CL4'KING',AL2(490)                                               
         DC    CL4'KIRO',AL2(490)                                               
         DC    CL4'KOMO',AL2(490)                                               
         DC    CL4'KSTW',AL2(490)                                               
         DC    CL4'KVOS',AL2(490)                                               
         DC    CL4'MUL ',AL2(490) EFFECTIVE 7/18/03                             
         DC    CL4'SPNP',AL2(490)                                               
         DC    X'FF'                                                            
                                                                                
SPILLTAB DC    C'A&&E '                                                         
         DC    C'ACCS'                                                          
         DC    C'GLBL'                                                          
         DC    C'SPNW'                                                          
         DC    C'TLC '                                                          
         DC    C'TOON'                                                          
         DC    C'TSN '                                                          
         DC    C'WTBS'                                                          
         DC    C'YTV '                                                          
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
         DS    0F                                                               
PASSFLT  DC    X'00'                                                            
PREVQH   DC    X'00'                                                            
HAV2WH   DC    X'00'                                                            
NFRST    DC    X'01'                                                            
FRST     DC    X'01'                                                            
RELOFRST DC    X'01'                                                            
NPRES    DC    X'00'                                                            
BYPREAD  DC    X'00'                                                            
STASW    DC    X'00'                                                            
VAPROC   DS    F                                                                
VYPROC   DS    F                                                                
STALIST  DS    CL200                                                            
M2RECA   DS    424C                                                             
NSIBOOKY DS    CL1                                                              
NSIBOOKM DS    CL1                                                              
PREVSTYP DS    CL1                                                              
PREVSTAT DS    CL5                                                              
                                                                                
*STTNAFF  DS    0XL(25*M1STATLQ)    STTN AFFL LIST FROM MKT INFO REC1           
*         DS    25XL(M1STATLQ)                                                  
         EJECT                                                                  
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00393,                                            X        
               MACRF=GM,                                               X        
               EODAD=MORET                                                      
IN1A     DS    4240C                                                            
         EJECT                                                                  
**********************************************************************          
*I-REC DEMO ORDERING                                                            
**********************************************************************          
*                                                                               
IMAHR    EQU   0                                                                
IMAHS    EQU   1                                                                
IMBHR    EQU   2                                                                
IMBHS    EQU   3                                                                
IDHHR    EQU   4                                                                
IDHI35R  EQU   5                                                                
IDI3575R EQU   6                                                                
IDHI75R  EQU   7                                                                
*                                                                               
IDV2PS   EQU   8                                                                
IDV1849S EQU   9                                                                
IDV2554S EQU   10                                                               
IDCBLHS  EQU   11                                                               
*                                                                               
IDV2PR   EQU   12                                                               
IDV18PR  EQU   13                                                               
IDV1834R EQU   14                                                               
IDV1849R EQU   15                                                               
IDV2554R EQU   16                                                               
IDV55PR  EQU   17                                                               
IDOMPR   EQU   18                                                               
IDW18PR  EQU   19                                                               
IDW1834R EQU   20                                                               
IDW1849R EQU   21                                                               
IDW2554R EQU   22                                                               
IDM18PR  EQU   23                                                               
IDM1834R EQU   24                                                               
IDM1849R EQU   25                                                               
IDM2554R EQU   26                                                               
IDTEENR  EQU   27                                                               
IDCH211R EQU   28                                                               
IDV2549R EQU   29                                                               
IDV1224R EQU   30                                                               
IDV1234R EQU   31                                                               
IDV1249R EQU   32                                                               
IDV25OR  EQU   33                                                               
IDV35OR  EQU   34                                                               
IDV1824R EQU   35                                                               
IDV3554R EQU   36                                                               
IDW2534R EQU   37                                                               
IDW3549R EQU   38                                                               
IDW3554R EQU   39                                                               
IDM3554R EQU   40                                                               
*                                                                               
ISTHHJ   EQU   41                                                               
ISHIL35J EQU   42                                                               
ISHI357J EQU   43                                                               
ISHIG75J EQU   44                                                               
ISV2PJ   EQU   45                                                               
ISV18PJ  EQU   46                                                               
ISV1834J EQU   47                                                               
ISV1849J EQU   48                                                               
ISV2554J EQU   49                                                               
ISV55PJ  EQU   50                                                               
ISOMPJ   EQU   51                                                               
ISW18PJ  EQU   52                                                               
ISW1834J EQU   53                                                               
ISW1849J EQU   54                                                               
ISW2554J EQU   55                                                               
ISM18PJ  EQU   56                                                               
ISM1834J EQU   57                                                               
ISM1849J EQU   58                                                               
ISM2554J EQU   59                                                               
ISTEENJ  EQU   60                                                               
ISW1217J EQU   61                                                               
ISCH211J EQU   62                                                               
ISV2549R EQU   63                                                               
ISV1224R EQU   64                                                               
ISV1234R EQU   65                                                               
ISV1249R EQU   66                                                               
ISV25OR  EQU   67                                                               
ISV35OR  EQU   68                                                               
ISV1824R EQU   69                                                               
ISV3554R EQU   70                                                               
ISW2534R EQU   71                                                               
ISW3549R EQU   72                                                               
ISW3554R EQU   73                                                               
ISM3554R EQU   74                                                               
*                                                                               
**********************************************************************          
*                                                                               
SLOTTAB  DS    0XL6                                                             
         DC    AL2(MAHR-TPDD,L'MAHR-1,IMAHR)                                    
         DC    AL2(MAHS-TPDD,L'MAHS-1,IMAHS)                                    
         DC    AL2(MBHR-TPDD,L'MBHR-1,IMBHR)                                    
         DC    AL2(MBHS-TPDD,L'MBHS-1,IMBHS)                                    
         DC    AL2(DHHR-TPDD,L'DHHR-1,IDHHR)                                    
*                                                                               
         DC    AL2(DHIL35R-TPDD,L'DHIL35R-1,IDHI35R)                            
         DC    AL2(DHI3575R-TPDD,L'DHI3575R-1,IDI3575R)                         
         DC    AL2(DHI75R-TPDD,L'DHI75R-1,IDHI75R)                              
         DC    AL2(DV2PS-TPDD,L'DV2PS-1,IDV2PS)                                 
         DC    AL2(DV1849S-TPDD,L'DV1849S-1,IDV1849S)                           
         DC    AL2(DV2554S-TPDD,L'DV2554S-1,IDV2554S)                           
         DC    AL2(DCBLHS-TPDD,L'DCBLHS-1,IDCBLHS)                              
*                                                                               
         DC    AL2(DV2PR-TPDD,L'DV2PR-1,IDV2PR)                                 
         DC    AL2(DV18PR-TPDD,L'DV18PR-1,IDV18PR)                              
         DC    AL2(DV1834R-TPDD,L'DV1834R-1,IDV1834R)                           
         DC    AL2(DV1849R-TPDD,L'DV1849R-1,IDV1849R)                           
         DC    AL2(DV2554R-TPDD,L'DV2554R-1,IDV2554R)                           
         DC    AL2(DV55PR-TPDD,L'DV55PR-1,IDV55PR)                              
         DC    AL2(DOMPR-TPDD,L'DOMPR-1,IDOMPR)                                 
         DC    AL2(DW18PR-TPDD,L'DW18PR-1,IDW18PR)                              
         DC    AL2(DW1834R-TPDD,L'DW1834R-1,IDW1834R)                           
         DC    AL2(DW1849R-TPDD,L'DW1849R-1,IDW1849R)                           
         DC    AL2(DW2554R-TPDD,L'DW2554R-1,IDW2554R)                           
         DC    AL2(DM18PR-TPDD,L'DM18PR-1,IDM18PR)                              
         DC    AL2(DM1834R-TPDD,L'DM1834R-1,IDM1834R)                           
         DC    AL2(DM1849R-TPDD,L'DM1849R-1,IDM1849R)                           
         DC    AL2(DM2554R-TPDD,L'DM2554R-1,IDM2554R)                           
         DC    AL2(DTEENR-TPDD,L'DTEENR-1,IDTEENR)                              
         DC    AL2(DCH211R-TPDD,L'DCH211R-1,IDCH211R)                           
*  ADDED APR/02                                                                 
         DC    AL2(DV2549R-TPDD,L'DV2549R-1,IDV2549R)                           
         DC    AL2(DV1224R-TPDD,L'DV1224R-1,IDV1224R)                           
         DC    AL2(DV1234R-TPDD,L'DV1234R-1,IDV1234R)                           
         DC    AL2(DV1249R-TPDD,L'DV1249R-1,IDV1249R)                           
         DC    AL2(DV25OR-TPDD,L'DV25OR-1,IDV25OR)                              
         DC    AL2(DV35OR-TPDD,L'DV35OR-1,IDV35OR)                              
         DC    AL2(DV1824R-TPDD,L'DV1824R-1,IDV1824R)                           
         DC    AL2(DV3554R-TPDD,L'DV3554R-1,IDV3554R)                           
         DC    AL2(DW2534R-TPDD,L'DW2534R-1,IDW2534R)                           
         DC    AL2(DW3549R-TPDD,L'DW3549R-1,IDW3549R)                           
         DC    AL2(DW3554R-TPDD,L'DW3554R-1,IDW3554R)                           
         DC    AL2(DM3554R-TPDD,L'DM3554R-1,IDM3554R)                           
*                                                                               
         DC    AL2(STHHJ-TPDD,L'STHHJ-1,ISTHHJ)                                 
         DC    AL2(SHIL35J-TPDD,L'SHIL35J-1,ISHIL35J)                           
         DC    AL2(SHI3575J-TPDD,L'SHI3575J-1,ISHI357J)                         
         DC    AL2(SHIG75J-TPDD,L'SHIG75J-1,ISHIG75J)                           
         DC    AL2(SV2PJ-TPDD,L'SV2PJ-1,ISV2PJ)                                 
         DC    AL2(SV18PJ-TPDD,L'SV18PJ-1,ISV18PJ)                              
         DC    AL2(SV1834J-TPDD,L'SV1834J-1,ISV1834J)                           
         DC    AL2(SV1849J-TPDD,L'SV1849J-1,ISV1849J)                           
         DC    AL2(SV2554J-TPDD,L'SV2554J-1,ISV2554J)                           
         DC    AL2(SV55PJ-TPDD,L'SV55PJ-1,ISV55PJ)                              
         DC    AL2(SOMPJ-TPDD,L'SOMPJ-1,ISOMPJ)                                 
         DC    AL2(SW18PJ-TPDD,L'SW18PJ-1,ISW18PJ)                              
         DC    AL2(SW1834J-TPDD,L'SW1834J-1,ISW1834J)                           
         DC    AL2(SW1849J-TPDD,L'SW1849J-1,ISW1849J)                           
         DC    AL2(SW2554J-TPDD,L'SW2554J-1,ISW2554J)                           
         DC    AL2(SM18PJ-TPDD,L'SM18PJ-1,ISM18PJ)                              
         DC    AL2(SM1834J-TPDD,L'SM1834J-1,ISM1834J)                           
         DC    AL2(SM1849J-TPDD,L'SM1849J-1,ISM1849J)                           
         DC    AL2(SM2554J-TPDD,L'SM2554J-1,ISM2554J)                           
         DC    AL2(STEENJ-TPDD,L'STEENJ-1,ISTEENJ)                              
         DC    AL2(SCH211J-TPDD,L'SCH211J-1,ISCH211J)                           
* ADDED APR/02                                                                  
         DC    AL2(SV2549R-TPDD,L'SV2549R-1,ISV2549R)                           
         DC    AL2(SV1224R-TPDD,L'SV1224R-1,ISV1224R)                           
         DC    AL2(SV1234R-TPDD,L'SV1234R-1,ISV1234R)                           
         DC    AL2(SV1249R-TPDD,L'SV1249R-1,ISV1249R)                           
         DC    AL2(SV25OR-TPDD,L'SV25OR-1,ISV25OR)                              
         DC    AL2(SV35OR-TPDD,L'SV35OR-1,ISV35OR)                              
         DC    AL2(SV1824R-TPDD,L'SV1824R-1,ISV1824R)                           
         DC    AL2(SV3554R-TPDD,L'SV3554R-1,ISV3554R)                           
         DC    AL2(SW2534R-TPDD,L'SW2534R-1,ISW2534R)                           
         DC    AL2(SW3549R-TPDD,L'SW3549R-1,ISW3549R)                           
         DC    AL2(SW3554R-TPDD,L'SW3554R-1,ISW3554R)                           
         DC    AL2(SM3554R-TPDD,L'SM3554R-1,ISM3554R)                           
         DC    X'FFFF'                                                          
**********************************************************************          
                                                                                
         EJECT                                                                  
         LTORG                                                                  
* DECAND                                                                        
       ++INCLUDE DECAND3                                                        
       EJECT                                                                    
* DEDEMFILE                                                                     
       ++INCLUDE DEDEMFILE                                                      
* DEDEMCNVD                                                                     
       ++INCLUDE DEDEMCNVD                                                      
* DEINTD                                                                        
       ++INCLUDE DEINTD                                                         
* DEINTTPT3D                                                                    
       ++INCLUDE DEINTTPT3D                                                     
* DDDPRINT                                                                      
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036DECAN02   02/14/14'                                      
         END                                                                    
