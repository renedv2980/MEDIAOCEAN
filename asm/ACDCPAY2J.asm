*          DATA SET ACDCPAY2J  AT LEVEL 003 AS OF 05/14/20                      
*PHASE ACDCP2JB                                                                 
                                                                                
         TITLE 'ACPAY2JOB Conversion + AC*UPDT data processing'                 
                                                                                
*INCLUDE ACRECTYP                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE BLDCUR                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE CASHVAL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DICTATE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE FATABOFF                                                               
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SCANNER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE LOADER                                                                 
*&&UK                                                                           
*INCLUDE PROMOTE                                                                
*INCLUDE TMPACK                                                                 
*INCLUDE TMUNPK                                                                 
*INCLUDE MEDCHA                                                                 
*&&                                                                             
                                                                                
*INCLUDE PAY2JOB                                                                
                                                                                
***********************************************************************         
* CHANGE DOCUMENTATION                                                *         
* ====================                                                *         
* USER LVL Date      CHANGE DESCRIPTION                              ==         
* ---- --- --------  ------------------------------------------------ *         
*                                                                     *         
* ABID 003 16MAY20   EXPENSES EU MF A55/5P - SEED CLAIM ON  DSRD-25289*         
* NMAL 003 16MAY20   Removed DDSIO validation check                   *         
*                    PAYMENTS - RELINK FOR ACPAY2JOB                  *         
* NMAL 002 13Aug18   Recompile - To pick latest Pay2job code          *         
*                                                                     *         
* NMAL 001 10Dec17   ACPAY2JOB PROCESSING FROM AC*UPDT DATA SPEC09021 *         
*                    & convert Acc Payments via Pay2Job to            *         
*                    JOBS AND TO REB INVOICES                         *         
*                                                                     *         
***********************************************************************         
                                                                                
* Notes                                                                         
* -----                                                                         
* - TKLU.P2MIN contains QAMEDIA test data                                       
* - TKLU.DDS.JCL(ACP2J*) for testing                                            
* - MODE=ACUP to run as MEPAY2MED equivalent based on AC*UPDT data              
* - TKLU.P2JIN contains PUBTST test data                                        
* - MODE=CONV to run as conversion (see ACDCRMP2J to remove this data)          
                                                                                
* Return code                                                                   
* -----------                                                                   
* - 00         = data processed OK                                              
* - 01         = no data to be processed                                        
* - 05         = array data level error                                         
* - 08         = global ACPAY2MED error                                         
                                                                                
* Parameter cards                                                               
* ---------------                                                               
* - AGENCY     = Y to set company as Pay2Job user                               
* - ANYLOCKS   = Required if run live against the onlines Y/N                   
* - CONSOLEM   = Console messages Y/N                                           
* - DDSIO      = Test override                                                  
* - DSPACE     = Required A/C/T                                                 
* - DUMPFOUT   = Dump out records Y/N                                           
* - MODE       = ACUP/CONV, see notes above                                     
* - OVERRIDE   = Optional to override CPY setting in CONV mode                  
* - PATCH      = Optional                                                       
* - PROCJOB    = Optional to update Jobs/Job trxs in CONV mode                  
* - PROCINV    = Optional to update REBs/Invoices in CONV mode                  
* - READDELS   = (not in use)                                                   
* - RECOVFOUT  = Create recovery output file Y/N                                
* - STARTFROM  = Skip first nnnn lines of disk/tape                             
* - TRACE      = Optional                                                       
* - UPDATEFILE = Updative Y/N                                                   
* - WHICHSYS   = Required media system                                          
* - WRITERECOV = Write to recovery Y/N                                          
                                                                                
         PRINT NOGEN                                                            
ACDCP2J  CSECT                                                                  
         NBASE WORKX-WORKD,**AP2J**,=V(REGSAVE),RA,RR=R2                        
         USING WORKD,RC                                                         
         LR    R0,RC               CLEAR LOCAL STORAGE                          
         L     R1,=A(WORKX-WORKD)                                               
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   WORKDESC,=C'*ACDCW**'  SET EYECATCHER                            
         MVC   VRSNCTRL,=X'B60901FFFFFF'                                        
         ST    R2,RELO                                                          
         ST    RB,BASERB                                                        
         ST    RA,BASERA                                                        
         ST    RD,BASERD                                                        
         L     R9,=V(CPRINT)                                                    
         ST    R9,VCPRINT          R9 = CPRINT THROUGHOUT                       
         USING DPRINT,R9                                                        
                                                                                
         GOTO1 =V(INITIAL),WORKD                                                
                                                                                
         MVI   RETCODE,0                                                        
                                                                                
         JAS   RE,DOMAIN                                                        
                                                                                
         GOTO1 =V(FINISHUP),WORKD                                               
                                                                                
         XBASE RC=RETCODE,RL=1     Set return code                              
                                                                                
***********************************************************************         
* MAIN RECORD PROCESSING CODE GOES HERE                               *         
***********************************************************************         
                                                                                
DOMAIN   NTR1  ,                                                                
                                                                                
         CLI   RUNMODE,RUNACUPQ                                                 
         JNE   DOMAIN10                                                         
                                                                                
         MVC   PRL,SPACES                                                       
         MVC   PRL(11),=C'<Open Tape>'                                          
         GOTO1 VPRINTIT                                                         
                                                                                
*        J     SKIP01                                                           
*XXXXX   DC    X'E934'                                                          
*KIP01   SR    R4,R4                                                            
*        IC    R4,XXXXXX                                                        
*        SLL   R4,8                                                             
*        IC    R4,XXXXXX+1                                                      
*        SR    R5,R5                                                            
*        SRL   R4,5                                                             
*        SRDL  R4,4                                                             
*        SRL   R5,4                                                             
*        SLDL  R4,8                                                             
*        STH   R4,HALF                                                          
*        XOUT  HALF,PRL+2,2                                                     
*        GOTO1 VPRINTIT                                                         
                                                                                
BUFF     USING JARAYD,R2                                                        
         SAM31 ,                                                                
         L     R2,AJARBUFF         Initialise buffer for all tape data          
         MVI   BUFF.JARDFIL,JARDEOT                                             
         ST    R2,ABUFFCPY                                                      
         SAM24 ,                                                                
                                                                                
         XR    R5,R5               Count entries                                
         LH    R6,SKIPNTRS                                                      
         MVI   TAPERROR,0                                                       
                                                                                
TAPE     USING JARAYD,R4                                                        
         L     R3,=A(P2JIN)        Open tape                                    
         ST    R3,AP2JIN                                                        
         OPEN  ((R3),INPUT)                                                     
         TM    48(R3),X'10'                                                     
         JZ    *+2                 DD statement missing                         
                                                                                
DOMAIN02 LA    R4,INDATA                                                        
         GET   (3),(4)                                                          
                                                                                
         CLI   TAPE.JARDFIL,JARDEOT                                             
         JE    *+2                 (should have gone to EODAD=)                 
                                                                                
         CHI   R6,0                Anything to skip?                            
         JE    DOMAIN04                                                         
         JCT   R6,DOMAIN02                                                      
                                                                                
DOMAIN04 AHI   R5,1                Add entry and copy to table                  
         C     R5,=A(JARAYBU#)                                                  
         JH    *+2                 JARAYBU# needs to be increased               
                                                                                
         CHI   R5,1                Ensure 1st entry is company header           
         JH    DOMAIN06                                                         
         CLC   TAPE.JARDPDA,FFS                                                 
         JNE   *+2                                                              
                                                                                
DOMAIN06 DS    0H                                                               
         SAM31 ,                                                                
         MVC   BUFF.JARDFIL(JARAYLQ),TAPE.JARDFIL                               
         XC    BUFF.JARDERR,BUFF.JARDERR                                        
         AHI   R2,JARAYLQ          ensure error is naught                       
         MVI   BUFF.JARDFIL,JARDEOT                                             
         SAM24 ,                                                                
         J     DOMAIN02            next entry                                   
         DROP  TAPE                                                             
                                                                                
DOMAIN08 L     R3,AP2JIN           EOT here - so all data in table              
         CLOSE ((R3))                                                           
                                                                                
         MVC   PRL,SPACES                                                       
         MVC   PRL(12),=C'<Close Tape>'                                         
         MVC   PRL+20(18),=C'Number of entries:'                                
         EDITR (R5),(8,PRL+39),0,ALIGN=LEFT                                     
         GOTO1 VPRINTIT                                                         
                                                                                
         ST    R5,TAPECNTR         Save number of entries                       
                                                                                
         CHI   R5,0                Any entry at all?                            
         JH    DOMAIN10                                                         
         MVI   RETCODE,1                                                        
         J     DOMAINX                                                          
                                                                                
DOMAIN10 DS    0H                                                               
         ZAP   CNTATRN,PZERO                                                    
         ZAP   CNTPTRN,PZERO                                                    
         ZAP   CNTUTRN,PZERO                                                    
         ZAP   CNTUACC,PZERO                                                    
         ZAP   CNTUINV,PZERO                                                    
         ZAP   CNTERRS,PZERO                                                    
         MVI   CPRNIND,NOQ                                                      
                                                                                
         MVC   CURCOMP,THISCOMP    Single or all agencies?                      
         CLI   THISCOMP,0                                                       
         JNE   DOMAIN12                                                         
         MVI   CURCOMP,X'41'                                                    
                                                                                
DOMAIN12 CLI   CPRNIND,YESQ                                                     
         JNE   DOMAIN14                                                         
                                                                                
         MVC   PRL,SPACES                                                       
         MVC   PRL+00(18),=C'* Company totals *'                                
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(21),=C'All transactions read'                             
         EDIT  (P6,CNTATRNC),(12,PRL+30),0                                      
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(22),=C'Processed transactions'                            
         EDIT  (P6,CNTPTRNC),(12,PRL+30),0                                      
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(23),=C'Updated SJ transactions'                           
         EDIT  (P6,CNTUTRNC),(12,PRL+30),0                                      
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(23),=C'Updated SJ job accounts'                           
         EDIT  (P6,CNTUACCC),(12,PRL+30),0                                      
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(23),=C'Updated REB invoices'                              
         EDIT  (P6,CNTUINVC),(12,PRL+30),0                                      
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(14),=C'Pay2Job errors'                                    
         EDIT  (P6,CNTERRSC),(12,PRL+30),0                                      
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         GOTO1 VPRINTIT                                                         
                                                                                
DOMAIN14 DS    0H                  Get this/next company                        
         GOTOR NXTCPY                                                           
         JNE   DOMAIN80                                                         
                                                                                
         CLI   RUNMODE,RUNACUPQ                                                 
         JNE   DOMAIN16                                                         
                                                                                
         BRAS  RE,DOP2J            Process this company's data                  
         J     DOMAIN12                                                         
                                                                                
         USING JARAYD,R4                                                        
DOMAIN16 L     R4,AJARAY           Init for array data                          
         MVI   JARDFIL,JARDEOT                                                  
         LHI   R5,JARAY#                                                        
         OC    THISULA,SPACES                                                   
                                                                                
KEY      USING LDGRECD,R6                                                       
DOMAIN18 LA    R6,AKEY             Read for ledger(s) present                   
         MVC   KEY.LDGKEY,SPACES                                                
         MVC   KEY.LDGKCPY,CURCOMP                                              
         MVI   KEY.LDGKUNT,C'S'                                                 
         MVC   KEY.LDGKLDG,CURCLDG                                              
         CLC   THISULA,SPACES                                                   
         JNH   DOMAIN20                                                         
         MVC   KEY.LDGKLDG,THISULA+1                                            
                                                                                
DOMAIN20 MVC   TESTKEY,KEY.LDGKEY                                               
         MVI   IOIND,0                                                          
         GOTO1 VACDIRIO,ACHIDMCB                                                
         CLC   TESTKEY,KEY.LDGKEY                                               
         JE    DOMAIN30                                                         
*&&UK                                                                           
         CLI   CURCLDG,C'V'                                                     
         JNE   DOMAIN22                                                         
         MVI   CURCLDG,C'X'                                                     
         J     DOMAIN18                                                         
DOMAIN22 CLI   CURCLDG,C'X'                                                     
         JE    DOMAIN26                                                         
         DC    H'0'                                                             
*&&                                                                             
***  NMALIK  CHANGED CODE TO ACCOMODATE SV,SY,SX,SW LEDGERS FOR US              
*&&US                                                                           
         CLI   CURCLDG,C'V'                                                     
         JNE   DOMAIN22                                                         
         MVI   CURCLDG,C'W'                                                     
         J     DOMAIN18                                                         
                                                                                
DOMAIN22 CLI   CURCLDG,C'W'                                                     
         JNE   DOMAI22A                                                         
         MVI   CURCLDG,C'X'                                                     
         J     DOMAIN18                                                         
                                                                                
DOMAI22A CLI   CURCLDG,C'X'                                                     
         JNE   DOMAI22B                                                         
         MVI   CURCLDG,C'Y'                                                     
         J     DOMAIN18                                                         
                                                                                
DOMAI22B CLI   CURCLDG,C'Y'                                                     
         JE    DOMAIN26                                                         
         DC    H'0'                                                             
*&&                                                                             
                                                                                
DOMAIN26 BRAS  RE,DOP2J                                                         
                                                                                
         CLI   THISCOMP,0                                                       
         JNE   DOMAIN80                                                         
         LLC   R1,CURCOMP                                                       
         AHI   R1,1                                                             
         STC   R1,CURCOMP                                                       
         J     DOMAIN12                                                         
         DROP  KEY                                                              
                                                                                
KEY      USING TRNRECD,R6                                                       
DOMAIN30 LA    R6,AKEY             Read transactions                            
         MVC   KEY.TRNKEY,SPACES                                                
         MVC   KEY.TRNKCPY,CURCOMP                                              
         MVI   KEY.TRNKUNT,C'S'                                                 
         MVC   KEY.TRNKLDG,CURCLDG                                              
         CLC   THISULA,SPACES                                                   
         JNH   DOMAIN32                                                         
         MVC   KEY.TRNKULA,THISULA                                              
                                                                                
DOMAIN32 MVI   KEY.TRNKDATE,X'41'                                               
         MVC   TESTKEY,KEY.TRNKEY                                               
         GOTO1 ,ACHIDMCB                                                        
         J     DOMAIN36                                                         
                                                                                
DOMAIN34 LA    R6,AKEY                                                          
         GOTO1 ,ACSQDMCB                                                        
                                                                                
DOMAIN36 MVI   IOIND,0                                                          
         AP    CNTATRN,PONE                                                     
         AP    CNTATRNC,PONE                                                    
         GOTO1 VACDIRIO                                                         
                                                                                
         CLC   KEY.TRNKDA,=X'00000000'   * debugging *                          
         JNE   DOMAIN38                                                         
         XR    R0,R0                                                            
                                                                                
DOMAIN38 CLC   TESTKEY(3),KEY.TRNKEY     Still Company/unit/ledger?             
         JNE   DOMAIN40                                                         
         CLC   THISULA,SPACES                                                   
         JE    DOMAIN50                                                         
         CLC   THISULA,KEY.TRNKULA                                              
         JE    DOMAIN50                                                         
         J     DOMAIN46                                                         
                                                                                
*&&UK                                                                           
DOMAIN40 CLI   CURCLDG,C'V'                                                     
         JNE   DOMAIN42                                                         
         MVI   CURCLDG,C'X'                                                     
         J     DOMAIN18                                                         
                                                                                
DOMAIN42 CLI   CURCLDG,C'X'                                                     
         JE    DOMAIN46                                                         
         DC    H'0'                                                             
*&&                                                                             
**                                                                              
***  NMALIK  CHANGED CODE TO ACCOMODATE SV,SY,SX,SW LEDGERS FOR US              
*&&US                                                                           
DOMAIN40 CLI   CURCLDG,C'V'                                                     
         JNE   DOMAIN42                                                         
         MVI   CURCLDG,C'W'                                                     
         J     DOMAIN18                                                         
                                                                                
DOMAIN42 CLI   CURCLDG,C'W'                                                     
         JNE   DOMAI42A                                                         
         MVI   CURCLDG,C'X'                                                     
         J     DOMAIN18                                                         
                                                                                
DOMAI42A CLI   CURCLDG,C'X'                                                     
         JNE   DOMAI42B                                                         
         MVI   CURCLDG,C'Y'                                                     
         J     DOMAIN18                                                         
                                                                                
DOMAI42B CLI   CURCLDG,C'Y'                                                     
         JE    DOMAIN46                                                         
         DC    H'0'                                                             
*&&                                                                             
                                                                                
DOMAIN46 BRAS  RE,DOP2J                                                         
                                                                                
         CLI   THISCOMP,0          Next Company or End of Processing            
         JNE   DOMAIN80                                                         
         LLC   R1,CURCOMP                                                       
         AHI   R1,1                                                             
         STC   R1,CURCOMP                                                       
         J     DOMAIN12                                                         
                                                                                
DOMAIN50 GOTO1 VRECTYP,DMCB,(C'I',KEY.TRNRECD)                                  
                                                                                
         CLI   0(R1),ACRTTRN                                                    
**       JE    DOMAIN53                                                         
         JE    DOMAIN52                                                         
         CLI   0(R1),ACRTTRNA                                                   
         JNE   DOMAIN34                                                         
                                                                                
DOMAIN52 CLC   KEY.TRNKDATE,STADAT1      Match on period (STADAT2)              
         JL    DOMAIN34                                                         
                                                                                
DOMAIN53 CLI   KEY.TRNKSTYP,TRNTPAYS     Payments?                              
         JE    DOMAIN54                                                         
*&&UK*&& CLI   KEY.TRNKSTYP,TRNTTRFS     German transfers?                      
*&&UK*&& JE    DOMAIN54                                                         
         CLI   KEY.TRNKSTYP,TRNTCHQS     /cheques                               
         JE    DOMAIN54                                                         
         CLI   KEY.TRNKSTYP,TRNTADPY     /marker                                
         JE    DOMAIN54                                                         
         CLI   KEY.TRNKSTYP,TRNTVOID     /marker void                           
         JNE   DOMAIN34                                                         
                                                                                
DOMAIN54 TM    KEY.TRNKSTAT,TRNSREVS     No reversals                           
         JNZ   DOMAIN34                                                         
                                                                                
         LA    RE,KEYTAB           exceptions                                   
         J     DOMAIN58                                                         
                                                                                
DOMAIN56 AHI   RE,KEYTABLQ                                                      
                                                                                
DOMAIN58 CLI   0(RE),X'FF'                                                      
         JE    DOMAIN60                                                         
         CLC   THISSEAL,0(RE)                                                   
         JNE   DOMAIN56                                                         
         CLC   KEY.TRNKEY,2(RE)                                                 
         JNE   DOMAIN56                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+00(17),=C'Exception skipped'                                 
         XOUT  KEY.TRNKDA,PRL+20,04                                             
         GOTO1 VPRINTIT                                                         
         J     DOMAIN34                                                         
                                                                                
         USING TRNRECD,R7                                                       
DOMAIN60 MVI   IOIND,0                                                          
         L     R7,AIOAREA1         get record for pay details                   
         TM    KEY.TRNKSTAT,TRNSARCH                                            
         JNZ   DOMAIN62                                                         
         GOTO1 VACMSTIO,ACGTDMCB                                                
         J     DOMAIN64                                                         
                                                                                
DOMAIN62 GOTO1 VACARCIO,ACGTDMCB                                                
                                                                                
         USING TRNELD,R2                                                        
DOMAIN64 LA    R2,TRNRFST          Record data processing                       
         CLI   TRNEL,TRNELQ                                                     
         JE    DOMAIN66                                                         
         DC    H'0'                                                             
                                                                                
DOMAIN66 TM    TRNSTAT,TRNSDR      Skip credits                                 
         JZ    DOMAIN34                                                         
                                                                                
         USING MPYELD,R3                                                        
         LR    R3,R2                                                            
                                                                                
DOMAIN68 LLC   R1,MPYLN                                                         
         AR    R3,R1                                                            
         CLI   MPYEL,0                                                          
         JE    DOMAIN34                                                         
         CLI   MPYEL,MPYELQ                                                     
         JNE   DOMAIN68                                                         
         CLC   MPYNO,SPACES                                                     
         JNH   DOMAIN34                                                         
                                                                                
         MVI   JARDFIL,JARDFAQ     add to table                                 
         TM    KEY.TRNKSTAT,TRNSARCH                                            
         JNZ   DOMAIN70                                                         
         MVI   JARDFIL,JARDFMQ                                                  
                                                                                
DOMAIN70 MVC   JARDPDA,KEY.TRNKDA                                               
         XC    JARDERR,JARDERR                                                  
         MVC   JARDATE,MPYDTE                                                   
         MVC   JARPAYR,MPYNO                                                    
         MVI   JARPREV,0           n/a as passing debits                        
                                                                                
         AHI   R4,JARAYLQ                                                       
         MVI   JARDFIL,JARDEOT                                                  
         SHI   R5,1                                                             
         LTR   R5,R5                                                            
         JNZ   DOMAIN34                                                         
                                                                                
         MVC   AKEYSV,KEY.TRNKEY   If table full process and carry on           
                                                                                
         BRAS  RE,DOP2J                                                         
                                                                                
         L     R4,AJARAY                                                        
         MVI   JARDFIL,JARDEOT                                                  
         LHI   R5,JARAY#                                                        
         MVC   KEY.TRNKEY,AKEYSV                                                
         MVI   IOIND,0                                                          
         GOTO1 VACDIRIO,ACRDDMCB                                                
         J     DOMAIN34                                                         
         DROP  R2,KEY,R7,R4                                                     
                                                                                
DOMAIN80 CLI   CPRNIND,YESQ                                                     
         JNE   DOMAIN82                                                         
                                                                                
         MVC   PRL,SPACES                                                       
         MVC   PRL+00(18),=C'* Company totals *'                                
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(21),=C'All transactions read'                             
         EDIT  (P6,CNTATRNC),(12,PRL+30),0                                      
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(22),=C'Processed transactions'                            
         EDIT  (P6,CNTPTRNC),(12,PRL+30),0                                      
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(23),=C'Updated SJ transactions'                           
         EDIT  (P6,CNTUTRNC),(12,PRL+30),0                                      
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(23),=C'Updated SJ job accounts'                           
         EDIT  (P6,CNTUACCC),(12,PRL+30),0                                      
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(23),=C'Updated REB invoices'                              
         EDIT  (P6,CNTUINVC),(12,PRL+30),0                                      
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(14),=C'Pay2Job errors'                                    
         EDIT  (P6,CNTERRSC),(12,PRL+30),0                                      
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         GOTO1 VPRINTIT                                                         
                                                                                
DOMAIN82 MVC   PRL,SPACES                                                       
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(19),=C'*** File totals ***'                               
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(19),=C'-------------------'                               
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(21),=C'All transactions read'                             
         EDIT  (P6,CNTATRN),(12,PRL+30),0                                       
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(22),=C'Processed transactions'                            
         EDIT  (P6,CNTPTRN),(12,PRL+30),0                                       
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(23),=C'Updated SJ transactions'                           
         EDIT  (P6,CNTUTRN),(12,PRL+30),0                                       
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(23),=C'Updated SJ job accounts'                           
         EDIT  (P6,CNTUACC),(12,PRL+30),0                                       
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(23),=C'Updated REB invoices'                              
         EDIT  (P6,CNTUINV),(12,PRL+30),0                                       
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         MVC   PRL+05(14),=C'Pay2Job errors'                                    
         EDIT  (P6,CNTERRS),(12,PRL+30),0                                       
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         GOTO1 VPRINTIT                                                         
                                                                                
DOMAIN84 CLI   RUNMODE,RUNACUPQ                                                 
         JNE   DOMAINX                                                          
         TM    TAPERROR,TAPERRSQ                                                
         JZ    DOMAIN86                                                         
         MVI   RETCODE,8                                                        
         J     DOMAINX                                                          
                                                                                
DOMAIN86 TM    TAPERROR,TAPERRDQ                                                
         JZ    DOMAINX                                                          
         MVI   RETCODE,5                                                        
                                                                                
DOMAINX  DS    0H                                                               
         XIT1  ,                                                                
                                                                                
PZERO    DC    PL1'0'                                                           
PONE     DC    PL1'1'                                                           
FFS      DC    4X'FF'                                                           
                                                                                
STADAT1  DC    AL3(STADAT1Q)                                                    
STADAT2  DC    AL2(STADAT2Q)                                                    
                                                                                
         DS    0H                                                               
                                                                                
KEYTAB   DS    0H                                                               
         DC    C'  '                                                            
         DC    X'0000000000000000000000000000000000000000'                      
         DC    X'0000000000000000000000000000000000000000'                      
         DC    X'0000'                                                          
KEYTABLQ EQU   *-KEYTAB                                                         
***      DC    C'I '                                                            
***      DC    X'D0E2C6E3E3E5F0F4F74040404040404040404040'                      
***      DC    X'C3C1D9E3D6D6D54040404040B21003F1F4F5F3F9'                      
***      DC    X'F200'                                                          
***      DC    C'J '                                                            
***      DC    X'42E2C6E6C5F0F0F1F64040404040404040404040'                      
***      DC    X'F0F0F0F6F3F9404040404040B11201F0F9F6F6F9'                      
***      DC    X'F400'                                                          
***      DC    C'J '                                                            
***      DC    X'42E2C6E6D6F0F0F1F64040404040404040404040'                      
***      DC    X'F1F8F0F0F1F8404040404040B20801F2F6F9F7F7'                      
***      DC    X'F800'                                                          
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* Get this or next company                                            *         
***********************************************************************         
                                                                                
NXTCPY   NTR1  ,                                                                
                                                                                
         MVI   CPRNIND,NOQ                                                      
                                                                                
         ZAP   CNTATRNC,PZERO                                                   
         ZAP   CNTPTRNC,PZERO                                                   
         ZAP   CNTUTRNC,PZERO                                                   
         ZAP   CNTUACCC,PZERO                                                   
         ZAP   CNTUINVC,PZERO                                                   
         ZAP   CNTERRSC,PZERO                                                   
                                                                                
         CLI   RUNMODE,RUNACUPQ                                                 
         JE    NXTCPY10                                                         
                                                                                
KEY      USING CPYRECD,R6                                                       
NXTCPY02 LA    R6,AKEY                                                          
         MVC   KEY.CPYKEY,SPACES                                                
         MVC   KEY.CPYKCPY,CURCOMP                                              
         MVC   TESTKEY,KEY.CPYKEY                                               
         MVI   IOIND,0                                                          
         GOTO1 VACDIRIO,ACHIDMCB                                                
         CLC   TESTKEY,KEY.CPYKEY                                               
         JE    NXTCPY30                                                         
         CLI   THISCOMP,0                                                       
         JNE   NXTCPYN                                                          
         CLI   KEY.CPYKCPY,X'FE'                                                
         JNL   NXTCPYN                                                          
         CLC   KEY.CPYKEY+L'CPYKCPY(L'CPYKEY-L'CPYKCPY),SPACES                  
         JE    NXTCPY30                                                         
                                                                                
NXTCPY04 LLC   R1,CURCOMP                                                       
         AHI   R1,1                                                             
         STC   R1,CURCOMP                                                       
         J     NXTCPY02                                                         
                                                                                
         USING JARAYD,R3                                                        
NXTCPY10 L     R3,AJARAY           reset array                                  
         MVI   JARDFIL,JARDEOT                                                  
         LHI   R5,JARAY#                                                        
                                                                                
         SAM31 ,                                                                
BUFF     USING JARAYD,R2                                                        
         L     R2,ABUFFCPY                                                      
         LTR   R2,R2               end of buffer?                               
         JZ    NXTCPYN                                                          
                                                                                
         MVC   BUFFCOMP,BUFF.JARDFIL                                            
         AHI   R2,JARAYLQ          skip company header                          
                                                                                
NXTCPY12 CLI   BUFF.JARDFIL,JARDEOT      end of table?                          
         JE    NXTCPY16                                                         
         CLC   BUFF.JARDPDA,FFS    look for next header                         
         JE    NXTCPY18                                                         
                                                                                
         MVC   JARDFIL(JARAYLQ),BUFF.JARDFIL                                    
         AHI   R3,JARAYLQ                                                       
         MVI   JARDFIL,JARDEOT                                                  
         JCT   R5,NXTCPY14                                                      
         DC    H'0'                increase JARAY#                              
                                                                                
NXTCPY14 AHI   R2,JARAYLQ                                                       
         J     NXTCPY12                                                         
                                                                                
NXTCPY16 XC    ABUFFCPY,ABUFFCPY   set to last time                             
         J     NXTCPY20                                                         
                                                                                
NXTCPY18 ST    R2,ABUFFCPY         set to next company header                   
         DROP  BUFF,R3                                                          
                                                                                
NXTCPY20 DS    0H                                                               
         SAM24 ,                                                                
         CLI   THISCOMP,0                                                       
         JE    NXTCPY22                                                         
         CLC   BUFFCOMP,THISCOMP                                                
         JNE   NXTCPY10                                                         
                                                                                
NXTCPY22 LA    R6,AKEY                                                          
         MVC   KEY.CPYKEY,SPACES                                                
         MVC   KEY.CPYKCPY,BUFFCOMP                                             
         MVC   TESTKEY,KEY.CPYKEY                                               
         MVI   IOIND,0                                                          
         GOTO1 VACDIRIO,ACRDDMCB                                                
         JNE   *+2                 Company should exist?                        
                                                                                
NXTCPY30 MVC   CURCOMP,KEY.CPYKCPY Save current agency                          
                                                                                
         USING CPYRECD,R7                                                       
         MVI   IOIND,0                                                          
         L     R7,AIOAREA1         Get record for pay details                   
         GOTO1 VACMSTIO,ACGTDMCB                                                
                                                                                
         USING CPYELD,R2                                                        
         LA    R2,CPYRFST          Record data processing                       
                                                                                
NXTCPY32 CLI   CPYEL,CPYELQ                                                     
         JE    NXTCPY34                                                         
         CLI   CPYEL,0                                                          
         JE    *+2                                                              
         LLC   R0,CPYLN                                                         
         AR    R2,R0                                                            
         J     NXTCPY32                                                         
                                                                                
NXTCPY34 MVC   CURCUID,CPYUID                                                   
         MVC   CURCALP,CPYALPHA                                                 
         MVI   CURCLDG,C'V'                                                     
         CLI   OVRIDE,YESQ         Test Company enabled unless override         
         JE    NXTCPY40                                                         
         CLI   CPYLN,CPYSTATD-CPYELD                                            
         JL    NXTCPY36                                                         
         TM    CPYSTATD,CPYSAP2J                                                
         JNZ   NXTCPY40                                                         
         CLI   SWAPAGY,YESQ        Change agency to be 'on' Pay2Job             
         JNE   NXTCPY36                                                         
         GOTOR UPDCPY                                                           
                                                                                
NXTCPY36 CLI   RUNMODE,RUNACUPQ    Fail if ACUP mode                            
         JE    *+2                                                              
         DROP  KEY,R2,R7                                                        
                                                                                
KEY      USING LDGRECD,R6                                                       
NXTCPY40 LA    R6,AKEY             Get SJ ledger values                         
         MVC   KEY.LDGKEY,SPACES                                                
         MVC   KEY.LDGKCPY,CURCOMP                                              
         MVI   KEY.LDGKUNT,C'S'                                                 
         MVI   KEY.LDGKLDG,C'J'                                                 
         MVI   IOIND,0                                                          
         GOTO1 VACDIRIO,ACRDDMCB                                                
         JNE   *+2                 ??? set up ???                               
                                                                                
         USING LDGRECD,R7                                                       
         MVI   IOIND,0                                                          
         L     R7,AIOAREA1                                                      
         GOTO1 VACMSTIO,ACGTDMCB                                                
                                                                                
         USING ACLELD,R2                                                        
         LA    R2,LDGRFST                                                       
                                                                                
NXTCPY42 CLI   ACLEL,ACLELQ                                                     
         JE    NXTCPY44                                                         
         CLI   ACLEL,0                                                          
         JE    *+2                 bad SJ LDGRECD                               
         LLC   R1,ACLLN                                                         
         AR    R2,R1                                                            
         J     NXTCPY42                                                         
                                                                                
NXTCPY44 MVC   BYTE1,ACLELLVA                                                   
         MVC   BYTE2,ACLELLVB                                                   
         MVC   BYTE3,ACLELLVC                                                   
                                                                                
         MVC   PCLILEN,BYTE1                                                    
         LLC   RE,BYTE1                                                         
         LLC   RF,BYTE2                                                         
         SR    RF,RE                                                            
         STC   RF,PPROLEN                                                       
         LLC   RE,BYTE2                                                         
         LLC   RF,BYTE3                                                         
         SR    RF,RE                                                            
         STC   RF,PJOBLEN                                                       
                                                                                
NXTCPYY  LHI   R1,1                                                             
         J     NXTCPYX                                                          
                                                                                
NXTCPYN  LHI   R1,0                                                             
                                                                                
NXTCPYX  CHI   R1,1                                                             
         XIT1  ,                                                                
         DROP  KEY,R2,R7                                                        
                                                                                
***********************************************************************         
* Update company record with CPYSAP2J                                 *         
***********************************************************************         
                                                                                
UPDCPY   NTR1  ,                                                                
                                                                                
         USING CPYRECD,R7                                                       
         MVI   IOIND,0                                                          
         CLI   UPDATFIL,UPUPDATQ                                                
         JNE   UCPY02                                                           
         MVI   IOIND,IORUPDTQ                                                   
                                                                                
UCPY02   L     R7,AIOAREA1                                                      
         GOTO1 VACMSTIO,ACGTDMCB                                                
                                                                                
         USING CPYELD,R2                                                        
         LA    R2,CPYRFST          Record data processing                       
                                                                                
UCPY04   CLI   CPYEL,CPYELQ                                                     
         JE    UCPY06                                                           
         CLI   CPYEL,0                                                          
         JE    *+2                                                              
         LLC   R0,CPYLN                                                         
         AR    R2,R0                                                            
         J     UCPY04                                                           
                                                                                
UCPY06   CLI   CPYLN,CPYSTATD-CPYELD                                            
         JL    UCPY08                                                           
         TM    CPYSTATD,CPYSAP2J                                                
         JNZ   *+2                                                              
         OI    CPYSTATD,CPYSAP2J                                                
         J     UCPY20                                                           
                                                                                
UCPY08   XC    ELEM,ELEM                                                        
         LLC   R1,CPYLN                                                         
         SHI   R1,1                                                             
         MVC   ELEM(0),CPYEL                                                    
         EX    R1,*-6                                                           
                                                                                
         MVI   CPYEL,FFQ                                                        
                                                                                
         LA    R2,ELEM                                                          
         MVI   CPYLN,CPYLN4Q                                                    
         OI    CPYSTATD,CPYSAP2J                                                
                                                                                
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',CPYRECD),0                      
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),CPYRECD,CPYELD                         
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
UCPY20   CLI   UPDATFIL,UPUPDATQ                                                
         JNE   UPDCPYX                                                          
         GOTO1 VACDIRIO,ACWRDMCB                                                
                                                                                
UPDCPYX  DS    0H                                                               
         XIT1  ,                                                                
         DROP  R2,R7                                                            
                                                                                
***********************************************************************         
* ACPAY2JOB call                                                      *         
***********************************************************************         
                                                                                
DOP2J    NTR1  ,                                                                
                                                                                
         USING JARAYD,R4                                                        
         L     R4,AJARAY                                                        
         CLI   JARDFIL,JARDEOT     any table entries?                           
         JE    DOP2JX                                                           
                                                                                
         CLI   CPRNIND,YESQ                                                     
         JE    DOP2J0                                                           
                                                                                
         MVI   CPRNIND,YESQ                                                     
         MVC   PRL,SPACES                                                       
         MVC   PRL(8),=C'Company:'                                              
         MVC   PRL+10(2),CURCALP                                                
         XOUT  CURCUID,PRL+14,2                                                 
         XOUT  CURCOMP,PRL+20,1                                                 
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         GOTO1 VPRINTIT                                                         
                                                                                
         USING JPYDETD,R2                                                       
DOP2J0   LA    R2,ELEM                                                          
         XC    JPYDETD(JPYDLNQ),JPYDETD                                         
                                                                                
         MVI   JPYDUPD,C'N'                                                     
         CLI   UPDATFIL,UPUPDATQ                                                
         JNE   DOP2J1                                                           
         MVI   JPYDUPD,C'Y'                                                     
                                                                                
DOP2J1   L     RE,AJARAY                                                        
         STCM  RE,B'1111',JPYDARY                                               
                                                                                
         MVC   JPYDCLL,PCLILEN                                                  
         MVC   JPYDPRL,PPROLEN                                                  
         MVC   JPYDJOL,PJOBLEN                                                  
                                                                                
         MVC   JPYDCOM,ACOMFACS                                                 
*&&UK*&& MVC   JPYDPRO,VPROMOTE                                                 
                                                                                
         CLI   PROCJOB,YESQ                                                     
         JNE   DOP2J1A                                                          
         OI    JPYDTYP,JPYDTJQ                                                  
                                                                                
DOP2J1A  CLI   PROCINV,YESQ                                                     
         JNE   DOP2J1B                                                          
         OI    JPYDTYP,JPYDRIQ                                                  
                                                                                
DOP2J1B  GOTO1 VPAY2JOB,JPYDETD                                                 
         JE    DOP2J2A                                                          
                                                                                
         OC    JPYDERR,JPYDERR                                                  
         JZ    DOP2J2                                                           
                                                                                
         MVC   PRL,SPACES                                                       
         MVC   PRL(9),=C'E R R O R'                                             
         XOUT  JPYDERR,PRL+11,2                                                 
         GOTO1 VPRINTIT                                                         
         CLI   RUNMODE,RUNACUPQ                                                 
         JNE   DOP2JX                                                           
         OI    TAPERROR,TAPERRSQ                                                
         J     DOP2JX                                                           
                                                                                
DOP2J2   MVC   PRL,SPACES                                                       
         MVC   PRL(19),=C'>>> Errors in Array'                                  
         GOTO1 VPRINTIT                                                         
                                                                                
DOP2J2A  XR    R5,R5                                                            
                                                                                
         OC    JPYDTRX,JPYDTRX                                                  
         JZ    DOP2J3                                                           
         AP    CNTUTRN,JPYDTRX                                                  
         AP    CNTUTRNC,JPYDTRX                                                 
         AP    CNTUACC,JPYDACT                                                  
         AP    CNTUACCC,JPYDACT                                                 
         AP    CNTUINV,JPYDINV                                                  
         AP    CNTUINVC,JPYDINV                                                 
         DROP  R2                                                               
                                                                                
DOP2J3   CLI   JARDFIL,JARDEOT                                                  
         JE    DOP2J5                                                           
         AP    CNTPTRN,PONE                                                     
         AP    CNTPTRNC,PONE                                                    
                                                                                
         MVC   PRL,SPACES                                                       
         MVC   PRL(17),=C'Job data updated:'                                    
         XOUT  JARDPDA,PRL+20,4                                                 
         OC    JARDATE,JARDATE                                                  
         JZ    DOP2J3B                                                          
         GOTO1 VDATCON,DMCB,(2,JARDATE),(13,PRL+30)                             
         MVI   PRL+39,C'/'                                                      
         MVC   PRL+40(6),JARPAYR                                                
                                                                                
DOP2J3B  OC    JARDERR,JARDERR                                                  
         JZ    DOP2J3C                                                          
         CLC   JARDERR,=AL2(AE$ITYAP)                                           
         JE    DOP2J3E                                                          
         CLC   JARDERR,=AL2(AE$ADMIS)                                           
         JE    DOP2J3E                                                          
         XOUT  JARDERR,PRL+55,2                                                 
         MVC   PRL+50(4),=C'ERR='                                               
         MVC   PRL(17),=C'Job data errors: '                                    
         CLI   RUNMODE,RUNACUPQ                                                 
         JNE   DOP2J3C                                                          
         OI    TAPERROR,TAPERRDQ                                                
                                                                                
DOP2J3C  DS    0H                                                               
*        CLI   DEBUG,YESQ          Special debug mode                           
*        JNE   DOP2J3D                                                          
*        USING JPYDETD,R2                                                       
*        LA    R2,ELEM                                                          
*        MVC   PRL+50(4),=C'Trx='                                               
*        EDITR (P4,JPYDTRX),(6,PRL+54),0                                        
*        MVC   PRL+62(4),=C'Act='                                               
*        EDITR (P4,JPYDACT),(6,PRL+66),0                                        
*        MVC   PRL+74(4),=C'Dup='                                               
*        EDITR (P4,JPYDDUP),(6,PRL+78),0                                        
*        MVC   PRL+86(4),=C'Inv='                                               
*        EDITR (P4,JPYDINV),(6,PRL+90),0                                        
*        DROP  R2                                                               
                                                                                
DOP2J3D  GOTO1 VPRINTIT                                                         
                                                                                
DOP2J3E  AHI   R5,1                                                             
         AHI   R4,JARAYLQ                                                       
         J     DOP2J3                                                           
         DROP  R4                                                               
                                                                                
DOP2J5   MVC   PRL,SPACES          END OF IO LOOP                               
         GOTO1 VPRINTIT                                                         
         MVC   PRL+12(18),=C'DOP2J - # of Trxs:'                                
         EDIT  (R5),(8,PRL+32),0                                                
         GOTO1 VPRINTIT                                                         
         MVC   PRL,SPACES                                                       
         GOTO1 VPRINTIT                                                         
                                                                                
DOP2JX   XIT1  ,                                                                
                                                                                
*&&UK                                                                           
***********************************************************************         
* MEDCHA CALL                                                         *         
***********************************************************************         
                                                                                
DOMEDCHA NTR1                                                                   
                                                                                
         LR    R2,R1                                                            
         XR    R3,R3                                                            
                                                                                
         CLC   VALMCHA,SPACES                                                   
         BNH   DOMCHAX                                                          
                                                                                
         MVC   DUB,=C'00000000'                                                 
                                                                                
         CLC   VALMCHA(2),=C'PR'                                                
         BNE   DOMCHA1                                                          
         MVC   DUB+2(6),VALMCHA+2                                               
         ICM   R3,8,=AL1(CHAWPRJQ)                                              
         B     DOMCHA8                                                          
                                                                                
DOMCHA1  CLC   VALMCHA(2),=C'BR'                                                
         BNE   DOMCHA3                                                          
         MVC   DUB+3(5),VALMCHA+2                                               
         CLI   VALMCHA+7,C'D'                                                   
         BE    DOMCHA2                                                          
         ICM   R3,8,=AL1(CHAWBRLQ)                                              
         B     DOMCHA8                                                          
                                                                                
DOMCHA2  ICM   R3,8,=AL1(CHAWBRDQ)                                              
         B     DOMCHA8                                                          
                                                                                
DOMCHA3  CLC   VALMCHA(2),=C'OT'                                                
         BNE   DOMCHA5                                                          
         MVC   DUB+3(5),VALMCHA+2                                               
         CLI   VALMCHA+7,C'D'                                                   
         BE    DOMCHA4                                                          
         ICM   R3,8,=AL1(CHAWOTLQ)                                              
         B     DOMCHA8                                                          
                                                                                
DOMCHA4  ICM   R3,8,=AL1(CHAWOTDQ)                                              
         B     DOMCHA8                                                          
                                                                                
DOMCHA5  CLC   VALMCHA(4),=C'UKCR'                                              
         BNE   DOMCHA6                                                          
         MVC   DUB+0(8),VALMCHA+4                                               
         ICM   R3,8,=AL1(CHAWGCRQ)                                              
         B     DOMCHA8                                                          
                                                                                
DOMCHA6  CLC   VALMCHA(4),=C'IDK-'                                              
         BNE   DOMCHA7                                                          
         MVC   DUB+4(4),VALMCHA+4                                               
         ICM   R3,8,=AL1(CHAWIDKQ)                                              
         B     DOMCHA8                                                          
                                                                                
DOMCHA7  DC    H'0'                                                             
                                                                                
DOMCHA8  PACK  DUB3,DUB                                                         
         CVB   RE,DUB3                                                          
         AR    R3,RE                                                            
         MVC   SOMFACS+0(4),VDATCON                                             
         MVC   SOMFACS+4(4),VHELLO                                              
         GOTO1 VMEDCHA,DMCB,(C'L',(R2)),(R3),('MCHAMSTQ',SOMFACS),0,0,0         
                                                                                
DOMCHAX  B     DOMAINX                                                          
*&&                                                                             
                                                                                
*&&UK                                                                           
***********************************************************************         
* PROMOTE AND UPDATE ARCHIVE RECORD                                   *         
***********************************************************************         
                                                                                
UPDARC   NTR1                                                                   
                                                                                
         L     R0,AIOARCAR         SAVE RECORD AWAY                             
         LA    R1,L'IOARCAR                                                     
         LR    RE,R7                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE               PROMOTE IT                                   
                                                                                
         GOTO1 VPROMOTE,DMCB,(R7),ACOMFACS                                      
                                                                                
         MVC   AKEY,0(R7)          REREAD DIRECTORY                             
         GOTO1 ,ACRDDMCB                                                        
         MVI   IOIND,IORUPDTQ                                                   
         GOTO1 VACDIRIO                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   IOIND,IORUPDTQ      RE-GET MASTER RECORD                         
         GOTO1 VACMSTIO,ACGTDMCB                                                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LR    R0,R7               OVERWRITE RECORD WITH SAVED COPY             
         LA    R1,L'IOARCAR                                                     
         L     RE,AIOARCAR                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE               AND TAKE AWAY ARCIVE BIT                     
                                                                                
         NI    ACTRSTAT-ACTRECD(R7),FFQ-TRNSARCH                                
                                                                                
         GOTO1 VACMSTIO,ACPUDMCB   UPDATE RECORD TO ACCMST AND                  
                                                                                
         MVC   AKEY,LASTKEY        RE-ESTABLISH READ SEQUENCE                   
         GOTO1 ,ACRDDMCB                                                        
         MVI   IOIND,IORUPDTQ                                                   
         GOTO1 VACDIRIO                                                         
         BE    UPDARCX                                                          
         DC    H'0'                                                             
                                                                                
UPDARCX  B     DOMAINX                                                          
*&&                                                                             
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
*  PADDLE CALLS TO MAINTAIN PASSIVE POINTERS                          *         
*  ASSUMES R7 = A(IOAREA) THAT CONTAINS ACCMST RECORD                 *         
*                                                                     *         
*  NB. DEPENDING ON THE TYPE OF ACCMST RECORD/AGENCY SETTINGS, IT MAY *         
*  BE NECESSARY TO SET FURTHER V-TYPES IN THE LOCAL COMFACS BLOCK TO  *         
*  MAKE THEM AVAILABLE TO ACLDCPTR                                    *         
***********************************************************************         
                                                                                
PPDEL    NTR1                                                                   
         USING CPTRBLK,ELEM        DELETE PASSIVES                              
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 APADDLE,DMCB,(C'D',(R7)),CPTRBLK,ADA,0,ACOMFACS                  
*        BE    *+6                 PADDLE RETURNS NEQ IF PRIME PTR              
*        DC    H'0'                IS ALREADY DELETED - THIS IS OK              
                                                                                
PPDELX   B     DOMAINX                                                          
                                                                                
PPADD    NTR1                                                                   
         USING CPTRBLK,ELEM        ADD PASSIVES                                 
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 APADDLE,DMCB,(C'A',(R7)),CPTRBLK,ADA,0,ACOMFACS                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
PPADDX   B     DOMAINX                                                          
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
*  GENERAL INITIALISATION, FILE OPENING ETC ETC                       *         
***********************************************************************         
                                                                                
INITIAL  CSECT                                                                  
         NMOD1 0,**INIT**                                                       
         LR    RC,R1                                                            
         MVI   SPACES,SPACEQ                                                    
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVI   HIGHVALS,X'FF'                                                   
         MVC   HIGHVALS+1(L'HIGHVALS-1),HIGHVALS                                
                                                                                
         MVC   VCARDS,=V(CARDS)    SET UP EXTERNAL ADCONS                       
         MVC   VDATCON,=V(DATCON)                                               
         MVC   VDATAMGR,=V(DATAMGR)                                             
         MVC   VPAY2JOB,=V(PAY2JOBC)                                            
         MVC   VHELLO,=V(HELLO)                                                 
         MVC   VHELEN,=V(HELEN)                                                 
         MVC   VHEXIN,=V(HEXIN)                                                 
         MVC   VHEXOUT,=V(HEXOUT)                                               
*&&UK*&& MVC   VMEDCHA,=V(MEDCHA)                                               
         MVC   VLOGIO,=V(LOGIO)                                                 
         MVC   VPRINTER,=V(PRINTER)                                             
         MVC   VRECTYP,=V(ACRECTYP)                                             
*&&UK*&& MVC   VPROMOTE,=V(PROMOTE)                                             
         MVC   VSORTER,=V(SORTER)                                               
         MVC   VLOADER,=V(LOADER)                                               
                                                                                
         MVC   VPUTERRM,=A(PUTERRM) SET UP LOCAL ADCONS                         
         MVC   VPUTMESS,=A(PUTMESS)                                             
         MVC   VACDIRIO,=V(ACCDIRIO)                                            
         MVC   VACMSTIO,=V(ACCMSTIO)                                            
         MVC   VACARCIO,=V(ACCARCIO)                                            
         MVC   VIOERROR,=V(IOERROR)                                             
         MVC   VIOTRACE,=V(IOTRACE)                                             
         MVC   VPRINTIT,=A(PRINTIT)                                             
         MVC   AMTRBUFF,=A(MTRBUFF)                                             
         MVC   ASSB,=A(SSB)                                                     
         MVC   VUTL,=V(UTL)                                                     
         MVC   ACOMFACS,=A(COMFACS)                                             
         MVC   AJARAY,=A(JARAYC)                                                
                                                                                
         L     R1,VDATAMGR         ADD VDATAMGR TO LOCAL COMFACS                
         L     RF,ACOMFACS                                                      
         ST    R1,CDATAMGR-COMFACSD(RF)                                         
         L     R1,VHELLO           ADD VHELLO TO LOCAL COMFACS                  
         L     RF,ACOMFACS                                                      
         ST    R1,CHELLO-COMFACSD(RF)                                           
         L     R1,VHELEN           ADD VHELEN TO LOCAL COMFACS                  
         L     RF,ACOMFACS                                                      
         ST    R1,CHELEN-COMFACSD(RF)                                           
                                                                                
         MVC   DUB,SPACES                                                       
         MVC   DUB(6),=C'T00A9F'   LOAD PADDLE                                  
         XC    APADDLE,APADDLE                                                  
         GOTO1 VLOADER,DMCB,DUB,0,0                                             
         OC    APADDLE,4(R1)                                                    
         JZ    *+2                                                              
                                                                                
         L     R1,=A(IOAREA1-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIOAREA1                                                      
         L     R1,=A(IOAREA2-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIOAREA2                                                      
         L     R1,=A(IOAREA3-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIOAREA3                                                      
         L     R1,=A(IOARCAR-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIOARCAR                                                      
                                                                                
         L     R1,=A(IOB4H-WORKD)                                               
         AR    R1,RC                                                            
         ST    R1,AIOB4H                                                        
         L     R1,=A(IOAFH-WORKD)                                               
         AR    R1,RC                                                            
         ST    R1,AIOAFH                                                        
                                                                                
         MVC   ACCMST,=CL8'ACCMST'                                              
         MVC   ACCARC,=CL8'ACCARC'                                              
         MVC   ACCDIR,=CL8'ACCDIR'                                              
                                                                                
         GOTO1 ,ACGTDMCB,('GETRECQ',WORKD)                                      
         GOTO1 ,ACPUDMCB,('PUTRECQ',WORKD)                                      
         GOTO1 ,ACADDMCB,('ADDRECQ',WORKD)                                      
                                                                                
         GOTO1 ,ACRDDMCB,('DMREADQ',WORKD)                                      
         GOTO1 ,ACHIDMCB,('DMRDHIQ',WORKD)                                      
         GOTO1 ,ACSQDMCB,('DMRSEQQ',WORKD)                                      
         GOTO1 ,ACDADMCB,('DMADDQ',WORKD)                                       
         GOTO1 ,ACWRDMCB,('DMWRTQ',WORKD)                                       
                                                                                
         GOTO1 =A(VALCARDS),WORKD                                               
                                                                                
         GOTO1 VDATCON,DMCB,(5,0),(3,DATEBIN)                                   
         GOTO1 VDATCON,DMCB,(5,0),(1,DUB)                                       
                                                                                
         CLC   DUB(3),VRSNCTRL                                                  
         JL    INIT02                                                           
         CLC   DUB(3),VRSNCTRL+3                                                
         JNH   INIT04                                                           
INIT02   MVC   CONSMSG(40),=CL40'FAILED VERSION CONTROL DATES'                  
         LA    R2,430                                                           
         GOTO1 VPUTERRM,WORKD                                                   
         ABEND (R2),DUMP                                                        
                                                                                
INIT04   LA    RE,FILELIS1                                                      
                                                                                
INIT10   CLI   0(RE),C'X'                                                       
         JE    INIT20                                                           
         CLI   UPDATFIL,UPUPDATQ   TEST FOR UPDATIVE RUN                        
         JNE   *+8                                                              
         MVI   0(RE),C'U'                                                       
         LA    RE,8(RE)                                                         
         J     INIT10                                                           
                                                                                
INIT20   L     RF,VUTL             SET SE NO. FOR CHOSEN SYS                    
         MVC   4(1,RF),THISSENO                                                 
         MVI   THISFILE,X'69'      ALWAYS ACCDIR                                
                                                                                
         L     R7,AIOAREA2                                                      
         GOTO1 VDATAMGR,DMCB,DMOPEN,ACCSYS,FILELIS1,(R7)                        
                                                                                
         CLI   ANYRECOV,NOQ                                                     
         JE    INIT50                                                           
                                                                                
         LA    RE,FILELIS2         OPEN RECOVERY FILE TOO                       
INIT30   CLI   0(RE),C'X'                                                       
         JE    INIT40                                                           
         CLI   UPDATFIL,UPUPDATQ   TEST FOR UPDATIVE RUN                        
         JNE   *+8                                                              
         MVI   0(RE),C'U'                                                       
         LA    RE,8(RE)                                                         
         J     INIT30                                                           
                                                                                
INIT40   GOTO1 VDATAMGR,DMCB,DMOPEN,ACCSYS,FILELIS2,(R7)                        
                                                                                
* SET UP OPERATOR COMMS                                                         
                                                                                
INIT50   EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
         MVC   ACOMBLOC,ACOMM      SET UP W/ST ADDRESS                          
         J     INIT70                                                           
                                                                                
ACOMM    DC    A(0)                                                             
         DROP  R2                                                               
                                                                                
* MAY ALSO BE PRODUCING A DUMP-FORMAT OUTPUT FILE                               
                                                                                
INIT70   CLI   DUMPOFIL,YESQ                                                    
         JNE   INIT80                                                           
         L     R3,=A(DUMPFOUT)                                                  
         ST    R3,ADMPFOUT                                                      
         OPEN  ((R3),OUTPUT)                                                    
                                                                                
* MAY (ALSO) BE PRODUCING A DFA STYLE RECOVERY FILE                             
                                                                                
INIT80   CLI   RECOVOUT,YESQ                                                    
         JNE   INIT90                                                           
         L     R3,=A(RCVFOUT)                                                   
         ST    R3,ARCVFOUT                                                      
         OPEN  ((R3),OUTPUT)                                                    
                                                                                
* MAY BE SORT TO INITIALISE                                                     
                                                                                
INIT90   GOTO1 VSORTER,DMCB,SORTCARD,RECCARD                                    
         MVI   SORTACTV,YESQ                                                    
                                                                                
* 31 BIT ...                                                                    
* THERE IS (AS I WRITE) AN ABSOLUTE SYSTEM LIMIT OF 400MB                       
* (409,600,000 BYTES), RATHER THAN THE THEORETICAL 2GB MAX                      
                                                                                
         L     R0,=A(JARBUFLN)                                                  
***      GETMAIN RC,LV=(R0),LOC=(ANY,ANY)                                       
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,AJARBUFF                                                      
                                                                                
INITIALX XIT1  ,                                                                
                                                                                
INITER5  MVC   CONSMSG(40),=CL40'???????????????????????????????'               
         LA    R2,500                                                           
         B     INITERRX                                                         
                                                                                
INITERRX GOTO1 VPUTERRM,WORKD                                                   
         ABEND (R2),DUMP                                                        
                                                                                
* CARDS FOR SIMPLE FIXED-LENGTH ACC SYSTEM RECORD SORT                          
                                                                                
SORTCARD DC    C'SORT FIELDS=(1,42,BI,A) '                                      
RECCARD  DC    C'RECORD TYPE=F,LENGTH=2004 '                                    
                                                                                
* CARDS FOR VB SORT. RECCARV MIN/MAX LENGTHS WILL NEED CHANGING                 
                                                                                
SORTCARV DC    C'SORT FIELDS=(5,42,BI,A) '                                      
RECCARV  DC    C'RECORD TYPE=V,LENGTH=(2048,,,25,60) '                          
                                                                                
P2JIN    DCB   DDNAME=P2JIN,DSORG=PS,RECFM=FB,MACRF=GM,EODAD=DOMAIN08           
                                                                                
DUMPFOUT DCB   DDNAME=DUMPFOUT,DSORG=PS,MACRF=PM                                
                                                                                
RCVFOUT  DCB   DDNAME=RCVFOUT,DSORG=PS,MACRF=PM,                       X        
               RECFM=VB,LRECL=8200,BLKSIZE=8204                                 
                                                                                
***********************************************************************         
* PRINTER INTERFACE                                                   *         
***********************************************************************         
                                                                                
PRINTIT  MVC   P,PRL               COPY PRL TO P                                
         L     RF,VPRINTER                                                      
         MVC   PRL,SPACES                                                       
         BR    RF                  LET PRINTER EXIT TO CALLER                   
                                                                                
***********************************************************************         
* TABLES AND CONSTANTS                                                          
***********************************************************************         
                                                                                
FILELIS1 DS    0XL9                                                             
         DC    C'NACCDIR '         ACC FILES TO OPEN                            
         DC    C'NACCMST '                                                      
         DC    C'NACCARC '                                                      
         DC    C'NCTFILE '                                                      
         DC    C'X'                                                             
                                                                                
FILELIS2 DC    C'NACCRCV '                                                      
         DC    C'X'                                                             
                                                                                
DMOPEN   DC    CL8'DMOPEN'                                                      
ACCSYS   DC    CL4'ACC'                                                         
CONSYS   DC    CL4'CON'                                                         
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
*  CLOSE FILES ETC AT END OF JOB                                      *         
***********************************************************************         
                                                                                
FINISHUP CSECT                                                                  
         NMOD1 0,**FISH**                                                       
                                                                                
         LR    RC,R1                                                            
         L     R7,AIOAREA1                                                      
         GOTO1 VDATAMGR,DMCB,DMCLOSE,=C'ACC',=A(FILELIS1),(R7)                  
         CLI   ANYRECOV,NOQ                                                     
         JE    FINI10                                                           
         GOTO1 VDATAMGR,DMCB,DMCLOSE,=C'ACC',=A(FILELIS2),(R7)                  
                                                                                
FINI10   DS    0H                                                               
                                                                                
* ALSO NEED TO CLOSE ANY OTHER INPUT/OUTPUT FILES                               
                                                                                
FINI40   CLI   DUMPOFIL,YESQ                                                    
         JNE   FINI50                                                           
         L     R7,ADMPFOUT                                                      
         CLOSE ((R7))                                                           
                                                                                
FINI50   CLI   RECOVOUT,YESQ                                                    
         JNE   FINI60                                                           
         L     R7,ARCVFOUT                                                      
         CLOSE ((R7))                                                           
                                                                                
* MAY NEED TO TERMINATE SORT                                                    
                                                                                
FINI60   CLI   SORTACTV,YESQ                                                    
         JNE   FINI70                                                           
         GOTO1 VSORTER,DMCB,=C'END'                                             
                                                                                
* MAY NEED TO FREEMAIN ANY ACQUIRED STORAGE                                     
                                                                                
FINI70   DS    0H                                                               
         L     R0,=A(JARBUFLN)                                                  
         L     R1,AJARBUFF                                                      
         FREEMAIN R,A=(1),LV=(0)                                                
                                                                                
         XIT1  ,                                                                
                                                                                
DMCLOSE  DC    CL8'DMCLSE'                                                      
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* ACCDIR I/O ROUTINES                                                 *         
* ENTRY - 0(1,R1) IS IOCOMMAND                                        *         
*       - 0(4,R1) IS A(WORKD)                                         *         
*       - KEY AT AKEY                                                 *         
* EXIT  - DIRECTORY RECORD READ INTO ACDIRREC                         *         
*       - CC NEQ ON RECORD NOT FOUND                                  *         
* PRINT ANY OTHER ERRORS AND DIE                                      *         
***********************************************************************         
                                                                                
ACCDIRIO CSECT                                                                  
         NMOD1 0,**ADIR**                                                       
         L     RC,0(R1)                                                         
         MVC   IOCMD,0(R1)                                                      
         MVC   SVIOIND,IOIND       SAVE CALLER'S IOIND                          
         LA    R2,ACDIRCMD                                                      
                                                                                
ADIR02   CLI   0(R2),EOTQ                                                       
         JE    *+2                                                              
         CLC   IOCMD,8(R2)                                                      
         JE    ADIR04                                                           
         AHI   R2,L'ACDIRCMD                                                    
         J     ADIR02                                                           
                                                                                
ADIR04   MVC   AKEYSAVE,AKEY                                                    
                                                                                
         CLI   READDELS,YESQ                                                    
         JNE   *+8                                                              
         OI    IOIND,IORDELQ                                                    
                                                                                
         GOTO1 VDATAMGR,DMCBIO,(IOIND,(R2)),ACCDIR,AKEY,ACDIRREC,      *        
               AMTRBUFF                                                         
         ORG   *-2                                                              
         CLI   UPDATFIL,UPREPORQ                                                
         JNE   ADIR06                                                           
         CLI   IOCMD,DMADDQ                                                     
         JE    ADIR10                                                           
         CLI   IOCMD,DMWRTQ                                                     
         JE    ADIR10                                                           
                                                                                
ADIR06   BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         JE    ADIR10                                                           
         TM    8(R1),IOEEOF+IOEDSK+IOEDUP                                       
         JNZ   ACCDIRER                                                         
                                                                                
ADIR10   MVI   IOIND,NULLQ                                                      
         TM    TRACEIND,TRACEDIQ                                                
         JZ    ADIR12                                                           
         GOTO1 VIOTRACE,WORKD                                                   
                                                                                
ADIR12   NI    IOIND,255-IOFTRQ                                                 
                                                                                
         GOTO1 =A(CHECKOPS),WORKD                                               
         TM    DMCBIO+8,IOERNF     TEST NOT FOUND BIT                           
         XIT1  ,                                                                
                                                                                
ACCDIRER GOTO1 VIOERROR,WORKD                                                   
         ABEND 600,DUMP                                                         
                                                                                
ACDIRCMD DS    0XL9                ACC DIR COMMAND TABLE                        
         DC    CL8'DMRDHI',AL1(DMRDHIQ)                                         
         DC    CL8'DMREAD',AL1(DMREADQ)                                         
         DC    CL8'DMRSEQ',AL1(DMRSEQQ)                                         
         DC    CL8'DMWRT ',AL1(DMWRTQ)                                          
         DC    CL8'DMADD ',AL1(DMADDQ)                                          
         DC    AL1(EOTQ)                                                        
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* ACCMST I/O ROUTINES                                                 *         
* ENTRY - 0(1,R1) IS IOCOMMAND                                        *         
*       - 0(4,R1) IS A(WORKD)                                         *         
*       - R7 IS A(CALLER'S IOAREA)                                    *         
*       - DISK ADDR IN DDA IN ACDIRREC                                *         
***********************************************************************         
                                                                                
ACCMSTIO CSECT                                                                  
         NMOD1 0,**AMST**                                                       
         L     RC,0(R1)                                                         
         MVC   IOCMD,0(R1)                                                      
         MVC   SVIOIND,IOIND       SAVE CALLER'S IOIND                          
         LA    R2,ACMSTCMD                                                      
                                                                                
AMST02   CLI   0(R2),EOTQ                                                       
         JE    *+2                                                              
         CLC   IOCMD,8(R2)                                                      
         JE    AMST04                                                           
         AHI   R2,L'ACMSTCMD                                                    
         J     AMST02                                                           
                                                                                
AMST04   L     RF,VDATAMGR                                                      
         LA    R1,DMCBIO                                                        
         CLI   IOCMD,ADDRECQ                                                    
         JE    AMST10                                                           
                                                                                
         CLI   READDELS,YESQ                                                    
         JNE   *+8                                                              
         OI    IOIND,IORDELQ                                                    
                                                                                
         GOTO1 (RF),(R1),(IOIND,(R2)),ACCMST,ADA,(R7),DMWORK                    
         ORG   *-2                                                              
         CLI   IOCMD,PUTRECQ                                                    
         JNE   AMST06                                                           
         CLI   UPDATFIL,UPUPDATQ                                                
         JNE   AMST20                                                           
                                                                                
AMST06   BASR  RE,RF                                                            
         TM    8(R1),FFQ-IOEDEL                                                 
         JNZ   ACCMSTER                                                         
                                                                                
         CLI   IOCMD,GETRECQ                                                    
         JNE   AMST20                                                           
         CLC   0(L'ACTKEY,R7),ACDIRREC                                          
         JNE   AMST20                                                           
         TM    ACDIRREC+TRNKSTAT-TRNRECD,TRNSARCH                               
         JZ    AMST20                                                           
         DC    H'0'                                                             
                                                                                
AMST10   XC    THISDA,THISDA                                                    
         GOTO1 (RF),(R1),0(R2),ACCMST,ADA,(R7),DMWORK                           
         ORG   *-2                                                              
         CLI   UPDATFIL,UPUPDATQ                                                
         JNE   AMST20                                                           
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         JNE   ACCMSTER                                                         
         MVC   THISDA,8(R1)                                                     
                                                                                
AMST20   MVI   IOIND,NULLQ                                                      
         TM    TRACEIND,TRACEMIQ                                                
         JZ    AMST30                                                           
         GOTO1 VIOTRACE,WORKD                                                   
                                                                                
AMST30   JAS   RE,OUTRECO                                                       
                                                                                
         GOTO1 =A(CHECKOPS),WORKD                                               
         XIT1  ,                                                                
                                                                                
ACCMSTER GOTO1 VIOERROR,WORKD                                                   
         ABEND 700,DUMP                                                         
                                                                                
ACMSTCMD DS    0XL9                ACC FILE COMMAND TABLE                       
         DC    CL8'ADDREC',AL1(ADDRECQ)                                         
         DC    CL8'GETREC',AL1(GETRECQ)                                         
         DC    CL8'PUTREC',AL1(PUTRECQ)                                         
         DC    AL1(EOTQ)                                                        
                                                                                
* MAY ALSO WANT RECOVERY (DFA) RECORDS WRITTEN OUT                              
                                                                                
         USING ACTRECD,R7                                                       
OUTRECO  CLI   RECOVOUT,YESQ       TEST WRITING RECOV OUT                       
         BNER  RE                                                               
         CLI   THISROUT,YESQ       TEST THIS RECORD TO GO OUT                   
         BNER  RE                                                               
                                                                                
         ST    RE,SAVERE                                                        
                                                                                
         XR    RF,RF                                                            
         ICM   RF,3,ACTRLEN                                                     
         LR    R2,RF                                                            
         LR    RE,R7                                                            
         CLI   IOCMD,GETRECQ                                                    
         JNE   OUTREC50                                                         
         L     R3,AIOB4H                                                        
         USING IOB4H,R3                                                         
         MVI   IOB4RH+(RRECTY-RECVHDR),1    SET AS A COPY                       
                                                                                
         LA    R0,IOB4AREA                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE               COPY XISTING RECORD TO IOB4                  
                                                                                
         LR    RF,R2                                                            
         AHI   RF,IOB4RHLQ         INCREASE LEN FOR RECOVERY HEADER             
         SLL   RF,16                                                            
         L     RE,AIOB4H                                                        
         ST    RF,0(RE)            SET NEW LENGTH IN RDW                        
         J     OUTRECX                                                          
                                                                                
OUTREC50 L     R3,AIOAFH                                                        
         USING IOAFH,R3                                                         
         LA    R0,IOAFAREA                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE               COPY NEW RECORD TO IOAFT                     
                                                                                
         LR    RF,R2                                                            
         AHI   RF,IOAFRHLQ         INCREASE LEN FOR RECOVERY HEADER             
         SLL   RF,16                                                            
         ST    RF,0(R3)            SET NEW LENGTH IN RDW                        
                                                                                
         MVI   IOAFRH+(RRECTY-RECVHDR),3   ADD                                  
         CLI   IOCMD,ADDRECQ                                                    
         JE    OUTREC90                                                         
         MVC   THISDA,ADA                                                       
         CLC   ACTKEY,IOAFAREA                                                  
         JNE   *+2                 PUTREC KEY NEQ GETREC KEY                    
         MVI   IOAFRH+(RRECTY-RECVHDR),2   CHANGE                               
         L     R3,AIOB4H                                                        
         JAS   RE,OUTRHEAD         COMPLETE RECOVERY HEADER                     
         L     R1,ARCVFOUT         WRITE OUT COPY                               
         PUT   (1),(R3)                                                         
         L     R3,AIOAFH                                                        
                                                                                
OUTREC90 JAS   RE,OUTRHEAD         COMPLETE RECOVERY HEADER                     
         L     R1,ARCVFOUT         WRITE OUT CHANGED OR NEW RECORD              
         PUT   (1),(R3)                                                         
                                                                                
OUTRECX  L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
* BUILD MOST OF RECOVERY HEADER FOR RECORD ABOUT TO BE OUTPUT.                  
* R3 IS A(RECORD RDW)                                                           
                                                                                
OUTRHEAD ST    RE,SAVERE2                                                       
         ST    R3,FULL                                                          
         LA    R3,IOB4RH-IOB4H(R3)                                              
         USING RECVHDR,R3                                                       
         MVC   RSIN,=F'1'                                                       
         MVC   RDATE,DATEBIN                                                    
         THMS                                                                   
         ST    R1,RTIME            0HHMMSS+                                     
         MVC   RSYS,THISSENO       SE NUMBER                                    
         MVC   RFILTY,THISFILE     FILE NUMBER                                  
         MVC   RVCHR,THISDA        DISK ADDR                                    
         L     R3,FULL                                                          
         L     RE,SAVERE2                                                       
         BR    RE                                                               
         DROP  R3                                                               
         DROP  R7                                                               
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* ACCARC I/O ROUTINES                                                 *         
* ENTRY - 0(1,R1) IS IOCOMMAND                                        *         
*       - 0(4,R1) IS A(WORKD)                                         *         
*       - R7 IS A(CALLER'S IOAREA)                                    *         
*       - DISK ADDR IN DDA IN ACDIRREC                                *         
***********************************************************************         
                                                                                
ACCARCIO CSECT                                                                  
         NMOD1 0,**AARC**                                                       
         L     RC,0(R1)                                                         
         MVC   IOCMD,0(R1)                                                      
         MVC   SVIOIND,IOIND       SAVE CALLER'S IOIND                          
         LA    R2,ACARCCMD                                                      
                                                                                
AARC02   CLI   0(R2),EOTQ                                                       
         JE    *+2                                                              
         CLC   IOCMD,8(R2)                                                      
         JE    AARC04                                                           
         AHI   R2,L'ACARCCMD                                                    
         J     AARC02                                                           
                                                                                
AARC04   L     RF,VDATAMGR                                                      
         LA    R1,DMCBIO                                                        
                                                                                
         CLI   READDELS,YESQ                                                    
         JNE   *+8                                                              
         OI    IOIND,IORDELQ                                                    
                                                                                
         GOTO1 (RF),(R1),(IOIND,(R2)),ACCARC,ADA,(R7),DMWORK                    
         ORG   *-2                                                              
         CLI   IOCMD,PUTRECQ                                                    
         JNE   AARC06                                                           
         CLI   UPDATFIL,UPUPDATQ                                                
         JNE   AARC08                                                           
                                                                                
AARC06   BASR  RE,RF                                                            
         TM    8(R1),FFQ-IOEDEL                                                 
         JNZ   ACCARCER                                                         
         CLI   IOCMD,GETRECQ                                                    
         JNE   AARC08                                                           
         CLC   0(L'ACTKEY,R7),ACDIRREC                                          
         JNE   AARC08                                                           
         TM    ACDIRREC+TRNKSTAT-TRNRECD,TRNSARCH                               
         JNZ   AARC08                                                           
         DC    H'0'                                                             
                                                                                
AARC08   MVI   IOIND,NULLQ                                                      
         TM    TRACEIND,TRACEAIQ                                                
         JZ    AARC10                                                           
         GOTO1 VIOTRACE,WORKD                                                   
                                                                                
AARC10   DS    0H                                                               
*        BAS   RE,OUTRECO          NO NEED AS NOT UPDATIVE                      
                                                                                
         GOTO1 =A(CHECKOPS),WORKD                                               
         XIT1  ,                                                                
                                                                                
ACCARCER GOTO1 VIOERROR,WORKD                                                   
         ABEND 700,DUMP                                                         
                                                                                
ACARCCMD DS    0XL9                ACC FILE COMMAND TABLE                       
         DC    CL8'GETREC',AL1(GETRECQ)                                         
         DC    AL1(EOTQ)                                                        
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* PRINT IO ERRORS FROM DMCBIO                                         *         
* ENTRY - R1 IS A(WORKD)                                              *         
***********************************************************************         
                                                                                
IOERROR  CSECT                                                                  
         NMOD1 0,**IOER**                                                       
         LR    RC,R1                                                            
         MVC   PRL,SPACES                                                       
         GOTO1 VPRINTIT                                                         
         MVC   PRL(11),=C'I/O ERROR -'                                          
         L     R1,DMCBIO                                                        
         MVC   PRL+12(7),0(R1)     COMMAND                                      
         L     R1,DMCBIO+4                                                      
         MVC   PRL+20(7),0(R1)     FILE NAME                                    
         L     R0,DMCBIO+8         KEY OR RECORD                                
         SLL   R0,8                                                             
         SRL   R0,8                KNOCK OFF IO ERROR ITSELF                    
         GOTO1 VHEXOUT,DUB,(R0),PRL+28,10,=C'TOG'                               
         MVC   PRL+50(6),=C'ERROR='                                             
         GOTO1 (RF),(R1),DMCBIO+8,PRL+56,1,=C'TOG'                              
         GOTO1 VPRINTIT                                                         
         MVC   PRL(22),=C'**PLEASE CONTACT DDS**'                               
         GOTO1 VPRINTIT                                                         
         XIT1  ,                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* TRACE ACC SYSTEM DIRECTORY AND MASTER I/O                           *         
* ENTRY - R1 IS A(WORKD)                                              *         
*       - R7 IS A(FILE RECORD)                                        *         
***********************************************************************         
                                                                                
IOTRACE  CSECT                                                                  
         NMOD1 0,**IOTR**                                                       
         LR    RC,R1                                                            
         MVC   PRL,SPACES                                                       
         L     R1,DMCBIO                                                        
         MVC   PRL(6),0(R1)       COMMAND                                       
         L     R1,DMCBIO+4                                                      
         CLC   3(3,R1),=C'DIR'                                                  
         JNE   IOTRFIL                                                          
         MVC   PRL+7(4),=C'KEY='                                                
         GOTO1 VHEXOUT,DMCB2,AKEY,PRL+11,42                                     
                                                                                
         MVC   PRL+96(3),=C'RC='                                                
         GOTO1 VHEXOUT,DMCB2,DMCBIO+8,PRL+99,1                                  
                                                                                
         MVC   PRL+102(5),=C'STAT='                                             
         GOTO1 VHEXOUT,DMCB2,ASTAT,PRL+107,8                                    
         GOTO1 VPRINTIT                                                         
                                                                                
         MVC   PRL+7(3),=C'DA='                                                 
         GOTO1 VHEXOUT,DMCB2,ADA,PRL+10,4                                       
                                                                                
         MVC   PRL+20(5),=C'KSV='                                               
         GOTO1 VHEXOUT,DMCB2,AKEYSAVE,PRL+25,42                                 
         GOTO1 VPRINTIT                                                         
         J     IOTRACEX                                                         
                                                                                
IOTRFIL  MVC   PRL+7(5),=C'RKEY='                                               
         USING ACTRECD,R7                                                       
         GOTO1 VHEXOUT,DMCB2,ACTKEY,PRL+12,42                                   
                                                                                
         MVC   PRL+98(3),=C'RC='                                                
         GOTO1 VHEXOUT,DMCB2,DMCBIO+8,PRL+101,1                                 
                                                                                
         MVC   PRL+103(6),=C'RSTAT='                                            
         GOTO1 VHEXOUT,DMCB2,ACTRSTA,PRL+110,8                                  
         GOTO1 VPRINTIT                                                         
                                                                                
         MVC   PRL+7(5),=C'RLEN='                                               
         GOTO1 VHEXOUT,DMCB2,ACTRLEN,PRL+12,2                                   
         GOTO1 VPRINTIT                                                         
         LA    R3,ACTRFST                                                       
                                                                                
IOTRF15  CLI   0(R3),0                                                          
         JE    IOTRACEX                                                         
         MVC   PRL(5),=C'DATA='                                                 
         LA    R0,(L'P-5)/2                                                     
         CLI   1(R3),(L'P-5)/2                                                  
         JNL   IOTRF20                                                          
         LLC   R0,1(R3)                                                         
                                                                                
IOTRF20  GOTO1 VHEXOUT,DMCB2,(R3),PRL+5,(R0)                                    
         CLI   1(R3),(L'P-5)/2                                                  
         JNH   *+10                                                             
         MVC   PRL+L'PRL-2(2),=C'>>'                                            
         GOTO1 VPRINTIT                                                         
         LLC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         J     IOTRF15                                                          
         DROP  R7                                                               
                                                                                
IOTRACEX XIT1  ,                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* OPERATOR COMMS.                                                     *         
* CHECK FOR OPERATOR CONSOLE COMMAND (STOP) TO ALLOW FOR 'CLEAN'      *         
* CANCELLATION OF JOB (I.E. NOT IN THE MIDDLE OF DATAMANAGER)         *         
* CONSOLE COMMAND IS: P JOBNAME                                       *         
***********************************************************************         
                                                                                
CHECKOPS CSECT                                                                  
         NMOD1 0,**CHKO**                                                       
         LR    RC,R1                                                            
         L     R1,AOPERECB                                                      
         TM    0(R1),X'40'         TEST OPS HAVE POSTED (STOPPED)               
         JNO   CHECKOPX                                                         
         L     RF,ACOMBLOC         SET RF TO COMM BLOCK                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         DROP  RF                                                               
                                                                                
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         JNE   CHECKOPX                                                         
         MVC   CONSMSG(40),=CL40'ACDC - STOP COMMAND ACCEPTED'                  
         GOTO1 VPUTMESS,WORKD                                                   
         DC    H'0'                TAKE A HIT (TRIGGER RECOVERY)                
         DROP  R2                                                               
                                                                                
CHECKOPX XIT1  ,                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* VALIDATE PARAMETER CARDS                                            *         
* EXIT - NOT EQUAL ON ANY ERROR                                       *         
***********************************************************************         
                                                                                
VALCARDS CSECT                                                                  
         NMOD1 0,**VCRD**                                                       
         LR    RC,R1                                                            
         L     R7,AIOAREA1                                                      
         MVC   TITLE(15),=C'PARAMETER CARDS'                                    
         MVI   CONSOLMS,YESQ                                                    
         MVI   DUMPOFIL,NOQ                                                     
                                                                                
VALC005  GOTO1 VCARDS,DMCB,(R7),=C'RE00'                                        
         CLC   =C'/*',0(R7)                                                     
         JE    VALC045                                                          
         MVC   P(80),0(R7)                                                      
         GOTO1 VPRINTER                                                         
                                                                                
         LR    R2,R7                                                            
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         JE    VALC035                                                          
         AHI   R1,79                                                            
         ST    R1,FULL             SAVE LAST CHR ADDR IN FULL                   
         USING CARDTABD,R4                                                      
VALC010  LA    R4,CARDTAB                                                       
VALC015  CLI   CARDKEY,EOTQ                                                     
         JE    VALCERR             ERROR IF ALL DONE                            
         LLC   R1,CARDKXLN                                                      
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         J     *+10                                                             
         CLC   0(0,R2),CARDKEY                                                  
         JE    VALC020                                                          
VALC018  AHI   R4,CARDTBLQ         TRY NEXT ENTRY                               
         J     VALC015                                                          
                                                                                
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
         CLI   0(R2),C'='                                                       
         JE    VALC022                                                          
         LR    R2,R7                                                            
         J     VALC018             TRY ANOTHER CARD                             
VALC022  ICM   RF,15,CARDVDSP                                                   
         CLI   CARDIND,CARDRTNQ                                                 
         JNE   VALC025                                                          
         BASR  RE,RF                                                            
         JNE   VALCERR                                                          
         J     VALC030                                                          
VALC025  LLC   R1,CARDVXLN         GET LEN FOR MOVE                             
         LA    RF,WORKD(RF)                                                     
         MVC   0(0,RF),1(R2)       MOVE TO OUTPUT AREA                          
         EX    R1,*-6                                                           
VALC030  MVI   ANYCARDS,YESQ                                                    
                                                                                
         CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         JE    VALC010             GO FIND TABLE ENTRY                          
         C     R2,FULL             TEST FOR END OF CARD                         
         JL    VALC030                                                          
VALC035  J     VALC005                                                          
                                                                                
VALC045  CLI   ANYCARDS,YESQ                                                    
         JNE   VALCMISS                                                         
         OC    THISSEAL,THISSEAL                                                
         JZ    VALCSYS             MUST SPECIFY AN ACC SYSTEM                   
         OC    UPDATFIL,UPDATFIL                                                
         JZ    VALCUPDF            MUST PROVIDE UPDATFIL=                       
                                                                                
         CLI   UPDATFIL,UPUPDATQ                                                
         JNE   VALC048                                                          
         CLI   ANYRECOV,0                                                       
         JE    VALCRECM            NEED TO KNOW ABOUT RECOVERY                  
         CLI   ANYLOCKS,0                                                       
         JE    VALCLOCK            AND ABOUT LOCKING                            
                                                                                
VALC048  CLI   DSPACE,0                                                         
         JE    VALCFILE            MUST PROVIDE DSPACE=                         
         CLI   DSPACE,C'T'                                                      
         JE    VALC050                                                          
         CLI   DSPACE,C'A'                                                      
         JE    VALC050                                                          
         CLI   DSPACE,C'C'                                                      
         JE    VALC050                                                          
         CLI   DSPACE,C'Q'                                                      
         JNE   VALCERR                                                          
                                                                                
VALC050  CLI   CONSOLMS,YESQ       CONSOLE MESSAGES                             
         JE    VALC052                                                          
         CLI   CONSOLMS,NOQ                                                     
         JNE   VALCERR                                                          
                                                                                
VALC052  CLI   DUMPOFIL,NOQ                                                     
         JE    VALC054                                                          
         CLI   DUMPOFIL,YESQ                                                    
         JNE   VALCERR                                                          
                                                                                
VALC054  CLI   RUNMODE,RUNACUPQ    In ACUP mode set both types on               
         JNE   VALC056                                                          
         MVI   PROCJOB,YESQ                                                     
         MVI   PROCINV,YESQ                                                     
         J     VALC060                                                          
                                                                                
VALC056  CLI   PROCJOB,YESQ                                                     
         JE    VALC060                                                          
         CLI   PROCINV,YESQ                                                     
         JNE   VALCTYPE                                                         
                                                                                
* CROSS-VALIDATE CARDS                                                          
                                                                                
VALC060  CLI   ANYRECOV,NOQ        Recovery                                     
         JE    VALC062                                                          
         CLI   UPDATFIL,UPUPDATQ                                                
         JNE   VALCRECO            HAS TO BE UPDATIVE RUN FOR RECOVERY          
                                                                                
VALC062  CLI   RUNMODE,RUNCONVQ    Mode depending settings                      
         JE    VALC070                                                          
         CLI   RUNMODE,RUNACUPQ                                                 
         JNE   VALCMODE                                                         
         DS    0H                                                               
         J     VALC080                                                          
                                                                                
VALC070  DS    0H                                                               
                                                                                
* END OF CARD VALIDATION                                                        
                                                                                
VALC080  L     RF,ASSB                                                          
         USING SSOOFF,RF                                                        
         MVC   SSODSPAC,DSPACE     SET DATASPACE ID                             
         MVI   SSOSTAT2,SSOSGALO+SSOSNRCV                                       
         CLI   UPDATFIL,UPUPDATQ                                                
         JNE   VALC090                                                          
                                                                                
* ENABLE LOCKS IF UPDATIVE RUN BUT ONLY IF CONFIDENT THERE WON'T BE             
* "TOO MANY" - LOCKTAB IN DATASPACE WILL JUST FILL UP AND JOB WILL DIE          
* DONT LOCK IF SYSTEM IS STOPPED ANYWAY - OR HAS BEEN SET READ-ONLY             
                                                                                
         CLI   ANYLOCKS,NOQ                                                     
         JE    *+8                                                              
         OI    SSOSTAT2,SSOSLOCK   ENABLE LOCKS IF UPDATIVE RUN                 
                                                                                
         CLI   ANYRECOV,NOQ                                                     
         JE    VALC090                                                          
         OI    SSOSTAT2,SSOSROLC   TRIGGER WRITING OF RECOV RECS                
         NI    SSOSTAT2,255-SSOSNRCV                                            
         OI    SSOFLAG1,SSOFRCVR   MAKE JOB RECOVER ON FAILURE                  
                                                                                
* FINALLY CALL MEDSID                                                           
                                                                                
VALC090  L     RF,VUTL             SET SE NO. FOR CHOSEN SYS                    
         MVI   4(RF),CONQ                                                       
                                                                                
         L     R7,AIOAREA2         OPEN CT FILE FIRST                           
         GOTO1 VDATAMGR,DMCB,DMOPEN2,CONSYS2,FILELIST,(R7)                      
                                                                                
         L     R2,AIOAREA1         READ SYSTEM LIST ACC RECORD                  
         USING CTWREC,R2                                                        
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVI   CTWKREC,CTWKRSYS                                                 
         MVI   CTWKSYSN,ACCQ                                                    
         GOTOR VDATAMGR,DMCB,DMREAD,CTFILE,CTWREC,CTWREC                        
         JNE   VALCISYS            NOT FOUND - INVALID                          
         USING CTLSTD,R1                                                        
         LA    R1,CTWDATA                                                       
                                                                                
VALC092  CLI   CTLSTD,0            TEST END OF RECORD - INVALID                 
         JE    VALCISYS                                                         
         CLI   CTLSTEL,CTLSTELQ    TEST LIST ELEMENT                            
         JNE   VALC094                                                          
         CLI   CTLSTSYS,ACCQ       TEST ACCPAK SE LIST ENTRY                    
         JNE   VALC094                                                          
         CLC   CTLSTNAM(3),SYSACC                                               
         JNE   VALC094                                                          
         CLC   CTLSTNAM+3(2),THISSEAL                                           
         JNE   VALC094                                                          
         MVC   THISSENO,CTLSTSE                                                 
         J     VALC096                                                          
                                                                                
VALC094  LLC   R0,CTLSTLEN         BUMP TO NEXT ELEMENT ON LIST RE              
         AR    R1,R0                                                            
         J     VALC092                                                          
         DROP  R1,R2                                                            
                                                                                
VALC096  MVC   HALF,THISSEAL                                                    
         CLC   THISSEAL,=C'0 '                                                  
         JNE   VALC098                                                          
         MVC   HALF,=C'T1'                                                      
                                                                                
VALC098  ZAP   LINE,=P'99'                                                      
         MVC   TITLE(20),=CL20'AccPay 2 Jobs/Invs'                              
***      L     R7,AIOAREA1                                                      
***      GOTO1 VDATAMGR,DMCB,DMCLOSE2,CONSYS2,FILELIST,(R7)                     
         J     VALCARDX                                                         
                                                                                
VALCISYS MVC   CONSMSG(40),=CL40'INVALID SYSTEM SPECIFIED'                      
         LA    R2,100                                                           
         J     VALCDIE                                                          
VALCMISS MVC   CONSMSG(40),=CL40'PUT SOME PARAMETER CARDS IN PLEASE'            
         LA    R2,100                                                           
         J     VALCDIE                                                          
VALCSYS  MVC   CONSMSG(30),=CL30'MISSING WHICHSYS='                             
         LA    R2,101                                                           
         J     VALCDIE                                                          
VALCUPDF MVC   CONSMSG(30),=CL30'MISSING UPDATFIL='                             
         LA    R2,102                                                           
         J     VALCDIE                                                          
VALCFILE MVC   CONSMSG(30),=CL30'MISSING DSPACE='                               
         LA    R2,103                                                           
         J     VALCDIE                                                          
VALCINP  MVC   CONSMSG(30),=CL30'MISSING OR BAD INPUT='                         
         LA    R2,104                                                           
         J     VALCDIE                                                          
VALCTYPE MVC   CONSMSG(30),=CL30'MISSING PROCJOB/PROCINV CARD'                  
         LA    R2,105                                                           
         J     VALCDIE                                                          
VALCRECM MVC   CONSMSG(40),=CL40'WRITERECOV= NEEDED ON UPDATIVE RUN'            
         LA    R2,106                                                           
         J     VALCDIE                                                          
VALCRECO MVC   CONSMSG(40),=CL40'UPDATIVE RUN NEEDED FOR RECOVERY'              
         LA    R2,107                                                           
         J     VALCDIE                                                          
VALCLOCK MVC   CONSMSG(50),=CL50'MUST PROVIDE ANYLOCKS=YES/NO CARD'             
         LA    R2,108                                                           
         J     VALCDIE                                                          
VALCMODE MVC   CONSMSG(50),=CL50'MUST PROVIDE MODE=ACUP/CONV CARD'              
         LA    R2,109                                                           
         J     VALCDIE                                                          
                                                                                
VALCERR  MVC   CONSMSG(20),=C'INVALID CONTROL CARD'                             
         LA    R2,420              PARAMETER TYPE ERROR                         
                                                                                
VALCDIE  GOTO1 VPUTERRM,WORKD                                                   
         ABEND (R2),DUMP                                                        
                                                                                
VALCARDX XIT1  ,                                                                
                                                                                
DMREAD   DC    CL8'DMREAD'                                                      
DMOPEN2  DC    CL8'DMOPEN'                                                      
DMCLOSE2 DC    CL8'DMCLSE'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
CONSYS2  DC    CL4'CON'                                                         
SYSACC   DC    CL4'ACC'                                                         
SYSMED   DC    CL4'MED'                                                         
ACUPQ    DC    CL4'ACUP'                                                        
CONVQ    DC    CL4'CONV'                                                        
                                                                                
FILELIST DS    0XL9                                                             
         DC    C'NCTFILE '         CONTROL FILE (FOR USERIDS)                   
         DC    C'X'                                                             
                                                                                
         DROP  R4                                                               
                                                                                
* READDELS=Y/N                                                                  
                                                                                
RDELVAL  NTR1  ,                                                                
         CLI   9(R7),YESQ                                                       
         JE    RDELV2                                                           
         CLI   9(R7),NOQ                                                        
         JNE   CARDERR                                                          
RDELV2   MVC   READDELS,9(R7)                                                   
         J     CARDOK                                                           
                                                                                
* RECOVFOUT=                                                                    
                                                                                
RECOFOUT NTR1  ,                                                                
         CLI   10(R7),YESQ                                                      
         JE    RECOFO2                                                          
         CLI   10(R7),NOQ                                                       
         JNE   CARDERR                                                          
RECOFO2  MVC   RECOVOUT,10(R7)                                                  
         J     CARDOK                                                           
                                                                                
* WHICHSYS=AA                                                                   
* JUST VALIDATE BASIC FORMAT AND CALL MEDSID @ END OF VALIDATION                
                                                                                
SYSVAL   NTR1  ,                                                                
         LA    R2,79(R7)           FIND LENGTH OF RHS                           
         LHI   RE,-1                                                            
         LA    RF,8(R7)                                                         
         CLI   0(R2),C' '                                                       
         JNE   SYSV5                                                            
         JXH   R2,RE,*-8                                                        
         J     CARDERR                                                          
SYSV5    SR    R2,RF                                                            
         CHI   R2,1                                                             
         JL    SYSV10                                                           
         CHI   R2,2                                                             
         JH    CARDERR                                                          
SYSV10   MVC   THISSEAL,9(R7)                                                   
         J     CARDOK                                                           
                                                                                
* WHICHCOMP=NN                                                                  
                                                                                
COMPVAL  NTR1  ,                                                                
         LA    R2,79(R7)           FIND LENGTH OF RHS                           
         LHI   RE,-1                                                            
         LA    RF,10(R7)                                                        
         CLI   0(R2),C' '                                                       
         JNE   *+12                                                             
         JXH   R2,RE,*-8                                                        
         J     CARDERR                                                          
                                                                                
         SR    R2,RF                                                            
         CHI   R2,1                                                             
         JNE   CARDERR                                                          
         GOTO1 VHEXIN,DMCB,10(R7),THISCOMP,2                                    
                                                                                
         CLI   THISCOMP,X'41'                                                   
         JL    CARDERR                                                          
         CLI   THISCOMP,X'FE'                                                   
         JNL   CARDERR                                                          
         J     CARDOK                                                           
                                                                                
* WRITERECOV=YES/NO (ONLYQ NOT SUPPORTED)                                       
                                                                                
WRITERCV NTR1  ,                                                                
         CLI   11(R7),YESQ                                                      
         JE    WRITER2                                                          
         CLI   11(R7),NOQ                                                       
         JNE   CARDERR                                                          
WRITER2  MVC   ANYRECOV,11(R7)                                                  
         J     CARDOK                                                           
                                                                                
* ANYLOCKS=YES/NO                                                               
                                                                                
LOCKVAL  NTR1  ,                                                                
         CLI   9(R7),YESQ                                                       
         JE    LOCKV2                                                           
         CLI   9(R7),NOQ                                                        
         JNE   CARDERR                                                          
LOCKV2   MVC   ANYLOCKS,9(R7)                                                   
         J     CARDOK                                                           
                                                                                
* STARTFROM=nnnn                                                                
                                                                                
STFROM   NTR1  ,                                                                
         MVC   FULL1,10(R7)                                                     
         MVC   FULL,ZEROES                                                      
         MVZ   FULL,FULL1                                                       
         CLC   FULL,ZEROES                                                      
         JNE   CARDERR                                                          
         PACK  DUB,FULL1                                                        
         CVB   R1,DUB                                                           
         STH   R1,SKIPNTRS                                                      
         J     CARDOK                                                           
* MEDCHA=...                                                                    
                                                                                
SETMCHA  NTR1  ,                                                                
         MVC   VALMCHA,7(R7)                                                    
         J     CARDOK                                                           
                                                                                
* MODE= ...                                                                     
                                                                                
SETMODE  NTR1  ,                                                                
         CLC   ACUPQ,5(R7)                                                      
         JE    SETMODE2                                                         
         CLC   CONVQ,5(R7)                                                      
         JE    SETMODE4                                                         
         J     CARDERR                                                          
                                                                                
SETMODE2 MVI   RUNMODE,RUNACUPQ                                                 
         J     CARDOK                                                           
                                                                                
SETMODE4 MVI   RUNMODE,RUNCONVQ                                                 
         J     CARDOK                                                           
                                                                                
* PATCH XXXXXX XXXX                                                             
* MULTIPLE CARDS ALLOWED                                                        
                                                                                
PATCHIT  NTR1  ,                                                                
         XC    FULL1,FULL1         GET DISPLACEMENT INTO FULL1                  
         GOTO1 VHEXIN,DMCB,6(R7),FULL1+1,6                                      
         CLC   DMCB+12(4),=F'3'                                                 
         JNE   CARDERR             MUST BE 6 HEX DIGITS                         
         LA    R2,79(R7)           FIND LENGTH OF PATCH DATA                    
         LHI   RE,-1                                                            
         LA    RF,12(R7)                                                        
         CLI   0(R2),C' '                                                       
         JNE   *+12                                                             
         JXH   R2,RE,*-8                                                        
         J     CARDERR                                                          
                                                                                
         SR    R2,RF               L'PATCH DATA IN R2                           
         GOTO1 VHEXIN,DMCB,13(R7),WORK,(R2)                                     
         ICM   R1,15,DMCB+12       GET L'HEX PATCH DATA IN R1                   
         JZ    CARDERR             ZERO IS NOT ALLOWED                          
         BCTR  R1,0                                                             
         L     RF,FULL1            PATCH DISPLACEMENT IN RF                     
         A     RF,BASERB           RF = A(AREA TO BE PATCHED)                   
         MVC   WORK2(0),0(RF)                                                   
         EX    R1,*-6              PRINT OLD DATA                               
         MVC   0(0,RF),WORK                                                     
         EX    R1,*-6              MOVE IN THE PATCH DATA                       
         LA    RF,1(R1)                                                         
         GOTO1 VHEXOUT,DMCB,WORK2,P+15,(RF),=C'TOG'                             
         MVC   P(15),=CL15'OLD DATA WAS:'                                       
         GOTO1 VPRINTER                                                         
         J     CARDOK                                                           
                                                                                
* DDSIO=DDSIO?                                                                  
                                                                                
DDSIO    NTR1  ,                                                                
         MVC   0(8,RF),6(R7)                                                    
         J     CARDOK                                                           
                                                                                
CARDOK   CR    RB,RB                                                            
         J     *+6                                                              
CARDERR  CR    RB,RE                                                            
         J     VALCARDX                                                         
                                                                                
* TRACE=                                                                        
* (MULTI CARDS ALLOWED)                                                         
TRACER   NTR1  ,                                                                
         CLC   6(8,R7),=C'ACCMSTIO'                                             
         JNE   *+12                                                             
         OI    TRACEIND,TRACEMIQ                                                
         J     CARDOK                                                           
         CLC   6(8,R7),=C'ACCARCIO'                                             
         JNE   *+12                                                             
         OI    TRACEIND,TRACEAIQ                                                
         J     CARDOK                                                           
         CLC   6(8,R7),=C'ACCDIRIO'                                             
         JNE   CARDERR                                                          
         OI    TRACEIND,TRACEDIQ                                                
         J     CARDOK                                                           
                                                                                
UPDFVAL  NTR1  ,                                                                
         MVI   UPDATFIL,UPREPORQ                                                
         CLI   11(R7),NOQ                                                       
         JE    CARDOK                                                           
         MVI   UPDATFIL,UPUPDATQ                                                
         CLI   11(R7),YESQ                                                      
         JE    CARDOK                                                           
         J     CARDERR                                                          
                                                                                
ZEROES   DC    CL8'00000000'                                                    
                                                                                
CARDTAB  DS    0H                                                               
         DC    C'AGENCY    ',AL1(5),AL1(0),AL1(0),AL4(SWAPAGY-WORKD)            
         DC    C'ANYLOCKS  ',AL1(7),AL1(0),AL1(CARDRTNQ),AL4(LOCKVAL)           
         DC    C'ACCOUNT   ',AL1(6),AL1(11),AL1(0),AL4(THISULA-WORKD)           
         DC    C'CONSOLEM  ',AL1(7),AL1(0),AL1(0),AL4(CONSOLMS-WORKD)           
         DC    C'DDSIO     ',AL1(4),AL1(0),AL1(CARDRTNQ),AL4(DDSIO)             
         DC    C'DSPACE    ',AL1(5),AL1(0),AL1(0),AL4(DSPACE-WORKD)             
         DC    C'DUMPFOUT  ',AL1(7),AL1(0),AL1(0),AL4(DUMPOFIL-WORKD)           
         DC    C'MEDCHA    ',AL1(5),AL1(0),AL1(CARDRTNQ),AL4(SETMCHA)           
         DC    C'MODE      ',AL1(3),AL1(0),AL1(CARDRTNQ),AL4(SETMODE)           
         DC    C'OVERRIDE  ',AL1(7),AL1(0),AL1(0),AL4(OVRIDE-WORKD)             
         DC    C'PATCH     ',AL1(4),AL1(0),AL1(CARDRTNQ),AL4(PATCHIT)           
         DC    C'PROCJOB   ',AL1(6),AL1(0),AL1(0),AL4(PROCJOB-WORKD)            
         DC    C'PROCINV   ',AL1(6),AL1(0),AL1(0),AL4(PROCINV-WORKD)            
         DC    C'READDELS  ',AL1(7),AL1(0),AL1(CARDRTNQ),AL4(RDELVAL)           
         DC    C'RECOVFOUT ',AL1(8),AL1(0),AL1(CARDRTNQ),AL4(RECOFOUT)          
         DC    C'STARTFROM ',AL1(8),AL1(0),AL1(CARDRTNQ),AL4(STFROM)            
         DC    C'TRACE     ',AL1(4),AL1(0),AL1(CARDRTNQ),AL4(TRACER)            
         DC    C'UPDATEFILE',AL1(9),AL1(1),AL1(CARDRTNQ),AL4(UPDFVAL)           
         DC    C'WHICHSYS  ',AL1(7),AL1(0),AL1(CARDRTNQ),AL4(SYSVAL)            
         DC    C'WHICHCOMP ',AL1(8),AL1(0),AL1(CARDRTNQ),AL4(COMPVAL)           
         DC    C'WRITERECOV',AL1(9),AL1(0),AL1(CARDRTNQ),AL4(WRITERCV)          
         DC    AL1(EOTQ)                                                        
                                                                                
* LITERALS FOR VALCARD CSECT                                                    
                                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* MESSAGE WRITER                                                                
* WRITES MESSAGE IN CONSMSG, UNBUFFERED, NO REPLY                               
* ERROR MESSAGES ARE WRITTEN TO PRINTER AND CONSOLE,                            
* OTHER MESSAGES ONLY TO CONSOLE, AND ONLY IF LOG=YES                           
***********************************************************************         
                                                                                
PUTERRM  CSECT                                                                  
         NMOD1 0,**PERM**                                                       
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         GOTO1 VPRINTER                                                         
         ZAP   LINE,=P'99'                                                      
         MVC   P(L'CONSMSG),CONSMSG                                             
         GOTO1 VPRINTER                                                         
         BASR  RE,RF                                                            
         LLC   R0,CONSOLMS                                                      
         MVI   CONSOLMS,YESQ                                                    
         GOTO1 VPUTMESS,WORKD                                                   
         STC   R0,CONSOLMS                                                      
         XIT1  ,                                                                
         LTORG                                                                  
***********************************************************************         
* WRITE MESSAGE IF LOG=YES                                            *         
***********************************************************************         
                                                                                
PUTMESS  CSECT                                                                  
         NMOD1 0,**PMES**                                                       
         LR    RC,R1                                                            
         CLI   CONSOLMS,YESQ                                                    
         JNE   PUTMES10                                                         
                                                                                
PUTMES4  LA    RF,CONSMSG+L'CONSMSG-1                                           
PUTMES6  CLI   0(RF),C' '                                                       
         JH    PUTMES8                                                          
         JCT   RF,PUTMES6                                                       
PUTMES8  LA    RE,CONSMSG-1                                                     
         SR    RF,RE                                                            
         GOTO1 VLOGIO,DMCB,X'FF000001',((RF),CONSMSG)                           
PUTMES10 XC    CONSMSG,CONSMSG                                                  
         XIT1  ,                                                                
         LTORG                                                                  
                                                                                
***********************************************************************         
* OFFLINE COMFACS                                                     *         
***********************************************************************         
                                                                                
COMFACS  CSECT ,                                                                
         DC    (COMFACSL)X'00'                                                  
                                                                                
*************************************************************                   
* INCLUDES                                                                      
*************************************************************                   
                                                                                
* ACGENFILE                                                                     
* CTGENFILE                                                                     
* SEACSFILE                                                                     
* ACRECEQUS                                                                     
* DDDPRINT                                                                      
* DDSYSELD                                                                      
* FASSBOFF                                                                      
* FASELIST                                                                      
* DMRCVRHDR                                                                     
* FAUTL                                                                         
* DDCOMFACS                                                                     
* ACRCVRECD                                                                     
* ACMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACPAY2JD                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE ACRECEQUS                                                      
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DDSYSELD                                                       
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE FASELIST                                                       
       ++INCLUDE DMRCVRHDR                                                      
       ++INCLUDE FAUTL                                                          
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE ACRCVRECD                                                      
       ++INCLUDE ACMSGEQUS                                                      
       ++INCLUDE ACLDCPTRD                                                      
         PRINT ON                                                               
                                                                                
CTLSTD   DSECT                                                                  
         ORG   CTLSTDTA                                                         
CTLSTNAM DS    CL7                 SE NAME                                      
CTLSTSYS DS    XL1                 CALLOV SYSTEM NUMBER                         
CTLSTSE  DS    XL1                 SE NUMBER                                    
         ORG                                                                    
                                                                                
***********************************************************************         
*  SUNDRY EQUATES                                                     *         
***********************************************************************         
                                                                                
EOTQ     EQU   255                                                              
FFQ      EQU   X'FF'                                                            
ACCQ     EQU   X'06'                                                            
MEDQ     EQU   X'04'                                                            
CONQ     EQU   X'0A'                                                            
SPACEQ   EQU   C' '                                                             
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
ONLYQ    EQU   C'O'                                                             
NULLQ    EQU   0                                                                
                                                                                
DMRDHIQ  EQU   1                                                                
DMREADQ  EQU   2                                                                
DMRSEQQ  EQU   3                                                                
DMWRTQ   EQU   4                                                                
DMADDQ   EQU   5                                                                
ADDRECQ  EQU   6                                                                
GETRECQ  EQU   7                                                                
PUTRECQ  EQU   8                                                                
                                                                                
IOEEOF   EQU   X'80'               END-OF-FILE                                  
IOEDSK   EQU   X'40'               NON-RECOVERABLE DISK ERROR                   
IOEDUP   EQU   X'20'               DUPLICATE KEY ON ADD                         
IOERNF   EQU   X'10'               RECORD NOT FOUND                             
IOEDEL   EQU   X'02'               RECORD IS DELETED                            
                                                                                
***********************************************************************         
*  OTHER DSECTS                                                       *         
***********************************************************************         
                                                                                
CARDTABD DSECT                                                                  
CARDKEY  DS    CL10                                                             
CARDKXLN DS    XL1                                                              
CARDVXLN DS    XL1                                                              
CARDIND  DS    XL1                                                              
CARDRTNQ EQU   X'80'                                                            
CARDVDSP DS    XL4                                                              
CARDTBLQ EQU   *-CARDTABD                                                       
                                                                                
* OPERATOR COMMS                                                                
                                                                                
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
                                                                                
***********************************************************************         
* OTHER CSECTS                                                        *         
***********************************************************************         
                                                                                
UTL      CSECT                     DUMMY UTL CSECT                              
         DC    X'0000000000000000'                                              
                                                                                
SSB      CSECT                                                                  
         DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSB+(SSOXTND-SSOOFF)                                             
         DC    X'FF'               OFFLINE EXTENSION IN USE                     
         ORG   SSB+(SSOSTAT2-SSOOFF)                                            
         DC    AL1(0)                                                           
         ORG                                                                    
                                                                                
*        DC    AL1(SSOSNRCV+SSOSGALO) NO RECOVERY+GLOBAL ALLOC                  
                                                                                
***********************************************************************         
* WORKING STORAGE AND REGISTER SAVE AREA                              *         
***********************************************************************         
                                                                                
REGSAVE  CSECT                                                                  
         DS    (WORKX+80000-WORKD)C                                             
                                                                                
***********************************************************************         
* FULL TRACK READ BUFFER                                              *         
***********************************************************************         
                                                                                
MTRBUFF  CSECT                                                                  
         DS    (64*1024)C                                                       
                                                                                
***********************************************************************         
* ARRAY DATA BUFFER                                                   *         
***********************************************************************         
                                                                                
JARAYC   CSECT                                                                  
         DS    ((JARAYLQ*JARAY#)+1)X                                            
JARAY#   EQU   20000                                                            
                                                                                
*************************************************************                   
* WORKING STORAGE DEFINITIONS                               *                   
*************************************************************                   
                                                                                
WORKD    DSECT                                                                  
WORKDESC DS    CL8                 EYECATCHER                                   
DMWORK   DS    12D                                                              
DUB      DS    D                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
DUB3     DS    D                                                                
FULL     DS    F                                                                
FULL1    DS    F                                                                
FULL2    DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
BYTE2    DS    X                                                                
BYTE3    DS    X                                                                
SAVERE   DS    F                                                                
SAVERE2  DS    F                                                                
RELO     DS    A                                                                
BASERB   DS    A                                                                
BASERA   DS    A                                                                
BASERD   DS    A                                                                
                                                                                
VADDAY   DS    V                   ** EXTERNAL MODULES **                       
VCARDS   DS    V                                                                
VCPRINT  DS    V                                                                
VDATAMGR DS    V                                                                
VDATCON  DS    V                                                                
VPAY2JOB DS    V                                                                
VHELLO   DS    V                                                                
VHELEN   DS    V                                                                
VHEXIN   DS    V                                                                
VHEXOUT  DS    V                                                                
VLOGIO   DS    V                                                                
VPRINTER DS    V                                                                
VPRINT   DS    V                                                                
VRECTYP  DS    V                                                                
VPROMOTE DS    V                                                                
VSORTER  DS    V                                                                
VUTL     DS    V                                                                
VLOADER  DS    V                                                                
VMEDCHA  DS    V                                                                
                                                                                
SOMFACS  DS    8AL4                                                             
                                                                                
VACDIRIO DS    F                   ** LOCAL ROUTINES, ETC **                    
VACMSTIO DS    F                                                                
VACARCIO DS    F                                                                
VPUTERRM DS    F                                                                
VPUTMESS DS    F                                                                
VPRINTIT DS    F                                                                
VIOERROR DS    F                                                                
VIOTRACE DS    F                                                                
                                                                                
APADDLE  DS    A                                                                
AOPERECB DS    A                                                                
ACOMBLOC DS    A                                                                
AMTRBUFF DS    A                                                                
AJARBUFF DS    A                                                                
AJARAY   DS    A                                                                
ADMPFOUT DS    A                                                                
AP2JIN   DS    A                                                                
ARCVFOUT DS    A                                                                
ASSB     DS    A                                                                
ACOMFACS DS    A                                                                
AACCFACS DS    A                                                                
                                                                                
DMCB     DS    6F                                                               
DMCB2    DS    6F                                                               
DMCBIO   DS    6F                                                               
ACADDMCB DS    6F                                                               
ACDADMCB DS    6F                                                               
ACGTDMCB DS    6F                                                               
ACHIDMCB DS    6F                                                               
ACPUDMCB DS    6F                                                               
ACRDDMCB DS    6F                                                               
ACSQDMCB DS    6F                                                               
ACWRDMCB DS    6F                                                               
                                                                                
AIOB4H   DS    A                                                                
AIOAFH   DS    A                                                                
AIOAREA1 DS    A                                                                
AIOAREA2 DS    A                                                                
AIOAREA3 DS    A                                                                
AIOARCAR DS    A                                                                
                                                                                
ABUFFCPY DS    A                                                                
TAPECNTR DS    F                                                                
                                                                                
SETTINGS EQU   *                   PARAMETER CARD SETTING AREAS                 
ANYCARDS DS    CL1                                                              
UPDATFIL DS    XL1                                                              
UPREPORQ EQU   X'01'                                                            
UPUPDATQ EQU   X'02'                                                            
MDUMPOTQ EQU   X'80'                                                            
                                                                                
WHICHSYS DS    XL1                                                              
THISULA  DS    CL14                                                             
THISSEAL DS    CL2                 SENUM OF SPECIFIED MEDIA SYS (ALPHA)         
THISSENO DS    XL1                 SENUM OF SPECIFIED MEDIA SYSTEM              
THISCOMP DS    XL1                 OPTIONAL AGENCY NUMBER                       
TRACEIND DS    XL1                                                              
TRACEDIQ EQU   X'08'                                                            
TRACEMIQ EQU   X'04'                                                            
TRACEAIQ EQU   X'04'                                                            
                                                                                
ANYLOCKS DS    XL1                                                              
ANYRECOV DS    XL1                                                              
CONSOLMS DS    XL1                                                              
DSPACE   DS    XL1                                                              
SWAPAGY  DS    XL1                                                              
DUMPOFIL DS    XL1                                                              
OVRIDE   DS    XL1                                                              
READDELS DS    XL1                                                              
RECOVOUT DS    XL1                                                              
PROCJOB  DS    XL1                                                              
PROCINV  DS    XL1                                                              
VALMCHA  DS    CL20                                                             
SKIPNTRS DS    H                                                                
RUNMODE  DS    CL1                                                              
RUNACUPQ EQU   C'U'                                                             
RUNCONVQ EQU   C'C'                                                             
SETTINGX EQU   *                                                                
                                                                                
RETCODE  DS    XL1                                                              
                                                                                
ACCMST   DS    CL8                                                              
ACCARC   DS    CL8                                                              
ACCDIR   DS    CL8                                                              
                                                                                
ACDIRREC DS    0XL54               ACCDIR DIRECTORY REC                         
AKEY     DS    XL(L'ACTKEY)                                                     
ASTAT    DS    XL(L'ACTKSTA)                                                    
ADA      DS    XL4                                                              
         DS    XL10                                                             
                                                                                
AKEYSAVE DS    XL(L'ACTKEY)                                                     
AKEY2    DS    XL(L'ACTKEY)                                                     
AKEYSV   DS    XL(L'ACTKEY)                                                     
TESTKEY  DS    XL(L'ACTKEY)                                                     
LASTKEY  DS    XL(L'ACTKEY)                                                     
         DS    XL16                SPARE FOR LAST KEY                           
                                                                                
CPRNIND  DS    CL1                                                              
CNTATRN  DS    PL6                                                              
CNTPTRN  DS    PL6                                                              
CNTUTRN  DS    PL6                                                              
CNTUACC  DS    PL6                                                              
CNTUINV  DS    PL6                                                              
CNTERRS  DS    PL6                                                              
CNTATRNC DS    PL6                                                              
CNTPTRNC DS    PL6                                                              
CNTUTRNC DS    PL6                                                              
CNTUACCC DS    PL6                                                              
CNTUINVC DS    PL6                                                              
CNTERRSC DS    PL6                                                              
                                                                                
PCLILEN  DS    XL1                                                              
PPROLEN  DS    XL1                                                              
PJOBLEN  DS    XL1                                                              
                                                                                
JARBUFLN EQU   (JARAYLQ*(JARAYBU#+1))                                           
JARAYBU# EQU   75000-1                                                          
                                                                                
SVPASDIR DS    XL(L'ACDIRREC)                                                   
                                                                                
IOIND    DS    XL1                 INDICATOR FOR I/O ROUTINES                   
IORDELQ  EQU   X'08'               - READ FOR DELETES                           
IORUPDTQ EQU   X'80'               - READ FOR UPDATE                            
IOFTRQ   EQU   X'10'               - DO FULL TRACK READ                         
SVIOIND  DS    XL1                 CALLER'S LAST IOIND                          
IOCMD    DS    XL1                                                              
                                                                                
SORTACTV DS    XL1                                                              
ANYSORTD DS    CL1                                                              
THISROUT DS    XL1                                                              
THISFILE DS    XL1                                                              
DATEBIN  DS    XL3                                                              
THISDA   DS    XL4                                                              
VRSNCTRL DS    XL6                                                              
BUFFCOMP DS    XL1                                                              
TAPERROR DS    XL1                                                              
TAPERRDQ EQU   X'80'                                                            
TAPERRSQ EQU   X'08'                                                            
CURCOMP  DS    XL1                                                              
CURCUID  DS    XL2                                                              
CURCALP  DS    CL2                                                              
CURCLDG  DS    CL1                                                              
                                                                                
CONSMSG  DS    CL80                                                             
HIGHVALS DS    XL50                                                             
PRL      DS    CL(L'P)                                                          
WORK     DS    XL255                                                            
WORK2    DS    XL255                                                            
ELEM     DS    XL255                                                            
INDATA   DS    XL64                                                             
                                                                                
         DS    XL4                 RDW IF DUMPFOUT                              
IOAREA1  DS    XL(2000+2)          ALLOW EXTRA 2 BYTES FOR DMGR                 
         DS    XL4                                                              
IOAREA2  DS    XL(2000+2)                                                       
         DS    XL4                                                              
IOAREA3  DS    XL(2000+2)                                                       
         DS    XL4                                                              
IOARCAR  DS    XL(2000+2)                                                       
         DS    XL4                                                              
                                                                                
IOB4H    DS    F                   RECOVERY - BEFORE IMAGE                      
IOB4RH   DS    XL(L'RECVHDR)                                                    
IOB4RHLQ EQU   *-IOB4H                                                          
IOB4AREA DS    XL(2000+2)                                                       
                                                                                
IOAFH    DS    F                   RECOVERY - AFTER IMAGE                       
IOAFRH   DS    XL(L'RECVHDR)                                                    
IOAFRHLQ EQU   *-IOAFH                                                          
IOAFAREA DS    XL(2000+2)                                                       
                                                                                
WORKX    EQU   *                                                                
                                                                                
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACDCPAY2J 05/14/20'                                      
         END                                                                    
