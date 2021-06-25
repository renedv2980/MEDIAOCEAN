*          DATA SET SPWRI13    AT LEVEL 131 AS OF 10/10/16                      
*PHASE T20413A                                                                  
*INCLUDE SPFMTINO                                                               
         TITLE 'T20413 - SPOTPAK WRITER CASHFLOW REPORT'                        
***********************************************************************         
*                                                                     *         
*        THIS MODULE ONLY VALIDATES THE CASHFLOW SCREEN               *         
*        REPORT IS PRODUCED IN SPWRI1B                                *         
*                                                                     *         
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ---------- -------- ------------------------------------------ *         
* AKAT SPEC-6073  10/10/16 SUPPORT NEW CLDT SPECIAL                   *         
***********************************************************************         
         SPACE 2                                                                
T20413   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20413,RA,RR=R2                                                
*                                                                               
         L     R6,=A(WORKD)        A(WORKING STORAGE)                           
         AR    R6,R2                                                            
         USING WORKD,R6                                                         
*                                                                               
         ST    R2,RELO                                                          
*                                                                               
         MVC   SAVERD,4(RD)        SAVE A(CALLING PROGRAM'S SAVE AREA)          
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
*                                                                               
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
*        IF OFF-LINE, LOAD OVERLAY TO HANDLE REPORT                             
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFFLINE                          
         BNE   OFFLINEX                                                         
*                                                                               
         OC    VT2041B,VT2041B     SKIP IF ALREADY LOADED                       
         BNZ   OFFLINEX                                                         
*                                                                               
         XC    DMCB(4),DMCB                                                     
         MVI   DMCB,X'1B'          OVERLAY CONTAINING REPORT CODE               
*                                                                               
         GOTO1 CALLOV,DMCB,,0,0                                                 
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                NEED TO FIND OVERLAY                         
*                                                                               
         MVC   VT2041B,DMCB        SAVE ADDRESS                                 
*                                                                               
OFFLINEX DS    0H                                                               
*                                                                               
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPVAL        INITIALIZATION                               
         BE    START                                                            
*                                                                               
*        GO CHECK FOR REPORT MODES                                              
*                                                                               
         ICM   RF,15,VT2041B       GET OVERLAY ADDRESS                          
         BZ    XIT                 NOT THERE                                    
*                                                                               
         GOTO1 (RF),DMCB,(RC),(R6)                                              
*                                                                               
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
*                                                                               
START    DS    0H                                                               
*                                                                               
         BRAS  RE,INIT             INITIALIZATION                               
*                                                                               
         B     XIT                                                              
*                                                                               
VT2041B  DS    V                   V(T2041B)                                    
*                                                                               
         TITLE 'T20413 - SPOTPAK WRITER CASHFLOW REPORT - ERROR'                
***********************************************************************         
*                                                                     *         
*        ERROR EXITS AND MESSAGES                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
*                                                                               
         TITLE 'T20413 - SPOTPAK WRITER CASHFLOW REPORT - INIT'                 
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
INIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INIT70                                                           
*                                                                               
         TM    WHEN,X'20'          IF THIS IS SOON                              
         BNO   *+14                                                             
         OC    SBBCLT,SBBCLT       ALL CLIENTS IS INVALID                       
         BZ    ECLT                                                             
*                                                                               
INIT20   MVI   BILLOPT,C'N'        DEFAULT - NO BILLING                         
         MVI   CASHOPT,0                     NO CASH DATA                       
         MVI   VDRBNKDT,0                    NO VENDOR BANK DATE                
         MVI   VENDOPT,C'N'                  NO VENDOR DATA                     
         MVI   XPCTOPT,0                     DON'T DROP CLIENT % PAID           
         MVI   BANKOPT,0                     PRINT CHECK DEPOSIT DATE           
         MVI   BNKDTOPT,0                    BANK RECONCILIATION DATE           
         MVI   INVDTOPT,C'N'                 NO INVOICE DATE                    
         MVI   CHKSOPT,C'N'                  NO CHECK DATA SUPPRESSED           
         MVI   CHKIVOPT,C'N'                 INIT CHECK INVOICE OPTION          
         MVI   DOLLAR,C'N'                   NET DOLLARS                        
         MVI   DTOPT,1                       INV DATE                           
         MVI   DAYS,C'N'                     NO DAYS                            
         MVI   SPOTOPT,C'N'                  SHOW N'SPOTS                       
         MVI   ORDERED,C' '                  ORDERED                            
         MVI   BLNK,0                        BLANK LINE                         
         MVI   CALC,C'N'                                                        
         MVI   PSUBT,C'Y'                                                       
         MVI   SVCHKTAB,X'FF'                INIT CHECK NUMBER TABLE            
         MVI   WDATEOPT,0                    NO SPECIAL DATE OPTIONS            
         MVI   STANETOP,0                    NO STANET FILTER                   
*                                                                               
         OC    SBWRPROF+4(2),SBWRPROF+4      IF NOT 00 - SET EXTRA COL          
         BZ    *+8                                                              
         MVI   CALC,C'Y'                                                        
*                                                                               
         LA    R2,SCASPECH         VALIDATE SPECIAL OPTIONS                     
         BRAS  RE,VALOPT                                                        
*                                  COLUMNS                                      
         LA    R2,SCAHEADH         HEADERS                                      
         MVI   MAX,3                                                            
         GOTO1 VALHEAD                                                          
*                                                                               
         LA    R2,SCAMIDH          MIDLINE                                      
         MVI   MAX,1                                                            
         GOTO1 VALMID                                                           
*                                                                               
         LA    R2,SCAROWSH         ROWS                                         
         MVI   MAX,6                                                            
         GOTO1 VALROWS                                                          
         BRAS  RE,CKCFMON          MAKE SURE CFMON IS AN ENTRY                  
*                                                                               
         LA    R2,SCAPRDH          PRODUCT                                      
         CLC   8(3,R2),=C'POL'     TREAT AS ALL                                 
         BNE   INIT50                                                           
         MVI   SBQBPRD,0                                                        
         MVC   SBQPRD,=C'ALL'                                                   
*                                                                               
INIT50   LA    R2,SCATITH          TITLE                                        
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(19),=CL19'CASH FLOW REPORT'                                
         CLI   5(R2),0                                                          
         BE    INIT60                                                           
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
*                                                                               
INIT60   GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
INIT70   MVI   MYFIRSTH,12         SET DRIVER'S FIRST HEADLINE                  
         LA    R1,LEVELS           WHAT LEVEL IS QPER (CFMON) AT                
         LA    R2,12                                                            
         LA    R3,1                                                             
*                                                                               
INIT80   CLI   0(R1),QPER          SET LEVEL OF CFMON                           
         BE    INIT90                                                           
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R2,INIT80                                                        
*                                                                               
INIT90   STC   R3,QPERLEV          SET THE LEVEL THAT QPER IS AT                
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE, GET BUFFERS                      
         BNE   INITX                                                            
*                                                                               
         BRAS  RE,MVDPG            MOVE DPG CODE TO AFTER DRONE CODE            
*                                                                               
         L     R3,BPLREC           BILL DATE BUFFER                             
         M     R2,BPMAX                                                         
         ST    R3,DMCB+4                                                        
         ST    R3,DMCB+8                                                        
         ST    R3,BPLTAB                                                        
         GOTO1 COVAIL,DMCB,C'GET'                                               
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,BPATAB                                                        
*                                                                               
INITX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
ECLT     MVC   CONHEAD(49),=C'TOO MANY CLIENTS IN REQUEST. PLEASE RUN OX        
               VERNIGHT'                                                        
         LA    R2,SCACLTH                                                       
*                                                                               
         MVI   ERROR,X'FE'                                                      
         GOTOR CURSERR                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'SPWRI13 - CASH APPLIED DATA - CKCFMON'                          
***********************************************************************         
*                                                                     *         
*                                                                     *         
*        MAKE SURE CFMON/CFMOND IS AN ENTRY                           *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
CKCFMON  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,SCAROWSH         ROWS                                         
         LA    R3,6                                                             
*                                                                               
CKC10    CLC   8(5,R2),=C'CFMON'                                                
         BE    CKCX                                                             
         ZIC   R1,0(R2)            BUMP TO NEXT ENTRY                           
         AR    R2,R1                                                            
         BCT   R3,CKC10                                                         
         LA    R2,SCAROWSH                                                      
         B     CFERR                                                            
*                                                                               
CKCX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
CFERR    MVC   CONHEAD(19),=C'CFMON ENTRY MISSING'                              
         B     CFERRX                                                           
*                                                                               
CFERRX   MVI   ERROR,X'FE'                                                      
         GOTOR CURSERR                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'SPWRI13 - CASH APPLIED DATA - CKCFMON'                          
***********************************************************************         
*                                                                     *         
*        OPTIONS VALIDATION ROUTINE                                   *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VALOPT   NTR1  BASE=*,LABEL=*                                                   
         CLI   5(R2),0                                                          
         BE    OPTX                                                             
         MVI   FIELDERR,1                                                       
         GOTO1 SCANNER,DMCB,(70,(R2)),(12,AIO3)                                 
         ZIC   R0,4(R1)                                                         
         L     R4,AIO3                                                          
         LTR   R0,R0                                                            
         BZ    OPTINV                                                           
*                                                                               
OPTVALLP DS    0H                                                               
*                                                                               
OPTBINV  CLC   12(4,R4),=C'BINV'         INVOICE                                
         BNE   OPTBINVN                                                         
*                                                                               
         TM    BLNK,X'02'          IF NOT A RUNDATE REQUEST                     
         BO    *+8                                                              
         OI    BLNK,X'01'             DEFAULT TO INVOICE DUE DATE               
*                                                                               
         MVI   BILLOPT,C'Y'                                                     
         NI    SBQSKIP,X'FF'-SBQSKBIL    DON'T SKIP STATION BILL RECS           
*                                                                               
         CLC   =C'MND',16(R4)      IF FULL INVOICE # W/O DASHES                 
         BNE   *+12                                                             
         MVI   BILLOPT,C'2'                                                     
         B     OPTBINVX                                                         
*                                                                               
         CLI   16(R4),C'M'         IF FULL INVOICE # WANTED'                    
         BNE   *+8                                                              
         MVI   BILLOPT,C'1'                                                     
*                                                                               
OPTBINVX DS    0H                                                               
*                                                                               
         B     OPTVALCN                                                         
*                                                                               
OPTBINVN DS    0H                                                               
*                                                                               
OPTCASH  CLC   12(4,R4),=C'CASH'         CASH RECEIVED DATA                     
         BNE   OPTCASHX                                                         
*                                                                               
         OI    CASHOPT,X'80'                                                    
*                                                                               
         LA    RF,SBLOCK           ESTABLISH END OF SPOTBLOCK                   
         USING SBLOCK,RF                                                        
*                                                                               
         OI    SBQREAD2,SBQRD2CA         SET INDICATOR                          
*                                                                               
         DROP  RF                                                               
*                                                                               
         NI    SBQSKIP,X'FF'-SBQSKBIL    DON'T SKIP STATION BILL RECS           
*                                                                               
         B     OPTVALCN                                                         
*                                                                               
OPTCASHX DS    0H                                                               
*                                                                               
OPTCBU   CLC   12(3,R4),=C'CBU'          CBU - CLEARED BUT UNDISBURSED          
         BNE   OPTCBUX                                                          
*                                                                               
         MVI   CBUOPT,C'Y'                                                      
*                                                                               
         B     OPTVALCN                                                         
*                                                                               
OPTCBUX DS     0H                                                               
*                                                                               
OPTSNT   CLC   12(6,R4),=C'STANET'       STANET - SUM BY LOCAL NETWORKS         
         BNE   OPTSNTX                                                          
*                                                                               
         MVI   STANETOP,STANETQ                                                 
*                                                                               
         B     OPTVALCN                                                         
*                                                                               
OPTSNTX DS     0H                                                               
*                                                                               
OPTNVD   CLC   12(8,R4),=C'NOVDRDTE'     NOVDRDTE -NO VENDOR INV DTE            
         BNE   OPTNVDX                                                          
*                                                                               
         MVI   NOVDRDTE,C'Y'                                                    
*                                                                               
         B     OPTVALCN                                                         
*                                                                               
OPTNVDX DS     0H                                                               
*                                                                               
OPTVEND  CLC   12(6,R4),=C'VENDOR'       VENDOR INVOICE DATA                    
         BNE   OPTVENDX                                                         
*                                                                               
         MVI   VENDOPT,C'V'                                                     
*                                                                               
         LA    RF,SBLOCK           ESTABLISH END OF SPOTBLOCK                   
         USING SBLOCK,RF                                                        
         OI    SBQREAD,SBQRDINV          SET INDICATOR                          
         DROP  RF                                                               
*                                                                               
         B     OPTVALCN                                                         
*                                                                               
OPTVENDX DS    0H                                                               
*                                                                               
OPTCHKS  CLC   12(5,R4),=C'NOPAY'        SUPPRESS PAY DATA                      
         BNE   OPTCHKSX                                                         
         CLI   CHKSOPT,C'O'        NOT VALID IF REPORTING NON-DISBURSED         
         BE    OPTZEROE                                                         
         MVI   CHKSOPT,C'Y'                                                     
         B     OPTVALCN                                                         
OPTCHKSX DS    0H                                                               
*                                                                               
OPTZERO  CLC   =C'PAYPEND',12(R4)       REPORT ONLY NON-DISBURSED               
         BNE   OPTZEROX                                                         
         CLI   CHKSOPT,C'Y'        NOT VALID IF SUPRESSING PAY DATA             
         BE    OPTZEROE                                                         
         MVI   CHKSOPT,C'0'                                                     
         B     OPTVALCN                                                         
OPTZEROX DS    0H                                                               
*                                                                               
OPTCKRP  CLC   =C'CLREP',12(R4)       REPORT CHECK REP                          
         BNE   OPTCKRPX                                                         
         CLI   CHKSOPT,C'N'        VALID ONLY IF SHOWING PAY DATA               
         BNE   OPTCKRPX                                                         
         MVI   CHKRPOPT,C'Y'       SHOW PAY REP                                 
         B     OPTVALCN                                                         
OPTCKRPX DS    0H                                                               
*                                                                               
OPTCLDT  CLC   =C'CLDT',12(R4)     REPORT CLEARANCE DATE?                       
         BNE   OPTCLDTX            NO                                           
         CLI   CHKSOPT,C'N'        SHOWING PAY DATA?                            
         BNE   OPTCLDTX            NO - INVALID                                 
         MVI   CHKDTOPT,C'Y'       SHOW PAY REP                                 
         B     OPTVALCN                                                         
OPTCLDTX DS    0H                                                               
*                                                                               
OPTCKIV  CLC   =C'CLINV',12(R4)       REPORT CHECK INVOICE NUMBER               
         BNE   OPTCKIVX                                                         
         CLI   CHKSOPT,C'N'        VALID ONLY IF SHOWING PAY DATA               
         BNE   OPTCKIVX                                                         
         MVI   CHKIVOPT,C'Y'       SHOW PAY INVOICE DATA                        
         B     OPTVALCN                                                         
OPTCKIVX DS    0H                                                               
*                                                                               
OPTINVDT CLC   12(4,R4),=C'INVDT'        INVOICE DATE TO PRINT                  
         BNE   OPTINVDX                                                         
         MVI   INVDTOPT,C'Y'                                                    
         NI    SBQSKIP,X'FF'-SBQSKBIL    DON'T SKIP STATION BILL RECS           
         B     OPTVALCN                                                         
OPTINVDX DS   0H                                                                
*                                                                               
OPTMDY   CLC   12(8,R4),=C'MM/DD/YY'     MM/DD/YY DATE OPTION                   
         BNE   OPTMDYX                                                          
         MVI   WDATEOPT,X'2A'         SET TO PRINT DATES IN FORMAT              
         B     OPTVALCN                                                         
OPTMDYX  DS   0H                                                                
*                                                                               
OPTXPCT  CLC   12(4,R4),=C'XPCT'      DROP CLIENT PERRCENT PAID                 
         BNE   OPTXPCTX                                                         
*                                                                               
         MVI   XPCTOPT,C'Y'           SET OPTION                                
*                                                                               
         B     OPTVALCN                                                         
*                                                                               
OPTXPCTX DS   0H                                                                
*                                                                               
OPTBKDT  CLC   12(8,R4),=C'BANKDATE'  USE BANK RECONCILIATION DATE              
         BNE   OPTBKDTX                                                         
*                                                                               
         MVI   VDRBNKDT,C'Y'          SET OPTION                                
*                                                                               
         B     OPTVALCN                                                         
*                                                                               
OPTBKDTX DS   0H                                                                
*                                                                               
OPTBANK  CLC   12(4,R4),=C'BANK'      PRINT BANK DEPOSIT DATE                   
         BNE   OPTBANKX                                                         
*                                                                               
         MVI   BANKOPT,C'Y'           SET OPTION                                
*                                                                               
         B     OPTVALCN                                                         
*                                                                               
OPTBANKX DS   0H                                                                
*                                                                               
OPT30    CLC   12(3,R4),=C'ACT'                                                 
         BNE   OPT40                                                            
         MVI   DOLLAR,C'A'                                                      
         B     OPTVALCN                                                         
*                                                                               
OPT40    CLC   12(5,R4),=C'GROSS'                                               
         BNE   OPT50                                                            
         MVI   DOLLAR,C'G'                                                      
         B     OPTVALCN                                                         
*                                                                               
OPT50    CLC   12(4,R4),=C'DAYS'                                                
         BNE   OPT60                                                            
         MVI   DAYS,C'Y'                                                        
         B     OPTVALCN                                                         
*                                                                               
OPT60    CLC   12(5,R4),=C'SPOTS'                                               
         BNE   OPT70                                                            
         MVI   SPOTOPT,C'S'                                                     
         B     OPTVALCN                                                         
*                                                                               
OPT70    CLC   12(4,R4),=C'SPTS'                                                
         BNE   OPT80                                                            
         MVI   SPOTOPT,C'S'                                                     
         B     OPTVALCN                                                         
*                                                                               
OPT80    CLC   12(8,R4),=C'SPOTSBLD'                                            
         BNE   OPT90                                                            
         MVI   SPOTOPT,C'B'                                                     
         B     OPTVALCN                                                         
*                                                                               
OPT90    CLC   12(7,R4),=C'SPOTSPD'                                             
         BNE   OPT100                                                           
         MVI   SPOTOPT,C'P'                                                     
         B     OPTVALCN                                                         
*                                                                               
OPT100   CLC   12(4,R4),=C'GORD'                                                
         BNE   OPT110                                                           
         MVI   ORDERED,C'G'                                                     
         B     OPTVALCN                                                         
*                                                                               
OPT110   CLC   12(4,R4),=C'NORD'                                                
         BNE   OPT120                                                           
         MVI   ORDERED,C'N'                                                     
         B     OPTVALCN                                                         
*                                                                               
OPT120   CLC   12(5,R4),=C'BLANK'                                               
         BNE   OPT130                                                           
         OI    BLNK,X'80'                                                       
         B     OPTVALCN                                                         
*                                                                               
OPT130   CLC   12(5,R4),=C'NOSUB'                                               
         BNE   OPT140                                                           
         MVI   PSUBT,C'N'                                                       
         OI    OPTIND3,OPTCFNT     DON'T SET TOTAL RTN FOR CFMON                
         B     OPTVALCN                                                         
*                                                                               
OPT140   CLC   12(7,R4),=C'RUNDATE'                                             
         BNE   OPT140X                                                          
*                                                                               
         NI    BLNK,X'FF'-X'01'    TURN OFF DUE DATE IND                        
         OI    BLNK,X'02'          TURN ON  RUN DATE IND                        
*                                                                               
         B     OPTVALCN                                                         
*                                                                               
OPT140X  DS    0H                                                               
*                                                                               
OPT141   CLC   12(8,R4),=C'NODUEDTE'                                            
         BNE   OPT141X                                                          
*                                                                               
         OI    BLNK,X'04'          TURN ON  NO DUE DATE IND                     
*                                                                               
         B     OPTVALCN                                                         
*                                                                               
OPT141X  DS    0H                                                               
*                                                                               
OPT150   B     OPTINV                                                           
*                                                                               
OPTVALCN LA    R4,70+22(R4)                                                     
         AI    FIELDERR,1                                                       
         BCT   R0,OPTVALLP                                                      
*                                                                               
OPTX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
OPTZEROE DS    0H                                                               
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         MVC   CONHEAD(37),=C'ZERO AND NOPAY ARE MUTUALLY EXCLUSIVE'            
         B     MYCURS                                                           
*                                                                               
OPTINV   MVI   ERROR,INVALID                                                    
         B     CURS                                                             
*                                                                               
MYCURS   MVI   ERROR,X'FE'                                                      
CURS     GOTO1 CURSERR                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T2041B - SPOTPAK WRITER CASHFLOW REPORT - MVDPG'                
***********************************************************************         
*                                                                     *         
*        MOVE DPG CODE AT ADPGPROG TO END OF WHAT DRONE JUST          *         
*        VALIDATED (DRCUFBUF)                                         *         
*                                                                     *         
* DRONEBLK MOVED IN RE-WORKED VERSION OF SPWRI (EJOR - 13JUL94)       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
MVDPG    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,4095(R9)                                                      
         LA    R5,1(R5)                                                         
         USING SYSD+4096,R5                                                     
*                                                                               
         L     R3,DRCURBUF         END OF BUFF-WHAT WAS JUST VALIDATED          
         L     R2,ADPGPROG         START OF DPG CODE                            
*                                                                               
MV10     CLI   0(R2),0             END OF CODE                                  
         BE    MV100                                                            
         ZIC   R1,1(R2)            L'ELEMENT                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)                                                    
         LA    R1,1(R1)            ADD 1 FOR BCTR                               
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         AR    R3,R1               BUMP TO NEXT AREA                            
         B     MV10                                                             
*                                                                               
MV100    MVC   ADPGPROG,DRSTBUF    SET START OF DPG CODE                        
         ST    R2,DRCURBUF                                                      
         AH    R2,=H'3'                                                         
         ST    R2,DRENDBUF                                                      
*                                                                               
MVDPGX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R5                                                               
*                                                                               
         TITLE 'SPWRI13 - CASH APPLIED DATA - INPUT - WORKD'                    
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
*                                                                               
WORKD    DS    0D                                                               
*                                                                               
OPTERRM  DC    C'ERROR - INVALID OPTION'                                        
         DS    0D                                                               
         DS    0D                                                               
*                                                                               
CSHIERC  DS    XL(CSHIERL)         DDCASHIER CONTROL BLOCK                      
*                                                                               
BDPARM   DS    0F                  PARM LIST FOR BINSRCH CALL FOR               
BPIND    DS    0XL1                BILL FORMULA BUFFER                          
BPAREC   DS    A                                                                
BPATAB   DS    A                                                                
         DC    F'0'                                                             
BPLREC   DC    AL4(L'BDREC)                                                     
         DC    AL1(0),AL3(L'BDKEY)                                              
BPMAX    DC    F'10000'            MAX N'RECORDS                                
BPLTAB   DS    F                   L'TABLE                                      
*                                                                               
VCASHIER DS    V(CASHIER)          V(DDCASHIER)                                 
VTSAROFF DS    V(TSAROFF)          V(TSAROFF)                                   
*                                                                               
XFF      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
FF       EQU   X'FF'                                                            
WDATEOPT DS    XL1                 DATE FORMAT OPTION                           
         EJECT                                                                  
*                                                                               
* HEADLINE POSITION TABLE                                                       
*                                                                               
HEADTAB  DC    C'N',X'01',X'00',X'000000',X'00',X'00'                           
         DC    C'N',X'01',X'01',X'050000',X'00',X'00'                           
         DC    C'N',X'01',X'02',X'050600',X'00',X'00'                           
         DC    C'N',X'01',X'03',X'050607',X'00',X'00'                           
         DC    C'Y',X'01',X'00',X'000000',X'00',X'07'                           
         DC    C'Y',X'01',X'01',X'050000',X'00',X'07'                           
         DC    C'Y',X'01',X'02',X'050600',X'00',X'08'                           
         DC    C'Y',X'01',X'03',X'050607',X'00',X'08'                           
         DC    C'A',X'02',X'00',X'000000',X'05',X'07'                           
         DC    C'A',X'02',X'01',X'050000',X'06',X'08'                           
         DC    C'A',X'02',X'02',X'040500',X'06',X'08'                           
         DC    C'A',X'02',X'03',X'040506',X'07',X'08'                           
         DC    C'A',X'03',X'00',X'000000',X'05',X'07'                           
         DC    C'A',X'03',X'01',X'050000',X'06',X'08'                           
         DC    C'A',X'03',X'02',X'040500',X'06',X'08'                           
         DC    C'A',X'03',X'03',X'040506',X'07',X'08'                           
         DC    X'00'                                                            
*                                                                               
MONTAB1  DC    C'0131'                                                          
         DC    C'0228'                                                          
         DC    C'0331'                                                          
         DC    C'0430'                                                          
         DC    C'0531'                                                          
         DC    C'0630'                                                          
         DC    C'0731'                                                          
         DC    C'0831'                                                          
         DC    C'0930'                                                          
         DC    C'1031'                                                          
         DC    C'1130'                                                          
         DC    C'1231'                                                          
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
AXTRA    DS    0F               ** EXTENTION ROUTINE ADDRESSES **               
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
SAVERD   DS    A                                                                
RELO     DS    A                                                                
WPKDIVD  DS    0PL16               DIVIDEND  - PACKED                           
WPKQUOT  DS    PL8                 QUOTIENT  - PACKED                           
WPKREMD  DS    PL8                 REMAINDER - PACKED                           
*                                                                               
CSHTBLVS DS    0A                  ADDRESSES IN T2041A WORKAREA OVERLAY         
VINVTBL  DC    A(0)               A(INVOICE SQN NUMBER TABLE)                   
VSTBTBL  DC    A(0)               A(STATION BILLING SQN NUMBER TABLE)           
VCSHREC  DC    A(0)               A(CASHIER RECORD SAVE AREA)                   
VCLTREC  DC    A(0)               A(CLIENT RECORD SAVEAREA)                     
         DC    16A(0)              SPARE                                        
*                                                                               
MYQUOTE  DS    D                                                                
MYREMAIN DS    D                                                                
MYDUB    DS    D                                                                
*                                                                               
SVUTLA   DS    A                   A(UTL)                                       
SVBKEY   DC    XL(L'BKEY)'00'                                                   
SVKEY    DS    XL13                                                             
SVSTAB   DS    XL21                STATION BILL ELEMENT SAVEAREA                
SVINV    DS    CL(L'CLS3INV)       CHECK INVOICE SAVE                           
SVCHKNUM DS    CL(L'SCCHKNUM)      CHECK NUMBER SAVE                            
SVINVNO  DS    CL10                VENDOR INVOICE NUIMBER SAVE                  
SVDSLN   DS    XL1                 SPOTLENGTH SAVEAREA                          
SVDPRD   DS    XL1                 PRODUCT    SAVEAREA                          
SVDPR2   DS    XL1                 PRODUCT 2  SAVEAREA                          
SVDEST   DS    XL1                 ESTIMATE   SAVEAREA                          
SVCLRDT  DS    XL2                 CLEAR DATE SAVEARE                           
SVCLRSQ  DS    XL1                 CLEAR SEQ  SAVEAREA                          
SVCSHNM  DS    CL6                 CLIENT CHECK NUMBER                          
SVCSHDT  DS    XL2                 CLIENT CHECK DATE                            
SVPAYN   DS    F                   CURRENT PAID NET                             
*                                                                               
PRNT1ST  DC    C'Y'                FIRST TIME TO PRINTING SWITCH                
*                                                                               
QPERLEV  DS    XL1                                                              
*                                                                               
DOLLAR   DS    CL1                 NET/GROSS                                    
BILLOPT  DS    CL1                 Y/N/1/2                                      
DTOPT    DS    CL1                 INVOICE(DEFAULT)/DUE                         
INVDTOPT DS    CL1                 INVOICE DATE OPTION                          
DAYS     DS    CL1                 PRINT DAYS                                   
SPOTOPT  DS    CL1                 SHOW SPOTS                                   
ORDERED  DS    CL1                 ORDERED - GROSS/NET                          
BLNK     DS    CL1                 BLANK LINE                                   
CALC     DS    CL1                 CALC MONTH FROM WR PROFILE                   
PSUBT    DS    CL1                 PRINT SUBTOTALS                              
CASHOPT  DS    CL1                 Y/N                                          
VENDOPT  DS    CL1                 V/N - PRINT VENDOR INVOICE DATA              
CHKSOPT  DS    CL1                 Y/N                                          
XPCTOPT  DS    CL1                 Y/N - PRINT CLIENT PER CENT PAID             
BANKOPT  DS    CL1                 Y/N - PRINT BANK DEPOSIT DATE                
CHKIVOPT DS    CL1                 Y/N - CHECK INVOICE NUMBER                   
CHKRPOPT DS    CL1                 Y/N - CHECK REP                              
CBUOPT   DS    CL1                 Y/N - REPT CLRD BUT UNDISBURSED              
STANETOP DS    CL1                 Y/N - SUMMARISE AT LOCAL NET LEVEL           
STANETQ  EQU   C'Y'                   C'Y' - OPTION IS ON                       
NOVDRDTE DS    CL1                 Y/N - NO VENDOR INVOICE DATE                 
*                                                                               
INVLINE  DS    CL1                 PRINT INVOICE ON THIS LINE                   
SW13PRSW DS    C                   C'X' - BACK LINE OUT OF TOTALS               
LSQN     DS    XL2                 LINE SEQUENCE NUMBER ON OUTPUT               
LBACTDT  DS    CL6                 BILL ACTIVITY DATE FOR LSQN = 1              
*                                                                               
LINESTAT DS    XL1                 LINE STATUS - 'P' -PRINT LINE                
STATSW   DS    XL1                 C'Y' - SAVE LINE STATUS                      
SVSTAMOS DS    0XL9                STATION/MOS                                  
SVPRD    DS    XL1                 PRODUCT CODE                                 
SVEST    DS    XL1                 ESTIMATE NUMBER                              
SVMKTSTA DS    CL5                 MARKET/STATION                               
SVMOS    DS    XL2                 MONTH OF SERVICE                             
SVNETWK  DS    XL2                 NETWORK CODE                                 
*                                                                               
*                                                                               
OPSTAMOS DS    XL9                 STATION/MOS - OUTPUT                         
SVBLLAMT DS    PL8                 CURRENT SAVED BILLED DOLLARS                 
SVCLRAMT DS    PL8                 CURRENT SAVED CHECK  DOLLARS                 
SVBLLDLR DS    PL8                 CURRENT SAVED BILL   DOLLAR DAYS             
SVCHKDLR DS    PL8                 CURRENT SAVED CHECK  DOLLAR DAYS             
*                                                                               
INVNO    DS    H                   INVOICE NUMBER                               
BILLYR   DS    XL1                                                              
BILLMON  DS    XL1                                                              
BILLYM   DS    XL1                                                              
*                                                                               
CHKDATE  DS    XL2                 CHECK DATE                                   
BACTDATE DS    XL2                 BILLING ACTIVITY DATE                        
BINVDATE DS    XL2                 BILLING INVOICE  DATE                        
BRUNDATE DS    XL2                 BILLING RUN      DATE                        
BDUEDTE  DS    XL2                 BILLING DUE      DATE                        
INVNUM   DS    XL3                 INVOICE NUMBER                               
***                                                                             
CHKNUM   DS    CL7                 CHECK NUMBER                                 
CHKLINE  DS    CL1                 THIS LINE HAS A CHECK NUMBER                 
LDMON    DS    CL6                 CALCULATION DATE                             
LDMON1   DS    CL6                 LAST DAY OF BROADCAST MONTH                  
ACTDT    DS    CL6                 ACTIVITY DATE                                
BACTDT   DS    CL6                 BILLING  ACTIVITY DATE                       
CACTDT   DS    CL6                 CHECKING ACTIVITY DATE                       
CSHACTDT DS    CL6                 CASH CHECKING ACTIVITY DATE                  
CSHCHKNM DS    CL6                 CASH CHECK NUMBER                            
BNKDTOPT DS    CL1                 C'Y' USE BANK RECONCILIATION DATE            
VDRBNKDT DS    CL1                 C'Y' - USE VENDOR BANK DATE                  
         DS    0D                  ALIGNMENT                                    
CLRAMT   DC    PL8'0'              THIS ITEM'S CLEARANCE AMOUNT                 
CLRPCT   DC    PL8'0'              THIS ITEM'S PER CENT OF PD AMOUNT            
CLRTOT   DS    PL8                 CLEARANCE TOTAL                              
CLRTOTDB DS    PL8                 CLEARANCE TOTAL - DEBIT                      
CLRTOTCR DS    PL8                 CLEARANCE TOTAL - CREDIT                     
BLLAMTP  DS    PL8                 THIS ITEMS BILLED    AMOUNT                  
BLLTOT   DS    PL8                 BILLED    TOTAL                              
BLLTOTDB DS    PL8                 BILLED    TOTAL - DEBIT                      
BLLTOTCR DS    PL8                 BILLED    TOTAL - CREDIT                     
CSHPCT   DS    F                   PER CENT CASH APPLIED TO BILL                
CSHAMTP  DS    PL8                 THIS ITEMS CASH APPLIED                      
CSHTOT   DS    PL8                 CASH APPLIED TOTAL                           
CSHTOTDB DS    PL8                 CASH APPLIED TOTAL - DEBIT                   
CSHTOTCR DS    PL8                 CASH APPLIED TOTAL - CREDIT                  
DOLTOT   DS    PL8                 DOLLAR DAYS TOTAL                            
TBDOLDB  DS    PL8                 BILLING  DOLLAR DAYS TOTAL - DEBIT           
TBDOLCR  DS    PL8                 BILLING  DOLLAR DAYS TOTAL - CREDIT          
TCDOLDB  DS    PL8                 CHECKING DOLLAR DAYS TOTAL - DEBIT           
TCDOLCR  DS    PL8                 CHECKING DOLLAR DAYS TOTAL - CREDIT          
DAYSTOT  DS    F                   DAYS TOTAL                                   
BDAYSTOT DS    F                   BILLING/CASH DAYS TOTAL                      
CDAYSTOT DS    F                   CHECKING     DAYS TOTAL                      
*                                                                               
WRPROF   DS    CL16                WR - PROFILE                                 
*                                                                               
BDREC    DS    0CL17               BILL - ACTIVITY DATE TABLE RECORD            
BDKEY    DS    0CL11                                                            
BDCLT    DS    XL2                                                              
BDPRD    DS    CL3                                                              
BDEST    DS    XL1                                                              
BDYMSER  DS    XL2                                                              
BDBILYM  DS    XL1                                                              
BDINUM   DS    XL2                                                              
BDDATA   DS    0CL6                                                             
BDDATE   DS    CL6                                                              
*                                                                               
CHKDTOPT DS    CL1                 Y/N - CHECK DATE                             
*                                                                               
       ++INCLUDE DDEBLOCK                                                       
         DS    0D                                                               
SVLSQN   DS    XL1                 LINE SEQUENCE NUMBER SAVEAREA                
SVLSQN2  DS    XL1                 LINE SEQ # - 2       SAVEAREA                
SVBHDELA DS    A                   A(CASHIER BUFFER ENTRY)                      
SVMBTELA DS    A                   A(MEDIA TRANSFER    ELEMENT)                 
SVMBTNXA DS    A                   A(NEXT MEDIA TRANSFER    ELEMENT)            
SVTRNELA DS    A                   A(CASH  TRANSACTION ELEMENT)                 
SVTRNNXA DS    A                   A(NEXT CASH  TRANSACTION ELEMENT)            
TOTDAYA  DS    A                   A(DAYS TOTAL PRINT AREA)                     
TRNELM   DS    XL256               DUMMY TRANSACTION ELEMENT                    
*                                                                               
*        BUFFERS TO HANDLE TOTALING ON OUTPUT - MAX 20 LEVELS OF TOTALS         
*                                                                               
TOTBKS   DS    0PL8                TOTALS BUCKETS                               
TBKBLLDB DS    20PL8               BILL         TOTAL     BUCKETS-DEBIT         
TOTNMLVQ EQU   (*-TOTBKS)/L'TOTBKS NUMBER OF LEVEL BUCKETS                      
TBKBLLCR DS    20PL8               BILL         TOTAL     BUCKETS-CREDT         
TBKCLRDB DS    20PL8               CLEARANCE    TOTAL     BUCKETS-DEBIT         
TBKCLRCR DS    20PL8               CLEARANCE    TOTAL     BUCKETS-CREDT         
TBKCSHDB DS    20PL8               CASH         TOTAL     BUCKETS-DEBIT         
TBKCSHCR DS    20PL8               CASH         TOTAL     BUCKETS-CREDT         
TBKBLDDB DS    20PL8               DISBURSEMENT BILL      BUCKETS-DEBIT         
TBKBLDCR DS    20PL8               DISBURSEMENT BILL      BUCKETS-CREDT         
TBKCLDDB DS    20PL8               DISBURSEMENT CLEARANCE BUCKETS-DEBIT         
TBKCLDCR DS    20PL8               DISBURSEMENT CLEARANCE BUCKETS-CREDT         
TBKPDNDB DS    20PL8               PAID NET     TOTAL     BUCKETS-DEBIT         
TBKPDNCR DS    20PL8               PAID NET     TOTAL     BUCKETS-CREDT         
TOTNMBKQ EQU   (*-TOTBKS)/L'TOTBKS TOTAL NUMBER OF BUCKETS                      
*                                                                               
SVCHKTAB DS    0D                  CHECK NUMBER TABLE                           
SVTMOS   DS    XL2                 MONTH OF SERVICE                             
SVTPRD   DS    XL1                 PRODUCT                                      
SVTEST   DS    XL1                 ESTIMATE                                     
SVTCLRDT DS    XL2                 CLEARANCE DATE                               
SVTCLRSQ DS    XL1                 CLEARANCE SEQUENCE #                         
SVTINV   DS    XL(L'CLS3INV)       INVOICE NUMBER                               
SVTCHK   DS    XL(L'SCCHKNUM)      CHECK NUMBER                                 
SVTLSQN  DS    XL(L'SVLSQN)        LINE SEQUENCE NUMBER                         
SVTLSQN2 DS    XL(L'SVLSQN2)       LINE SEQ # - 2                               
SVTENTL  EQU   *-SVCHKTAB          LENGTH OF TABLE ENTRY                        
         DS    XL(400*SVTENTL)      REST OF TABLE                               
*                                                                               
         DS    0D                                                               
PRDLST   DS    XL256                                                            
*                                                                               
WORKL    EQU   *-WORKD                                                          
*                                                                               
         TITLE 'SPWRI13 - CASHFLOW - ICSHIVTD'                                  
***********************************************************************         
*                                                                     *         
*        VENDOR INVOICE SEQUENCE NUMBER TABLE ENTRY                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ICSHIVTD DSECT                                                                  
IVTSLN   DS    XL(L'SNVIDSLN)      SPOT LENGTH                                  
IVTPRD   DS    XL(L'SNVIDPRD)      PRODUCT CODE                                 
IVTPR2   DS    XL(L'SNVIDPR2)      SECOND PRODUCT CODE                          
IVTEST   DS    XL(L'SNVIDEST)      ESTIMATE CODE                                
IVTNETWK DS    XL(L'SVNETWK)       NETWORK CODE                                 
IVTMOS   DS    XL(L'SNVKMOS)       MONTH OF SERVICE COMPRESSED                  
IVTINVNO DS    XL(L'SNVKINV)       INVOICE NUMBER                               
IVTSQN   DS    XL1                 SEQUENCE NUMBER                              
ICSHIVLQ EQU   *-ICSHIVTD          ENTRY LENGTH                                 
*                                                                               
         TITLE 'SPWRI13 - CASHFLOW - ICSHSTTD'                                  
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF ADDRESSES IN WORKAREA OVERLAY T2041A      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CSHTBLD  DSECT                                                                  
         DC    CL8'*CASHTBLS'      EYECATCHER                                   
*                                                                               
*        DISPLACEMENTS TO WORKAREAS                                             
*                                                                               
CSHTBLAS DS    0A                  DISPLACEMENTS TABLE TO WORKAREAS             
ASTBTBL  DS    A                   STATION BILLING TABLE                        
AINVTBL  DS    A                   INVOICE         TABLE                        
ACSHREC  DS    A                   CASHIER RECORD SAVEAREA                      
ACLTREC  DS    A                   CLIENT RECORD  SAVEAREA                      
         DS    16A                 SPARE                                        
*                                                                               
         TITLE 'SPWRI13 - CASHFLOW - ICSHSTTD'                                  
***********************************************************************         
*                                                                     *         
*        STATION BILL SEQUENCE NUMBER TABLE ENTRY                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ICSHSTTD DSECT                                                                  
STTNETWK DS    XL(L'SVNETWK)       NETWORK CODE                                 
         DS    XL1                 SPARE                                        
STTPER   DS    XL(L'STABPER)       BILLING PERIOD                               
STTBDT   DS    XL(L'STABBDT)       INVOICE BILLING DATE (COMPRESSED)            
STTINV   DS    XL(L'STABINV)       INVOICE NUMBER                               
STTCSHNM DS    XL6                 CASH CHECK NUMBER                            
STTCSHDT DS    XL2                 CASH CHECK DATE                              
STTSQN   DS    XL1                 SEQUENCE NUMBER                              
ICSHSTLQ EQU   *-ICSHSTTD          ENTRY LENGTH                                 
*                                                                               
         EJECT                                                                  
HEADTABD DSECT                                                                  
HDDPT    DS    X                   DAYPART OPTION                               
HDREP    DS    X                   REPORT NUMBER                                
HDNMGR   DS    X                   N'MARKET GROUPS                              
HDMGR1   DS    X                   HEADLINE FOR MARKET GROUP 1                  
HDMGR2   DS    X                   HEADLINE FOR MARKET GROUP 2                  
HDMGR3   DS    X                   HEADLINE FOR MARKET GROUP 3                  
HDMKTDPT DS    X                   HEADLINE FOR MARKET OR DAYPART               
HDSUM    DS    X                   HEADLINE FOR MKT OR DPT SUMMARY              
HEADTABL EQU   *-HEADTABD                                                       
         EJECT 1                                                                
       ++INCLUDE DDCASHIERD                                                     
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*SPOTTABD                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*DEDBLOCK                                                                       
*SPGENAGY                                                                       
*SPGENCLRST                                                                     
*DDCOMFACS                                                                      
*DDMASTD                                                                        
*SPWRIFFD                                                                       
*DDCOREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE SPOTTABD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
       ++INCLUDE DDCOREQUS                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENSTAB                                                      
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
                                                                                
       ++INCLUDE SPGENSNV                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENCLRST                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRID3D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 2                                                                
* DDTSARD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'131SPWRI13   10/10/16'                                      
         END                                                                    
