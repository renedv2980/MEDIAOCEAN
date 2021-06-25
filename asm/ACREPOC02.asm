*          DATA SET ACREPOC02  AT LEVEL 003 AS OF 01/17/13                      
*PHASE ACOC02A                                                                  
*INCLUDE BUFFERIN                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
         TITLE 'OFFICE-CONTRA BUCKET REPORT'                                    
ACOC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACOC**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACOCD,RC                                                         
         MVI   FCRESET,C'Y'                                                     
         L     R7,VBIGPRNT                                                      
         USING BIGPRNTD,R7                                                      
         EJECT                                                                  
***********************************************************************         
* FIRST FOR RUN                                                       *         
***********************************************************************         
         CLI   MODE,RUNFRST                                                     
         BNE   REQF00                                                           
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
         L     RF,GETOPT                                                        
         MVC   0(2,RF),=X'07FE'                                                 
         MVI   FCPRORAT,C'N'                                                    
         MVI   FCSUPOFC,C'Y'                                                    
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
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST FOR REQUEST                                                   *         
***********************************************************************         
REQF00   CLI   MODE,REQFRST                                                     
         BNE   LDGF00                                                           
         L     R1,ADBOX                                                         
         USING BOXD,R1                                                          
         MVC   BOXWIDTH,=F'198'                                                 
         DROP  R1                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   OPT,0                                                            
         MVI   FLG,0                                                            
*                                                                               
         CLI   QOPT1,C'Y'          ONLY ACCOUNTS WITH DIFFERENCES               
         BNE   *+8                                                              
         OI    OPT,OPTDIF                                                       
*                                                                               
         CLI   QOPT2,C'Y'          ONLY OFFICE SUMMARY                          
         BNE   *+8                                                              
         OI    OPT,OPTOFS                                                       
*                                                                               
         CLI   QOPT3,C'Y'          INCLUDE ALL CONTRAS                          
         BNE   *+8                                                              
         OI    OPT,OPTCON                                                       
*                                                                               
         CLI   QOPT3,C'S'          SUPPRESS ALL CONTRAS                         
         BNE   *+8                                                              
         OI    OPT,OPTNOC                                                       
*                                                                               
         CLI   QOPT4,C'Y'          FIX RECORDS                                  
         BNE   *+12                                                             
         OI    OPT,OPTFIX                                                       
         NI    OPT,ALL-OPTOFS      CAN'T HAVE 'ONLY OFFICE SUMMARY'             
*                                                                               
         CLI   QOPT5,C'Y'          DUMP RECORDS                                 
         BNE   *+8                                                              
         OI    OPT,OPTDMP                                                       
*                                                                               
         MVI   FCRDACC,C'Y'                                                     
         L     R4,ADCMPEL                                                       
         USING CPYELD,R4                                                        
         TM    CPYSTAT4,CPYSOFF2   TEST 2 CHARACTER OFFICE                      
         BNO   *+8                                                              
         OI    FLG,F2OFF                                                        
*                                                                               
         CLI   QOPT6,C' '                                                       
         BE    XIT                 1 AND 2 CHARACTER OFFICE                     
         CLI   QOPT6,C'1'          ONLY 1 CHARACTER                             
         BNE   *+16                                                             
         TM    FLG,F2OFF                                                        
         BO    REQF03              COMPANY IS 2, SKIP IT                        
         B     XIT                 COMPANY IS 1, OK                             
*                                                                               
         CLI   QOPT6,C'2'          ONLY 2 CHARACTER                             
         BNE   REQF03                                                           
         TM    FLG,F2OFF                                                        
         BO    XIT                 COMPANY IS 2, OK                             
                                                                                
*                                                                               
REQF03   MVI   FCRDACC,C'N'        DON'T PROCESS                                
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVI   ACMMODE,REQFRST     SET FOR NEXT COMPANY                         
         B     XIT                                                              
         DROP  R4,RF                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LEDGER                                                    *         
***********************************************************************         
LDGF00   CLI   MODE,LEDGFRST                                                    
         BNE   PRAC00                                                           
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCACC                                                             *         
***********************************************************************         
PRAC00   CLI   MODE,PROCACC                                                     
         BNE   RUNL00                                                           
         MVI   FORCEHED,C'Y'                                                    
         NI    FLG,ALL-(FPROD)                                                  
         L     R2,ADLEDGER                                                      
         CLC   1(2,R2),=C'SJ'                                                   
         BNE   *+8                                                              
         OI    FLG,FPROD                                                        
         GOTO1 BUFFERIN,DMCB,('BUFFAINI',ABUFF),(0),ADCOMFAC                    
         ZAP   ACCBAL,=P'0'                                                     
         ZAP   OFFACC,=P'0'                                                     
         ZAP   CONBAL,=P'0'                                                     
         ZAP   OFFCON,=P'0'                                                     
         XC    OFSNUM,OFSNUM                                                    
         XC    CASNUM,CASNUM                                                    
         XC    CMSNUM,CMSNUM                                                    
         MVI   OLDMOS,X'FF'                                                     
         XC    OLDCON,OLDCON                                                    
         MVC   BIGOFF,SPACES       OFFICE WITH BIGGEST BALANCE                  
         ZAP   BIGBAL,=P'0'                                                     
*                                                                               
         MVC   DKEY,SPACES                                                      
         L     RF,ADACC                                                         
         LA    R2,DKEY                                                          
         USING ACTRECD,R2                                                       
         MVC   ACTKCULA,0(RF)      SET FOR READ HIGH OF ACCOUNT                 
         BAS   RE,DMHGH                                                         
*                                                                               
PRAC01   CLC   DIR(L'ACTKCULA),DKEY     TEST SAME ACCOUNT                       
         BNE   PRAC11                                                           
         L     R2,AIO2                                                          
         ST    R2,AIO                                                           
         BAS   RE,DMGETR                                                        
         SR    R1,R1                                                            
         LA    R4,ACTRFST                                                       
PRAC03   CLI   0(R4),0                                                          
         BE    PRAC09                                                           
         CLI   0(R4),ABLELQ        TEST BALANCE ELEMENT                         
         BNE   *+12                                                             
         BAS   RE,PBBF             POST BALANCE FORWARD                         
         B     PRAC09                                                           
         CLI   0(R4),BUKELQ        TEST BUCKET ELEMENT                          
         BNE   *+12                                                             
         BAS   RE,PBUK             POST BUCKET                                  
         B     PRAC09                                                           
         CLI   0(R4),PBKELQ        TEST PRIOR BUCKET ELEMENT                    
         BNE   *+12                                                             
         BAS   RE,PBUK             POST BUCKET                                  
         B     PRAC09                                                           
         CLI   0(R4),TRNELQ        TEST TRANSACTION ELEMENT                     
         BE    PRAC10                                                           
         CLI   0(R4),TIMELQ        TEST TIME ELEMENT                            
         BE    PRAC10                                                           
*                                                                               
PRAC07   SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     PRAC03                                                           
*                                                                               
PRAC09   BAS   RE,DMSEQ                                                         
         B     PRAC01                                                           
*                                                                               
PRAC10   XC    DKEY,DKEY                                                        
         MVC   DKEY(TRNKDATE-TRNKEY),DIR                                        
         LA    R2,DKEY                                                          
         USING TRNRECD,R2                                                       
         SR    R0,R0                                                            
         IC    R0,TRNKCULC+L'TRNKCULC-1                                         
         AHI   R0,1                                                             
         STC   R0,TRNKCULC+L'TRNKCULC-1                                         
         BAS   RE,DMHGH            GET NEXT CONTRA THIS ACCOUNT                 
         B     PRAC01                                                           
*                                                                               
PRAC11   NI    OPT,X'FF'-(OPTADIF)                                              
         ZAP   CONOUT,ACCBAL       GET CONTRA ACCOUNT DIFFERENCE                
         SP    CONOUT,CONBAL                                                    
         BZ    *+8                                                              
         OI    OPT,OPTADIF         SET 'ACCOUNT HAS A DIFFERENCE'               
         TM    FLG,F2OFF                                                        
         BZ    PRAC13                                                           
         TM    FLG,FPROD                                                        
         BO    PRAC13                                                           
         ZAP   OFAOUT,ACCBAL       GET OFFICE ACCOUNT DIFFERENCE                
         SP    OFAOUT,OFFACC                                                    
         BZ    *+8                                                              
         OI    OPT,OPTADIF                                                      
         ZAP   OFCOUT,ACCBAL       GET OFF/CONTRA DIFFERENCE                    
         SP    OFCOUT,OFFCON                                                    
         BZ    *+8                                                              
         OI    OPT,OPTADIF                                                      
*                                                                               
PRAC13   TM    OPT,OPTDIF          ONLY WANT DIFFERENCES ?                      
         BNO   *+12                                                             
         TM    OPT,OPTADIF         DOES ACCOUNT HAVE A DIFFERENCE ?             
         BNO   XIT                 NO, SKIP IT                                  
*                                                                               
         TM    OPT,OPTADIF         DOES ACCOUNT HAVE A DIFFERENCE ?             
         BNO   *+10                                                             
         AP    CNTACTS,=P'1'       COUNT ACCOUNTS WITH ERRORS                   
         BAS   RE,OFFS             PRINT OFFICE SUMMARY                         
         TM    OPT,OPTOFS          OFFICE SUMMARY ONLY ?                        
         BO    PRAC17                                                           
         BAS   RE,CONS             PRINT CONTRA SUMMARY                         
         BAS   RE,MONS             PRINT MONTH SUMMARY                          
*                                                                               
PRAC17   TM    OPT,OPTFIX+OPTADIF  FIX THE FILE                                 
         BNO   XIT                                                              
         TM    FLG,FPROD           CAN'T FIX PROD                               
         BO    XIT                                                              
         BAS   RE,FIX                                                           
         B     XIT                                                              
*                                                                               
PRAC19   MVI   FCRESET,C'N'        FORCE TO END OF SJ                           
         L     R2,ADACC                                                         
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(3),0(R2)                                                    
         MVI   DKEY+3,X'FF'                                                     
         BAS   RE,DMHGH                                                         
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
RUNL00   CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         L     RF,ADBOX                                                         
         MVI   BOXREQ-BOXD(RF),C' '                                             
         MVI   BOXYORN-BOXD(RF),C'N'                                            
         GOTO1 ACREPORT                                                         
*                                                                               
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,CNT                                                           
*                                                                               
RUNL03   MVC   XP+1(30),4(R3)       EDIT THE RECORDS COUNTS                     
         EDIT  (P4,0(R3)),(7,XP+36),ZERO=NOBLANK                                
         GOTO1 ACREPORT                                                         
         LA    R3,L'CNT(R3)                                                     
         CLI   0(R3),X'FF'                                                      
         BNE   RUNL03                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* POST BALANCE BROUGHT FORWARD                                        *         
***********************************************************************         
         USING OFARECD,R2                                                       
         USING ABLELD,R4                                                        
PBBF     NTR1  ,                                                                
         LA    R5,BUFREC                                                        
         USING BUFD,R5                                                          
         MVC   BUFKEY,SPACES                                                    
         MVI   BUFTYP,BUFTOFF      OFFICE SUMMARY                               
         MVC   BUFOFF,OFAKOFF      OFFICE CODE                                  
*                                                                               
         LA    RF,BUFNUM                                                        
         LA    R1,BUFBBF                                                        
         ZAP   0(L'BUFBBF,R1),=P'0'                                             
         LA    R1,L'BUFBBF(R1)                                                  
         BCT   RF,*-10                                                          
*                                                                               
         ZAP   BUFBBF,ABLFRWD      POST BALANCE FORWARD                         
         ZAP   BUFBDR,ABLDR        DEBITS                                       
         ZAP   BUFBCR,ABLCR        AND CREDITS                                  
         GOTO1 BUFFERIN,DMCB,('BUFFAPUT',ABUFF),BUFKEY,ADCOMFAC                 
*                                                                               
         LA    RE,ACCBAL                                                        
         CLC   OFAKOFF,SPACES      TEST ACCOUNT BALANCE                         
         BE    PBBF3                                                            
         MVC   BUFOFF,TOTLQ        SET KEY TO GET TOTAL OF 'ALL'                
         BASR  RE,RF               POST TOTAL RECORD                            
         LA    RE,OFFACC                                                        
*                                                                               
PBBF3    AP    0(L'ACCBAL,RE),ABLFRWD   GET ACCOUNT BALANCE                     
         AP    0(L'ACCBAL,RE),ABLDR                                             
         SP    0(L'ACCBAL,RE),ABLCR                                             
         CLC   OFAKOFF,SPACES      TEST ACCOUNT BALANCE                         
         BE    XIT                                                              
*                                                                               
         ZAP   DUB,ABLFRWD                                                      
         CLC   DUB(7),BIGBAL                                                    
         BL    XIT                                                              
         ZAP   BIGBAL,DUB          GET OFFICE WITH BIGGEST BALFRWD              
         MVC   BIGOFF,OFAKOFF                                                   
         B     XIT                                                              
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* POST BUCKET ELEMENTS                                                *         
***********************************************************************         
         USING CACRECD,R2                                                       
PBUK     NTR1  ,                                                                
         CLC   CACKSPAC(10),SPACES IGNORE SPECIAL BUCKETS                       
         BNE   XIT                                                              
PBUK1    LA    R5,BUFREC                                                        
         USING BUFD,R5                                                          
         LA    RF,BUFNUM                                                        
         LA    R1,BUFBBF                                                        
         ZAP   0(L'BUFBBF,R1),=P'0'                                             
         LA    R1,L'BUFBBF(R1)                                                  
         BCT   RF,*-10                                                          
*                                                                               
         USING PBKELD,R4                                                        
         CLI   0(R4),PBKELQ        TEST PRIOR BUCKET                            
         BNE   PBUK3                                                            
         ZAP   DR,PBKDR            DEBITS                                       
         ZAP   CR,PBKCR            CREDITS                                      
         TM    FLG,FPROD                                                        
         BO    PBUK5                                                            
         OC    PBKLOW,PBKLOW       FIX BAD START DATES                          
         BNZ   PBUK2                                                            
         MVC   PBKLOW,=X'8001'                                                  
         CLC   PBKLOW,PBKHI                                                     
         BNH   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DMPUTR                                                        
         BAS   RE,DMPPUT                                                        
PBUK2    MVC   MOS,PBKLOW          MONTH LOW & HIGH                             
         CLC   CACKOFF,SPACES      TEST NON-OFFICE BUCKET                       
         BNE   PBUK5                                                            
         CLC   PBKLOW,OLDMOS                                                    
         BNL   PBUK5                                                            
         CLC   CACKCACT(7),=C'***VOID'   CAN'T USE THIS CONTRA                  
         BE    PBUK5                                                            
         MVC   OLDMOS,PBKLOW      GET OLDEST MONTH                              
         MVC   OLDCON,CACKCULC     AND CONTRA                                   
         B     PBUK5                                                            
*                                                                               
         USING BUKELD,R4                                                        
PBUK3    CLI   0(R4),BUKELQ                                                     
         BNE   PBUK15                                                           
         ZAP   DR,BUKDR            DEBITS                                       
         ZAP   CR,BUKCR            CREDITS                                      
         TM    FLG,FPROD                                                        
         BO    PBUK5                                                            
         XC    MOS,MOS                                                          
         MVC   MOSS,BUKMOS         MONTH                                        
         CLC   CACKOFF,SPACES      TEST NON-OFFICE BUCKET                       
         BNE   PBUK5                                                            
         CLC   BUKMOS,OLDMOS                                                    
         BNL   PBUK5                                                            
         CLC   CACKCACT(7),=C'***VOID'   CAN'T USE THIS CONTRA                  
         BE    PBUK5                                                            
         MVC   OLDMOS,BUKMOS       GET OLDEST MONTH                             
         MVC   OLDCON,CACKCULC     AND CONTRA                                   
*                                                                               
PBUK5    MVC   BUFKEY,SPACES                                                    
         MVI   BUFTYP,BUFTOFF      OFFICE SUMMARY                               
         TM    FLG,FPROD                                                        
         BO    *+10                                                             
         MVC   BUFOFF,CACKOFF      OFFICE CODE                                  
         ZAP   BUFHDR,DR                                                        
         ZAP   BUFHCR,CR                                                        
         GOTO1 BUFFERIN,DMCB,('BUFFAPUT',ABUFF),BUFKEY,ADCOMFAC                 
         TM    FLG,FPROD                                                        
         BO    PBUK7                                                            
         CLC   CACKOFF,SPACES       TEST 'NON-OFFICE' BUCKET                    
         BE    PBUK7                YES, DON'T ADD TO ALL                       
         MVC   BUFOFF,TOTLQ         TOTAL 'ALL' OFFICES                         
         BASR  RE,RF                                                            
*                                                                               
PBUK7    MVI   BUFTYP,BUFTCON       CONTRA SUMMARY                              
         MVC   BUFOFF,SPACES        TOTAL RECORD                                
         MVC   BUFCON,CACKCULC      CONTRA ACCOUNT                              
         ZAP   BUFHDR,=P'0'                                                     
         ZAP   BUFHCR,=P'0'                                                     
         LA    RE,BUFNDR                                                        
         TM    FLG,FPROD                                                        
         BO    *+18                                                             
         CLC   CACKOFF,SPACES       TEST NON-OFFICE  BUCKET                     
         BE    *+8                                                              
         LA    RE,BUFHDR                                                        
         ZAP   0(L'BUFNDR,RE),DR                                                
         ZAP   L'BUFNDR(L'BUFNCR,RE),CR                                         
         BASR  RE,RF                  CONTRA TOTAL RECORD                       
*                                                                               
         MVI   BUFTYP,BUFTMON                                                   
         XC    BUFMOS,BUFMOS                                                    
         XC    BUFEND,BUFEND        CHECK FOR RANGE                             
         MVC   BUFRECX(L'BUFKEY),BUFREC                                         
         GOTO1 BUFFERIN,DMCB,('BUFFARDH',ABUFF),BUFRECX,ADCOMFAC                
         CLI   4(R1),0                                                          
         BNE   PBUK9               NONE, USE MOS                                
         LA    R6,BUFRECX                                                       
X        USING BUFD,R6                                                          
         CLC   BUFKEY(BUFMOS-BUFD),X.BUFKEY                                     
         BNE   PBUK9                                                            
         OC    X.BUFEND,X.BUFEND   TEST END DATE                                
         BZ    PBUK9                                                            
         CLC   MOSS,X.BUFMOS                                                    
         BL    PBUK9                                                            
         LA    R1,MOSE                                                          
         OC    MOSE,MOSE           ANY END ?                                    
         BNZ   *+8                                                              
         LA    R1,MOSS                                                          
         CLC   0(L'MOSS,R1),X.BUFMOS+L'MOSS                                     
         BH    PBUK9                                                            
         MVC   BUFMOS(L'MOS),X.BUFMOS  USE RANGE                                
         B     PBUK11                                                           
         DROP  X                                                                
*                                                                               
PBUK9    MVC   BUFMOS(L'MOS),MOS                                                
PBUK11   GOTO1 BUFFERIN,DMCB,('BUFFAPUT',ABUFF),BUFKEY,ADCOMFAC                 
*                                                                               
         TM    FLG,FPROD                                                        
         BO    PBUK13                                                           
         CLC   CACKOFF,SPACES                                                   
         BE    PBUK13                                                           
         MVI   BUFTYP,BUFTCON                                                   
         MVC   BUFOFF,CACKOFF        CONTRA BY OFFICE                           
         MVC   BUFEND,SPACES                                                    
         ZAP   BUFNDR,=P'0'                                                     
         ZAP   BUFNCR,=P'0'                                                     
         ZAP   BUFHDR,DR                                                        
         ZAP   BUFHCR,CR                                                        
         BASR  RE,RF                 CONTRA TOTAL RECORD                        
*                                                                               
PBUK13   LA    RE,CONBAL                                                        
         TM    FLG,FPROD                                                        
         BO    *+18                                                             
         CLC   CACKOFF,SPACES        TEST NON-OFFICE BUCKET                     
         BE    *+8                                                              
         LA    RE,OFFCON                                                        
         AP    0(L'CONBAL,RE),DR     GET BUCKET BALANCES                        
         SP    0(L'CONBAL,RE),CR                                                
*                                                                               
PBUK15   SR    R1,R1                                                            
         IC    R1,1(R4)              GET NEXT BUCKET                            
         AR    R4,R1                                                            
         CLI   0(R4),0                                                          
         BNE   PBUK1                                                            
         B     XIT                                                              
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* PRINT THE OFFICE SUMMARY                                            *         
***********************************************************************         
OFFS     NTR1  ,                                                                
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,XP                                                            
         USING PLD,R3                                                           
         LA    R5,BUFREC                                                        
         USING BUFD,R5                                                          
*                                                                               
         XC    BUFKEY,BUFKEY                                                    
         GOTO1 BUFFERIN,DMCB,('BUFFARDH',ABUFF),BUFKEY,ADCOMFAC                 
*                                                                               
OFFS3    TM    4(R1),X'80'                                                      
         BO    OFFS7                                                            
         CLI   BUFTYP,BUFTOFF      OFFICE SUMMARY RECORD                        
         BNE   OFFS7                                                            
         CURED BUFBBF,(L'PLOSFWD,PLOSFWD),2,CR=YES                              
         CURED BUFBDR,(L'PLOSTDR,PLOSTDR),2,CR=YES                              
         CURED BUFBCR,(L'PLOSTCR,PLOSTCR),2,CR=YES                              
         ZAP   DUB,BUFBBF                                                       
         AP    DUB,BUFBDR                                                       
         SP    DUB,BUFBCR                                                       
         ZAP   OUT,DUB                                                          
         CURED DUB,(L'PLOSTBL,PLOSTBL),2,CR=YES                                 
*                                                                               
         CURED BUFHDR,(L'PLOSBDR,PLOSBDR),2,CR=YES                              
         CURED BUFHCR,(L'PLOSBCR,PLOSBCR),2,CR=YES                              
         ZAP   DUB,BUFHDR                                                       
         SP    DUB,BUFHCR                                                       
         SP    OUT,DUB                                                          
         CURED DUB,(L'PLOSBBL,PLOSBBL),2,CR=YES                                 
         MVC   PLOSOFF+2(2),BUFOFF OFFICE                                       
         CLC   BUFOFF,TOTLQ        TEST NON-OFFICE RECORD                       
         BNE   *+10                                                             
         MVC   PLOSOFF(5),=C'*ALL*'                                             
*                                                                               
         CP    OUT,=P'0'            TEST ANY DIFFERENCE                         
         BE    OFFS5                NO, IT'S OK                                 
         CURED OUT,(L'PLOSOUT,PLOSOUT),2,CR=YES                                 
         CLC   BUFOFF,TOTLQ        TEST NON-OFFICE RECORD                       
         BNH   OFFS5               YES, DON'T ADD TO SUMMARY                    
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,OFSNUM                                                      
         CHI   R1,MXOFD                                                         
         BL    *+6                                                              
         DC    H'0'                 OFFICE SUMMARY TABLE IS FULL                
         LA    RF,1(R1)                                                         
         STCM  RF,3,OFSNUM          ADD TO OFFICE DIFFERENCE SUMMARY            
         MHI   R1,OFSLNQ                                                        
         A     R1,AOFSUM                                                        
         USING OFSUMD,R1                                                        
         MVC   OFSOFF,BUFOFF                                                    
         ZAP   OFSAMT,OUT                                                       
         DROP  R1                                                               
*                                                                               
OFFS5    GOTO1 ACREPORT                                                         
         GOTO1 BUFFERIN,DMCB,('BUFFASEQ',ABUFF),BUFKEY,ADCOMFAC                 
         B     OFFS3                                                            
*                                                                               
OFFS7    L     RF,ADBOX                                                         
         USING BOXD,RF                                                          
         MVC   BOXROWS,SPACES      CLOSE THE BOX                                
         SR    RE,RE                                                            
         IC    RE,LINE                                                          
         LA    RE,BOXROWS-1(RE)                                                 
         MVI   0(RE),C'M'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R3,R5,RF                                                         
         EJECT                                                                  
***********************************************************************         
* PRINT THE CONTRA SUMMARY                                            *         
***********************************************************************         
CONS     NTR1  ,                                                                
         MVI   RCSUBPRG,2                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   LASTCON,SPACES                                                   
         LA    R3,XP                                                            
         USING PLD,R3                                                           
         LA    R5,BUFREC                                                        
         USING BUFD,R5                                                          
*                                                                               
CONS3    BAS   RE,GETN             GET NEXT CONTRA WITH ERROR                   
         BNE   XIT                 TEST EOF                                     
         TM    OPT,OPTCON          INCLUDE ALL CONTRAS ?                        
         BO    *+14                                                             
         CP    OUT,=P'0'           TEST OUT OF BALANCE                          
         BE    CONS3               NO, GET NEXT                                 
         XC    BUFKEY,BUFKEY                                                    
         MVI   BUFTYP,BUFTCON                                                   
         MVC   BUFCON,LASTCON                                                   
         GOTO1 BUFFERIN,DMCB,('BUFFARDH',ABUFF),BUFKEY,ADCOMFAC                 
*                                                                               
CONS5    TM    4(R1),X'80'                                                      
         BO    CONS11                                                           
         CLI   BUFTYP,BUFTCON                                                   
         BNE   CONS11                                                           
         CLC   BUFCON,LASTCON                                                   
         BNE   CONS11                                                           
         CLC   BUFOFF,SPACES       TEST 'TOTAL RECORD'                          
         BNE   CONS7                                                            
*                                                                               
         TM    OPT,OPTNOC          SUPPRESS CONTRA PRINTING ?                   
         BO    CONS6                                                            
         MVC   PLCSCON,BUFCON+1    CONTRA                                       
         CURED BUFNDR,(L'PLOSBDR,PLOSBDR),2,CR=YES                              
         CURED BUFNCR,(L'PLOSBCR,PLOSBCR),2,CR=YES                              
         ZAP   DUB,BUFNDR                                                       
         SP    DUB,BUFNCR                                                       
         CURED DUB,(L'PLOSBBL,PLOSBBL),2,CR=YES                                 
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   PLCSCON,BUFCON+1    CONTRA                                       
         MVC   PLCSOFF(5),=C'*ALL*'                                             
         CURED BUFHDR,(L'PLOSBDR,PLOSBDR),2,CR=YES                              
         CURED BUFHCR,(L'PLOSBCR,PLOSBCR),2,CR=YES                              
         ZAP   DUB,BUFHDR                                                       
         SP    DUB,BUFHCR                                                       
         CURED DUB,(L'PLOSBBL,PLOSBBL),2,CR=YES                                 
         CURED OUT,(L'PLOSOUT,PLOSOUT),2,CR=YES                                 
         GOTO1 ACREPORT                                                         
*                                                                               
CONS6    XC    ACASNTRY,ACASNTRY                                                
         CP    OUT,=P'0'                                                        
         BE    CONS9                                                            
         SR    R1,R1                                                            
         ICM   R1,3,CASNUM                                                      
         CHI   R1,MXCAD                                                         
         BL    *+6                                                              
         DC    H'0'                 CONTRA SUMMARY TABLE IS FULL                
         LA    RF,1(R1)                                                         
         STCM  RF,3,CASNUM          ADD TO CONTRA DIFFERENCE SUMMARY            
         MHI   R1,CASLNQ                                                        
         A     R1,ACASUM                                                        
         USING CASUMD,R1                                                        
         MVC   CASCAC,BUFCON        ADD NEW ITEM                                
         ZAP   CASAMT,OUT                                                       
         ZAP   CASTAMT,=P'0'                                                    
         XC    CASOFF,CASOFF                                                    
         ST    R1,ACASNTRY          SAVE THE ADDRESS OF THIS ENTRY              
         B     CONS9                                                            
         DROP  R1                                                               
*                                                                               
CONS7    TM    OPT,OPTNOC          SUPPRESS PRINTING ?                          
         BO    CONS9                                                            
         MVC   PLCSCON,BUFCON+1    CONTRA                                       
         MVC   PLCSOFF+2(2),BUFOFF OFFICE                                       
         CURED BUFHDR,(L'PLOSBDR,PLOSBDR),2,CR=YES                              
         CURED BUFHCR,(L'PLOSBCR,PLOSBCR),2,CR=YES                              
         ZAP   DUB,BUFHDR                                                       
         SP    DUB,BUFHCR                                                       
         CURED DUB,(L'PLOSBBL,PLOSBBL),2,CR=YES                                 
         GOTO1 ACREPORT                                                         
         ICM   R1,15,ACASNTRY       GET ADDRESS OF THIS ENTRY                   
         BZ    CONS9                                                            
         USING CASUMD,R1                                                        
         CLI   CASOFF,X'FF'         ALREADY MORE THAN 1 OFFICE ?                
         BE    CONS9                YES,                                        
         OC    CASOFF,CASOFF        FIRST TIME ?                                
         BNZ   *+10                 NO,                                         
         MVC   CASOFF,BUFOFF                                                    
         CLC   CASOFF,BUFOFF        SAME OFFICE ?                               
         BE    *+8                  YES,                                        
         MVI   CASOFF,X'FF'         MORE THAN ONE OFFICE                        
         DROP  R1                                                               
*                                                                               
CONS9    GOTO1 BUFFERIN,DMCB,('BUFFASEQ',ABUFF),BUFKEY,ADCOMFAC                 
         B     CONS5                                                            
*                                                                               
CONS11   TM    OPT,OPTNOC                                                       
         BO    CONS3                                                            
         GOTO1 ACREPORT                                                         
         L     RF,ADBOX                                                         
         USING BOXD,RF                                                          
         MVC   BOXROWS,SPACES                                                   
         SR    RE,RE                                                            
         IC    RE,LINE                                                          
         LA    RE,BOXROWS-1(RE)                                                 
         MVI   0(RE),C'M'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 ACREPORT                                                         
         B     CONS3                                                            
*                                                                               
         DROP  R3,R5,RF                                                         
         EJECT                                                                  
***********************************************************************         
* GET NEXT CONTRA RECORD FROM BUFFERIN                                *         
***********************************************************************         
GETN     NTR1  ,                                                                
         LA    R5,BUFREC                                                        
         USING BUFD,R5                                                          
*                                                                               
GETN3    XC    BUFKEY,BUFKEY                                                    
         MVI   BUFTYP,BUFTCON                                                   
         MVC   BUFCON,LASTCON                                                   
         ZAP   OUT,=P'0'                                                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,BUFCON+L'BUFCON-1                                             
         AHI   RF,1                                                             
         STC   RF,BUFCON+L'BUFCON-1                                             
         GOTO1 BUFFERIN,DMCB,('BUFFARDH',ABUFF),BUFKEY,ADCOMFAC                 
         TM    4(R1),X'80'                                                      
         BO    GETNO                                                            
         CLI   BUFTYP,BUFTCON                                                   
         BNE   GETNO                                                            
         MVC   LASTCON,BUFCON      SAVE THIS CONTRA                             
         ZAP   OUT,BUFNDR                                                       
         SP    OUT,BUFNCR                                                       
         ZAP   DUB,BUFHDR                                                       
         SP    DUB,BUFHCR                                                       
         SP    OUT,DUB                                                          
*                                                                               
GETYES   CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
GETNO    LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT THE MONTH  SUMMARY                                            *         
***********************************************************************         
MONS     NTR1  ,                                                                
         MVI   RCSUBPRG,3                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   LASTCON,SPACES                                                   
         LA    R3,XP                                                            
         USING PLD,R3                                                           
         LA    R5,BUFREC                                                        
         USING BUFD,R5                                                          
         XC    BUFKEY,BUFKEY                                                    
         MVI   BUFTYP,BUFTMON                                                   
*                                                                               
         GOTO1 BUFFERIN,DMCB,('BUFFARDH',ABUFF),BUFKEY,ADCOMFAC                 
*                                                                               
MONS3    TM    4(R1),X'80'                                                      
         BO    MONS11                                                           
         CLI   BUFTYP,BUFTMON                                                   
         BNE   MONS11                                                           
         CP    BUFNDR,BUFHDR       TEST DEBITS EQUAL                            
         BNE   *+14                                                             
         CP    BUFNCR,BUFHCR       TEST CREDITS EQUAL                           
         BE    MONS9                                                            
*                                                                               
         TM    OPT,OPTNOC          SUPPRESS PRINTING?                           
         BO    MONS8                                                            
         CLC   BUFCON,LASTCON      SAME CONTRA ?                                
         BE    MONS5                                                            
         CLC   LASTCON,SPACES      FIRST TIME ?                                 
         BE    MONS5                                                            
         BAS   RE,MONBX            NO, DRAW A BOX                               
*                                                                               
MONS5    MVC   LASTCON,BUFCON                                                   
         MVC   PLMSCON,BUFCON+1    CONTRA                                       
         MVC   FULL(2),BUFMOS                                                   
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(1,FULL),(6,PLMSMON)                                 
         OC    BUFEND,BUFEND                                                    
         BZ    MONS7                                                            
         MVI   PLMSMON+6,C'-'                                                   
         MVC   FULL(2),BUFEND                                                   
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(1,FULL),(6,PLMSMON+7)                               
*                                                                               
MONS7    CURED BUFNDR,(L'PLOSTDR,PLOSTDR),2,CR=YES                              
         CURED BUFNCR,(L'PLOSTCR,PLOSTCR),2,CR=YES                              
         ZAP   DUB,BUFNDR                                                       
         SP    DUB,BUFNCR                                                       
         ZAP   OUT,DUB                                                          
         CURED DUB,(L'PLOSTBL,PLOSTBL),2,CR=YES                                 
*                                                                               
         CURED BUFHDR,(L'PLOSBDR,PLOSBDR),2,CR=YES                              
         CURED BUFHCR,(L'PLOSBCR,PLOSBCR),2,CR=YES                              
         ZAP   DUB,BUFHDR                                                       
         SP    DUB,BUFHCR                                                       
         SP    OUT,DUB                                                          
         CURED DUB,(L'PLOSBBL,PLOSBBL),2,CR=YES                                 
         CURED OUT,(L'PLOSOUT,PLOSOUT),2,CR=YES                                 
         GOTO1 ACREPORT                                                         
*                                                                               
MONS8    ZAP   OUT,BUFNDR                                                       
         SP    OUT,BUFNCR                                                       
         SP    OUT,BUFHDR                                                       
         AP    OUT,BUFHCR                                                       
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,CMSNUM                                                      
         L     RF,=AL4(MXCMD)                                                   
         CR    R1,RF                                                            
         BL    *+6                                                              
         DC    H'0'                 MONTHLY SUMMARY TABLE IS FULL               
         LA    RF,1(R1)                                                         
         STCM  RF,3,CMSNUM          ADD TO CONTRA/MONTH DIFF. SUMMARY           
         MHI   R1,CMSLNQ                                                        
         A     R1,ACMSUM                                                        
         USING CMSUMD,R1                                                        
         MVC   CMSCAC,BUFCON                                                    
         MVC   CMSMOS,BUFMOS                                                    
         ZAP   CMSAMT,OUT           GET NET (OVR/UNDR)                          
         ZAP   CMSDR,BUFNDR                                                     
         SP    CMSDR,BUFHDR         SET BUCKET AMOUNTS                          
         ZAP   CMSCR,BUFNCR                                                     
         SP    CMSCR,BUFHCR                                                     
*                                                                               
         L     RF,ACASUM            POST TO CONTRA TOTAL SUMMARY                
         USING CASUMD,RF                                                        
         SR    R0,R0                                                            
         ICM   R0,3,CASNUM                                                      
         BZ    MONS9                                                            
         CLC   CMSCAC,CASCAC        MATCH CONTRA                                
         BE    *+16                                                             
         LA    RF,CASLNQ(RF)                                                    
         BCT   R0,*-14                                                          
         B     MONS9                                                            
         AP    CASTAMT,CMSAMT                                                   
         DROP  R1,RF                                                            
*                                                                               
MONS9    GOTO1 BUFFERIN,DMCB,('BUFFASEQ',ABUFF),BUFKEY,ADCOMFAC                 
         B     MONS3                                                            
*                                                                               
MONS11   TM    OPT,OPTNOC                                                       
         BO    XIT                                                              
         CLC   LASTCON,SPACES      ANYTHING PRINTED ?                           
         BE    XIT                                                              
         BAS   RE,MONBX            END THE BOX                                  
         B     XIT                                                              
*                                                                               
MONBX    LR    R0,RE               DRAW A BOX LINE                              
         GOTO1 ACREPORT                                                         
         L     RF,ADBOX                                                         
         USING BOXD,RF                                                          
         MVC   BOXROWS,SPACES                                                   
         SR    RE,RE                                                            
         IC    RE,LINE                                                          
         LA    RE,BOXROWS-1(RE)                                                 
         MVI   0(RE),C'M'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 ACREPORT                                                         
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         DROP  R3,R5,RF                                                         
         EJECT                                                                  
***********************************************************************         
* ATTEMPT TO FIX THE FILE                                             *         
***********************************************************************         
FIX      NTR1  ,                                                                
         MVI   XFLG,0                                                           
         LA    R5,BUFREC           POST OFFICE TO NON-OFFICE                    
         USING BUFD,R5                                                          
         XC    BUFKEY,BUFKEY                                                    
         MVI   BUFTYP,BUFTMON                                                   
         LA    R4,CMSWRK                                                        
         USING CMSUMD,R4                                                        
         ZAP   CMSAMT,=P'0'                                                     
         XC    CMSMOS,CMSMOS                                                    
         CP    CONOUT,=P'0'        TEST CONTRAS IN BALANCE                      
         BE    FIX9                YES, DON'T FIX                               
         GOTO1 BUFFERIN,DMCB,('BUFFARDH',ABUFF),BUFKEY,ADCOMFAC                 
*                                                                               
FIX3     TM    4(R1),X'80'         EOF ?                                        
         BO    FIX7                                                             
         CLI   BUFTYP,BUFTMON                                                   
         BNE   FIX7                                                             
         CP    BUFNDR,=P'0'        TEST NON-OFFICE DEBITS                       
         BNE   FIX5                                                             
         CP    BUFNCR,=P'0'        TEST NON-OFFICE CREDITS                      
         BNE   FIX5                                                             
         CP    BUFNDR,BUFHDR       SKIP IF BUCKETS EQUAL                        
         BNE   *+14                                                             
         CP    BUFNCR,BUFHCR                                                    
         BE    FIX5                                                             
         CLC   BUFCON,CMSCAC                                                    
         BE    *+8                                                              
         BAS   RE,PUTR             PUT LAST RECORD(IF ANY)                      
         MVC   CMSCAC,BUFCON                                                    
         MVC   CMSMOS(2),BUFMOS                                                 
         ZAP   CMSDR,BUFHDR                                                     
         ZAP   CMSCR,BUFHCR                                                     
         MVC   OFC,SPACES                                                       
         GOTO1 BUKR,DMCB,CMSUMD    POST TO NON-OFFICE                           
         AP    CONBAL,CMSDR        ADJUST NON-OFFICE BUCKETS                    
         SP    CONBAL,CMSCR                                                     
*                                                                               
FIX5     GOTO1 BUFFERIN,DMCB,('BUFFASEQ',ABUFF),BUFKEY,ADCOMFAC                 
         B     FIX3                                                             
*                                                                               
FIX7     BAS   RE,PUTR             PUT LAST RECORD(IF ANY)                      
         MVI   XFLG,0                                                           
         ZAP   CONOUT,ACCBAL       FIX CONTRA ACCOUNT DIFFERENCE                
         SP    CONOUT,CONBAL                                                    
         BZ    FIX9                NO DIFFERENCE                                
         CLC   OLDCON,SPACES                                                    
         BNH   XIT                 CAN'T FIX - NO CONTRA                        
         MVC   CMSCAC,OLDCON       POST DIFFERENCE TO OLDEST CONTRA             
         MVC   CMSMOS(2),OLDMOS    AND MONTH                                    
         MVC   OFC,SPACES          NON-OFFICE                                   
         ZAP   CMSCR,=P'0'                                                      
         ZAP   CMSDR,CONOUT                                                     
         GOTO1 BUKR,DMCB,CMSUMD                                                 
         BAS   RE,PUTR                                                          
                                                                                
FIX9     MVI   XFLG,0                                                           
         TM    FLG,F2OFF           2 CHARACTER OFFICE ?                         
         BNO   XIT                                                              
         SR    RF,RF                                                            
         SR    R0,R0               FIND OFFICE ENTRY                            
         ICM   R0,3,OFSNUM         THAT'S OUT OF BALANCE                        
         BNZ   *+8                                                              
         BAS   RE,OFAC             IF NONE, PUT DIFF. TO BIGOFF                 
         ICM   R0,3,OFSNUM         TRY THAT AGAIN                               
         BZ    NOOFFICE                                                         
         L     R3,AOFSUM                                                        
         USING OFSUMD,R3                                                        
         CHI   R0,1                ONLY ONE OFFICE NOT IN BALANCE               
         BE    FIX12               USE IT                                       
*                                                                               
FIX10    LTR   RF,RF               TEST FIRST TIME                              
         BZ    *+14                                                             
         CLC   OFSAMT(L'OFSAMT-1),OFSAMT-OFSUMD(RF)                             
         BNH   *+6                                                              
         LR    RF,R3               SAVE ADDRESS OF BIGGEST DIFFERENCE           
         LA    R3,OFSLNQ(R3)                                                    
         BCT   R0,FIX10                                                         
         LTR   RF,RF               MISSING DEFAULT OFFICE                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LR    R3,RF                                                            
*                                                                               
FIX12    LA    R5,BUFREC           POST  NON-OFFICE TO OFFICE                   
         USING BUFD,R5                                                          
         XC    BUFKEY,BUFKEY                                                    
         MVI   BUFTYP,BUFTMON                                                   
         GOTO1 BUFFERIN,DMCB,('BUFFARDH',ABUFF),BUFKEY,ADCOMFAC                 
*                                                                               
FIX13    TM    4(R1),X'80'         EOF ?                                        
         BO    FIX17                                                            
         CLI   BUFTYP,BUFTMON                                                   
         BNE   FIX17                                                            
         CP    BUFNDR,=P'0'        SKIP IF NON-OFFICE ZERO                      
         BNE   *+14                                                             
         CP    BUFNCR,=P'0'                                                     
         BE    FIX15                                                            
         CP    BUFNDR,BUFHDR       SKIP IF BUCKETS EQUAL                        
         BNE   *+14                                                             
         CP    BUFNCR,BUFHCR                                                    
         BE    FIX15                                                            
         CLC   BUFCON,CMSCAC                                                    
         BE    *+8                                                              
         BAS   RE,PUTR             PUT LAST RECORD(IF ANY)                      
         MVC   CMSCAC,BUFCON                                                    
         MVC   CMSMOS(2),BUFMOS                                                 
         ZAP   CMSDR,BUFNDR                                                     
         SP    CMSDR,BUFHDR                                                     
         ZAP   CMSCR,BUFNCR                                                     
         SP    CMSCR,BUFHCR                                                     
         MVC   OFC,OFSOFF                                                       
         GOTO1 BUKR,DMCB,CMSUMD    POST TO DEFAULT OFFICE                       
         SP    OFSAMT,CMSDR        ADJUST DIFF. FOR DEFAULT OFFICE              
         AP    OFSAMT,CMSCR                                                     
*                                                                               
FIX15    GOTO1 BUFFERIN,DMCB,('BUFFASEQ',ABUFF),BUFKEY,ADCOMFAC                 
         B     FIX13                                                            
*                                                                               
FIX17    BAS   RE,PUTR             PUT LAST RECORD(IF ANY)                      
         SR    R0,R0               POST OFFICE DIFFERENCE                       
         ICM   R0,3,OFSNUM                                                      
         L     R3,AOFSUM                                                        
         USING OFSUMD,R3                                                        
         MVC   CMSCAC,OLDCON       POST DIFFERENCE TO OLDEST CONTRA             
         MVC   CMSMOS(2),OLDMOS     AND MONTH                                   
         ZAP   CMSCR,=P'0'                                                      
*                                                                               
FIX19    MVC   OFC,OFSOFF          OFFICE                                       
         ZAP   CMSDR,OFSAMT                                                     
         CP    CMSDR,=P'0'                                                      
         BE    FIX21                                                            
         GOTO1 BUKR,DMCB,CMSUMD                                                 
         BAS   RE,PUTR                                                          
FIX21    LA    R3,OFSLNQ(R3)                                                    
         BCT   R0,FIX19                                                         
         B     XIT                                                              
         DROP  R3,R4,R5                                                         
*                                                                               
NOOFFICE MVC   XP+3(20),=CL20'ERROR NO OFFICE'                                  
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD DIFFERENCE TO OFFICE ACCOUNT RECORD                             *         
***********************************************************************         
OFAC     NTR1  ,                                                                
         LA    R2,DKEY                                                          
         USING OFARECD,R2                                                       
         L     RF,ADACC                                                         
         MVC   OFAKEY,0(RF)                                                     
         MVC   OFAKOFF,BIGOFF      USE OFFICE WITH BIGGEST BALANCE              
         CLC   OFAKOFF,SPACES      IS THERE ONE ?                               
         BH    OFAC1               YES, USE IT                                  
         MVC   OFAKOFF,QOFFICE     DEFAULT                                      
         CLC   OFAKOFF,SPACES                                                   
         BH    OFAC1                                                            
         B     XIT                                                              
*                                                                               
OFAC1    BAS   RE,DMRD           GET THE OFFICE/ACCOUNT RECORD                  
         CLC   OFAKEY,DIR                                                       
         BNE   OFAC5                                                            
         MVC   AIO,AIO2                                                         
         L     R2,AIO                                                           
         BAS   RE,DMGETR         GET THE RECORD                                 
         BAS   RE,DMPGET                                                        
         SR    R0,R0                                                            
         LA    R3,OFARFST                                                       
OFAC2    CLI   0(R3),ABLELQ                                                     
         BE    OFAC3                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BNE   OFAC2                                                            
         DC    H'0'                NO BALANCE ELEMENT                           
*                                                                               
         USING ABLELD,R3                                                        
OFAC3    AP    ABLFRWD,OFAOUT      ADD OUT-OF-BALNACE AMOUNT                    
         BAS   RE,DMPUTR           WRITE THE RECORD                             
         AP    CNTCHA,=P'1'                                                     
         BAS   RE,DMPPUT           PRINT RECORD                                 
         B     OFAC7                                                            
*                                                                               
OFAC5    MVC   AIO,AIO2            ADD OFFICE ACCOUNT                           
         L     R2,AIO                                                           
         MVC   OFAKEY,DKEY                                                      
         XC    OFARLEN(30),OFARLEN                                              
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ABLEL,ABLELQ                                                     
         MVI   ABLLN,ABLLN3Q                                                    
         ZAP   ABLFRWD,OFAOUT      ADD OUT-OF-BALANCE AMOUNT                    
         ZAP   ABLDR,=P'0'                                                      
         ZAP   ABLCR,=P'0'                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCMST),AIO,ELEMENT,0                           
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                 CAN'T ADD THE ELEMENT                       
*                                                                               
         BAS   RE,DMADDR            ADD RECORD                                  
         AP    CNTADD,=P'1'                                                     
         BAS   RE,DMPNEW            PRINT RECORD                                
*                                                                               
OFAC7    L     R3,AOFSUM                                                        
         USING OFSUMD,R3                                                        
         MVC   OFSOFF,OFAKOFF      PUT OFFICE  OUT OF BALANCE                   
         ZAP   OFSAMT,OFAOUT                                                    
         LA    R0,1                                                             
         STCM  R0,3,OFSNUM                                                      
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* PUT/ADD  BUCKET RECORD                                              *         
***********************************************************************         
PUTR     NTR1  ,                                                                
         TM    XFLG,XADD            ADD NEW RECORD ?                            
         BNO   PUTR3                                                            
         BAS   RE,DMADDR            ADD RECORD                                  
         AP    CNTADD,=P'1'                                                     
         BAS   RE,DMPNEW            PRINT RECORD                                
         BAS   RE,HEADR             CHECK FOR/ADD HEADER                        
         B     PUTRX                                                            
*                                                                               
PUTR3    TM    XFLG,XPUT            PUT RECORD ?                                
         BNO   PUTRX                                                            
         BAS   RE,DMPUTR            PUT RECORD                                  
         AP    CNTCHA,=P'1'                                                     
         BAS   RE,DMPPUT            PRINT RECORD                                
         BAS   RE,HEADR             CHECK FOR/ADD HEADER                        
*                                                                               
PUTRX    NI    XFLG,ALL-(XADD+XPUT) SET FOR NEW CONTRA                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD A BUCKET RECORD                                               *         
***********************************************************************         
BUKR     NTR1  ,                                                                
         L     R5,0(R1)                                                         
         USING CMSUMD,R5                                                        
         MVC   AIO,AIO2                                                         
         TM    XFLG,XADD+XPUT       TEST FIRST FOR CONTRA                       
         BNZ   BUKR5                NO,                                         
         LA    R2,DKEY                                                          
         USING CACRECD,R2                                                       
         MVC   CACKEY,SPACES        BUILD RECORD KEY                            
         L     RF,ADACC                                                         
         MVC   CACKCULA,0(RF)       ACCOUNT                                     
         MVC   CACKOFF,OFC          OFFICE                                      
         MVC   CACKCULC,CMSCAC      CONTRA                                      
         BAS   RE,DMHGH                                                         
         CLC   DKEY,DIR                                                         
         BE    BUKR3                                                            
         OI    XFLG,XADD            SET FOR NEW RECORD                          
         L     R2,AIO                                                           
         MVC   CACKEY,DKEY          RECORD KEY                                  
         XC    CACRLEN(30),CACRLEN                                              
         B     BUKR5                                                            
*                                                                               
BUKR3    OI    XFLG,XPUT            SET PUTREC                                  
         BAS   RE,DMGETR            GET THE EXISTING RECORD                     
         BAS   RE,DMPGET                                                        
*                                                                               
BUKR5    OC    CMSMOSE,CMSMOSE      TEST END DATE                               
         BNZ   BUKR11                                                           
         GOTO1 HELLO,DMCB,(C'G',ACCMST),('PBKELQ',AIO),0                        
         CLI   DMCB+12,0                                                        
         BNE   BUKR6                                                            
         L     R4,DMCB+12                                                       
         USING PBKELD,R4                                                        
         CLC   CMSMOS,PBKHI                                                     
         BH    BUKR6                                                            
         CLC   PBKLOW,CMSMOS                                                    
         BL    *+10                                                             
         MVC   PBKLOW,CMSMOS                                                    
         AP    PBKDR,CMSDR                                                      
         AP    PBKCR,CMSCR                                                      
         B     XIT                                                              
*                                                                               
BUKR6    LA    R3,ELEMENT                                                       
         USING BUKELD,R3                                                        
         MVI   BUKEL,BUKELQ                                                     
         MVI   BUKLN,BUKLNQ                                                     
         MVC   BUKMOS,CMSMOS        YEAR/MONTH                                  
         ZAP   BUKDR,CMSDR                                                      
         ZAP   BUKCR,CMSCR                                                      
*                                                                               
         LA    R0,L'BUKMOS                                                      
         GOTO1 HELLO,DMCB,(C'G',ACCMST),('BUKELQ',AIO),((R0),BUKMOS)            
         CLI   DMCB+12,0            TEST ALREADY IN RECORD                      
         BNE   BUKR7                                                            
         L     R1,DMCB+12                                                       
         AP    BUKDR-BUKELD(L'BUKDR,R1),BUKDR                                   
         AP    BUKCR-BUKELD(L'BUKCR,R1),BUKCR                                   
         B     XIT                                                              
*                                                                               
BUKR7    GOTO1 HELLO,DMCB,(C'P',ACCMST),AIO,ELEMENT,0                           
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
*                                                                               
         L     R2,AIO                                                           
         LA    R3,CACRFST           POINT TO OLDEST (FIRST) BUCKET              
         MVC   BUKSAVE,BUKELD                                                   
         CLI   0(R3),BUKELQ         MAKE SURE IT'S A BUCKET                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   BUKEL,X'FF'                                                      
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',AIO),0                           
         GOTO1 HELLO,DMCB,(C'P',ACCMST),AIO,ELEMENT,0                           
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                 CAN'T ADD THE ELEMENT                       
*                                                                               
         LA    R3,BUKSAVE                                                       
         GOTO1 HELLO,DMCB,(C'G',ACCMST),('PBKELQ',AIO),0                        
         CLI   DMCB+12,0                                                        
         BNE   BUKR9                                                            
         L     R4,DMCB+12           UPDATE PRIOR BUCKET ELEMENT                 
         USING PBKELD,R4                                                        
         CLC   PBKLOW,BUKYEAR                                                   
         BL    *+10                                                             
         MVC   PBKLOW,BUKYEAR                                                   
         CLC   PBKHI,BUKYEAR                                                    
         BH    *+10                                                             
         MVC   PBKHI,BUKYEAR                                                    
         AP    PBKDR,BUKDR                                                      
         AP    PBKCR,BUKCR                                                      
         B     XIT                                                              
*                                                                               
BUKR9    XC    ELEMENT,ELEMENT      CREATE PRIOR BUCKET ELEMENT                 
         LA    R4,ELEMENT                                                       
         MVI   PBKEL,PBKELQ                                                     
         MVI   PBKLN,PBKLNQ                                                     
         MVC   PBKLOW,BUKYEAR                                                   
         MVC   PBKHI,BUKYEAR                                                    
         ZAP   PBKDR,BUKDR                                                      
         ZAP   PBKCR,BUKCR                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCMST),AIO,PBKEL,0                             
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                 CAN'T ADD THE ELEMENT                       
         B     XIT                                                              
*                                                                               
BUKR11   GOTO1 HELLO,DMCB,(C'G',ACCMST),('PBKELQ',AIO),0                        
         CLI   DMCB+12,0                                                        
         BNE   BUKR13                                                           
         L     R4,DMCB+12                UPDATE PRIOR BUCKET ELEMENT            
         CLC   PBKLOW,CMSMOS                                                    
         BL    *+10                                                             
         MVC   PBKLOW,CMSMOS                                                    
         CLC   PBKHI,CMSMOSE                                                    
         BH    *+10                                                             
         MVC   PBKHI,CMSMOSE                                                    
         AP    PBKDR,CMSDR                                                      
         AP    PBKCR,CMSCR                                                      
         B     XIT                                                              
*                                                                               
BUKR13   XC    ELEMENT,ELEMENT           CREATE PRIOR BUCKET ELEMENT            
         LA    R4,ELEMENT                                                       
         MVI   PBKEL,PBKELQ                                                     
         MVI   PBKLN,PBKLNQ                                                     
         MVC   PBKLOW,CMSMOS                                                    
         MVC   PBKHI,CMSMOSE                                                    
         ZAP   PBKDR,CMSDR                                                      
         ZAP   PBKCR,CMSCR                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCMST),AIO,PBKEL,0                             
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                      CAN'T ADD THE ELEMENT                  
         B     XIT                                                              
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
***********************************************************************         
* CHECK FOR A HEADER FOR RECORD IN IO2                                *         
***********************************************************************         
HEADR    NTR1  ,                                                                
         L     R2,AIO2                                                          
         ST    R2,AIO                                                           
         MVC   DKEY,0(R2)                                                       
         LA    R2,DKEY              READ FOR HEADER                             
         USING CHDRECD,R2                                                       
         MVC   CHDKSPCS,SPACES                                                  
         MVC   CHDKBTYP,SPACES                                                  
         XC    CHDKNULL,CHDKNULL                                                
         BAS   RE,DMHGH                                                         
         CLC   DKEY,DIR             HEADER FOUND ?                              
         BE    XIT                  YES,                                        
         L     R2,AIO2                                                          
         MVC   CHDKEY,DKEY          NO, BUILD ONE                               
         XC    CHDKSTA(30),CHDKSTA                                              
*                                                                               
         LA    R3,ELEMENT           BUILD CONTRA ELEMENT                        
         USING CACEL,R3                                                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   CACEL,CACELQ                                                     
         MVI   CACLN,CACLN1Q                                                    
         MVC   CACCNT,CHDKCULC                                                  
*                                                                               
         MVC   DKEY,SPACES          READ FOR CONTRA                             
         MVC   DKEY(L'CHDKCULC),CHDKCULC                                        
         MVC   AIO,AIO3                                                         
         BAS   RE,DMHGH                                                         
         CLC   DKEY,DIR             CONTRA FOUND ?                              
         BNE   HEADR7               NO, JUST ADD ACCOUNT CODE                   
         BAS   RE,DMGETR            GET CONTRA ACCOUNT                          
         L     R4,AIO                                                           
         LA    R4,ACTRFST-ACTRECD(R4)                                           
         SR    R1,R1                                                            
*                                                                               
         USING NAMELD,R4                                                        
HEADR3   CLI   0(R4),0                                                          
         BE    HEADR7                                                           
         CLI   0(R4),NAMELQ         GET ACCOUNT NAME                            
         BE    HEADR5                                                           
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     HEADR3                                                           
*                                                                               
HEADR5   IC    R1,NAMLN             ADD NAME TO CACEL                           
         SHI   R1,NAMLN1Q+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CACNAME(0),NAMEREC                                               
*                                                                               
         LA    R1,CACLN1Q+1(R1)     SET LENGTH OF ELEMENT                       
         STC   R1,CACLN                                                         
*                                                                               
HEADR7   MVC   AIO,AIO2             RESTORE AIO                                 
         GOTO1 HELLO,DMCB,(C'P',ACCMST),AIO,CACEL,0                             
         BAS   RE,DMADDR            ADD CONTRA HEADER                           
         AP    CNTADD,=P'1'                                                     
         BAS   RE,DMPNEW                                                        
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
                                                                                
DMRD     ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMHGH    ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMSEQ    ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGETR   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,AIO,DMWORK                         
         B     DMERR                                                            
*                                                                               
DMWRTR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         B     DMERR                                                            
*                                                                               
DMADDR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,AIO,DMWORK                         
         B     DMERR                                                            
*                                                                               
DMPUTR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,AIO,DMWORK                         
*                                                                               
DMERR    MVC   BYTE,8(R1)                                                       
         NI    BYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED               
         CLI   BYTE,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO DUMP RECORDS                                            *         
***********************************************************************         
DMPGET   LA    RF,=C'GET'                                                       
         B     DUMP                                                             
*                                                                               
DMPPUT   LA    RF,=C'PUT'                                                       
         B     DUMP                                                             
*                                                                               
DMPNEW   LA    RF,=C'NEW'                                                       
*                                                                               
DUMP     TM    OPT,OPTDMP          DUMP RECORDS                                 
         BNOR  RE                                                               
         CP    PDUMP,MAXDUMP                                                    
         BHR   RE                                                               
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNER  RE                                                               
DUMP3    NTR1  ,                                                                
         L     R2,AIO                                                           
         USING ACTRECD,R2                                                       
         SR    R3,R3                                                            
         ICM   R3,3,ACTRLEN                                                     
         GOTO1 PRNTBL,DMCB,(3,(RF)),(R2),C'DUMP',(R3),=C'2D'                    
         B     XIT                                                              
         DROP  R2                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
BUFFERIN DC    V(BUFFERIN)                                                      
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
*                                                                               
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
AIO3     DC    A(IO3)                                                           
*                                                                               
ABUFF    DC    A(BUFF)                                                          
AOFSUM   DC    A(OFSUM)                                                         
ACASUM   DC    A(CASUM)                                                         
ACMSUM   DC    A(CMSUM)                                                         
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT'                                                     
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
*                                                                               
EFFS     DC    X'FFFFFFFF'                                                      
BHI      DC    PL6'99999999999'                                                 
BLO      DC    PL6'-99999999999'                                                
*                                                                               
TOTLQ    DC    X'4141'                                                          
*                                                                               
CNT      DS    0XL34                                                            
CNTACTS  DC    PL4'0',CL30'ACCOUNTS WITH ERRORS'                                
CNTFIX   DC    PL4'0',CL30'ACCOUNTS FIXED'                                      
CNTPART  DC    PL4'0',CL30'ACCOUNTS PARTIALLY FIXED'                            
CNTNOT   DC    PL4'0',CL30'ACCOUNTS NOT FIXED'                                  
CNTADD   DC    PL4'0',CL30'RECORDS ADDED'                                       
CNTCHA   DC    PL4'0',CL30'RECORDS CHANGED'                                     
CNTX     DC    X'FF'                                                            
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'1000'                                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         CLI   RCSUBPRG,0                                                       
         BE    BXXIT                                                            
         L     R2,ADACC                                                         
         USING ACTRECD,R2                                                       
         MVC   XHEAD6+10(L'ACTKACT),ACTKACT   ACCOUNT CODE                      
         L     RF,ADACCNAM                                                      
         USING NAMELD,RF                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   XHEAD6+11+L'ACTKACT(0),NAMEREC   ACCOUNT NAME                    
         DROP  R2                                                               
*                                                                               
         MVC   XHEAD3+70(15),=C'ACCOUNT BALANCE'                                
         CURED ACCBAL,(17,XHEAD3+90),2,CR=YES                                   
         MVC   XHEAD4+70(15),=C'CONTRA ACCOUNT '                                
         CURED CONBAL,(17,XHEAD4+90),2,CR=YES                                   
         CURED CONOUT,(17,XHEAD4+110),2,CR=YES,ZERO=BLANK                       
         TM    FLG,F2OFF                                                        
         BNO   BXHOOK3                                                          
         TM    FLG,FPROD                                                        
         BO    BXHOOK3                                                          
*                                                                               
         MVC   XHEAD5+70(15),=C'OFFICE/ACCOUNT '                                
         CURED OFFACC,(17,XHEAD5+90),2,CR=YES                                   
         CURED OFAOUT,(17,XHEAD5+110),2,CR=YES,ZERO=BLANK                       
         MVC   XHEAD6+70(15),=C'OFFICE/CONTRA  '                                
         CURED OFFCON,(17,XHEAD6+90),2,CR=YES                                   
         CURED OFCOUT,(17,XHEAD6+110),2,CR=YES,ZERO=BLANK                       
*                                                                               
BXHOOK3  L     RF,ADBOX                                                         
         USING BOXD,RF                                                          
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
*                                                                               
         MVI   BOXROWS+7,C'T'               HEAD 8                              
         MVI   BOXROWS+11,C'M'              HEAD 12                             
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS+(PLCL-PLD),C'L'                                          
         MVI   BOXCOLS+(PLCR-PLD),C'R'                                          
         CLI   RCSUBPRG,1                                                       
         BNE   BXCON                                                            
*                                                                               
*              BOXES FOR OFFICE SUMMARY                                         
*                                                                               
         MVI   BOXCOLS+(PLOSC1-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC2-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC3-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC4-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC5-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC6-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC7-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC8-PLD),C'C'                                        
*                                                                               
         MVI   XHEAD8+(PLOSC2-PLD),X'BF'                                        
         MVI   XHEAD8+(PLOSC3-PLD),X'BF'                                        
         MVI   XHEAD8+(PLOSC4-PLD),X'BF'                                        
         MVI   XHEAD8+(PLOSC6-PLD),X'BF'                                        
         MVI   XHEAD8+(PLOSC7-PLD),X'BF'                                        
*                                                                               
         MVI   XHEAD9+(PLOSC2-PLD),X'41'                                        
         MVI   XHEAD9+(PLOSC3-PLD),X'41'                                        
         MVI   XHEAD9+(PLOSC4-PLD),X'41'                                        
         MVI   XHEAD9+(PLOSC6-PLD),X'41'                                        
         MVI   XHEAD9+(PLOSC7-PLD),X'41'                                        
*                                                                               
         MVI   XHEAD10+(PLOSC1-PLD),X'BF'    DASH                               
         LA    R1,PLOSC8-PLOSC1-2                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   XHEAD10+(PLOSC1-PLD+1)(0),XHEAD10+(PLOSC1-PLD)                   
         MVI   XHEAD10+(PLOSC1-PLD),X'EB'    LEFT T                             
         MVI   XHEAD10+(PLOSC2-PLD),X'CC'    TOP T                              
         MVI   XHEAD10+(PLOSC3-PLD),X'CC'    TOP T                              
         MVI   XHEAD10+(PLOSC4-PLD),X'CC'    TOP T                              
         MVI   XHEAD10+(PLOSC5-PLD),X'8F'    +                                  
         MVI   XHEAD10+(PLOSC6-PLD),X'CC'    TOP T                              
         MVI   XHEAD10+(PLOSC7-PLD),X'CC'    TOP T                              
         MVI   XHEAD10+(PLOSC8-PLD),X'EC'    RIGHT T                            
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
         B     BXXIT                                                            
*                                                                               
*              BOXES FOR CONTRA SUMMARY                                         
*                                                                               
BXCON    DS    0H                                                               
         CLI   RCSUBPRG,2                                                       
         BNE   BXMON                                                            
         MVI   BOXCOLS+(PLCSC1-PLD),C'C'                                        
         MVI   BOXCOLS+(PLCSC2-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC5-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC6-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC7-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC8-PLD),C'C'                                        
         MVI   XHEAD10+(PLOSC5-PLD),X'BF'    DASH                               
         LA    R1,PLOSC8-PLOSC5-2                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   XHEAD10+(PLOSC5-PLD+1)(0),XHEAD10+(PLOSC5-PLD)                   
*                                                                               
         MVI   XHEAD8+(PLOSC6-PLD),X'BF'                                        
         MVI   XHEAD8+(PLOSC7-PLD),X'BF'                                        
*                                                                               
         MVI   XHEAD9+(PLOSC6-PLD),X'41'                                        
         MVI   XHEAD9+(PLOSC7-PLD),X'41'                                        
*                                                                               
         MVI   XHEAD10+(PLOSC5-PLD),X'EB'    LEFT T                             
         MVI   XHEAD10+(PLOSC6-PLD),X'CC'    TOP T                              
         MVI   XHEAD10+(PLOSC7-PLD),X'CC'    TOP T                              
         MVI   XHEAD10+(PLOSC8-PLD),X'EC'    RIGHT T                            
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
         B     BXXIT                                                            
*                                                                               
*              BOXES FOR MONTH SUMMARY                                          
*                                                                               
BXMON    DS    0H                                                               
         MVI   BOXCOLS+(PLMSC1-PLD),C'C'                                        
         MVI   BOXCOLS+(PLMSC2-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC3-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC4-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC5-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC6-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC7-PLD),C'C'                                        
         MVI   BOXCOLS+(PLOSC8-PLD),C'C'                                        
*                                                                               
         MVI   XHEAD8+(PLOSC3-PLD),X'BF'                                        
         MVI   XHEAD8+(PLOSC4-PLD),X'BF'                                        
         MVI   XHEAD8+(PLOSC6-PLD),X'BF'                                        
         MVI   XHEAD8+(PLOSC7-PLD),X'BF'                                        
*                                                                               
         MVI   XHEAD9+(PLOSC4-PLD),X'41'                                        
         MVI   XHEAD9+(PLOSC6-PLD),X'41'                                        
         MVI   XHEAD9+(PLOSC7-PLD),X'41'                                        
*                                                                               
         MVI   XHEAD10+(PLMSC2-PLD),X'BF'    DASH                               
         LA    R1,PLOSC8-PLMSC2-2                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   XHEAD10+(PLMSC2-PLD+1)(0),XHEAD10+(PLMSC2-PLD)                   
         MVI   XHEAD10+(PLMSC2-PLD),X'EB'    |-                                 
         MVI   XHEAD10+(PLOSC3-PLD),X'CC'    TOP T                              
         MVI   XHEAD10+(PLOSC4-PLD),X'CC'    TOP T                              
         MVI   XHEAD10+(PLOSC5-PLD),X'8F'    +                                  
         MVI   XHEAD10+(PLOSC6-PLD),X'CC'    TOP T                              
         MVI   XHEAD10+(PLOSC7-PLD),X'CC'    TOP T                              
         MVI   XHEAD10+(PLOSC8-PLD),X'EC'    RIGHT T                            
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
         B     BXXIT                                                            
*                                                                               
BXXIT    XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         DROP  R7,RF                                                            
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* BUFFERIN                                                           *          
**********************************************************************          
BUFF     BUFFD TYPE=P,                                                 X        
               KEYLEN=L'BUFKEY,                                        X        
               COLUMNS=BUFNUM,                                         X        
               BUFFERS=500,                                            X        
               FILE=BUFFWK                                                      
         EJECT                                                                  
**********************************************************************          
* IO AREAS/SUMMARY BUFFERS                                           *          
**********************************************************************          
         DS    0D                                                               
         DC    CL8'**IO1**'                                                     
IO1      DS    XL2000                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'**IO2**'                                                     
IO2      DS    XL2000                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'**IO3**'                                                     
IO3      DS    XL2000                                                           
*                                                                               
MXOFD    EQU   200             MAX OFFICES WITH DIFFERENCES                     
MXCAD    EQU   20000           MAX CONTRAS WITH DIFFERENCES                     
MXCMD    EQU   80000           MAX CONTRAS/MONTHS WITH DIFFERENCES              
*                                                                               
         DS    0D                                                               
         DC    CL8'*OFFSUM*'                                                    
OFSUM    DS    (MXOFD)XL(OFSLNQ)                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*CACSUM*'                                                    
CASUM    DS    (MXCAD)XL(CASLNQ)                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*CAMSUM*'                                                    
CMSUM    DS    (MXCMD)XL(CMSLNQ)                                                
         EJECT                                                                  
***********************************************************************         
* DSECT FOR LOCAL STORAGE                                             *         
***********************************************************************         
ACOCD    DSECT                                                                  
AIO      DS    A                                                                
ADBOX    DS    A                   ADDRESS OF THE BOX ROUTINE                   
*                                                                               
SAVRE    DS    F                                                                
*                                                                               
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DIR      DS    CL64                DIRECTORY RECORD                             
DA       DS    XL4                                                              
*                                                                               
TODAY2   DS    CL2                                                              
*                                                                               
FLG      DS    X                                                                
F2OFF    EQU   X'80'        2 CHARACTER OFFICE                                  
FPROD    EQU   X'40'        PRODICTION LEDGER                                   
*                                                                               
OPT      DS    X            OPTION SWITCH                                       
OPTDIF   EQU   X'80'        ONLY ACCOUNTS WITH DIFFERENCES                      
OPTOFS   EQU   X'40'        ONLY OFFICE SUMMARY                                 
OPTCON   EQU   X'20'        PRINT ALL CONTRAS                                   
OPTNOC   EQU   X'10'        SUPPRESS  CONTRAS                                   
OPTFIX   EQU   X'08'        FIX THE FILE                                        
OPTDMP   EQU   X'04'        DUMP OUTPUT RECORDS                                 
OPTADIF  EQU   X'02'        ACCOUNT HAS A DIFFERENCE                            
*                                                                               
XFLG     DS    XL1          FIX CONTROL                                         
XADD     EQU   X'80'        ADD NEW RECORD                                      
XPUT     EQU   X'40'        PUT A CHANGED RECORD                                
*                                                                               
ALL      EQU   X'FF'                                                            
EOT      EQU   X'FF'                                                            
*                                                                               
ELEMENT  DS    XL255                                                            
BUKSAVE  DS    XL(BUKLNQ)                                                       
*                                                                               
MOS      DS    XL4                                                              
         ORG   MOS                                                              
MOSS     DS    XL2                                                              
MOSE     DS    XL2                                                              
*                                                                               
OFC      DS    CL2                                                              
*                                                                               
OLDMOS   DS    XL2                 OLDEST MOS     (NON-OFFICE)                  
OLDMOSE  DS    XL2                                                              
OLDCON   DS    XL15                OLDEST CONTRA                                
BIGOFF   DS    CL2                 OFFICE WITH BIGGEST BALANCE                  
BIGBAL   DS    PL8                 BIGGEST BALANCE FORWARD                      
*                                                                               
CMSWRK   DS    XL(CMSLNQ)                                                       
*                                                                               
DR       DS    PL8                                                              
CR       DS    PL8                                                              
OUT      DS    PL8                                                              
*                                                                               
ACCBAL   DS    PL8                 ACCOUNT BALANCE                              
OFFACC   DS    PL8                 OFFICE/ACCOUNT BALANCE                       
CONBAL   DS    PL8                 CONTRA BALANCE                               
OFFCON   DS    PL8                 OFFICE/CONTRA BALANCE                        
*                                                                               
OFAOUT   DS    PL8                 OFFICE ACCOUNT (OUT OF BALANCE)              
CONOUT   DS    PL8                 CONTRA (OUT OF BALANCE)                      
OFCOUT   DS    PL8                 OFFICE CONTRA (OUT OF BALANCE)               
*                                                                               
OFSNUM   DS    XL2                 NUMBER IN OFFICE SUMMARY                     
CASNUM   DS    XL2                           CONTRA ACCOUNT SUMMARY             
CMSNUM   DS    XL2                           CONTRA MONTHLY SUMMARY             
*                                                                               
         DS    0D                                                               
BUFREC   DS    CL(BUFLNQ)          BUFFERIN I/O                                 
BUFRECX  DS    CL(BUFLNQ)          BUFFERIN I/O                                 
*                                                                               
LASTCON  DS    CL(L'BUFCON)                                                     
ACASNTRY DS    A                                                                
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER BUFFERIN RECORD                                      *         
***********************************************************************         
BUFD     DSECT                                                                  
BUFKEY   DS    0XL20                                                            
BUFTYP   DS    XL1                 RECORD TYPE                                  
BUFTOFF  EQU   1                    OFFICE SUMMARY                              
BUFTCON  EQU   2                    CONTRA SUMMARY                              
BUFTMON  EQU   3                    MONTH SUMMARY                               
BUFCON   DS    CL15                CONTRA ACCOUNT                               
BUFOFF   DS    CL2                 OFFICE                                       
         ORG   BUFOFF                                                           
BUFMOS   DS    XL2                 MONTH- START                                 
BUFEND   DS    XL2                      - END                                   
*                                                                               
BUFBBF   DS    PL8                 BALANCE FORWARD                              
BUFBDR   DS    PL8                         DEBITS                               
BUFBCR   DS    PL8                         CREDITS                              
         ORG   BUFBDR                                                           
BUFNDR   DS    PL8                 NON-0FFICE DEBITS                            
BUFNCR   DS    PL8                            CREDITS                           
BUFHDR   DS    PL8                 HISTORY DEBITS                               
BUFHCR   DS    PL8                         CREDITS                              
BUFNUM   EQU   (*-BUFBBF)/L'BUFBBF                                              
BUFLNQ   EQU   *-BUFKEY                                                         
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
PLD      DSECT                                                                  
PLCL     DS    XL1                 LEFT COLUMN                                  
PLOS     DS    0X                  OFFICE SUMMARY                               
PLOSOFF  DS    CL10                OFFICE                                       
PLOSC1   DS    XL1                                                              
PLOSFWD  DS    CL17                B/FRWD                                       
PLOSC2   DS    XL1                                                              
PLOSTDR  DS    CL17                TRANSACTION DEBITS                           
PLOSC3   DS    XL1                                                              
PLOSTCR  DS    CL17                TRANSACTION CREDITS                          
PLOSC4   DS    XL1                                                              
PLOSTBL  DS    CL17                TRANSACTION BALANCE                          
PLOSLNQ  EQU   *-PLOSOFF                                                        
PLOSC5   DS    XL1                                                              
PLOSBDR  DS    CL17                BUCKET CREDITS                               
PLOSC6   DS    XL1                                                              
PLOSBCR  DS    CL17                BUCKET DEBITS                                
PLOSC7   DS    XL1                                                              
PLOSBBL  DS    CL17                BUCKET BALANCE                               
PLOSC8   DS    XL1                                                              
PLOSOUT  DS    CL17                OUT OF BALANCE                               
PLCR     DS    XL1                 RIGHT                                        
         ORG   PLOS                                                             
PLCS     DS    0X                  CONTRA SUMMARY                               
PLCSCON  DS    CL14                CONTRA                                       
PLCSC1   DS    XL1                                                              
PLCSOFF  DS    CL6                 OFFICE                                       
PLCSC2   DS    XL1                                                              
         ORG   PLOS                                                             
PLMS     DS    0X                  MONTH SUMMARY                                
PLMSCON  DS    CL14                CONTRA                                       
PLMSC1   DS    XL1                                                              
PLMSMON  DS    CL13                MONTH                                        
PLMSC2   DS    XL1                                                              
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* DSECTS TO COVER SUMMARY DIFFERENCES                                 *         
***********************************************************************         
OFSUMD   DSECT                     OFFICE SUMMARY                               
OFSOFF   DS    CL2                 OFFICE                                       
OFSAMT   DS    PL8                 AMOUNT OF DISCREPANCY                        
OFSLNQ   EQU   *-OFSUMD                                                         
*                                                                               
*                                                                               
CASUMD   DSECT                     CONTRA SUMMARY                               
CASCAC   DS    CL15                CONTRA ACCOUNT                               
CASAMT   DS    PL8                 AMOUNT OF DISCREPANCY                        
CASTAMT  DS    PL8                 TOTAL OF MONTHLY DISCREPANCIES               
CASOFF   DS    CL2                 OFFICE                                       
CASLNQ   EQU   *-CASUMD                                                         
*                                                                               
*                                                                               
CMSUMD   DSECT                     CONTRA/MONTHLY SUMMARY                       
CMSCAC   DS    CL15                CONTRA ACCOUNT                               
CMSMOS   DS    XL4                 YEAR/MONTH START-END                         
         ORG   CMSMOS+2                                                         
CMSMOSE  DS    XL2                                                              
CMSAMT   DS    PL8                 NET (OVER/UNDER)                             
CMSDR    DS    PL8                 DEBIT AMOUNT TO CORRECT                      
CMSCR    DS    PL8                 CREDIT AMOUNT TO CORRECT                     
CMSLNQ   EQU   *-CMSUMD                                                         
         EJECT                                                                  
* ACBIGPRNTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*  ACGENMODES                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*  ACREPWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
* DDBUFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPOC02 01/17/13'                                      
         END                                                                    
