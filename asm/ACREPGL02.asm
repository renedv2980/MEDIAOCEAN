*          DATA SET ACREPGL02  AT LEVEL 014 AS OF 08/29/11                      
*PHASE ACGL02A                                                                  
*INCLUDE BINSR31                                                                
*INCLUDE BUFFERIN                                                               
*INCLUDE ACRECTYP                                                               
*INCLUDE ACGLKEY                                                                
*INCLUDE ACGLPOST                                                               
*INCLUDE ACADDBUK                                                               
*INCLUDE PERVERT                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
ACGL02   TITLE 'Create history GL records for 1 character office'               
         PRINT NOGEN                                                            
***********************************************************************         
*   QOPT1 = Delete all existing GLBRECS for company               X/O *         
*   QOPT2 = Seed file with GLDELs, items on or after CPYGLMOA     S   *         
*   QOPT3 = Update CPYEL with GLMOA date and NEWGL bit            O/D *         
*   QOPT4 = Dump some GLU or Seeds (Opt2)                         G   *         
*   QOPT5 = Write records directly to file, instead of dump/load  W   *         
*   QOPT6 = Generate balanceing information                     B/D/X *         
*   QOPT7 = Show all accounts                                     A   *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*   For 1 character offices create new GL bucket records with office  *         
*   in key for filtering by GL office                                 *         
***********************************************************************         
         USING GLOBALS,R9                                                       
         USING ACGLD,RA            RA=A(WORK AREA saved area)                   
         USING ACWORKD,RC                                                       
ACGL02   CSECT                                                                  
         NMOD1 0,**GLOF**,CLEAR=Y                                               
         L     RC,0(,R1)                                                        
         LA    RA,SPACEND                                                       
         BASR  R9,0                                                             
         AHI   R9,GLOBALS-*        Resolve GLOBALS addressing                   
                                                                                
         CLI   MODE,RUNFRST                                                     
         JE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         JE    REQF                                                             
         CLI   MODE,RUNLAST                                                     
         JE    RUNL                                                             
                                                                                
EXIT     XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         USING COMFACSD,R2                                                      
RUNF     DS    0H                                                               
         L     R2,ADCOMFAC                                                      
         MVI   PKZERO,X'0C'                                                     
         MVI   PKONE,X'1C'                                                      
         MVI   DUMP#,0                                                          
         MVI   FIRSTX,YES          First time in                                
         ZAP   COUNT,PKZERO                                                     
         MVC   HELLO,CHELLO                                                     
         L     RE,=A(IOCPY)                                                     
         ST    RE,AIOCPY                                                        
         L     RE,=A(IOLDG)                                                     
         ST    RE,AIOLDG                                                        
         DROP  R2                                                               
                                                                                
         USING MASTD,R6                                                         
         USING BOXD,R2                                                          
         L     R6,ADMASTC                                                       
         MVC   UPSI,MCUPSI                                                      
         L     R2,MCBXAREA                                                      
         LA    R1,132                                                           
         ST    R1,BOXWIDTH                                                      
         DROP  R2,R6               BOXD, MASTD                                  
                                                                                
         GOTOR DATCON,DMCB,(5,0),ETODAY                                         
         GOTOR DATCON,DMCB,ETODAY,(1,PTODAY)                                    
         L     R0,=A(MAX#OFFS*2*K+16)                                           
         GETMAIN R,LV=(0)          LOW CORE STORAGE                             
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                                                             
         MVC   0(16,R1),=CL16'*REC*REC*REC*REC'                                 
         AHI   R1,16                                                            
         ST    R1,AGLRECS          Records                                      
         BRAS  RE,CPYLIST                                                       
                                                                                
         GOTOR BUFFRIN,DMCB,('BUFFAINI',KEYBUFF),BUFFREC,ADCOMFAC               
         JE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* Read through file for company                                                 
***********************************************************************         
REQF     DS    0H                                                               
         MVI   FCRDACC,NO                                                       
         MVI   FCRDTRNS,NO                                                      
         LHI   R0,4000             # of accounts                                
         CLI   QOPT6,BALB          Balance basic                                
         JE    REQF001                                                          
         LHI   R0,2000             # of accounts                                
         MHI   R0,12*5*10          x 12 months x 5 years x # of offices         
         CLI   QOPT6,BALD          Balance detail                               
         JE    *+8                                                              
         CLI   QOPT6,BALX          Extra balance detail                         
         JNE   REQF001A                                                         
                                                                                
REQF001  ST    R0,MAX#ACCS                                                      
         MHI   R0,ACBLNQ                                                        
         AHI   R0,16                                                            
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         SAM31                                                                  
         MVC   0(16,R1),=CL16'*BAL*BAL*BAL*BAL'                                 
         AHI   R1,16                                                            
         ST    R1,AGLBAL           Balancing Records                            
         SAM24                                                                  
                                                                                
REQF001A MVC   IOKEY,SPACES                                                     
         MVI   IOKEY,X'3F'         Start reading records                        
         CLI   QCOMPANY,X'FF'                                                   
         JNE   *+6                                                              
         DC    H'00'               Don't allow anymore                          
         MVC   IOKEY(1),QCOMPANY                                                
         XC    MOASTR,MOASTR                                                    
         MVC   MOAEND,=X'FFFF'                                                  
         CLC   QMOSSTRT,SPACES                                                  
         JNH   REQF002                                                          
         MVC   WORK(4),QMOSSTRT                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTOR DATCON,DMCB,WORK,(1,WORK+6)                                      
         MVC   MOASTR,WORK+6                                                    
                                                                                
REQF002  CLI   QOPT3,TURNDTE                                                    
         JE    *+8                                                              
         CLI   QOPT3,TURNON                                                     
         JNE   REQF002A                                                         
         MVC   GLMOA,MOASTR        This is the company new GL start             
         CLC   GLMOA,SPACES                                                     
         JNL   *+6                                                              
         DC    H'00'               You need QMOS with this option               
                                                                                
REQF002A CLC   QMOSEND,SPACES                                                   
         JNH   REQF003                                                          
         MVC   WORK(4),QMOSEND                                                  
         MVC   WORK+4(2),=C'01'                                                 
         GOTOR DATCON,DMCB,WORK,(1,WORK+6)                                      
         MVC   MOAEND,WORK+6                                                    
                                                                                
REQF003  GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,IOKEY,LASTKEY                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING ACTRECD,LASTKEY                                                  
REQF010  LA    RF,ACCMST                                                        
         TM    ACTKSTAT,TRNSARCH                                                
         BZ    *+8                                                              
         LA    RF,ACCARC                                                        
         GOTOR DATAMGR,DMCB,GETREC,(RF),ACTKDA,DAREC,IOWORK                     
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         BRAS  RE,DMXREC                                                        
                                                                                
         GOTOR DATAMGR,DMCB,DMKEY,ACCDIR,IOKEY,IOKEY                            
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLC   IOKEY,LASTKEY       Did it change ?                              
         JE    REQF020             No                                           
         GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,IOKEY,IOKEY                           
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
REQF020  GOTOR DATAMGR,DMCB,DMRSEQ,ACCDIR,IOKEY,LASTKEY                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   QCOMPANY,X'FF'                                                   
         JE    REQF022                                                          
         CLC   QCOMPANY,ACTKCPY                                                 
         JE    REQF010                                                          
         J     EXIT                                                             
                                                                                
REQF022  CLI   ACTKCPY,X'FE'                                                    
         JL    REQF010                                                          
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* END OF INPUT FILE                                                   *         
***********************************************************************         
RUNL     BRAS  RE,BINPUT           No, so put to BUFFERIN                       
         BRAS  RE,SHOWBAL                                                       
         BRAS  RE,TAPEPUT                                                       
         GOTOR BUFFRIN,DMCB,('BUFFACLO',KEYBUFF),BUFFREC,ADCOMFAC               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING ACCMD,R2                                                         
RUNL20   LA    R0,ACCM#OF                                                       
         L     R2,=A(ACCMTAB)                                                   
RUNL30   MVC   P+1(L'ACCMMSG),ACCMMSG                                           
         ICM   RE,15,ACCMBUK                                                    
         ZAP   DUB,0(8,RE)                                                      
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  P+L'ACCMMSG(14),DUB                                              
         GOTOR ACREPORT                                                         
         AHI   R2,ACCMLNQ                                                       
         BRCT  R0,RUNL30                                                        
         DROP  R2                                                               
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS A RECORD                                                    *         
***********************************************************************         
                                                                                
DMXREC   NTR1                                                                   
         L     R2,DAREC                                                         
         GOTOR VRECTYPE,DMCB,(C'D',(R2))                                        
         MVC   RECTYPE,0(R1)                                                    
         MVC   COMPANY,1(R1)                                                    
         BRAS  RE,ISCPY                                                         
         JNE   EXIT                Not intrested in this company then           
                                                                                
***********************************************************************         
* Create records for tape                                                       
***********************************************************************         
         CLI   RECTYPE,ACRTCPY     Test company record                          
         JNE   *+8                                                              
         BRAS  RE,CPYRTE                                                        
                                                                                
         CLI   RECTYPE,ACRTLDG     Test ledger  record                          
         JNE   *+8                                                              
         BRAS  RE,LDGRTE                                                        
                                                                                
         CLI   RECTYPE,ACRTACTH    High level account?                          
         JNE   *+8                                                              
         BRAS  RE,ACCTRTE                                                       
                                                                                
         CLI   RECTYPE,ACRTACTL    Low level account?                           
         JNE   *+8                                                              
         BRAS  RE,ACCTRTE                                                       
                                                                                
         TM    UPSI,UPSIAOLV       Process at account/office level              
         JO    DMXREC12                                                         
         CLI   RECTYPE,ACRTCHDH    TEST Account/Contra header record            
         JE    *+8                                                              
DMXREC12 CLI   RECTYPE,ACRTCAC     TEST Account/Contra bucket record            
         JNE   *+8                                                              
         BRAS  RE,ACRCRTE                                                       
                                                                                
*&&UK*&& CLI   RECTYPE,ACRTTRNA    ARCHIVE TRANSACTION                          
*&&UK*&& JE    *+8                                                              
         CLI   RECTYPE,ACRTTRN     TEST TRANSACTION RECORD                      
         JNE   *+8                                                              
         BRAS  RE,TRNSRTE                                                       
                                                                                
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Collect list of companies for Accfile to create records for                   
***********************************************************************         
CPYLIST  NTR1                                                                   
*------------------------------*                                                
* Get current key to for reset *                                                
*------------------------------*                                                
*        GOTOR DATAMGR,DMCB,DMKEY,ACCDIR,LASTKEY,LASTKEY                        
*        JE    *+6                                                              
*        DC    H'0'                                                             
                                                                                
         CLI   QOPT3,TURNDTE                                                    
         JE    EXIT                                                             
         CLI   QOPT3,TURNON                                                     
         JE    EXIT                                                             
         USING CPYRECD,R4                                                       
         LA    R4,IOKEY                                                         
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKEY(2),=X'3FFF'  Start the search for companies               
                                                                                
CPYL050  GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,CPYRECD,CPYRECD                       
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   CPYKCPY,X'FE'       End of file                                  
         JNL   CPYLXIT                                                          
         CLI   CPYKCPY+1,X'40'     Company record ?                             
         JE    *+6                 Yes                                          
         DC    H'00'               Should always get next company               
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,CPYKDA,IO,WORK                        
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING CPYELD,R3                                                        
         LA    R4,IO                                                            
         LA    R3,CPYRFST          First element                                
CPYL060  CLI   0(R3),EOR           End of record ?                              
         JNE   *+6                                                              
         DC    H'00'               Must find company element                    
         CLI   0(R3),CPYELQ        X'10'                                        
         JE    CPYL100                                                          
         LLC   RF,1(,R3)                                                        
         AR    R3,R1                                                            
         J     CPYL060                                                          
                                                                                
CPYL100  TM    CPYSTAT4,CPYSOFF2   New offices                                  
         JO    CPYL200             Get next company                             
         CLI   CPYLN,CPYLN4Q                                                    
         JL    CPYL200             Had to be at least this length               
         OC    CPYGLMOA,CPYGLMOA                                                
         JZ    CPYL200                                                          
         LLC   R6,#OFCPYS                                                       
         AHI   R6,1                                                             
         STC   R6,#OFCPYS                                                       
         CHI   R6,MAX#CPYS                                                      
         JNH   *+6                                                              
         DC    H'00'               Too many, new bigger table                   
                                                                                
         USING CPYTABD,R6                                                       
         BCTR  R6,0                                                             
         MHI   R6,CPYTLNQ                                                       
         A     R6,=A(CPYTABS)                                                   
         MVC   CPYCODE,CPYKCPY                                                  
         MVC   CPYMOA,CPYGLMOA                                                  
         DROP  R6                                                               
                                                                                
CPYL200  LA    R4,IOKEY                                                         
         MVC   CPYKCPY+1(L'CPYKEY-1),SPACES                                     
         MVI   CPYKCPY+1,X'FF'                                                  
         J     CPYL050                                                          
         DROP  R3,R4                                                            
                                                                                
*-------------------------------------------*                                   
* Reset key so that things process as usual *                                   
*-------------------------------------------*                                   
*PYLXIT  GOTOR CDATAMGR,DMCB,DMRDHI,ACCDIR,LASTKEY,IOKEY                        
*        JE    *+6                                                              
*        DC    H'0'                                                             
*        CLC   IOKEY(L'ACTKEY),LASTKEY                                          
*        JE    EXIT                                                             
CPYLXIT  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Is this company worth processing                                              
***********************************************************************         
ISCPY    CLI   QOPT3,TURNDTE                                                    
         JE    ISCPY22             Don't bother                                 
         CLI   QOPT3,TURNON                                                     
         JE    ISCPY22             Don't bother                                 
         LLC   R1,#OFCPYS          Number of companies to convert               
         LTR   R1,R1                                                            
         JNZ   *+6                                                              
         DC    H'00'               Stop the madness                             
                                                                                
         USING CPYTABD,RF                                                       
         LA    RF,CPYTABS                                                       
ISCPY10  CLC   COMPANY,CPYCODE                                                  
         JE    ISCPY20                                                          
         LA    RF,CPYTLNQ(,RF)     Next entry                                   
         BRCT  R1,ISCPY10                                                       
         LTR   RE,RE                                                            
         BR    RE                                                               
                                                                                
ISCPY20  MVC   GLMOA,CPYMOA        Set date                                     
ISCPY22  CR    RE,RE                                                            
         BR    RE                                                               
         DROP  RF                                                               
         EJECT ,                                                                
***********************************************************************         
* Put all bin records (GLBRECDs) to TAPE, check for data first                  
***********************************************************************         
         USING GLBRECD,R2                                                       
BINPUT   NTR1                                                                   
         L     R8,AGLRECS                                                       
         ICM   R6,15,#OFKEYS       Any records to put ?                         
         JZ    BINPUTX             No                                           
         XC    ELEM43,ELEM43                                                    
BINPUT10 LA    R2,4(,R8)           Point past tape record length                
         BRAS  RE,CK4AMNT                                                       
         JE    BINPUT60            All zeros so skip                            
         CLC   GLBKGOFF,SPACES     Is this a non-office one?                    
         JH    BINPUT20                                                         
         BRAS  RE,RCSILE           Reconsile X'45' with X'55'                   
         BRAS  RE,CK4AMNT                                                       
         JE    BINPUT60            All zeros so skip                            
                                                                                
BINPUT20 CLI   NEWOFF,YES                                                       
         JE    *+10                                                             
         AP    OLDBUKS,PKONE                                                    
         GOTOR BUFFRIN,DMCB,('BUFFAPUT',KEYBUFF),(R8),ADCOMFAC                  
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   GLBKGOFF,SPACES     If a no office record                        
         JNH   BINPUT55            then dump                                    
                                                                                
BINPUT22 TM    UPSI,UPSIDBBF       Dump records to examine                      
         JZ    BINPUT60                                                         
*        CP    DUMPCNT,DUMPMAX                                                  
*        JH    BINPUT60                                                         
                                                                                
BINPUT55 AP    DUMPCNT,PKONE                                                    
         LA    RF,GLBRFST-GLBKEY+4                                              
         GOTOR PRNTBL,DMCB,=C'BBF Office',(R8),C'DUMP',(RF),=C'1R',    +        
               (C'P',VPRINT)                                                    
                                                                                
BINPUT60 AHI   R8,L'BINREC         Next record                                  
         BRCT  R6,BINPUT10         # of keys left to put                        
         XC    #OFKEYS,#OFKEYS     All done, so set to zero                     
         DROP  R2                                                               
                                                                                
BINPUTX  J     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* See if record that we want to put to tape has any numbers                     
*    If yes then exit not equal                                                 
*    If no  then exit equal                                                     
***********************************************************************         
         USING GLBRECD,R2                                                       
         USING BUKELD,R3                                                        
CK4AMNT  NTR1                                                                   
         LA    R3,GLBRFST                                                       
         SR    RF,RF                                                            
CK4AMT12 CLI   0(R3),EOR           End of record                                
         JE    CK4AMNTX                                                         
         CLI   0(R3),BUKELQ        X'45', bucket element                        
         JNE   CK4AMT14                                                         
         CP    BUKDR,PKZERO        Check for activity                           
         JNZ   CK4AMNTX                                                         
         CP    BUKCR,PKZERO        Check for activity                           
         JNZ   CK4AMNTX                                                         
                                                                                
         USING PBKELD,R3                                                        
CK4AMT14 CLI   0(R3),PBKELQ        X'55'                                        
         JNE   CK4AMT16                                                         
         CP    PBKDR,PKZERO                                                     
         JNZ   CK4AMNTX                                                         
         CP    PBKCR,PKZERO                                                     
         JNZ   CK4AMNTX                                                         
                                                                                
CK4AMT16 IC    RF,1(,R3)                                                        
         AR    R3,RF                                                            
         J     CK4AMT12                                                         
                                                                                
CK4AMNTX J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
* Reconsile any buckets that should go into PBKEL element                       
***********************************************************************         
         USING GLBRECD,R2                                                       
         USING PBKELD,R3                                                        
         USING BUKELD,R4                                                        
RCSILE   NTR1                                                                   
         LA    R3,GLBRFST                                                       
         SR    R4,R4                                                            
RCSILE10 CLI   0(R3),EOR                                                        
         JE    RCSILEX                                                          
         CLI   0(R3),BUKELQ        X'45'                                        
         JNE   RCSILE20                                                         
         LTR   R4,R4                                                            
         JNZ   RCSILE20                                                         
         LR    R4,R3               Save address of first bucket                 
                                                                                
RCSILE20 CLI   0(R3),PBKELQ        X'55'                                        
         JE    RCSILE40                                                         
         LLC   RF,1(,R3)                                                        
         AR    R3,RF                                                            
         J     RCSILE10                                                         
                                                                                
RCSILE40 LTR   R4,R4                                                            
         JNZ   *+6                                                              
         DC    H'00'                                                            
                                                                                
RCSILE50 CLI   0(R4),EOR                                                        
         JE    RCSILEX                                                          
         CLI   0(R4),BUKELQ        X'45' bucket element                         
         JNE   RCSILE60                                                         
         CLC   BUKMOS,PBKHI                                                     
         JH    RCSILEX             Done                                         
         AP    PBKDR,BUKDR                                                      
         AP    PBKCR,BUKCR                                                      
         ZAP   BUKDR,PKZERO                                                     
         ZAP   BUKCR,PKZERO                                                     
                                                                                
RCSILE60 LLC   RF,1(,R4)                                                        
         AR    R4,RF                                                            
         J     RCSILE50                                                         
                                                                                
RCSILEX  J     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT ,                                                                
***********************************************************************         
* Save off record in area for GLPOST (maybe)                                    
***********************************************************************         
SAVE_IT  NTR1                                                                   
         ICM   RE,15,0(R1)            Where to save                             
         ICM   R0,15,4(R1)            What  to save                             
         SR    R1,R1                                                            
         LR    RF,R0                                                            
         ICM   R1,3,ACCRLEN-ACCRECD(RF)                                         
         LA    RF,2*K                                                           
         MVCL  RE,R0                                                            
         J     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* Process company record                                              *         
***********************************************************************         
         USING CPYRECD,R2                                                       
CPYRTE   NTR1                                                                   
         MVI   NEWOFF,NO                                                        
         LA    R3,CPYRFST                                                       
                                                                                
         SR    R0,R0                                                            
CPYRTE10 CLI   0(R3),EOR           TEST END OF RECORD                           
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),CPYELQ        X'10'                                        
         JE    CPYRTE20                                                         
         IC    R0,1(,R3)                                                        
         AR    R3,R0                                                            
         J     CPYRTE10                                                         
                                                                                
         USING CPYELD,R3                                                        
CPYRTE20 MVC   ALPHA,CPYALPHA                                                   
         MVC   CLOGO,CPYLOGO                                                    
         TM    CPYSTAT4,CPYSOFF2   SET OFFICE INDICATORS                        
         JZ    *+8                                                              
         MVI   NEWOFF,YES                                                       
                                                                                
         CLI   QOPT3,TURNDTE                                                    
         JE    *+8                                                              
         CLI   QOPT3,TURNON                                                     
         JNE   CPYRTE50                                                         
         CLI   0(R3),CPYELQ        X'10'                                        
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   CPYLN,CPYLN4Q                                                    
         JE    CPYRTE30                                                         
         XC    ELEM,ELEM                                                        
         LLC   RF,CPYLN                                                         
         BCTR  RF,0                                                             
         MVC   ELEM(0),CPYEL                                                    
         EX    RF,*-6                                                           
         MVI   CPYEL,X'FF'                                                      
         GOTOR HELLO,DMCB,(C'D',ACCMST),(X'FF',(R2)),0,0                        
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         LA    R3,ELEM                                                          
         MVI   CPYLN,CPYLN4Q       New length                                   
CPYRTE30 MVC   CPYGLMOA,GLMOA                                                   
         CLI   QOPT3,TURNDTE       Not bit only date?                           
         JE    CPYRTE32                                                         
*&&US*&& OI    CPYSTAT8,CPYNEWGL                                                
*&&UK*&& OI    CPYSTATC,CPYNEWGL                                                
CPYRTE32 LA    RE,ELEM                                                          
         CR    RE,R3                                                            
         JNE   CPYRTE40                                                         
         GOTOR HELLO,DMCB,(C'P',ACCMST),(R2),(R3),0,0                           
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
CPYRTE40 CLI   RCWRITE,YES                                                      
         JNE   CPYRTE50                                                         
         GOTOR DATAMGR,DMCB,(X'80',PUTREC),ACCMST,ACTKDA,DAREC,IOWORK           
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
CPYRTE50 DS    0H                                                               
         GOTOR SAVE_IT,DMCB,AIOCPY,(R2)                                         
         J     EXIT                                                             
         DROP  R2,R3               CPYRECD, CPYELD                              
         EJECT                                                                  
***********************************************************************         
* Process ledger record                                               *         
***********************************************************************         
         USING LDGRECD,R2                                                       
LDGRTE   NTR1                                                                   
         GOTOR SAVE_IT,DMCB,AIOLDG,(R2)                                         
         LA    R2,LDGRFST                                                       
         SR    R0,R0                                                            
         XC    LEVELS(LEVLNQ),LEVELS                                            
         XC    LDGTLVS,LDGTLVS                                                  
         MVI   LDGT#LVS,0                                                       
                                                                                
LDGRTE10 CLI   0(R2),EOR           TEST END OF RECORD                           
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),ACLELQ        X'16'                                        
         JE    LDGRTE20                                                         
         IC    R0,1(,R2)                                                        
         AR    R2,R0                                                            
         J     LDGRTE10                                                         
                                                                                
         USING ACLELD,R2                                                        
LDGRTE20 SR    R1,R1                                                            
         IC    R1,ACLLN            R1 = LENGTH OF ELEMENT                       
         SHI   R1,ACLLN1Q+1        SUBTRACT OVERHEAD                            
         EXMVC R1,LEVELS,ACLVALS                                                
         DROP  R2                                                               
                                                                                
* example - CONVERT 1,3,5,12 -> 1,2,2,7                                         
                                                                                
         MVI   LDGT#LVS,1                                                       
         ICM   RF,1,LEVB                                                        
         JZ    LDGRTE40                                                         
         ICM   R1,1,LEVA                                                        
         JZ    LDGRTE40                                                         
         SR    RF,R1                                                            
         STC   RF,LDGTLV2L         Length of level 2                            
                                                                                
         MVI   LDGT#LVS,2                                                       
         ICM   RF,1,LEVC                                                        
         JZ    LDGRTE40                                                         
         ICM   R1,1,LEVB                                                        
         JZ    LDGRTE40                                                         
         SR    RF,R1                                                            
         STC   RF,LDGTLV3L         Length of level 3                            
                                                                                
         MVI   LDGT#LVS,3                                                       
         ICM   RF,1,LEVD                                                        
         JZ    LDGRTE40                                                         
         ICM   R1,1,LEVC                                                        
         JZ    LDGRTE40                                                         
         SR    RF,R1                                                            
         STC   RF,LDGTLV4L         Length of level 4                            
         MVI   LDGT#LVS,4                                                       
                                                                                
LDGRTE40 J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Save off name of lowest level account for unit S only                         
*  Convert to X'43' element                                                     
***********************************************************************         
         USING ACTRECD,R2                                                       
         USING CACELD,R4                                                        
ACCTRTE  NTR1                                                                   
         CLI   ACTKUNT,C'S'        Process unit S only                          
         JNE   ACCTXIT                                                          
         BRAS  RE,WHICHLV                                                       
         CLI   RECTYPE,ACRTACTH    High level account?                          
         JE    EXIT                Done for now                                 
         CLI   ACTKLDG,C'J'        Is it production ?                           
         JNE   *+8                                                              
         L     R2,AIOLVB           For production use level 2 instead           
                                                                                
         LA    R4,ACCT43           Store name to use later                      
         XC    ACCT43,ACCT43       Clear                                        
         LA    R3,ACTRFST          A(First element)                             
ACCT010  CLI   0(R3),EOR           End of record ?                              
         JE    ACCTXIT             Yes                                          
         CLI   0(R3),NAMELQ        X'20', name element?                         
         JE    ACCT100                                                          
         LLC   RF,1(,R3)                                                        
         AR    R3,RF                                                            
         J     ACCT010                                                          
                                                                                
         USING NAMELD,R3                                                        
ACCT100  LLC   R1,NAMLN            Length of name                               
         SHI   R1,NAMLN1Q+1                                                     
         JM    ACCTXIT             Should not happend                           
         MVI   CACEL,CACELQ        X'43'                                        
         MVC   CACCNT,ACTKCULA                                                  
         MVC   CACNAME(0),NAMEREC                                               
         EX    R1,*-6                                                           
         AHI   R1,CACLN1Q+1        Add one back for EX instr                    
         STC   R1,CACLN                                                         
                                                                                
ACCTXIT  J     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT ,                                                                
***********************************************************************         
* Figure out which level to save off in io area for GLPOST                      
***********************************************************************         
         USING ACTRECD,R2                                                       
WHICHLV  NTR1                                                                   
         L     R5,=A(IOLVA)                                                     
         LHI   RF,11                                                            
         LLC   RE,LEVA                                                          
         SR    RF,RE                                                            
         BNP   WHICH12                                                          
         LA    RE,ACTKACT(RE)                                                   
         CLC   0(0,RE),SPACES      Level 1                                      
         EX    RF,*-6                                                           
         JNE   WHICH20                                                          
WHICH12  ST    R5,AIOLVA                                                        
         XC    AIOLVB(AIOEND-AIOLVB),AIOLVB                                     
         J     WHICH90                                                          
                                                                                
WHICH20  L     R5,=A(IOLVB)                                                     
         LHI   RF,11                                                            
         LLC   RE,LEVB                                                          
         SR    RF,RE                                                            
         BNP   WHICH22                                                          
         LA    RE,ACTKACT(RE)                                                   
         CLC   0(0,RE),SPACES      Level 2                                      
         EX    RF,*-6                                                           
         JNE   WHICH30                                                          
WHICH22  ST    R5,AIOLVB                                                        
         XC    AIOLVC(AIOEND-AIOLVC),AIOLVC                                     
         J     WHICH90                                                          
                                                                                
WHICH30  L     R5,=A(IOLVC)                                                     
         LHI   RF,11                                                            
         LLC   RE,LEVC                                                          
         SR    RF,RE                                                            
         BNP   WHICH32                                                          
         LA    RE,ACTKACT(RE)                                                   
         CLC   0(0,RE),SPACES      Level 3                                      
         EX    RF,*-6                                                           
         JNE   WHICH40                                                          
WHICH32  ST    R5,AIOLVC                                                        
         XC    AIOLVD(AIOEND-AIOLVD),AIOLVD                                     
         J     WHICH90                                                          
                                                                                
WHICH40  L     R5,=A(IOLVD)        Level 4                                      
         ST    R5,AIOLVD                                                        
                                                                                
WHICH90  GOTOR SAVE_IT,DMCB,(R5),(R2)                                           
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* Process a account/contra records                                              
* Use account/contra bucket records as first entry in bin table with            
*  1) Save of contra header name                                                
*  2) Move account/contra bucket record into bin table                          
*  3) Convert to GLBRECD                                                        
*  4) Add contra header name to record                                          
***********************************************************************         
         USING CACRECD,R2                                                       
         USING CACELD,R3                                                        
ACRCRTE  NTR1                                                                   
*        CLI   QOPT1,NO            New records only?                            
*        JE    ACRCXIT                                                          
         CLI   NEWOFF,YES                                                       
         JE    ACRCXIT             Two char, only for one char                  
         CLC   =C'GB',CACKUNT                                                   
         JE    ACRC010                                                          
         CLC   =C'GP',CACKUNT                                                   
         JNE   ACRCXIT                                                          
                                                                                
ACRC010  LA    R1,CACKCULC-CACKEY-1                                             
         TM    UPSI,UPSIAOLV       Forget contra ?                              
         JO    *+8                                                              
         LA    R1,CACKSPAC-CACKEY-1                                             
         EXCLC R1,LASTCAC,CACKCPY  Same account/contra ?                        
         JE    *+8                 Yes                                          
         BRAS  RE,BINPUT           No, so put to tape                           
         MVC   LASTCAC,CACKCPY     Save off key                                 
         LA    R3,CACRFST                                                       
         CLI   CACEL,CACELQ        X'43'                                        
         JNE   ACRC100                                                          
         LLC   R1,CACLN                                                         
         BCTR  R1,0                                                             
         MVC   ELEM43(0),0(R3)     Save off name element to combine             
         EX    R1,*-6                                                           
         J     ACRCXIT                                                          
         DROP  R3                                                               
                                                                                
         USING BUKELD,R3                                                        
ACRC100  CLI   BUKEL,BUKELQ        X'45'                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R3                                                               
                                                                                
         USING GLBRECD,R4                                                       
         L     R4,AGLRECS          A(No office account/contra GLBRECD)          
         AHI   R4,4                Bump past record length storage              
         OC    #OFKEYS,#OFKEYS     First time adding ?                          
         JNZ   ACRC200             Same account different contra                
         LR    RF,R4                                                            
         SHI   RF,4                                                             
         XC    0(100,RF),0(RF)     Clear for new record                         
         MVC   GLBKEY,SPACES       Build key                                    
         MVI   GLBKTYP,GLBKTYPQ    X'28'                                        
         MVC   GLBKCPY,CACKCPY                                                  
         MVC   GLBKGLA,CACKLDG     Ledger/Acount                                
         MVC   GLBKSULA,CACKULC                                                 
         CLC   CACKCULC(3),SPACES  Production ?                                 
         JNE   *+10                No                                           
         MVC   GLBKGSJ,CACKCULC    Yes                                          
         LA    R1,1                                                             
         ST    R1,#OFKEYS          Set to first entry                           
         TM    UPSI,UPSIAOLV       Forget contra ?                              
         JO    ACRC200             So don't add name                            
         GOTOR ADDBUK,DMCB,(X'80',GLBRECD),ELEM43,HELLO                         
                                                                                
         USING BUKELD,R3                                                        
ACRC200  LA    R3,CACRFST          Add elements to GLBRECD                      
ACRC210  CLI   0(R3),EOR           End of record                                
         JE    ACRC300                                                          
         CLI   0(R3),BUKELQ        X'45'                                        
         JNE   ACRC220                                                          
         CLC   BUKMOS,GLMOA        Clear buckets after GLMOA                    
         JNL   ACRC220             Only put buckets before GLMOA                
         CP    BUKDR,PKZERO                                                     
         JNE   *+10                                                             
         CP    BUKCR,PKZERO                                                     
         JE    ACRC220             Don't add zero elements                      
         GOTOR ADDBUK,DMCB,(X'80',(R4)),BUKEL,HELLO                             
                                                                                
ACRC220  LLC   RF,1(,R3)                                                        
         AR    R3,RF                                                            
         J     ACRC210                                                          
         DROP  R3                                                               
                                                                                
**********************************                                              
* Process prior bucket and merge *                                              
**********************************                                              
ACRC300  LA    R3,CACRFST          Add elements to GLBRECD                      
ACRC310  CLI   0(R3),EOR           End of record                                
         JE    ACRCXIT                                                          
         CLI   0(R3),PBKELQ        X'55'  Prior bucket                          
         JE    ACRC330                                                          
         LLC   RF,1(,R3)                                                        
         AR    R3,RF                                                            
         J     ACRC310                                                          
                                                                                
ACRC330  LA    R5,GLBRFST                                                       
ACRC340  CLI   0(R5),EOR           GLBRECD end ?                                
         JE    ACRC400                                                          
         CLI   0(R5),PBKELQ        See if it also has a X'55'                   
         JNE   ACRC380                                                          
                                                                                
C        USING PBKELD,R3           Account/Contra                               
G        USING PBKELD,R5           GL record                                    
ACRC350  DS    0H                                                               
         CLC   G.PBKLOW,C.PBKLOW                                                
         JNH   *+10                                                             
         MVC   G.PBKLOW,C.PBKLOW   Save lower of two                            
         CLC   G.PBKHI,C.PBKHI                                                  
         JNL   *+10                                                             
         MVC   G.PBKHI,C.PBKHI                                                  
         AP    G.PBKDR,C.PBKDR                                                  
         AP    G.PBKCR,C.PBKCR                                                  
         J     ACRCXIT                                                          
                                                                                
ACRC380  LLC   RF,1(,R5)                                                        
         AR    R5,RF                                                            
         J     ACRC340                                                          
         DROP  C,G                                                              
*                                                                               
* Add element to record                                                         
*                                                                               
ACRC400  GOTOR HELLO,DMCB,(C'P',ACCMST),(R4),(R3),0,0                           
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
ACRCXIT  J     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* Process a transaction and put to BINTAB as GLBRECD record with                
*  office in the GLBKGOFF                                                       
***********************************************************************         
         USING TRNRECD,R2                                                       
         USING TRNELD,R3                                                        
TRNSRTE  NTR1                                                                   
         LA    R3,TRNRFST                                                       
         CLI   TRNEL,TRNELQ        X'44'                                        
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         TM    TRNRSTAT,TRNSDRFT+TRNSDELT                                       
         JNZ   TRNSXIT                                                          
         CP    TRNAMNT,PKZERO                                                   
         JE    TRNSXIT                                                          
                                                                                
         USING TRSELD,R5                                                        
         LR    R5,R3               R5 = A(TRNELD)                               
TRNS040  CLI   0(R5),EOR           TEST END OF RECORD                           
         JNE   *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   0(R5),TRSELQ        X'60' element                                
         JE    TRNS050                                                          
         LLC   RF,1(,R5)                                                        
         AR    R5,RF                                                            
         J     TRNS040                                                          
                                                                                
         USING GLBRECD,R4                                                       
TRNS050  MVI   DIRECT,NO                                                        
         CLI   NEWOFF,YES                                                       
         JNE   TRNS051                                                          
         CLC   TRSPMOS,GLMOA                                                    
         JL    TRNSXIT             Two char only GL and after                   
                                                                                
TRNS051  CLI   TRNKUNT,C'G'        Unit G                                       
         JNE   TRNS200                                                          
         CLC   TRSPMOS,GLMOA                                                    
         JL    TRNS058             Want only prior to GLMOA                     
         CLI   QOPT6,C' '                                                       
         JNH   TRNS052                                                          
         BRAS  RE,BAD              Balance add                                  
                                                                                
TRNS052  CLI   TRNTYPE,25          Posting made by A25                          
         JNE   TRNS056             Must be via input direct                     
         CLC   TRNBREF,SPACES      Don't want A25 or GLU                        
         JNH   TRNSXIT             Regular 25                                   
         AP    TRNGGLU,PKONE       GLUed, accounted for via unit S              
         LA    RE,GLUGDR                                                        
         TM    TRNSTAT,TRNSDR                                                   
         JO    *+8                                                              
         LA    RE,GLUGCR                                                        
         AP    0(L'GLUSDR,RE),TRNAMNT                                           
         J     TRNSXIT                                                          
                                                                                
TRNS056  MVI   DIRECT,YES                                                       
         AP    DIRECTG,PKONE       Direct posting to Unit G                     
         LA    RE,TOTDDR           On or After GLMOA                            
         TM    TRNSTAT,TRNSDR                                                   
         JO    *+8                                                              
         LA    RE,TOTDCR                                                        
         J     TRNS060                                                          
                                                                                
TRNS058  LA    RE,TOTGDR           Prior to GLMOA                               
         TM    TRNSTAT,TRNSDR                                                   
         JO    *+8                                                              
         LA    RE,TOTGCR                                                        
                                                                                
TRNS060  AP    0(L'TOTSDR,RE),TRNAMNT                                           
                                                                                
         LA    R4,BINREC+4                                                      
         XC    0(GLBRFST-GLBRECD+5,R4),0(R4)                                    
         MVC   GLBKEY,SPACES                                                    
         XC    GLBRSTA,GLBRSTA     Clear status                                 
         MVI   GLBKTYP,GLBKTYPQ    X'28'                                        
         MVC   GLBKCPY,TRNKCPY                                                  
         MVC   GLBKGLA,TRNKLDG    Ledger/Acount                                 
         MVC   GLBKSULA,TRNKULC                                                 
         MVC   GLBKGOFF,TRNOFFC                                                 
         USING BUKELD,R6                                                        
         LA    R6,ELEM                                                          
         MVI   BUKEL,BUKELQ        Create X'45'                                 
         MVI   BUKLN,BUKLNQ                                                     
         MVC   BUKMOS,TRSPMOS      Get posting month                            
         ZAP   BUKDR,PKZERO                                                     
         ZAP   BUKCR,PKZERO                                                     
         LA    R7,BUKDR                                                         
         TM    TRNSTAT,TRNSDR      Debit ?                                      
         JO    *+8                                                              
         LA    R7,BUKCR                                                         
         ZAP   0(L'BUKDR,R7),TRNAMNT                                            
*-----------------------------------------------*                               
* Subtract from no office account/contra record *                               
*-----------------------------------------------*                               
         CLI   DIRECT,YES                                                       
         JNE   TRNS065                                                          
         LA    RF,GLBRFST-GLBKEY+4                                              
         GOTOR PRNTBL,DMCB,=C'Direct',(R4),C'DUMP',(RF),=C'1R',        +        
               (C'P',VPRINT)                                                    
         J     TRNS070                                                          
                                                                                
TRNS065  XI    L'BUKDR-1(R7),X'01'                                              
         L     R5,AGLRECS                                                       
         AHI   R5,4                                                             
         GOTOR ADDBUK,DMCB,(X'80',(R5)),BUKEL,HELLO                             
         XI    L'BUKDR-1(R7),X'01'                                              
*                                                                               
* Take transaction amount and put to X'45' record                               
* Add record to bin table, add amount to record in bin tab                      
*                                                                               
* FYI - Not using BIN31 in 31 bit mode                                          
*                                                                               
TRNS070  GOTOR BIN31,DMCB,BINREC,AGLRECS,#OFKEYS,(1,L'BINREC),         +        
               (4,L'GLBKEY),MAX#OFFS                                            
         MVC   #OFKEYS,8(R1)                                                    
         ICM   R8,15,0(R1)                                                      
         JNZ   *+6                                                              
         DC    H'00'               Table full                                   
         AHI   R8,4                Point to record, not length of               
                                                                                
         TM    0(R1),X'80'         Record not found?                            
         JZ    TRNS080             No record found                              
         GOTOR ADDBUK,DMCB,(X'80',(R8)),ELEM43,HELLO                            
TRNS080  GOTOR ADDBUK,DMCB,(X'80',(R8)),BUKEL,HELLO                             
         J     TRNSXIT                                                          
         EJECT ,                                                                
***********************************************************************         
* Convert Unit S to new records based on new GL data                            
***********************************************************************         
         USING TRSELD,R5                                                        
TRNS200  CLI   TRNKUNT,C'S'        Unit S                                       
         JNE   TRNSXIT                                                          
         CLI   TRNKLDG,C'9'        Don't bother                                 
         JE    TRNSXIT                                                          
         CLC   TRNOFFC,=C'**'      Orders                                       
         JE    TRNSXIT             Don't bother                                 
         CLC   TRSPMOS,GLMOA                                                    
         JL    TRNSXIT             Want only GLMOA and after                    
         TM    TRSSTAT,TRSSGLUP    Updated to GL?                               
         JZ    TRNSXIT                                                          
         OC    TRSUPDT,TRSUPDT                                                  
         JNZ   *+6                                                              
         DC    H'00'               Should have a date                           
                                                                                
         GOTOR DATCON,DMCB,(2,TRSUPDT),(1,GLDATE)                               
         LA    RE,TOTSDR                                                        
         TM    TRNSTAT,TRNSDR                                                   
         JO    *+8                                                              
         LA    RE,TOTSCR                                                        
         AP    0(L'TOTSDR,RE),TRNAMNT                                           
                                                                                
         USING GLDELD,R6                                                        
         LR    R6,R3                                                            
         TM    TRSSTAT,TRSSGLIP    Glued                                        
         JZ    TRNS210                                                          
         AP    TRNSGLU,PKONE                                                    
         LA    RE,GLUSDR                                                        
         TM    TRNSTAT,TRNSDR                                                   
         JO    *+8                                                              
         LA    RE,GLUSCR                                                        
         AP    0(L'GLUSDR,RE),TRNAMNT                                           
                                                                                
TRNS210  CLI   0(R6),EOR           End of record ?                              
         JNE   TRNS216                                                          
         CLI   QOPT2,SEEDFILE                                                   
         JE    TRNS214                                                          
         TM    TRSSTAT,TRSSGLIP    Via INPUT                                    
         JO    *+6                                                              
         DC    H'00'                                                            
                                                                                
TRNS214  BRAS  RE,ADDGLD           Add GLDEL to GLU's after GLMOA               
         JNZ   TRNS220             R6 is returned with address of GLDEL         
         GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,LASTKEY,IOKEY                         
         J     TRNSXIT             R6 was not set, so error                     
                                                                                
TRNS216  CLI   0(R6),GLDELQ        X'63' General ledger details                 
         JNE   TRNS218                                                          
         CLC   GLDATE,GLDDATE      Find current one                             
         JE    TRNS220                                                          
TRNS218  LLC   RF,1(,R6)                                                        
         AR    R6,RF                                                            
         J     TRNS210                                                          
                                                                                
TRNS220  CLI   QOPT6,C' '                                                       
         JNH   *+8                                                              
         BRAS  RE,BAD              Balance add                                  
                                                                                
         GOTOR DATAMGR,DMCB,DMKEY,ACCDIR,LASTKEY,LASTKEY                        
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R4,BUFFKEY                                                       
         XC    BUFFREC(100),BUFFREC                                             
         MVI   BYTE,X'80'          New file structure                           
         CLI   NEWOFF,YES          NEW OFFICE                                   
         JNE   *+8                 No                                           
         OI    BYTE,X'40'          Yes                                          
         GOTOR ACGLKEY,DMCB,(BYTE,TRNRECD),GLDEL,WORK,BUFFKEY,         +        
               (L'SVLDG,SVLDG),ADCOMFAC                                         
                                                                                
         CLC   GLBKGOFF,SPACES                                                  
         JH    *+10                                                             
         MVC   GLBKGOFF,GLDOFFO                                                 
         CLC   GLBKGOFF,SPACES                                                  
         JH    TRNS224                                                          
         MVC   GLBKGOFF,TRNOFFC                                                 
*&&UK                                                                           
         CLI   TRNKLDG,C'J'        Is it production?                            
         JE    TRNS224                                                          
         CLI   NEWOFF,YES          2 char office?                               
         JE    *+8                 Yes - OK                                     
         MVI   GLBKGOFF+1,C' '     Clear 2nd byte for German BT59               
*&&                                                                             
         DROP  R6                                                               
                                                                                
TRNS224  MVC   GLBRLEN,=AL2(GLBRFST-GLBRECD)                                    
         MVI   GLBRFST,EOR                                                      
         MVC   WORK,BUFFREC        Save key                                     
                                                                                
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,LASTKEY,IOKEY                         
         GOTOR BUFFRIN,DMCB,('BUFFAGET',KEYBUFF),BUFFREC,ADCOMFAC               
         JE    TRNS300                                                          
         XC    BUFFREC(100),BUFFREC                                             
         MVC   BUFFREC(L'BUFFKEY+4),WORK     Build this record                  
                                                                                
         USING BUKELD,R6                                                        
TRNS300  LA    R6,ELEM                                                          
         MVI   BUKEL,BUKELQ        X'45'                                        
         MVI   BUKLN,BUKLNQ                                                     
         MVC   BUKMOS,TRSPMOS                                                   
         ZAP   BUKDR,PKZERO                                                     
         ZAP   BUKCR,PKZERO                                                     
         LA    RE,BUKDR                                                         
         TM    TRNSTAT,TRNSDR                                                   
         JO    *+8                                                              
         LA    RE,BUKCR                                                         
         ZAP   0(L'BUKDR,RE),TRNAMNT                                            
                                                                                
         GOTOR ADDBUK,DMCB,(X'80',BUFFKEY),ACCT43,HELLO                         
         GOTOR ADDBUK,DMCB,(X'80',BUFFKEY),BUKEL,HELLO                          
                                                                                
         TM    UPSI,UPSIDCVT       Dump converted records                       
         JZ    TRNS320                                                          
         CP    MAXOUT,=P'2500'                                                  
         JH    TRNS320                                                          
         AP    MAXOUT,PKONE                                                     
         LA    RF,GLBRFST-GLBKEY+4                                              
         GOTOR PRNTBL,DMCB,=C'Converted',BUFFREC,C'DUMP',(RF),=C'1R',  +        
               (C'P',VPRINT)                                                    
                                                                                
TRNS320  GOTOR BUFFRIN,DMCB,('BUFFAPUT',KEYBUFF),BUFFREC,ADCOMFAC               
         DROP  R6                                                               
                                                                                
TRNSXIT  J     EXIT                                                             
         DROP  R2,R3,R4,R5                                                      
         EJECT ,                                                                
***********************************************************************         
* Balance add                                                                   
***********************************************************************         
         USING ACBD,ACCBREC                                                     
         USING TRNRECD,R2                                                       
         USING TRNELD,R3                                                        
         USING GLDELD,R6                                                        
         USING TRSELD,R5                                                        
BAD      NTR1                                                                   
         SAM31                                                                  
         CLI   0(R5),TRSELQ        X'60' element                                
         JE    BAD030                                                           
                                                                                
         LR    R5,R3               R5 = A(TRNELD)                               
BAD010   CLI   0(R5),EOR           TEST END OF RECORD                           
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R5),TRSELQ        X'60' element                                
         JE    BAD030                                                           
         LLC   RF,1(,R5)                                                        
         AR    R5,RF                                                            
         J     BAD010                                                           
                                                                                
BAD030   XC    ACCBREC,ACCBREC                                                  
         ZAP   ACBSDR,PKZERO                                                    
         ZAP   ACBSCR,PKZERO                                                    
         ZAP   ACBGDR,PKZERO                                                    
         ZAP   ACBGCR,PKZERO                                                    
         CLI   TRNKUNT,C'G'                                                     
         JNE   BAD100                                                           
         CLI   QOPT6,BALD          Detailed                                     
         JE    *+8                                                              
         CLI   QOPT6,BALX          Extra detailed                               
         JNE   BAD080                                                           
         MVC   ACBSACT,TRNKCUNT                                                 
         MVC   ACBMOA,TRSPMOS                                                   
         CLC   ACBMOA,SPACES                                                    
         JH    *+6                                                              
         DC    H'0'                                                             
         CLI   QOPT6,BALX          Extra detailed                               
         JNE   BAD080              No                                           
         MVC   ACBOFFC,TRNOFFC                                                  
BAD080   MVC   ACBGACT,TRNKUNT                                                  
         LA    RE,ACBGDR                                                        
         TM    TRNSTAT,TRNSDR                                                   
         JO    *+8                                                              
         LA    RE,ACBGCR                                                        
         ZAP   0(L'ACBSDR,RE),TRNAMNT                                           
         J     BAD300                                                           
                                                                                
BAD100   CLI   TRNKUNT,C'S'                                                     
         JNE   BADXIT                                                           
         CLI   QOPT6,BALX          Extra detailed                               
         JE    *+8                                                              
         CLI   QOPT6,BALD          Detailed                                     
         JNE   BAD110                                                           
         MVC   ACBMOA,TRSPMOS                                                   
         CLC   ACBMOA,SPACES                                                    
         JH    *+6                                                              
         DC    H'0'                                                             
         MVC   ACBSACT,GLDCULA                                                  
                                                                                
         CLI   QOPT6,BALX          Extra deltails                               
         JNE   BAD110              Done                                         
         MVC   ACBOFFC,GLDOFFC                                                  
         CLC   ACBOFFC,SPACES                                                   
         JH    *+10                                                             
         MVC   ACBOFFC,GLDOFFO                                                  
         CLC   ACBOFFC,SPACES                                                   
         JH    BAD110                                                           
         MVC   ACBOFFC,TRNOFFC                                                  
                                                                                
BAD110   MVC   ACBGACT,GLDULA                                                   
         LA    RE,ACBSDR                                                        
         TM    TRNSTAT,TRNSDR                                                   
         JO    *+8                                                              
         LA    RE,ACBSCR                                                        
         ZAP   0(L'ACBSDR,RE),TRNAMNT                                           
                                                                                
BAD300   GOTOR BIN31,DMCB,ACCBREC,AGLBAL,#OFACCS,(1,ACBLNQ),           +        
               L'ACBKEY,MAX#ACCS                                                
         MVC   #OFACCS,8(R1)                                                    
         ICM   R8,15,0(R1)         R8 = A(Record added or found)                
         JNZ   *+6                                                              
         DC    H'00'               Table full                                   
                                                                                
BIN      USING ACBD,R8                                                          
         TM    0(R1),X'80'         Record not found?                            
         JO    BADXIT              No record found                              
         AP    BIN.ACBGDR,ACBGDR                                                
         AP    BIN.ACBGCR,ACBGCR                                                
         AP    BIN.ACBSDR,ACBSDR                                                
         AP    BIN.ACBSCR,ACBSCR                                                
                                                                                
BADXIT   SAM24                                                                  
         J     EXIT                                                             
         DROP  R2,R3,R6                                                         
         EJECT ,                                                                
***********************************************************************         
* Print out balances                                                            
***********************************************************************         
SHOWBAL  NTR1                                                                   
         SAM31                                                                  
         L     R8,AGLBAL                                                        
         ICM   R0,15,#OFACCS                                                    
         JZ    SHOWBXIT                                                         
SHOWB100 MVC   P+1(14),BIN.ACBGACT     Unit G account                           
         OC    BIN.ACBSACT,SPACES                                               
         OC    BIN.ACBOFFC,SPACES                                               
         MVC   WORK,BIN.ACBMOA                                                  
         MVI   WORK+2,1                                                         
         SAM24                                                                  
         GOTOR DATCON,DMCB,(1,WORK),(6,P+16)                                    
         SAM31                                                                  
         MVC   P+23(2),BIN.ACBOFFC     Office code                              
         MVC   P+26(14),BIN.ACBSACT    Unit S account                           
                                                                                
         ZAP   DUB,BIN.ACBGDR                                                   
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  P+43(14),DUB                                                     
         CP    BIN.ACBGDR,PKZERO                                                
         JNM   *+8                                                              
         MVI   P+42,C'-'                                                        
                                                                                
         ZAP   DUB,BIN.ACBGCR                                                   
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  P+59(14),DUB                                                     
         CP    BIN.ACBGCR,PKZERO                                                
         JNM   *+8                                                              
         MVI   P+58,C'-'                                                        
                                                                                
         ZAP   DUB,BIN.ACBSDR                                                   
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  P+75(14),DUB                                                     
         CP    BIN.ACBSDR,PKZERO                                                
         JNM   *+8                                                              
         MVI   P+74,C'-'                                                        
                                                                                
         ZAP   DUB,BIN.ACBSCR                                                   
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  P+91(14),DUB                                                     
         CP    BIN.ACBSCR,PKZERO                                                
         JNM   *+8                                                              
         MVI   P+90,C'-'                                                        
                                                                                
*AH debug for AVNY start                                                        
         CLI   BIN.ACBGACT+L'ACBGACT-1,C' '                                     
         JNH   SHOWB180                                                         
         CLC   BIN.ACBOFFC(1),BIN.ACBGACT+L'ACBGACT-1                           
         JE    SHOWB180                                                         
         MVC   P+108(8),=CL8'E-Office'                                          
*AH debug for AVNY end                                                          
                                                                                
SHOWB180 CLI   QOPT7,ALL           Show all                                     
         JE    SHOWB200            Yes                                          
         CP    BIN.ACBGDR,BIN.ACBSDR                                            
         JNE   SHOWB200            Show it                                      
         CP    BIN.ACBGCR,BIN.ACBSCR                                            
         JE    SHOWB210            Show it                                      
         AP    #OFWRNS,=P'1'                                                    
                                                                                
SHOWB200 SAM24                                                                  
         GOTOR ACREPORT                                                         
         SAM31                                                                  
                                                                                
SHOWB210 AHI   R8,ACBLNQ           Next entry                                   
         BRCT  R0,SHOWB100                                                      
                                                                                
SHOWBXIT SAM24                                                                  
         MVC   P,SPACES                                                         
         GOTOR ACREPORT                                                         
         J     EXIT                                                             
         DROP  BIN                                                              
         EJECT ,                                                                
***********************************************************************         
* Add GLDEL to GLU's done after the GLMOA Date                                  
***********************************************************************         
         USING GLBLKD,R8                                                        
         USING TRNRECD,R2                                                       
ADDGLD   NTR1                                                                   
*        GOTOR DATAMGR,DMCB,DMKEY,ACCDIR,RSTRKEY,RSTRKEY                        
*        JE    *+6                                                              
*        DC    H'0'                                                             
                                                                                
         L     R8,=A(LGLBLK)         Local GL Block                             
         MVC   @COMFACS,ADCOMFAC                                                
         MVC   @COMPANY,AIOCPY                                                  
         MVC   @LEDGER,AIOLDG                                                   
         MVC   @ACTLVA,AIOLVA                                                   
         MVC   @ACTLVB,AIOLVB                                                   
         MVC   @ACTLVC,AIOLVC                                                   
         MVC   @ACTLVD,AIOLVD                                                   
         ST    R2,@TRANACT                                                      
         LA    R1,PRGNEWF          New file                                     
         CLI   NEWOFF,YES                                                       
         JNE   *+8                                                              
         LA    R1,PRGNEWOF(R1)     New office                                   
                                                                                
         STC   R1,PRGIND                                                        
         MVI   GLACTION,GLADDEL                                                 
         MVI   GLPRG#,3                                                         
         MVC   GLWRITE,RCWRITE                                                  
         GOTOR ACGLPOST,DMCB,('MAXGL',(R8))                                     
         JE    ADDGL090                                                         
         MVC   P+1(9),=C'**ERROR**'                                             
                                                                                
         USING ERRTABD,RE                                                       
         L     RE,=A(ERRTAB)                                                    
ADDGL020 CLI   ERRTAB#,0                                                        
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLC   ERROR#,ERRTAB#                                                   
         JE    ADDGL022                                                         
         LA    RE,ERRTLNQ(,RE)                                                  
         J     ADDGL020                                                         
                                                                                
ADDGL022 MVC   P+11(L'ERRTMSG),ERRTMSG                                          
         MVC   P+46(14),TRNKUNT                                                 
         MVC   P+62(14),TRNKCUNT                                                
         MVC   P+78(14),GLTOACC                                                 
         GOTOR ACREPORT                                                         
         AP    #OFERRS,=P'1'                                                    
                                                                                
         USING TRNELD,R4                                                        
         LA    R4,TRNRFST                                                       
         LA    RE,ERRTDR                                                        
         TM    TRNSTAT,TRNSDR                                                   
         JO    *+8                                                              
         LA    RE,ERRTCR                                                        
         AP    0(L'ERRTDR,RE),TRNAMNT                                           
         SR    R6,R6                                                            
         J     ADDGLX                                                           
         DROP  R4,RE                                                            
                                                                                
         USING TRNELD,R4                                                        
         USING GLDELD,R6                                                        
ADDGL090 L     R6,@GLDEL                                                        
         LA    R4,TRNRFST                                                       
         LR    R5,R4                                                            
ADDGL100 CLI   0(R5),EOR                                                        
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R5),TRSELQ        X'60'                                        
         JE    ADDGL110                                                         
         LLC   RF,1(,R5)                                                        
         AR    R5,RF                                                            
         J     ADDGL100                                                         
                                                                                
         USING TRSELD,R5                                                        
ADDGL110 DS    0H                                                               
         GOTOR DATCON,DMCB,(2,TRSUPDT),(1,GLDDATE)                              
*        GOTOR DATCON,DMCB,(5,0),(1,GLDTODAY)                                   
         DROP  R5                  TRSELD                                       
                                                                                
         CLI   QOPT4,DUMPGLU       Print GLU GL accounts                        
         JNE   ADDGL200                                                         
         LA    RF,TRNRFST-TRNKEY                                                
         MVC   WORK,SPACES                                                      
         MVC   WORK(3),=C'GLU'                                                  
         CLI   QOPT2,SEEDFILE                                                   
         JNE   ADDGL112                                                         
         MVC   WORK(4),=C'SEED'                                                 
         CLC   TRNKUNT(2),LASTUL                                                
         JE    *+10                                                             
         ZAP   MAXSDMP,PKZERO      Reset when ledger changes                    
         MVC   LASTUL,TRNKUNT      Save off Unit ledger                         
         CP    MAXSDMP,=P'50'                                                   
         JH    ADDGL200                                                         
         AP    MAXSDMP,=P'1'                                                    
                                                                                
ADDGL112 GOTOR PRNTBL,DMCB,(8,WORK),(R2),C'DUMP',(RF),=C'1R',          +        
               (C'P',VPRINT)                                                    
*        MVC   P+1(14),GLDULA                                                   
*        MVC   P+18(12),GLDCNTRA                                                
*        ZAP   DUB,TRNAMNT                                                      
*        OI    DUB+L'DUB-1,X'0F'                                                
*        UNPK  P+32(10),DUB                                                     
*        MVC   P+45(L'TRNKULA),TRNKULA                                          
*        GOTOR ACREPORT                                                         
         DROP  R2,R4,R6            TRNRECD, TRNELD, GLDELD                      
                                                                                
ADDGL200 GOTOR DATAMGR,DMCB,DMKEY,ACCDIR,WORK,WORK                              
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   DA,WORK+(ACTKDA-ACTKEY)                                          
                                                                                
         CLC   LASTKEY,WORK        Same key                                     
         JE    ADDGL210                                                         
         GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,LASTKEY,IOKEY                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   DA,IOKEY+(ACTKDA-ACTKEY)                                         
                                                                                
ADDGL210 CLC   ACTKDA,DA                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   RCWRITE,YES                                                      
         JNE   ADDGLX                                                           
         GOTOR DATAMGR,DMCB,(X'80',PUTREC),ACCMST,ACTKDA,DAREC,IOWORK           
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
ADDGLX   LTR   R6,R6                                                            
         XIT1  REGS=(R6)                                                        
         DROP  R8                  GLBLKD                                       
         EJECT ,                                                                
***********************************************************************         
* Put records from Bufferin to tape 2, for concatination load                   
***********************************************************************         
         USING GLBRECD,R2                                                       
TAPEPUT  NTR1  BASE=*,LABEL=*                                                   
         CLI   QOPT1,DELGLBO       Delete all GLBRECS for this company          
         JE    *+8                                                              
         CLI   QOPT1,DELGLBS       Delete all GLBRECS for this company          
         JNE   TAPEP050                                                         
         LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   GLBKTYP,GLBKTYPQ    X'28'                                        
         MVC   GLBKCPY,QCOMPANY                                                 
         MVC   COMMAND,DMRDHI                                                   
         MVI   DMIND,X'88'                                                      
                                                                                
TAPEP010 LA    R2,IOKEY                                                         
         GOTOR DATAMGR,DMCB,(DMIND,COMMAND),ACCDIR,IOKEY,IOKEY,IOWORK           
         CLI   8(R1),0                                                          
         JE    TAPEP012                                                         
         TM    8(R1),X'02'         Marked deleted already ?                     
         JO    TAPEP040            Skip for now                                 
         DC    H'00'                                                            
                                                                                
TAPEP012 CLI   GLBKTYP,GLBKTYPQ    X'28'                                        
         JNE   TAPEP050                                                         
         CLC   GLBKCPY,QCOMPANY    Must be same company                         
         JNE   TAPEP050                                                         
         MVC   DA,IOKEY+(ACTKDA-ACTKEY)                                         
                                                                                
         GOTOR DATAMGR,DMCB,(X'80',GETREC),ACCMST,DA,DAREC,IOWORK               
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         L     R2,DAREC                                                         
         CLC   GLBKEY,IOKEY                                                     
         JE    *+6                                                              
         DC    H'00'                                                            
         OI    GLBRSTA,X'80'       Mark deleted                                 
         CLI   RCWRITE,YES                                                      
         JNE   TAPEP040                                                         
         GOTOR DATAMGR,DMCB,(X'80',PUTREC),ACCMST,DA,DAREC,IOWORK               
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         LA    R2,IOKEY                                                         
         OI    GLBKSTA,X'80'       Mark directory deleted                       
         GOTOR DATAMGR,DMCB,(X'80',DMWRT),ACCDIR,IOKEY,IOKEY,IOWORK             
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
TAPEP040 MVC   COMMAND,DMRSEQ                                                   
         MVI   DMIND,X'88'                                                      
         J     TAPEP010                                                         
                                                                                
TAPE     USING GLBRECD,BUFFKEY                                                  
TAPEP050 LA    R7,40               Dump R7 records                              
         CLI   QOPT1,DELGLBO       Delete all GLBRECS for this company          
         JNE   *+8                                                              
         MVI   RCWRITE,NO          Reset now, stop writing                      
                                                                                
         CLI   QOPT5,WRITEFIL      Write to file instead?                       
         JE    TAPEP060                                                         
         L     R2,=A(TAPEOUT)                                                   
         OPEN  ((2),OUTPUT)                                                     
         TM    48(R2),X'10'        Test was okay?                               
         JO    TAPEP060                                                         
         ABEND 206,DUMP                                                         
                                                                                
TAPEP060 XC    BUFFREC(100),BUFFREC                                             
         MVI   BUFFACT,BUFFARDH                                                 
                                                                                
TAPEP080 GOTOR BUFFRIN,DMCB,(BUFFACT,KEYBUFF),BUFFREC,ADCOMFAC                  
         JNE   TAPEP300            Done                                         
         MVI   BUFFACT,BUFFASEQ                                                 
         SR    RF,RF                                                            
         ICM   RF,3,TAPE.GLBRLEN                                                
         LA    R2,BUFFKEY                                                       
         AR    R2,RF                                                            
         XC    0(10,R2),0(R2)      Clear end                                    
         CLI   QOPT5,WRITEFIL      Write to file directly ?                     
         JNE   TAPEP200            No, output tape                              
********************************                                                
*        WRITE to file         *                                                
********************************                                                
         MVC   IOKEY,TAPE.GLBKEY                                                
         GOTOR DATAMGR,DMCB,(X'88',DMREAD),ACCDIR,IOKEY,IOKEY,IOWORK            
         CLI   8(R1),0                                                          
         JE    TAPEP120                                                         
         TM    8(R1),X'02'         Record mark deleted                          
         JO    TAPEP120                                                         
         TM    8(R1),X'10'         Not found                                    
         JO    TAPEP160            Add record                                   
         DC    H'00'               Something went wrong                         
                                                                                
********************************                                                
*        UPDATE RECORD         *                                                
********************************                                                
TAPEP120 LA    R2,IOKEY                                                         
         MVC   DA,GLBKDA                                                        
         GOTOR DATAMGR,DMCB,(X'80',GETREC),ACCMST,DA,DAREC,IOWORK               
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLC   TAPE.GLBKEY,IOKEY   Just double check                            
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         NI    GLBKSTA,TURNOFF-X'80'                                            
         XC    GLBKUPDT,GLBKUPDT                                                
         XC    GLBKUPTM,GLBKUPTM                                                
*        MVC   DA,GLBKDA                                                        
         CLI   RCWRITE,YES                                                      
         JNE   TAPEP280                                                         
         GOTOR DATAMGR,DMCB,(X'80',PUTREC),ACCMST,DA,TAPE.GLBKEY,IOWORK         
         CLI   8(R1),0             Error ?                                      
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         GOTOR DATAMGR,DMCB,(X'80',DMWRT),ACCDIR,IOKEY,IOKEY,IOWORK             
         CLI   8(R1),0                                                          
         JE    TAPEP280                                                         
         DC    H'00'                                                            
*****************************                                                   
*        ADD RECORD         *                                                   
*****************************                                                   
TAPEP160 CLI   RCWRITE,YES         Yes, to file we go?                          
         JNE   TAPEP280            Not really                                   
         XC    DA,DA                                                            
         GOTOR DATAMGR,DMCB,(X'80',ADDREC),ACCMST,DA,TAPE.GLBKEY,IOWORK         
         CLI   8(R1),0             Error ?                                      
         JE    TAPEP280                                                         
         DC    H'00'                                                            
         DROP  R2                                                               
********************************                                                
*        WRITE to tape         *                                                
********************************                                                
TAPEP200 SR    RF,RF                                                            
         ICM   RF,3,TAPE.GLBRLEN                                                
         AHI   RF,4                                                             
         STCM  RF,3,BUFFREC                                                     
         L     R2,=A(TAPEOUT)                                                   
         LA    R0,BUFFREC                                                       
         PUT   (2),(0)                                                          
                                                                                
TAPEP280 AP    TAPECNT,PKONE                                                    
         LTR   R7,R7                                                            
         JNP   TAPEP080                                                         
         LA    RF,GLBRFST-GLBKEY+4                                              
         GOTOR PRNTBL,DMCB,=C'Tape',BUFFREC,C'DUMP',(RF),=C'1R',       +        
               (C'P',VPRINT)                                                    
         SHI   R7,1                                                             
         J     TAPEP080                                                         
                                                                                
TAPEP300 CLI   QOPT5,WRITEFIL                                                   
         JE    TAPEXIT                                                          
         L     R2,=A(TAPEOUT)                                                   
         CLOSE ((2))                                                            
         J     TAPEXIT                                                          
*                                                                               
TAPEXIT  J     EXIT                                                             
         DROP  TAPE                                                             
         DROP  R9,RA,RB,RC                                                      
         EJECT ,                                                                
GLOBALS  DS    0D                                                               
                                                                                
MAXGL    EQU   16                  For 16K block                                
K        EQU   1024                                                             
TURNOFF  EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   X'FF'                                                            
EOR      EQU   0                                                                
DELGLBS  EQU   C'X'                QOPT1 Delete GLBRECS for company             
DELGLBO  EQU   C'O'                QOPT1 Delete GLBRECS for cpy only            
SEEDFILE EQU   C'S'                QOPT2 Seed file with GLDELs                  
TURNON   EQU   C'O'                QOPT3 Write back GLMOA and NEWGL bit         
TURNDTE  EQU   C'D'                QOPT3 Write back GLMOA only                  
DUMPGLU  EQU   C'G'                QOPT4 Dump some GLU records                  
WRITEFIL EQU   C'W'                QOPT5 Seed file with GLDELs                  
BALB     EQU   C'B'                QOPT6 Basic  balance                         
BALD     EQU   C'D'                QOPT6 Detail balance                         
BALX     EQU   C'X'                QOPT6 Extra detail balance                   
ALL      EQU   C'A'                QOPT7 Show all accounts                      
DUMPMAX  DC    P'10'                                                            
DUMPCNT  DC    PL8'0'                                                           
MAXOUT   DC    PL8'0'                                                           
MAXSDMP  DC    PL8'0'                                                           
                                                                                
T00A     DC    C'T00A'                                                          
HEXTAB   DC    C'0123456789ABCDEF'                                              
                                                                                
BIN31    DC    V(BINSRCH)                                                       
BUFFRIN  DC    V(BUFFERIN)                                                      
VRECTYPE DC    V(ACRECTYP)                                                      
ACGLKEY  DC    V(ACGLKEY)                                                       
ACGLPOST DC    V(ACGLPOST)                                                      
ADDBUK   DC    V(ACADDBUK)                                                      
PERVERT  DC    V(PERVERT)                                                       
PRNTBL   DC    V(PRNTBL)                                                        
VPRINT   DC    V(PRINT)                                                         
                                                                                
DAREC    DC    A(IOREC)                                                         
DA       DS    F                                                                
                                                                                
DMIND    DS    X                                                                
COMMAND  DC    C'        '                                                      
GETREC   DC    C'GETREC  '                                                      
PUTREC   DC    C'PUTREC  '                                                      
ADDREC   DC    C'ADDREC  '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
ACCARC   DC    C'ACCARC  '                                                      
                                                                                
CPYTABS  DC    (MAX#CPYS*CPYTLNQ)X'00'                                          
#OFCPYS  DC    AL1(0)                                                           
#OFKEYS  DC    A(0)                                                             
#OFACCS  DC    A(0)                                                             
MAX#ACCS DC    A(0)                                                             
MAX#OFFS EQU   50                                                               
MAX#CPYS EQU   15                                                               
                                                                                
TAPECNT  DC    PL8'0'                                                           
OLDBUKS  DC    PL8'0'                                                           
DIRECTG  DC    PL8'0'              Direct via input                             
TRNSGLU  DC    PL8'0'              GLUed unit S                                 
TRNGGLU  DC    PL8'0'              GLUed unit G                                 
GLUGDR   DC    PL8'0'              GLU postings to G on/after GLMOA             
GLUGCR   DC    PL8'0'                                                           
GLUSDR   DC    PL8'0'              GLU postings from S on/after GLMOA           
GLUSCR   DC    PL8'0'                                                           
TOTGDR   DC    PL8'0'              1 Char off. Prior to GLMOA                   
TOTGCR   DC    PL8'0'                                                           
TOTDDR   DC    PL8'0'              Direct to Unit G, on/after GLMOA             
TOTDCR   DC    PL8'0'                                                           
TOTSDR   DC    PL8'0'                                                           
TOTSCR   DC    PL8'0'                                                           
ERRTDR   DC    PL8'0'                                                           
ERRTCR   DC    PL8'0'                                                           
#OFERRS  DC    PL8'0'                                                           
#OFWRNS  DC    PL8'0'                                                           
                                                                                
LASTCAC  DC    CL(CACKSPAC-CACKEY)' '                                           
SVLDG    DC    XL120'00'                                                        
                                                                                
         LTORG                                                                  
                                                                                
KEYBUFF  BUFFD TYPE=D,KEYLEN=L'GLBKEY+4,COMLEN=2*K,BUFFERS=255,        +        
               FILE=NEWGL                                                       
                                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     +        
               RECFM=VB,LRECL=6024,BUFNO=2,BLKSIZE=32760                        
                                                                                
         DS    0F                                                               
         DC    C'*BUFFREC'                                                      
BUFFREC  DS    CL(GLBRFST-GLBRECD+(2*K)+54)                                     
         ORG   BUFFREC                                                          
         DS    XL4                                                              
BUFFKEY  DS    CL(GLBRFST-GLBRECD)                                              
         ORG                                                                    
         DS    0F                                                               
         DC    C'IOIOIOIO'                                                      
IO       DS    XL(4*K)                                                          
         DC    C'DADADADA'                                                      
IOREC    DS    XL(4*K)                                                          
                                                                                
         DC    C'CPY*CPY*'                                                      
IOCPY    DS    XL(4*K)                                                          
         DC    C'LDG*LDG*'                                                      
IOLDG    DS    XL(4*K)                                                          
         DC    C'IOA*IOA*'                                                      
IOLVA    DS    XL(4*K)                                                          
         DC    C'IOB*IOB*'                                                      
IOLVB    DS    XL(4*K)                                                          
         DC    C'IOC*IOC*'                                                      
IOLVC    DS    XL(4*K)                                                          
         DC    C'IOD*IOD*'                                                      
IOLVD    DS    XL(4*K)                                                          
                                                                                
         DC    C'GLB*GLB*'                                                      
LGLBLK   DS    XL(MAXGL*K)                                                      
                                                                                
ERRTAB   DS    0F                                                               
         DC    AL1(ERRNOTRN),CL30'No transaction passed'                        
         DC    AL1(ERRNF44),CL30'Bad transaction not found'                     
         DC    AL1(ERRNOCPY),CL30'No company record found'                      
         DC    AL1(ERRNXCPY),CL30'No CPYEL on company record'                   
         DC    AL1(ERRNOLGR),CL30'No ledger record found'                       
         DC    AL1(ERRNOMED),CL30'No media record found'                        
         DC    AL1(ERRNOCLI),CL30'No client record found'                       
         DC    AL1(ERRNOPRD),CL30'No product record found'                      
         DC    AL1(ERRNOJOB),CL30'No job  record found'                         
         DC    AL1(ERRNOLVA),CL30'No 1st lev record found'                      
         DC    AL1(ERRNOLVB),CL30'No 2nd lev record found'                      
         DC    AL1(ERRNOLVC),CL30'No 3rd lev record found'                      
         DC    AL1(ERRNOLVD),CL30'No 4th lev record found'                      
         DC    AL1(ERR2SMAL),CL30'Record too small to add'                      
         DC    AL1(ERRNOTRS),CL30'No TRSELD element'                            
         DC    AL1(ERRIVTRN),CL30'Invalid transaction'                          
         DC    AL1(ERRNOFLT),CL30'No filter setup for office'                   
         DC    AL1(ERROFPOS),CL30'Offpos is undecernable'                       
         DC    AL1(ERRNOCLO),CL30'No client office'                             
         DC    AL1(ERRNOGLA),CL30'GL account is invalid'                        
         DC    AL1(ERRLKOCL),CL30'Account locked or closed'                     
         DC    AL1(ERRNORUL),CL30'No default rules setup'                       
         DC    AL1(ERRNOOFF),CL30'No office US error only'                      
         DC    AL1(0)                                                           
                                                                                
ACCMTAB  DS    0F                                                               
         DC    CL34'Tape records                      ',AL4(TAPECNT)            
         DC    CL34'1 char buckets records <  GLMOA   ',AL4(OLDBUKS)            
                                                                                
         DC    CL34'Direct to G via $INPUT >= GLMOA   ',AL4(DIRECTG)            
         DC    CL34'Total DR Direct Unit G >= GLMOA   ',AL4(TOTDDR)             
         DC    CL34'Total CR Direct Unit G >= GLMOA   ',AL4(TOTDCR)             
                                                                                
         DC    CL34'GLUed detected Unit G  >= GLMOA   ',AL4(TRNGGLU)            
         DC    CL34'GLUed detected Unit S  >= GLMOA   ',AL4(TRNSGLU)            
         DC    CL34'Total DR GLUed Unit G  >= GLMOA   ',AL4(GLUGDR)             
         DC    CL34'Total CR GLUed Unit G  >= GLMOA   ',AL4(GLUGCR)             
         DC    CL34'Total DR GLUed Unit S  >= GLMOA   ',AL4(GLUSDR)             
         DC    CL34'Total CR GLUed Unit S  >= GLMOA   ',AL4(GLUSCR)             
                                                                                
         DC    CL34'Total DR to Unit G     <  GLMOA   ',AL4(TOTGDR)             
         DC    CL34'Total CR to Unit G     <  GLMOA   ',AL4(TOTGCR)             
         DC    CL34'Total DR Unit S        >= GLMOA   ',AL4(TOTSDR)             
         DC    CL34'Total CR Unit S        >= GLMOA   ',AL4(TOTSCR)             
                                                                                
         DC    CL34'Balancing warnings,    G <> S     ',AL4(#OFWRNS)            
         DC    CL34'# of errors, or missing GLDELs    ',AL4(#OFERRS)            
         DC    CL34'Error DR amount, No GLDELs        ',AL4(ERRTDR)             
         DC    CL34'Error CR amount, No GLDELs        ',AL4(ERRTCR)             
                                                                                
ACCM#OF  EQU   (*-ACCMTAB)/ACCMLNQ                                              
         EJECT                                                                  
***********************************************************************         
* Application working storage                                                   
***********************************************************************         
ACGLD    DSECT                                                                  
DUB2     DS    D                                                                
DIRECT   DS    C                   Yes/No. Direct via input to unit G           
GLDATE   DS    XL3                                                              
MOASTR   DS    XL2                 From QMOSSTRT (packed YYMM)                  
MOSEND   DS    XL2                 From QMOSEND  (packed YYMM)                  
BUFFACT  DS    X                                                                
                                                                                
ELEM     DS    XL256                                                            
                                                                                
PKZERO   DS    PL1                                                              
PKONE    DS    PL1                                                              
                                                                                
UPSI     DS    XL1                                                              
UPSIDCVT EQU   X'80'               .  Dump some new converted records           
UPSIDTAP EQU   X'40'               .  Dump out some tape records                
UPSIDBBF EQU   X'10'               .  Dump tape records out                     
UPSIAOLV EQU   X'20'               .  Process as account/office instead         
                                                                                
LEVELS   DS    0XL16                                                            
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVADSC  DS    CL15                                                             
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVBDSC  DS    CL15                                                             
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVCDSC  DS    CL15                                                             
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVDDSC  DS    CL15                                                             
LEVLNQ   EQU   *-LEVELS                                                         
                                                                                
LDGTLVS  DS    0XL4                                                             
LDGTLV1L DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LDGTLV2L DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LDGTLV3L DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LDGTLV4L DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
                                                                                
LDGT#LVS DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
                                                                                
AIOCPY   DS    A                                                                
AIOLDG   DS    A                                                                
AIOLVA   DS    A                                                                
AIOLVB   DS    A                                                                
AIOLVC   DS    A                                                                
AIOLVD   DS    A                                                                
AIOEND   EQU   *                                                                
                                                                                
APARM    DS    A                                                                
SVRE     DS    A                                                                
SVRF     DS    A                                                                
AGLRECS  DS    A                                                                
AGLBAL   DS    A                                                                
HELLO    DS    A                                                                
                                                                                
PTODAY   DS    0PL3                ** TODAY'S DATE (PWOS) **                    
PTODAYYY DS    PL1                 YEAR                                         
PTODAYMM DS    PL1                 MONTH                                        
PTODAYDD DS    PL1                 DAY                                          
                                                                                
ETODAY   DS    0CL6                ** TODAY'S DATE (EBCDIC) **                  
ETODAYYY DS    CL2                 YEAR                                         
ETODAYMM DS    CL2                 MONTH                                        
ETODAYDD DS    CL2                 DAY                                          
GLMOA    DS    XL2                                                              
MOAEND   DS    XL2                                                              
                                                                                
RECTYPE  DS    X                   EXTRACTED RECORD TYPE                        
COMPANY  DS    X                   Company hex code                             
ACFILE   DS    C                   ACCn FILE                                    
NEWOFF   DS    C                   Yes / No                                     
LASTUL   DS    CL2                                                              
ALPHA    DS    CL2                 Company alpha code                           
CLOGO    DS    CL8                 Log on                                       
LASTCPY  DS    XL1                                                              
FIRSTX   DS    CL1                 Yes / No                                     
COUNT    DS    PL3                                                              
BUFREAD  DS    XL1                                                              
DUMP#    DS    XL1                                                              
                                                                                
CURMOS   DS    0XL2                Year/month                                   
CURYEAR  DS    XL1                                                              
CURMONTH DS    XL1                                                              
                                                                                
RSTRKEY  DS    XL(ACTRFST-ACTRECD)                                              
LASTKEY  DS    XL(ACTRFST-ACTRECD)                                              
IOKEY    DS    XL(ACTRFST-ACTRECD)                                              
IOWORK   DS    XL96                                                             
HAVE45   DS    C                                                                
ELEM43   DS    XL(CACLN1Q+L'CACNAME+3)                                          
ACCT43   DS    XL(CACLN1Q+L'CACNAME+3)                                          
                                                                                
ACCBREC  DS    CL(ACBLNQ)                                                       
BINREC   DS    XL(2*K)                                                          
ACGLDEND DS    0X                  End                                          
         EJECT                                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* DSECT                                                                         
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
CPYTABD  DSECT                                                                  
CPYCODE  DS    X                   Hex company code                             
CPYMOA   DS    XL2                 New GL moa date                              
CPYTLNQ  EQU   *-CPYTABD                                                        
                                                                                
ACBD     DSECT                                                                  
ACBKEY   DS    0CL32                                                            
ACBGACT  DS    CL14                Unit G Account                               
ACBSACT  DS    CL14                Unit S Account                               
ACBMOA   DS    XL2                 MOA                                          
ACBOFFC  DS    CL2                 Office code                                  
ACBGDR   DS    PL8                 Debits  for Unit G for account               
ACBGCR   DS    PL8                 Credits for Unit G for account               
ACBSDR   DS    PL8                 Debits  for Unit S for account               
ACBSCR   DS    PL8                 Credits for Unit S for account               
ACBLNQ   EQU   *-ACBD                                                           
                                                                                
ACCMD    DSECT                                                                  
ACCMMSG  DS    CL34                                                             
ACCMBUK  DS    AL4                                                              
ACCMLNQ  EQU   *-ACCMD                                                          
                                                                                
ERRTABD  DSECT                                                                  
ERRTAB#  DS    AL1                                                              
ERRTMSG  DS    CL30                                                             
ERRTLNQ  EQU   *-ERRTABD                                                        
                                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACGLPOSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGLPOSTD                                                      
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDBUFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014ACREPGL02 08/29/11'                                      
         END                                                                    
