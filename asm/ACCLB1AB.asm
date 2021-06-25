*          DATA SET ACCLB1AB   AT LEVEL 136 AS OF 12/23/99                      
*PHASE T6211AB                                                                  
CLB1A    TITLE '- FEE ADJUSTMENT LIST'                                          
CLB1A    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLB1A*,R8,R7,R6,CLEAR=YES,RR=RE                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK         RC=A(LOCAL WORKING STORAGE)                  
         USING FWORKD,RC                                                        
         ST    RE,BORELO                                                        
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING PRORATAD,LSPRATA                                                 
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     LSTFRST             FIRST FOR THIS LIST                          
         B     SCRFRST             FIRST FOR THIS SCREEN                        
         B     SCRLAST             LAST FOR THIS SCREEN                         
         B     VALFRST             FIRST FOR VALIDATE THIS LINE                 
         B     VALCLM              VALIDATE COLUMN                              
         B     VALLAST             LAST FOR VALIDATE THIS LINE                  
         B     DISCLM              DISPLAY COLUMN                               
         B     GETFRST             GET FIRST RECORD FOR LIST                    
         B     GETNEXT             GET NEXT RECORD                              
         B     EXIT                SET UP MY OWN HEADING                        
         B     VALSEL              VALIDATE SELECT TABLE                        
         B     EXIT                DISPLAY COLUMN TOTAL                         
         B     EXIT                DISPLAY SCREEN TOTAL                         
         B     PFKRTN              PFKEY ROUTINES                               
         EJECT                                                                  
***********************************************************************         
* FIRST FOR THIS SCREEN                                               *         
***********************************************************************         
         SPACE 1                                                                
SCRFRST  MVC   BASJOBC,BCJOBCOD                                                 
         L     RE,=A(ALOVAL)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,ALSVALS    SET OUTPUT BASE ADDRESS                      
         MVC   SFLTOPS,LSOPS       SAVE FILTERING OPTIONS FOR COMPARE           
         XC    LSOPS,LSOPS         CLEAR ALL OPTIONS                            
         XC    LSCLM,LSCLM         CLEAR DISPLAY COLUMNS                        
*                                                                               
         L     RF,AIO1             ENSURE OPTIONS READ FOR THIS LEVEL           
         USING TRNRECD,RF                                                       
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'TRNKUNT+L'TRNKLDG),BCCPYEL+(CPYPROD-CPYELD)            
         MVC   TRNKACT,BCJOBCOD                                                 
         DROP  RF                                                               
         GOTO1 AGETOPT,BODMCB,AIO1                                              
         L     RE,AGOPBLK                                                       
         L     RE,GOABEXT-GOBLOCK(RE)                                           
         XC    BOWORK2,BOWORK2                                                  
         LA    RF,BOWORK2                                                       
         SR    R0,R0                                                            
         LA    RF,DEFCLMR                                                       
SCRF020  GOTO1 AVALOPT,BOPARM,ALOTAB,(RF),(R0)                                  
         BE    SCRF022                                                          
         MVC   LSOPS(L'SFLTOPS),LSOPS   RESTORE OLD FILTERS IF ERROR            
         B     EXITL                                                            
*                                                                               
SCRF022  GOTO1 AVALBAT,BOPARM,(C'V',PBFEEQ),BASREFH,BASMONH                     
         BNE   EXITL                                                            
*                                                                               
         CLC   SFLTOPS,LSOPS                                                    
         BE    EXITY                                                            
         B     EXITH               FILT OPTIONS CHANGED - RESTART               
         EJECT                                                                  
***********************************************************************         
* LAST FOR THIS SCREEN - R1=A(FIRST FOOT)                             *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST  LR    R2,R1               DISPLAY JOB INFO                             
         LA    RF,=AL1(SCITCBIP,SCITCBAP,EOT)                                   
         GOTO1 AUPDJOB,BOPARM,(C'D',(RF)),AIO4,FOOTLINL(R2)                     
*                                                                               
SCRLASTX B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* PFKEY ROUTINES                                                      *         
***********************************************************************         
         SPACE 1                                                                
PFKRTN   LA    R0,C'D'                                                          
         CLI   BCPFKEY,PFK06                                                    
         BNE   *+8                                                              
         LA    R0,C'U'                                                          
         GOTO1 AVALBAT,BOPARM,((R0),PBFEEQ),BASREFH,BASMONH                     
         B     EXIT                EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT TABLE                                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   SLL   R1,2                                                             
         B     *(R1)                                                            
         B     VALDEL              DELETE                                       
         SPACE 1                                                                
***********************************************************************         
* DELETE SUB-ACTION                                                   *         
***********************************************************************         
         SPACE 1                                                                
VALDEL   L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         ZAP   TRNAMNT,BCPZERO     CLEAR AMOUNT                                 
         ZAP   BODUB1,BCPZERO      CLEAR ALLOCATION                             
         GOTO1 AALLTRN,BOPARM,('PTATRAL',TRNRECD),LSPRATA,(1,BODUB1),  *        
               (1,BODUB1)                                                       
         BNE   EXITN                                                            
         OI    TRNRSTAT,X'80'      DELETE RECORD                                
*                                                                               
         L     R3,AIO4                                                          
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         USING ASTELD,R3                                                        
         XR    RF,RF                                                            
VDEL02   CLI   ASTEL,0                                                          
         BE    VDEL10                                                           
         CLI   ASTEL,ASTELQ                                                     
         BE    VDEL06                                                           
VDEL04   IC    RF,1(R3)                                                         
         BXH   R3,RF,VDEL02                                                     
*                                                                               
VDEL06   ICM   RE,7,ASTDRAFT       REDUCE JOB DRAFTS COUNT                      
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         STCM  RE,7,ASTDRAFT                                                    
         DROP  R3                                                               
*                                                                               
VDEL10   DS    0H                                                               
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST FOR VALIDATE LINE                                             *         
***********************************************************************         
         SPACE 1                                                                
VALFRST  MVI   INPINDS,0                                                        
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE COLUMN - R1 CONTAINS COLUMN ROUTINE NUMBER                 *         
***********************************************************************         
         SPACE 1                                                                
VALCLM   TM    LSINDS1,LSICLMIN    TEST FIRST TIME                              
         BO    VCLM02                                                           
         CP    PP$AALLO,BCPZERO                                                 
         BE    *+8                                                              
         OI    INPINDS,INPIALCY    SET ALLOCATED=YES                            
*                                                                               
VCLM02   L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         SLL   R1,2                                                             
         B     *(R1)                                                            
         B     VALNET      01      NET AMOUNT (PRO-RATA)                        
         B     VALCOM      02      COMMISSIONABLE STATUS                        
         B     VALALC      03      ALLOCATED STATUS                             
         B     VALOFF      04      FINANCIAL OFFICE                             
         SPACE 1                                                                
***********************************************************************         
* VALIDATE NET                                                        *         
***********************************************************************         
         SPACE 1                                                                
VALNET   XR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   0(R1),FF                                                         
         BE    VALNETN                                                          
         ZAP   TRNAMNT,BODMCB+4(8)                                              
         BZ    VALNETN                                                          
*                                                                               
         XC    ASCIRATE,ASCIRATE                                                
         XC    ASCIHRS,ASCIHRS                                                  
         XR    RF,RF                                                            
         LA    R3,TRNELD                                                        
         USING SCIELD,R3                                                        
VNET02   CLI   SCIEL,0                                                          
         BE    VNET10                                                           
         CLI   SCIEL,OCAELQ                                                     
         BE    VNET10                                                           
         CLI   SCIEL,SCIELQ                                                     
         BNE   VNET08                                                           
         CLI   SCITYPE,SCITCRAT                                                 
         BNE   *+8                                                              
         ST    R3,ASCIRATE                                                      
         CLI   SCITYPE,SCITSJHR                                                 
         BNE   *+8                                                              
         ST    R3,ASCIHRS                                                       
VNET08   IC    RF,1(R3)                                                         
         BXH   R3,RF,VNET02                                                     
         DROP  R3                                                               
*                                                                               
VNET10   DS    0H                                                               
*&&UK                                                                           
         TM    BCCPYST7,CPYSSCNV  TEST CONVERTED FOR 2ND CURRENCY               
         BZ    VNET20                                                           
*&&                                                                             
         USING OCAELD,R3                                                        
         CLI   OCAEL,OCAELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   OCAINDS,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   OCAINDS,OCAIDSEC                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   OCANTYPE,QTRNAMNT                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BOWORK1(L'TOBCUR),TOBCUR                                         
         TM    LSINDS2,LSTOBACC                                                 
         BZ    *+16                                                             
         MVC   BOWORK1(L'TOBCURO),TOBCURO                                       
         MVC   BOWORK1+L'TOBCURO(L'TOBCURI),TOBCURI                             
         GOTO1 VCASHVAL,BOPARM,(X'80',TRNAMNT),(X'26',0),BOWORK1                
         ZAP   OCANCASH,12(8,R1)                                                
         DROP  R3                                                               
*                                                                               
VNET20   ICM   R3,15,ASCIRATE                                                   
         BZ    VALNETY                                                          
R        USING SCIELD,R3                                                        
         ICM   R4,15,ASCIHRS                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
H        USING SCIELD,R4                                                        
         ZAP   BODUB1,TRNAMNT      NET AMOUNT                                   
         ZAP   BODUB2,R.SCIAMNT    RATE                                         
         ZAP   BOPL81(16),BODUB1   CALCULATE HOURS=NET/RATE                     
         SRP   BOPL81(16),4,0                                                   
         DP    BOPL81(16),BODUB2                                                
         SRP   BOPL81,64-2,5                                                    
NEW      USING SCIELD,BOELEM                                                    
         MVC   NEW.SCIELD(SCILN2Q),H.SCIELD                                     
         ZAP   NEW.SCIAMNT,BOPL81                                               
         GOTO1 ATOBCHA,BOPARM,TRNRECD,H.SCIELD,NEW.SCIELD                       
         DROP  R,H,NEW                                                          
*                                                                               
VALNETY  OI    INPINDS,INPIUALC    SET UPDATE ALLOCATION                        
         B     EXITY                                                            
*                                                                               
VALNETN  MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE COMMISSIONABLE STATUS                                      *         
***********************************************************************         
         SPACE 1                                                                
VALCOM   IC    RE,FVXLEN           TEST YES                                     
         EX    RE,*+8                                                           
         BNE   VCOM02                                                           
         CLC   FVIFLD(0),BC@YES                                                 
         NI    TRNSTAT,FF-TRNSNOCM                                              
         B     VALCOMY                                                          
*                                                                               
VCOM02   EX    RE,*+8              TEST NO                                      
         BNE   EXITN                                                            
         CLC   FVIFLD(0),BC@NO                                                  
         OI    TRNSTAT,TRNSNOCM                                                 
*                                                                               
VALCOMY  OI    INPINDS,INPIUALC                                                 
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE ALLOCATED STATUS                                           *         
***********************************************************************         
         SPACE 1                                                                
VALALC   IC    RE,FVXLEN           TEST YES                                     
         EX    RE,*+8                                                           
         BNE   VALC02                                                           
         CLC   FVIFLD(0),BC@YES                                                 
         OI    INPINDS,INPIALCY                                                 
         B     VALALCY                                                          
*                                                                               
VALC02   EX    RE,*+8              TEST NO                                      
         BNE   EXITN                                                            
         CLC   FVIFLD(0),BC@NO                                                  
         NI    INPINDS,FF-INPIALCY                                              
*                                                                               
VALALCY  OI    INPINDS,INPIUALC                                                 
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FINANCIAL OFFICE                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALOFF   DS    0H                  DELETE CURRENT OFFICE                        
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('FFTELQ',TRNRECD),         *        
               (L'FFTTYPE,=AL1(FFTTOFFC))                                       
         OI    LSINDS1,LSIUPREC                                                 
*                                                                               
         CLI   FVILEN,0            NO INPUT = OKAY                              
         BE    EXITY                                                            
*                                                                               
         CLC   =C'SI',TRNKULC      ONLY VALID IF CREDIT ACCOUNT LEDGER          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NAFLE)                                           
         B     EXITN                                                            
*                                                                               
         GOTO1 AVALOFF,BOPARM,(X'80',FVIFLD)                                    
         BNE   EXITN                                                            
         PUSH  USING                                                            
         USING FFTELD,BOELEM                                                    
         XC    FFTEL(FFTLN1Q),FFTEL                                             
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+2                                        
         MVI   FFTTYPE,FFTTOFFC                                                 
         MVI   FFTDLEN,2                                                        
         MVC   FFTDATA(2),FVIFLD                                                
         GOTO1 VHELLO,BODMCB,(C'P',ACCMST),TRNRECD,BOELEM,0,0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*                                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* LAST FOR VALIDATE LINE                                              *         
***********************************************************************         
         SPACE 1                                                                
VALLAST  TM    INPINDS,INPIUALC    TEST UPDATE ALLOCATION                       
         BZ    EXITY                                                            
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
*                                                                               
         ZAP   BODUB1,BCPZERO      CLEAR ALLOCATION                             
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO1),LSPRATA,(1,BODUB1),     *        
               (1,BODUB1)                                                       
         BNE   EXITN                                                            
*                                                                               
         TM    INPINDS,INPIALCY    TEST ALLOCATED=YES                           
         BZ    VALLASTY                                                         
         ZAP   BODUB1,BCPZERO                                                   
         TM    TRNSTAT,TRNSNOCM                                                 
         BO    *+10                                                             
         ZAP   BODUB1,PA$NLCOM                                                  
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO1),('PTASCASH',LSPRATA),   *        
               (1,PA$NET),(1,BODUB1)                                            
         BNE   EXITN                                                            
*                                  TURN OFF PENDING BIT                         
         NI    TRNRSTA2,FF-TRNSBILP                                             
         LA    R3,TRNRFST                                                       
         USING TRXELD,R3                                                        
         XR    RF,RF                                                            
         CLI   TRXEL,TRXELQ                                                     
         BE    *+12                                                             
         IC    RF,TRXLN                                                         
         BXH   R3,RF,*-12                                                       
         NI    TRXSTA2,FF-TRXSBILP                                              
*                                                                               
VALLASTY B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMN - R1 CONTAINS COLUMN ROUTINE NUMBER                  *         
***********************************************************************         
         SPACE 1                                                                
DISCLM   L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     EXITN       00      FIRST TIME CALL                              
         B     EXITN       01      NET AMOUNT (PRO-RATA AMOUNT)                 
         B     DISCOM      02      COMMISSIONABLE STATUS                        
         B     DISALC      03      ALLOCATED STATUS                             
         B     DISOFF      04      FININCIAL OFFICE                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY COMMISSIONABLE STATUS                                       *         
***********************************************************************         
         SPACE 1                                                                
DISCOM   LA    RF,BC@YES                                                        
         TM    TRNSTAT,TRNSNOCM                                                 
         BZ    *+8                                                              
         LA    RF,BC@NO                                                         
         MVC   FVIFLD(L'BC@YES),0(RF)                                           
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY ALLOCATED STATUS                                            *         
***********************************************************************         
         SPACE 1                                                                
DISALC   LA    RF,BC@YES                                                        
         CP    PP$AALLO,BCPZERO                                                 
         BNE   *+8                                                              
         LA    RF,BC@NO                                                         
         MVC   FVIFLD(L'BC@YES),0(RF)                                           
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY FINANCIAL OFFICE                                            *         
***********************************************************************         
         SPACE 1                                                                
DISOFF   LA    R3,TRNELD                                                        
         USING FFTELD,R3                                                        
         XR    RF,RF                                                            
DOFF02   CLI   FFTEL,0                                                          
         BE    EXIT                                                             
         CLI   FFTEL,FFTELQ                                                     
         BNE   DOFF08                                                           
         CLI   FFTTYPE,FFTTOFFC                                                 
         BE    DOFF10                                                           
DOFF08   IC    RF,FFTLN                                                         
         BXH   R3,RF,DOFF02                                                     
*                                                                               
DOFF10   MVC   FVIFLD(2),FFTDATA                                                
         B     EXIT                                                             
         DROP  R3                                                               
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
LSTFRST  LA    R2,IOKEY            INITIALISE KEY                               
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,BCJOBCOD                                                 
*                                                                               
         B     EXITY                                                            
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* GET FIRST/NEXT LIST RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
GETFRST  LA    R1,IOACCDIR+IOHI+IO1  * GET FIRST RECORD FOR SCREEN *            
         B     *+8                                                              
GETNEXT  LA    R1,IOACCDIR+IOSQ+IO1  * GET NEXT RECORD FOR SCREEN *             
         LA    R2,IOKEY                                                         
         USING TRNRECD,R2                                                       
         GOTO1 AIO,(R1)                                                         
         BNE   EXITN                                                            
         CLC   TRNKCULA,IOKEYSAV+TRNKCULA-TRNKEY                                
         BNE   EXITN                                                            
         CLC   TRNKDATE,BCSPACES   NOT TRANSACTION RECORD                       
         BE    GETNEXT                                                          
*                                                                               
         CLI   TRNKSTYP,169                                                     
         BNE   GETNEXT                                                          
         GOTO1 ASETTRN,BOPARM,(C'D',TRNRECD)                                    
         BNE   GETNEXT             FILTER DIRECTORY RECORD                      
*                                                                               
         GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  GENERAL FILTERS                              
GETN150  GOTO1 ASETTRN,BOPARM,(C'M',AIO1)                                       
         BNE   GETNEXT                                                          
*                                                                               
GETNEXTX B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* GENERAL EXIT POINTS                                                 *         
***********************************************************************         
         SPACE 1                                                                
EXITY    CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,FF                                                             
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
TRNBILL  DC    C'99'                                                            
TRNORDER DC    C'**'                                                            
ACCMST   DC    C'ACCMST '                                                       
*                                                                               
DEFCLMR  EQU   *                                                                
         DC    AL1(PRO#NET-LSCLMPRO)                                            
         DC    AL1(BIL#CST-LSCLMPRO)                                            
         DC    AL1(PRO#ALC-LSCLMPRO)                                            
         DC    AL1(BIL#OFF-LSCLMPRO)                                            
         DC    AL1(EOT)                                                         
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
***********************************************************************         
* OPTION TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
ALOTAB   DS    0X                                                               
*                                  DISPLAY=COLUMN CODES                         
         DC    AL2(UC8DSP-TWAD,UC3DSP-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,LSCLMMAX,LSCLMMAX)                               
         DC    AL1((*-ALOTAB)/OPTTABL)                                          
         DC    AL2(OPTDISQ,0)                                                   
         DC    CL4'+'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  GENERAL TRANSACTION OPTIONS                  
         DC    AL2(TRNOPTQ)                                                     
*                                                                               
ALOTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
* VALIDATE OPTION INPUT                                             *           
*********************************************************************           
         SPACE 1                                                                
         DROP  R6,R7,R8,RB                                                      
ALOVAL   NMOD1 250,**ALOV**,CLEAR=YES                                           
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         LR    R4,R2               R4=A(OPTTAB ENTRY)                           
         USING OPTTABD,R4                                                       
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *(RF)                                                            
         SPACE 1                                                                
VALX     XMOD1                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE ACCLBCOLS                                                      
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBE7D                                                       
         SPACE 1                                                                
         ORG   OSVALS                                                           
*                                                                               
INPINDS  DS    XL1                 INPUT INDICATORS                             
INPIUALC EQU   X'80'               UPDATE ALLOCATION                            
INPIALCY EQU   X'40'               ALLOCATED=YES                                
*                                                                               
SFLTOPS  DS    CL(LSFLTOPL)        SAVED FILTERING OPTIONS                      
         ORG   OSVALS+OSVALSL                                                   
         SPACE 1                                                                
FWORKD   DSECT                     LOCAL W/S                                    
ASCIRATE DS    A                                                                
ASCIHRS  DS    A                                                                
HRS      DS    PL6                                                              
RATE     DS    PL6                                                              
         DS    0X                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'136ACCLB1AB  12/23/99'                                      
         END                                                                    
