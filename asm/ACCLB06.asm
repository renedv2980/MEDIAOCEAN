*          DATA SET ACCLB06    AT LEVEL 168 AS OF 03/13/01                      
*PHASE T62106A                                                                  
CLB06    TITLE '- BILL PROGRAM - UPDATE PENDING ACTIVITY'                       
CLB06    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL UPDWORKX-UPDWORKD,**CLB6**,R8,R7,R6,CLEAR=YES,RR=RE              
         USING UPDWORKD,RC         RC=A(LOCAL WORKING STORAGE)                  
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING PRORATAD,LSPRATA                                                 
         L     R5,ALSVALS                                                       
         ST    RE,RELO                                                          
         STCM  RF,8,UPDFLAG        SAVE ROUTINE NUMBER                          
         LA    R0,AROUTN           R0=(NUMBER OF NON ADDR ROUTINES)             
         LA    RE,AROUT            RE=A(NON ADDRESS ROUT ADDRESSES)             
         LA    RF,ARELROUT         RF=A(RELOCATED ADDRESSES)                    
MAIN10   L     R1,0(RE)            GET NEXT ADDRESS                             
         A     R1,RELO             RELOCATE IT                                  
         ST    R1,0(RF)            PUT IN RELOACTED LIST                        
         LA    RE,4(RE)            BUMP UP ADDRESS LISTS                        
         LA    RF,4(RF)                                                         
         BCT   R0,MAIN10                                                        
         CLI   CSACT,ACTDRA        TEST ACTION DRAFT                            
         BNE   MAIN20                                                           
         CLI   BCPFKEY,PFK06       TEST PF6 HIT (UPDATE)                        
         BNE   MAIN20                                                           
         MVI   CSACT,ACTUPD        SET ACTION UPDATE                            
         GOTO1 ARECACT,CSREC                                                    
*                                  GETOPT CALLS FROM HERE USE JOBBLOCK          
MAIN20   L     RE,AGOPBLK          FOR BILLING EXTENSION                        
         MVC   GOABEXT-GOBLOCKD(L'GOABEXT,RE),AJOBBLK                           
*                                                                               
         ICM   RF,8,UPDFLAG        RESTORE ROUTINE NUMBER                       
         MVI   UPDFLAG,0           CLEAR FLAG BYTS                              
         SRL   RF,32-8                                                          
         LTR   RF,RF               TEST IF EXIT ROUTINE REQUIRED                
         BZ    UPD02                                                            
         OI    UPDFLAG,UPDFEXIT    SET EXIT FLAG                                
         BCTR  RF,0                                                             
         CLM   RF,1,=AL1(UPDNTRYM)                                              
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     UPDNTRYS(RF)                                                     
*                                                                               
UPDNTRYS DS    0XL4                                                             
UPDXXX#1 EQU   1                                                                
         B     UPD02                                                            
UPDNTRYM EQU   (*-UPDNTRYS)/L'UPDNTRYS                                          
         SPACE 2                                                                
EXIT     XIT1  ,                                                                
         SPACE 2                                                                
UPD02    CLI   TWASCRN,S#BILUPD    TEST UPDATE SCREEN LOADED                    
         BE    UPDVAL                                                           
         L     RE,AGOPBLK          FIRST TIME CLEAR SAVED GOBLOCK VALS          
         XC    8(GOADM+8-GOBLOCK,RE),8(RE)                                      
         GOTO1 AOVRSCR,BOPARM,('S#BILUPD',BASOLAYH)                             
         BNE   EXIT                                                             
         B     UPD04                                                            
*                                                                               
         XC    UPDRVBH,UPDRVBH     CONCEAL US RECOVERY FIELDS                   
         XC    UPDRVMH,UPDRVMH                                                  
         OI    UPDRVBRH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    UPDRVBRH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    UPDRVMOH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    UPDRVMOH+(FVATRB-FVIHDR),FVAPROT                                 
*                                                                               
UPD04    CLI   CUCTRY,CTRYGER                                                   
         BE    UPD06                                                            
         XC    UPDLANP,UPDLANP     CONCEAL GERMAN LANGUAGE FIELDS               
         OI    UPDLANGH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    UPDLANGH+(FVATRB-FVIHDR),FVAPROT                                 
*                                                                               
UPD06    OI    UPDFLAG,UPDFFRST    SET FIRST TIME THROUGH                       
         MVC   FVMSGNO,=AL2(AI$EREQF)                                           
         TM    CSINDSG1,CSINDSET   TEST SETUP VALUES SAVED                      
         BNZ   UPDDSP                                                           
         LA    R1,UPDCLIH                                                       
         ST    R1,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY CURRENT KEY VALUES                                          *         
***********************************************************************         
         SPACE 1                                                                
UPDDSP   MVC   UPDCLI(L'BCCLICOD),BCCLICOD                                      
         OI    UPDCLIH+(FVIIND-FVIHDR),FVIVAL                                   
         SR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         LA    R1,BCPROCOD(RE)                                                  
         SR    RF,RF                                                            
         IC    RF,BCPROLEN                                                      
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   UPDPRO(0),0(R1)                                                  
         OI    UPDPROH+(FVIIND-FVIHDR),FVIVAL                                   
         IC    RF,BCPROLEN                                                      
         LA    R1,BCJOBCOD(RF)                                                  
         IC    RE,BCJOBLEN                                                      
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   UPDJOB(0),0(R1)                                                  
         OI    UPDJOBH+(FVIIND-FVIHDR),FVIVAL                                   
         MVC   UPDBLDF,CSBILNUM    PASSED FROM LIST/SETUP                       
         OI    UPDBLDFH+(FVIIND-FVIHDR),FVIVAL                                  
         GOTO1 AFVAL,UPDBLDFH      VALIDATE DRAFT BILL NUMBER                   
         BNE   UPDDSP20                                                         
         GOTO1 RDDFBIL                                                          
         BNE   EXIT                                                             
         L     R2,AIO1                                                          
         LA    R2,PBRRFST-PBRRECD(R2)                                           
         USING BLHELD,R2                                                        
         SR    R0,R0                                                            
UPDDSP02 CLI   BLHEL,0             TEST E-O-R                                   
         BE    UPDDSP20                                                         
         CLI   BLHEL,BLHELQ        TEST BILL HEADER ELEMENT                     
         BE    UPDDSP04                                                         
         IC    R0,BLHLN                                                         
         AR    R2,R0                                                            
         B     UPDDSP02                                                         
UPDDSP04 OC    BLHDSC,BLHDSC       TEST ANY DISCOUNT                            
         BZ    UPDDSP10                                                         
         ZAP   BLHDSC,BLHDSC                                                    
         BZ    UPDDSP10                                                         
         CURED (P6,BLHDSC),(6,UPDDSCA),2,ALIGN=LEFT,DMCB=BODMCB                 
         OI    UPDDSCAH+(FVOIND-FVIHDR),FVOXMT                                  
UPDDSP10 OC    BLHSCH,BLHSCH       TEST ANY SURCHARGE                           
         BZ    UPDDSP20                                                         
         ZAP   BLHSCH,BLHSCH                                                    
         BZ    UPDDSP20                                                         
         CURED (P6,BLHSCH),(6,UPDSRCA),2,ALIGN=LEFT,DMCB=BODMCB                 
         OI    UPDSRCAH+(FVOIND-FVIHDR),FVOXMT                                  
UPDDSP20 MVC   UPDDEB(L'ACTKACT),BCCMPPRF+(PPRRECVA-PPRELD)                     
         OI    UPDDEBH+(FVOIND-FVIHDR),FVOXMT                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY FIELDS                                                 *         
***********************************************************************         
         SPACE 1                                                                
UPDVAL   NI    CSINDSG1,FF-CSINDSET                                             
         LA    R2,UPDKEY                                                        
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
*                                                                               
         GOTO1 VACSRCHC,BOPARM,(4,UPDCLIH),ATWA,BCCPYPRD,ACOM,         *        
               (X'11',0)                                                        
         MVI   FVMINL,1                                                         
         MVC   FVMAXL,BCCLILEN                                                  
         GOTO1 AFVAL,UPDCLIH                                                    
         BE    *+14                                                             
         XC    BCCLI(BCCLIL),BCCLI                                              
         B     EXIT                                                             
         MVC   ACTKACT,FVIFLD                                                   
         TM    UPDCLIH+FHIID,FHIIVA                                             
         BNZ   UPDVAL04                                                         
         NI    UPDPROH+FHIID,FF-FHIIVA                                          
         OI    UPDFLAG,UPDFCHNG                                                 
*                                                                               
UPDVAL04 GOTO1 ASETUP,BOPARM,(X'80',ACTKACT),0,0                                
         BNE   EXIT                                                             
*&&US                                                                           
         MVC   ACCODE+(ACTKACT-ACTRECD)(L'BCCLICOD),BCCLICOD                    
         GOTO1 VACSRCHC,BOPARM,UPDCLIH,ATWA,(BCCLILEN,0),              *        
               (X'C0',19),ACCODE,(L'BCCLINAM,BCCLINAM)                          
         OI    UPDCLIH+FHIID,FHIIVA                                             
*&&                                                                             
         GOTO1 VACSRCHC,BOPARM,(4,UPDPROH),ATWA,BCCPYPRD,              *        
               (BCCLILEN,ACOM),(X'22',BCCLICOD)                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,BCCLILEN                                                      
         IC    RF,BCPROLEN                                                      
         SR    RF,RE                                                            
         STC   RF,FVMAXL                                                        
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,UPDPROH                                                    
         BE    *+14                                                             
         XC    BCPRO(BCPROL),BCPRO                                              
         B     EXIT                                                             
         SR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         LA    RE,ACTKACT(RE)                                                   
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+4                                                           
         MVC   0(0,RE),FVIFLD                                                   
         TM    UPDPROH+FHIID,FHIIVA                                             
         BNZ   UPDVAL08                                                         
         NI    UPDJOBH+FHIID,FF-FHIIVA                                          
         OI    UPDFLAG,UPDFCHNG                                                 
UPDVAL08 GOTO1 ASETUP,BOPARM,(X'40',ACTKACT),0,0                                
         BNE   EXIT                                                             
*                                                                               
*&&US                                                                           
         MVC   ACCODE+(ACTKACT-ACTRECD)(L'BCPROCOD),BCPROCOD                    
         GOTO1 VACSRCHC,BOPARM,UPDPROH,ATWA,(BCPROLEN,BCCLILEN),       *        
               (X'C0',19),ACCODE,(L'BCPRONAM,BCPRONAM)                          
         OI    UPDPROH+FHIID,FHIIVA                                             
*&&                                                                             
         GOTO1 VACSRCHC,BOPARM,(4,UPDJOBH),ATWA,BCCPYPRD,              *        
               (BCPROLEN,ACOM),(X'33',BCPROCOD)                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,BCPROLEN                                                      
         IC    RF,BCJOBLEN                                                      
         SR    RF,RE                                                            
         STC   RF,FVMAXL                                                        
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,UPDJOBH                                                    
         BE    *+14                                                             
         XC    BCJOB(BCJOBL),BCJOB                                              
         B     EXIT                                                             
         SR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         LA    RE,ACTKACT(RE)                                                   
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+4                                                           
         MVC   0(0,RE),FVIFLD                                                   
         TM    UPDJOBH+FHIID,FHIIVA                                             
         BNZ   *+8                                                              
         OI    UPDFLAG,UPDFCHNG                                                 
         GOTO1 ASETUP,BOPARM,(X'20',ACTKACT),0,0                                
         BNE   EXIT                                                             
         L     RF,AGOPBLK                                                       
         MVC   LSUGODUE,GOIDUE-GOBLOCKD(RF)                                     
         DROP  R2                                                               
UPDVAL12 DS   0H                                                                
*&&US                                                                           
         MVC   ACCODE+(ACTKACT-ACTRECD)(L'BCJOBCOD),BCJOBCOD                    
         GOTO1 VACSRCHC,BOPARM,UPDJOBH,ATWA,(BCJOBLEN,BCPROLEN),       *        
               (X'C0',19),ACCODE,(L'BCJOBNAM,BCJOBNAM)                          
         OI    UPDJOBH+FHIID,FHIIVA                                             
*&&                                                                             
*&&UK                                                                           
         GOTO1 VACSRCHC,BOPARM,UPDCLIH,ATWA,(BCCLILEN,0),              *        
               (X'C0',19),ACCODE,(L'BCCLINAM,BCCLINAM)                          
         OI    UPDCLIH+FHIID,FHIIVA                                             
         GOTO1 VACSRCHC,BOPARM,UPDPROH,ATWA,(BCPROLEN,BCCLILEN),       *        
               (X'C0',19),ACCODE,(L'BCPRONAM,BCPRONAM)                          
         OI    UPDPROH+FHIID,FHIIVA                                             
         GOTO1 VACSRCHC,BOPARM,UPDJOBH,ATWA,(BCJOBLEN,BCPROLEN),       *        
               (X'C0',19),ACCODE,(L'BCJOBNAM,BCJOBNAM)                          
         OI    UPDJOBH+FHIID,FHIIVA                                             
*                                                                               
         USING CONBLKD,R1                                                       
         GOTO1 AFVAL,UPDBLDDH                                                   
         BE    UPDVAL14            DUE DATE INPUT PRESENT                       
         LA    R1,BOELEM                                                        
         OC    LSUGODUE,LSUGODUE   TEST ANY OPT/MAINT SETTING                   
         BZ    UPDVAL14            NO                                           
         XC    CONBLKD(CONBLKL),CONBLKD                                         
         MVI   CONACTN,CONATRAQ                                                 
         MVI   CONFLD,CONFIDUE                                                  
         LA    RF,LSUGODUE                                                      
         STCM  RF,15,CONIADD                                                    
         LA    RF,UPDBLDD                                                       
         STCM  RF,15,CONOADD                                                    
         MVI   CONILEN,L'GOIDUE                                                 
         MVC   CONCOMF,ACOM                                                     
         GOTO1 VCONVERT                                                         
         BNE   EXIT                                                             
         DROP  R1                                                               
UPDVAL14 EQU   *                                                                
*&&                                                                             
*                                  DISPLAY DEBTORS ACCOUNT                      
         TM    UPDFLAG,UPDFCHNG    TEST IF KEY CHANGED                          
         BZ    UPDV060             NO                                           
         MVI   TWASESNL,0          RESET NTRSES LEVEL                           
         TM    UPDDEBH+(FVIIND-FVIHDR),FVITHIS                                  
         BO    UPDV060             DEBTORS OVERRIDDEN                           
         CLC   UPDDEB(L'ACTKACT),BCCMPPRF+(PPRRECVA-PPRELD)                     
         BE    UPDV060                                                          
         MVC   UPDDEB(L'ACTKACT),BCCMPPRF+(PPRRECVA-PPRELD)                     
         NI    UPDDEBH+(FVIIND-FVIHDR),FF-(FVIVAL)                              
         OI    UPDDEBH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
UPDV060  L     RF,AGOPBLK          DISPLAY SURCHARGE ACCOUNT                    
         L     RF,GOABEXT-GOBLOCKD(RF)                                          
         TM    UPDSRCH+(FVIIND-FVIHDR),FVITHIS                                  
         BO    UPDV070             SURCHARGE ACCOUNT OVERRIDDEN                 
         MVC   UPDSRC,GOSRGAC+1-GOBBLOCK(RF)                                    
         NI    UPDSRCH+(FVIIND-FVIHDR),FF-(FVIVAL)                              
         OI    UPDSRCH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
UPDV070  L     RF,AGOPBLK          DISPLAY DISCOUNT ACCOUNT                     
         L     RF,GOABEXT-GOBLOCKD(RF)                                          
         TM    UPDDSCH+(FVIIND-FVIHDR),FVITHIS                                  
         BO    UPDV080             DISCOUNT ACCOUNT OVERRIDDEN                  
         MVC   UPDDSC,GODSCAC+1-GOBBLOCK(RF)                                    
         NI    UPDDSCH+(FVIIND-FVIHDR),FF-(FVIVAL)                              
         OI    UPDDSCH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
UPDV080  ZAP   UPDAMTS,BCPZERO     INITIALISE BILL AMOUNTS                      
         MVC   UPDAMTS+L'UPDAMTS(UPDAMTSL-L'UPDAMTS),UPDAMTS                    
*                                                                               
         L     R1,AIO1             PROCESS JOB RECORD                           
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         SR    R0,R0                                                            
*                                                                               
         USING ABLELD,R1                                                        
UPDV090  CLI   ABLEL,ABLELQ        PROCESS BALANCE ELEMENT                      
         BNE   UPDV100                                                          
         ZAP   UPDBLBL,ABLDR                                                    
         SP    UPDBLBL,ABLCR                                                    
         B     UPDV120                                                          
*                                                                               
         USING SCIELD,R1                                                        
UPDV100  CLI   SCIEL,SCIELQ        PROCESS SUBSIDIARY CASH ELEMENTS             
         BE    UPDV102                                                          
         CLI   SCIEL,JCBELQ        TEST JOB CLIENT BILLING ELEMENT              
         BE    UPDV118                                                          
         BNE   UPDV120                                                          
UPDV102  CLI   SCITYPE,SCITCBAP    ALLOCATION                                   
         BNE   UPDV110                                                          
         AP    UPDCBAPN,SCIAMNT                                                 
         AP    UPDCBAPC,SCIADMN                                                 
         B     UPDV120                                                          
UPDV110  CLI   SCITYPE,SCITCBWP    WRITE-OFFS                                   
         BNE   UPDV112                                                          
         AP    UPDCBWP,SCIAMNT                                                  
         AP    UPDCBWPT,SCIAMNT                                                 
         AP    UPDCBWP,SCIADMN                                                  
         AP    UPDCBWPC,SCIADMN                                                 
         B     UPDV120                                                          
UPDV112  CLI   SCITYPE,SCITCBTP    TRANSFERS                                    
         BNE   UPDV114                                                          
         AP    UPDCBTP,SCIAMNT                                                  
         B     UPDV120                                                          
UPDV114  CLI   SCITYPE,SCITCBRP    RECOVERIES                                   
         BNE   UPDV116                                                          
         AP    UPDCBRP,SCIAMNT                                                  
         AP    UPDCBRPT,SCIAMNT                                                 
         AP    UPDCBRP,SCIADMN                                                  
         AP    UPDCBRPC,SCIADMN                                                 
         B     UPDV120                                                          
*                                                                               
UPDV116  CLI   SCITYPE,SCITCBIP    FEE ADJUSTMENT                               
         BNE   UPDV120                                                          
         AP    UPDCBFP,SCIAMNT                                                  
         B     UPDV120                                                          
*                                                                               
UPDV118  MVC   UPDJCBEL,SCIEL      SAVE JCB ELEMENT                             
*                                                                               
UPDV120  IC    R0,SCILN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   SCIEL,0                                                          
         BNE   UPDV090                                                          
*                                                                               
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         TM    UPDDEBH+(FVIIND-FVIHDR),FVIVAL                                   
         BO    UPDV130             DEBTOR A/C ALREADY VALIDATED                 
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYEL+(CPYRECV-CPYELD)                     
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,UPDDEBH                                                    
         BNE   EXIT                                                             
         MVC   ACTKACT,FVIFLD                                                   
         GOTO1 AGETACT,0                                                        
         BNE   EXIT                                                             
         OI    UPDDEBH+(FVIIND-FVIHDR),FVIVAL                                   
         MVC   UPDDEBN,ACNAME                                                   
         OI    UPDDEBNH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
UPDV130  MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,UPDSRCH                                                    
         BNE   UPDV140                                                          
         MVC   ACTKULA,FVIFLD                                                   
         GOTO1 AGETACT,SRCHLEDG                                                 
         BNE   EXIT                                                             
         OI    UPDSRCH+(FVIIND-FVIHDR),FVIVAL                                   
         MVC   UPDSRCN,ACNAME                                                   
         OI    UPDSRCNH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   UPDSRCAN,BCSPACES   CLEAR SURCHARGE A/C COST CODE                
         MVC   UPDSRCAN(L'ACCOST),ACCOST                                        
*&&US                                                                           
         ICM   RF,15,ACASPA                                                     
         BZ    *+10                                                             
         MVC   UPDSRCAN,SPAAULA-SPAELD(RF)                                      
*&&                                                                             
*                                                                               
UPDV140  TM    UPDDSCH+(FVIIND-FVIHDR),FVIVAL                                   
         BO    UPDV180             DISCOUNT A/C ALREADY VALIDATED               
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,UPDDSCH                                                    
         BNE   UPDV180                                                          
         MVC   ACTKULA,FVIFLD                                                   
         GOTO1 AGETACT,DSCTLEDG                                                 
         BNE   EXIT                                                             
         OI    UPDDSCH+(FVIIND-FVIHDR),FVIVAL                                   
         MVC   UPDDSCN,ACNAME                                                   
         OI    UPDDSCNH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
UPDV180  CLI   UPDBLDTH+(FVILEN-FVIHDR),0                                       
         BNE   UPDV200                                                          
         GOTO1 VDATCON,BOPARM,(2,BCTODAYC),(17,UPDBLDT)                         
*                                                                               
UPDV200  GOTO1 AFVAL,UPDBLDTH      VALIDATE BILL DATE                           
         GOTO1 VPERVAL,BOPARM,(FVILEN,FVIFLD),(X'40',BOELEM)                    
         CLI   4(R1),0             TEST VALID DATE                              
         BE    UPDV220                                                          
         CLI   4(R1),4                                                          
         BE    UPDV220                                                          
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     EXIT                                                             
*                                                                               
UPDV220  MVC   LSUBILDC,BOELEM+(PVALCSTA-PERVALD)                               
         MVC   LSUBILDP,BOELEM+(PVALPSTA-PERVALD)                               
         MVC   BOWORK1+00(6),BOELEM+(PVALESTA-PERVALD)                          
         GOTO1 VDATCON,BOPARM,(2,BCTODAYC),(0,BOWORK1+6)                        
         LA    R2,BOELEM                                                        
         USING PERVALD,R2                                                       
         MVC   UPDBLDT,PVALCPER                                                 
         OI    UPDBLDTH+(FVOIND-FVIHDR),FVOXMT                                  
         SR    R0,R0                                                            
         ICM   R0,1,BCP104         NUMBER OF DAYS                               
         BNZ   *+8                                                              
         LA    R0,60               DEFAULT IS 60 DAYS                           
         GOTO1 VADDAY,BOPARM,BOWORK1,BOWORK1+12,(R0)                            
         CLC   BOWORK1+6(6),BOWORK1+12                                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$DOPSP)                                           
         B     EXIT                                                             
         LNR   R0,R0                                                            
         GOTO1 VADDAY,BOPARM,BOWORK1,BOWORK1+12,(R0)                            
         CLC   BOWORK1+6(6),BOWORK1+12                                          
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$DOPSP)                                           
         B     EXIT                                                             
         XC    BOWORK1,BOWORK1                                                  
         GOTO1 VDATCON,BOPARM,(2,LSUBILDC),(17,BOWORK1)                         
         CLC   UPDBLDT,BOWORK1                                                  
         BE    *+14                                                             
         MVC   UPDBLDT,BOWORK1                                                  
         OI    UPDBLDTH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
         TM    UPDFLAG,UPDFFRST    TEST FIRST TIME                              
         BZ    UPDV230                                                          
         LA    R0,UPDBLDFH                                                      
         STCM  R0,15,FVADDR                                                     
         MVC   FVMSGNO,=AL2(AI$EREQF)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
*                                                                               
UPDV230  GOTO1 AFVAL,UPDBLDFH      VALIDATE DRAFT BILL NUMBER                   
         BE    *+14                                                             
         XC    LSUBILDF,LSUBILDF                                                
         B     UPDV440                                                          
         GOTO1 RDDFBIL                                                          
         BNE   EXIT                                                             
         GOTO1 GETAMT                                                           
         MVC   FVMSGNO,=AL2(AE$DBNEJ)                                           
         CP    UPDBHAPN,UPDCBAPN   TEST NET & COMMISSION ARE THE SAME           
         BNE   EXIT                                                             
         CP    UPDBHAPC,UPDCBAPC                                                
         BNE   EXIT                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLC   UPDBLDF(L'LSUBILDF),LSUBILDF                                     
         BE    *+14                                                             
         MVC   UPDBLDF(L'LSUBILDF),LSUBILDF                                     
         OI    UPDBLDFH+(FVOIND-FVIHDR),FVOXMT                                  
         L     R2,AIO1                                                          
         LA    R2,PBRRFST-PBRRECD(R2)                                           
         USING BLHELD,R2                                                        
         CLI   BLHEL,BLHELQ        TEST BILL HEADER ELEMENT                     
         BNE   UPDV440                                                          
         OC    BLHDSC,BLHDSC       TEST ANY DISCOUNT                            
         BZ    UPDV400                                                          
         ZAP   BLHDSC,BLHDSC                                                    
         BZ    UPDV400                                                          
         TM    UPDDSCAH+(FVIIND-FVIHDR),FVIVAL                                  
         BO    UPDV400                                                          
         TM    UPDDSCAH+(FVIIND-FVIHDR),FVITHIS                                 
         BO    UPDV400             DISCOUNT OVERRIDDEN                          
         CURED (P6,BLHDSC),(6,UPDDSCA),2,ALIGN=LEFT,DMCB=BODMCB                 
         OI    UPDDSCAH+(FVOIND-FVIHDR),FVOXMT                                  
UPDV400  OC    BLHSCH,BLHSCH                                                    
         BZ    UPDV440                                                          
         ZAP   BLHSCH,BLHSCH                                                    
         BZ    UPDV440                                                          
         TM    UPDSRCAH+(FVIIND-FVIHDR),FVIVAL                                  
         BO    UPDV440                                                          
         TM    UPDSRCAH+(FVIIND-FVIHDR),FVITHIS                                 
         BO    UPDV440             SURCHARGE OVERRIDDEN                         
         CURED (P6,BLHSCH),(6,UPDSRCA),2,ALIGN=LEFT,DMCB=BODMCB                 
         OI    UPDSRCAH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
UPDV440  ZAP   BLHSCH,BCPZERO                                                   
         GOTO1 AVALAMT,BODMCB,(X'82',UPDSRCAH),(L'BLHSCH,BLHSCH)                
         BH    EXIT                                                             
         ZAP   UPDSCHPT,BLHSCH     TEST ANY SURCHARGE                           
         BZ    UPDV442                                                          
         MVI   FVMINL,1            SURCHARGE ACCOUNT REQUIRED                   
         GOTO1 AFVAL,UPDSRCH                                                    
         BH    EXIT                                                             
UPDV442  ZAP   BLHDSC,BCPZERO                                                   
         GOTO1 AVALAMT,BODMCB,(X'82',UPDDSCAH),(L'BLHDSC,BLHDSC)                
         BH    EXIT                                                             
         ZAP   UPDDSCPT,BLHDSC     TEST ANY DISCOUNT                            
         BZ    UPDV450                                                          
         MVI   FVMINL,1            DISCOUNT ACCOUNT REQUIRED                    
         GOTO1 AFVAL,UPDDSCH                                                    
         BH    EXIT                                                             
UPDV450  OC    LSUBILDF,LSUBILDF   TEST IF DRAFT BILL ENTERED                   
         BZ    *+16                NO - LIVE BILL OPTIONAL                      
         CLI   CSACT,ACTDRA        IF ACTION DRAFT                              
         BE    *+8                 LIVE BILL IS ALSO OPTIONAL                   
         MVI   FVMINL,1            LIVE BILL INPUT REQUIRED                     
         XC    LSUBILLV,LSUBILLV                                                
         GOTO1 AFVAL,UPDBLLVH      VALIDATE LIVE BILL NUMBER                    
         BH    EXIT                MISSING AND REQUIRED                         
         BNE   UPDV500                                                          
*                                                                               
         OC    LSUBILDF,LSUBILDF   TEST DRAFT BILL NUMBER SET                   
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$DBNOF)                                           
         B     EXIT                                                             
*                                                                               
         TM    FVIIND,FVINUM       TEST NUMERIC FIELD                           
         BNZ   UPDV460                                                          
         LH    RE,=Y(UC@AUTO-TWAD)                                              
         LA    RE,TWAD(RE)                                                      
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RE)                                                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
         TM    BCJOBSTA,BCJOBBNA   TEST 'AUTO' IS ALLOWED                       
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
         CLC   UPDBLLV(L'UC@AUTO),0(RE)                                         
         BE    *+14                                                             
         MVC   UPDBLLV(L'UC@AUTO),0(RE)                                         
         OI    UPDBLLVH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
         BAS   RE,GETBNO           ESTABLISH LIVE BILL# (LSUBILLV)              
         B     UPDV500                                                          
*                                                                               
UPDV460  CLI   BCP103,C'N'         TEST MANUAL BILL NUMBERS ALLOWED             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
         OI    BCDUB+L'BCDUB-1,X'0F'                                            
         UNPK  LSUBILLV,BCDUB                                                   
         OI    UPDBLLVH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R2,IOKEY                                                         
         USING PBRRECD,R2                                                       
         XC    PBRPAS,PBRPAS       TEST BILL NUMBER ALREADY USED                
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ                                                 
         MVC   PBRPBLNO,LSUBILLV                                                
         MVI   PBRPIND,PBRPILVE                                                 
         GOTO1 AIO,IOHIUP+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PBRPAS(PBRPUSER-PBRPAS),IOKEYSAV                                 
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$RECAE)                                           
         B     EXIT                                                             
*                                                                               
UPDV500  MVI   UPDBLNG,NOQ         SET NO BILLING                               
         OC    LSUBILDF,LSUBILDF                                                
         BZ    *+8                                                              
         MVI   UPDBLNG,YESQ        BILLING IS ON                                
         OC    LSUBILLV,LSUBILLV                                                
         BZ    *+8                                                              
         MVI   UPDBLNG,YESQ        BILLING IS ON                                
         CLI   UPDBLNG,YESQ                                                     
         BNE   UPDV600             DO NOT VALIDATE DUE DATE                     
         TM    BCJOBSTA,BCJOBSCB   TEST 'CLIENT BILLING' JOB                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INBTY)                                           
         B     EXIT                                                             
         GOTO1 AFVAL,UPDBLDDH                                                   
         BNE   UPDV520                                                          
*                                                                               
         LA    R1,BOELEM           VALIDATE INPUT EXPRESSION                    
         USING CONBLKD,R1                                                       
         XC    CONBLKD(CONBLKL),CONBLKD                                         
         MVI   CONACTN,CONAVGTQ                                                 
         MVI   CONFLD,CONFIDUE                                                  
         MVC   CONIDATE,LSUBILDP                                                
         MVC   CONILEN,FVILEN                                                   
         LA    R0,FVIFLD                                                        
         STCM  R0,15,CONIADD                                                    
         LA    R0,LSUDUEDT                                                      
         STCM  R0,15,CONOADD                                                    
         MVC   CONCOMF,ACOM                                                     
         GOTO1 VCONVERT                                                         
         BE    UPDV540                                                          
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
         DROP  R1                                                               
*                                                                               
UPDV520  OC    LSUGODUE,LSUGODUE   TEST DEFAULT DUE DATE RULE GIVEN             
         BNZ   *+14                                                             
         MVC   LSUDUEDT,LSUBILDC   SET DUE DATE FROM BILL DATE                  
         B     UPDV540                                                          
         LA    R1,BOELEM           USE DEFAULT DUE DATE EXPRESSION              
         USING CONBLKD,R1                                                       
         XC    CONBLKD(CONBLKL),CONBLKD                                         
         MVI   CONACTN,CONAGETQ                                                 
         MVI   CONFLD,CONFIDUE                                                  
         MVC   CONIDATE,LSUBILDP                                                
         LA    R0,LSUGODUE                                                      
         STCM  R0,15,CONIADD                                                    
         LA    R0,LSUDUEDT                                                      
         STCM  R0,15,CONOADD                                                    
         MVC   CONCOMF,ACOM                                                     
         GOTO1 VCONVERT                                                         
         BE    UPDV540                                                          
         CLI   CONERR,CERRLOW      DUE DATE LOW IS ONLY GOOD ERROR              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDATCON,BOPARM,(2,LSUDUEDT),(17,UPDBLDD)                         
         OI    UPDBLDDH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   FVMSGNO,=AL2(AE$DULOW)                                           
         B     EXIT                                                             
         DROP  R1                                                               
*                                                                               
UPDV540  GOTO1 VDATCON,BOPARM,(2,LSUDUEDT),(17,UPDBLDD)                         
         OI    UPDBLDDH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
UPDV600  XC    LSUBILRF,LSUBILRF   CLEAR BILL BATCH REF                         
         NI    UPDTYPES,FF-UPDBILQ                                              
         CLI   UPDBLNG,YESQ        TEST BILLING IS ON                           
         BE    UPDV620                                                          
         LA    R0,UPDBLBRH         IF NOT THEN NO BREF/BMON ALLOWED             
         STCM  R0,15,FVADDR                                                     
         CLI   UPDBLBRH+(FVILEN-FVIHDR),0                                       
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
         LA    R0,UPDBLMOH                                                      
         STCM  R0,15,FVADDR                                                     
         CLI   UPDBLMOH+(FVILEN-FVIHDR),0                                       
         BE    UPDV700                                                          
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                  ** BILL BATCH REFERENCE **                   
UPDV620  OI    UPDTYPES,UPDBILQ                                                 
         CLI   UPDBLBRH+(FVILEN-FVIHDR),0                                       
         BNE   UPDV630                                                          
         CLI   CSACT,ACTUPD                                                     
         BE    UPD625                                                           
         OC    LSUBILRF,LSUBILDF   USE PART OF DRAFT NUMBER FOR BREF            
         BNZ   UPDV640                                                          
UPD625   SR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         LA    RE,BCJOBCOD(RE)                                                  
         MVC   LSUBILRF(1),0(RE)                                                
         MVC   LSUBILRF+1(3),LSUBILLV+3                                         
         B     UPDV640                                                          
UPDV630  GOTO1 VALBRF,BOPARM,UPDBLBRH,LSUBILRF                                  
         BNE   EXIT                                                             
*                                  ** BILL BATCH MOA **                         
UPDV640  CLI   UPDBLMOH+(FVILEN-FVIHDR),0                                       
         BNE   UPDV650                                                          
         GOTO1 VDATCON,BOPARM,(1,LSUBILDP),(9,UPDBLMO)                          
         OI    UPDBLMOH+(FVOIND-FVIHDR),FVOXMT                                  
UPDV650  GOTO1 VALBMO,BOPARM,('BILBTY',UPDBLMOH),LSUBILMO                       
         BNE   EXIT                                                             
         MVC   UPDBLMCS,LSUBILMC   SAVE CHARACTER MOA                           
         B     UPDV700                                                          
*                                                                               
UPDJCB   USING JCBELD,UPDJCBEL                                                  
UPDV700  OC    UPDJCB.JCBWOF,UPDJCB.JCBWOF                                      
         BNZ   UPDV720                                                          
         LA    R0,UPDWOBRH                                                      
         STCM  R0,15,FVADDR                                                     
         CLI   UPDWOBRH+(FVILEN-FVIHDR),0                                       
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
         LA    R0,UPDWOMOH                                                      
         STCM  R0,15,FVADDR                                                     
         CLI   UPDWOMOH+(FVILEN-FVIHDR),0                                       
         BE    UPDV720                                                          
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
UPDV720  XC    LSUWOFRF,LSUWOFRF                                                
         NI    UPDTYPES,FF-UPDWOFQ                                              
         CLI   UPDWOBRH+(FVILEN-FVIHDR),0                                       
         BNE   *+12                                                             
         CLI   UPDWOMOH+(FVILEN-FVIHDR),0                                       
         BE    UPDV730                                                          
         GOTO1 VALBRF,BOPARM,UPDWOBRH,LSUWOFRF                                  
         BNE   EXIT                                                             
         GOTO1 VALBMO,BOPARM,('WOFBTY',UPDWOMOH),LSUWOFMO                       
         BNE   EXIT                                                             
         OI    UPDTYPES,UPDWOFQ                                                 
         MVC   UPDTOTAL,UPDJCB.JCBWOF                                           
*                                                                               
UPDJCB   USING JCBELD,UPDJCBEL                                                  
UPDV730  OC    UPDJCB.JCBRCV,UPDJCB.JCBRCV                                      
         BNZ   UPDV735                                                          
         LA    R0,UPDRVBRH                                                      
         STCM  R0,15,FVADDR                                                     
         CLI   UPDRVBRH+(FVILEN-FVIHDR),0                                       
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
         LA    R0,UPDRVMOH                                                      
         STCM  R0,15,FVADDR                                                     
         CLI   UPDRVMOH+(FVILEN-FVIHDR),0                                       
         BE    UPDV735                                                          
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
UPDV735  XC    UPDRECRF,UPDRECRF                                                
         NI    UPDTYPES,FF-UPDRECQ                                              
         CLI   UPDRVBRH+(FVILEN-FVIHDR),0                                       
         BNE   *+12                                                             
         CLI   UPDRVMOH+(FVILEN-FVIHDR),0                                       
         BE    UPDV740                                                          
         GOTO1 VALBRF,BOPARM,UPDRVBRH,UPDRECRF                                  
         BNE   EXIT                                                             
         GOTO1 VALBMO,BOPARM,('WOFBTY',UPDRVMOH),UPDRECMO                       
         BNE   EXIT                                                             
         OI    UPDTYPES,UPDRECQ                                                 
         SR    RE,RE                                                            
         ICM   RE,3,UPDJCB.JCBRCV                                               
         SR    RF,RF                                                            
         ICM   RF,3,UPDTOTAL                                                    
         AR    RF,RE                                                            
         STCM  RF,3,UPDTOTAL                                                    
         DROP  UPDJCB                                                           
*                                                                               
UPDJCB   USING JCBELD,UPDJCBEL                                                  
UPDV740  OC    UPDJCB.JCBXFR,UPDJCB.JCBXFR                                      
         BNZ   UPDV760                                                          
         LA    R0,UPDXFBRH                                                      
         STCM  R0,15,FVADDR                                                     
         CLI   UPDXFBRH+(FVILEN-FVIHDR),0                                       
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
         LA    R0,UPDXFMOH                                                      
         STCM  R0,15,FVADDR                                                     
         CLI   UPDXFMOH+(FVILEN-FVIHDR),0                                       
         BE    UPDV760                                                          
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
UPDV760  XC    LSUTRFRF,LSUTRFRF                                                
         NI    UPDTYPES,FF-UPDTRFQ                                              
         CLI   UPDXFBRH+(FVILEN-FVIHDR),0                                       
         BNE   *+12                                                             
         CLI   UPDXFMOH+(FVILEN-FVIHDR),0                                       
         BE    UPDV770                                                          
         GOTO1 VALBRF,BOPARM,UPDXFBRH,LSUTRFRF                                  
         BNE   EXIT                                                             
         GOTO1 VALBMO,BOPARM,('TRFBTY',UPDXFMOH),LSUTRFMO                       
         BNE   EXIT                                                             
         OI    UPDTYPES,UPDTRFQ                                                 
         SR    RE,RE                                                            
         ICM   RE,3,UPDJCB.JCBXFR                                               
         SR    RF,RF                                                            
         ICM   RF,3,UPDTOTAL                                                    
         AR    RF,RE                                                            
         STCM  RF,3,UPDTOTAL                                                    
         DROP  UPDJCB                                                           
*                                                                               
UPDJCB   USING JCBELD,UPDJCBEL                                                  
UPDV770  OC    UPDJCB.JCBFEE,UPDJCB.JCBFEE                                      
         BNZ   UPDV780                                                          
         LA    R0,UPDINBRH                                                      
         STCM  R0,15,FVADDR                                                     
         CLI   UPDINBRH+(FVILEN-FVIHDR),0                                       
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
         LA    R0,UPDINMOH                                                      
         STCM  R0,15,FVADDR                                                     
         CLI   UPDINMOH+(FVILEN-FVIHDR),0                                       
         BE    UPDV780                                                          
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
UPDV780  XC    UPDFEERF,UPDFEERF                                                
         NI    UPDTYPES,FF-UPDFEEQ                                              
         CLI   UPDINBRH+(FVILEN-FVIHDR),0                                       
         BNE   *+12                                                             
         CLI   UPDINMOH+(FVILEN-FVIHDR),0                                       
         BE    UPDV800                                                          
         GOTO1 VALBRF,BOPARM,UPDINBRH,UPDFEERF                                  
         BNE   EXIT                                                             
         GOTO1 VALBMO,BOPARM,('POSTFEEA',UPDINMOH),UPDFEEMO                     
         BNE   EXIT                                                             
         OI    UPDTYPES,UPDFEEQ                                                 
         SR    RE,RE                                                            
         ICM   RE,3,UPDJCB.JCBFEE                                               
         SR    RF,RF                                                            
         ICM   RF,3,UPDTOTAL                                                    
         AR    RF,RE                                                            
         STCM  RF,3,UPDTOTAL                                                    
         DROP  UPDJCB                                                           
*                                                                               
UPDV800  CLI   UPDTYPES,0          TEST ANY ACTIVITY BEING UPDATED              
         BNE   UPDV810                                                          
         LA    R0,UPDBLDFH         NOTHING HAPPENING - SET CURSOR               
         STCM  R0,15,FVADDR                                                     
         MVC   FVMSGNO,=AL2(AI$EREQF)                                           
         MVI   FVOMTYP,GTMINF      ENTER REQUIRED FIELDS                        
         B     EXIT                                                             
*                                                                               
UPDV810  CLC   UPDTOTAL,=H'100'    TEST # OF ITEMS MARKED FOR UPODATE           
         BNH   UPDV812                                                          
         MVC   FVMSGNO,=AL2(AE$TMTRN)                                           
         MVI   FVOMTYP,GTMERR      TOO MANY TRANSACTIONS                        
         B     EXIT                                                             
*                                                                               
UPDV812  CLI   UPDBLNG,YESQ        TEST BILLING IS ON                           
         BNE   UPDV820                                                          
         BAS   RE,VALACCR          VALIDATE MONTHLY ACCRUALS                    
         BNE   EXIT                                                             
         OC    UPDMNTHS(L'UPDMNTH),UPDMNTHS                                     
         BNZ   UPDV814                                                          
         CLI   UPDACCH+(FVILEN-FVIHDR),0                                        
         BE    UPDV820                                                          
         MVI   FVMINL,1            ACCRUAL DETAILS ARE REQUIRED                 
         GOTO1 AFVAL,UPDACC1H                                                   
         B     EXIT                WE KNOW ITS AN ERROR                         
UPDV814  LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVI   FVMINL,1            ACCRUAL ACCOUNT REQUIRED                     
         GOTO1 AFVAL,UPDACCH                                                    
         BH    EXIT                MOI                                          
         MVC   ACTKULA,FVIFLD                                                   
         GOTO1 AGETACT,ULBALSHT                                                 
         BNE   EXIT                                                             
         OI    UPDACCH+(FVIIND-FVIHDR),FVIVAL                                   
         MVC   UPDACCN,ACNAME                                                   
         OI    UPDACCNH+(FVOIND-FVIHDR),FVOXMT                                  
UPDV820  CLI   CSACT,ACTUPD        TEST ACTION UPDATE                           
         BNE   UPDVPOST                                                         
*                                                                               
         CLI   UPDBLNG,YESQ        VERIFY BATCH HEADERS                         
         BNE   UPDV822                                                          
         XR    RF,RF                                                            
         CLI   UPDBLBRH+(FVILEN-FVIHDR),0                                       
         BNE   *+8                                                              
         LA    RF,X'80'            BILL BREF MAY BE INCREMENTED                 
         GOTO1 AADDOBH,BODMCB,('POSTBILL',LSUBILRF),((RF),UPDBLBRH)             
         BNE   EXIT                                                             
*                                                                               
UPDV822  OC    LSUWOFRF,LSUWOFRF                                                
         BZ    UPDV825                                                          
*&&UK                                                                           
         GOTO1 AADDOBH,BODMCB,('POSTWOFF',LSUWOFRF),UPDWOBRH                    
         BNE   EXIT                                                             
*&&                                                                             
*&&US                                                                           
         CP    UPDCBWPT,BCPZERO                                                 
         BE    UPDV824                                                          
         GOTO1 AADDOBH,BODMCB,('POSTWOFT',LSUWOFRF),UPDWOBRH                    
         BNE   EXIT                                                             
UPDV824  CP    UPDCBWPC,BCPZERO                                                 
         BE    UPDV825                                                          
         GOTO1 AADDOBH,BODMCB,('POSTWOFC',LSUWOFRF),UPDWOBRH                    
         BNE   EXIT                                                             
*&&                                                                             
UPDV825  OC    UPDRECRF,UPDRECRF                                                
         BZ    UPDV828                                                          
*&&UK                                                                           
         GOTO1 AADDOBH,BODMCB,('POSTWOFF',UPDRECRF),UPDRVBRH                    
         BNE   EXIT                                                             
*&&                                                                             
*&&US                                                                           
         CP    UPDCBRPT,BCPZERO                                                 
         BE    UPDV826                                                          
         GOTO1 AADDOBH,BODMCB,('POSTWOFT',UPDRECRF),UPDRVBRH                    
         BNE   EXIT                                                             
UPDV826  CP    UPDCBRPC,BCPZERO                                                 
         BE    UPDV828                                                          
         GOTO1 AADDOBH,BODMCB,('POSTWOFC',UPDRECRF),UPDRVBRH                    
         BNE   EXIT                                                             
*&&                                                                             
*                                                                               
UPDV828  OC    LSUTRFRF,LSUTRFRF                                                
         BZ    UPDV830                                                          
         GOTO1 AADDOBH,BODMCB,('POSTTRNF',LSUTRFRF),UPDXFBRH                    
         BNE   EXIT                                                             
*                                                                               
UPDV830  OC    UPDFEERF,UPDFEERF                                                
         BZ    UPDV832                                                          
         GOTO1 AADDOBH,BODMCB,('POSTFEEA',UPDFEERF),UPDINBRH                    
         BNE   EXIT                                                             
*                                                                               
UPDV832  CLI   UPDBLNG,YESQ        ADD BATCH HEADERS                            
         BNE   UPDV834                                                          
         GOTO1 AADDOBH,BODMCB,('POSTBILL',LSUBILRF),0                           
*                                                                               
UPDV834  OC    LSUWOFRF,LSUWOFRF                                                
         BZ    UPDV838                                                          
*&&UK                                                                           
         GOTO1 AADDOBH,BODMCB,('POSTWOFF',LSUWOFRF),0                           
*&&                                                                             
*&&US                                                                           
         CP    UPDCBWPT,BCPZERO                                                 
         BE    UPDV836                                                          
         GOTO1 AADDOBH,BODMCB,('POSTWOFT',LSUWOFRF),0                           
UPDV836  CP    UPDCBWPC,BCPZERO                                                 
         BE    UPDV838                                                          
         GOTO1 AADDOBH,BODMCB,('POSTWOFC',LSUWOFRF),0                           
*&&                                                                             
UPDV838  OC    UPDRECRF,UPDRECRF                                                
         BZ    UPDV842                                                          
*&&UK                                                                           
         GOTO1 AADDOBH,BODMCB,('POSTWOFF',UPDRECRF),0                           
*&&                                                                             
*&&US                                                                           
         CP    UPDCBRPT,BCPZERO                                                 
         BE    UPDV840                                                          
         GOTO1 AADDOBH,BODMCB,('POSTWOFT',UPDRECRF),0                           
UPDV840  CP    UPDCBRPC,BCPZERO                                                 
         BE    UPDV842                                                          
         GOTO1 AADDOBH,BODMCB,('POSTWOFC',UPDRECRF),0                           
*&&                                                                             
*                                                                               
UPDV842  OC    LSUTRFRF,LSUTRFRF                                                
         BZ    UPDV844                                                          
         GOTO1 AADDOBH,BODMCB,('POSTTRNF',LSUTRFRF),0                           
*                                                                               
UPDV844  OC    UPDFEERF,UPDFEERF                                                
         BZ    UPDV848                                                          
         GOTO1 AADDOBH,BODMCB,('POSTFEEA',UPDFEERF),0                           
*                                                                               
UPDV848  CLI   UPDBLNG,YESQ        TEST BILLING IS ON                           
         BNE   UPDVPOST                                                         
         MVC   UPDBLRFS,LSUBILRF   SAVE BILL BATCH REF                          
         OC    UPDMNTHS(L'UPDMNTH),UPDMNTHS                                     
         BZ    UPDV890             NO ACCRUAL POSTINGS                          
         LA    R3,UPDMNTHS                                                      
         USING UPDMONTB,R3                                                      
         MVC   UPDMREF,LSUBILRF    SET MONTH1 REF FROM BILL REFERENCE           
         LA    R3,L'UPDMNTHS(R3)                                                
UPDV870  OC    UPDMNTH,UPDMNTH                                                  
         BZ    UPDV890                                                          
         MVC   LSUBILMC,UPDMNTH    SET ACCRUAL CHARACTER MOA                    
         MVC   LSUBILRF,UPDBLRFS   SET BILLING BATCH REFERENCE                  
         MVI   BCBYTE1,POSTBILL                                                 
         GOTO1 AADDOBH,BODMCB,('POSTBILL',LSUBILRF),(X'80',UPDBLBRH)            
         BE    UPDV880                                                          
         DC    H'0',C'$ABEND'      UNWIND ANY HEADERS ADDED ALREADY             
UPDV880  GOTO1 (RF),(R1),,0                                                     
         MVC   UPDMREF,LSUBILRF    SET REFERENCE - MAY HAVE CHANGED             
         LA    R3,L'UPDMNTHS(R3)                                                
         B     UPDV870                                                          
UPDV890  MVC   LSUBILRF,UPDBLRFS   RESTORE REAL BILL REFERENCE                  
         MVC   LSUBILMC,UPDBLMCS   RESTORE REAL BILL MONTH                      
         LH    RE,=Y(UC@AUTO-TWAD)                                              
         LA    RE,TWAD(RE)                                                      
         CLC   UPDBLLV(L'UC@AUTO),0(RE)                                         
         BNE   UPDVPOST            MANUAL INPUT BILL NUMBER                     
         MVC   UPDBLLV,LSUBILLV    DISPLAY AUTO BILL NUMBER                     
         BAS   RE,PUTBNO           WRITE-BACK ORIGIN OF BILL NUMBER             
UPDVPOST GOTO1 APOST,(RC)                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET NEXT AUTOMATIC BILL NUMBER                           *         
***********************************************************************         
         SPACE 1                                                                
GETBNO   NTR1  ,                                                                
         SR    R0,R0                                                            
         IC    R0,BCTMONP+1        GET PWOS MONTH                               
         SRDL  R0,4                AND CONVERT TO EBCDIC                        
         STC   R0,UPDBMNTH                                                      
         SRL   R1,28                                                            
         STC   R1,UPDBMNTH+1                                                    
         OC    UPDBMNTH,=X'F0F0'                                                
*                                                                               
GETBNO02 LA    R2,IOKEY                                                         
         TM    BCJOBSTA,BCJOBBNO   TEST OFFICE LEVEL BILL NUMBERS               
         BZ    GETBNO10                                                         
         USING OGRRECD,R2          READ PRODUCTION OFFICE RECORD                
         XC    OGRKEY,OGRKEY                                                    
         MVI   OGRKTYP,OGRKTYPQ                                                 
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY,CUABIN                                                   
         MVC   OGRKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   OGRKOFC,CSOFFICE                                                 
         L     R1,AIO5                                                          
         CLC   OGRKEY,0(R1)                                                     
         BE    GETBNO04                                                         
         L     R1,=A(IOREAD+IOACCDIR+IO5)                                       
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=A(IOGETRUP+IOACCMST+IO5)                                     
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO5                                                          
*                                                                               
GETBNO04 LA    R1,OGRRFST-OGRRECD(R1)                                           
         USING BNCELD,R1                                                        
         SR    R0,R0                                                            
GETBNO06 CLI   BNCEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BNCEL,BNCELQ                                                     
         BE    *+14                                                             
         IC    R0,BNCLN                                                         
         AR    R1,R0                                                            
         B     GETBNO06                                                         
         MVC   LSUBILLV,BNCLBIL                                                 
         CLI   BCP102,C'N'         TEST RESET BILL NUMBER                       
         BE    GETBNO08                                                         
         CLC   UPDBMNTH,LSUBILLV                                                
         BE    GETBNO08                                                         
         MVC   LSUBILLV(L'UPDBMNTH),UPDBMNTH                                    
         MVC   LSUBILLV+L'UPDBMNTH(L'BNCRSET),BNCRSET                           
GETBNO08 PACK  BODUB1,LSUBILLV                                                  
         AP    BODUB1,=P'1'                                                     
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  BNCLBIL,BODUB1                                                   
         B     GETBNO22                                                         
*                                                                               
GETBNO10 TM    BCJOBSTA,BCJOBBNL   TEST LEDGER LEVEL BILL NUMBERS               
         BZ    GETBNO12                                                         
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN                                                   
         MVC   LDGKUNT(L'BCCPYPRD),BCCPYPRD                                     
         B     GETBNO14                                                         
*                                                                               
         USING PMDRECD,R2                                                       
GETBNO12 TM    BCJOBSTA,BCJOBBNM   TEST MEDIA LEVEL BILL NUMBERS                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   PMDKEY,BCSPACES                                                  
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUABIN                                                   
         SR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         LA    RE,BCJOBCOD(RE)                                                  
         MVC   PMDKMED,0(RE)                                                    
*                                                                               
GETBNO14 L     R1,AIO5                                                          
         CLC   PMDKEY,0(R1)                                                     
         BE    GETBNO16                                                         
         L     R1,=A(IOREAD+IOACCDIR+IO5)                                       
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=A(IOGETRUP+IOACCMST+IO5)                                     
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO5                                                          
*                                                                               
GETBNO16 LA    R1,PMDRFST-PMDRECD(R1)                                           
         USING PMDELD,R1                                                        
         SR    R0,R0                                                            
GETBNO18 CLI   PMDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PMDEL,PMDELQ                                                     
         BE    *+14                                                             
         IC    R0,PMDLN                                                         
         AR    R1,R0                                                            
         B     GETBNO18                                                         
         MVC   LSUBILLV,PMDLBILL                                                
         CLI   BCP102,C'N'         TEST RESET BILL NUMBER                       
         BE    GETBNO20                                                         
         CLC   UPDBMNTH,LSUBILLV                                                
         BE    GETBNO20                                                         
         MVC   LSUBILLV(L'UPDBMNTH),UPDBMNTH                                    
         MVC   LSUBILLV+L'UPDBMNTH(L'PMDRBILL),PMDRBILL                         
GETBNO20 PACK  BODUB1,LSUBILLV                                                  
         AP    BODUB1,=P'1'                                                     
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  PMDLBILL,BODUB1                                                  
*                                                                               
         USING PBRRECD,R2                                                       
GETBNO22 XC    PBRPAS,PBRPAS       TEST BILL NUMBER ALREADY USED                
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ                                                 
         MVC   PBRPBLNO,LSUBILLV                                                
         MVI   PBRPIND,PBRPILVE                                                 
         GOTO1 AIO,IOHIUP+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PBRPAS(PBRPUSER-PBRPAS),IOKEYSAV                                 
         BE    GETBNO02                                                         
*                                                                               
GETBNOX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A BATCH REFERENCE                               *         
*                                                                     *         
* NTRY - R1 POINTS TO PARAMETER LIST AS FOLLOWS                       *         
*                                                                     *         
*        1/1-4 A(INPUT FIELD HEADER)                                  *         
*        2/1-4 A(OUTPUT FIELD)                                        *         
***********************************************************************         
         SPACE 1                                                                
VALBRF   NTR1  ,                                                                
         LR    R2,R1                                                            
         L     R1,0(R2)                                                         
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL                                                            
         BNE   VALBRFX                                                          
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         LA    R1,FVIFLD                                                        
*                                                                               
VALBRF02 CLI   0(R1),C'A'                                                       
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     VALBRFN                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,VALBRF02                                                      
         L     R1,4(R2)                                                         
         MVC   0(L'LSUBILRF,R1),FVIFLD                                          
         B     VALBRFY                                                          
*                                                                               
VALBRFN  CLI   *,0                                                              
         B     VALBRFX                                                          
VALBRFY  CLI   *+1,0                                                            
VALBRFX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO PUT OFFICE/MEDIA RECORD TO UPDATE BILL NUMBER            *         
***********************************************************************         
         SPACE 1                                                                
PUTBNO   NTR1  ,                                                                
         L     R1,=A(IOPUTREC+IOACCMST+IO5)                                     
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
PUTBNOX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALCULATE NET/COMMISSION AMOUNTS                         *         
*                                                                     *         
* NTRY - IO1=BILL HEADER RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
GETAMT   NTR1  ,                                                                
         L     R2,AIO1                                                          
         USING PBRRECD,R2                                                       
         ZAP   UPDBHAPN,BCPZERO    SET ALLOCATION (NET) TO ZERO                 
         ZAP   UPDBHAPC,BCPZERO    SET ALLOCATION (COMMISSION) TO ZERO          
*                                                                               
         LA    R1,PBRRFST                                                       
         USING NDXELD,R1           R1=A(INDEX ELEMENT)                          
         XR    RF,RF                                                            
         CLI   NDXEL,NDXELQ                                                     
         BE    *+12                                                             
         IC    RF,NDXLN                                                         
         BXH   R1,RF,*-12                                                       
         XR    R0,R0                                                            
         ICM   R0,1,NDXACTV        R0=NO. OF ACTIVE ENTRIES                     
         BZ    EXIT                                                             
         LA    R3,NDXINDX          R3=A(LIST OF ACTIVE ENTRIES)                 
         DROP  R1                                                               
*                                                                               
GETAMT02 MVC   IOKEY,PBRKEY                                                     
         MVC   IOKEY+(PBRKPARA-PBRRECD)(L'PBRKPARA),0(R3)                       
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         L     R1,AIO2                                                          
         LA    R1,PBRRFST-PBRRECD(R1)                                           
         USING PGHELD,R1                                                        
         SR    RF,RF                                                            
         CLI   PGHEL,PGHELQ                                                     
         BE    *+12                                                             
         IC    RF,PGHLN                                                         
         BXH   R1,RF,*-12                                                       
         AP    UPDBHAPN,PGHNET     UPDATE AMOUNTS                               
         AP    UPDBHAPC,PGHCOM                                                  
         LA    R3,1(R3)                                                         
         BCT   R0,GETAMT02                                                      
         DROP  R1                                                               
*                                                                               
GETAMTX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A BATCH MONTH                                   *         
*                                                                     *         
* NTRY - R1 POINTS TO PARAMETER LIST AS FOLLOWS:-                     *         
*                                                                     *         
* PARM - 1/0   BATCH INPUT TYPE                                       *         
*        1/1-3 A(INPUT FIELD HEADER)                                  *         
*        2/1-3 A(OUTPUT FIELD)                                        *         
***********************************************************************         
         SPACE 1                                                                
VALBMO   NTR1  ,                                                                
         LR    R2,R1                                                            
         SR    R1,R1                                                            
         ICM   R1,7,1(R2)                                                       
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL                                                            
         BNE   VALBMOX                                                          
         MVC   CSBSECL,BCCPYEL+(CPYBSEC-CPYELD)                                 
         LA    R3,BOWORK1                                                       
         USING BMONVALD,R3                                                      
         GOTO1 VBMONVAL,BCPARM,(FVILEN,FVIFLD),(0(R2),ACOM),           X        
               (CULANG,BMONVALD),(CUABIN,0)                                     
         MVC   BOBYTE1,0(R1)       BATCH SECURITY LEVEL                         
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BE    *+14                                                             
         MVC   FVMSGNO,BMOMSG                                                   
         B     VALBMON                                                          
*                                                                               
         MVC   BODUB1,BMOMOSP                                                   
         MVI   BODUB1+L'BMOMOSP,X'01'                                           
         BAS   RE,CLRFLD                                                        
         LA    RF,L'FVIHDR(RF)                                                  
         GOTO1 VDATCON,BCPARM,(1,BODUB1),(9,(RF))                               
*                                                                               
VALBMO08 CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BNE   VALBMON                                                          
         ICM   R1,7,5(R2)                                                       
         MVC   0(L'LSUBILMO,R1),BMOMOSP                                         
         MVC   L'LSUBILMO(L'LSUBILMC,R1),BMOMOSC                                
         MVC   CSBSECL,BOBYTE1     SET BATCH SECURITY                           
         B     VALBMOY                                                          
*                                                                               
VALBMON  CLI   *,0                                                              
         B     VALBMOX                                                          
VALBMOY  CLI   *+1,0                                                            
VALBMOX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE ACCRUAL MONTHS AND AMOUNT/PERCENTS              *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALACCR  NTR1  ,                                                                
         LA    R2,UPDACC1H                                                      
         LA    R3,UPDMNTHS                                                      
         USING UPDMONTB,R3                                                      
PREVMOA  USING UPDMONTB,RE                                                      
VACCR10  XC    0(L'UPDMNTH,R3),0(R3)                                            
         LA    RF,UPDPFKH                                                       
         CR    R2,RF                                                            
         BE    VACCR30             ALL DONE                                     
         XC    UPDMONTH,UPDMONTH                                                
         ZAP   UPDMAMT,BCPZERO                                                  
         STCM  R2,15,FVADDR                                                     
         CLI   (FVILEN-FVIHDR)(R2),0                                            
         BE    VACCR25             NOTHING IN THIS FIELD                        
         SR    R0,R0                                                            
         ICM   R0,12,=C',='                                                     
         ICM   R0,2,BCSLASH                                                     
         GOTO1 VSCANNER,BODMCB,(R2),(2,BOWORK1),(R0)                            
         CLI   BODMCB+4,2                                                       
         BNE   VACCRERR            THERE MUST BE TWO BITS                       
         LA    R4,BOWORK1                                                       
         USING SCANBLKD,R4                                                      
         GOTO1 VPERVAL,BODMCB,(SC1STLEN,SC1STFLD),(CULANG,BOELEM)               
         TM    4(R1),X'01'                                                      
         BO    VACCRERM            BAD DATE                                     
         CLC   =Y(1),BOELEM+(PVALNMNS-PERVALD)                                  
         BNE   VACCRERM            ONLY ALLOW 1 MONTH                           
         MVC   UPDMOA,BOELEM+(PVALPSTA+0-PERVALD)                               
         MVC   UPDMNTH(1),BOELEM+(PVALPSTA+0-PERVALD)                           
         OI    UPDMNTH,X'F0'       YEAR                                         
         MVC   UPDMNTH+1(1),BOELEM+(PVALPSTA+1-PERVALD)                         
         TR    UPDMNTH+1(1),UPDMONTR                                            
         LA    RE,UPDMNTHS         TEST THIS IS THE FIRST MONTH                 
         CR    RE,R3                                                            
         BNE   VACCR12                                                          
         CLC   LSUBILMO,UPDMOA     FIRST MOA MUST EQUAL BILL MOA                
         BNE   VACCRERF                                                         
         B     VACCR18                                                          
*                                                                               
VACCR12  CR    RE,R3                                                            
         BE    VACCR18                                                          
         CLC   UPDMOA,LSUBILMO                                                  
         BNH   VACCRERG            ACCRUALS MUST BE FORWARD                     
         CLC   UPDMOA,PREVMOA.UPDMOA                                            
         BE    VACCRERD            DUPLICATED MONTH                             
         LA    RE,L'UPDMNTHS(RE)                                                
         B     VACCR12                                                          
*                                                                               
VACCR18  LA    R4,L'SCLINE(R4)     NEXT SCANNER ENTRY IS PERCENT                
         ZAP   UPDMPCT,BCPZERO                                                  
         CLI   SC1STFLD,C'*'       * MEANS EQUAL - WORK IT OUT AT END           
         BE    VACCR20                                                          
         MVC   FVIFLD(L'SC1STFLD),SC1STFLD                                      
         MVC   FVILEN,SC1STLEN                                                  
         GOTO1 AVALAMT,BODMCB,(X'84',FVIHDR),(L'UPDMPCT,UPDMPCT)                
         BNE   VACCRERA                                                         
         CP    UPDMPCT,=P'1000000'                                              
         BNL   VACCRERA                                                         
         CP    UPDMPCT,BCPZERO                                                  
         BNH   VACCRERA                                                         
*                                                                               
VACCR20  LA    R3,L'UPDMNTHS(R3)                                                
VACCR25  LA    R2,(UPDACC2H-UPDACC1H)(R2)                                       
         B     VACCR10                                                          
*                                                                               
VACCR30  LA    R3,UPDMNTHS         ENSURE IT ALL COMES TO 100%                  
         LA    R2,UPDACC1H         ENSURE IT ALL COMES TO 100%                  
         STCM  R2,15,FVADDR                                                     
         OC    UPDMNTH,UPDMNTH     TEST ANYTHIN AT ALL                          
         BZ    VACCRY              NO - FINE                                    
         ZAP   BODUB1,BCPZERO      TOTAL PERCENTS                               
         ZAP   BODUB2,BCPZERO      NO OF MONTHS FOR EQUAL SPLIT                 
VACCR40  OC    UPDMNTH,UPDMNTH                                                  
         BZ    VACCR50             FIN                                          
         AP    BODUB1,UPDMPCT                                                   
         CP    UPDMPCT,BCPZERO                                                  
         BNE   *+10                                                             
         AP    BODUB2,=P'1'                                                     
         LA    R3,L'UPDMNTHS(R3)                                                
         B     VACCR40                                                          
VACCR50  CP    BODUB2,BCPZERO      TEST ANY NULL MONTHS TO FILL IN              
         BNE   VACCR60             YES                                          
         CP    BODUB1,=P'1000000'  THEN THE TOTAL MUST BE EXACTLY 100%          
         BE    VACCR81                                                          
         B     VACCRERP                                                         
VACCR60  CP    BODUB1,=P'1000000'  TEST 100 ALREADY INPUT                       
         BNL   VACCRERP                                                         
         XC    BODUB3,BODUB3       DIVIDE REM. % BY # OF NULL MONTHS            
         ZAP   BODUB4,=P'1000000'                                               
         SP    BODUB4,BODUB1                                                    
         DP    BODUB3(2*L'BODUB3),BODUB2                                        
*                                                                               
         LA    R3,UPDMNTHS                                                      
VACCR70  CP    UPDMPCT,BCPZERO                                                  
         BNE   VACCR80                                                          
         ZAP   UPDMPCT,BODUB3                                                   
         AP    UPDMPCT,BODUB4                                                   
         ZAP   BODUB4,BCPZERO      REMAINDER ADDED TO FIRST ONE ONLY            
VACCR80  LA    R3,L'UPDMNTHS(R3)                                                
         OC    UPDMNTH,UPDMNTH                                                  
         BNZ   VACCR70                                                          
*                                  RE-DISPLAY MONTHS AND PERCENTS               
VACCR81  LA    R3,UPDMNTHS                                                      
         LA    R2,UPDACC1H                                                      
VACCR82  XC    8(L'UPDACC1,R2),8(R2)                                            
         MVC   BODUB1(L'UPDMOA),UPDMOA                                          
         MVI   BODUB1+L'UPDMOA,X'01'                                            
         GOTO1 VDATCON,BODMCB,(1,BODUB1),(18,8(R2))                             
         LA    R4,8+L'UPDACC1-1(R2)                                             
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   1(1,R4),BCSLASH                                                  
         LA    R4,2(R4)                                                         
         ZAP   BODUB1,UPDMPCT                                                   
         CURED BODUB1,(7,0(R4)),4,ALIGN=LEFT,DMCB=BODMCB                        
         AR    R4,R0               ADD EDITED LENGTH                            
         SR    R4,R2                                                            
         SH    R4,=Y(L'FVIHDR)     TAKE OFF LENGTH OF HEADER                    
         STC   R4,5(R2)            SET NEW INPUT LENGTH                         
         OI    FVOIND-FVIHDR(R2),FVOXMT                                         
         LA    R3,L'UPDMNTHS(R3)                                                
         LA    R2,(UPDACC2H-UPDACC1H)(R2)                                       
         OC    UPDMNTH,UPDMNTH                                                  
         BZ    VACCRY                                                           
         LA    RF,UPDPFKH                                                       
         CR    R2,RF                                                            
         BNE   VACCR82                                                          
         B     VACCRY                                                           
*                                                                               
VACCRERR MVC   FVMSGNO,=AL2(AE$INVIF)    INVALID INPUT                          
         B     VACCRN                                                           
VACCRERA MVC   FVMSGNO,=AL2(AE$INAMT)    INVALID AMOUNT                         
         B     VACCRN                                                           
VACCRERP MVC   FVMSGNO,=AL2(AE$INPCT)    NOT EQUAL TO 100%                      
         B     VACCRN                                                           
VACCRERD MVC   FVMSGNO,=AL2(AE$DUPIF)    DUPLICATE INPUT                        
         B     VACCRN                                                           
VACCRERM MVC   FVMSGNO,=AL2(AE$INMON)    INVALID MONTH                          
         B     VACCRN                                                           
VACCRERF MVC   FVMSGNO,=AL2(AE$FMEBM)    FIRST MOA MUST=BILLMOA                 
         B     VACCRN                                                           
VACCRERG MVC   FVMSGNO,=AL2(AE$AMABM)    ACCRUAL MOA MUST BE FUTURE             
         B     VACCRN                                                           
VACCRN   CLI   *,0                                                              
         B     VACCRX                                                           
VACCRY   CLI   *+1,0                                                            
VACCRX   B     EXIT                                                             
*                                                                               
UPDMONTR DC    C'.123456789......ABC'                                           
         EJECT                                                                  
***********************************************************************         
* CLEAR AND TRANSMIT AN INPUT FIELD ADDRESSED BY FVADDR               *         
***********************************************************************         
         SPACE 1                                                                
CLRFLD   L     RF,FVADDR                                                        
CLRFLD2  OI    FVOIND-FVIHDR(RF),FVOXMT                                         
         SR    R1,R1                                                            
         IC    R1,FVTLEN-FVIHDR(RF)                                             
         SH    R1,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(RF),FVAXTND                                        
         BZ    *+8                                                              
         SH    R1,=Y(L'FVIHDR)                                                  
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         XC    L'FVIHDR(0,RF),L'FVIHDR(RF)                                      
CLRFLDX  BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* READ DRAFT BILL RECORD                                              *         
***********************************************************************         
         SPACE 1                                                                
RDDFBIL  NTR1                                                                   
         LA    R2,IOKEY            READ DRAFT BILL RECORD & DISPLAY             
         USING PBRRECD,R2                                                       
         XC    PBRPAS,PBRPAS       BUILD KEY OF PASSIVE POINTER                 
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ                                                 
         OI    BCDUB+L'BCDUB-1,X'0F'                                            
         UNPK  PBRPBLNO,BCDUB                                                   
         CLC   PBRPBLNO,LSUBILDF                                                
         MVC   LSUBILDF,PBRPBLNO                                                
         BE    *+12                SAME BILL NUMBER                             
         NI    UPDDSCAH+(FVIIND-FVIHDR),FF-(FVIVAL)                             
         NI    UPDSRCAH+(FVIIND-FVIHDR),FF-(FVIVAL)                             
         MVI   PBRPIND,PBRPIDFT                                                 
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PBRPAS(PBRPUSER-PBRPAS),IOKEYSAV                                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BLNOF)                                           
         B     EXIT                                                             
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
**********************************************************************          
*              LITERALS AND CONSTANTS FOR VALIDATION CSECT           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
SRCHLEDG DS    0C                                                               
DSCTLEDG DS    0C                                                               
ULINCOME DC    C'SI'                                                            
ULEXPENS DC    C'SE'                                                            
         DC    X'00'                                                            
ULBALSHT DC    C'SQ'                                                            
ACCMAST  DC    C'ACCMST  '                                                      
*                                                                               
AROUT    DS    0A                  ENTERABLE ROUTINES                           
         DC    A(POST)                                                          
         DC    A(POSTINIT)                                                      
         DC    A(ALLXFR)                                                        
         DC    A(POSTACCR)                                                      
         DC    A(POSTTRNA)                                                      
         DC    A(POSTDIFF)                                                      
AROUTN   EQU   (*-AROUT)/L'AROUT                                                
         EJECT                                                                  
**********************************************************************          
*              READ TRANSACTION DIRECTORY RECORDS                    *          
*              BUILD TABLE OF DISK ADDRESSES AND PENDING STATUS      *          
**********************************************************************          
         SPACE 1                                                                
POST     CSECT                                                                  
         NMOD1 0,**CLB6A*,R8,R7,R6                                              
         LR    RC,R1                                                            
         MVC   CSBILNUM,LSUBILDF   TRANSACTION REF IS DRAFT BILL NUM            
         USING TRNRECD,R2                                                       
         ZAP   UPDTOTLS,BCPZERO    INITIALISE BILL TOTALS                       
         MVC   UPDTOTLS+L'UPDTOTLS(UPDTOTLL-L'UPDTOTLS),UPDTOTLS                
         LA    R0,UPDMAXDA         SET MAXIMUM LOOP COUNT                       
         MVI   BOBYTE1,0           BOBYTE1=PENDING MASK                         
         CLI   UPDBLNG,YESQ        TEST BILL PENDINGS REQUIRED                  
         BNE   *+8                                                              
         OI    BOBYTE1,TRNSBILP                                                 
         OC    LSUTRFRF,LSUTRFRF   TEST TRANSFER PENDINGS REQUIRED              
         BZ    *+8                                                              
         OI    BOBYTE1,TRNSXFRP                                                 
         OC    LSUWOFRF,LSUWOFRF   TEST WRITE-OFF PENDINGS REQUIRED             
         BZ    *+8                                                              
         OI    BOBYTE1,TRNSWOFP                                                 
         OC    UPDRECRF,UPDRECRF                                                
         BZ    *+8                                                              
         OI    BOBYTE1,TRNSWOFP                                                 
         MVI   UPDDATAB,FF         SET E-O-L MARKER                             
         LA    R4,UPDDATAB         R4=A(D/A TABLE)                              
         LA    R2,IOKEY                                                         
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'TRNKUNT+L'TRNKLDG),BCCPYEL+(CPYPROD-CPYELD)            
         MVC   TRNKACT,BCJOBCOD                                                 
         LA    R1,IOACCDIR+IOHI+IO1                                             
         B     *+8                                                              
POST02   LA    R1,IOACCDIR+IOSQ+IO1                                             
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TRNKCULA,IOKEYSAV+TRNKCULA-TRNKEY                                
         BNE   POST06              CHANGE OF JOB - FINISHED                     
         CLC   TRNKDATE,BCSPACES                                                
         BE    POST02              NOT A TRANSACTION                            
         MVC   0(L'TRNKDA,R4),TRNKDA                                            
         MVC   L'TRNKDA(L'TRNKSTA2,R4),TRNKSTA2                                 
         NC    L'TRNKDA(L'TRNKSTA2,R4),BOBYTE1                                  
         TM    TRNKSTAT,TRNSDRFT   TEST IF DRAFT TRANSACTION                    
         BNO   POST04                                                           
         CLI   TRNKSTYP,169        TEST IF CBIL ORIGIN BT8                      
         BNE   POST04                                                           
         OI    L'TRNKDA(R4),CBILFEEA                                            
POST04   TM    L'TRNKDA(R4),TRNSXFRP+TRNSWOFP+TRNSBILP+CBILFEEA                 
         BZ    POST02                                                           
         LA    R4,L'TRNKDA+L'TRNKSTA2(R4)                                       
         BCT   R0,POST02                                                        
         DC    H'0'                RUN OFF TABLE END                            
*                                                                               
POST06   MVI   0(R4),FF            SET NEW E-O-L MARKER                         
         CLI   UPDDATAB,FF         TEST ANYTHING THERE (MUST BE)                
         BNE   POST08                                                           
         LA    R0,UPDJOBH          SET CURSOR TO JOB                            
         STCM  R0,15,FVADDR        YOU HAVEN'T DONE ANYTHING YET                
         MVC   FVMSGNO,=AL2(AE$YHDAY)                                           
         B     EXIT2                                                            
POST08   B     PSTBIL              POST ANY BILLING FIRST                       
         EJECT                                                                  
**********************************************************************          
*              BILLING UPDATE                                        *          
*              WRITE TOTALS RECORDS TO TSAR:                         *          
*                1) NET/COMM BY WORKCODE                             *          
*                2) GROSS BY VAT TYPE                                *          
*                3) NET BY SK ACCOUNT (FOR SI TRANSFER)              *          
*              UPDATE PTAELS IN ALLOCATED TRANSACTIONS               *          
**********************************************************************          
         SPACE 1                                                                
PSTBIL   CLI   UPDBLNG,YESQ        TEST BILLING IS ON                           
         BNE   PSTB900             NO - UPDATE JOB RECORD THOUGH                
         ZAP   UPDCBAPN,BCPZERO    CLEAR NET TOTAL                              
         ZAP   UPDCBAPC,BCPZERO    CLEAR COMMISSION TOTAL                       
         ZAP   UPDCBALN,BCPZERO    CLEAR NET BILLCURR TOTAL                     
         ZAP   UPDCBALC,BCPZERO    CLEAR COM BILLCURR TOTAL                     
*                                                                               
         LA    R4,UPDDATAB         START OF D/A LIST                            
PSTB020  ST    R4,UPDLSTDA         SAVE FOR NEXT                                
         CLI   0(R4),FF                                                         
         BE    PSTB500             ALL DONE- READ TSAR RECORDS BACK             
         TM    L'TRNKDA(R4),TRNSBILP                                            
         BZ    PSTB240             NO BILL ALLOCATION PENDING                   
         MVC   IODAOVER,0(R4)                                                   
         GOTO1 AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO3                                                          
         LA    RE,TRNKWORK                                                      
         CLC   TRNKWORK,=C'**'                                                  
         BNE   PSTB025                                                          
         SR    R0,R0                                                            
         LA    R1,TRNRFST-TRNRECD(R2)                                           
PSTB022  IC    R0,OAMLN-OAMELD(R1)                                              
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    PSTB025                                                          
         CLI   0(R1),OAMELQ                                                     
         BNE   PSTB022                                                          
         LA    RE,(OAMWORK-OAMELD)(R1)                                          
*                                                                               
PSTB025  CLC   UPDLSTWC,0(RE)      TEST HAD THIS WORKCODE BEFORE                
         MVC   UPDLSTWC,0(RE)                                                   
         BE    PSTB060                                                          
         GOTO1 AGETOPT,BODMCB,AIO3                                              
*                                                                               
         MVI   UPDWCSRG,C'Y'       SET THIS WORKCODE SURCHARGABLE               
*&&UK                                                                           
         LA    RE,IOKEY                                                         
         USING WCORECD,RE          READ WORKCODE FOR SURCHARGE STATUS           
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   WCOKWRK,UPDLSTWC                                                 
         DROP  RE                                                               
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING WCOELD,RF                                                        
         SR    R0,R0                                                            
         L     RF,AIO1                                                          
         LA    RF,ACTRFST-ACTRECD(RF)                                           
         B     *+10                                                             
PSTB030  IC    R0,WCOLN                                                         
         AR    RF,R0                                                            
         CLI   WCOEL,0             TEST E-O-R                                   
         BE    PSTB060                                                          
         CLI   WCOEL,WCOELQ                                                     
         BNE   PSTB030                                                          
         TM    WCOSTAT2,WCOSNSCH   TEST WORKCODE NON-SURCHARGABLE               
         BNO   PSTB060                                                          
         MVI   UPDWCSRG,C'N'                                                    
         DROP  RF                                                               
*&&                                                                             
*                                                                               
PSTB060  SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,AIO3,AGOPBLK,ACOM,(R0),LSPRATA,0                 
*                                                                               
         XC    TLKEY,TLKEY         GET WORKCODE TSAR RECORD                     
         MVI   TLKSES,TLKUWCQ                                                   
         L     RF,AGOPBLK                                                       
*&&UK                                                                           
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         MVC   TLKUACT,GOINCAC+1-GOBBLOCK(RF)                                   
*&&                                                                             
*&&US                                                                           
         MVC   TLKUACT,GOTBC+1-GOBLOCK(RF)                                      
         CLC   TLKUACT,BCSPACES                                                 
         BH    *+10                                                             
         MVC   TLKUACT,LSUMEDIN+1                                               
*&&                                                                             
         MVC   TLKUWC,TRNKWORK                                                  
         GOTO1 ATSARIO,TSARDH                                                   
         BE    PSTB130             TEST ON FILE                                 
         LA    R3,IOKEY                                                         
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,BCSPACES     READ INCOME ACCOUNT FOR COST BYTE            
         L     RF,AGOPBLK                                                       
*&&UK                                                                           
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         MVC   ACTKCULA,GOINCAC-GOBBLOCK(RF)                                    
*&&                                                                             
*&&US                                                                           
         MVC   ACTKCULA,GOTBC-GOBLOCK(RF)                                       
         CLC   ACTKCULA,BCSPACES                                                
         BH    *+10                                                             
         MVC   ACTKCULA,LSUMEDIN                                                
*&&                                                                             
         GOTO1 AGETACT,0                                                        
         BE    PSTB120                                                          
         CLI   CSACT,ACTDRA                                                     
         BE    EXIT2                                                            
         DC    H'0'                TOO LATE FOR AN ERROR MESSAGE                
PSTB120  XC    TLKEY,TLKEY         BUILD NEW RECORD                             
         XC    TLDU,TLDU           CLEAR RECORD                                 
         MVC   TLRLEN,=Y(TLUPDLNQ)                                              
         MVI   TLKSES,TLKUWCQ                                                   
         MVC   TLKUWC,TRNKWORK                                                  
         MVC   TLKUACT,IOKEY+1                                                  
         L     RF,AGOPBLK                                                       
         MVC   TLDUVT1,GOTAXCOD-GOBLOCK(RF)                                     
         BAS   RE,SETVATBL         SET VATABLE AMOUNT IN BODUB3                 
         ZAP   TLDUVAM1,BODUB3                                                  
         MVC   TLDUANAL,BCSPACES   CLEAR COSTING ANALYSIS                       
*&&UK*&& MVC   TLDUANAL(L'ACCOST),ACCOST                                        
*&&US                                                                           
         ICM   RF,15,ACASPA                                                     
         BZ    *+10                                                             
         MVC   TLDUANAL,SPAAULA-SPAELD(RF)                                      
*&&                                                                             
         MVC   TLDUACOM,PP$ACOMM                                                
         MVC   TLDUFCOM,PP$FCOMM                                                
         MVC   TLDUANET,PP$AALLO                                                
         MVC   TLDUFNET,PP$FALLO                                                
         AP    UPDTOTFN,PP$FALLO   THIS TOTAL NEEDED FOR 99 POSTING             
         LA    R1,TSAADD                                                        
         B     PSTB140                                                          
PSTB130  AP    TLDUACOM,PP$ACOMM   UPDATE EXISTING RECORD                       
         AP    TLDUFCOM,PP$FCOMM                                                
         AP    TLDUANET,PP$AALLO                                                
         AP    TLDUFNET,PP$FALLO                                                
         AP    UPDTOTFN,PP$FALLO   THIS TOTAL NEEDED FOR 99 POSTING             
         BAS   RE,SETVATBL         GET VATABLE AMOUNT IN BODUB3                 
         L     RF,AGOPBLK                                                       
         LA    RE,TLDUVT1                                                       
         LA    R0,TLDUVTMX                                                      
PSTB132  CLI   0(RE),0                                                          
         BE    PSTB134                                                          
         CLC   0(L'TLDUVT1,RE),GOTAXCOD-GOBLOCK(RF)                             
         BE    PSTB136                                                          
         LA    RE,(TLDUVT2-TLDUVT1)(RE)                                         
         BCT   R0,PSTB132                                                       
         DC    H'0'                                                             
PSTB134  MVC   0(L'TLDUVT1,RE),GOTAXCOD-GOBLOCK(RF)                             
         ZAP   L'TLDUVT1(L'TLDUVAM1,RE),BCPZERO                                 
PSTB136  AP    L'TLDUVT1(L'TLDUVAM1,RE),BODUB3                                  
         LA    R1,TSAWRT                                                        
PSTB140  GOTO1 ATSARIO,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    UPDCBAPN,PP$AALLO   ACCUMULATE AGYCURR NET TOTAL                 
         AP    UPDCBAPC,PP$ACOMM   ACCUMULATE AGYCURR COMM TOTAL                
         LA    RF,PP$AALLO         AGENCY NET ALLOCATION                        
         CLC   CSBILCUR,CSCPYCUR                                                
         BE    *+8                                                              
         LA    RF,PP$FALLO         BILLING NET ALLOCATION                       
         AP    UPDCBALN,0(L'PP$AALLO,RF)                                        
         AP    UPDCBALC,PP$ACOMM-PP$AALLO(L'PP$AALLO,RF)                        
         AP    UPDCBAPD,PP$ADSCB   ACCUMULATE AGYCURR DISC TOTAL                
         MVC   UPDSANAL,TLDUANAL   SAVE COSTING CODE                            
*                                                                               
         CLC   UPDSANAL,BCSPACES   TEST ANY COSTING CODE                        
         BNH   PSTB148                                                          
         XC    TLKEY,TLKEY         GET COSTING TSAR RECORD                      
         MVI   TLKSES,TLKUCSTQ                                                  
         MVC   TLKUCOST,UPDSANAL                                                
         GOTO1 ATSARIO,TSARDH                                                   
         BE    PSTB144                                                          
         XC    TLKEY,TLKEY         BUILD NEW RECORD                             
         XC    TLDU,TLDU           CLEAR RECORD                                 
         MVC   TLRLEN,=Y(TLUPDLNQ)                                              
         MVI   TLKSES,TLKUCSTQ                                                  
         MVC   TLKUCOST,UPDSANAL                                                
         MVC   TLDUCNET,PP$AALLO                                                
         MVC   TLDUCCOM,PP$ACOMM                                                
         LA    R1,TSAADD           ADD NEW TSAR RECORD                          
         B     PSTB146                                                          
PSTB144  AP    TLDUCNET,PP$AALLO                                                
         AP    TLDUCCOM,PP$ACOMM                                                
         LA    R1,TSAWRT           WRITE BACK UPDATED RECORD                    
PSTB146  GOTO1 ATSARIO,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PSTB148  TM    BCGLOB1,BCGLVAT                                                  
         BZ    PSTB168                                                          
         XC    TLKEY,TLKEY         GET VAT TYPE TSAR RECORD                     
         MVI   TLKSES,TLKUVATQ                                                  
         L     RF,AGOPBLK                                                       
         MVC   TLKUVATT,GOTAXCOD-GOBLOCK(RF)                                    
         GOTO1 ATSARIO,TSARDH                                                   
         BE    PSTB150                                                          
         XC    TLKEY,TLKEY         BUILD NEW RECORD                             
         MVC   TLRLEN,=Y(TLUPDLNQ)                                              
         MVI   TLKSES,TLKUVATQ                                                  
         L     RF,AGOPBLK                                                       
         MVC   TLKUVATT,GOTAXCOD-GOBLOCK(RF)                                    
         MVC   TLDUVATA,PP$AALLO   VATABLE IS NET PLUS COMM                     
         AP    TLDUVATA,PP$ACOMM                                                
         ZAP   TLDUVAT,BCPZERO     INITIALISE VAT AMOUNT FIELD                  
         ZAP   TLDUVVSG,BCPZERO                                                 
         CLI   UPDWCSRG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   TLDUVVSG,TLDUVATA   SURCHARGABLE AMOUNT                          
         LA    R1,TSAADD                                                        
         B     PSTB160                                                          
*                                                                               
PSTB150  AP    TLDUVATA,PP$AALLO   UPDATE EXISTING RECORD                       
         AP    TLDUVATA,PP$ACOMM                                                
         CLI   UPDWCSRG,C'Y'                                                    
         BNE   *+16                                                             
         AP    TLDUVVSG,PP$AALLO                                                
         AP    TLDUVVSG,PP$ACOMM                                                
         LA    R1,TSAWRT                                                        
PSTB160  GOTO1 ATSARIO,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRNELD,R3                                                        
PSTB168  LA    R3,TRNRFST          FIND ANY INCOME SUSPENSE ACCOUNT             
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TRNTYPE,8           IF INTERNAL INVOICE USE SK CONTRA            
         BNE   PSTB170                                                          
         CLC   =C'SK',TRNKULC                                                   
         BNE   PSTB200                                                          
         LA    R4,TRNKULC                                                       
         B     PSTB175                                                          
*                                                                               
         USING APEELD,RF           FIND SK ATTRIBUTE                            
PSTB170  LA    RF,TRNEL                                                         
         SR    R0,R0                                                            
PSTB172  IC    R0,APELN                                                         
         AR    RF,R0                                                            
         CLI   APEEL,0             NO SK ATTRIBUTE                              
         BE    PSTB200                                                          
         CLI   APEEL,APEELQ                                                     
         BNE   PSTB172                                                          
         CLI   APELN,APENACT-APEELD                                             
         BNH   PSTB200                                                          
         CLC   =C'SK',APENACT                                                   
         BNE   PSTB200                                                          
         LA    R4,BOWORK1                                                       
         MVC   0(L'APENACT,R4),BCSPACES                                         
         IC    RE,APELN                                                         
         SH    RE,=Y(APENACT+1-APEELD)                                          
         EX    RE,*+4                                                           
         MVC   0(0,R4),APENACT                                                  
         DROP  RF                                                               
PSTB175  XC    TLKEY,TLKEY         GET TRANSFER RECORD                          
         MVI   TLKSES,TLKUTRFQ                                                  
         MVC   TLKUTACT,0(R4)                                                   
         MVC   TLKUTDAT,TRNKDATE                                                
         MVC   TLKUTREF,TRNKREF                                                 
         GOTO1 ATSARIO,TSARDH                                                   
         BE    PSTB180                                                          
         XC    TLKEY,TLKEY         BUILD NEW RECORD                             
         MVC   TLRLEN,=Y(TLUPDLNQ)                                              
         MVI   TLKSES,TLKUTRFQ                                                  
         MVC   TLKUTACT,0(R4)                                                   
         MVC   TLKUTDAT,TRNKDATE                                                
         MVC   TLKUTREF,TRNKREF                                                 
         ZAP   TLDUTRFA,PP$AALLO   TRANSFER AMOUNT IS NET ALLOCATION            
         LA    R1,TSAADD                                                        
         B     PSTB190                                                          
*                                                                               
PSTB180  AP    TLDUTRFA,PP$AALLO   UPDATE EXISTING RECORD                       
         LA    R1,TSAWRT                                                        
PSTB190  GOTO1 ATSARIO,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PTAELD,R4                                                        
PSTB200  SR    R0,R0               FIND PENDING PTAEL                           
         SR    RE,RE               FIND PENDING PTAEL                           
         LA    R4,TRNRFST                                                       
PSTB210  IC    R0,PTALN                                                         
         AR    R4,R0                                                            
         CLI   PTAEL,0             TEST E-O-R                                   
         BE    PSTB230                                                          
         CLI   PTAEL,PTAELQ                                                     
         BNE   PSTB210                                                          
         CLI   PTATYPE,PTATRAL     TEST ALLOCATION ELEMENT                      
         BNE   PSTB210                                                          
         TM    PTASTAT1,PTASPEND   TEST ACTIVITY PENDING                        
         BO    PSTB220                                                          
         TM    PTASTAT1,PTASREVD   TEST THIS PTAEL BEING REVERSED               
         BNO   PSTB210                                                          
         OI    PTASTAT1,PTASREVU   THEN SET AS UPDATED                          
         OI    UPDFLAG,UPDREVBL    SET REVERSAL INDICATOR                       
         B     PSTB210                                                          
*                                  UPDATE PTAEL AND RECORD                      
PSTB220  NI    PTASTAT1,FF-PTASPEND                                             
         MVC   PTARDATE,BCTODAYC   BILLING DATE (TODAY)                         
         MVC   PTARPERS,CUPASS     PERSON CODE                                  
         MVC   PTARBLNO,LSUBILLV   BILL NUMBER (LIVE)                           
         MVC   PTARBLDT,LSUBILDC   BILL DATE                                    
         MVC   PTAMOA,LSUBILMO     MONTH OF BILL                                
         MVC   PTAOFFC,CSOFFICE    SET OFFICE FOR DEBTOR PASSIVE                
         NI    TRNRSTA2,FF-TRNSBILP                                             
         LR    RE,RB               SET PENDING ELEMENT FOUND                    
         B     PSTB210                                                          
*                                                                               
PSTB230  LTR   RE,RE               TEST PENDING ELEMENT FOUND                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   CSACT,ACTDRA        TEST ACTION IS DRAFT                         
         BE    PSTB240                                                          
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('TRXELQ',AIO3),0,0                  
         CLI   BODMCB+12,0                                                      
         BNE   *+12                                                             
         L     RE,BODMCB+12                                                     
         NI    TRXSTA2-TRXELD(RE),FF-TRXSBILP                                   
*                                  UPDATE TRANSACTION RECORD                    
         LA    R2,IOKEY                                                         
         L     R1,AIO3                                                          
         MVC   TRNKEY,0(R1)                                                     
         GOTO1 AIO,IORDUPD+IOACCDIR+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    TRNKSTA2,FF-TRNSBILP                                             
         GOTO1 AIO,IOWRITE+IOACCDIR+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOPUT+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  UPDATE WORK-CODE SUB-TOTALS                  
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,AIO3,AGOPBLK,ACOM,(R0),LSPRATAS,0                
         GOTO1 ASETTRN,BODMCB,(C'S',IOKEY),AIO3,LSPRATA                         
         GOTO1 ASUBTOT,BOPARM,(C'U',LSPRATA),LSPRATAS                           
*&&UK                                                                           
*                                  BUILD PASSIVE                                
         PUSH  USING                                                            
         USING BDPPASD,R2                                                       
         XC    BDPPKEY,BDPPKEY                                                  
         MVI   BDPPTYP,BDPPTYPQ                                                 
         MVC   BDPPCPY,CUABIN                                                   
         MVI   BDPPSYS,BDPPACCQ                                                 
         MVC   BDPPOFF,CSOFFICE                                                 
         MVC   BDPPJOB,BCJOBCOD                                                 
         MVC   BDPPBILM,LSUBILMO                                                
         MVC   BDPPBIL#,LSUBILLV                                                
         MVC   BDPPBILD,LSUBILDC                                                
         MVI   BDPPRTYP,BDPPRWDR                                                
         MVC   BDPPDA,BDPPKDA      DISK ADDRESS GOES IN THE KEY TOO             
         XC    BDPPKSTA,BDPPKSTA   CLEAR STATUS AREA                            
         GOTO1 AIO,IOADD+IOACCDIR+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
         POP   USING                                                            
*&&                                                                             
*                                                                               
PSTB240  L     R4,UPDLSTDA                                                      
         LA    R4,L'TRNKDA+L'TRNKSTA2(R4)                                       
         B     PSTB020                                                          
*                                  GET VATABLE AMOUNT FOR THIS ITEM             
SETVATBL ZAP   BODUB3,PP$AALLO     SET GROSS AS VATABLE                         
         AP    BODUB3,PP$ACOMM                                                  
         CP    UPDSCHPT,BCPZERO    TEST ANY SURCHARGE RATE                      
         BE    SETVAT2                                                          
         CLI   UPDWCSRG,C'N'       TEST WORKCODE NON-SURCHARGABLE               
         BE    SETVAT2                                                          
         CLI   P#VATSCH,C'Y'       TEST SURCHARGE VATABLE                       
         BNE   SETVAT2                                                          
         ZAP   BODUB1(2*L'BODUB1),BODUB3                                        
         MP    BODUB1(2*L'BODUB1),UPDSCHPT                                      
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         AP    BODUB3,BODUB2       ADD SURCHARGE TO VATABLE                     
*                                                                               
SETVAT2  CP    UPDDSCPT,BCPZERO    TEST ANY DISCOUNT                            
         BE    SETVATX                                                          
         ZAP   BODUB1(2*L'BODUB1),PP$AALLO                                      
         AP    BODUB1(2*L'BODUB1),PP$ACOMM                                      
         MP    BODUB1(2*L'BODUB1),UPDDSCPT                                      
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         SP    BODUB3,BODUB2       VATABLE IS ALWAYS DISCOUNTED                 
SETVATX  BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
*              READ BACK TSAR RECORDS AND BUILD POSTING ELEMENTS     *          
**********************************************************************          
         SPACE 1                                                                
PSTB500  CP    UPDBHAPN,UPDCBALN   TEST NET & COMM ARE STILL THE SAME           
         BE    *+6                                                              
         DC    H'0'                                                             
         CP    UPDBHAPC,UPDCBALC                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 APINIT,(RC)         INITIALISE REPORT AND ADDTRN                 
         LA    R4,UPDPOSTB                                                      
         USING POSTVALS,R4         R4=A(POSTING VALUES BLOCK)                   
         MVI   POSTTYPE,POSTBILL   SET TRNTYPE                                  
         MVC   POSTCAC,BCSPACES                                                 
         MVC   POSTCACN,BCSPACES                                                
         MVC   POSTBTMC,LSUBILMC   SET CHARACTER MONTH FOR BATCH REF            
         MVC   POSTBTRF,LSUBILRF   SET BATCH REFERENCE                          
         XC    UPDVRATT(TLDUVTMX*L'UPDVRATT),UPDVRATT                           
         EJECT                                                                  
**********************************************************************          
*              BUILD SK/SI INCOME SUSPENSE TRANSFERS                 *          
**********************************************************************          
         SPACE 1                                                                
         XC    TLKEY,TLKEY         GET SK/SI TRANSFER RECORDS                   
         MVI   TLKSES,TLKUTRFQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PSTB506A LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    PSTB506F                                                         
         CLI   TLKSES,TLKUTRFQ                                                  
         BNE   PSTB506F            NO MORE TRANSFERS                            
         MVC   POSTACT,TLKUTACT                                                 
         MVC   POSTCAC(L'TRNKCPY),CUABIN                                        
         MVC   POSTCAC+L'CUABIN(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)             
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCJOBCOD),BCJOBCOD                  
         MVC   POSTCACN,BCJOBNAM                                                
         MVC   POSTDATE,LSUBILDP   TRANSACTION DATE IS BILL DATE                
         MVC   POSTREF,LSUBILLV    TRANSACTION REF IS LIVE BILL NUM             
         OC    POSTREF,POSTREF     OR DRAFT BILL NUM                            
         BNZ   *+10                                                             
         MVC   POSTREF,LSUBILDF                                                 
         MVI   POSTSTAT,TRNSDR     DEBIT SK                                     
         ZAP   POSTAMNT,TLDUTRFA                                                
         MVC   POSTOFFC,CSOFFICE                                                
         XC    POSTPTRS,POSTPTRS                                                
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         MVC   POSTACT(ACTKACT-ACTKUNT),=C'SI'                                  
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
         MVC   POSTCACN,BCPRONAM                                                
         MVI   POSTSTAT,0          CREDIT SI                                    
         XC    POSTPTRS,POSTPTRS                                                
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
*                                                                               
         LA    R3,IOKEY                                                         
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,BCSPACES     READ INCOME ACCOUNT FOR COST BYTE            
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,POSTACT                                                  
         GOTO1 AGETACT,0                                                        
         BE    *+6                                                              
         DC    H'0'                IT MUST BE THERE                             
         DROP  R3                                                               
         CLI   ACCOST,C' '                                                      
         BNH   PSTB506D                                                         
*                                  DEBIT COSTING CLIENT                         
         MVC   POSTACT,BCCMPPRF+(PPRCOSTU-PPRELD)                               
         MVC   POSTCAC,BCSPACES                                                 
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'ACTKCPY(ACTKACT-ACTKUNT),ULREV                         
         MVC   POSTCAC+(ACTKACT-ACTKEY)(L'ACCOST),ACCOST                        
         BAS   RE,SETCACN                                                       
         MVI   POSTSTAT,TRNSDR                                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
*                                  CREDIT COSTING INCOME                        
         MVC   POSTCAC+1(14),POSTACT                                            
         MVC   POSTACT,BCSPACES                                                 
         MVC   POSTACT(ACTKACT-ACTKUNT),ULREV                                   
         MVC   POSTACT+(ACTKACT-ACTKUNT)(L'ACCOST),ACCOST                       
         BAS   RE,SETCACN                                                       
         MVI   POSTSTAT,0                                                       
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
*                                                                               
PSTB506D B     PSTB506A            GET NEXT TRANSFER RECORD                     
         EJECT                                                                  
**********************************************************************          
*              BUILD 99 BILLING POSTING TO JOB                       *          
**********************************************************************          
         SPACE 1                                                                
PSTB506F MVC   POSTACT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         MVC   POSTACT+L'CPYPROD(L'BCJOBCOD),BCJOBCOD                           
         MVC   POSTACTN,BCJOBNAM                                                
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'ACTKCPY(L'CPYRECV),BCCPYEL+(CPYRECV-CPYELD)            
         MVC   POSTCAC+L'ACTKCPY+L'CPYRECV(L'ACTKACT),UPDDEB                    
         OC    POSTCAC+L'ACTKCPY+L'CPYRECV(L'ACTKACT),BCSPACES                  
         MVC   POSTCACN,UPDDEBN                                                 
         MVC   POSTOFFC,=C'99'                                                  
         MVC   POSTDATE,LSUBILDP   TRANSACTION DATE IS BILL DATE                
         MVC   POSTREF,LSUBILLV    TRANSACTION REF IS LIVE BILL NUM             
         OC    POSTREF,POSTREF     OR DRAFT BILL NUM                            
         BNZ   *+10                                                             
         MVC   POSTREF,LSUBILDF                                                 
         ZAP   POSTAMNT,UPDCBAPN   TOTAL NET ALLOCATION                         
*&&UK*&& LH    RE,=Y(LC@CLIBG-TWAD)                                             
*&&US*&& LH    RE,=Y(UC@CLIBG-TWAD)                                             
         AR    RE,RA                                                            
         XC    POSTNARR,POSTNARR                                                
         MVC   POSTNARR(L'LC@CLIBG),0(RE)                                       
*                                                                               
*&&US                                                                           
         LA    RE,POSTNARR+15      SET BUCKETS FOR COMM/CDISC/PAYABLE           
         LA    R0,13                                                            
         ZAP   0(6,RE),BCPZERO                                                  
         LA    RE,6(RE)                                                         
         BCT   R0,*-10                                                          
         SP    POSTAMNT,UPDCBAPD       NET POSTING IS LESS DISCOUNT             
         ZAP   POSTNARR+15(6),UPDCBAPC COMMISSION BILLED                        
         ZAP   POSTNARR+21(6),UPDCBAPD DISCOUNT BILLED                          
         ZAP   POSTNARR+27(6),UPDCBAPN PAYABLE=(GROSS-DISCOUNT)                 
         AP    POSTNARR+27(6),UPDCBAPC                                          
         SP    POSTNARR+27(6),UPDCBAPD                                          
         MVI   POSTNARL,13*6+15    SET NARRATIVE LENGTH                         
*                                                                               
         LA    RF,BOELEM                                                        
         USING BSCELD,RF                                                        
         MVI   BSCEL,BSCELQ        ADD BILLING SOURCE ELEMENT                   
         MVI   BSCLN,BSCLNQ                                                     
         MVC   BSCBSRC,LSUMEDNM                                                 
         OC    BSCBSRC,BCSPACES                                                 
         MVC   BSCBOFF,CSOFFICE                                                 
         BAS   RE,ADDXTRA                                                       
*&&                                                                             
*                                                                               
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    PSTB508                                                          
         LA    RF,BOELEM                                                        
         USING SCIELD,RF                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCOMM                                                 
         ZAP   SCIAMNT,UPDCBAPC                                                 
         BAS   RE,ADDXTRA                                                       
         XC    TLKEY,TLKEY         GET VAT TYPE TSAR RECORD                     
         MVI   TLKSES,TLKUVATQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PSTB507  LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    PSTB508                                                          
         CLI   TLKSES,TLKUVATQ                                                  
         BNE   PSTB508             END OF VAT RECORDS                           
*                                                                               
         ZAP   BODUB3,BCPZERO      SET BODUB3 WITH DISCOUNT AMOUNT              
         CP    UPDDSCPT,BCPZERO    TEST ANY DISCOUNT                            
         BE    PSTB507A                                                         
         ZAP   BODUB1(2*L'BODUB1),TLDUVATA                                      
         MP    BODUB1(2*L'BODUB1),UPDDSCPT                                      
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         ZAP   BODUB3,BODUB2                                                    
*                                                                               
PSTB507A CP    UPDSCHPT,BCPZERO    TEST ANY SURCHARGE                           
         BE    PSTB507B                                                         
         ZAP   BODUB1(2*L'BODUB1),TLDUVVSG                                      
         MP    BODUB1(2*L'BODUB1),UPDSCHPT                                      
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         ZAP   TLDUVVSG,BODUB2     SET SURCHARGE AMOUNT                         
         AP    UPDTOTAS,BODUB2     ADD TO SURCHARGE TOTAL                       
         CLI   P#VATSCH,C'Y'                                                    
         BNE   *+10                                                             
         AP    TLDUVATA,BODUB2     ADD SURCHARGE TO VATABLE                     
*                                                                               
PSTB507B AP    UPDTOTAD,BODUB3     ADD TO DISCOUNT TOTAL                        
         SP    TLDUVATA,BODUB3     VATABLE AMOUNT IS DISCOUNTED                 
*                                                                               
         PUSH  USING                                                            
         L     R1,AIO1                                                          
         USING VTCD,R1                                                          
         XC    VTCD(VTCLNQ),VTCD                                                
         MVI   VTCACTN,VTCALOOK                                                 
         MVC   VTCCPY,CUABIN                                                    
         MVC   VTCOFFC,CSOFFICE                                                 
         MVC   VTCTYPE,TLKUVATT                                                 
         MVC   VTCCOMF,ACOM                                                     
         MVC   VTCINVD,LSUBILDC                                                 
*                                  MAY BE OVERRIDE VAT DATE                     
         GOTO1 VVATICAN                                                         
         MVC   TLDUVRAT,VTCRATE                                                 
         MVC   TLDUVACT,VTCACT                                                  
         MVC   TLDUVIND,VTCINDS                                                 
         MVC   TLDUVANM,VTCACTNM                                                
         MVC   TLDUVTNM,VTCTTYPE                                                
         OC    TLDUVRAT,TLDUVRAT                                                
         BZ    PSTB507C                                                         
         SR    RE,RE                                                            
         ICM   RE,3,TLDUVRAT                                                    
         ZAP   BODUB1(2*L'BODUB1),BCPZERO                                       
         CVD   RE,BODUB2                                                        
         MP    BODUB1(2*L'BODUB1),TLDUVATA                                      
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         ZAP   TLDUVAT,BODUB1(2*L'BODUB1)                                       
         AP    UPDTOTAV,TLDUVAT    ADD VAT INTO BILL TOTAL                      
PSTB507C LA    R0,TLDUVTMX         SAVE VAT RATE FOR INCOME BUCKETS             
         LA    RF,UPDVRATT                                                      
         CLI   0(RF),0                                                          
         BE    *+14                                                             
         LA    RF,L'UPDVRATT(RF)                                                
         BCT   R0,*-12                                                          
         DC    H'0'                                                             
         MVC   0(L'VTCTYPE,RF),VTCTYPE                                          
         MVC   L'VTCTYPE(L'VTCRATE,RF),VTCRATE                                  
         POP   USING                                                            
PSTB507F LA    RF,BOELEM                                                        
         USING SCIELD,RF                                                        
         MVI   SCIEL,SCIELQ        USERS MUST USE NEW VAT RECORD                
         MVI   SCILN,SCILN2Q                                                    
         MVC   SCITYPE,TLKUVATT                                                 
         NI    SCITYPE,FF-X'40'                                                 
         ZAP   SCIGBIL,TLDUVATA                                                 
         ZAP   SCIVBIL,TLDUVAT                                                  
         BAS   RE,ADDXTRA                                                       
         GOTO1 ATSARIO,TSAPUT      WRITE BACK UPDATED RECORD                    
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PSTB507             GET NEXT VAT RECORD                          
*                                                                               
PSTB508  CP    UPDTOTAS,BCPZERO                                                 
         BE    PSTB508C            NO SURCHARGE                                 
         CLC   UPDSRCAN,BCSPACES   TEST SURCHARGE A/C IS ANALYSED               
         BNH   PSTB508C                                                         
         XC    TLKEY,TLKEY         GET COSTING TSAR RECORD                      
         MVI   TLKSES,TLKUCSTQ                                                  
         MVC   TLKUCOST,UPDSRCAN                                                
         GOTO1 ATSARIO,TSARDH                                                   
         BE    PSTB508B                                                         
         XC    TLKEY,TLKEY         BUILD NEW RECORD                             
         MVC   TLRLEN,=Y(TLUPDLNQ)                                              
         XC    TLDU,TLDU           CLEAR RECORD                                 
         MVI   TLKSES,TLKUCSTQ                                                  
         MVC   TLKUCOST,UPDSRCAN                                                
         ZAP   TLDUCNET,BCPZERO    NO NET                                       
         ZAP   TLDUCCOM,UPDTOTAS   SURCHARGE IS ALL REVENUE                     
         LA    R1,TSAADD           ADD NEW TSAR RECORD                          
         B     *+14                                                             
PSTB508B AP    TLDUCCOM,UPDTOTAS   ADD SURCHARGE TO REVENUE                     
         LA    R1,TSAPUT           WRITE BACK UPDATED RECORD                    
         GOTO1 ATSARIO,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PSTB508C CLC   CSBILCUR,CSCPYCUR                                                
         BE    PSTB509                                                          
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    PSTB509                                                          
         ZAP   BODUB1,UPDTOTAV     GET BILL TOTAL VAT IN CURRENCY               
         EXCHP BODUB1,CSEXCVAL,DUB=BODUB1,WRK=BOWORK1                           
         ZAP   UPDTOTFV,BODUB1                                                  
         LA    RF,BOELEM           FOREIGN CURRENCY ELEMENT                     
         USING AFCELD,RF                                                        
         XC    AFCEL(AFCLNQ),AFCEL                                              
         MVI   AFCEL,AFCELQ                                                     
         MVI   AFCLN,AFCLNQ                                                     
         MVC   AFCCURR,CSBILCUR                                                 
         ZAP   AFCAMNT,UPDTOTFN                                                 
         MVC   AFCX,CSEXCVAL                                                    
         BAS   RE,ADDXTRA                                                       
*                                                                               
PSTB509  MVI   POSTSTAT,0          JOB POSTING IS CREDIT                        
         XC    POSTPTRS,POSTPTRS   NO ANALYSIS POINTERS                         
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         L     RE,POSTXTRA         CLEAR EXTRA ELEMENT BLOCK                    
         MVI   0(RE),0                                                          
         MVC   POSTACT,BCSPACES                                                 
         MVC   POSTCAC,BCSPACES                                                 
         EJECT                                                                  
**********************************************************************          
*              BUILD INCOME POSTINGS                                 *          
**********************************************************************          
         SPACE 1                                                                
         MVC   POSTOFFC,CSOFFICE                                                
         LH    RE,=Y(LC@BILC-TWAD)                                              
         AR    RE,RA                                                            
         XC    POSTNARR,POSTNARR                                                
         MVC   POSTNARR(L'LC@BILC),0(RE)                                        
         MVC   POSTNARR+(L'LC@BILC+1)(L'LSUBILLV),LSUBILLV                      
         XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,TLKUWCQ      READ INCOME TOTAL RECORDS                    
         LA    R1,TSARDH                                                        
         B     PSTB515                                                          
*                                                                               
PSTB510  LA    R1,TSANXT                                                        
PSTB515  GOTO1 ATSARIO,(R1)                                                     
         BL    PSTB530                                                          
         CLI   TLKSES,TLKUWCQ      TEST END OF THIS RECORD TYPE                 
         BNE   PSTB530                                                          
         CLC   TLKUACT,POSTACT                                                  
         BE    *+14                SAME INCOME ACCOUNT                          
         CLC   POSTACT,BCSPACES    TEST THIS IS THE FIRST ONE                   
         BNE   PSTB530                                                          
         MVC   POSTACT,TLKUACT                                                  
*                                                                               
         AP    UPDACCAC,TLDUACOM                                                
         AP    UPDACCFC,TLDUFCOM                                                
         AP    UPDACCAN,TLDUANET                                                
         AP    UPDACCFN,TLDUFNET                                                
*                                                                               
PSTB516  LA    RF,BOELEM           INCOME-BY-WORKCODE CASH ELEMENT              
         USING SCIELD,RF                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITANAL                                                 
         ZAP   SCIAMNT,TLDUANET                                                 
         ZAP   SCIADMN,TLDUACOM                                                 
         MVC   SCISUBTY,BCSPACES                                                
         MVC   SCISUBTY(L'TLKUWC),TLKUWC                                        
         BAS   RE,ADDXTRA                                                       
*                                  ADD VAT TYPE BUCKETS                         
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    PSTB510                                                          
         LA    R1,TLDUVT1                                                       
PSTB518  CLI   0(R1),0                                                          
         BE    PSTB510             END OF VAT TYPE CODES                        
         LA    RF,BOELEM                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVC   SCITYPE,0(R1)                                                    
         NI    SCITYPE,FF-X'40'                                                 
         ZAP   SCIAMNT,L'TLDUVT1(L'TLDUVAM1,R1)                                 
         BAS   RE,ADDXTRA                                                       
         LA    R1,(TLDUVT2-TLDUVT1)(R1)                                         
         B     PSTB518                                                          
         DROP  RF                                                               
*                                                                               
PSTB530  LA    RF,BOELEM           GROSS BUCKET                                 
         USING SCIELD,RF                                                        
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGRSS                                                 
         ZAP   SCIAMNT,UPDACCAN                                                 
         AP    SCIAMNT,UPDACCAC                                                 
         BAS   RE,ADDXTRA                                                       
*                                  CALCULATE VAT IN SCIELS                      
         ZAP   BODUB4,BCPZERO                                                   
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    PSTB538                                                          
         L     RF,POSTXTRA         RF=(ELEMENTS)                                
         SR    R0,R0                                                            
PSTB532  CLI   SCIEL,0                                                          
         BE    PSTB536                                                          
         CLI   SCIEL,SCIELQ                                                     
         BNE   PSTB534                                                          
         TM    SCITYPE,X'40'                                                    
         BO    PSTB534                                                          
         MVC   BCBYTE1,SCITYPE                                                  
         OI    BCBYTE1,X'40'                                                    
         LA    RE,UPDVRATT         RE=(TABLE OF RATES)                          
         CLC   BCBYTE1,0(RE)                                                    
         BE    *+12                                                             
         LA    RE,L'UPDVRATT(RE)                                                
         B     *-14                                                             
         ZAP   BODUB1(2*L'BODUB1),BCPZERO                                       
         SR    R2,R2               CALCULATE VAT AMOUNT                         
         ICM   R2,3,L'VTCTYPE(RE)                                               
         CVD   R2,BODUB2                                                        
         BZ    PSTB533                                                          
         MP    BODUB1(2*L'BODUB1),SCIAMNT                                       
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
PSTB533  ZAP   SCIAMNT,BODUB2                                                   
         AP    BODUB4,SCIAMNT      SAVE TOTAL VAT FOR THIS ACCOUNT              
PSTB534  IC    R0,SCILN                                                         
         AR    RF,R0                                                            
         B     PSTB532                                                          
*                                                                               
PSTB536  LA    RF,BOELEM           TOTAL VAT BUCKET                             
         MVI   SCITYPE,SCITIVAT    TOTAL VAT BUCKET                             
         ZAP   SCIAMNT,BODUB4                                                   
         BAS   RE,ADDXTRA                                                       
         DROP  RF                                                               
*                                                                               
PSTB538  ZAP   POSTAMNT,UPDACCAC   INCOME AMOUNT                                
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'CUABIN(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)             
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
         MVC   POSTCACN,BCPRONAM                                                
         MVI   POSTSTAT,0          CREDIT                                       
         XC    POSTPTRS,POSTPTRS                                                
         GOTO1 APACCR,(RC)                                                      
         BH    EXIT2                                                            
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
*                                                                               
         AP    UPDTOTAC,UPDACCAC   ADD INCOME ACC TOTS INTO BILL TOTS           
         AP    UPDTOTFC,UPDACCFC                                                
         AP    UPDTOTAN,UPDACCAN                                                
*                                                                               
         L     R1,ATSABLK                                                       
         TM    TSERRS-TSARD(R1),TSEEOF       E-O-F                              
         BO    PSTB540                                                          
         CLI   TLKSES,TLKUWCQ      WAS THIS THE LAST INCOME RECORD              
         BNE   PSTB540                                                          
         ZAP   UPDACCAC,TLDUACOM   START NEW INCOME ACCOUNT TOTALS              
         ZAP   UPDACCFC,TLDUFCOM                                                
         ZAP   UPDACCAN,TLDUANET                                                
         ZAP   UPDACCFN,TLDUFNET                                                
         MVC   POSTACT,TLKUACT                                                  
         B     PSTB516                                                          
         EJECT                                                                  
**********************************************************************          
*              BUILD DEBTORS POSTING                                 *          
**********************************************************************          
         SPACE 1                                                                
PSTB540  MVC   POSTACT(L'CPYRECV),BCCPYEL+(CPYRECV-CPYELD)                      
         MVC   POSTACT+L'CPYRECV(L'ACTKACT),UPDDEB                              
         OC    POSTACT+L'CPYRECV(L'ACTKACT),BCSPACES                            
         MVC   POSTCAC,BCSPACES                                                 
         MVC   POSTCAC+(ACTKACT-ACTRECD)(L'LSUMEDNM),LSUMEDNM                   
         MVC   POSTCACN,BCSPACES                                                
         MVC   POSTACTN,UPDDEBN                                                 
         ZAP   POSTAMNT,UPDTOTAN   NET                                          
         AP    POSTAMNT,UPDTOTAC   COMMISSION                                   
         AP    POSTAMNT,UPDTOTAV   VAT                                          
         CLI   CUCTRY,CTRYUSA                                                   
         BNE   *+10                                                             
         SP    POSTAMNT,UPDTOTAD   REDUCE DEBTORS BY DISCOUNT                   
         AP    POSTAMNT,UPDTOTAS   INCREASE DEBTORS BY SURCHARGE                
         MVI   POSTSTAT,TRNSDR                                                  
*                                                                               
         CLC   CSBILCUR,CSCPYCUR                                                
         BE    PSTB542                                                          
         ZAP   BODUB1,UPDTOTAD     GET DISCOUNT IN CURRENCY                     
         BZ    PSTB541                                                          
         EXCHP BODUB1,CSEXCVAL,DUB=BODUB1,WRK=BOWORK1                           
PSTB541  ZAP   UPDTOTFD,BODUB1                                                  
         ZAP   BODUB1,UPDTOTAS     GET SURCHARGE IN CURRENCY                    
         BZ    PSTB541A                                                         
         EXCHP BODUB1,CSEXCVAL,DUB=BODUB1,WRK=BOWORK1                           
PSTB541A ZAP   UPDTOTFS,BODUB1                                                  
         LA    RF,BOELEM           FOREIGN CURRENCY ELEMENT                     
         USING AFCELD,RF                                                        
         XC    AFCEL(AFCLNQ),AFCEL                                              
         MVI   AFCEL,AFCELQ                                                     
         MVI   AFCLN,AFCLNQ                                                     
         OI    AFCXSTAT,AFCXPRIM                                                
         MVC   AFCCURR,CSBILCUR                                                 
         ZAP   AFCAMNT,UPDTOTFN    NET                                          
         AP    AFCAMNT,UPDTOTFC    COMMISSION                                   
         AP    AFCAMNT,UPDTOTFV    VAT                                          
         CLI   CUCTRY,CTRYUSA                                                   
         BNE   *+10                                                             
         SP    AFCAMNT,UPDTOTFD    DISCOUNT                                     
         AP    AFCAMNT,UPDTOTFS    SURCHARGE                                    
         MVC   AFCX,CSEXCVAL                                                    
         BAS   RE,ADDXTRA                                                       
*                                                                               
PSTB542  LA    RF,BOELEM                                                        
         USING SCIELD,RF                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCDSC                                                 
         ZAP   SCIAMNT,UPDTOTAS                                                 
         SP    SCIAMNT,UPDTOTAD                                                 
         BZ    *+8                 NO BUCKET FOR ZERO DISCOUNT                  
         BAS   RE,ADDXTRA                                                       
*                                                                               
         LA    RF,BOELEM                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITTTAX                                                 
         ZAP   SCIAMNT,UPDTOTAV                                                 
         BAS   RE,ADDXTRA                                                       
*                                                                               
         LA    RF,BOELEM           BILLING SOURCE ELEMENT (JOB)                 
         USING SORELD,RF                                                        
         MVI   SOREL,SORELQ                                                     
         MVI   SORLN,SORALNQ                                                    
         MVI   SORSYS,SORSACC                                                   
         MVC   SORAULA(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         MVC   SORAULA+L'CPYPROD(L'BCPROCOD),BCJOBCOD                           
         BAS   RE,ADDXTRA                                                       
         XC    POSTPTRS,POSTPTRS                                                
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         L     RE,POSTXTRA         CLEAR EXTRA ELEMENT BLOCK                    
         MVI   0(RE),0                                                          
         XC    POSTNARR,POSTNARR                                                
         EJECT                                                                  
**********************************************************************          
*              BUILD DISCOUNT AND SURCHARGE POSTINGS                 *          
**********************************************************************          
         SPACE 1                                                                
PSTB560  CP    UPDTOTAD,BCPZERO                                                 
         BE    PSTB570             NO DISCOUNT                                  
         CLI   CUCTRY,CTRYUSA                                                   
         BNE   PSTB570             DISCOUNT POSTING IN USA ONLY                 
         MVC   POSTACT,UPDDSC                                                   
         OC    POSTACT,BCSPACES                                                 
         MVC   POSTACTN,UPDDSCN                                                 
         MVC   POSTCAC(L'CUABIN),CUABIN                                         
         MVC   POSTCAC+L'CUABIN(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)             
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
         MVC   POSTCACN,BCPRONAM                                                
         ZAP   POSTAMNT,UPDTOTAD                                                
         MP    POSTAMNT,=P'-1'                                                  
         MVI   POSTSTAT,0          CREDIT                                       
         CLC   =C'SI',POSTACT                                                   
         BE    *+8                                                              
         MVI   POSTSTAT,TRNSDR     SET DEBIT IF NOT SI                          
         XC    POSTPTRS,POSTPTRS                                                
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
*                                                                               
PSTB570  CP    UPDTOTAS,BCPZERO                                                 
         BE    PSTB600             NO SURCHARGE                                 
         MVC   POSTACT,UPDSRC                                                   
         OC    POSTACT,BCSPACES                                                 
         MVC   POSTACTN,UPDSRCN                                                 
         MVC   POSTCAC(L'CUABIN),CUABIN                                         
         MVC   POSTCAC+L'CUABIN(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)             
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
         MVC   POSTCACN,BCPRONAM                                                
         ZAP   POSTAMNT,UPDTOTAS                                                
         MVI   POSTSTAT,0          CREDIT                                       
         CLC   =C'SI',POSTACT                                                   
         BE    PSTB572                                                          
         CLC   =C'SQ',POSTACT                                                   
         BE    PSTB572                                                          
         MVI   POSTSTAT,TRNSDR     SET DEBIT IF NOT SI                          
         MP    POSTAMNT,=P'-1'                                                  
PSTB572  XC    POSTPTRS,POSTPTRS                                                
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
         EJECT                                                                  
**********************************************************************          
*              BUILD VAT/GST POSTINGS                                *          
**********************************************************************          
         SPACE 1                                                                
PSTB600  ZAP   LSUVATT,BCPZERO                                                  
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    PSTB620                                                          
         XC    TLKEY,TLKEY         GET VAT TYPE TSAR RECORD                     
         MVI   TLKSES,TLKUVATQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PSTB607  LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    PSTB620                                                          
         CLI   TLKSES,TLKUVATQ                                                  
         BNE   PSTB620             END OF VAT RECORDS                           
         MVC   POSTACT,TLDUVACT+(ACTKUNT-ACTKEY)                                
         MVC   POSTACTN,TLDUVANM                                                
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'CUABIN(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)             
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
         MVC   POSTCACN,BCPRONAM                                                
         ZAP   POSTAMNT,TLDUVAT    VAT AMOUNT                                   
         AP    LSUVATT,TLDUVAT     VAT TOTAL FOR BILL                           
         MVI   POSTSTAT,0          CREDIT                                       
         LA    RF,BOELEM                                                        
         USING SCIELD,RF                                                        
         MVI   SCIEL,SCIELQ        USERS MUST USE NEW VAT RECORD                
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGLEV                                                 
         ZAP   SCIAMNT,TLDUVATA                                                 
         BAS   RE,ADDXTRA                                                       
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BNH   PSTB610                                                          
         CLC   FVXTRA,BCSPACES                                                  
         BH    EXIT2                                                            
         MVC   FVMSGNO,=AL2(AE$VRNDE)                                           
         B     EXIT2                                                            
PSTB610  L     RE,POSTXTRA         CLEAR EXTRA ELEMENT BLOCK                    
         MVI   0(RE),0                                                          
         B     PSTB607             GET NEXT VAT RECORD                          
         EJECT                                                                  
**********************************************************************          
*              BUILD COSTING POSTINGS                                *          
**********************************************************************          
         SPACE 1                                                                
PSTB620  MVC   POSTCAC,BCCMPPRF+(PPRCOST-PPRELD)                                
         BAS   RE,SETCACN                                                       
         MVC   UPDCSTNM,POSTCACN   SAVE CLIENT COSTING NAME                     
         MVC   UPDBLMCS,LSUBILMC   SAVE TRANSACTION MOA                         
         XC    TLKEY,TLKEY         GET COSTING TSAR RECORD                      
         MVI   TLKSES,TLKUCSTQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PSTB630  LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    PSTB700                                                          
         CLI   TLKSES,TLKUCSTQ     TEST END OF COSTING RECORDS                  
         BNE   PSTB700                                                          
         MVC   POSTCAC,BCSPACES    GET COSTING BILLINGS ACCOUNT NAME            
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'ACTKCPY(ACTKACT-ACTKUNT),ULBIL                         
         MVC   POSTCAC+(ACTKACT-ACTKEY)(L'TLKUCOST),TLKUCOST                    
         BAS   RE,SETCACN                                                       
         MVC   UPDULBNM,POSTCACN                                                
         MVC   POSTCAC,BCSPACES    GET COSTING REVENUES ACCOUNT NAME            
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'ACTKCPY(ACTKACT-ACTKUNT),ULREV                         
         MVC   POSTCAC+(ACTKACT-ACTKEY)(L'TLKUCOST),TLKUCOST                    
         BAS   RE,SETCACN                                                       
         MVC   UPDULRNM,POSTCACN                                                
         SR    R3,R3                                                            
         OC    UPDMNTHS(L'UPDMNTH),UPDMNTHS                                     
         BZ    PSTB642                                                          
         LA    R3,UPDMNTHS                                                      
*                                  DEBIT COSTING CLIENT                         
PSTB640  CLI   0(R3),0                                                          
         BE    PSTB650                                                          
PSTB642  MVC   POSTACT,BCCMPPRF+(PPRCOSTU-PPRELD)                               
         MVC   POSTACTN,UPDCSTNM                                                
         MVC   POSTCAC,BCSPACES                                                 
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'ACTKCPY(ACTKACT-ACTKUNT),ULBIL                         
         MVC   POSTCAC+(ACTKACT-ACTKEY)(L'TLKUCOST),TLKUCOST                    
         MVC   POSTCACN,UPDULBNM                                                
         ZAP   POSTAMNT,TLDUCCOM   COMMISSION                                   
         LTR   R3,R3                                                            
         BZ    PSTB645                                                          
         ZAP   UPDPSTAS,POSTAMNT                                                
         GOTO1 APTRNA,(RC)                                                      
         CLC   UPDBLMCS,POSTBTMC                                                
         BNE   *+10                                                             
PSTB645  AP    POSTAMNT,TLDUCNET   NET ALL GOES IN MONTH 1                      
         MVI   POSTSTAT,TRNSDR                                                  
*                                  CREDIT COSTING BILLINGS                      
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         MVC   POSTCAC+1(14),POSTACT                                            
         MVC   POSTCACN,UPDCSTNM                                                
         MVC   POSTACT,BCSPACES                                                 
         MVC   POSTACT(ACTKACT-ACTKUNT),ULBIL                                   
         MVC   POSTACT+(ACTKACT-ACTKUNT)(L'TLKUCOST),TLKUCOST                   
         MVC   POSTACTN,UPDULBNM                                                
         BAS   RE,SETCACN                                                       
         MVI   POSTSTAT,0                                                       
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
*                                  DEBIT COSTING CLIENT                         
         MVC   POSTACT,BCCMPPRF+(PPRCOSTU-PPRELD)                               
         MVC   POSTACTN,UPDCSTNM                                                
         MVC   POSTCAC,BCSPACES                                                 
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'ACTKCPY(ACTKACT-ACTKUNT),ULREV                         
         MVC   POSTCAC+(ACTKACT-ACTKEY)(L'TLKUCOST),TLKUCOST                    
         MVC   POSTCACN,UPDULRNM                                                
         ZAP   POSTAMNT,TLDUCCOM   COMMISSION                                   
         LTR   R3,R3                                                            
         BZ    PSTB648                                                          
         ZAP   UPDPSTAS,POSTAMNT                                                
         GOTO1 APTRNA,(RC)                                                      
PSTB648  MVI   POSTSTAT,TRNSDR                                                  
*                                  CREDIT COSTING INCOME                        
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         MVC   POSTCAC+1(14),POSTACT                                            
         MVC   POSTCACN,UPDCSTNM                                                
         MVC   POSTACT,BCSPACES                                                 
         MVC   POSTACT(ACTKACT-ACTKUNT),ULREV                                   
         MVC   POSTACT+(ACTKACT-ACTKUNT)(L'TLKUCOST),TLKUCOST                   
         MVC   POSTACTN,UPDULRNM                                                
         MVI   POSTSTAT,0                                                       
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         LTR   R3,R3                                                            
         BZ    PSTB650                                                          
         LA    R3,L'UPDMNTHS(R3)                                                
         B     PSTB640                                                          
*                                                                               
PSTB650  B     PSTB630             GET NEXT COSTING RECORD                      
*                                  FINAL CALL CLOSES REPORT ETC                 
PSTB700  OC    LSUWOFRF,LSUWOFRF   TEST WRITE-OFFS UPDATE REQUIRED              
         BNZ   PSTB900                                                          
         OC    UPDRECRF,UPDRECRF   TEST RECOVERIES UPDATE REQUIRED              
         BNZ   PSTB900                                                          
         OC    LSUTRFRF,LSUTRFRF   TEST TRANSFERS UPDATE REQUIRED               
         BNZ   PSTB900                                                          
         OC    UPDFEERF,UPDFEERF   TEST FEEADJ/INTERNAL INVOICE                 
         BNZ   PSTB900                                                          
*                                  CLOSE REPORT NOW                             
         GOTO1 ABLDTRN,BODMCB,('FF',POSTVALS)                                   
         BH    EXIT2                                                            
         BAS   RE,PRTSUMM                                                       
         EJECT                                                                  
**********************************************************************          
*              UPDATE JOB RECORD ALLOC/WRITE-OFF/XFER TOTALS         *          
**********************************************************************          
         SPACE 1                                                                
PSTB900  CLI   CSACT,ACTDRA        TEST ACTION DRAFT                            
         BE    PSTB950                                                          
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   ACTKACT,BCJOBCOD                                                 
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   UPDBLNG,YESQ        TEST IF BILLING BEING UPDATED                
         BNE   PSTB902                                                          
         GOTO1 AUPDJOB,BODMCB,(C'C',SCITCBAP),AIO1                              
*                                                                               
PSTB902  OC    LSUWOFRF,LSUWOFRF   TEST IF WRITE-OFF BEING UPDATED              
         BZ    PSTB904                                                          
         GOTO1 AUPDJOB,BODMCB,(C'C',SCITCBWP),AIO1                              
*                                                                               
PSTB904  OC    LSUTRFRF,LSUTRFRF   TEST IF TRANSFER BEING UPDATED               
         BZ    PSTB906                                                          
         GOTO1 AUPDJOB,BODMCB,(C'C',SCITCBTP),AIO1                              
*                                                                               
PSTB906  OC    UPDRECRF,UPDRECRF   TEST IF RECOVERY BEING UPDATED               
         BZ    PSTB908                                                          
         GOTO1 AUPDJOB,BODMCB,(C'C',SCITCBRP),AIO1                              
*                                                                               
PSTB908  OC    UPDFEERF,UPDFEERF   TEST IF FEE BEING UPDATED                    
         BZ    PSTB910                                                          
         GOTO1 AUPDJOB,BODMCB,(C'C',SCITCBIP),AIO1                              
*                                                                               
PSTB910  EQU   *                                                                
*                                                                               
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
*                                                                               
         TM    UPDFLAG,UPDREVBL    DID THIS BILL INCLUDE REVERSALS              
         BNO   PSTB950                                                          
         LA    R2,IOKEY            READ AND UPDATE 99 PRIOR BILLS               
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,BCJOBCOD                                                 
         MVC   TRNKWORK,=C'99'                                                  
         LA    R1,IOHI+IOACCDIR+IO1                                             
         B     *+8                                                              
PSTB942  LA    R1,IOSQ+IOACCDIR+IO1                                             
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TRNKEY(TRNKCULC-TRNKEY),IOKEYSAV                                 
         BNE   PSTB950                                                          
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RATELD,RF                                                        
         SR    R0,R0                                                            
         L     RF,AIO1                                                          
         LA    RF,TRNRFST-TRNRECD(RF)                                           
PSTB945  IC    R0,RATLN                                                         
         AR    RF,R0                                                            
         CLI   RATEL,0                                                          
         BE    PSTB942                                                          
         CLI   RATEL,RATETAXQ                                                   
         BNE   PSTB945                                                          
         OC    RATRATE,RATRATE     TEST THIS BILL HAD ANY REVERSALS             
         BZ    PSTB942             IF NOT DON'T BOTHER TO WRITE BACK            
         NC    RATRATE,=X'8000'    CLEAR COUNT OF REVERSED ITEMS                
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    PSTB942                                                          
         DC    H'0'                                                             
*                                                                               
         DROP  RF                                                               
         EJECT                                                                  
**********************************************************************          
*              UPDATE BILL HEADER RECORD                             *          
**********************************************************************          
         SPACE 1                                                                
PSTB950  CLI   UPDBLNG,YESQ        TEST IF BILLING BEING UPDATED                
         BNE   PSTWOF                                                           
         LA    R2,IOKEY                                                         
         USING PBRRECD,R2                                                       
         XC    PBRPAS,PBRPAS                                                    
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ                                                 
         MVC   PBRPBLNO,CSBILNUM                                                
         MVI   PBRPIND,PBRPIDFT                                                 
         GOTO1 AIO,IO1+IOHIUP+IOACCDIR                                          
         CLC   PBRPAS(PBRPUSER-PBRPAS),IOKEYSAV                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    PBRKSTAT,PBRSDELT   DELETE DRAFT PASSIVE                         
         CLI   CSACT,ACTDRA                                                     
         BE    PSTB954                                                          
         GOTO1 AIO,IO1+IOWRITE+IOACCDIR                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   PBRPIND,PBRPILVE    ADD LIVE PASSIVE                             
         MVC   PBRPBILD,BCTODAYC                                                
         MVC   PBRPBLNO,LSUBILLV                                                
         MVC   PBRKBILD,PBRPBILD                                                
         NI    PBRKSTAT,FF-(PBRSDELT)                                           
         GOTO1 AIO,IO1+IOADD+IOACCDIR                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  GET MASTER RECORD                            
PSTB954  GOTO1 AIO,IO1+IOGETRUP+IOACCMST                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
*                                                                               
         LA    R3,PBRRFST                                                       
         USING BLHELD,R3                                                        
         XR    RF,RF                                                            
         CLI   BLHEL,BLHELQ                                                     
         BE    *+12                                                             
         IC    RF,BLHLN                                                         
         BXH   R3,RF,*-12                                                       
         ZAP   BLHDSC,UPDDSCPT                                                  
         ZAP   BLHSCH,UPDSCHPT                                                  
         CLI   CSACT,ACTDRA        TEST IF ACTION DRAFT                         
         BE    PSTB956                                                          
         MVC   BLHBILD,BCTODAYC    BILLED DATE                                  
         MVC   BLHTRND,LSUBILDC    BILL DATE                                    
         MVC   BLHBLNO,LSUBILLV    LIVE BILL NUMBER                             
         MVC   BLHDUED,LSUDUEDT    DUE DATE                                     
         MVC   BLHBREF,LSUBILRF    BATCH REFERENCE                              
         MVC   BLHBMOS,LSUBILMO    BILL MOS                                     
         MVC   PBRRBILD,BCTODAYC   SET BILLED DATE IN STATUS AREA               
*                                                                               
PSTB956  GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('SCIELQ',AIO1),            *        
               (1,=AL1(SCITCBSG)),0                                             
         CLI   12(R1),0                                                         
         BNE   PSTB957                                                          
         L     RE,12(R1)                                                        
         ZAP   SCIAMNT-SCIELD(L'SCIAMNT,RE),UPDTOTAS                            
         CLC   CSBILCUR,CSCPYCUR   TEST FOREIGN CURRENCY                        
         BE    *+10                                                             
         ZAP   SCIAMNT-SCIELD(L'SCIAMNT,RE),UPDTOTFS                            
         B     PSTB958                                                          
*                                                                               
         PUSH  USING                                                            
         USING SCIELD,BOELEM                                                    
PSTB957  MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCBSG    ACTUAL SURCHARGE AMOUNT                      
         ZAP   SCIAMNT,UPDTOTAS                                                 
         CLC   CSBILCUR,CSCPYCUR   TEST FOREIGN CURRENCY                        
         BE    *+10                                                             
         ZAP   SCIAMNT,UPDTOTFS                                                 
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO1,BOELEM                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PSTB958  GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('SCIELQ',AIO1),            *        
               (1,=AL1(SCITCBDC)),0                                             
         CLI   12(R1),0                                                         
         BNE   PSTB959                                                          
         L     RE,12(R1)                                                        
         ZAP   SCIAMNT-SCIELD(L'SCIAMNT,RE),UPDTOTAD                            
         CLC   CSBILCUR,CSCPYCUR   TEST FOREIGN CURRENCY                        
         BE    *+10                                                             
         ZAP   SCIAMNT-SCIELD(L'SCIAMNT,RE),UPDTOTFD                            
         B     PSTB960                                                          
*                                                                               
PSTB959  MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCBDC    ACTUAL DISCOUNT AMOUNT                       
         ZAP   SCIAMNT,UPDTOTAD                                                 
         CLC   CSBILCUR,CSCPYCUR   TEST FOREIGN CURRENCY                        
         BE    *+10                                                             
         ZAP   SCIAMNT,UPDTOTFD                                                 
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO1,BOELEM                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*                                                                               
PSTB960  GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   CSACT,ACTDRA                                                     
         BE    PSTWOF                                                           
         MVC   IOKEY,0(R2)         GET PRIME DIRECTORY KEY                      
         GOTO1 AIO,IO1+IORDUP+IOACCDIR                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         MVC   PBRKBILD,BCTODAYC   SET BILL DATE IN PRIME KEY                   
         GOTO1 AIO,IO1+IOWRITE+IOACCDIR                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CSBILNUM,LSUBILLV   PASS LIVE NUMBER TO PRINT                    
         B     PSTWOF                                                           
         EJECT                                                                  
**********************************************************************          
*              UPDATE WRITE-OFFS                                     *          
**********************************************************************          
         SPACE 1                                                                
PSTWOF   OC    LSUWOFRF,LSUWOFRF   TEST WRITE-OFFS UPDATE REQUIRED              
         BNZ   *+14                                                             
         OC    UPDRECRF,UPDRECRF   TEST RECOVERIES UPDATE REQUIRED              
         BZ    PSTXFR                                                           
         LA    R4,UPDDATAB                                                      
         CLI   UPDINIT,YESQ                                                     
         BE    PSTW100                                                          
         GOTO1 APINIT,(RC)         INITIALISE REPORT AND ADDTRN                 
PSTW100  ST    R4,UPDLSTDA         SAVE FOR NEXT                                
         CLI   0(R4),FF                                                         
         BE    PSTW500             ALL DONE                                     
         TM    L'TRNKDA(R4),TRNSWOFP                                            
         BZ    PSTW900             NO WRITE-OFF PENDING                         
         MVC   IODAOVER,0(R4)                                                   
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
*                                                                               
*        GOTO1 AGETOPT,BODMCB,AIO1                                              
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,AIO1,AGOPBLK,ACOM,(R0),LSPRATA,0                 
*                                                                               
         LA    R3,TRNRFST                                                       
         USING PTAELD,R3                                                        
PSTW210  SR    R0,R0                                                            
         IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         CLI   PTAEL,0                                                          
         BE    PSTW900                                                          
         CLI   PTAEL,PTAELQ                                                     
         BNE   PSTW210                                                          
PSTW220  TM    PTASTAT1,PTASPEND   TEST ACTIVITY PENDING                        
         BNO   PSTW210                                                          
         CLI   PTATYPE,PTATWOF     TEST WRITE-OFF ELEMENT                       
         BNE   PSTW225                                                          
         OC    LSUWOFRF,LSUWOFRF   CHECK WRITE-OFFS BEING UPDATED               
         BZ    PSTW210                                                          
         B     PSTW230                                                          
PSTW225  CLI   PTATYPE,PTATWOFR    TEST RECOVERY ELEMENT                        
         BNE   PSTW210                                                          
         OC    UPDRECRF,UPDRECRF   CHECK RECOVERIES BEING UPDATED               
         BZ    PSTW210                                                          
*                                  UPDATE PTAEL AND RECORD                      
PSTW230  NI    PTASTAT1,FF-PTASPEND                                             
         LA    RF,LSUWOFMO         SET W-OFF MONTH                              
         CLI   PTATYPE,PTATWOF                                                  
         BE    *+8                                                              
         LA    RF,UPDRECMO         SET RECVR MONTH                              
         MVC   PTAMOA,0(RF)                                                     
*&&US*&& MVC   PTADATE,BCTODAYC    SET TODAY FOR NARR AND DISPLAY               
         NI    TRNRSTA2,FF-TRNSWOFP                                             
         CLI   CSACT,ACTDRA        TEST ACTION IS DRAFT                         
         BE    PSTW240                                                          
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('TRSELQ',AIO1),0,0                  
         CLI   BODMCB+12,0                                                      
         BNE   PSTW232                                                          
         L     RE,BODMCB+12                                                     
         OI    TRSSTAT3-TRSELD(RE),TRSSNBIL                                     
         CLI   PTATYPE,PTATWOFR    TEST THIS IS A RECOVERY                      
         BNE   PSTW232                                                          
         NI    TRSSTAT3-TRSELD(RE),FF-TRSSNBIL                                  
PSTW232  GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('TRXELQ',AIO1),0,0                  
         CLI   BODMCB+12,0                                                      
         BNE   *+12                                                             
         L     RE,BODMCB+12                                                     
         NI    TRXSTA2-TRXELD(RE),FF-TRXSWOFP                                   
*                                  UPDATE TRANSACTION RECORD                    
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         L     R1,AIO1                                                          
         MVC   TRNKEY,0(R1)                                                     
         GOTO1 AIO,IORDUPD+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    TRNKSTA2,FF-TRNSWOFP                                             
         GOTO1 AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  UPDATE WORK-CODE SUB-TOTALS                  
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,AIO1,AGOPBLK,ACOM,(R0),LSPRATAS,0                
         GOTO1 ASETTRN,BODMCB,(C'S',IOKEY),AIO1,LSPRATA                         
         GOTO1 ASUBTOT,BOPARM,(C'U',LSPRATA),LSPRATAS                           
*                                  PTAREC FOR EXTRA POSTING ACCOUNTS            
PSTW240  LA    R2,IOKEY                                                         
         USING PTARECD,R2                                                       
         XC    PTAKEY,PTAKEY                                                    
         MVI   PTAKTYP,PTAKTYPQ                                                 
         MVC   PTAKCPY,CUABIN                                                   
         MVC   PTAKJOB,BCJOBCOD                                                 
         MVC   PTAKSEQN,PTASEQN                                                 
         L     R1,=A(IO8)                                                       
         GOTO1 AIO,IORDUPD+IOACCDIR(R1)                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=A(IO8)                                                       
         GOTO1 AIO,IOGETRUP+IOACCMST(R1)                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
**********************************************************************          
*              EXPENSE AND JOB POSTINGS                              *          
**********************************************************************          
         SPACE 1                                                                
         L     R2,AIO8                                                          
         LA    R2,TRNRFST-TRNRECD(R2)                                           
         USING TRNELD,R2           R2=A(PTAREC TRNEL)                           
         LA    R4,UPDPOSTB                                                      
         USING POSTVALS,R4                                                      
         MVC   POSTACT,PTAWEXPA    USE EXPENSE ACCOUNT                          
         CLI   PTALN,PTAWLN2Q      TEST IF WRITE-OFF ACCOUNT SPECIFIED          
         BNE   *+10                                                             
         MVC   POSTACT,PTAWWOFA                                                 
         MVC   POSTACTN,BCSPACES                                                
         MVC   POSTCAC(L'TRNKCPY),CUABIN                                        
         MVC   POSTCAC+L'CUABIN(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)             
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCJOBCOD),BCJOBCOD                  
         MVC   POSTCACN,BCJOBNAM                                                
         MVC   POSTOFFC,TRNOFFC                                                 
         MVC   POSTDATE,TRNDATE                                                 
         MVC   POSTREF,TRNREF                                                   
         ZAP   POSTAMNT,PTANET                                                  
         MVI   POSTSTAT,TRNSDR                                                  
         CLC   =C'SI',POSTACT      TEST GOING TO INCOME                         
         BNE   PSTW260                                                          
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
         MVC   POSTCACN,BCPRONAM                                                
         MVI   POSTSTAT,0          THEN POST CREDIT                             
         MP    POSTAMNT,=P'-1'     AND REVERSE SIGN                             
PSTW260  MVC   POSTTYPE,TRNTYPE                                                 
         OI    POSTSTAT,TRNSNOCM                                                
         LA    RF,LSUWOFRF         SET W-OFF REFERENCE/MONTH                    
         CLI   PTATYPE,PTATWOF                                                  
         BE    *+8                                                              
         LA    RF,UPDRECRF         SET RECVR REFERENCE/MONTH                    
         MVC   POSTBTRF,0(RF)                                                   
         MVC   POSTBTMC,LSUWOFMC-LSUWOFRF(RF)                                   
*                                                                               
         GOTO1 BLDWONAR,BOPARM,TRNELD,PTAELD   BUILD NARRATIVE                  
*                                                                               
         XC    POSTPTRS,POSTPTRS                                                
         MVI   WOFILT,0                                                         
         BAS   RE,BLDPTRS          BUILD ANALYSIS POINTERS                      
         LH    RF,=Y(UPDPTRS-UPDWORKD)                                          
         AR    RF,RC                                                            
         CLI   0(RF),FF                                                         
         BE    *+8                                                              
         STCM  RF,15,POSTPTRS                                                   
         L     RE,POSTXTRA         CLEAR EXTRA ELEMENT BLOCK                    
         MVI   0(RE),0                                                          
*&&US                                                                           
         CLC   =C'SI',POSTACT      TEST GOING TO INCOME                         
         BNE   *+12                                                             
         LA    R1,BCJOBCOD                                                      
         BAS   RE,ADDMDTEL         ADD MEDIA TRANSFER ELEMENT                   
         L     R1,AIO1                                                          
         LA    R1,TRNKWORK-TRNRECD(R1)                                          
         BAS   RE,ADDRFLEL         ADD WORKCODE FILTER ELEMENT                  
*&&                                                                             
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCJOBCOD),BCJOBCOD                  
*                                  GET WC/DATE/REF FROM ORIGINAL                
         L     RF,AIO1                                                          
         MVC   POSTOFFC,(TRNRFST-TRNRECD)+(TRNOFFC-TRNEL)(RF)                   
         MVC   POSTDATE,(TRNKDATE-TRNRECD)(RF)                                  
         MVC   POSTREF,(TRNKREF-TRNRECD)(RF)                                    
         MVC   POSTACT,POSTCAC+L'TRNKCPY                                        
         L     RF,AIO1                                                          
         MVC   POSTCAC,TRNKCULC-TRNRECD(RF)                                     
         LR    R0,R2                                                            
         L     R2,AIO1             SET R2=A(ORIGINAL TRNEL)                     
         LA    R2,TRNRFST-TRNRECD(R2)                                           
         BAS   RE,XTRAELS          ADD EXTRA ELEMENTS                           
         LR    R2,R0               RESTORE R2=A(PTAREC TRNEL)                   
*                                                                               
         PUSH USING                                                             
         USING TRSELD,BOELEM       ADD TRSELD                                   
         XC    TRSELD(TRSLNQ),TRSELD                                            
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         OI    TRSSTAT3,TRSSNBIL   NOT BILLABLE ON A21                          
         BAS   RE,ADDXTRA                                                       
*                                                                               
         USING SPAELD,BOELEM       ADD WRITE-OFF SPAEL                          
         XC    SPAELD(SPALNQ),SPAELD                                            
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATWOFF                                                 
         MVC   SPAAULA,PTAWEXPA                                                 
         BAS   RE,ADDXTRA                                                       
         POP   USING                                                            
*&&US                                                                           
         IC    RE,PTALN            COPY PTA ELEMENT                             
         EX    RE,*+4                                                           
         MVC   BOELEM(0),PTAELD                                                 
NEWEL    USING PTAELD,BOELEM                                                    
         MP    NEWEL.PTANET,=P'-1'                                              
         MP    NEWEL.PTANETF,=P'-1'                                             
         MP    NEWEL.PTACDSC,=P'-1'                                             
         XC    NEWEL.PTAHOURS,BCEFFS                                            
         ICM   RF,3,NEWEL.PTAHOURS                                              
         LA    RF,1(RF)                                                         
         STCM  RF,3,NEWEL.PTAHOURS                                              
         BAS   RE,ADDXTRA                                                       
*&&                                                                             
*                                                                               
         ZAP   POSTAMNT,PTANET                                                  
         MVI   POSTSTAT,TRNSDR                                                  
         L     RF,AIO1                                                          
         TM    (TRNRFST-TRNRECD)+(TRNSTAT-TRNELD)(RF),TRNSNOCM                  
         BNO   *+8                                                              
         OI    POSTSTAT,TRNSNOCM   -DR TO JOB GETS SAME STATUS                  
         MP    POSTAMNT,=P'-1'                                                  
         XC    POSTPTRS,POSTPTRS   NO ANALYSIS POINTERS                         
         BAS   RE,SETCACN                                                       
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         L     RE,POSTXTRA         CLEAR EXTRA ELEMENT BLOCK                    
         MVI   0(RE),0                                                          
         MVC   POSTOFFC,TRNOFFC                                                 
         MVC   POSTDATE,TRNDATE    RESET REF/DATE FROM W/O INPUT                
         MVC   POSTREF,TRNREF                                                   
*&&US                                                                           
*        CLI   TRNTYPE,57          TEST TIME                                    
*        BNE   PSTW280                                                          
         L     RE,AIO1                                                          
         CLC   =C'SK',TRNKULC-TRNKEY(RE)                                        
         BNE   *+14                                                             
         MVC   POSTACT,TRNKULC-TRNKEY(RE)                                       
         B     PSTW270                                                          
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('SPDELQ',AIO1),(2,=C'SK')           
         CLI   12(R1),0                                                         
         BNE   PSTW280                                                          
         L     RF,12(R1)                                                        
         USING SPDELD,RF                                                        
         MVC   POSTACT,BCSPACES                                                 
         IC    RE,SPDLN                                                         
         SH    RE,=Y(SPDLN1Q+1)                                                 
         EX    RE,*+4                                                           
         MVC   POSTACT(0),SPDACCS                                               
         DROP  RF                                                               
PSTW270  MVC   POSTCAC(L'CUABIN),CUABIN                                         
         MVC   POSTCAC+L'CUABIN(L'BCCPYPRD),BCCPYPRD                            
         MVC   POSTCAC+L'CUABIN+L'BCCPYPRD(L'BCJOBCOD),BCJOBCOD                 
         MVC   POSTCACN,BCJOBNAM                                                
         ZAP   POSTAMNT,PTANET                                                  
         MVI   POSTSTAT,TRNSDR                                                  
         XC    POSTPTRS,POSTPTRS                                                
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         MVC   POSTACT(2),=C'SI'                                                
         MVC   POSTCAC+L'CUABIN+L'BCCPYPRD(L'BCPROCOD),BCPROCOD                 
         MVC   POSTCACN,BCPRONAM                                                
         MVI   POSTSTAT,0                                                       
         MVI   WOFILT,WOIAPESK                                                  
         BAS   RE,BLDPTRS          BUILD ANALYSIS POINTERS                      
         LH    RF,=Y(UPDPTRS-UPDWORKD)                                          
         AR    RF,RC                                                            
         CLI   0(RF),FF                                                         
         BE    *+8                                                              
         STCM  RF,15,POSTPTRS                                                   
         LA    R1,BCJOBCOD                                                      
         BAS   RE,ADDMDTEL         ADD MEDIA TRANSFER ELEMENT                   
         L     R1,AIO1                                                          
         LA    R1,TRNKWORK-TRNRECD(R1)                                          
         BAS   RE,ADDRFLEL         ADD WORKCODE FILTER ELEMENT                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
*&&                                                                             
         EJECT                                                                  
**********************************************************************          
*              ANALYSIS WRITE-OFF POSTINGS                           *          
**********************************************************************          
         SPACE 1                                                                
PSTW280  CLI   PTALN,PTAWLN2Q                                                   
         BNE   PSTW300                                                          
         MVI   POSTSTAT,TRNSDR                                                  
         MVC   POSTACT,PTAWEXPA                                                 
         MVC   POSTCAC(L'TRNKCPY),CUABIN                                        
         MVC   POSTCAC+L'CUABIN(L'TRNKULA),PTAWWOFA                             
         ZAP   POSTAMNT,PTANET                                                  
         CLC   =C'SI',POSTACT                                                   
         BNE   PSTW286                                                          
         MVI   POSTSTAT,0                                                       
         MP    POSTAMNT,=P'-1'                                                  
         LA    R1,BCJOBCOD                                                      
         BAS   RE,ADDMDTEL         ADD MEDIA TRANSFER ELEMENT                   
         L     R1,AIO1                                                          
         LA    R1,TRNKWORK-TRNRECD(R1)                                          
         BAS   RE,ADDRFLEL         ADD WORKCODE FILTER ELEMENT                  
PSTW286  XC    POSTPTRS,POSTPTRS   NO ANALYSIS POINTERS                         
         BAS   RE,SETCACN                                                       
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         L     RE,POSTXTRA         CLEAR EXTRA ELEMENT BLOCK                    
         MVI   0(RE),0                                                          
*                                                                               
         XC    POSTACT,POSTCAC+1                                                
         XC    POSTCAC+1(L'POSTACT),POSTACT                                     
         XC    POSTACT,POSTCAC+1                                                
         MVI   POSTSTAT,0                                                       
         ZAP   POSTAMNT,PTANET                                                  
         XC    POSTPTRS,POSTPTRS   NO ANALYSIS POINTERS                         
         CLC   =C'SI',POSTACT                                                   
         BNE   PSTW288                                                          
         LA    R1,BCJOBCOD                                                      
         BAS   RE,ADDMDTEL         ADD MEDIA TRANSFER ELEMENT                   
         L     R1,AIO1                                                          
         LA    R1,TRNKWORK-TRNRECD(R1)                                          
         BAS   RE,ADDRFLEL         ADD WORKCODE FILTER ELEMENT                  
PSTW288  BAS   RE,SETCACN                                                       
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         L     RE,POSTXTRA         CLEAR EXTRA ELEMENT BLOCK                    
         MVI   0(RE),0                                                          
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
*              MAKE DEPARTMENT/COSTING/PERSON POSTINGS               *          
**********************************************************************          
         SPACE 1                                                                
PSTW300  LA    R2,WOTAB                                                         
         USING WOTABD,R2                                                        
PSTW310  CLI   WOTABD,EOT          TEST END-OF-TABLE                            
         BE    PSTW380                                                          
         GOTO1 TESTWO,BOPARM,WOTABD  TEST POSTING VALID                         
         BNE   PSTW370                                                          
         LM    RE,RF,0(R1)                                                      
         MVC   POSTACT,0(RE)                                                    
         MVC   POSTCAC(L'CUABIN),CUABIN                                         
         MVC   POSTCAC+(L'CUABIN)(L'POSTCAC-L'CUABIN),0(RF)                     
         MVI   POSTSTAT,0                                                       
         CLI   WODRCR,WODRQ        TEST DEBIT/CREDIT                            
         BNE   *+8                                                              
         OI    POSTSTAT,TRNSDR                                                  
         ZAP   POSTAMNT,PTANET                                                  
         CLI   WOSIGN,WOSNEG       TEST POSITIVE/NEGATIVE                       
         BNE   *+10                                                             
         MP    POSTAMNT,=P'-1'                                                  
         TM    WOINDS1,WOIANACA    TEST CONTRA ONLY FOR ANALYSIS                
         BO    PSTW320                                                          
         BAS   RE,SETCACN                                                       
         B     PSTW330                                                          
*                                                                               
PSTW320  MVC   POSTCACN,BCSPACES                                                
         MVI   POSTCAC,C'*'                                                     
         TM    WOINDS1,WOICAN2P    TEST CONTRA NAME FROM 2P ACCOUNT             
         BZ    PSTW322                                                          
         MVC   BOWORK1(L'POSTCAC),POSTCAC                                       
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),(SPAELQ,AIO8),              *        
               (1,=AL1(SPATW2PA))                                               
         CLI   12(R1),0                                                         
         BNE   PSTW330                                                          
         L     RF,12(R1)                                                        
         MVC   POSTCAC(L'CUABIN),CUABIN                                         
         MVC   POSTCAC+L'CUABIN(L'SPAAULA),SPAAULA-SPAELD(RF)                   
         BAS   RE,SETCACN                                                       
         MVC   POSTCAC,BOWORK1                                                  
         B     PSTW330                                                          
PSTW322  TM    WOINDS1,WOICANCL    TEST CONTRA NAME FROM CLIENT                 
         BZ    PSTW330                                                          
         MVC   POSTCACN,BCCLINAM                                                
*                                                                               
PSTW330  GOTO1 ABLDTRN,BOPARM,POSTVALS                                          
         BH    EXIT2                                                            
*                                                                               
PSTW370  LA    R2,WOTABL(R2)                                                    
         B     PSTW310                                                          
         DROP  R2                                                               
*                                                                               
PSTW380  GOTO1 AXFRTMS,BOPARM,AIO1,(C'W',PTAELD),POSTVALS                       
         B     PSTW210             MAY BE ANOTHER WOFF PTAEL (REC)              
*                                                                               
PSTW900  L     R4,UPDLSTDA         GET NEXT FROM D/A LIST                       
         LA    R4,L'TRNKDA+L'TRNKSTA2(R4)                                       
         B     PSTW100                                                          
*                                  FINAL CALL CLOSES REPORT ETC                 
PSTW500  OC    LSUTRFRF,LSUTRFRF   TEST TRANSFER UPDATE REQUIRED                
         BNZ   PSTXFR                                                           
         LA    R4,UPDPOSTB                                                      
         GOTO1 ABLDTRN,BODMCB,('FF',POSTVALS)                                   
         BH    EXIT2                                                            
         BAS   RE,PRTSUMM                                                       
         B     PSTBPRNT                                                         
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*              UPDATE JOB-TO-JOB TRANSFERS                           *          
**********************************************************************          
         SPACE 1                                                                
PSTXFR   OC    LSUTRFRF,LSUTRFRF   TEST TRANSFER UPDATE REQUIRED                
         BZ    PSTFEE                                                           
         LA    R4,UPDDATAB         DA TABLE                                     
         CLI   UPDINIT,YESQ        TEST IF REPORT/ADDTRN INITIALISED            
         BE    PSTX100                                                          
         GOTO1 APINIT,(RC)         INITIALISE REPORT AND ADDTRN                 
PSTX100  ST    R4,UPDLSTDA         SAVE A(CURRENT TABLE ENTRY)                  
         CLI   0(R4),FF            FIN                                          
         BE    PSTX900             CLOSE REPORT ETC                             
         TM    L'TRNKDA(R4),TRNSXFRP                                            
         BZ    PSTX800             NO TRANSFER PENDING HERE                     
         MVC   IODAOVER,0(R4)                                                   
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
*                                                                               
*        GOTO1 AGETOPT,BODMCB,AIO1                                              
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,AIO1,AGOPBLK,ACOM,(R0),LSPRATA,0                 
*                                                                               
         LA    R3,TRNRFST                                                       
         USING PTAELD,R3                                                        
         SR    R0,R0                                                            
PSTX120  IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         CLI   PTAEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PTAEL,PTAELQ                                                     
         BNE   PSTX120                                                          
         CLI   PTATYPE,PTATTRFT    TEST TRANSFER TO                             
         BNE   PSTX120                                                          
         TM    PTASTAT1,PTASPEND   TEST PENDING                                 
         BNO   PSTX120                                                          
*                                  UPDATE PTAEL/TRNREC PENDING STATUS           
         NI    PTASTAT1,FF-PTASPEND                                             
         MVC   PTAMOA,LSUTRFMO                                                  
*&&US*&& MVC   PTADATE,BCTODAYC    SET TODAY FOR DISPLAY                        
         NI    TRNRSTA2,FF-TRNSXFRP                                             
         CLI   CSACT,ACTDRA        TEST ACTION IS DRAFT                         
         BE    PSTX130                                                          
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('TRSELQ',AIO1),0,0                  
         CLI   BODMCB+12,0                                                      
         BNE   *+12                                                             
         L     RE,BODMCB+12                                                     
         OI    TRSSTAT3-TRSELD(RE),TRSSNBIL                                     
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('TRXELQ',AIO1),0,0                  
         CLI   BODMCB+12,0                                                      
         BNE   PSTX122                                                          
         L     RE,BODMCB+12                                                     
         NI    TRXSTA2-TRXELD(RE),FF-TRXSXFRP                                   
         CP    PTANET,TRNRFST+(TRNAMNT-TRNELD)(L'TRNAMNT)                       
         BNE   PSTX122                                                          
         OI    TRXSTA1-TRXELD(RE),TRXSXALC                                      
*                                                                               
PSTX122  GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         L     R1,AIO1                                                          
         MVC   TRNKEY,0(R1)                                                     
         GOTO1 AIO,IORDUPD+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    TRNKSTA2,FF-TRNSXFRP                                             
         GOTO1 AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  UPDATE WORK-CODE SUB-TOTALS                  
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,AIO1,AGOPBLK,ACOM,(R0),LSPRATAS,0                
         GOTO1 ASETTRN,BODMCB,(C'S',IOKEY),AIO1,LSPRATA                         
         GOTO1 ASUBTOT,BOPARM,(C'U',LSPRATA),LSPRATAS                           
*                                                                               
         USING POSTVALS,R4         R4=A(POSTING VALUES BLOCK)                   
PSTX130  LA    R4,UPDPOSTB                                                      
         L     R2,AIO1             RESET R2=A(TRNREC)                           
*                                                                               
         MVC   XFRTOCST,BCSPACES   CLEAR ACCOUNT CODES                          
         MVC   XFRSIACC,BCSPACES                                                
         MVC   XFRSICST,BCSPACES                                                
         MVC   XFRSKACC,BCSPACES                                                
*                                                                               
         MVC   XFR1CACC,BCCMPPRF+(PPRCOSTU-PPRELD)                              
         MVC   XFRFROFC,CSOFFICE   SET FROM 1C ACCOUNT/OFFICE                   
*                                                                               
         LA    RE,TRNRFST                                                       
         XR    RF,RF                                                            
PSTX132  CLI   0(RE),0                                                          
         BE    PSTX140                                                          
*                                                                               
         USING SPAELD,RE                                                        
         CLI   SPAEL,SPAELQ        TEST 1C ACCOUNT SAVED ON TRANSACTION         
         BNE   PSTX134                                                          
         CLI   SPATYPE,SPATCCST                                                 
         BNE   PSTX134                                                          
         MVC   XFR1CACC,SPAAULA                                                 
         B     PSTX138                                                          
*                                                                               
         USING ANOELD,RE                                                        
PSTX134  CLI   ANOEL,ANOELQ        TEST FOR CLIENT OFFICE                       
         BNE   PSTX138                                                          
         CLI   ANOTYPE,ANOTCLI                                                  
         BNE   PSTX138                                                          
         MVC   XFRFROFC,ANOOFFC                                                 
         DROP  RE                                                               
*                                                                               
PSTX138  IC    RF,1(RE)                                                         
         BXH   RE,RF,PSTX132                                                    
*                                                                               
PSTX140  CLC   =C'SK',TRNKULC      SET SK OR SI ACCOUNT FROM CONTRA             
         BNE   *+10                                                             
         MVC   XFRSKACC,TRNKULC                                                 
         CLC   =C'SI',TRNKULC                                                   
         BNE   *+10                                                             
         MVC   XFRSIACC,TRNKULC                                                 
         CLC   XFRSKACC,BCSPACES                                                
         BNE   PSTX150                                                          
         CLC   XFRSIACC,BCSPACES                                                
         BNE   PSTX150                                                          
         BAS   RE,GETSKSI          FIND SK OR SI ACCOUNT IN MEMO                
*                                                                               
PSTX150  CLC   XFRSIACC,BCSPACES   IF SI ACCOUNT READ FOR COSTING CODE          
         BE    PSTX160                                                          
         MVC   POSTCAC(L'CUABIN),CUABIN                                         
         MVC   POSTCAC+(L'CUABIN)(L'TRNKULC),XFRSIACC                           
         BAS   RE,SETCACN                                                       
         MVC   XFRSICST,CACACTP-CACTABD(RF)                                     
*                                                                               
         USING CACTABD,RF                                                       
PSTX160  LA    R0,CACMAXQ          SEE IF THIS JOB IN ACCOUNT TABLE             
         LA    RF,CACTAB                                                        
         CLI   CACACTK,0                                                        
         BE    PSTX170                                                          
         CLC   PTATJOB,CACACTK+(TRNKACT-TRNKUNT)                                
         BE    PSTX200                                                          
         LA    RF,CACTABLQ(RF)                                                  
         BCT   R0,PSTX160+L'PSTX160                                             
         LA    RF,CACTAB                                                        
*                                                                               
PSTX170  BAS   RE,GETJBCST         READ CLI/PRO/JOB AND SAVE COSTING            
                                                                                
PSTX200  MVC   XFRTOCST,CACACTP    SET TO JOB COSTING ACCOUNT                   
         MVC   XFRTOOFC,CACOFFC    SET TO JOB OFFICE CODE                       
         DROP  RF                                                               
*                                                                               
PSTX240  MVC   POSTBTMC,LSUTRFMC                                                
         MVC   POSTBTRF,LSUTRFRF                                                
         MVI   POSTTYPE,POSTTRNF                                                
         MVC   POSTACT,TRNKULA                                                  
         MVC   POSTACTN,BCJOBNAM                                                
         MVC   POSTCAC,TRNKCULC                                                 
         BAS   RE,SETCACN                                                       
         MVC   POSTREF,TRNKREF                                                  
         MVC   POSTDATE,TRNKDATE                                                
         MVI   POSTSTAT,TRNSDR                                                  
         TM    TRNRFST+(TRNSTAT-TRNELD),TRNSNOCM                                
         BNO   *+8                                                              
         OI    POSTSTAT,TRNSNOCM   DR TO ORIG JOB GETS SAME STATUS              
         TM    TRNRFST+(TRNSTAT-TRNELD),TRNSAUTH                                
         BNO   *+8                                                              
         OI    POSTSTAT,TRNSAUTH   POSTINGS GET ORIGINAL AUTH STATUS            
         ZAP   POSTAMNT,PTANET                                                  
         MP    POSTAMNT,=P'-1'                                                  
         MVC   POSTOFFC,TRNKWORK                                                
         XC    POSTPTRS,POSTPTRS                                                
         XC    POSTNARR,POSTNARR                                                
         LA    R2,TRNRFST                                                       
         USING TRNELD,R2                                                        
         SR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         SH    RF,=Y(TRNNARR-TRNELD+1)                                          
         BM    *+14                                                             
         EX    RF,*+4                                                           
         MVC   POSTNARR(0),TRNNARR                                              
         BAS   RE,XTRAELS                                                       
*                                  COPY OVER THE 1C ACCOUNT CODE                
         LR    RF,R2                                                            
PSTX242  SR    RE,RE                                                            
         IC    RE,SPALN-SPAELD(RF)                                              
         AR    RF,RE                                                            
         CLI   0(RF),0             TEST E-O-R                                   
         BE    PSTX246                                                          
         CLI   0(RF),SPAELQ                                                     
         BNE   PSTX244                                                          
         CLI   SPATYPE-SPAELD(RF),SPATCCST                                      
         BNE   PSTX242                                                          
         IC    RE,SPALN-SPAELD(RF)                                              
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BOELEM(0),0(RF)                                                  
         BAS   RE,ADDXTRA                                                       
         B     PSTX242                                                          
*                                                                               
PSTX244  CLI   0(RF),ANOELQ        COPY ANALYSED OFFICE ELEMENT                 
         BNE   PSTX242                                                          
         CLI   ANOTYPE-ANOELD(RF),ANOTCLI                                       
         BNE   PSTX242                                                          
         SR    RE,RE                                                            
         IC    RE,ANOLN-ANOELD(RF)                                              
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BOELEM(0),0(RF)                                                  
         BAS   RE,ADDXTRA                                                       
         B     PSTX242                                                          
*                                                                               
PSTX246  LA    RF,BOELEM                                                        
         USING PXDELD,RF                                                        
         MVI   PXDEL,PXDELQ                                                     
         MVI   PXDLN,PXDLNQ                                                     
         MVI   PXDTYPE,PXDTTO                                                   
         MVC   PXDDATE,BCTODAYP                                                 
         MVC   PXDFRTOC,CUABIN                                                  
         MVC   PXDFRTOU(2),POSTACT                                              
         MVC   PXDFRTOA,PTATJOB                                                 
         BAS   RE,ADDXTRA                                                       
*                                                                               
         PUSH  USING                                                            
         USING TRSELD,BOELEM                                                    
         XC    TRSEL(TRSLNQ),TRSEL                                              
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         OI    TRSSTAT3,TRSSNBIL   NOT BILLABLE ON A21                          
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('TRSELQ',AIO1),0,0                  
         CLI   BODMCB+12,0                                                      
         BNE   PSTX250                                                          
         L     RE,BODMCB+12                                                     
         MVC   BOBYTE1,TRSSTAT2-TRSELD(RE)                                      
         NI    BOBYTE1,TRSSTADJ+TRSSTMSS+TRSSTIME                               
         MVC   TRSSTAT2,BOBYTE1                                                 
PSTX250  BAS   RE,ADDXTRA                                                       
         POP   USING                                                            
*                                                                               
PSTX300  LA    R0,UPDXTRAS         COPY EXTRA ELEMENT AREA INTO SAVE            
         LA    RE,UPDXTRA                                                       
         LH    R1,=Y(L'UPDXTRAS)                                                
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         NI    POSTSTAT,FF-TRNSNOCM                                             
         TM    PTASTAT2,PTASXCOM   SET NEW COMMISSIONABLE STATUS                
         BO    *+8                                                              
         OI    POSTSTAT,TRNSNOCM                                                
*                                                                               
         LA    R0,UPDXTRA          RESTORE EXTRA ELEMENT SAVE AREA              
         LA    RE,UPDXTRAS                                                      
         LH    R1,=Y(L'UPDXTRAS)                                                
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     RF,POSTXTRA         REVERSE SCIEL AMOUNTS                        
         SR    R0,R0                                                            
         USING SCIELD,RF                                                        
PSTX310  CLI   SCIEL,0                                                          
         BE    PSTX350                                                          
         CLI   SCIEL,SCIELQ                                                     
         BNE   PSTX315                                                          
         ZAP   BODUB1,SCIAMNT                                                   
         MP    BODUB1,=P'-1'                                                    
         ZAP   SCIAMNT,BODUB1                                                   
         CLI   SCILN,SCILN2Q                                                    
         BNE   PSTX345                                                          
         ZAP   BODUB1,SCIADMN                                                   
         MP    BODUB1,=P'-1'                                                    
         ZAP   SCIADMN,BODUB1                                                   
         B     PSTX345                                                          
PSTX315  DS    0H                                                               
*&&US                                                                           
         USING PRTELD,RF                                                        
         CLI   PRTEL,PRTELQ        REVERSE PRTHOUR                              
         BNE   PSTX320                                                          
         ZAP   BODUB1,PRTHOUR                                                   
         MP    BODUB1,=P'-1'                                                    
         ZAP   PRTHOUR,BODUB1                                                   
         B     PSTX345                                                          
*&&                                                                             
         USING PXDEL,RF                                                         
PSTX320  CLI   PXDEL,PXDELQ        CHANGE POSTING TRANSFER ELEMENT              
         BNE   PSTX330                                                          
         MVI   PXDTYPE,PXDTFROM                                                 
         MVC   PXDFRTOA,BCJOBCOD                                                
         B     PSTX345                                                          
         USING TRXELD,RF                                                        
PSTX330  CLI   TRXEL,TRXELQ        REMOVE EXCLUDE FROM ALLOC STATUS             
         BNE   PSTX340                                                          
         NI    TRXSTA1,FF-TRXSXALC                                              
         USING TRSELD,RF                                                        
PSTX340  CLI   TRSEL,TRSELQ        REMOVE NOT 21 BILLABLE STATUS                
         BNE   PSTX342                                                          
         NI    TRSSTAT3,FF-TRSSNBIL                                             
         USING SPAELD,RF                                                        
PSTX342  CLI   SPAEL,SPAELQ                                                     
         BNE   PSTX344                                                          
         MVC   SPAAULA,XFRTOCST                                                 
         B     PSTX345                                                          
*                                                                               
         USING ANOELD,RF                                                        
PSTX344  CLI   ANOEL,ANOELQ                                                     
         BNE   PSTX345                                                          
         MVC   ANOOFFC,XFRTOOFC                                                 
*                                                                               
PSTX345  IC    R0,ANOLN                                                         
         AR    RF,R0                                                            
         B     PSTX310                                                          
*                                                                               
PSTX350  MVC   BCWORK(L'POSTCAC),POSTCAC  SAVE CONTRA ACCOUNT CODE              
         MVC   POSTACTN,POSTCACN          SAVE CONTRA ACCOUNT NAME              
         MVC   POSTCAC+1(TRNKACT-TRNKULA),POSTACT                               
         MVC   POSTCAC+(TRNKACT-TRNKCPY)(L'TRNKACT),PTATJOB                     
         MVC   POSTOFFC,PTATWRK                                                 
         BAS   RE,SETCACN                                                       
         XC    POSTACTN,POSTCACN   SWAP ACCOUNT/CONTRA NAMES                    
         XC    POSTCACN,POSTACTN                                                
         XC    POSTACTN,POSTCACN                                                
         MVC   POSTACT,POSTCAC+1   SET POSTING ACCOUNT (NEW JOB)                
         MVC   POSTCAC,BCWORK      RESTORE SAVED CONTRA ACCOUNT                 
         CLI   PTALN,PTATLN1Q      TEST SK ACCOUNT OVERRIDDEN                   
         BNH   PSTX360                                                          
         MVC   POSTCAC+(ACTKACT-ACTKCPY)(L'PTATSKAC),PTATSKAC                   
         BAS   RE,SETCACN          SET NEW SK CONTRA NAME                       
PSTX360  L     RF,AIO1                                                          
         MP    POSTAMNT,=P'-1'                                                  
*                                                                               
         TM    PTASTAT2,PTASXISA   TEST IF TRANSFER IS ALLOCATED                
         BNO   PSTX380                                                          
         MVI   POSTSTA2,POSTALCQ   SET ITEM IS ALLOCATED                        
         GOTO1 AALLXFR,(RC)                                                     
         CLI   BOELEM,PTAELQ       CHECK IF ELEMENT BUILT                       
         BNE   *+12                                                             
         BAS   RE,ADDXTRA          ADD EXTRA ELEMENT                            
         B     PSTX380                                                          
         MVI   POSTSTA2,POSTNALQ   SET 'SHOULD HAVE BEEN ALLOC' FLAG            
         MVC   FVMSGNO,=Y(AI$LOKXF)                                             
         MVI   FVPARMS,4                                                        
         MVC   FVPARMS+1(3),PTATJOB                                             
PSTX380  GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         MVI   POSTSTA2,0                                                       
         L     RE,AIO3             IF ALLOCATED UPDATE JOB                      
         TM    TRNRSTA2-TRNRECD(RE),TRNSBILP                                    
         BNO   PSTX390                                                          
         CLI   POSTMODE,POSTLVQ                                                 
         BNE   PSTX390                                                          
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,AIO3,AGOPBLK,ACOM,(R0),LSPRATA,0                 
*                                                                               
         L     RE,AIO4             CHECK JOB RECORD IS THERE                    
         CLC   POSTACT,ACTKUNT-ACTKEY(RE)                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AUPDJOB,BOPARM,(C'U',AIO3),AIO4,0,LSPRATA                        
         EJECT                                                                  
***********************************************************************         
*              MAKE EXTRA TRANSFER POSTINGS SK SI AND COSTING         *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
PSTX390  L     R2,AIO1                                                          
                                                                                
         CLC   XFRSKACC,BCSPACES   TEST ANY SK ACCOUNT                          
         BE    PSTX400                                                          
*&&UK                                                                           
         CLC   PTATJOB,TRNKACT                                                  
         BE    PSTX400             SAME JOB - DO NOT BOTHER                     
*&&                                                                             
         MVC   POSTACT,XFRSKACC    POST TO ORIGINAL SK ACCOUNT                  
         MVC   POSTCAC,TRNKCULA                                                 
         MVC   POSTCACN,BCJOBNAM                                                
         MVC   POSTOFFC,XFRFROFC                                                
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
         MVI   POSTSTAT,0                                                       
         MP    POSTAMNT,=P'-1'                                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
*                                                                               
         CLI   PTALN,PTATLN1Q      TEST SK ACCOUNT OVERRIDDEN                   
         BNH   *+10                                                             
         MVC   POSTACT+(ACTKACT-ACTKUNT),PTATSKAC                               
         MVC   POSTCAC+(ACTKACT-ACTKEY),PTATJOB                                 
         MVC   POSTOFFC,XFRTOOFC                                                
         BAS   RE,SETCACN                                                       
         MP    POSTAMNT,=P'-1'                                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
*                                                                               
PSTX400  CLC   XFRSIACC,BCSPACES   TEST ANY SI ACCOUNT                          
         BE    PSTX500                                                          
         MVC   POSTACT,XFRSIACC                                                 
         MVC   POSTCAC,TRNKCULA    CONTRA IS FROM-JOB                           
         MVC   POSTCACN,BCJOBNAM                                                
         MVC   POSTOFFC,XFRFROFC                                                
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
         MVI   POSTSTAT,0          CREDIT SI                                    
         ZAP   POSTAMNT,PTANET                                                  
         MP    POSTAMNT,=P'-1'                                                  
*&&US                                                                           
         LA    R1,BCJOBCOD                                                      
         BAS   RE,ADDMDTEL         ADD MEDIA TRANSFER ELEMENT                   
         LA    R1,TRNKWORK                                                      
         BAS   RE,ADDRFLEL         ADD WORKCODE FILTER ELEMENT                  
*                                                                               
         LH    RE,=Y(UPDPTRS-UPDWORKD)                                          
         AR    RE,RC                                                            
         ST    RE,POSTPTRS         ADD ANALYSIS POINTERS                        
         LA    R1,TRNKULC                                                       
         CLC   =C'1R',0(R1)                                                     
         BNE   *+16                                                             
         ST    R1,0(RE)                                                         
         MVI   0(RE),APENSDR       DR - 1R                                      
         LA    RE,4(RE)                                                         
         LA    R1,XFR1CACC                                                      
         ST    R1,0(RE)                                                         
         MVI   0(RE),APENSDR       DR - 1C                                      
         LA    RE,4(RE)                                                         
         LA    R1,XFRSICST                                                      
         ST    R1,0(RE)                                                         
         MVI   0(RE),0             CR - 12                                      
         MVI   4(RE),FF            E-O-L                                        
*&&                                                                             
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
*                                                                               
         MVC   POSTCAC+(ACTKACT-ACTKEY),PTATJOB                                 
         BAS   RE,SETCACN                                                       
         MVC   POSTOFFC,XFRTOOFC                                                
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
         MVI   POSTSTAT,0          CREDIT SI                                    
         MP    POSTAMNT,=P'-1'                                                  
*&&US                                                                           
         LA    R1,PTATJOB                                                       
         BAS   RE,ADDMDTEL         ADD MEDIA TRANSFER ELEMENT                   
         LA    R1,PTATWRK                                                       
         BAS   RE,ADDRFLEL         ADD WORKCODE FILTER ELEMENT                  
*                                                                               
         L     RE,POSTPTRS         PICK UP LIST OF ANALYSIS POINTERS            
         CLC   =C'1R',TRNKULC                                                   
         BNE   *+8                                                              
         LA    RE,4(RE)                                                         
         LA    R1,XFRTOCST         SET A(NEW 1C ACCOUNT)                        
         ST    R1,0(RE)                                                         
         MVI   0(RE),APENSDR                                                    
*&&                                                                             
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         XC    POSTPTRS,POSTPTRS                                                
*                                                                               
PSTX500  CLC   XFRTOCST,XFR1CACC                                                
         BE    PSTX700             SAME JOB COST CODE - NO COSTING              
         CLC   XFRSIACC,BCSPACES                                                
         BE    PSTX600             NO SI ACCOUNT - NO COSTING                   
         MVC   POSTACT,XFR1CACC                                                 
         MVC   POSTCAC+(ACTKUNT-ACTKEY),XFRSICST                                
         BAS   RE,SETCACN                                                       
         MVC   POSTOFFC,XFRFROFC                                                
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
         MVI   POSTSTAT,TRNSDR     DEBIT 1C/12                                  
         ZAP   POSTAMNT,PTANET                                                  
         MP    POSTAMNT,=P'-1'                                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
*                                                                               
         XC    POSTACT,POSTCAC+1                                                
         XC    POSTCAC+1(L'POSTACT),POSTACT                                     
         XC    POSTACT,POSTCAC+1                                                
         BAS   RE,SETCACN                                                       
         MVI   POSTSTAT,0          CREDIT 12/1C                                 
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
*                                                                               
         MVC   POSTACT,XFRTOCST                                                 
         MVC   POSTCAC+(ACTKUNT-ACTKEY),XFRSICST                                
         BAS   RE,SETCACN                                                       
         MVC   POSTOFFC,XFRTOOFC                                                
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
         MVI   POSTSTAT,TRNSDR     DEBIT 1C/12                                  
         ZAP   POSTAMNT,PTANET                                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
*                                                                               
         XC    POSTACT,POSTCAC+1                                                
         XC    POSTCAC+1(L'POSTACT),POSTACT                                     
         XC    POSTACT,POSTCAC+1                                                
         BAS   RE,SETCACN                                                       
         MVI   POSTSTAT,0          CREDIT 12/1C                                 
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
*                                                                               
PSTX600  TM    BCCPYST7,CPYSTMSY   TEST IF TMS USER                             
         BO    PSTX700                                                          
         CLC   =C'1R',TRNKULC      TEST IF 1R/1C POSTINGS REQUIRED              
         BNE   PSTX700             ONLY FOR 1R CONTRA                           
         OC    PTAHOURS,PTAHOURS                                                
         BZ    PSTX700             IF NO HOURS DON'T BOTHER                     
         MVC   POSTACT,TRNKULC                                                  
         MVC   POSTCAC+(ACTKUNT-ACTKEY),XFR1CACC                                
         BAS   RE,SETCACN                                                       
         MVC   POSTOFFC,XFRFROFC                                                
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
         MVI   POSTSTAT,TRNSDR     DEBIT 1R/1C                                  
         ZAP   POSTAMNT,PTANET                                                  
         MP    POSTAMNT,=P'-1'                                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
*                                                                               
         MVC   POSTCAC+(ACTKUNT-ACTKEY),XFRTOCST                                
         MVC   POSTOFFC,XFRTOOFC                                                
         BAS   RE,SETCACN                                                       
         MP    POSTAMNT,=P'-1'     DEBIT 1R/1C                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
*                                                                               
PSTX700  GOTO1 AXFRTMS,BOPARM,TRNRECD,(C'T',PTAELD),POSTVALS                    
*                                                                               
PSTX800  L     R4,UPDLSTDA         GET NEXT FROM D/A LIST                       
         LA    R4,L'TRNKDA+L'TRNKSTA2(R4)                                       
         B     PSTX100                                                          
*                                                                               
PSTX900  OC    UPDFEERF,UPDFEERF   TEST TRANSFER UPDATE REQUIRED                
         BNZ   PSTFEE                                                           
         LA    R4,UPDPOSTB                                                      
         GOTO1 ABLDTRN,BODMCB,('FF',POSTVALS)                                   
         BH    EXIT2                                                            
         BAS   RE,PRTSUMM                                                       
         B     PSTBPRNT                                                         
         EJECT                                                                  
*                                                                               
**********************************************************************          
*              UPDATE US FEE ADJUSTMENTS                             *          
**********************************************************************          
         SPACE 1                                                                
PSTFEE   OC    UPDFEERF,UPDFEERF   TEST FEES UPDATE REQUIRED                    
         BZ    PSTBPRNT                                                         
         LA    R4,UPDDATAB         DA TABLE                                     
         CLI   UPDINIT,YESQ        TEST IF REPORT/ADDTRN INITIALISED            
         BE    PSTF100                                                          
         GOTO1 APINIT,(RC)         INITIALISE REPORT AND ADDTRN                 
*                                                                               
PSTF100  ST    R4,UPDLSTDA         SAVE A(CURRENT TABLE ENTRY)                  
         CLI   0(R4),FF            FIN                                          
         BE    PSTF900             CLOSE REPORT ETC                             
         TM    L'TRNKDA(R4),CBILFEEA                                            
         BZ    PSTF800             NOT A FEE ADJUSTMENT                         
         MVC   IODAOVER,0(R4)                                                   
         GOTO1 AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         CLI   CSACT,ACTDRA        TEST ACTION IS DRAFT                         
         BE    PSTF105                                                          
         USING TRNRECD,R2                                                       
         OI    TRNRSTAT,TRNSDELT   DELETE THE FEEADJ RECORD                     
         GOTO1 AIO,IOPUT+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         L     R1,AIO3                                                          
         MVC   TRNKEY,0(R1)                                                     
         GOTO1 AIO,IORDUPD+IOACCDIR+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TRNKSTAT,TRNSDELT   DELETE DIRECTORY RECORD                      
         GOTO1 AIO,IOWRITE+IOACCDIR+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING POSTVALS,R4         R4=A(POSTING VALUES BLOCK)                   
PSTF105  LA    R4,UPDPOSTB                                                      
         L     R2,AIO3             RESET R2=A(FEEADJ TRANSACTION)               
         USING TRNELD,TRNRFST                                                   
*                                                                               
         MVC   UPDFEEAN,BCSPACES                                                
         CLC   =C'SI',TRNKULC      IF INCOME GET ANALYSIS ACCOUNT CODE          
         BNE   PSTF112                                                          
         USING ACTRECD,IOKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCULA,TRNKCULC                                                
         GOTO1 AGETACT,0                                                        
         BE    PSTF110                                                          
         CLI   CSACT,ACTDRA                                                     
         BE    EXIT2                                                            
         DC    H'0'                                                             
PSTF110  EQU   *                                                                
*&&UK*&& MVC   UPDFEEAN(L'ACCOST),ACCOST                                        
*&&US                                                                           
         ICM   RF,15,ACASPA                                                     
         BZ    *+10                                                             
         MVC   UPDFEEAN,SPAAULA-SPAELD(RF)                                      
*&&                                                                             
*                                                                               
PSTF112  MVC   POSTBTMC,UPDFEEMC                                                
         MVC   POSTBTRF,UPDFEERF                                                
         MVI   POSTTYPE,8                                                       
         MVC   POSTACT,TRNKULA                                                  
         MVC   POSTACTN,BCJOBNAM                                                
         MVC   POSTCAC,TRNKCULC                                                 
         BAS   RE,SETCACN                                                       
         MVC   POSTREF,TRNKREF                                                  
         MVC   POSTDATE,TRNKDATE                                                
         MVI   POSTSTAT,TRNSDR                                                  
         TM    TRNSTAT,TRNSNOCM                                                 
         BNO   *+8                                                              
         OI    POSTSTAT,TRNSNOCM                                                
         TM    TRNSTAT,TRNSAUTH                                                 
         BNO   *+8                                                              
         OI    POSTSTAT,TRNSAUTH                                                
         ZAP   POSTAMNT,TRNAMNT                                                 
         MVC   POSTOFFC,TRNKWORK                                                
         XC    POSTPTRS,POSTPTRS                                                
         XC    POSTNARR,POSTNARR                                                
         SR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         SH    RF,=Y(TRNNARR-TRNELD+1)                                          
         BM    *+14                                                             
         EX    RF,*+4                                                           
         MVC   POSTNARR(0),TRNNARR                                              
*                                                                               
         MVC   UPDFOFFC,BCSPACES   CLEAR SAVED FINANCIAL OFFICE                 
         LA    R3,TRNELD                                                        
PSTF120  CLI   PTAEL,0                                                          
         BE    PSTF140                                                          
         CLI   PTAEL,PTAELQ                                                     
         BE    PSTF130                                                          
         CLI   PTAEL,FFTELQ                                                     
         BNE   PSTF122                                                          
         CLI   FFTTYPE-FFTELD(R3),FFTTOFFC                                      
         BNE   PSTF122                                                          
         MVC   UPDFOFFC,FFTDATA-FFTELD(R3)                                      
PSTF122  SR    RE,RE                                                            
         IC    RE,PTALN                                                         
         AR    R3,RE                                                            
         B     PSTF120                                                          
PSTF130  IC    RE,PTALN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BOELEM(0),PTAEL                                                  
         BAS   RE,ADDXTRA                                                       
         B     PSTF122                                                          
*                                                                               
PSTF140  GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
*                                                                               
         L     R2,AIO3                                                          
         CLI   CSACT,ACTDRA        TEST IF UPDATING FILE                        
         BE    PSTF160                                                          
         TM    TRNRSTA2-TRNRECD(RE),TRNSBILP                                    
         BNO   PSTF160                                                          
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,AIO3,AGOPBLK,ACOM,(R0),LSPRATA,0                 
U        USING TRNRECD,IOKEY                                                    
         MVC   U.TRNKEY,TRNKEY                                                  
         MVC   U.TRNKSTA,TRNRSTA                                                
         MVC   U.TRNKDA,2000-4(R2)                                              
         DROP  U                                                                
         GOTO1 ASETTRN,BODMCB,(C'S',IOKEY),AIO3,LSPRATA                         
*                                                                               
         L     RE,AIO4             CHECK JOB RECORD IS THERE                    
         CLC   TRNKCULA,0(RE)                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AUPDJOB,BOPARM,(C'U',AIO3),AIO4,0,LSPRATA                        
*                                                                               
PSTF160  MVI   POSTSTAT,0          CLEAR STATUS - CREDIT                        
         MVC   POSTOFFC,CSOFFICE                                                
         CLC   UPDFOFFC,BCSPACES   TEST ANY OVERRIDE FINANCIAL OFFICE           
         BNH   *+10                                                             
         MVC   POSTOFFC,UPDFOFFC                                                
         MVC   POSTACT,TRNKULC                                                  
         MVC   POSTCAC,TRNKCULA    FULL JOB FOR SK CONTRA                       
         CLC   =C'SK',POSTACT                                                   
         BE    PSTF200                                                          
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
*                                                                               
         USING SCIELD,BOELEM                                                    
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGRSS                                                 
         ZAP   SCIAMNT,BCPZERO                                                  
         BAS   RE,ADDXTRA                                                       
         LA    R1,BCJOBCOD                                                      
*                                  FUDGE ADDRESS OF PTAEL                       
         LA    R3,UPDFEEMO-(PTAMOA-PTAELD)                                      
         BAS   RE,ADDMDTEL         ADD MEDIA TRANSFER ELEMENT                   
*                                  SET UP ANALYSIS CODES FOR APEEL              
         LH    RE,=Y(UPDPTRS-UPDWORKD)                                          
         AR    RE,RC                                                            
         STCM  RE,15,POSTPTRS                                                   
         LA    RF,ACTKULA          RF=A(U/L/CLI/PRO/JOB)                        
         STCM  RF,15,0(RE)                                                      
         MVI   0(RE),APENSDR       JOB POSTING WAS A DEBIT                      
         LA    RE,4(RE)                                                         
         CLC   UPDFEEAN,BCSPACES   TEST ANY COSTING CODE                        
         BNH   PSTF180                                                          
         LA    RF,BCCMPPRF+(PPRCOSTU-PPRELD)                                    
         STCM  RF,15,0(RE)                                                      
         MVI   0(RE),APENSDR       1C POSTING IS A DEBIT                        
         LA    RE,4(RE)                                                         
         MVC   UPDFEECA(L'ULREV),ULREV                                          
         MVC   UPDFEECA+L'ULREV(L'UPDFEEAN),UPDFEEAN                            
         LA    RF,UPDFEECA                                                      
         STCM  RF,15,0(RE)                                                      
         MVI   0(RE),0             REVENUE POSTING IS CREDIT                    
         LA    RE,4(RE)                                                         
PSTF180  MVI   0(RE),X'FF'                                                      
*                                                                               
PSTF200  DS    0H                                                               
*&&US                                                                           
         LA    R1,TRNKWORK                                                      
         BAS   RE,ADDRFLEL         ADD WORKCODE FILTER ELEMENT                  
*&&                                                                             
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT2                                                            
         XC    POSTPTRS,POSTPTRS                                                
         CLC   =C'SK',POSTACT      NO COSTING FOR SK                            
         BE    PSTF800                                                          
*                                                                               
         CLC   UPDFEEAN,BCSPACES   TEST ANY ANALYSIS ACCOUNT                    
         BNH   PSTF800                                                          
         MVC   POSTACT,BCCMPPRF+(PPRCOSTU-PPRELD)                               
         MVC   POSTACTN,UPDCSTNM                                                
         MVC   POSTCAC,BCSPACES                                                 
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'ACTKCPY(ACTKACT-ACTKUNT),ULREV                         
         MVC   POSTCAC+(ACTKACT-ACTKEY)(L'UPDFEEAN),UPDFEEAN                    
         BAS   RE,SETCACN                                                       
         MVI   POSTSTAT,TRNSDR                                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BNE   EXIT2                                                            
         MVC   POSTACT,POSTCAC+(ACTKUNT-ACTKEY)                                 
         MVC   POSTACTN,POSTCACN                                                
         MVC   POSTCAC,BCCMPPRF+(PPRCOST-PPRELD)                                
         MVC   POSTCACN,UPDCSTNM                                                
         MVI   POSTSTAT,0                                                       
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BNE   EXIT2                                                            
*                                                                               
PSTF800  L     R4,UPDLSTDA         GET NEXT FROM D/A LIST                       
         LA    R4,L'TRNKDA+L'TRNKSTA2(R4)                                       
         B     PSTF100                                                          
*                                                                               
PSTF900  LA    R4,UPDPOSTB                                                      
         GOTO1 ABLDTRN,BODMCB,('FF',POSTVALS)                                   
         BH    EXIT2                                                            
         BAS   RE,PRTSUMM                                                       
         B     PSTBPRNT                                                         
*                                                                               
EXIT2    XIT1  ,                                                                
         DROP  R2,R3,R4,RF                                                      
         EJECT                                                                  
***********************************************************************         
*              BUILD WRITE-OFF ANLAYSIS POINTERS                      *         
*                                                                     *         
*              - PTA RECORD IN IO8                                    *         
***********************************************************************         
         SPACE 1                                                                
BLDPTRS  NTR1  ,                                                                
         LH    R3,=Y(UPDPTRS-UPDWORKD)                                          
         AR    R3,RC               R3=A(LIST OF ANALYSIS POINTERS)              
         LA    R4,WOTAB                                                         
         USING WOTABD,R4           R4=WRITE-OFF ANALYSIS TABLE                  
         L     RE,AIO1                                                          
         CLC   =C'1R',TRNKULC-TRNRECD(RE)                                       
         BNE   BPTRS02                                                          
         LA    R1,TRNKULC-TRNRECD(RE)                                           
         ST    R1,0(R3)                                                         
         MVI   0(R3),APENSDR                                                    
         LA    R3,4(R3)                                                         
*                                                                               
BPTRS02  CLI   WOTABD,EOT          TEST END OF TABLE                            
         BE    BPTRS10                                                          
         MVC   BCBYTE1,WOFILT      SET CALLERS FILTERS                          
         NI    BCBYTE1,WOIAPESK                                                 
         MVC   BCBYTE2,WOINDS1     SET THIS ENTRY FILTERS                       
         NI    BCBYTE2,WOIAPESK                                                 
         CLC   BCBYTE1,BCBYTE2     MUST MATCH                                   
         BNE   BPTRS08                                                          
         GOTO1 TESTWO,BOPARM,WOTABD  TEST VALID TABLE ENTRY                     
         BNE   BPTRS08                                                          
         TM    WOINDS1,WOIAPEAC    TEST APEEL REQUIRED FOR ACCOUNT              
         BZ    BPTRS04                                                          
         MVI   0(R3),0             SET DEBIT/CREDIT                             
         CLI   WODRCR,WODRQ                                                     
         BNE   *+8                                                              
         OI    0(R3),APENSDR                                                    
         MVC   1(3,R3),1(R1)       SET A(U/L/ACCOUNT)                           
         LA    R3,4(R3)                                                         
BPTRS04  TM    WOINDS1,WOIAPECA    TEST APEEL REQUIRED FOR CONTRA               
         BZ    BPTRS08                                                          
         MVI   0(R3),0             REVERSE DEBIT/CREDIT                         
         CLI   WODRCR,WODRQ                                                     
         BE    *+8                                                              
         OI    0(R3),APENSDR                                                    
         MVC   1(3,R3),5(R1)       SET A(U/L/ACCOUNT)                           
         LA    R3,4(R3)                                                         
*                                                                               
BPTRS08  LA    R4,WOTABL(R4)       BUMP R4 TO NEXT TABLE ENTRY                  
         B     BPTRS02                                                          
*                                                                               
BPTRS10  MVI   0(R3),X'FF'         SET END OF LIST                              
         B     EXIT2                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*              GET TO JOB COSTING ACCOUNT                             *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING PTAELD,R3                                                        
GETJBCST NTR1                                                                   
         ST    RF,UPDSAVRE         SAVE ACC TABEL ENTRY                         
         L     R1,UPDSAVRE                                                      
         USING CACTABD,R1                                                       
         MVC   CACACTK(L'BCCPYPRD),BCCPYPRD                                     
         MVC   CACACTK+L'BCCPYPRD(L'PTATJOB),PTATJOB                            
         ICM   RE,15,AIO1                                                       
         SR    RF,RF                                                            
         ICM   RF,3,TRNRLEN-TRNRECD(RE)                                         
         LR    R1,RF                                                            
         ICM   R0,15,AIO2                                                       
         MVCL  R0,RE               SAVE TRANSACTION IN IO2                      
         S     R3,AIO1                                                          
         A     R3,AIO2                                                          
*                                                                               
         USING ACTRECD,RF                                                       
         LA    RF,IOKEY                                                         
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
         SR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   PTATJOB(0),BCCLICOD TEST SAME CLIENT                             
         BNE   GJBC10                                                           
         L     R1,UPDSAVRE                                                      
         MVC   CACACTP,BCCLIPRF+(PPRCOSTU-PPRELD)                               
         MVC   CACOFFC,BCCLIPRF+(PPRGAOFF-PPRELD)                               
         IC    RE,BCPROLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   PTATJOB(0),BCCLICOD TEST SAME PRODUCT                            
         BNE   GJBC20                                                           
         OC    BCPROPRF+(PPRCOST-PPRELD)(L'PPRCOST),BCPROPRF+(PPRCOST-PX        
               PRELD)                                                           
         BZ    *+10                                                             
         MVC   CACACTP,BCPROPRF+(PPRCOSTU-PPRELD)                               
         OC    BCPROPRF+(PPRGAOFF-PPRELD)(L'PPRGAOFF),BCPROPRF+(PPRGAOFX        
               F-PPRELD)                                                        
         BZ    GJBC30                                                           
         MVC   CACOFFC,BCCLIPRF+(PPRGAOFF-PPRELD)                               
         B     GJBC30              NOW READ JOB                                 
*                                                                               
GJBC10   EX    RE,*+4              READ FOR CLIENT                              
         MVC   ACTKACT(0),PTATJOB                                               
         GOTO1 AGETACT,0                                                        
         BE    GJBC12                                                           
         DC    H'0'                                                             
GJBC12   ICM   RE,15,ACAPPR        PICK UP PROFILE                              
         L     R1,UPDSAVRE                                                      
         MVC   CACACTP,(PPRCOSTU-PPRELD)(RE)                                    
         MVC   CACOFFC,(PPRGAOFF-PPRELD)(RE)                                    
*                                                                               
GJBC20   LA    RF,IOKEY            READ FOR PRODUCT                             
         SR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ACTKACT(0),PTATJOB                                               
         GOTO1 AGETACT,0                                                        
         BE    GJBC22                                                           
         DC    H'0'                                                             
GJBC22   ICM   RE,15,ACAPPR        PICK UP PROFILE                              
         BZ    GJBC30                                                           
         L     R1,UPDSAVRE                                                      
         OC    (PPRCOST-PPRELD)(L'PPRCOST,RE),(PPRCOST-PPRELD)(RE)              
         BZ    *+10                                                             
         MVC   CACACTP,(PPRCOSTU-PPRELD)(RE)                                    
         CLC   (PPRGAOFF-PPRELD)(L'PPRGAOFF,RE),BCSPACES                        
         BNH   GJBC30                                                           
         MVC   CACOFFC,(PPRGAOFF-PPRELD)(RE)                                    
*                                                                               
GJBC30   LA    RF,IOKEY            READ FOR JOB                                 
         SR    RE,RE                                                            
         IC    RE,BCJOBLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ACTKACT(0),PTATJOB                                               
         GOTO1 AGETACT,0                                                        
         BE    GJBC32                                                           
         DC    H'0'                                                             
GJBC32   L     R1,UPDSAVRE                                                      
         ICM   RE,15,ACAPPR        PICK UP PROFILE                              
         BZ    GJBC40                                                           
         OC    (PPRCOST-PPRELD)(L'PPRCOST,RE),(PPRCOST-PPRELD)(RE)              
         BZ    *+10                                                             
         MVC   CACACTP,(PPRCOSTU-PPRELD)(RE)                                    
         CLC   (PPRGAOFF-PPRELD)(L'PPRGAOFF,RE),BCSPACES                        
         BNH   GJBC40                                                           
         MVC   CACOFFC,(PPRGAOFF-PPRELD)(RE)                                    
GJBC40   MVC   CACACTN,ACNAME                                                   
*                                                                               
         ICM   RE,15,AIO2          RESTORE TRANSACTION TO AIO1                  
         SR    RF,RF                                                            
         ICM   RF,3,TRNRLEN-TRNRECD(RE)                                         
         LR    R1,RF                                                            
         ICM   R0,15,AIO1                                                       
         MVCL  R0,RE                                                            
         L     RF,UPDSAVRE                                                      
*                                                                               
         XIT1  REGS=(RF)                                                        
         DROP  R1,RF                                                            
         EJECT                                                                  
***********************************************************************         
*              FIND SK OR SI ACCOUNT POINTER FOR TRANSFER POSTINGS    *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
GETSKSI  NTR1                                                                   
         LA    RF,TRNRFST                                                       
         SR    R0,R0                                                            
         USING APEELD,RF                                                        
GSKSI10  CLI   APEEL,0             FIND ANALYSIS POINTER ELEMENT                
         BE    EXIT2                                                            
         CLI   APEEL,APEELQ                                                     
         BE    GSKSI30                                                          
         CLI   APEEL,SPDELQ                                                     
         BE    GSKSI70                                                          
GSKSI20  IC    R0,APELN                                                         
         AR    RF,R0                                                            
         B     GSKSI10                                                          
GSKSI30  IC    R0,APENUM           R0=NUMBER OF SUB-ELEMENTS                    
         LA    RE,APENTRY                                                       
         SR    RF,RF                                                            
         USING APENTRY,RE                                                       
GSKSI40  IC    RF,APENLEN                                                       
         SH    RF,=Y(APENACT-APENLEN+1)                                         
         CLC   =C'SK',APENACT      TEST SK POINTER                              
         BNE   GSKSI50                                                          
         EX    RF,*+4                                                           
         MVC   XFRSKACC(0),APENACT                                              
         B     EXIT2                                                            
GSKSI50  CLC   =C'SI',APENACT      TEST SI POINTER                              
         BNE   GSKSI60                                                          
         CLC   =C'1R',TRNKULC      ONLY IF 1R CONTRA                            
         BNE   GSKSI60                                                          
         EX    RF,*+4                                                           
         MVC   XFRSIACC(0),APENACT                                              
         B     EXIT2                                                            
GSKSI60  IC    RF,APENLEN                                                       
         AR    RE,RF                                                            
         BCT   R0,GSKSI40                                                       
         B     GSKSI20                                                          
*                                                                               
         USING SPDELD,RF                                                        
GSKSI70  SR    RE,RE                                                            
         IC    RE,SPDLN                                                         
         SH    RE,=Y(SPDACCS-SPDELD+1)                                          
         CLC   =C'SK',SPDACCS                                                   
         BNE   GSKSI80                                                          
         EX    RE,*+4                                                           
         MVC   XFRSKACC(0),SPDACCS                                              
         B     EXIT2                                                            
GSKSI80  CLC   =C'SI',SPDACCS      TEST SI MEMO                                 
         BNE   GSKSI20                                                          
         CLC   =C'1R',TRNKULC      ONLY USE IF 1R CONTRA                        
         BNE   GSKSI20                                                          
         EX    RE,*+4                                                           
         MVC   XFRSIACC(0),SPDACCS                                              
         B     EXIT2                                                            
         DROP  R2,RF                                                            
         EJECT                                                                  
**********************************************************************          
*              ADD EXTRA ELEMENTS FOR WRITE-OFFS/TRANSFERS           *          
**********************************************************************          
         SPACE 1                                                                
         USING PTAELD,R3                                                        
         USING TRNELD,R2                                                        
XTRAELS  NTR1                                                                   
*                                  GET TRANSFER PERCENTAGE                      
         ZAP   BODUB1(2*L'BODUB1),PTANET                                        
         ZAP   BODUB3,TRNAMNT                                                   
         SRP   BODUB1(2*L'BODUB1),4,5                                           
         DP    BODUB1(2*L'BODUB1),BODUB3                                        
         ZAP   UPDXPCNT,BODUB1                                                  
*                                                                               
         USING TRXELD,RF                                                        
         LA    RF,BOELEM                                                        
         XC    TRXEL(TRXLN1Q),TRXEL                                             
         MVI   TRXEL,TRXELQ        ADD EXTRA STATUS TO JOB POSTING              
         MVI   TRXLN,TRXLN1Q                                                    
         OI    TRXSTA1,TRXSXALC    EXCLUDE THIS FROM ALLOCATION                 
         LA    R1,TRNEL            FIND TRXEL ON ORIGINAL                       
         XR    R0,R0                                                            
XTRA01   CLI   0(R1),0                                                          
         BE    XTRA03                                                           
         CLI   0(R1),TRXELQ                                                     
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     XTRA01                                                           
         TM    TRXSTA2-TRXELD(R1),TRXSNTMS                                      
         BZ    XTRA03                                                           
         OI    TRXSTA2,TRXSNTMS                                                 
XTRA03   BAS   RE,ADDXTRA                                                       
*                                                                               
         USING SCIELD,RF                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCDSC                                                 
         ZAP   SCIAMNT,PTACDSC                                                  
         MP    SCIAMNT,=P'-1'                                                   
         BZ    *+8                                                              
         BAS   RE,ADDXTRA                                                       
*                                                                               
*&&US                                                                           
         LA    R1,TRNEL            DON'T ADD HOURS SCIEL                        
         USING PRTELD,R1             IF PRTEL ON RECORD                         
         XR    R0,R0                                                            
XTRA05   CLI   PRTEL,0                                                          
         BE    XTRA06                                                           
         CLI   PRTEL,PRTELQ                                                     
         BE    XTRA08                                                           
         IC    R0,PRTLN                                                         
         AR    R1,R0                                                            
         B     XTRA05                                                           
         DROP  R1                                                               
*&&                                                                             
XTRA06   MVI   SCITYPE,SCITSJHR                                                 
         MVC   BCHALF,PTAHOURS                                                  
         LH    RE,BCHALF                                                        
         CVD   RE,BODUB1                                                        
         ZAP   SCIAMNT,BODUB1                                                   
         MP    SCIAMNT,=P'-1'                                                   
         BZ    *+8                                                              
         BAS   RE,ADDXTRA                                                       
*                                                                               
XTRA08   LA    RF,TRNEL                                                         
         SR    R0,R0                                                            
         USING SCIELD,RF                                                        
NEWEL    USING SCIELD,BOELEM                                                    
XTRA10   CLI   SCIEL,0                                                          
         BE    XTRAX                                                            
         CLI   SCIEL,SCIELQ                                                     
         BNE   XTRA20                                                           
         CLI   SCITYPE,SCITCOMM                                                 
         BNE   XTRA90                                                           
         SR    RE,RE                                                            
         IC    RE,SCILN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   NEWEL.SCIEL(0),SCIEL                                             
         ZAP   BODUB1(2*L'BODUB1),NEWEL.SCIAMNT                                 
         MP    BODUB1(2*L'BODUB1),UPDXPCNT                                      
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         ZAP   NEWEL.SCIAMNT,BODUB2                                             
         MP    NEWEL.SCIAMNT,=P'-1'                                             
         BAS   RE,ADDXTRA                                                       
         B     XTRA90                                                           
*                                                                               
         USING OTHELD,RF                                                        
NEWEL    USING OTHELD,BOELEM                                                    
XTRA20   CLI   OTHEL,OTHELQ                                                     
         BNE   XTRA30                                                           
         SR    RE,RE                                                            
         IC    RE,OTHLN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   NEWEL.OTHEL(0),OTHEL                                             
         BAS   RE,ADDXTRA                                                       
         B     XTRA90                                                           
*                                                                               
         USING FFNELD,RF                                                        
NEWEL    USING FFNELD,BOELEM                                                    
XTRA30   CLI   FFNEL,FFNELQ                                                     
         BNE   XTRA40                                                           
         SR    RE,RE                                                            
         IC    RE,FFNLN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   NEWEL.FFNEL(0),FFNEL                                             
         BAS   RE,ADDXTRA                                                       
         B     XTRA90                                                           
*                                                                               
         USING APEELD,RF                                                        
NEWEL    USING APEELD,BOELEM                                                    
XTRA40   CLI   APEEL,APEELQ                                                     
         BNE   XTRA50                                                           
         SR    RE,RE                                                            
         IC    RE,APELN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   NEWEL.APEEL(0),APEEL                                             
         BAS   RE,ADDXTRA                                                       
         B     XTRA90                                                           
*                                                                               
         USING RATELD,RF                                                        
NEWEL    USING RATELD,BOELEM                                                    
XTRA50   CLI   RATEL,RATETAXQ                                                   
         BNE   XTRA60                                                           
         SR    RE,RE                                                            
         IC    RE,RATLN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   NEWEL.RATEL(0),RATEL                                             
         BAS   RE,ADDXTRA                                                       
         B     XTRA90                                                           
*                                                                               
*&&UK                                                                           
XTRA60   B     XTRA90                                                           
*&&                                                                             
*                                                                               
*&&US                                                                           
         USING SPDELD,RF                                                        
NEWEL    USING SPDELD,BOELEM                                                    
XTRA60   CLI   SPDEL,SPDELQ                                                     
         BNE   XTRA70                                                           
         SR    RE,RE                                                            
         IC    RE,SPDLN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   NEWEL.SPDEL(0),SPDEL                                             
         BAS   RE,ADDXTRA                                                       
         B     XTRA90                                                           
*                                                                               
         USING PRTELD,RF                                                        
NEWEL    USING PRTELD,BOELEM                                                    
XTRA70   CLI   PRTEL,PRTELQ                                                     
         BNE   XTRA90                                                           
         IC    RE,PRTLN                                                         
         EX    RE,*+4                                                           
         MVC   NEWEL.PRTELD(0),PRTELD                                           
         ZAP   BODUB1(2*L'BODUB1),NEWEL.PRTHOUR                                 
         MP    BODUB1(2*L'BODUB1),UPDXPCNT                                      
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         MP    BODUB2,=P'-1'                                                    
         ZAP   NEWEL.PRTHOUR,BODUB2                                             
         BAS   RE,ADDXTRA                                                       
         B     XTRA90                                                           
*&&                                                                             
*                                                                               
XTRA90   SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     XTRA10                                                           
*                                                                               
XTRAX    B     EXIT2                                                            
         DROP  R2,R3,NEWEL                                                      
         EJECT                                                                  
**********************************************************************          
*              SET CONTRA-ACCOUNT NAME IN POSTCACN (KEY IN POSTCAC)  *          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,R2                                                       
         USING POSTVALS,R4                                                      
SETCACN  NTR1                                                                   
         LA    R4,UPDPOSTB                                                      
         MVC   POSTCACN,BCSPACES   PRE-SET TO SPACES                            
*                                                                               
         LA    RF,CACTAB                                                        
         LA    R0,CACMAXQ                                                       
         USING CACTABD,RF                                                       
SETC10   CLI   CACACTK,0           TEST E-O-T                                   
         BE    SETC20                                                           
         CLC   CACACTK,POSTCAC+(ACTKUNT-ACTKEY)                                 
         BE    *+16                FOUND IT - WE'VE SAVED AN IO!                
         LA    RF,CACTABLQ(RF)                                                  
         BCT   R0,SETC10                                                        
         B     SETC20                                                           
         MVC   POSTCACN,CACACTN    SET CONTRA NAME                              
         B     SETC90                                                           
*                                                                               
SETC20   LA    R2,IOKEY            READ CONTRA-ACCOUNT RECORD                   
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCULA,POSTCAC                                                 
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BE    SETC30                                                           
*&&US                                                                           
         CLC   =C'13',POSTCAC+(ACTKUNT-ACTKEY)                                  
         BE    EXIT2                                                            
*&&                                                                             
         DC    H'0'                                                             
SETC30   L     R2,AIO2                                                          
         LA    R3,ACTRFST                                                       
         SR    RE,RE                                                            
         LTR   RF,R0               TEST ANY SPARE SLOTS IN TABLE                
         BZ    *+10                RE-START AT THE TOP                          
         LA    RF,CACMAXQ                                                       
         SR    RF,R0                                                            
         MH    RF,=Y(CACTABLQ)                                                  
         LA    RF,CACTAB(RF)       RF=SPARE SLOT IN CACTAB                      
         MVI   CACTABLQ(RF),0      FREE-UP THE NEXT SLOT                        
*                                                                               
         USING NAMELD,R3                                                        
SETC40   CLI   NAMEL,0                                                          
         BE    SETC90                                                           
         CLI   NAMEL,NAMELQ                                                     
         BE    SETC50                                                           
         CLI   NAMEL,SPAELQ                                                     
         BE    SETC60                                                           
         CLI   NAMEL,RSTELQ                                                     
         BE    SETC70                                                           
SETC45   IC    RE,NAMLN                                                         
         AR    R3,RE                                                            
         B     SETC40                                                           
*                                                                               
SETC50   IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMEREC+1-NAMEL)                                           
         EX    RE,*+4                                                           
         MVC   POSTCACN(0),NAMEREC                                              
         MVC   CACACTK,ACTKUNT-ACTKEY(R2)                                       
         MVC   CACACTN,POSTCACN                                                 
         B     SETC45                                                           
*                                                                               
         USING SPAELD,R3                                                        
SETC60   MVC   CACACTP(2),=C'12'                                                
         MVC   CACACTP+2(L'ACTKACT),SPAAULA                                     
         B     SETC45                                                           
*                                                                               
         USING RSTELD,R3                                                        
SETC70   OC    CACACTP,CACACTP     TEST ALREADY SET FROM SPAEL                  
         BNZ   SETC45              YES - SPAEL OVERRIDES                        
         MVC   CACACTP,BCSPACES                                                 
         MVC   CACACTP(2),=C'12'                                                
         MVC   CACACTP+2(L'RSTCOSTG),RSTCOSTG                                   
         B     SETC45                                                           
*                                                                               
SETC90   XIT1  REGS=(RF)                                                        
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
*              PRINT POSTING SUMMARY REPORT                          *          
**********************************************************************          
         SPACE 1                                                                
PRTSUMM  NTR1                                                                   
         L     R4,AREP                                                          
         USING REPD,R4                                                          
         OI    REPHEADI,REPHFRCE   FORCE NEW PAGE FOR SUMMARY                   
         LA    R2,REPP1                                                         
         USING JRNLINED,R2                                                      
         MVCDD JRNSUM,AC#PSTGU,F                                                
         GOTO1 VDICTAT,BODMCB,C'SL  ',JRNSUM,0                                  
         LA    R2,(2*L'REPP1)(R2)                                               
         GOTO1 VDICTAT,BODMCB,C'LL  ',SUMDICT,BOELEM                            
         MVC   JRNDESC,BOELEM+(AC@INCCR-BOELEM)                                 
         LA    R3,UPDMNTHS                                                      
         USING UPDMONTB,R3                                                      
*                                                                               
PRTS10   OC    UPDMNTH,UPDMNTH                                                  
         BZ    PRTS20                                                           
         MVC   BOWORK1(2),UPDMOA                                                
         MVI   BOWORK1+2,X'01'                                                  
         GOTO1 VDATCON,BODMCB,(1,BOWORK1),(9,JRNLEDG)                           
         CURED UPDMAMT,(L'JRNAMNT,JRNAMNT),2,MINUS=YES,DMCB=BODMCB              
         LA    R3,L'UPDMONTH(R3)                                                
         LA    R2,L'REPP1(R2)                                                   
         B     PRTS10                                                           
*                                                                               
PRTS20   GOTO1 VREPORT,REPD                                                     
         LA    R2,REPP1                                                         
         MVC   JRNDESC,BOELEM+(AC@BLG-BOELEM)                                   
         MVC   JRNLEDG,=C'SR'                                                   
         ZAP   BODUB1,UPDTOTAC                                                  
         AP    BODUB1,UPDTOTAN                                                  
         AP    BODUB1,UPDTOTAV                                                  
         AP    BODUB1,UPDTOTAS                                                  
         CURED BODUB1,(L'JRNAMNT,JRNAMNT),2,MINUS=YES,DMCB=BODMCB               
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDESC,BOELEM+(AC@INCM-BOELEM)                                  
         MVC   JRNLEDG,=C'SI'                                                   
         CURED UPDTOTAC,(L'JRNAMNT,JRNAMNT),2,MINUS=YES,DMCB=BODMCB             
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDESC,BOELEM+(AC@XFRS-BOELEM)                                  
         MVC   JRNLEDG,=C'SJ'                                                   
         ZAP   BODUB1,BCPZERO                                                   
         OC    LSUTRFRF,LSUTRFRF                                                
         BZ    *+10                                                             
         ZAP   BODUB1,UPDCBTP                                                   
         CURED BODUB1,(L'JRNAMNT,JRNAMNT),2,MINUS=YES,DMCB=BODMCB               
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDESC,BOELEM+(AC@WRTFS-BOELEM)                                 
         MVC   JRNLEDG,=C'SE'                                                   
         ZAP   BODUB1,BCPZERO                                                   
         OC    LSUWOFRF,LSUWOFRF                                                
         BZ    *+10                                                             
         ZAP   BODUB1,UPDCBWP                                                   
         CURED BODUB1,(L'JRNAMNT,JRNAMNT),2,MINUS=YES,DMCB=BODMCB               
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDESC,BOELEM+(AC@SRCRG-BOELEM)                                 
         MVC   JRNLEDG(5),=C'SQ/SI'                                             
         CURED UPDTOTAS,(L'JRNAMNT,JRNAMNT),2,MINUS=YES,DMCB=BODMCB             
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDESC,BOELEM+(AC@DISS-BOELEM)                                  
         CURED UPDTOTAD,(L'JRNAMNT,JRNAMNT),2,MINUS=YES,DMCB=BODMCB             
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    PRTSX                                                            
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDESC,BOELEM+(AC@VAT1-BOELEM)                                  
         MVC   JRNLEDG,=C'SG'                                                   
         CURED UPDTOTAV,(L'JRNAMNT,JRNAMNT),2,MINUS=YES,DMCB=BODMCB             
         GOTO1 VREPORT,REPD                                                     
PRTSX    B     EXIT2                                                            
         EJECT                                                                  
**********************************************************************          
*              CLOSE REPORT                                          *          
**********************************************************************          
         SPACE 1                                                                
         USING REPD,R4                                                          
PSTBPRNT L     R4,AREP                                                          
         MVI   REPACTN,REPACLO                                                  
         GOTO1 VREPORT,REPD                                                     
*                                                                               
         MVI   FVOMTYP,GTMINF                                                   
         LA    RE,FVXTRA                                                        
         CLC   FVMSGNO,=Y(AI$LOKXF)                                             
         BE    PBPRNT01                                                         
         MVC   FVMSGNO,=AL2(AI$RPSPL)                                           
         MVI   FVPARMS,9                                                        
         LA    RE,FVPARMS+1                                                     
PBPRNT01 MVC   0(L'REPSUBID,RE),REPSUBID                                        
         MVC   L'REPSUBID(1,RE),BCCOMMA                                         
         LA    RF,L'REPSUBID+1(RE)                                              
         EDIT  (2,REPREPNO),(5,(RF)),ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1          
         LA    RE,UPDCLIH          SET CURSOR TO CLIENT                         
         ST    RE,FVADDR                                                        
*                                                                               
         CLI   P#DQU,C'Y'          TEST ALWAYS OUTPUT DQU                       
         BE    PBPRNT02                                                         
         CLI   CSACT,ACTDRA                                                     
         BE    *+16                                                             
         CLI   P#DQU,C'L'          TEST ONLY FOR LIVE                           
         BE    PBPRNT02                                                         
         B     PBPRNT04                                                         
         CLI   P#DQU,C'D'          TEST ONLY FOR DRAFT                          
         BNE   PBPRNT04                                                         
PBPRNT02 MVC   BASSRV,DQU                                                       
         OI    BASSRVH+FHOID,FHOITR                                             
*                                                                               
PBPRNT04 CLI   UPDBLNG,YESQ                                                     
         BNE   EXIT2                                                            
         L     RE,AGOPBLK                                                       
         LA    RF,L'GOBLOCK                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     RE,AGOPBLK          FOR BILLING EXTENSION                        
         MVC   GOABEXT-GOBLOCKD(L'GOABEXT,RE),AJOBBLK                           
         L     R2,AIO1                                                          
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   ACTKACT,BCJOBCOD                                                 
         L     RF,ATIA                                                          
         XC    0(256,RF),0(RF)                                                  
         GOTO1 AGETOPT,BODMCB,AIO1                                              
         DS    0H                                                               
         GOTO1 VCOLY,BODMCB,(X'0B',0),0,0                                       
         L     RF,0(R1)                                                         
         BASR  RE,RF                                                            
         B     EXIT2                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD ELEMENT IN BOELEM INTO THE EXTRA POSTING ELEMENT AREA           *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING POSTVALS,R4                                                      
ADDXTRA  NTR1                      SAVE RETURN ADDR                             
         L     RE,POSTXTRA         START IF EXTRA AREA                          
         SR    RF,RF                                                            
ADDX10   CLI   0(RE),0             END MARKER                                   
         BE    ADDX20                                                           
*                                  IF VAT SCIEL FIND MATCH                      
         USING SCIELD,RE                                                        
         CLI   BOELEM+(SCIEL-SCIELD),SCIELQ                                     
         BNE   ADDX15                                                           
         CLI   SCIEL,SCIELQ                                                     
         BNE   ADDX15                                                           
         TM    BOELEM+(SCITYPE-SCIELD),X'40'                                    
         BO    ADDX15                                                           
         CLC   SCITYPE,BOELEM+(SCITYPE-SCIELD)                                  
         BNE   ADDX15                                                           
         AP    SCIAMNT,BOELEM+(SCIAMNT-SCIELD)(L'SCIAMNT)                       
         B     ADDXX                                                            
         DROP  RE                                                               
*                                                                               
ADDX15   IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     ADDX10                                                           
ADDX20   IC    RF,BOELEM+1         RF=L'NEW ELEMENT                             
         LA    R0,UPDXTRA+UPDXTRLN R0=END OF AVAILABLE SPACES                   
         SR    R0,RF                                                            
         CR    RE,R0               CHECK THE NEW ONE WILL FIT                   
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),BOELEM                                                   
         LA    RE,1(RE,RF)                                                      
         MVI   0(RE),0                                                          
ADDXX    B     EXIT2                                                            
         SPACE 2                                                                
**********************************************************************          
*              ADD MEDIA TRANSFER ELEMENT                            *          
*              NB: HARD LEDGER LENGTHS ASSUMES US ONLY               *          
**********************************************************************          
         SPACE 1                                                                
         USING PTAELD,R3                                                        
ADDMDTEL NTR1                                                                   
         LA    RF,BOELEM                                                        
         USING MDTEL,RF                                                         
         XC    MDTEL(MDTLNQ),MDTEL                                              
         MVI   MDTEL,MDTELQ                                                     
         MVI   MDTLN,MDTLNQ                                                     
         MVI   MDTSYS,MDTSPROD                                                  
         MVC   MDTMED,6(R1)                                                     
         MVC   MDTCLI(L'BCJOBCOD),0(R1)                                         
         MVC   MDTMOS,PTAMOA                                                    
         ZAP   BCDUB,POSTAMNT                                                   
         CVB   R0,BCDUB                                                         
         STCM  R0,15,MDTCOM                                                     
         STCM  R0,15,MDTINTL                                                    
         BAS   RE,ADDXTRA                                                       
         B     EXIT2                                                            
         SPACE 2                                                                
**********************************************************************          
*              ADD R. L. FILTER ELEMENT FOR WORKCODE                 *          
**********************************************************************          
         SPACE 1                                                                
ADDRFLEL NTR1                                                                   
         LA    RF,BOELEM                                                        
         USING RFLEL,RF                                                         
         XC    RFLEL(RFLLNQ+L'TRNKWORK),RFLEL                                   
         MVI   RFLEL,RFLELQ                                                     
         MVI   RFLLN,(RFLLNQ+L'TRNKWORK)                                        
         MVI   RFLTYPE,RFLWC                                                    
         MVC   RFLDATA(L'TRNKWORK),0(R1)                                        
         BAS   RE,ADDXTRA                                                       
         B     EXIT2                                                            
         DROP  R3,RF                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD WRITE OFF NARRATIVE                                *         
*                                                                     *         
* NTRY: P1=A(TRNELD)                                                  *         
*       P2=A(PTAELD)                                                  *         
* EXIT: POSTNARR = NARRATIVE                                          *         
***********************************************************************         
         SPACE 1                                                                
BLDWONAR NTR1  ,                                                                
         LM    R3,R4,0(R1)                                                      
         USING TRNELD,R3                                                        
         USING PTAELD,R4                                                        
*                                                                               
         LA    R2,UPDPOSTB+(POSTNARR-POSTVALS)                                  
         MVI   0(R2),C' '                                                       
         MVC   1(L'POSTNARR-1,R2),0(R2)                                         
*&&US                                                                           
         MVC   0(3,R2),=C'W/O'                                                  
         CLI   PTATYPE,PTATWOF                                                  
         BE    *+10                                                             
         MVC   0(3,R2),=C'W/R'                                                  
         LA    R2,4(R2)                                                         
         MVC   BOHALF1,PTASEQN                                                  
         LH    RF,BOHALF1                                                       
         LCR   RF,RF                                                            
         CVD   RF,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  0(4,R2),BODUB1                                                   
         LA    R2,5(R2)                                                         
         GOTO1 VDATCON,BOPARM,(2,PTADATE),(17,(R2))                             
         LA    R2,9(R2)                                                         
         OC    PTAHOURS,PTAHOURS                                                
         BZ    BWONAR02                                                         
         EDIT  (B2,PTAHOURS),(8,(R2)),2,ALIGN=LEFT                              
         AR    R2,R0                                                            
         MVC   1(3,R2),=C'HRS'                                                  
         LA    R2,5(R2)                                                         
*&&                                                                             
BWONAR02 SR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         SH    RF,=Y(TRNNARR-TRNELD+1)                                          
         BM    *+14                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R2),TRNNARR                                                  
*                                                                               
         B     EXIT2                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST VALIDATY OF WRITE-OFF TABLE ENTRY                   *         
*                                                                     *         
* NTRY: P1 = A(WOTAB ENTRY)                                           *         
*      IO8 = PTA RECORD                                               *         
* EXIT: P1 = A(U/L ACCOUNT)                                           *         
*       P2 = A(CONTRA U/L/ACCOUNT)                                    *         
***********************************************************************         
         SPACE 1                                                                
TESTWO   NTR1  ,                                                                
         LR    R2,R1               R2=A(PARAMETER LIST)                         
         L     R3,0(R2)                                                         
         USING WOTABD,R3           R3=A(WRITE OFF TABLE ENTRY)                  
         GOTO1 TESTSC,WOSYS        TEST SYSTEM/COUNTRY FILTERS                  
         BNE   TESTWONO                                                         
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('SPAELQ',AIO8),            *        
               (L'WOACSPA,WOACSPA)                                              
         CLI   12(R1),0            TEST ACCOUNT ON RECORD                       
         BNE   TESTWONO                                                         
         L     RE,12(R1)                                                        
         LA    R0,SPAAULA-SPAELD(RE)                                            
         GOTO1 (RF),(R1),,,(L'WOCASPA,WOCASPA)                                  
         CLI   12(R1),0            TEST CONTRA-ACCOUNT ON RECORD)               
         BNE   TESTWONO                                                         
         L     RE,12(R1)                                                        
         LA    RE,SPAAULA-SPAELD(RE)                                            
         ST    RE,4(R2)            P2=A(CONTRA U/L ACCOUNT)                     
         ST    R0,0(R2)            P1=A(U/L/ ACCOUNT)                           
*                                                                               
TESTWOY  CR    RB,RB               SET CC=EQUAL                                 
         B     EXIT2                                                            
TESTWONO LTR   RB,RB               SET CC=NOT EQUAL                             
         B     EXIT2                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST SYSTEM/COUNTRY FILTERS                              *         
*                                                                     *         
* NTRY: R1=A(SYSTEM/COUNTRY)                                          *         
***********************************************************************         
         SPACE 1                                                                
TESTSC   OC    0(2,R1),0(R1)       TEST ALL SYSTEMS/COUTRIES                    
         BZR   RE                  YES - CC=EQUAL                               
*                                                                               
         CLI   0(R1),0             TEST ALL SYSTEMS                             
         BE    TSC02                                                            
*&&UK*&& CLI   0(R1),SYSUK         MATCH ON UK SYSTEM                           
*&&US*&& CLI   0(R1),SYSUS         MATCH ON US SYSTEM                           
         BNER  RE                  RETURN WITH CC=NOT EQUAL                     
*                                                                               
TSC02    CLI   1(R1),0             TEST ALL COUNTRIES                           
         BER   RE                                                               
         CLC   CUCTRY,1(R1)        MATCH ON CONNECTED COUNTRY                   
         BER   RE                                                               
         TM    1(R1),CTRYNOT       TEST ALL BUT A COUNTRY                       
         BZ    TESTSCN                                                          
         MVC   BCBYTE1,1(R1)                                                    
         XI    BCBYTE1,CTRYNOT                                                  
         CLC   BCBYTE1,CUCTRY                                                   
         BNE   TESTSCY                                                          
*                                                                               
TESTSCN  LTR   RE,RE               CC=NOT EQUAL                                 
         BR    RE                                                               
TESTSCY  CR    RE,RE               CC=EQUAL                                     
         BR    RE                                                               
         EJECT                                                                  
FF       EQU   X'FF'                                                            
TWACOLS  EQU   80                                                               
TWAROWS  EQU   24                                                               
BILBTY   EQU   7                                                                
BILBGP   EQU   C'G'                                                             
WOFBTY   EQU   14                                                               
WOFBGP   EQU   C'G'                                                             
TRFBTY   EQU   34                                                               
TRFBGP   EQU   C'G'                                                             
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
ALL      EQU   0                                                                
DUB      EQU   BODUB1                                                           
WORK     EQU   BOWORK1                                                          
CBILFEEA EQU   X'08'                                                            
ULBIL    DC    C'11'                                                            
ULREV    DC    C'12'                                                            
DQU      DC    CL(L'BASSRV)'=DQU'                                               
*                                                                               
ACCMST   DC    C'ACCMST  '                                                      
         DC    X'00'                                                            
*                                                                               
SUMDICT  DCDDL AC#INCCR,20                                                      
         DCDDL AC#BLG,20                                                        
         DCDDL AC#INCM,20                                                       
         DCDDL AC#XFRS,20                                                       
         DCDDL AC#WRTFS,20                                                      
         DCDDL AC#SRCRG,20                                                      
         DCDDL AC#DISS,20                                                       
         DCDDL AC#VAT1,20                                                       
         DC    X'00'                                                            
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WRITE OFF ANALYSIS TABLE                                            *         
***********************************************************************         
         SPACE 1                                                                
WOTAB    DS    0X                                                               
*                                                                               
         DC    AL1(SPATW2DA,SPATW28A),C'D+'                                     
         DC    AL1(ALL,ALL,WOIAPEAC,0,0,0,0,0)                                  
                                                                                
         DC    AL1(SPATW28A,SPATW2DA),C'C+'                                     
         DC    AL1(ALL,ALL,WOIAPEAC,0,0,0,0,0)                                  
*                                                                               
         DC    AL1(SPATW1PA,SPATW1CA),C'D+'                                     
         DC    AL1(ALL,ALL,WOIAPEAC,0,0,0,0,0)                                  
                                                                                
         DC    AL1(SPATW1CA,SPATW13A),C'C+'                                     
         DC    AL1(ALL,ALL,WOIAPEAC+WOIAPECA,0,0,0,0,0)                         
*                                                                               
         DC    AL1(SPATW1CA,SPATW12K),C'D+'                                     
         DC    AL1(SYSUS,ALL,WOIAPEAC+WOIAPESK,0,0,0,0,0)                       
                                                                                
         DC    AL1(SPATW12K,SPATW1CA),C'C+'                                     
         DC    AL1(SYSUS,ALL,WOIAPEAC+WOIAPESK,0,0,0,0,0)                       
*                                                                               
         DC    AL1(SPATW1CA,SPATW12A),C'D-'                                     
         DC    AL1(ALL,ALL,WOIAPEAC,0,0,0,0,0)                                  
                                                                                
         DC    AL1(SPATW12A,SPATW1CA),C'C-'                                     
         DC    AL1(ALL,ALL,WOIAPEAC,0,0,0,0,0)                                  
*                                                                               
         DC    AL1(SPATW29A,SPATW29C),C'C+'                                     
         DC    AL1(SYSUK,ALL,WOIAPEAC+WOIANACA+WOICAN2P,0,0,0,0,0)              
                                                                                
         DC    AL1(SPATW29A,SPATW29C),C'C+'                                     
         DC    AL1(SYSUS,ALL,WOIAPEAC+WOIANACA+WOICAN2P,0,0,0,0,0)              
*                                                                               
         DC    AL1(SPATW2PA,SPATW2PC),C'D+'                                     
         DC    AL1(SYSUK,ALL,WOIAPEAC+WOIANACA+WOICANCL,0,0,0,0,0)              
                                                                                
         DC    AL1(SPATW2PA,SPATW2PC),C'C-'                                     
         DC    AL1(SYSUS,ALL,WOIAPEAC+WOIANACA+WOICANCL,0,0,0,0,0)              
*                                                                               
WOTABX   DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
*              INITIALISE REPORT AREA FOR JOURNAL                    *          
**********************************************************************          
         SPACE 1                                                                
POSTINIT NMOD1 0,**PINI**                                                       
         LR    RC,R1                                                            
         MVI   UPDINIT,YESQ                                                     
         GOTO1 AINIADT             INITIALISE ADDTRN BLOCK                      
         L     RF,AADTBLK                                                       
         XC    TRNOFA-ADDTRND(L'TRNOFA,RF),TRNOFA-ADDTRND(RF)                   
         L     R4,AREP             INITIALISE REPORT                            
         USING REPD,R4                                                          
         MVI   REPSUBPG,0          SET FOR DRAFT JOURNAL HEADS                  
         CLI   CSACT,ACTDRA                                                     
         BE    *+8                                                              
         MVI   REPSUBPG,1          SET FOR LIVE JOURNAL HEADS                   
         MVC   REPRLH,=AL2(48)                                                  
         MVC   REPRDH,=AL2(12)                                                  
         LH    RE,=Y(LC@LVLST-TWAD)                                             
         AR    RE,RA                                                            
         MVC   REPDESC,0(RE)                                                    
         MVC   REPSUBID,=C'BWL'                                                 
         CLI   CSACT,ACTDRA                                                     
         BNE   PINI050                                                          
         SH    RE,=Y(LC@LVLST-LC@DFLST)                                         
         MVC   REPSUBID,=C'DWL'                                                 
         MVC   REPDESC,0(RE)                                                    
*                                                                               
PINI050  MVI   REPACTN,REPAINI                                                  
         GOTO1 VREPORT,REPD                                                     
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         BASR  RE,RF                                                            
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   REPACTN,REPAPUT                                                  
*                                                                               
         L     R0,AIO1             CLEAR SPACE FOR HEADER SCREEN FORMAT         
         LH    R1,=Y(TWACOLS*TWAROWS)                                           
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE                                                            
*                                                                               
         LA    R1,BASMSGH          FORMAT HEADER SCREEN INTO IO/IO2             
         SR    RE,RE                                                            
         LA    RF,4095(R1)                                                      
PINI100  ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    PINI200                                                          
         SR    R2,R2                                                            
         ICM   R2,3,FVABSA-FVIHDR(R1)                                           
         A     R2,AIO1                                                          
         SH    RE,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RE,=Y(L'FVIHDR)                                                  
         BCTR  RE,0                                                             
         TM    FVATRB-FVIHDR(R1),FVAZERO                                        
         BO    *+14                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),L'FVIHDR(R1)                                             
         ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BXLE  R1,RE,PINI100                                                    
*                                                                               
PINI200  L     RF,VREPORT          INITIALISE HEADER PRINT LOOP                 
         LA    R1,REPD                                                          
         L     R2,AIO1                                                          
         MVC   0(L'BASMSG,R2),BCSPACES                                          
         LA    R0,TWAROWS                                                       
*                                                                               
         MVI   REPP1,C'*'          PRINT HEADER SCREEN ON FIRST PAGE            
         MVC   REPP1+1(TWACOLS+1),REPP1                                         
         BASR  RE,RF                                                            
PINI300  MVI   REPP1,C'*'                                                       
         MVC   REPP1+1(TWACOLS),0(R2)                                           
         MVI   REPP1+1+TWACOLS,C'*'                                             
         BASR  RE,RF                                                            
         LA    R2,TWACOLS(R2)                                                   
         BCT   R0,PINI300                                                       
         MVI   REPP1,C'*'                                                       
         MVC   REPP1+1(TWACOLS+1),REPP1                                         
         BASR  RE,RF                                                            
         LA    RE,JRNSPEC                                                       
         ST    RE,REPAPHS                                                       
         OI    REPHEADI,REPHFRCE                                                
         MVC   REPPAGE,=Y(1)                                                    
*                                                                               
         LA    R4,UPDPOSTB                                                      
         USING POSTVALS,R4         R4=A(POSTING VALUES BLOCK)                   
         LA    RE,UPDXTRA                                                       
         MVI   0(RE),0                                                          
         ST    RE,POSTXTRA         A(EXTRA POSTING ELEMENTS)                    
         LH    RE,=Y(UPDPTRS-UPDWORKD)                                          
         AR    RE,RC                                                            
         MVI   0(RE),0                                                          
         ST    RE,POSTPTRS         A(ANALYSIS POINTERS)                         
         MVI   POSTMODE,POSTLVQ    SET BLDTRN MODE LIVE                         
         CLI   CSACT,ACTUPD                                                     
         BE    *+8                                                              
         MVI   POSTMODE,POSTDFQ    SET BLDTRN MODE DRAFT                        
         DROP  R4                                                               
         B     EXIT3                                                            
         SPACE 1                                                                
EXIT3    XIT1  ,                                                                
         SPACE 1                                                                
JRNSPEC  DS    0X                  ** SPECS FOR JOURNAL **                      
         SPROG 0,1                                                              
         SPEC  H1,1,RUN                                                         
         SPEC  H1,73,PAGE                                                       
         SPROG 0                                                                
         SPEC  H1,20,AC#DPOS,40,C                                               
         SPEC  H2,20,AC#DPOS,40,CU                                              
         SPROG 1                                                                
         SPEC  H1,20,AC#APOS,40,C                                               
         SPEC  H2,20,AC#APOS,40,CU                                              
JRNSPECX DC    AL1(EOT)                                                         
*                                                                               
         EJECT                                                                  
**********************************************************************          
*              INCOME POSTINGS COME THROUGH HERE FOR ACCRUING        *          
**********************************************************************          
         SPACE 1                                                                
POSTACCR NMOD1 0,**ACCR**                                                       
         LR    RC,R1                                                            
         USING POSTVALS,R4                                                      
         OC    UPDMNTHS(L'UPDMNTH),UPDMNTHS                                     
         BNZ   PSTA020                                                          
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT4                                                            
         B     PSTAX               NO MONTHLY SPREADING AROUND                  
*                                                                               
PSTA020  MVC   UPDPSTAC,POSTACT    SAVE POSTING ACCOUNT                         
         ZAP   UPDPSTAS,POSTAMNT   SAVE TRANSACTION AMOUNT                      
*                                                                               
         LA    R0,UPDXTRAS         COPY EXTRA ELEMENT AREA INTO SAVE            
         LA    RE,UPDXTRA                                                       
         LH    R1,=Y(L'UPDXTRAS)                                                
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING UPDMONTB,R3                                                      
         LA    R3,UPDMNTHS                                                      
PSTA050  OC    UPDMNTH,UPDMNTH     TEST END OF MONTH LIST                       
         BZ    PSTA500             FINISHED                                     
         GOTO1 APTRNA,(RC)                                                      
         AP    UPDMAMT,POSTAMNT    ADD TO THIS MONTH AMOUNT                     
         LA    RF,UPDXTRAS                                                      
         SR    RE,RE                                                            
         LA    R2,UPDXTRA                                                       
PSTA100  MVI   0(R2),0                                                          
         CLI   0(RF),0                                                          
         BE    PSTA300             NO MORE ELEMENTS                             
         SR    RE,RE                                                            
         IC    RE,1(RF)                                                         
PSTA120  EX    RE,*+4                                                           
         MVC   0(0,R2),0(RF)       MOVE ELEMENT AND E-O-R MARKER                
         USING SCIELD,R2           APPLY MONTHLY PERCENTAGE                     
         CLI   SCIEL,SCIELQ                                                     
         BNE   PSTA200                                                          
         CLI   SCITYPE,SCITIVAT    TEST IF VAT OR NEWVAT BUCKET                 
         BE    *+12                                                             
         TM    SCITYPE,X'40'                                                    
         BO    PSTA140                                                          
*                                                                               
         CLC   POSTBTMC,UPDBLMCS   TEST FIRST MONTH POSTING                     
         BE    PSTA200             YES - BUCKET IS FULL AMOUNT                  
         ZAP   SCIAMNT,BCPZERO     ELSE SET AMOUNT TO ZERO                      
         CLI   SCILN,SCILN1Q                                                    
         BE    PSTA200                                                          
         ZAP   SCINET,BCPZERO                                                   
         B     PSTA200                                                          
*                                                                               
PSTA140  ZAP   BODUB1(2*L'BODUB1),SCIAMNT                                       
         MP    BODUB1(2*L'BODUB1),UPDMPCT                                       
         SRP   BODUB1(2*L'BODUB1),64-6,5                                        
         ZAP   BODUB1,SCIAMNT      SAVE ORIGINAL (TOTAL)                        
         ZAP   SCIAMNT,BODUB2      SET THIS MONTH CALCULATED                    
         CLC   POSTBTMC,UPDBLMCS   TEST FIRST MONTH POSTING                     
         BNE   PSTA142                                                          
         LR    R0,RF                                                            
         GOTO1 APDIFF,(RC)                                                      
         LR    RF,R0                                                            
         ZAP   SCIAMNT,BODUB2                                                   
PSTA142  CLI   SCILN,SCILN1Q                                                    
         BE    PSTA200                                                          
         ZAP   BODUB1(2*L'BODUB1),SCINET                                        
         MP    BODUB1(2*L'BODUB1),UPDMPCT                                       
         SRP   BODUB1(2*L'BODUB1),64-6,5                                        
         ZAP   BODUB1,SCINET       SAVE ORIGINAL (TOTAL)                        
         ZAP   SCINET,BODUB2       SET THIS MONTH CALCULATED                    
         CLC   POSTBTMC,UPDBLMCS   TEST FIRST MONTH POSTING                     
         BNE   PSTA200                                                          
         LR    R0,RF                                                            
         GOTO1 APDIFF,(RC)                                                      
         LR    RF,R0                                                            
         ZAP   SCINET,BODUB2       SET ADJUSTED MONTH 1 AMOUNT                  
PSTA200  SR    RE,RE               RESET INCREMENT VALUE                        
         IC    RE,1(RF)                                                         
         AR    RF,RE               MOVE 'FROM' POINTER                          
         AR    R2,RE               MOVE 'TO' POINTER                            
         B     PSTA100                                                          
*                                                                               
PSTA300  XC    POSTPTRS,POSTPTRS                                                
         MVI   POSTSTAT,0                                                       
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT4                                                            
         MVC   POSTACT,UPDACC      SET ACCRUAL ACCOUNT                          
         OC    POSTACT,BCSPACES                                                 
         CLC   POSTBTMC,UPDBLMCS   TEST FIRST MONTH POSTING                     
         BE    *+12                                                             
         MVI   POSTSTAT,TRNSDR                                                  
         B     PSTA400                                                          
         ZAP   BODUB1,POSTAMNT     MONTH 1 SQ POSTING IS CREDIT                 
         ZAP   POSTAMNT,UPDPSTAS                                                
         SP    POSTAMNT,BODUB1                                                  
PSTA400  GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT4                                                            
         MVC   POSTACT,UPDPSTAC    RESET SAVED ACCOUNT                          
         LA    R3,L'UPDMNTHS(R3)   NEXT MONTH                                   
         B     PSTA050                                                          
PSTA500  MVC   POSTBTMC,LSUBILMC   RESTORE BILL MOA                             
         MVC   POSTBTRF,LSUBILRF   RESTORE BILL REFERENCE                       
PSTAX    CR    RB,RB                                                            
         B     EXIT4                                                            
*                                                                               
EXIT4    XIT1  ,                                                                
         EJECT                                                                  
**********************************************************************          
*              FIX POSTAMNT                                          *          
**********************************************************************          
         SPACE 1                                                                
POSTTRNA NMOD1 0,**PTRN**                                                       
         LR    RC,R1                                                            
         MVC   POSTBTMC,UPDMNTH    SET THIS MOS                                 
         MVC   POSTBTRF,UPDMREF    SET THIS REFERENCE                           
         ZAP   BODUB1(2*L'BODUB1),UPDPSTAS                                      
         MP    BODUB1(2*L'BODUB1),UPDMPCT                                       
         SRP   BODUB1(2*L'BODUB1),64-6,5                                        
         CLC   POSTBTMC,UPDBLMCS   TEST FIRST MONTH POSTING                     
         BNE   PTRN20                                                           
         ZAP   BODUB1,UPDPSTAS                                                  
         GOTO1 APDIFF,(RC)                                                      
PTRN20   ZAP   POSTAMNT,BODUB2     SET TRANSACTION AMOUNT                       
         XIT1  ,                                                                
*                                                                               
**********************************************************************          
*              POST PENNY DIFFERENCES TO MONTH 1                     *          
**********************************************************************          
         SPACE 1                                                                
POSTDIFF NMOD1 0,**PDIF**                                                       
         LR    RC,R1                                                            
         ZAP   UPDTARGT,BODUB1     TOTAL TO AIM FOR                             
         ZAP   UPDMNTH1,BODUB2     MONTH 1 CALCULATED AMOUNT                    
         ZAP   BODUB3,BCPZERO      ROLLING TOTAL                                
PSTD100  OC    UPDMNTH,UPDMNTH                                                  
         BZ    PSTD200                                                          
         ZAP   BODUB1(2*L'BODUB1),UPDTARGT                                      
         MP    BODUB1(2*L'BODUB1),UPDMPCT                                       
         SRP   BODUB1(2*L'BODUB1),64-6,5                                        
         AP    BODUB3,BODUB2                                                    
         LA    R3,L'UPDMNTHS(R3)                                                
         B     PSTD100                                                          
PSTD200  SP    BODUB3,UPDTARGT     TAKE OFF TARGET                              
         SP    UPDMNTH1,BODUB3     ADJUST MONTH 1 AMOUNT                        
         ZAP   BODUB2,UPDMNTH1     RETURN ADJUSTED AMOUNT                       
         XIT1  ,                                                                
         EJECT                                                                  
**********************************************************************          
*              ALLOCATE A TRANSFER POSTING                           *          
**********************************************************************          
         SPACE 1                                                                
ALLXFR   NMOD1 0,**AXFR**                                                       
         LR    RC,R1                                                            
X        USING TRNKEY,BOWORK2                                                   
         USING POSTVALS,R4         R4=A(POSTING VALUES BLOCK)                   
         MVC   X.TRNKEY,BCSPACES   SET KEY OF NEW POSTING                       
         MVC   X.TRNKCPY,CUABIN                                                 
         MVC   X.TRNKULA,POSTACT                                                
         MVC   X.TRNKWORK,POSTOFFC                                              
         MVC   X.TRNKCULC,POSTCAC                                               
         L     R0,ATIA             DO NOT ALLOW GETOPT TO USE TIA               
         XC    ATIA,ATIA                                                        
         L     RF,AGOPBLK          CLEAR GETOPT OWN SAVED STORAGE               
         XC    0(GOADM-GOBLOCK,RF),0(RF)                                        
         GOTO1 AGETOPT,BODMCB,X.TRNKEY                                          
         ST    R0,ATIA                                                          
         DROP  X                                                                
         USING PTAELD,R3                                                        
X        USING PTAELD,BOELEM                                                    
         XC    X.PTAEL(PTARLN1Q),X.PTAEL                                        
         L     RE,AGOPBLK          GET DESTINATION CLIENT RECORD                
         ICM   RF,15,GOACLI-GOBLOCK(RE)                                         
         AH    RF,=Y(ACCORFST)     NOTE RECORD IN ACCFIL FORMAT                 
         SR    R0,R0                                                            
         USING LOKELD,RF                                                        
ALLX02   IC    R0,LOKLN            LOKEL CANNOT BE FIRST ELEMENT                
         AR    RF,R0                                                            
         CLI   LOKEL,0             FIND LOCK ELEMENT                            
         BE    ALLX04                                                           
         CLI   LOKEL,LOKELQ                                                     
         BNE   ALLX02                                                           
         TM    LOKSTAT,LOKSLOCK    TEST RECORD IS LOCKED                        
         BO    EXIT5               YES - DO NOT ADD PTAEL                       
         DROP  RF                                                               
*                                                                               
ALLX04   MVI   X.PTAEL,PTAELQ                                                   
         MVI   X.PTALN,PTARLN1Q                                                 
         MVC   X.PTADATE,BCTODAYC                                               
         MVC   X.PTAPERS,PTAPERS                                                
         MVI   X.PTATYPE,PTATRAL                                                
         MVI   X.PTASTAT1,PTASPEND+PTASCASH                                     
         ZAP   X.PTANET,PTANET                                                  
         ZAP   X.PTANETF,PTANETF                                                
         ZAP   X.PTACDSC,PTACDSC                                                
         MVC   X.PTACUR,PTACUR                                                  
         MVC   X.PTAHOURS,PTAHOURS                                              
         MVC   X.PTAOFFC,PTAOFFC                                                
         L     RE,AGOPBLK                                                       
         ZAP   X.PTARCORT,GOAGYCOM-GOBLOCK(L'GOAGYCOM,RE)                       
         ZAP   X.PTARCOM,BCPZERO                                                
         TM    PTASTAT2,PTASXCOM   TEST TRANSFER IS COMMISSIONABLE              
         BNO   EXIT5                                                            
         ZAP   BOPL81(16),X.PTANET SET NET ALLOCATED                            
         AP    BOPL81(16),X.PTACDSC  ADD IN CASH DISCOUNT                       
         SRP   BOPL81(16),2,0                                                   
         MP    BOPL81(16),X.PTARCORT                                            
         SRP   BOPL81(16),64-8,5                                                
         ZAP   X.PTARCOM,BOPL81+8(8)                                            
EXIT5    XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
WOTABD   DSECT                                                                  
WOACSPA  DS    XL1                 ACCOUNT SPATYPE                              
WOCASPA  DS    XL1                 CONTRA-ACCOUNT SPATYPE                       
WODRCR   DS    CL1                 DEBIT/CREDIT INDICATOR                       
WODRQ    EQU   C'D'                DEBIT                                        
WOCRQ    EQU   C'C'                CREDIT                                       
WOSIGN   DS    XL1                 AMOUNT SIGN                                  
WOSPOS   EQU   C'+'                + PTANET                                     
WOSNEG   EQU   C'-'                - PTANET                                     
WOSYS    DS    XL1                 SYSTEM FILTER                                
WOCTRY   DS    XL1                 COUNTRY FILTER                               
WOINDS1  DS    XL1                 INDICATOR BYTE                               
WOIAPEAC EQU   X'80'               ADD TO APEEL FOR ACCOUNT                     
WOIAPECA EQU   X'40'               ADD TO APEEL FOR CONTRA-ACCOUNT              
WOIANACA EQU   X'20'               CONTRA-ACCOUNT IS FOR ANALYSIS ONLY          
WOICAN2P EQU   X'10'               GET C/A NAME FROM 2P ACCOUNT                 
WOICANCL EQU   X'08'               GE C/A NAME FROM CLIENT                      
WOIAPESK EQU   X'04'               THIS FOR SK/SI FLIP ONLY                     
         DS    XL5                 N/D                                          
WOTABL   EQU   *-WOTABD                                                         
         EJECT                                                                  
UPDWORKD DSECT                     ** LOCAL WORKING STORAGE **                  
RELO     DS    A                                                                
UPDSAVRE DS    A                                                                
ARELROUT DS    0A                                                               
APOST    DS    A                                                                
APINIT   DS    A                                                                
AALLXFR  DS    A                                                                
APACCR   DS    A                                                                
APTRNA   DS    A                                                                
APDIFF   DS    A                                                                
UPDLSTDA DS    F                   A(LAST D/A TABLE ENTRY)                      
UPDLSTWC DS    CL2                 PREVIOUS WORKCODE                            
         ORG   UPDLSTWC                                                         
UPDFOFFC DS    CL2                 FINANCIAL OFFICE (FEEADJ)                    
UPDFLAG  DS    XL1                 FLAG BYTE                                    
UPDFEXIT EQU   X'80'                                                            
UPDFFRST EQU   X'40'                                                            
UPDFCHNG EQU   X'20'                                                            
UPDREVBL EQU   X'10'               BILL INCLUDES REVERSALS                      
UPDBLNG  DS    CL1                 BILLING FLAG                                 
UPDKEY   DS    CL42                UPDATE JOB KEY                               
UPDWCSRG DS    CL1                 SURCHARGABLE WORKCODE FLAG                   
WOFILT   DS    XL1                 FILTER BITS FOR APEEL                        
UPDBMNTH DS    CL2                 EBCDIC MONTH NUMBER FOR BILL RESET           
UPDTRNEL DS    XL(FF)              TRANSACTION ELEMENT FOR BLDTRN               
UPDSQACT DS    CL14                SQ ACCOUNT FOR ACCRUALS                      
UPDPSTAC DS    CL14                SAVED POSTING ACOUNT                         
UPDSRCAN DS    CL(L'ACTKACT)       SURCHARGE A/C COSTING CODE                   
         ORG   UPDSRCAN                                                         
UPDFEEAN DS    CL(L'ACTKACT)       FEEADJ INCOME COSTING CODE                   
UPDCSTNM DS    CL(L'NAMEREC)       CLIENT COSTING ACCOUNT NAME                  
UPDULBNM DS    CL(L'NAMEREC)       BILLINGS COSTING ACCOUNT NAME                
UPDULRNM DS    CL(L'NAMEREC)       REVENUE COSTING ACCCOUNT NAME                
UPDSANAL DS    CL(L'ACTKACT)       SAVE COSTING CODE                            
*                                                                               
XFRTOCST DS    CL(L'ACTKULA)       TRANSFER TO JOB COSTING CODE                 
         ORG   XFRTOCST                                                         
UPDFEECA DS    CL(L'ACTKULA)       FEE COST ACCOUNT (ULREV+UPDFEEAN)            
XFRSIACC DS    CL(L'ACTKULA)       TRANSFER SIACCOUNT                           
XFRSICST DS    CL(L'ACTKULA)       TRANSFER SIACCOUNT ANALYSIS CODE             
XFRSKACC DS    CL(L'ACTKULA)       TRANSFER SKACCOUNT                           
XFR1CACC DS    CL(L'ACTKULA)       TRANSFER 1CACCOUNT                           
XFRTOOFC DS    CL2                 TRANSFER TO OFFICE CODE                      
XFRFROFC DS    CL2                 TRANSFER FROM OFFICE CODE                    
*                                                                               
UPDMNTHS DS    0CL18                                                            
         DS    12XL(L'UPDMNTHS)                                                 
         DS    XL(L'UPDMNTH)                                                    
UPDPSTAS DS    PL6                 SAVED TRANSACTION AMOUNT                     
UPDBLMCS DS    CL2                 SAVED CHARACTER FORMAT MOA                   
UPDBLRFS DS    CL4                 SAVED BILL REFERENCE                         
UPDMNTH1 DS    PL8                 SAVED MONTH1 AMOUNT                          
UPDTARGT DS    PL8                 TARGET AMOUNT WHEN ACCRUING                  
UPDXPCNT DS    PL4                 TRANSFER PERCENTAGE                          
*                                                                               
UPDVRATT DS    (TLDUVTMX)XL(L'VTCTYPE+L'VTCRATE)                                
*                                                                               
UPDAMTS  DS    0PL8                                                             
UPDDSCPT DS    PL8                 DISCOUNT PERCENT                             
UPDSCHPT DS    PL8                 SURCHARGE PERCENT                            
UPDCBAPN DS    PL8                 PENDING ALLOCATION (NET)                     
UPDCBAPC DS    PL8                 PENDING ALLOCATION (COMMISSION)              
UPDCBALN DS    PL8                 PENDING ALLOCATION (NET) BILLCURR            
UPDCBALC DS    PL8                 PENDING ALLOCATION (COMM) BILLCURR           
UPDCBAPD DS    PL8                 PENDING ALLOCATION (DISCOUNT)                
UPDCBWP  DS    PL8                 PENDING WRITE-OFF                            
UPDCBWPT DS    PL8                 PENDING WRITE-OFF (TIME)                     
UPDCBWPC DS    PL8                 PENDING WRITE-OFF (COST)                     
UPDCBRP  DS    PL8                 PENDING RECOVERY                             
UPDCBRPT DS    PL8                 PENDING RECOVERY (TIME)                      
UPDCBRPC DS    PL8                 PENDING RECOVERY (COST)                      
UPDCBTP  DS    PL8                 PENDING TRANSFER                             
UPDCBFP  DS    PL8                 PENDING FEE ADJUSTMENT                       
UPDBLBL  DS    PL8                 TOTAL BILLABLE ON JOB                        
UPDBHAPN DS    PL8                 BILL HEADER ALLOCATION (NET)                 
UPDBHAPC DS    PL8                 BILL HEADER ALLOCATION (COMMISSION)          
UPDAMTSL EQU   *-UPDAMTS                                                        
*                                                                               
UPDTOTLS DS    0PL8                                                             
UPDACCAC DS    PL8                 INCOME A/C AGYCURR COMMISSION                
UPDACCFC DS    PL8                 INCOME A/C FOREIGN COMMISSION                
UPDACCAN DS    PL8                 INCOME A/C AGYCURR NET                       
UPDACCFN DS    PL8                 INCOME A/C FOREIGN NET                       
*                                                                               
UPDTOTAC DS    PL8                 BILL TOTAL AGYCURR COMMISSION                
UPDTOTFC DS    PL8                 BILL TOTAL FOREIGN COMMISSION                
UPDTOTAN DS    PL8                 BILL TOTAL AGYCURR NET                       
UPDTOTFN DS    PL8                 BILL TOTAL FOREIGN COMMISSION                
UPDTOTAV DS    PL8                 BILL TOTAL VAT AGYCURR                       
UPDTOTFV DS    PL8                 BILL TOTAL VAT FOREIGN                       
UPDTOTAD DS    PL8                 BILL TOTAL DISCOUNT  (AGYCURR)               
UPDTOTFD DS    PL8                 BILL TOTAL DISCOUNT  (FOREIGN)               
UPDTOTAS DS    PL8                 BILL TOTAL SURCHARGE (AGYCURR)               
UPDTOTFS DS    PL8                 BILL TOTAL SURCHARGE (FOREIGN)               
UPDTOTLL EQU   *-UPDTOTLS                                                       
*                                                                               
UPDPOSTB DS    XL(POSTVALL)                                                     
*                                                                               
UPDTYPES DS    XL1                 TYPES OF ACTIVITY BEING UPDATED              
UPDBILQ  EQU   X'80'               UPDATE BILL ALLOCATION                       
UPDWOFQ  EQU   X'40'               UPDATE WRITE-OFFS                            
UPDRECQ  EQU   X'20'               UPDATE WRITE-OFF RECOVERIES                  
UPDTRFQ  EQU   X'10'               UPDATE TRANSFERS                             
UPDFEEQ  EQU   X'08'               UPDATE FEE ADJUSTMENTS                       
*                                                                               
UPDINIT  DS    CL1                 ADDTRN AND REPORT INITIALISED                
*                                                                               
UPDRECRF DS    CL4                 RECOVERY REF/MONTH                           
UPDRECMO DS    PL2                                                              
UPDRECMC DS    CL2                                                              
*                                                                               
UPDFEERF DS    CL4                 FEEADJ REF/MONTH                             
UPDFEEMO DS    PL2                                                              
UPDFEEMC DS    CL2                                                              
*                                                                               
UPDJCBEL DS    XL(JCBLNQ)          SAVED JOB JCBEL                              
UPDTOTAL DS    XL2                 TOTAL MARKED ITEMS FOR THIS UPDATE           
*                                                                               
UPDXTRA  DS    CL500                                                            
UPDXTRAS DS    CL500                                                            
UPDXTRLN EQU   *-UPDXTRA                                                        
CACMAXQ  EQU   12                                                               
CACTAB   DS    (CACMAXQ)XL(CACTABLQ)                                            
         DS    C                                                                
UPDMAXDA EQU   400                 400 FOR THE TIME BEING                       
UPDDATAB DS    (UPDMAXDA)XL5       TABLE OF TRANSACTION D/AS                    
UPDPTRS  DS    500C                                                             
*                                                                               
UPDWORKX EQU   *                                                                
*                                                                               
UPDMONTB DSECT                                                                  
UPDMONTH DS    0CL18                                                            
UPDMNTH  DS    CL2                 MOA CHARACTER FORMAT                         
UPDMOA   DS    PL2                 MOA PWOS                                     
UPDMREF  DS    CL4                 BATCH REFERENCE                              
UPDMPCT  DS    PL4                 THIS MONTH PERCENT OF TOTAL                  
UPDMAMT  DS    PL6                 AMOUNT (FOR SUMMARY)                         
*                                                                               
CACTABD  DSECT                                                                  
CACACTK  DS    CL14                ACCOUNT KEY                                  
CACACTN  DS    CL(L'NAMEREC)       ACCOUNT NAME                                 
CACACTP  DS    CL14                SI=ANALYSIS CODE  SJ=COSTING CODE            
CACOFFC  DS    CL2                 CLI/PRO/JOB OFFICE CODE                      
CACTABLQ EQU   *-CACTABD                                                        
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
       ++INCLUDE ACCLBWORK                                                      
         SPACE 1                                                                
WORKD    DSECT                                                                  
         ORG   BOELEM                                                           
         DSDDL PRINT=YES                                                        
*                                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBFDD                                                       
         SPACE 1                                                                
* DDCONBLK                                                                      
         PRINT OFF                                                              
CONBLKD  DSECT                                                                  
       ++INCLUDE DDCONBLK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'168ACCLB06   03/13/01'                                      
         END                                                                    
