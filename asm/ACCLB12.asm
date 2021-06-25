*          DATA SET ACCLB12    AT LEVEL 141 AS OF 08/16/00                      
*PHASE T62112A                                                                  
CLB12    TITLE '- BILL PROGRAM - MATCH 2 LIST - C VERSION'                      
CLB12    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB12**,R8,R7,RR=RE                                           
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK                                                      
         USING MWORKD,RC                                                        
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING PRORATAD,LSPRATA                                                 
M        USING TLREC,BCLSTMAT                                                   
         USING TRNRECD,IOKEY                                                    
         USING POSTVALS,MPOSTVAL                                                
         ST    RE,BORELO                                                        
*                                                                               
         XC    MAEXC,MAEXC         SAVE A(EXCHANGE RATE RULE) IF NECC.          
         CLC   M.TLMCUR,CSCPYCUR                                                
         BE    *+16                                                             
         LA    RE,M.TLMX                                                        
         ST    RE,MAEXC                                                         
         OI    MINDS,MIFCB                                                      
*                                                                               
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
         B     SETHEAD             SET UP MY OWN HEADING                        
         B     VALSEL              VALIDATE LIST SELECTION                      
         B     EXITY               DISPLAY SUB-TOTAL                            
         B     DISSCR              DISPLAY SCREEN TOTAL                         
         B     PFKRTN                                                           
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* FIRST FOR SCREEN                                                    *         
***********************************************************************         
         SPACE 1                                                                
SCRFRST  MVC   BASJOBC,BCJOBCOD                                                 
*        MVC   BASJOBN,BCJOBNAM                                                 
         MVC   BASBILL,M.TLMBILL                                                
         BAS   RE,GETMTC                                                        
         TM    MINDS,MIFCB                                                      
         BO    *+10                                                             
         OC    CSMASK,=AL2(CSMMAC)                                              
*                                                                               
         MVC   MLSOPS,LSOPS        EVALUATE FILTERS/OPTIONS                     
         XC    LSOPS(LSOPL),LSOPS                                               
*                                                                               
         L     RE,=A(VALOPT)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         LA    RE,OSVALS                                                        
         MVC   AOVEROUT,ALSVALS                                                 
*                                                                               
         XC    BOWORK2,BOWORK2                                                  
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         OC    BOWORK2(L'GOCBDINV),GOCBDINV-GOBBLOCK(RF)                        
         BNZ   *+10                                                             
         MVC   BOWORK2(DEFCLML),DEFCLM                                          
         GOTO1 AVALOPT,BOPARM,OPTTAB,BOWORK2,0                                  
         BNE   SCRFRSTN                                                         
*                                                                               
         CLC   MLSOPS(LSFLTOPL),LSOPS SET CC=HIGH IF FILETS CHANGED             
         BNE   EXITH                                                            
         B     EXITY                                                            
*                                                                               
SCRFRSTN MVC   LSOPS(LSOPL),MLSOPS RESTORE PREVIOUS FILTERS                     
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXITL                                                            
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE                                                                        
***********************************************************************         
         SPACE 1                                                                
PFKRTN   CLI   BCPFKEY,PFKMUPDQ                                                 
         BE    UPDATE                                                           
         CLI   BCPFKEY,PFKMDFTQ                                                 
         BE    DRAFT                                                            
*                                                                               
UPDATE   GOTO1 PROC,MILIVE                                                      
         BNE   EXIT                                                             
         TM    MINDS,MIPOST                                                     
         BO    EXIT                                                             
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$MINUP)                                           
         B     EXIT                                                             
*                                                                               
DRAFT    GOTO1 PROC,MIDRAFT                                                     
         BNE   EXIT                                                             
         TM    MINDS,MIPOST                                                     
         BO    EXIT                                                             
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$NOPMM)                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PROCESS RECORDS FOR DRAFT/UPDATE                         *         
*                                                                     *         
* NTRY: R1 = MILIVE / MIDRAFT                                         *         
***********************************************************************         
         SPACE 1                                                                
PROC     NTR1  ,                                                                
         STC   R1,BOBYTE1                                                       
         OC    MINDS,BOBYTE1                                                    
*                                                                               
         CP    SVNETMB,BCPZERO     TEST ANYTHING MATCHED                        
         BNE   PROC02                                                           
         CP    SVCOMMB,BCPZERO                                                  
         BNE   PROC02                                                           
         MVC   FVMSGNO,=AL2(AE$YHDAY)                                           
         OI    FVCURIND,FVCKEEP                                                 
         B     EXITL                                                            
*                                                                               
                                                                                
PROC02   GOTO1 TSTRANGE,1                                                       
         BNE   EXIT                                                             
*                                                                               
         GOTO1 POSTINIT                                                         
*                                                                               
         L     R1,AIOMCH                                                        
         GOTO1 GETPTA,TRNRFST-TRNRECD(R1)                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R4,R1                                                            
MCH      USING PTAELD,R4           R4=A(PTA ELEMENT FROM MATCH RECORD)          
         SP    MCH.PTANET,SVNETMA  ADJUST MATCH RECORD                          
         SP    MCH.PTARCOM,SVCOMMA                                              
         LH    RE,MCH.PTAHOURS                                                  
         CVD   RE,BODUB1                                                        
         SP    BODUB1,SVHOURM                                                   
         BNM   *+10                                                             
         ZAP   BODUB1,BCPZERO                                                   
         CVB   RE,BODUB1                                                        
         STH   RE,MCH.PTAHOURS                                                  
         TM    MINDS,MIFCB                                                      
         BZ    *+16                                                             
         SP    MCH.PTANETF,SVNETMB                                              
         SP    MCH.PTARFCOM,SVCOMMB                                             
*                                                                               
         MVC   TLNUM,CSPSRECN                                                   
PROC12   ICM   RE,3,TLNUM                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,3,TLNUM                                                       
         CLC   TLNUM,CSHIRECN                                                   
         BH    PROC20                                                           
         GOTO1 ATSARIO,TSAGET                                                   
         CP    TLINMB,BCPZERO                                                   
         BE    PROC12                                                           
*                                                                               
         MVC   IODAOVER,TLDA                                                    
         L     R1,=A(IOINV)                                                     
         LA    R1,IOGET+IOACCMST(R1)                                            
         TM    MINDS,MILIVE                                                     
         BZ    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,POST             MAKE ANY POSTINGS REQUIRED                   
         TM    MINDS,MIDRAFT       TEST DRAFTING ONLY                           
         BO    PROC12                                                           
*                                                                               
         L     R1,AIOINV                                                        
         GOTO1 GETPTA,TRNRFST-TRNRECD(R1)                                       
         BNE   PROC14                                                           
INV      USING PTAELD,R1           R3=A(PTA ELEMENT FOR INVOICE)                
         XR    RF,RF                                                            
         AP    INV.PTANET,TLINMA   ADD TO EXISTING AGENCY TOTALS                
         AP    INV.PTARCOM,TLICMA                                               
         ZAP   BODUB1,TLIHRM                                                    
         CVB   RE,BODUB1                                                        
         AH    RE,INV.PTAHOURS                                                  
         STH   RE,INV.PTAHOURS                                                  
         TM    MINDS,MIFCB                                                      
         BZ    PROC18                                                           
         AP    INV.PTANETF,TLINMB ADD TO EXISTING BILLING TOTALS                
         AP    INV.PTARFCOM,TLICMB                                              
         B     PROC18                                                           
         DROP  INV                                                              
*                                                                               
INV      USING PTAELD,BOELEM                                                    
PROC14   IC    RF,MCH.PTALN        ADD NEW ELEMENT                              
         EX    RF,*+4                                                           
         MVC   INV.PTAELD(0),MCH.PTAELD                                         
         ZAP   INV.PTANET,TLINMA   SET AGENCY TOTALS FOR NEW ELEMENT            
         ZAP   INV.PTARCOM,TLICMA                                               
         ZAP   BODUB1,TLIHRM                                                    
         CVB   RE,BODUB1                                                        
         STH   RE,INV.PTAHOURS                                                  
         TM    MINDS,MIFCB                                                      
         BZ    PROC16                                                           
         ZAP   INV.PTANETF,TLINMB SET BILLING CURRENCY TOTALS                   
         ZAP   INV.PTARFCOM,TLICMB                                              
PROC16   GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIOINV,INV.PTAELD                    
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  INV                                                              
*                                                                               
PROC18   L     R1,=A(IOINV)                                                     
         GOTO1 AIO,IOWRITE+IOACCMST(R1)                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   MNET,BCPZERO                                                     
         GOTO1 MATCHNET,1                                                       
         ZAP   MCOM,BCPZERO                                                     
         GOTO1 MATCHCOM,1                                                       
         GOTO1 ATSARIO,TSAPUT                                                   
         B     PROC12                                                           
*                                                                               
PROC20   TM    MINDS,MIDRAFT       TEST DRAFT ONLY                              
         BO    PROCX                                                            
*                                                                               
         ZAP   SVNETTA,SVNETAA     NEW TOTALS=AMOUNT AVAILABLE                  
         ZAP   SVNETTB,SVNETAB                                                  
         ZAP   SVCOMTA,SVCOMAA                                                  
         ZAP   SVCOMTB,SVCOMAB                                                  
         ZAP   SVHOURT,SVHOURA     TEST HOURS GONE NEGATIVE                     
         BNM   *+10                                                             
         ZAP   SVHOURT,BCPZERO                                                  
*                                                                               
         CP    MCH.PTANET,BCPZERO  TEST FULLY MATCHED                           
         BNE   PROC30                                                           
         CP    MCH.PTARCOM,BCPZERO                                              
         BNE   PROC30                                                           
         MVI   MCH.PTAEL,FF        YES - DELETE ELEMENT                         
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('FF',AIO2),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  MCH                                                              
*                                                                               
         L     R3,AIOMCH           TEST ANY MORE ACTIVE PTAELS                  
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         USING PTAELD,R3                                                        
         XR    RF,RF                                                            
PROC22   CLI   PTAEL,0                                                          
         BE    PROC26                                                           
         CLI   PTAEL,PTAELQ                                                     
         BNE   PROC24                                                           
         OC    PTANET,PTANET                                                    
         BZ    PROC24                                                           
         CP    PTANET,BCPZERO                                                   
         BNE   PROC30                                                           
         CLI   PTATYPE,PTATRAL                                                  
         BNE   PROC24                                                           
         CP    PTARCOM,BCPZERO                                                  
         BNE   PROC30                                                           
PROC24   IC    RF,PTALN                                                         
         BXH   R3,RF,PROC22                                                     
         DROP  R3                                                               
*                                                                               
PROC26   OI    MINDS,MIDEL         NO - DELETE RECORD                           
         L     R2,AIOMCH                                                        
         USING TRNRECD,R2                                                       
         OI    TRNRSTA,TRNSDELT    SET DELETE BIT ON FILE RECORD                
         MVC   IOKEY,TRNKEY        DELETE THE DIRECTORY RECORD                  
         GOTO1 AIO,IORDUP+IOACCDIR                                              
         OI    IOKEY+(TRNKSTA-TRNRECD),TRNSDELT                                 
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         DROP  R2                                                               
*                                                                               
PROC30   L     R1,=A(IOMCH)                                                     
         GOTO1 AIO,IOWRITE+IOACCMST(R1)                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,CLRTOTS                                                       
*                                                                               
         TM    MINDS,MIDEL         TEST ADVANCE ITEM DELETED                    
         BNO   PROCX                                                            
         MVC   IODAOVER,BCJOBDA                                                 
         GOTO1 AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,AIO2                                                          
         LA    RE,TRNRFST-TRNRECD(RE)                                           
         SR    R0,R0                                                            
PROC32   IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),ASTELQ                                                     
         BE    PROC34                                                           
         CLI   0(RE),JCBELQ                                                     
         BE    PROC36                                                           
         CLI   0(RE),0                                                          
         BE    PROC38                                                           
         B     PROC32                                                           
*                                                                               
PROC34   SR    RF,RF               PICK UP DRAFT COUNT                          
         ICM   RF,7,ASTDRAFT-ASTELD(RE)                                         
         BZ    *+6                 DO NOT SET NEGATIVE                          
         BCTR  RF,0                                                             
         STCM  RF,7,ASTDRAFT-ASTELD(RE)                                         
         B     PROC32                                                           
*                                                                               
PROC36   SR    RF,RF               PICK UP ADVANCE COUNT                        
         ICM   RF,3,JCBADV-JCBELD(RE)                                           
         BZ    *+6                 DO NOT SET NEGATIVE                          
         BCTR  RF,0                                                             
         STCM  RF,3,JCBADV-JCBELD(RE)                                           
         B     PROC32                                                           
*                                                                               
PROC38   GOTO1 AIO,IOWRITE+IOACCMST+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PROCX    LA    RF,BASACTH                                                       
         ST    RF,FVADDR                                                        
         TM    MINDS,MIPOST                                                     
         BZ    EXITY                                                            
         GOTO1 ABLDTRN,BOPARM,(X'FF',POSTVALS)                                  
         L     R4,AREP                                                          
         USING REPD,R4                                                          
         MVI   REPACTN,REPACLO                                                  
         GOTO1 VREPORT,REPD                                                     
         MVI   FVOMTYP,GTMINF      REPORT SPOOLED MESSAGE                       
         MVC   FVMSGNO,=AL2(AI$RPSPL)                                           
         MVI   FVPARMS,9                                                        
         MVC   FVPARMS+1(L'REPSUBID),REPSUBID                                   
         MVC   FVPARMS+1+L'REPSUBID(1),BCCOMMA                                  
         LA    RF,FVPARMS+1+L'REPSUBID+1                                        
         EDIT (2,REPREPNO),(5,(RF)),ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1           
*                                                                               
         CLI   P#DQU,C'Y'          TEST ALWAYS OUTPUT DQU                       
         BE    PROCX4                                                           
         TM    MINDS,MILIVE                                                     
         BZ    PROCX2                                                           
         CLI   P#DQU,C'L'          TEST ONLY FOR LIVE                           
         BE    PROCX4                                                           
         B     EXITY                                                            
PROCX2   CLI   P#DQU,C'D'          TEST ONLY FOR DRAFT                          
         BNE   EXITY                                                            
PROCX4   MVC   BASSRV,DQU                                                       
         OI    BASSRVH+FHOID,FHOITR                                             
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR POSTINGS                                             *         
***********************************************************************         
         SPACE 1                                                                
POSTINIT NTR1  ,                                                                
         MVC   POSTOFFC,CSOFFICE                                                
         MVC   POSTREF,M.TLMBILL                                                
         MVC   POSTDATE,M.TLMDATP                                               
         MVI   POSTTYPE,POSTMADJ                                                
         MVI   POSTMODE,POSTDFQ                                                 
         MVC   POSTCCPY,CUABIN                                                  
         TM    MINDS,MILIVE                                                     
         BZ    *+8                                                              
         MVI   POSTMODE,POSTLVQ                                                 
POSTINIX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MAKE POSINGS                                             *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING PPRELD,BCCMPPRF                                                  
POST     NTR1  ,                                                                
         CLC   M.TLMINCAC,TLIINCAC TEST CHANGE OF INCOME ACCOUNTS               
         BE    POST10                                                           
I        USING TRNKEY,M.TLMINCAC                                                
         GOTO1 BLDTRN,BTPARM,=C'C-',                                   *        
               I.TRNKUNT,I.TRNKACT,0,                                  *        
               BCCPYPRD,BCPROCOD,BCPRONAM                                       
I        USING TRNKEY,TLIINCAC                                                  
         GOTO1 (RF),(R1),=C'C+',                                       *        
               I.TRNKUNT,I.TRNKACT,0,                                  *        
               ,,                                                               
         DROP  I                                                                
*                                                                               
         GOTO1 GETCOST,TLIINCAC                                                 
         CLC   MANC,MATCHANC       TEST CHANGE OF COSTING CODES                 
         BE    POST10                                                           
         CLI   MATCHANC,C' '                                                    
         BNH   POST02                                                           
         GOTO1 BLDTRN,BTPARM,=C'D-',                                   *        
               PPRCOSTU,PPRCOSTA,0,                                    *        
               UL12,MATCHANC,0                                                  
         GOTO1 (RF),(R1),=C'C-',                                       *        
               UL12,MATCHANC,0,                                        *        
               PPRCOSTU,PPRCOSTA,0                                              
*                                                                               
POST02   CLI   MANC,C' '                                                        
         BNH   POST10                                                           
         GOTO1 BLDTRN,BTPARM,=C'D+',                                   *        
               PPRCOSTU,PPRCOSTA,0,                                    *        
               UL12,MANC,0                                                      
         GOTO1 (RF),(R1),=C'C+',                                       *        
               UL12,MANC,0,                                            *        
               PPRCOSTU,PPRCOSTA,0                                              
*                                                                               
POST10   L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         CLI   TRNTYPE,49          TEST A TIMESHEET                             
         BNE   POST20                                                           
*                                                                               
         XR    RF,RF               TEST FOR SK ATTRIBUTE                        
         LA    R3,TRNRFST                                                       
         USING APEELD,R3                                                        
POST12   CLI   APEEL,0                                                          
         BE    POST20                                                           
         IC    RF,APELN                                                         
         CLI   APEEL,APEELQ                                                     
         BNE   *+14                                                             
         CLC   ULSK,APENACT                                                     
         BE    *+8                                                              
         BXH   R3,RF,POST12                                                     
         MVC   SKULACT,BCSPACES                                                 
         SH    RF,=Y(APENACT+1-APEELD)                                          
         EX    RF,*+4                                                           
         MVC   SKULACT(0),APENACT                                               
         DROP  R3                                                               
*                                                                               
         GOTO1 BLDTRN,BTPARM,=C'C-',                                   *        
               SKUL,SKACT,0,                                           *        
               BCCPYPRD,BCJOBCOD,BCJOBNAM                                       
         GOTO1 (RF),(R1),=C'C+',                                       *        
               ULSI,SKACT,0,                                           *        
               BCCPYPRD,BCPROCOD,BCPRONAM                                       
*                                                                               
         MVC   BOWORK1(L'TRNKCPY),CUABIN                                        
         MVC   BOWORK1+L'TRNKCPY(L'POSTACT),POSTACT                             
         GOTO1 GETCOST,BOWORK1                                                  
         CLI   MANC,C' '                                                        
         BNH   POST20                                                           
         GOTO1 BLDTRN,BTPARM,=C'D+',                                   *        
               PPRCOSTU,PPRCOSTA,0,                                    *        
               UL1C,MANC,0                                                      
         GOTO1 (RF),(R1),=C'C+',                                       *        
               UL1C,MANC,0,                                            *        
               PPRCOSTU,PPRCOSTA,0                                              
*                                                                               
POST20   DS    0H                                                               
         B     EXIT                                                             
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL BLDTRN                                              *         
*                                                                     *         
* NTRY: P1 = A(HALF WORD) - BYTE 1 = 'C'REDIT / 'D'EBIT               *         
*                           BYTE 2 = '+' / '-'                        *         
*       P2 = A(ACCOUNT UNIT/LEDGER)                                   *         
*       P3 = A(ACCOUNT CODE)                                          *         
*       P4 = A(ACCOUNT NAME) / 0                                      *         
*       P5 = A(CONTRA UNIT/LEDGER)                                    *         
*       P6 = A(CONTRA CODE)                                           *         
*       P7 = A(CONTRA NAME) / 0                                       *         
***********************************************************************         
         SPACE 1                                                                
BLDTRN   NTR1  ,                                                                
         L     RF,BTPARM1                                                       
         MVC   BTHALF,0(RF)                                                     
         LM    R2,R3,BTPARM2       SET ACCOUNT DETAILS                          
         MVC   POSTAUL,0(R2)                                                    
         MVC   POSTAACT,0(R3)                                                   
         ICM   R2,15,BTPARM4                                                    
         BZ    *+14                                                             
         MVC   POSTACTN,0(R2)                                                   
         B     BTRN02                                                           
         GOTO1 GETNAM,BOPARM,POSTAUL,POSTACTN                                   
*                                                                               
BTRN02   LM    R2,R3,BTPARM5       SET CONTRA DETAILS                           
         MVC   POSTCUL,0(R2)                                                    
         MVC   POSTCACT,0(R3)                                                   
         ICM   R2,15,BTPARM7                                                    
         BZ    *+14                                                             
         MVC   POSTCACN,0(R3)                                                   
         B     BTRN04                                                           
         GOTO1 GETNAM,BOPARM,POSTCUL,POSTCACN                                   
*                                                                               
BTRN04   TM    MINDS,MIPOST        TEST ANY POSTINGS MADE YET                   
         BNZ   BTRN10                                                           
         GOTO1 VALTRN                                                           
         BE    BTRN06                                                           
         TM    MINDS,MILIVE                                                     
         BZ    *+8                                                              
         OI    CSINDSG1,CSINDUNW                                                
         L     RD,BCSVRD           RETURN TO ROOT                               
         L     RD,8(RD)                                                         
         B     EXIT                                                             
*                                                                               
BTRN06   OI    MINDS,MIPOST                                                     
         GOTO1 PRTINIT             INITIALIZE PRINTING                          
         MVC   POSTBTRF,OSREF#                                                  
         MVC   POSTBTMC,OSMONC                                                  
         TM    MINDS,MILIVE                                                     
         BZ    BTRN10                                                           
         GOTO1 AADDOBH,BOPARM,('POSTMADJ',OSREF#),0                             
         GOTO1 AINIADT                                                          
*                                                                               
BTRN10   MVI   POSTSTAT,0                                                       
         CLI   BTHALF,C'D'                                                      
         BNE   *+8                                                              
         MVI   POSTSTAT,TRNSDR                                                  
         ZAP   BODUB1,TLINMA                                                    
         CLI   BTHALF+1,C'-'                                                    
         BNE   *+10                                                             
         MP    BODUB1,PMINUS1                                                   
         ZAP   POSTAMNT,BODUB1                                                  
*                                                                               
         GOTO1 ABLDTRN,BOPARM,POSTVALS                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE TRANSACTION DETAILS (DATE/REF#/MONTH)           *         
***********************************************************************         
         SPACE 1                                                                
VALTRN   NTR1  ,                                                                
*                                                                               
*        TM    BASDATH+FHIID,FHIIVA                                             
*        BNZ   VTRN02                                                           
*        OI    BASDATH+FHOID,FHOITR                                             
*        BAS   RE,VALDAT                                                        
*        BNE   EXITN                                                            
*        OI    BASDATH+FHIID,FHIIVA                                             
*        OI    BCINDS2,BCIHLDLP                                                 
*                                                                               
VTRN02   CLI   BASREFH+FHILD,0                                                  
         BNE   VTRN04                                                           
         CLI   BASMONH+FHILD,0                                                  
         BNE   VTRN04                                                           
*                                                                               
VTRN04   TM    BASREFH+FHIID,FHIIVA                                             
         BNZ   VTRN06                                                           
         OI    BASREFH+FHOID,FHOITR                                             
         BAS   RE,VALREF                                                        
         BNE   EXITN                                                            
         OI    BASREFH+FHIID,FHIIVA                                             
         OI    BCINDS2,BCIHLDLP                                                 
*                                                                               
VTRN06   TM    BASMONH+FHIID,FHIIVA                                             
         BNZ   VTRN08                                                           
         OI    BASMONH+FHOID,FHOITR                                             
         BAS   RE,VALMON                                                        
         BNE   EXITN                                                            
         OI    BASMONH+FHIID,FHIIVA                                             
         OI    BCINDS2,BCIHLDLP                                                 
*                                                                               
VTRN08   GOTO1 AADDOBH,BOPARM,('POSTMADJ',OSREF#),BASREFH                       
         BNE   EXITN                                                            
*                                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE THE DATE FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
*ALDAT   NTR1  ,                                                                
*        CLI   BASDATH+FHILD,0                                                  
*        BNE   VDAT02                                                           
*        GOTO1 VDATCON,BOPARM,(2,BCTODAYC),(17,BASDAT)                          
*DAT02   GOTO1 AFVAL,BASDATH                                                    
*        GOTO1 VPERVAL,BOPARM,(FVILEN,FVIFLD),(X'40',BOELEM)                    
*        CLI   4(R1),0             TEST VALID DATE                              
*        BE    VDAT04                                                           
*        CLI   4(R1),4                                                          
*        BE    VDAT04                                                           
*        MVC   FVMSGNO,=AL2(AE$INDAT)                                           
*        B     EXITN                                                            
*                                                                               
*DAT04   LA    R2,BOELEM                                                        
*        USING PERVALD,R2                                                       
*        MVC   OSDATC,PVALCSTA                                                  
*        MVC   OSDATP,PVALPSTA                                                  
*        MVC   BOWORK1+00(6),PVALESTA                                           
*        GOTO1 VDATCON,BOPARM,(2,BCTODAYC),(0,BOWORK1+6)                        
*        MVC   BASDAT,PVALCPER                                                  
*        XR    RF,RF                                                            
*        ICM   RF,1,BCP204         NUMBER OF DAYS BACKWARDS                     
*        BNZ   *+8                                                              
*        LA    RF,60               DEFAULT IS 60 DAYS                           
*        MVC   BOWORK1+4(2),=C'28' SET DAY TO 28                                
*        LA    RF,3(RF)            AND ALLOW 3 MORE DAYS                        
*        GOTO1 VADDAY,BOPARM,BOWORK1,BOWORK1+12,(RF)                            
*        CLC   BOWORK1+6(6),BOWORK1+12                                          
*        BNH   *+14                                                             
*        MVC   FVMSGNO,=AL2(AE$DOPSP)                                           
*        B     EXITN                                                            
*        MVC   BOWORK1+4(2),=C'01' SET DAY TO 1                                 
*        GOTO1 VADDAY,BOPARM,BOWORK1,BOWORK1+12,-60                             
*        CLC   BOWORK1+6(6),BOWORK1+12                                          
*        BNL   *+14                                                             
*        MVC   FVMSGNO,=AL2(AE$DOPSP)                                           
*        B     EXITN                                                            
*        XC    BOWORK1,BOWORK1                                                  
*        GOTO1 VDATCON,BOPARM,(2,OSDATC),(17,BOWORK1)                           
*        MVC   BASDAT,BCSPACES                                                  
*        MVC   BASDAT,BOWORK1                                                   
*        B     EXITY                                                            
*        DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDTE THE REFERENCE # FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
VALREF   NTR1  ,                                                                
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL,BASREFH                                                    
         BNE   EXITN                                                            
         XR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         LA    R1,FVIFLD                                                        
VREF02   CLI   0(R1),C'A'                                                       
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITN                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,VREF02                                                        
         MVC   OSREF#,FVIFLD                                                    
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE THE MONTH FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALMON   NTR1  ,                                                                
         CLI   BASMONH+FHILD,0     VALIDATE MONTH                               
         BNE   VMON02                                                           
         GOTO1 VDATCON,BOPARM,(2,BCTODAYC),(9,BASMON)                           
VMON02   GOTO1 AFVAL,BASMONH                                                    
         BNE   EXITN                                                            
         MVC   CSBSECL,BCCPYEL+(CPYBSEC-CPYELD)                                 
         LA    R3,BOWORK1                                                       
         USING BMONVALD,R3                                                      
         GOTO1 VBMONVAL,BOPARM,(FVILEN,FVIFLD),(79,ACOM),              *        
               (CULANG,BMONVALD),(CUABIN,0)                                     
         MVC   BOBYTE1,0(R1)       BATCH SECURITY LEVEL                         
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BE    *+14                                                             
         MVC   FVMSGNO,BMOMSG                                                   
         B     EXITN                                                            
*                                                                               
         MVC   BODUB1,BMOMOSP                                                   
         MVI   BODUB1+L'BMOMOSP,X'01'                                           
         MVC   BASMON,BCSPACES                                                  
         GOTO1 VDATCON,BOPARM,(1,BODUB1),(9,BASMON)                             
*                                                                               
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BNE   EXITN                                                            
         MVC   OSMONP,BMOMOSP                                                   
         MVC   OSMONC,BMOMOSC                                                   
         MVC   CSBSECL,BOBYTE1     SET BATCH SECURITY                           
*                                                                               
         B     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR PRINTING                                             *         
***********************************************************************         
         SPACE 1                                                                
PRTINIT  NTR1  ,                                                                
         L     R4,AREP                                                          
         USING REPD,R4                                                          
*                                                                               
         LA    R0,REPSPEC          INITIALIZE FOR PRINTING                      
         ST    R0,REPAPHS                                                       
         LA    RE,=C'AMD'          ??                                           
         CLI   BCPFKEY,PFKRLIVQ                                                 
         BNE   *+8                                                              
         LA    RE,=C'AMV'          ??                                           
         XC    REPSUBID,REPSUBID                                                
         OC    REPSUBID,CSREPID                                                 
         BNZ   *+10                                                             
         MVC   REPSUBID,0(RE)                                                   
         MVCDD REPDESC,AC#DRMAT                                                 
         MVI   REPSUBPG,REPSDPST                                                
         TM    MINDS,MILIVE                                                     
         BZ    *+14                                                             
         MVCDD REPDESC,AC#LIMAT                                                 
         MVI   REPSUBPG,REPSLPST                                                
         GOTO1 VDICTAT,BOPARM,C'SL  ',REPDESC                                   
*                                                                               
         MVI   REPACTN,REPAINI                                                  
         GOTO1 VREPORT,REPD                                                     
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         GOTO1 (RF),(R1)                                                        
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   REPACTN,REPAPUT                                                  
*                                                                               
         LA    R3,REPH3                                                         
         USING HLINE,R3                                                         
         MVC   HDATA1(L'CUAALF),CUAALF                                          
         L     RF,AGOPBLK                                                       
         L     RF,GOACOMP-GOBLOCKD(RF)                                          
         GOTO1 AGETELS,BOPARM,ACCORFST(RF)                                      
         MVC   HNAME1,ACNAME                                                    
         MVC   HDATA2(L'BCUSERID),BCUSERID                                      
*                                                                               
         LA    R3,REPH4                                                         
         MVC   HDATA1(L'CSOFFICE),CSOFFICE                                      
         LA    R2,IOKEY                                                         
         USING OGRRECD,R2                                                       
         XC    OGRKEY,OGRKEY                                                    
         MVI   OGRKTYP,OGRKTYPQ                                                 
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY,CUABIN                                                   
         MVC   OGRKUNT(L'OGRKUNT+L'OGRKLDG),BCCPYPRD                            
         MVC   OGRKOFC,CSOFFICE                                                 
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         L     R2,AIO1                                                          
         GOTO1 AGETELS,BOPARM,OGRRFST                                           
         MVC   HNAME1,ACNAME                                                    
         DROP  R2                                                               
         MVC   HDATA2(L'CSBPID),CSBPID                                          
*                                                                               
         LA    R3,REPH5                                                         
         MVC   HDATA1,BCCLICOD                                                  
         MVC   HNAME1,BCCLINAM                                                  
*                                                                               
         LA    R3,REPH6                                                         
         XR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         LA    RF,L'BCPROCOD-1                                                  
         SR    RF,RE                                                            
         LA    RE,BCPROCOD(RE)                                                  
         EX    RF,*+4                                                           
         MVC   HDATA1(0),0(RE)                                                  
         MVC   HNAME1,BCPRONAM                                                  
*                                                                               
         LA    R3,REPH7                                                         
         XR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         LA    RF,L'BCJOBCOD-1                                                  
         SR    RF,RE                                                            
         LA    RE,BCJOBCOD(RE)                                                  
         EX    RF,*+4                                                           
         MVC   HDATA1(0),0(RE)                                                  
         MVC   HNAME1,BCJOBNAM                                                  
         DROP  R3                                                               
*                                                                               
PRTINITX B     EXIT                                                             
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET COSTING CODE FROM INCOME ACCOUNT                     *         
*                                                                     *         
* NTRY: R1 = A(INCOME ACCOUNT)                                        *         
* EXIT: MINCAC = INOCME ACCOUNT                                       *         
*       MANC   = ANALYSIS COSTING ACCOUNT                             *         
***********************************************************************         
         SPACE 1                                                                
GETCOST  NTR1  ,                                                                
         CLC   MINCAC,0(R1)        TEST ALREADY HAVE INCOME ACCOUNT             
         BE    EXIT                                                             
         MVC   MINCAC,0(R1)                                                     
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'MINCAC),MINCAC                                           
         GOTO1 AGETACT,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MANC,BCSPACES                                                    
         MVC   MANC(L'ACCOST),ACCOST                                            
*&&US                                                                           
         CLI   MANC,C' '                                                        
         BH    GCOST04                                                          
         ICM   R3,15,ACASPA                                                     
         BZ    GCOST04                                                          
         USING SPAELD,R3                                                        
         XR    RF,RF                                                            
GCOST02  CLI   SPAEL,SPAELQ                                                     
         BNE   GCOST04                                                          
         CLI   SPATYPE,SPATANAL                                                 
         BE    *+12                                                             
         IC    RF,SPALN                                                         
         BXH   R3,RF,GCOST02                                                    
         MVC   MANC,SPAAANAL                                                    
         DROP  R3                                                               
*&&                                                                             
GCOST04  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET ACCOUNT NAME                                         *         
*                                                                     *         
* NTRY: P1=A(ACCOUNT CODE)                                            *         
*       P2=A(ACCOUNT NAME)                                            *         
***********************************************************************         
         SPACE 1                                                                
GETNAM   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
N        USING TLSTD,BOELEM                                                     
         XC    N.TLKEY,N.TLKEY                                                  
         MVI   N.TLKSES,TLKNAMQ                                                 
         MVC   N.TLKACT,0(R2)                                                   
         GOTO1 ATSARIO,BOPARM,('TSARDH',N.TLSTD)                                
         BE    GETNAMX                                                          
*                                                                               
         XC    N.TLKEY,N.TLKEY                                                  
         MVI   N.TLKSES,TLKNAMQ                                                 
         MVC   N.TLKACT,0(R2)                                                   
         MVC   N.TLDNAM,BCSPACES                                                
         MVC   N.TLRLEN,=AL2(TLNAMLNQ)                                          
*                                                                               
K        USING TRNRECD,IOKEY                                                    
         MVC   K.TRNKEY,BCSPACES                                                
         MVC   K.TRNKCPY,CUABIN                                                 
         MVC   K.TRNKUNT(L'TLKACT),N.TLKACT                                     
         DROP  K                                                                
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO1                                                          
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING NAMELD,R1                                                        
         XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         CLI   NAMEL,NAMELQ                                                     
         BE    *+8                                                              
         BXH   R1,RF,*-12                                                       
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+4                                                           
         MVC   N.TLDNAM(0),NAMEREC                                              
         DROP  R1                                                               
         GOTO1 ATSARIO,BOPARM,('TSAADD',N.TLSTD)                                
*                                                                               
GETNAMX  MVC   0(L'TLDNAM,R3),N.TLDNAM                                          
         B     EXITY                                                            
         DROP  N                                                                
         EJECT                                                                  
***********************************************************************         
* SET UP HEADINGS                                                     *         
***********************************************************************         
         SPACE 1                                                                
SETHEAD  TM    MINDS,MIFCB                                                      
         BZ    EXIT                                                             
         LM    R2,R4,0(R1)         R4=A(2ND HEADLINE)                           
         USING CLMTABD,R2          R2=A(COLUMN TABLE ENTRY)                     
*                                                                               
         XR    R3,R3               POINT R3 TO END OF HEADLINE                  
         IC    R3,CLMHWDTH                                                      
         AR    R3,R4                                                            
         BCTR  R3,0                                                             
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
*                                                                               
         CLI   0(R4),C' '          POINT R4 TO START OF HEADLINE                
         BH    *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
*                                                                               
         LR    RF,R3                                                            
         SR    RF,R4                                                            
         SH    RF,=Y(L'TLMCUR+2)                                                
         SRL   RF,1                                                             
         AR    RF,R4                                                            
         MVI   1(RF),C'('                                                       
         MVC   2(L'CSCPYCUR,RF),M.TLMCUR                                        
         CLC   CLMCHAR,=AL2(MT2#NMA)     ??                                     
         BL    *+10                ??                                           
         MVC   2(L'CSCPYCUR,RF),CSCPYCUR      ??                                
         MVI   L'CSCPYCUR+2(RF),C')'                                            
*                                                                               
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LAST FOR SCREEN                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING FOOTLIND,R1                                                      
SCRLAST  MVCDD FOOTLIN+FOOTLINL+7(21),AC#TOTAV                                  
         MVCDD FOOTLIN+FOOTLINL+FOOTLINL+7(21),AC#TOTMT                         
         CLC   FVMSGNO,=AL2(AI$ACTOK)                                           
         BNE   SCRLASTX                                                         
         GOTO1 TSTRANGE,0                                                       
         BNE   SCRLASTX                                                         
         MVC   FVMSGNO,=AL2(AI$AFCON)                                           
SCRLASTX B     EXITY                                                            
         DROP  R1                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY SCREEN TOTALS (TOTAL AMOUNT CHANGED)                        *         
***********************************************************************         
         SPACE 1                                                                
DISSCR   L     R2,AIOMCH                                                        
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     SCRWC               WORKCODE                                     
         B     SCRSUP              SUPPLIER/CONTRA                              
         B     SCRREF              TRANSACTION REFERENCE                        
         B     SCRNETB             NET                                          
         B     EXIT                NET AVAILABLE                                
         B     SCRCOMB             COMMISSION                                   
         B     SCRNETA             NET (AGENCY)                                 
         B     EXIT                NET AVAILABLE (AGENCY)                       
         B     SCRCOMA             COMMISSION (AGENCY)                          
         B     EXIT                COMMISSION RATE                              
         B     EXIT                MATCH EXCHANGE RATE                          
         B     EXIT                INVOICE EXCHANGE RATE                        
         B     SCRHRSM             HOURS MATCHED                                
         B     EXIT                HOURS AVAILABLE                              
*                                                                               
SCRWC    CLI   LSFOOT#,0                                                        
         BNE   EXIT                                                             
         BAS   RE,GETMTC                                                        
         MVC   FVIFLD(L'TRNKWORK),TRNKWORK                                      
         B     EXIT                                                             
         SPACE 1                                                                
SCRSUP   CLI   LSFOOT#,0                                                        
         BNE   EXIT                                                             
         MVC   FVIFLD(L'TRNKULC),TRNKULC                                        
         B     EXIT                                                             
         SPACE 1                                                                
SCRREF   CLI   LSFOOT#,0                                                        
         BNE   EXIT                                                             
         MVC   FVIFLD(L'TRNKREF),TRNKREF                                        
         B     EXIT                                                             
         SPACE 1                                                                
SCRNETB  XR    RF,RF                                                            
         IC    RF,LSFOOT#                                                       
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     SNETT                                                            
         B     SNETA                                                            
         B     SNETM                                                            
*                                                                               
SNETT    GOTO1 SCRFMT,BOPARM,SVNETTB,SVNETAB,SVNETMB,SCRNET1,SCRNET2,  *        
               SCRNET3                                                          
         MVC   FVIFLD(L'SCRNET1),SCRNET1                                        
         B     EXIT                                                             
SNETA    MVC   FVIFLD(L'SCRNET2),SCRNET2                                        
         B     EXIT                                                             
SNETM    MVC   FVIFLD(L'SCRNET3),SCRNET3                                        
         B     EXIT                                                             
*                                                                               
SCRCOMB  XR    RF,RF                                                            
         IC    RF,LSFOOT#                                                       
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     SCOMT                                                            
         B     SCOMA                                                            
         B     SCOMM                                                            
*                                                                               
SCOMT    GOTO1 SCRFMT,BOPARM,SVCOMTB,SVCOMAB,SVCOMMB,SCRCOM1,SCRCOM2,  *        
               SCRCOM3                                                          
         MVC   FVIFLD(L'SCRCOM1),SCRCOM1                                        
         B     EXIT                                                             
SCOMA    MVC   FVIFLD(L'SCRCOM2),SCRCOM2                                        
         B     EXIT                                                             
SCOMM    MVC   FVIFLD(L'SCRCOM3),SCRCOM3                                        
         B     EXIT                                                             
         SPACE 1                                                                
SCRNETA  XR    RF,RF                                                            
         IC    RF,LSFOOT#                                                       
         SLL   RF,3                                                             
         B     *+4(RF)                                                          
         LA    R0,SVNETTA                                                       
         B     SCRAGY                                                           
         LA    R0,SVNETAA                                                       
         B     SCRAGY                                                           
         LA    R0,SVNETMA                                                       
         B     SCRAGY                                                           
*                                                                               
SCRCOMA  XR    RF,RF                                                            
         IC    RF,LSFOOT#                                                       
         SLL   RF,3                                                             
         B     *+4(RF)                                                          
         LA    R0,SVCOMTA                                                       
         B     SCRAGY                                                           
         LA    R0,SVCOMAA                                                       
         B     SCRAGY                                                           
         LA    R0,SVCOMMA                                                       
*                                                                               
SCRAGY   GOTO1 FMTAGY,BOPARM,(R0),FVIFLD                                        
         B     EXIT                                                             
         SPACE 1                                                                
SCRHRSM  XR    RF,RF                                                            
         IC    RF,LSFOOT#                                                       
         SLL   RF,3                                                             
         B     *+4(RF)                                                          
         LA    R0,SVHOURT                                                       
         B     SCRHRS                                                           
         LA    R0,SVHOURA                                                       
         B     SCRHRS                                                           
         LA    R0,SVHOURM                                                       
*                                                                               
SCRHRS   GOTO1 FMTHRS,BOPARM,(R0),FVIFLD                                        
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*                                                                     *         
* NTRY: P1 = A(AMOUNT 1)                                              *         
*       P2 = A(AMOUNT 2)                                              *         
*       P3 = A(AMOUNT 3)                                              *         
*       P4 = A(OUTPUT FOR AMOUNT 1)                                   *         
*       P5 = A(OUTPUT FOR AMOUNT 2)                                   *         
*       P6 = A(OUTPUT FOR AMOUNT 3)                                   *         
***********************************************************************         
         SPACE 1                                                                
SCRFMT   NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         GOTO1 SCRCURE,BOPARM,(R2),SCRAMT1                                      
         GOTO1 (RF),(R1),(R3),SCRAMT2                                           
         GOTO1 (RF),(R1),(R4),SCRAMT3                                           
*                                                                               
         LA    RF,SCRAMT1                                                       
SFMT02   CLI   0(RF),C' '                                                       
         BH    SFMT04                                                           
         CLI   SCRAMT2-SCRAMT1(RF),C' '                                         
         BH    SFMT04                                                           
         CLI   SCRAMT3-SCRAMT1(RF),C' '                                         
         BH    SFMT04                                                           
         MVC   SCRAMT1(L'SCRAMT1-1),SCRAMT1+1                                   
         MVI   SCRAMT1+L'SCRAMT1-1,C' '                                         
         MVC   SCRAMT2(L'SCRAMT2-1),SCRAMT2+1                                   
         MVI   SCRAMT2+L'SCRAMT2-1,C' '                                         
         MVC   SCRAMT3(L'SCRAMT3-1),SCRAMT3+1                                   
         MVI   SCRAMT3+L'SCRAMT3-1,C' '                                         
         B     SFMT02                                                           
*                                                                               
SFMT04   LM    R2,R4,12(R1)                                                     
         MVC   0(L'SCRAMT1,R2),SCRAMT1                                          
         MVC   0(L'SCRAMT2,R3),SCRAMT2                                          
         MVC   0(L'SCRAMT3,R4),SCRAMT3                                          
*                                                                               
SCRFMTX  B     EXIT                                                             
*                                                                               
SCRCURE  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         MVC   0(L'SCRAMT1,R3),BCSPACES                                         
         CURED (P8,0(R2)),(L'SCRAMT1,(R3)),M.TLMCTAB,MINUS=YES                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST FOR VALIDATE LINE                                             *         
***********************************************************************         
         SPACE 1                                                                
VALFRST  XC    MAPCT,MAPCT         RESET PERCENT ENTERED FOR COMMISSION         
         NI    MINDS,FF-(MINET+MICOM)                                           
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE COLUMN                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALCLM   LR    R3,R1                                                            
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         TM    LSINDS1,LSICLMIN    TEST FIRST TIME                              
         BO    VALCLM02                                                         
         BAS   RE,SETLST                                                        
*                                                                               
VALCLM02 SLL   R3,2                                                             
         B     *+4(R3)                                                          
         B     EXITN               WORKCODE                                     
         B     EXITN               SUPPLIER/CONTRA                              
         B     EXITN               TRANSACTION REFERENCE                        
         B     VALNETM             NET MATCHED                                  
         B     EXITN               NET AVAILABLE                                
         B     VALCOMM             COMMISSION MATCHED                           
         B     EXITN               NET MATCHED (AGENCY)                         
         B     EXITN               NET AVAILABLE (AGENCY)                       
         B     EXITN               COMMISSION MATCHED (AGENCY)                  
         B     EXITN               COMMISSION RATE                              
         B     EXITN               MATCH EXCHANGE RATE                          
         B     EXITN               INVOICE EXCHANGE RATE                        
         SPACE 1                                                                
***********************************************************************         
* VALIDATE MATCH AMOUNT                                               *         
***********************************************************************         
         SPACE 1                                                                
VALNETM  CLI   FVILEN,0                                                         
         BE    EXITY                                                            
         OI    MINDS,MINET                                                      
*                                                                               
         MVC   FVMSGNO,=AL2(AE$INVAM)                                           
         MVC   BOBYTE1,M.TLMCTAB+(CURTDECP-CURTABD)                             
         OI    BOBYTE1,X'80'                                                    
         XR    R3,R3                                                            
         IC    R3,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(R3))                    
         CLI   0(R1),FF                                                         
         BE    EXITN               INPUT MUST BE STRAIGHT CASH                  
*                                                                               
         ZAP   MNET,4(8,R1)                                                     
* ??     ZAP   BODUB1,SVNETAB      TEST LEAVES >=0 FOR MATCHING                 
* ??     AP    BODUB1,TLINMB                                                    
* ??     CP    MNET,BODUB1                                                      
* ??     BH    EXITN                                                            
         CP    TLINAB,BCPZERO      TEST POSTITIVE VALUES REQUIRED               
         BL    VNETM02                                                          
         CP    MNET,BCPZERO        TEST >0                                      
         BL    EXITN                                                            
         CP    MNET,TLINAB         TEST <= AMOUNT AVAILABLE                     
         BH    EXITN                                                            
         B     VNETM04                                                          
VNETM02  CP    MNET,BCPZERO        TEST <0                                      
         BH    EXITN                                                            
         CP    MNET,TLINAB         TEST >= AMOUNT AVAILABLE                     
         BL    EXITN                                                            
*                                                                               
VNETM04  GOTO1 AFNDCLM,BOPARM,=AL1(MT2#CMB)                                     
         BNE   EXITY                                                            
         L     R2,0(R1)                                                         
         USING FHD,R2                                                           
         TM    FHAT,FHATPR                                                      
         BO    EXITY                                                            
         TM    FHII,FHIIVA                                                      
         BZ    EXITY                                                            
         OI    FHOI,FHOITR         DISPLAY DEFAULT PERCENTAGE                   
         NI    FHII,FF-FHIIVA                                                   
         IC    RE,FHLN                                                          
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         EX    RE,*+4                                                           
         MVC   FHDA(0),BCSPACES                                                 
         GOTO1 FMTPER,BOPARM,TLICOMR,FHDA                                       
         MVI   FHDA+7,C'%'                                                      
         B     EXITY                                                            
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE COMMISSION AMOUNT                                          *         
***********************************************************************         
         SPACE 1                                                                
VALCOMM  CLI   FVILEN,0                                                         
         BE    EXITY                                                            
*                                                                               
         OI    MINDS,MICOM                                                      
         MVC   FVMSGNO,=AL2(AE$INVAM)                                           
         MVC   BOBYTE1,M.TLMCTAB+(CURTDECP-CURTABD)                             
         OI    BOBYTE1,X'80'                                                    
         XR    R3,R3                                                            
         IC    R3,FVILEN                                                        
         LA    RE,FVIFLD-1(R3)                                                  
         CLI   0(RE),C'%'          TEST INPUT IS PERCENTAGE                     
         BNE   VCOMM02                                                          
         BCTR  R3,0                                                             
         GOTO1 VCASHVAL,BODMCB,(X'84',FVIFLD),(R3)                              
         CLI   0(R1),FF                                                         
         BE    EXITN                                                            
         ZAP   MPCT,4(8,R1)                                                     
         BZ    VCOMM04                                                          
         MVC   MAPCT,FVADDR                                                     
         B     EXITY                                                            
*                                                                               
VCOMM02  GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(R3))                    
         CLI   0(R1),FF                                                         
         BE    EXITN               INPUT MUST BE STRAIGHT CASH                  
*                                                                               
VCOMM04  ZAP   MCOM,4(8,R1)                                                     
* ??     ZAP   BODUB1,SVCOMAB      TEST LEAVES >=0 FOR MATCHING                 
* ??     AP    BODUB1,TLICMB                                                    
* ??     CP    MCOM,BODUB1                                                      
* ??     BH    EXITN                                                            
         CP    TLINAB,BCPZERO      TEST POSTITIVE VALUES REQUIRED               
         BL    VCOMM06                                                          
         CP    MCOM,BCPZERO        TEST >0                                      
         BL    EXITN                                                            
         B     VCOMM08                                                          
VCOMM06  CP    MCOM,BCPZERO        TEST <0                                      
         BH    EXITN                                                            
VCOMM08  B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR VALIDATE LINE                                              *         
***********************************************************************         
         SPACE 1                                                                
VALLAST  ICM   R2,15,MAPCT         TEST COMMISSION % ENTERED                    
         BZ    VLAST10                                                          
         USING FHD,R2                                                           
         TM    MINDS,MINET                                                      
         BO    *+10                                                             
         ZAP   MNET,TLINMB                                                      
         ZAP   BOPL81(16),MNET                                                  
         MP    BOPL81(16),MPCT                                                  
         AP    BOPL81(16),SVCOMREM                                              
         MVC   BOWORK1(16),BOPL81                                               
         SRP   BOPL81(16),64-6,5                                                
         ZAP   MCOM,BOPL82                                                      
*                                                                               
         CP    SVNETAB,BCPZERO     TEST NET FULLY MATCHED                       
         BNE   VLAST02                                                          
         ZAP   BODUB1,MCOM                                                      
         SP    BODUB1,SVCOMAB                                                   
         BZ    VLAST02                                                          
         CP    BODUB1,PPLUS1       IF COMMISSION IS WITHIN 0.01                 
         BE    *+14                  OF BEING FULLY MATCHED.....                
         CP    BODUB1,PMINUS1                                                   
         BNE   VLAST02                                                          
         ZAP   MCOM,SVCOMAB        .....ADJUST IT SO THAT IT IS                 
*                                                                               
VLAST02  IC    RE,FHLN             DISPLAY CALCULATED FIGURE                    
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         EX    RE,*+4                                                           
         MVC   FHDA(0),BCSPACES                                                 
         GOTO1 FMTBILL,BOPARM,MCOM,FHDA                                         
         OI    FHOI,FHOITR                                                      
* ??     ZAP   BODUB1,MCOM                                                      
* ??     SP    BODUB1,TLICMB                                                    
* ??     CP    BODUB1,SVCOMAB                                                   
* ??     BNH   *+14                                                             
* ??     MVC   FVADDR,MAPCT                                                     
* ??     B     EXITN                                                            
         SRP   BOPL81(16),6,0                                                   
         SP    BOWORK1(16),BOPL81(16)                                           
         ZAP   SVCOMREM,BOWORK1(16)                                             
         DROP  R2                                                               
*                                                                               
VLAST10  TM    MINDS,MINET         TEST NET ENTERED                             
         BZ    VLAST12                                                          
         GOTO1 MATCHNET,0                                                       
VLAST12  TM    MINDS,MICOM         TEST COMMISSION ENTERED                      
         BZ    VLAST14                                                          
         GOTO1 MATCHCOM,0                                                       
VLAST14  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMN                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISCLM   L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     DISWC               WORKCODE                                     
         B     DISSUP              SUPPLIER/CONTRA                              
         B     DISREF              TRANSACTION REFERENCE                        
         B     DISNETM             NET MATCHED                                  
         B     DISNETA             NET AVAILABLE                                
         B     DISCOMM             COMMISSION MATCHED                           
         B     DISNETMA            NET MATCHED (AGENCY)                         
         B     DISNETAA            NET AVAILABLE (AGENCY)                       
         B     DISCOMMA            COMMISSION MATCHED (AGENCY)                  
         B     DISCOMR             COMMISSION RATE (DEFAULT)                    
         B     DISMEXC             MATCH EXCHANGE RATE                          
         B     DISIEXC             INVOICE EXCHANGE RATE                        
         B     DISHRSM             HOURS MATCHED                                
         B     DISHRSA             HOURS AVAILABLE                              
         SPACE 1                                                                
***********************************************************************         
* DISPLAY WORKCODE                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISWC    BAS   RE,SETLST                                                        
         MVC   FVIFLD(L'TRNKWORK),TRNKWORK                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY SUPPLIER/CONTRA                                             *         
***********************************************************************         
         SPACE 1                                                                
DISSUP   MVC   FVIFLD(L'TRNKULC),TRNKULC                                        
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY REFERENCE                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISREF   MVC   FVIFLD(L'TRNKREF),TRNKREF                                        
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY NET MATCHED                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISNETM  TM    TLIINDS,TLIIREV                                                  
         BO    DISNETM2                                                         
         CP    TLINAB,BCPZERO                                                   
         BE    DISNETM2                                                         
         CP    SVNETTB,BCPZERO                                                  
         BNE   DISNETM4                                                         
*                                                                               
DISNETM2 OI    FVIHDR+FHATD,FHATPR                                              
         B     DISNETM6                                                         
*                                                                               
DISNETM4 CP    TLINAB,TLINMB                                                    
         BNE   *+8                                                              
         OI    FVIHDR+FHATD,FHATHI                                              
*                                                                               
DISNETM6 GOTO1 FMTBILL,BOPARM,TLINMB,FVIFLD                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY NET AVAILABLE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISNETA  GOTO1 FMTBILR,BOPARM,TLINAB,FVIFLD                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY COMMISSION MATCHED                                          *         
***********************************************************************         
         SPACE 1                                                                
DISCOMM  TM    TLIINDS,TLIIREV                                                  
         BO    DISCOMM2                                                         
         CP    TLINAB,BCPZERO                                                   
         BE    DISCOMM2                                                         
         CP    SVCOMTB,BCPZERO                                                  
         BNE   DISCOMM4                                                         
*                                                                               
DISCOMM2 OI    FVIHDR+FHATD,FHATPR                                              
*                                                                               
DISCOMM4 GOTO1 FMTBILL,BOPARM,TLICMB,FVIFLD                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY NET MATCHED (AGENCY CURRENCY)                               *         
***********************************************************************         
         SPACE 1                                                                
DISNETMA GOTO1 FMTAGY,BOPARM,TLINMA,FVIFLD                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY NET AVAILABLE (AGENCY CURRENCY)                             *         
***********************************************************************         
         SPACE 1                                                                
DISNETAA GOTO1 FMTAGY,BOPARM,TLINAA,FVIFLD                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY COMMISSION MATCHED (AGENCY CURRENCY)                        *         
***********************************************************************         
         SPACE 1                                                                
DISCOMMA GOTO1 FMTAGY,BOPARM,TLICMA,FVIFLD                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY COMMISSION RATE (DEFAULT)                                   *         
***********************************************************************         
         SPACE 1                                                                
DISCOMR  GOTO1 FMTPER,BOPARM,TLICOMR,FVIFLD                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY MATCH EXCHANGE RATE                                         *         
***********************************************************************         
         SPACE 1                                                                
DISMEXC  LA    R2,M.TLMXEXC                                                     
         B     DISEXC                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY INVIOCE EXCHANGE RATE                                       *         
***********************************************************************         
         SPACE 1                                                                
DISIEXC  TM    TLIINDS,TLIISFC                                                  
         BZ    EXIT                                                             
         LA    R2,TLIXEXC                                                       
         SPACE 1                                                                
DISEXC   ZAP   BODUB1,BCPZERO                                                   
         MVO   BODUB1,0(5,R2)                                                   
         CURED (P8,BODUB1),(11,FVIFLD),5,DMCB=BODMCB                            
         LA    RF,FVIFLD+10                                                     
         LA    R0,4                                                             
DISEXC02 CLI   0(RF),C'0'                                                       
         BNE   EXIT                                                             
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         BCT   R0,DISEXC02                                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY HOURS MATCHED                                               *         
***********************************************************************         
         SPACE 1                                                                
DISHRSM  GOTO1 FMTHRS,BOPARM,TLIHRM,FVIFLD                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY HOURS AVAILABLE                                             *         
***********************************************************************         
         SPACE 1                                                                
DISHRSA  GOTO1 FMTHRS,BOPARM,TLIHRA,FVIFLD                                      
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
LSTFRST  BAS   RE,CLRTOTS                                                       
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,BCJOBCOD                                                 
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* GET FIRST/NEXT RECORD FOR LIST                                      *         
***********************************************************************         
         SPACE 1                                                                
GETFRST  LA    R1,IOHIGH+IOACCDIR                                               
         B     *+8                                                              
GETNEXT  LA    R1,IOSEQ+IOACCDIR                                                
         GOTO1 AIO                                                              
         BNE   EXITN                                                            
         CLC   TRNKEY(TRNKWORK-TRNKEY),IOKEYSAV                                 
         BNE   EXITN                                                            
         CLC   TRNKWORK,REVWC                                                   
         BNL   EXITN                                                            
         CLC   TRNKWORK,ORDWC                                                   
         BE    GETNEXT                                                          
         OC    TRNKDATE,TRNKDATE   ENSURE HAVE TRANSACTION RECORD               
         BZ    GETNEXT                                                          
         CLC   TRNKDATE,BCSPACES                                                
         BE    GETNEXT                                                          
         TM    TRNKSTAT,TRNSREVS+TRNSDRFT                                       
         BNZ   GETNEXT             EXCLUDE DRAFTS/REVERSALS                     
         GOTO1 ASETTRN,BOPARM,(C'D',TRNRECD)                                    
         BNE   GETNEXT                                                          
*                                                                               
         MVC   IODAOVER,TRNKDA                                                  
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
*                                                                               
         GOTO1 ASETTRN,BOPARM,(C'M',TRNRECD)                                    
         BNE   GETNEXT                                                          
         CP    PM$ANVBL,BCPZERO                                                 
         BE    GETNEXT                                                          
         CLC   PG$BLCUR,BCSPACES                                                
         BNH   GNEXT02                                                          
         CLC   PG$BLCUR,M.TLMCUR                                                
         BE    GNEXT02                                                          
         CP    PA$NETBL,BCPZERO                                                 
         BNE   GETNEXT                                                          
         CP    PA$COMBL,BCPZERO                                                 
         BNE   GETNEXT                                                          
*                                                                               
GNEXT02  CP    SVHOURT,BCPZERO     TEST ANY HOURS ON ADVANCE                    
         BE    GNEXT04                                                          
         CP    PM$HRVBL,BCPZERO    THEN ONLY LIST TIMESHEETS                    
         BE    GETNEXT                                                          
*                                                                               
GNEXT04  ZAP   TLINMB,BCPZERO                                                   
         ZAP   TLINMA,BCPZERO                                                   
         ZAP   TLICMB,BCPZERO                                                   
         ZAP   TLICMA,BCPZERO                                                   
         ZAP   TLIHRM,BCPZERO                                                   
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         MVC   TLIINCAC,GOINCAC-GOBBLOCK(RF)                                    
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECTION                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   SLL   R1,2                                                             
         B     *(R1)                                                            
         B     VALCLR              CLEAR                                        
         B     VALYES              YES                                          
*                                                                               
VALCLR   ZAP   BODUB1,BCPZERO                                                   
         GOTO1 AFNDCLM,BOPARM,=AL1(MT2#NMB)                                     
         BNE   VCLR02              FIND NET MATCHED COLUMN                      
         L     R2,0(R1)                                                         
         USING FHD,R2                                                           
         TM    FHAT,FHATPR                                                      
         BZ    VYES02                                                           
VCLR02   GOTO1 AFNDCLM,BOPARM,=AL1(MT2#CMB)                                     
         BNE   EXITN               FIND COMM. MATCHED COLUMN                    
         L     R2,0(R1)                                                         
         TM    FHAT,FHATPR                                                      
         BZ    VYES02                                                           
         B     EXITN                                                            
*                                                                               
VALYES   ZAP   BODUB1,TLINAB                                                    
*                                                                               
         GOTO1 AFNDCLM,BOPARM,=AL1(MT2#NMB)                                     
         BNE   EXITN               FIND NET MATCHED COLUMN                      
         L     R2,0(R1)                                                         
*                                                                               
VYES02   OI    FHOI,FHOITR         DISPLAY AMOUNT                               
         NI    FHII,FF-FHIIVA                                                   
         IC    RE,FHLN                                                          
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         EX    RE,*+4                                                           
         MVC   FHDA(0),BCSPACES                                                 
         GOTO1 FMTBILL,BOPARM,BODUB1,FHDA                                       
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUITNE TO GET MATCH RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETMTC   NTR1  ,                                                                
         MVC   IODAOVER,M.TLDA                                                  
         L     R1,=A(IOMCH)                                                     
         LA    R1,IOACCMST+IOGET(R1)                                            
         CLI   BCPFKEY,PFKMUPDQ    TEST UPDATING                                
         BNE   *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZAP   SVNETTB,BCPZERO                                                  
         ZAP   SVCOMTB,BCPZERO                                                  
         ZAP   SVHOURT,BCPZERO                                                  
         ZAP   SVNETTA,BCPZERO                                                  
         ZAP   SVCOMTA,BCPZERO                                                  
*                                                                               
         L     R3,AIOMCH                                                        
         GOTO1 GETCOST,M.TLMINCAC                                               
         MVC   MATCHANC,MANC                                                    
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         TM    M.TLMINDS,TLMIORD   TEST AN ORDER                                
         BZ    GETMTC02            YES - MAY BE PARTLY MATCHED                  
         GOTO1 AMCHORD,BOPARM,(C'I',(R3)),AIO1                                  
         L     R3,AIO1                                                          
GETMTC02 GOTO1 GETPTA,(R3)                                                      
         BNE   GETMTCX                                                          
         USING PTAELD,R1                                                        
         ZAP   SVNETTA,PTANET                                                   
         ZAP   SVCOMTA,PTARCOM                                                  
         ZAP   SVNETTB,PTANET                                                   
         ZAP   SVCOMTB,PTARCOM                                                  
         LH    RE,PTAHOURS                                                      
         CVD   RE,BODUB1                                                        
         ZAP   SVHOURT,BODUB1                                                   
         TM    MINDS,MIFCB                                                      
         BZ    GETMTCX                                                          
         ZAP   SVNETTB,PTANETF                                                  
         ZAP   SVCOMTB,PTARFCOM                                                 
         DROP  R1                                                               
*                                                                               
GETMTCX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET PTA ELEMENT FOR BILL                                 *         
*                                                                     *         
* NTRY: R1 = A(FIRST ELEMENT)                                         *         
* EXIT: R1 = A(PTA ELEMENT)                                           *         
***********************************************************************         
         SPACE 1                                                                
GETPTA   XR    RF,RF                                                            
         USING PTAELD,R1                                                        
GPTA02   CLI   PTAEL,0                                                          
         BE    GETPTAN                                                          
         CLI   PTAEL,PTAELQ        FIND UPDATED ALLOCATION                      
         BNE   GPTA08                                                           
         CLI   PTATYPE,PTATRAL                                                  
         BNE   GPTA08                                                           
         TM    PTASTAT1,PTASPEND                                                
         BO    GPTA08                                                           
         CLC   PTARBLNO,M.TLMBILL  MATCH ON BILL #                              
         BNE   GPTA08                                                           
         CLC   PTARBLDT,M.TLMDATC  MATCH ON BILL DATE                           
         BE    GETPTAY                                                          
GPTA08   IC    RF,PTALN                                                         
         BXH   R1,RF,GPTA02                                                     
*                                                                               
GETPTAY  CR    RE,RE               SET CC = EQUAL                               
         BR    RE                                                               
*                                                                               
GETPTAN  XR    R1,R1                                                            
         LTR   RE,RE               SET CC = NO                                  
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* MATCH NET AMOUNT                                                    *         
*                                                                     *         
* NTRY:      R1 = 0 TO UPDATE MATCHED AND AVAILABLE TOTALS            *         
*            R1 = NON-0 TO UPDATE MATCHED TOTALS ONLY                 *         
*          MNET = NET AMOUNT BILLED IN BILLING CURRENCY               *         
* EXIT: TLINMB = NET AMOUNT BILLED IN BILLING CURRENCY                *         
*       TLINMA = NET AMOUNT BILLED IN AGENCY CURRENCY                 *         
*       SVNETAB,SVNETMB,SVNETAA,SVNETMA TOTALS ARE UPDATED            *         
*       SVHOURA, SVHOURM TOTALS ARE UPDATED                           *         
***********************************************************************         
         SPACE 1                                                                
MATCHNET NTR1  ,                                                                
         LTR   R2,R1                                                            
         BNZ   MNET02                                                           
         AP    SVNETAB,TLINMB      ADJUST TOTALS FOR OLD VALUES                 
         AP    SVNETAA,TLINMA                                                   
         AP    SVHOURA,TLIHRM                                                   
MNET02   SP    SVNETMA,TLINMA                                                   
         SP    SVNETMB,TLINMB                                                   
         SP    SVHOURM,TLIHRM                                                   
*                                                                               
         ZAP   TLINMB,MNET         UPDATE BILLING CURRENCY                      
         ZAP   TLINMA,TLINMB       TEST ZERO                                    
         BZ    MNET06                                                           
         TM    MINDS,MIFCB         TEST SAME CURRENCY                           
         BZ    MNET06                                                           
*                                                                               
         CP    TLINMB,TLINAB       IF INVOICE FULLY MATCHED (BILLING)           
         BNE   *+14                  FULLY MATCH INVIOCE (CURRENCY)             
         ZAP   TLINMA,TLINAA                                                    
         B     MNET04                                                           
*                                                                               
         ZAP   BODUB1,SVNETAA                                                   
         ZAP   BODUB2,SVNETAB                                                   
         BNZ   *+16                                                             
         ZAP   BODUB1,SVNETTA                                                   
         ZAP   BODUB2,SVNETTB                                                   
*                                                                               
         ZAP   BOPL81(16),TLINMB   BOPL81(16)=BILLING AMOUNT                    
         MP    BOPL81(16),BODUB1             *AGENCY AVAILABLE                  
         SRP   BOPL81(16),2,0                *100                               
         DP    BOPL81(16),BODUB2             /BILLING AVAILABLE                 
         SRP   BOPL81,64-2,5                 /100                               
         ZAP   TLINMA,BOPL81                                                    
         BNE   *+10                IF MATCHED(AGENCY)=ZERO                      
         ZAP   TLINMB,BCPZERO        MATCHED(BILLING)=ZERO                      
*                                                                               
MNET04   CP    TLINMA,SVNETAA      IF MATCHED(AGENCY) > AVAILABLE               
         BNH   *+10                  MATCHED(AGENCY)=AVAILABLE                  
         ZAP   TLINMA,SVNETAA                                                   
*                                                                               
MNET06   CP    SVNETAB,TLINMB      IF BILLING FULLY MATCHED                     
         BNE   MNET08                                                           
         CP    SVNETAA,TLINMA        IF AGENCY NOT FULLY MATCHED                
         BE    MNET08                                                           
         ZAP   TLINMA,SVNETAA          FULLY MATCH AGENCY                       
*                                                                               
MNET08   DS    0H                  ESTABLISH HOURS IN PROPORTION TO NET         
         CP    MNET,BCPZERO                                                     
         BE    MNET10                                                           
         CP    SVHOURT,BCPZERO                                                  
         BE    MNET10                                                           
         ZAP   BOPL81(16),TLINMA             AGENCY NET MATCHED                 
         SRP   BOPL81(16),2,0                *100                               
         MP    BOPL81(16),TLIHRA             *HOURS AVAILABLE                   
         DP    BOPL81(16),TLINAA             /AGENCY NET AVAILABLE              
         SRP   BOPL81,64-2,5                 /100                               
         ZAP   TLIHRM,BOPL81                                                    
*                                                                               
MNET10   LTR   R2,R2                                                            
         BNZ   MNET12                                                           
         SP    SVNETAB,TLINMB      ADJUST TOTALS FOR NEW VALUES                 
         SP    SVNETAA,TLINMA                                                   
         SP    SVHOURA,TLIHRM                                                   
MNET12   AP    SVNETMA,TLINMA                                                   
         AP    SVNETMB,TLINMB                                                   
         SP    SVHOURM,TLIHRM                                                   
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* MATCH COMMISSION AMOUNT                                             *         
*                                                                     *         
* NTRY:      R1 = 0 TO UPDATE MATCHED AND AVAILABLE TOTALS            *         
*            R1 = NON-0 TO UPDATE MATCHED TOTALS ONLY                 *         
*          MCOM =  COMMISSION AMOUNT BILLED IN BILLING CURRENCY       *         
* EXIT: TLICMB = COMMISSION AMOUNT BILLED IN BILLING CURRENCY         *         
*       TLICMA = COMMISSION AMOUNT BILLED IN AGENCY CURRENCY          *         
*       SVCOMAB,SVCOMMB,SVCOMAA,SVCOMMA TOTALS ARE UPDATED            *         
***********************************************************************         
         SPACE 1                                                                
MATCHCOM NTR1  ,                                                                
         LTR   R2,R1                                                            
         BNZ   *+16                                                             
         AP    SVCOMAB,TLICMB      ADJUST TOTALS FOR OLD VALUES                 
         AP    SVCOMAA,TLICMA                                                   
         SP    SVCOMMA,TLICMA                                                   
         SP    SVCOMMB,TLICMB                                                   
*                                                                               
         ZAP   TLICMB,MCOM         UPDATE BILLING CURRENCY                      
         ZAP   TLICMA,TLICMB       TEST ZERO                                    
         BZ    MCOM02                                                           
         TM    MINDS,MIFCB         TEST SAME CURRENCY                           
         BZ    MCOM02                                                           
*                                                                               
         ZAP   BODUB1,SVCOMAA                                                   
         ZAP   BODUB2,SVCOMAB                                                   
         BNZ   *+16                                                             
         ZAP   BODUB1,SVCOMTA                                                   
         ZAP   BODUB2,SVCOMTB                                                   
*                                                                               
         ZAP   BOPL81(16),TLICMB   BOPL81(16)=BILLING AMOUNT                    
         MP    BOPL81(16),BODUB1             *AGENCY AVAILABLE                  
         SRP   BOPL81(16),2,0                *100                               
         DP    BOPL81(16),BODUB2             /BILLING AVAILABLE                 
         SRP   BOPL81,64-2,5                 /100                               
         ZAP   TLICMA,BOPL81                                                    
         BNE   *+10                IF MATCHED(AGENCY)=ZERO                      
         ZAP   TLICMB,BCPZERO        MATCHED(BILLING)=ZERO                      
*                                                                               
MCOM02   CP    SVCOMAB,TLICMB      IF BILLING FULLY MATCHED                     
         BNE   MCOM04                                                           
         CP    SVCOMAA,TLICMA        IF AGENCY NOT FULLY MATCHED                
         BE    MCOM04                                                           
         ZAP   TLICMA,SVCOMAA          FULLY MATCH AGENCY                       
*                                                                               
MCOM04   LTR   R2,R2                                                            
         BNZ   *+16                                                             
         SP    SVCOMAB,TLICMB      ADJUST TOTALS FOR NEW VALUES                 
         SP    SVCOMAA,TLICMA                                                   
         AP    SVCOMMB,TLICMB                                                   
         AP    SVCOMMA,TLICMA                                                   
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET UP THINGS IN TLSTD                                   *         
*                                                                     *         
* NTRY: IO1=TRANSACTION RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
SETLST   NTR1  ,                                                                
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         TM    TRNRSTAT,TRNSREVS   TEST RECORD HAS BEEN REVERSED                
         BZ    SETLST10              (I.E. REVALUED)                            
         MVC   IOKEY(L'TRNKEY),TRNKEY                                           
         LA    R1,IOREAD+IOACCDIR                                               
         B     *+8                                                              
SETLST02 LA    R1,IOSEQ+IOACCDIR   FIND REVALUED RECORD                         
         GOTO1 AIO                                                              
         CLC   IOKEY(TRNKSBR-1-TRNRECD),TRNKEY                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    IOKEY+(TRNKSTA-TRNRECD),TRNSREVS                                 
         BO    SETLST02                                                         
         MVC   TLDA,IOKEY+(TRNKDA-TRNRECD)                                      
         MVC   IODAOVER,TLDA                                                    
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SETLST10 L     RE,AGOPBLK                                                       
         ZAP   TLICOMR,GOAGYCOM-GOBLOCK(L'GOAGYCOM,RE)                          
*                                                                               
         GOTO1 VPRORATA,BOPARM,AIO1,AGOPBLK,ACOM,MAEXC,LSPRATA,0                
         ZAP   TLINAA,PM$ANVBL                                                  
         ZAP   TLINAB,PM$ANVBL                                                  
         ZAP   TLIHRA,PM$HRVBL                                                  
         MVI   TLIINDS,0                                                        
         TM    MINDS,MIFCB                                                      
         BZ    SETLSTX                                                          
         ZAP   TLINAB,PM$FNVBL                                                  
*                                                                               
         LA    R3,TRNRFST                                                       
         USING AFCELD,R3           FIND AFCELD ON MATCHER                       
         XR    RF,RF                                                            
SETLST12 CLI   AFCEL,0                                                          
         BE    SETLSTX                                                          
         CLI   AFCEL,AFCELQ                                                     
         BE    *+12                                                             
         IC    RF,AFCLN                                                         
         BXH   R3,RF,SETLST12                                                   
         CLC   AFCCURR,M.TLMCUR    TEST SAME FOREIGN CURRENCY                   
         BNE   SETLSTX                                                          
         OI    TLIINDS,TLIISFC                                                  
         MVC   TLIX,AFCX                                                        
         CLC   TLIXEXC,M.TLMXEXC TEST DIFFERENT EXCHANGE RATE                   
         BE    SETLSTX                                                          
         OI    TLIINDS,TLIIDEX                                                  
         CP    PP$AALLO,BCPZERO    TEST ANYTHING PENDING                        
         BNE   SETLSTX             (YES - CAN'T REVALUE)                        
         CLC   PP$AALLO(PM$VALS-PP$ACOMM),PP$ACOMM                              
         BNE   SETLSTX                                                          
         CP    PA$GRSBL,BCPZERO    TEST ANYTHING BILLED                         
         BNE   SETLSTX             (YES - CAN'T REVALUE)                        
         CP    PA$WOFAM,BCPZERO    TEST ANYTHING WRITTEN OFF                    
         BNE   SETLSTX             (YES - CAN'T REVALUE)                        
         CP    PA$XFRAM,BCPZERO    TEST ANYTHING TRANSFERRED                    
         BNE   SETLSTX             (YES - CAN'T REVALUE)                        
         CLI   P#FREVFI,C'N'       TEST REVALUE TO BE FORCED                    
         BE    SETLSTX                                                          
         OI    TLIINDS,TLIIREV     SET REVALUE REQUIRED                         
         DROP  R3                                                               
*                                                                               
SETLSTX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CLEAR TOTALS                                                        *         
***********************************************************************         
         SPACE 1                                                                
CLRTOTS  ZAP   SVNETMA,BCPZERO                                                  
         ZAP   SVNETMB,BCPZERO                                                  
         ZAP   SVHOURM,BCPZERO                                                  
         ZAP   SVNETAA,SVNETTA                                                  
         ZAP   SVNETAB,SVNETTB                                                  
         ZAP   SVHOURA,SVHOURT                                                  
         ZAP   SVCOMMA,BCPZERO                                                  
         ZAP   SVCOMMB,BCPZERO                                                  
         ZAP   SVCOMAA,SVCOMTA                                                  
         ZAP   SVCOMAB,SVCOMTB                                                  
         ZAP   SVCOMREM,BCPZERO                                                 
         BR    RE                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO FORMAT AMOUNT IN BILLING CURRENCY (LEFT ALIGNED)         *         
*                                                                     *         
* NTRY: P1=PL8(AMOUNT)                                                *         
*       P2=A(OUTPUT)                                                  *         
***********************************************************************         
         SPACE 1                                                                
FMTBILL  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         CURED (P8,(R2)),(14,(R3)),M.TLMCTAB,MINUS=YES,ALIGN=LEFT               
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO FORMAT AMOUNT IN BILLING CURRENCY (RIGHT ALIGNED)        *         
*                                                                     *         
* NTRY: P1=PL8(AMOUNT)                                                *         
*       P2=A(OUTPUT)                                                  *         
***********************************************************************         
         SPACE 1                                                                
FMTBILR  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         CURED (P8,(R2)),(14,(R3)),M.TLMCTAB,MINUS=YES                          
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO FORMAT AMOUNT IN AGENCY CURRENCY (RIGHT ALIGNED)         *         
*                                                                     *         
* NTRY: P1=PL8(AMOUNT)                                                *         
*       P2=A(OUTPUT)                                                  *         
***********************************************************************         
         SPACE 1                                                                
FMTAGY   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         CURED (P8,(R2)),(14,(R3)),CSCURCPY,MINUS=YES                           
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO FORMAT PERCENTAGE                                        *         
*                                                                     *         
* NTRY: P1=PL8(AMOUNT)                                                *         
*       P2=A(OUTPUT)                                                  *         
***********************************************************************         
         SPACE 1                                                                
FMTPER   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         CURED (P8,(R2)),(7,(R3)),4,DMCB=BODMCB                                 
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO FORMAT HOURS (RIGHT ALIGNED)                             *         
*                                                                     *         
* NTRY: P1=PL8(AMOUNT)                                                *         
*       P2=A(OUTPUT)                                                  *         
***********************************************************************         
         SPACE 1                                                                
FMTHRS   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         CURED (P8,(R2)),(14,(R3)),2,MINUS=YES                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST TOTAL MATCHED IN VALID RANGE                        *         
*                                                                     *         
* NTRY: R1 = NON-ZERO TO SET ERROR MESSAGE                            *         
***********************************************************************         
         SPACE 1                                                                
TSTRANGE NTR1  ,                                                                
         CP    SVNETTB,BCPZERO                                                  
         BL    TRAN02                                                           
*                                                                               
         CP    SVNETMB,BCPZERO     TEST NET IN +VE RANGE                        
         BNH   TRANEN                                                           
         CP    SVNETMB,SVNETTB                                                  
         BH    TRANEN                                                           
         B     TRAN04                                                           
*                                                                               
TRAN02   CP    SVNETMB,BCPZERO     TEST NET IN -VE RANGE                        
         BNL   TRANEN                                                           
         CP    SVNETMB,SVNETTB                                                  
         BL    TRANEN                                                           
*                                                                               
TRAN04   CP    SVCOMTB,BCPZERO                                                  
         BL    TRAN06                                                           
*                                                                               
         CP    SVCOMMB,BCPZERO     TEST COMMISSION IN +VE RANGE                 
         BL    TRANEC                                                           
         CP    SVCOMMB,SVCOMTB                                                  
         BH    TRANEC                                                           
         B     TRAN08                                                           
*                                                                               
TRAN06   CP    SVCOMMB,BCPZERO     TEST COMMISSION IN -VE RANGE                 
         BH    TRANEC                                                           
         CP    SVCOMMB,SVCOMTB                                                  
         BL    TRANEC                                                           
*                                                                               
TRAN08   B     EXITY                                                            
*                                                                               
TRANEN   LTR   R1,R1               TEST SET ERROR MESSAGE                       
         BZ    EXITL                                                            
         GOTO1 FMTRANGE,BOPARM,BODUB1,SVNETTB                                   
         MVC   FVMSGNO,=AL2(AE$NMMIR)                                           
         OI    FVCURIND,FVCKEEP                                                 
         B     EXITL                                                            
*                                                                               
TRANEC   LTR   R1,R1               TEST SET ERROR MESSAGE                       
         BZ    EXITL                                                            
         GOTO1 FMTRANGE,BOPARM,BODUB1,SVCOMTB                                   
         MVC   FVMSGNO,=AL2(AE$CMMIR)                                           
         OI    FVCURIND,FVCKEEP                                                 
         B     EXITL                                                            
*                                                                               
FMTRANGE NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         CURED (P8,(R2)),(14,FVPARMS+1),M.TLMCTAB,MINUS=YES,ALIGN=LEFT          
         LR    RF,R0                                                            
         LA    RF,1(RF)                                                         
         STC   RF,FVPARMS                                                       
         LA    R4,FVPARMS(RF)                                                   
         CURED (P8,(R3)),(14,1(R4)),M.TLMCTAB,MINUS=YES,ALIGN=LEFT              
         LR    RF,R0                                                            
         LA    RF,1(RF)                                                         
         STC   RF,0(R4)                                                         
         AR    R4,RF                                                            
         MVI   1(R4),0                                                          
         B     EXIT                                                             
         EJECT                                                                  
FF       EQU   X'FF'                                                            
ACCMST   DC    C'ACCMST '                                                       
REVWC    DC    C'99'                                                            
ORDWC    DC    C'**'                                                            
UL1C     DC    C'1C'                                                            
UL12     DC    C'12'                                                            
ULSI     DC    C'SI'                                                            
ULSK     DC    C'SK'                                                            
PPLUS1   DC    P'1'                                                             
PMINUS1  DC    P'-1'                                                            
DQU      DC    CL(L'BASSRV)'=DQU'                                               
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
DEFCLM   DS    0XL1                                                             
         DC    AL1(MT2#NMB)                                                     
         DC    AL1(MT2#NAB)                                                     
         DC    AL1(MT2#CMB)                                                     
         DC    AL1(MT2#NMA)                                                     
         DC    AL1(MT2#NAA)                                                     
         DC    AL1(MT2#CMA)                                                     
         DC    AL1(MT2#HRSM)                                                    
         DC    AL1(MT2#HRSA)                                                    
         DC    AL1(MT2#COMR)                                                    
         DC    AL1(MT2#MEXC)                                                    
         DC    AL1(MT2#IEXC)                                                    
         DC    AL1(EOT)                                                         
DEFCLML  EQU   *-DEFCLM                                                         
         SPACE 1                                                                
*                                  * REPSUBPG EQUATES *                         
REPSDPST EQU   03                  DRAFT POSTINGS                               
REPSLPST EQU   04                  LIVE POSTINGS                                
*                                                                               
REPSPEC  DS    0X                                                               
         SPROG 3,4                                                              
         SPEC  H1,001,RUN                                                       
         SPEC  H1,073,PAGE                                                      
         SPEC  H3,001,AC#CPY,8,L                                                
         SPEC  H3,061,AC#USRID,10,L                                             
         SPEC  H4,001,AC#OFF,8,L                                                
         SPEC  H4,061,AC#REQR,10,L                                              
         SPEC  H5,001,AC#CLINT,8,L                                              
         SPEC  H6,001,AC#PRO,8,L                                                
         SPEC  H7,001,AC#JOB,8,L                                                
         SPROG 3                                                                
         SPEC  H1,020,AC#DPOS,40,C                                              
         SPEC  H2,020,AC#DPOS,40,CU                                             
         SPROG 4                                                                
         SPEC  H1,020,AC#APOS,40,C                                              
         SPEC  H2,020,AC#APOS,40,CU                                             
*                                                                               
         SPEC  END                                                              
         EJECT                                                                  
***********************************************************************         
* OPTION TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
CLB12    CSECT                                                                  
OPTTAB   DS    0X                                                               
*                                  DISPLAY=COLUMN CODES                         
         DC    AL2(UC8DSP-TWAD,UC3DSP-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,L'LSDISLST,L'LSDISLST)                           
         DC    AL1((*-OPTTAB)/OPTTABL)                                          
         DC    AL2(OPTDISQ,LSDIS-LSVALSD)                                       
         DC    CL4'+'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  GENERAL TRANSACTION OPTIONS                  
         DC    AL2(TRNOPTQ)                                                     
*                                                                               
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
* VALIDATE OPTION INPUT                                             *           
*********************************************************************           
         SPACE 1                                                                
         DROP  R8,RB                                                            
         DS    0D                                                               
VALOPT   NMOD1 0,**VALO**                                                       
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         LR    R4,R2               R4=A(OPTTAB ENTRY)                           
         USING OPTTABD,R4                                                       
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *(RF)                                                            
VALROUTS DS    0XL4                                                             
         SPACE 1                                                                
VALX     XIT1  ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
WORKD    DSECT                                                                  
         ORG   AIO9                                                             
AIOMCH   DS    A                   A(MATCH RECORD)                              
IOMCH    EQU   IO9                                                              
         ORG   AIOA                                                             
AIOINV   DS    A                   A(INVOICE RECORD)                            
IOINV    EQU   IOA                                                              
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF7D                                                       
         SPACE 1                                                                
         ORG   OSVALS                                                           
SVNETTB  DS    PL8                 NET TOTAL (BILLING CURRENCY)                 
SVNETTA  DS    PL8                 NET TOTAL (AGENCY CURRENCY)                  
SVNETAB  DS    PL8                 NET AVAILABLE (BILLING CURRENCY)             
SVNETAA  DS    PL8                 NET AVAILABLE (AGENCY CURRENCY)              
SVNETMB  DS    PL8                 NET MATCHED (BILLING CURRENCY)               
SVNETMA  DS    PL8                 NET MATCHED (AGENCY CURRENCY)                
*                                                                               
SVCOMTB  DS    PL8                 COMMISSION TOTAL (BILLING CURRENCY)          
SVCOMTA  DS    PL8                 COMMISSION TOTAL (AGENCY CURRENCY)           
SVCOMAB  DS    PL8                 COMMISSION AVAILABLE (BILLING)               
SVCOMAA  DS    PL8                 COMMISSION AVAILABLE (AGENCY)                
SVCOMMB  DS    PL8                 COMMISSION MATCHED (BILLING)                 
SVCOMMA  DS    PL8                 COMMISSION MATCHED (AGENCY)                  
*                                                                               
SVCOMREM DS    PL8                 COMMISION REMAINDER                          
*                                                                               
SVHOURT  DS    PL8                 HOURS TOTAL                                  
SVHOURA  DS    PL8                 HOURS AVAILABLE                              
SVHOURM  DS    PL8                 HOURS MATCHED                                
*                                                                               
*SDATC   DS    XL2                                                              
*SDATP   DS    PL3                                                              
OSREF#   DS    CL4                                                              
OSMONP   DS    PL2                                                              
OSMONC   DS    CL2                                                              
         ORG  OSVALS+OSVALSL                                                    
*                                                                               
TLSTD    DSECT                     TEAR RECORD FOR ACCOUNT CODE/NAME            
         ORG   TLKSES                                                           
TLKNAMQ  EQU   X'F1'                                                            
         ORG   TLKSRT                                                           
TLKACT   DS    CL14                ACCOUNT CODE                                 
         ORG   TLDATA                                                           
TLDNAM   DS    CL36                ACCOUNT NAME                                 
TLNAMLNQ EQU   *-TLREC                                                          
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBCOLS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBCOLS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
***********************************************************************         
* LOCAL W/S                                                           *         
***********************************************************************         
         SPACE 1                                                                
MWORKD   DSECT                                                                  
DMCB     DS    6A                                                               
*                                                                               
MLSOPS   DS    XL(LSOPL)           SAVED FILTERS                                
*                                                                               
MAEXC    DS    A                   A(EXCHANGE RATE RULE)                        
MAPCT    DS    A                   A(COMMISION FIELD IF % ENTERED)              
MPCT     DS    PL8                 % ENTERED IN COMMISSION FIELD                
MNET     DS    PL8                 NET AMOUNT FOR MATCHING                      
MCOM     DS    PL8                 COMMISSION AMOUNT FOR MATCHING               
*                                                                               
MINDS    DS    XL1                 INDICATOR BYTE                               
MIFCB    EQU   X'80'               FOREIGN CURRENCY BIILL                       
MINET    EQU   X'40'               INPUT TO NET COLUMN                          
MICOM    EQU   X'20'               INPUT TO COMMISSION COLUMN                   
MIDEL    EQU   X'10'               ADVANCE TRANSACTION WAS DELETED              
MILIVE   EQU   X'08'               LIVE POSTINGS REQUIRED                       
MIDRAFT  EQU   X'04'               DRAFT POSITNGS REQUIRED                      
MIPOST   EQU   X'02'               POSTINGS MADE                                
*                                                                               
MINCAC   DS    CL15                CURRENTLY SAVED INCOME ACCOUNT               
MANC     DS    CL12                ANALYSIS CODE                                
*                                                                               
MATCHANC DS    CL12                MATCH ANALYSIS CODE                          
*                                                                               
SKULACT  DS    0CL14                                                            
SKUL     DS    CL2                                                              
SKACT    DS    CL12                                                             
*                                                                               
BTHALF   DS    H                                                                
BTPARM   DS    8A                                                               
         ORG   BTPARM                                                           
BTPARM1  DS    A                                                                
BTPARM2  DS    A                                                                
BTPARM3  DS    A                                                                
BTPARM4  DS    A                                                                
BTPARM5  DS    A                                                                
BTPARM6  DS    A                                                                
BTPARM7  DS    A                                                                
BTPARM8  DS    A                                                                
*                                                                               
SCRAMT1  DS    CL14                                                             
SCRAMT2  DS    CL14                                                             
SCRAMT3  DS    CL14                                                             
SCRNET1  DS    CL14                                                             
SCRNET2  DS    CL14                                                             
SCRNET3  DS    CL14                                                             
SCRCOM1  DS    CL14                                                             
SCRCOM2  DS    CL14                                                             
SCRCOM3  DS    CL14                                                             
*                                                                               
MPOSTVAL DS    XL(POSTVALL)                                                     
*                                                                               
SCANBYTE DS    CL1                                                              
SCANBLK  DS    5CL(SCBLKLQ)                                                     
*                                                                               
         ORG   MWORKD+OVERWRKL                                                  
         SPACE 1                                                                
HLINE    DSECT                     ** HEAD LINE **                              
         ORG   HLINE+01                                                         
HWORD1   DS    CL08,CL1                                                         
HDATA1   DS    CL08,CL1                                                         
HNAME1   DS    CL36                                                             
         ORG   HLINE+61                                                         
HWORD2   DS    CL10,CL1                                                         
HDATA2   DS    CL08                                                             
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'141ACCLB12   08/16/00'                                      
         END                                                                    
