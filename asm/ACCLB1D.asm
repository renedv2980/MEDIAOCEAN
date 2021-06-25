*          DATA SET ACCLB1D    AT LEVEL 083 AS OF 08/16/00                      
*PHASE T6211DA                                                                  
CLB1D    TITLE '- BILL PROGRAM - JOB/LIST'                                      
CLB1D    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB1D**,R7,R6,RR=RE                                           
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK                                                      
         USING LWORKD,RC           RC=A(LOCAL WORKIN STORAGE)                   
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING PRORATAD,LSPRATA                                                 
         LH    R8,=Y(BSDICT-TWAD)                                               
         LA    R8,TWAD(R8)                                                      
         USING BSDICT,R8                                                        
         ST    RE,BORELO                                                        
*                                                                               
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     LSTFRST             FIRST FOR THIS LIST                          
         B     SCRFRST             FIRST FOR THIS SCREEN                        
         B     SCRLAST             LAST FOR THIS SCREEN                         
         B     EXITY               FIRST FOR VALIDATE THIS LINE                 
         B     EXITY               VALIDATE COLUMN                              
         B     EXITY               LAST FOR VALIDATE THIS LINE                  
         B     DISCLM              DISPLAY COLUMN                               
         B     GETFRST             GET FIRST RECORD FOR LIST                    
         B     GETNEXT             GET NEXT RECORD                              
         B     EXITY               SET UP MY OWN HEADING                        
         B     VALSEL              VALIDATE LINE SELECTION                      
         B     DISCLM              DISPLAY (SUB-) TOTAL ROUTINE                 
         B     EXITY               DISPLAY SCREEN TOTAL ROUTINE                 
         B     EXITY               PF KEY ROUTINES                              
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
SCRFRST  DS    0H                                                               
         TM    BCINDS1,BCINACT     TEST FIRST TIME                              
         BZ    SFRST10                                                          
         CLI   BCCLICOD,C' '       TEST ALREADY HAVE CLIENT CODE                
         BNH   SFRST10                                                          
         MVC   BASCLI,BCCLICOD     YES - PREFIL CLIENT FIELD                    
         OI    BASCLIH+FHOID,FHOITR                                             
*                                                                               
SFRST10  MVC   SVOPS,LSOPS                                                      
         GOTO1 VALCLI                                                           
         BNE   EXITL                                                            
         GOTO1 VALPRO                                                           
         BNE   EXITL                                                            
         GOTO1 VALJOB                                                           
         BNE   EXITL                                                            
*                                                                               
         L     RE,=A(VALOPT)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,ALSVALS                                                 
*                                                                               
         XC    BOWORK2,BOWORK2                                                  
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         OC    BOWORK2(L'GOCBDLST),GOCBDLST-GOBBLOCK(RF)                        
         BNZ   *+10                                                             
         MVC   BOWORK2(DEFCLML),DEFCLM                                          
*                                                                               
         GOTO1 AVALOPT,BOPARM,OPTTAB,BOWORK2,0                                  
         BNE   EXITL                                                            
         CLC   SVOPS,LSOPS         TEST CHANGE IN OPTIONS                       
         BNE   EXITH                                                            
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST                                                      *         
* - BUILD KEY FOR FIRST RECORD TO BE READ FOR TSAR LIST               *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING ACTRECD,IOKEY                                                    
LSTFRST  MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   KEY,BCSPACES                                                     
         MVC   KEYCLI,LSCLI                                                     
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* GET FIRST/NEXT RECORD FOR TSAR LIST                                 *         
***********************************************************************         
         SPACE 1                                                                
GETFRST  MVI   LSTINDS,0                                                        
*                                                                               
GETNEXT  TM    LSTINDS,LSTISEQ     TEST DO SEQUENTIAL READ                      
         BZ    GNEXT02                                                          
         NI    LSTINDS,FF-LSTISEQ                                               
         LA    R1,IOSEQ+IOACCDIR+IO1                                            
         B     GNEXT10                                                          
*                                                                               
GNEXT02  GOTO1 KEYIN,ACTKEY        SET KEY FROM CLIENT/PRODUCT/JOB              
         LA    R1,IOHIGH+IOACCDIR+IO1                                           
*                                                                               
GNEXT10  GOTO1 AIO                                                              
         BNE   GETNEXTN                                                         
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEYSAV                                  
         BNE   GETNEXTN                                                         
         GOTO1 KEYOUT,ACTKEY       EXTRACT CLIENT/PRODUCT/JOB                   
*                                                                               
         CLC   KEYCLI,LSCLI        TEST SAME CLIENT                             
         BNE   GETNEXTN                                                         
         CLC   KEYPRO,LSPROMAX     TEST HIGHER THAN PRODUCT MAX                 
         BH    GETNEXTN                                                         
         CLC   KEYJOB,LSJOBMAX     TEST HIGHER THAN JOB MAX                     
         BNH   GNEXT12                                                          
         TM    LSPROIND,LSPROISI   YES - TEST SINGLE PRODUCT                    
         BO    GETNEXTN                                                         
         MVI   KEYJOB,FF           GET NEXT PRODUCT                             
         B     GETNEXT                                                          
*                                                                               
GNEXT12  CLI   KEYJOB,C' '         TEST JOB                                     
         BE    GNEXT14                                                          
         GOTO1 GETJOB                                                           
         BE    GETNEXTY                                                         
         B     GETNEXT                                                          
*                                                                               
GNEXT14  LA    RF,GETPRO           TEST PRODUCT/CLIENT                          
         CLI   KEYPRO,C' '                                                      
         BNE   *+8                                                              
         LA    RF,GETCLI                                                        
         BASR  RE,RF                                                            
         BE    GETNEXT                                                          
         BNE   GETNEXTN                                                         
*                                                                               
GETNEXTY DS    0H                                                               
         B     EXITY                                                            
*                                                                               
GETNEXTN DS    0H                                                               
         BAS   RE,PUTPRO           PUT BACK CLIENT/PRODUCT TOTALS               
         BAS   RE,PUTCLI                                                        
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* ADD CLIENT TO LIST                                                  *         
***********************************************************************         
         SPACE 1                                                                
GETCLI   NTR1  ,                                                                
         TM    ACTKSTAT,ACTSLOCK+ACTSCLOS                                       
         BNZ   GETCLIN                                                          
         BAS   RE,GETACT                                                        
         BNE   GETCLIN                                                          
         MVC   LCLIOFFC,ACOFFC                                                  
*                                                                               
         TM    LSPROIND,LSPROISI   TEST SINGLE PRODUCT                          
         BO    GETCLIY             YES - DON'T NEED CLIENT TOTAL                
*                                                                               
C        USING TLSTD,CLITSAR                                                    
         MVI   C.TLJLEV,TLJLCLIQ     SET UP TSAR RECORD                         
         OI    C.TLINDS1,TLITOTL                                                
         GOTO1 INITSAR,C.TLSTD                                                  
         XR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         LA    RE,C.TLKJOB(RE)                                                  
         MVI   0(RE),FF                                                         
         OI    LSTINDS,LSTICLI     SET HAVE CLIENT RECORD                       
*                                                                               
GETCLIY  CLI   LSPROMIN,C' '       TEST PRODUCT HAS MINUMUM VALUE               
         BH    *+12                                                             
         OI    LSTINDS,LSTISEQ     NO - DO READ SEQUENTIAL NEXT                 
         B     *+10                                                             
         MVC   KEYPRO,LSPROMIN     YES - READ HIGH FOR FIRST PRODUCT            
         B     EXITY                                                            
*                                                                               
GETCLIN  B     EXITN               FINISHED                                     
         SPACE 1                                                                
         DROP  C                                                                
         SPACE 1                                                                
***********************************************************************         
* ADD PRODUCT TO LIST                                                 *         
***********************************************************************         
         SPACE 1                                                                
GETPRO   NTR1  ,                                                                
         TM    ACTKSTAT,ACTSLOCK+ACTSCLOS                                       
         BNZ   GETPRON                                                          
         BAS   RE,GETACT                                                        
         BNE   GETPRON                                                          
         MVC   LPROOFFC,ACOFFC                                                  
*                                                                               
         BAS   RE,PUTPRO           PUT BACK PREVIOUS PRODUCT TOTAL              
*                                                                               
P        USING TLSTD,PROTSAR                                                    
         MVI   P.TLJLEV,TLJLPROQ   SET UP TSAR RECORD                           
         OI    P.TLINDS1,TLITOTL                                                
         GOTO1 INITSAR,P.TLSTD                                                  
         XR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         LA    RE,P.TLKJOB(RE)                                                  
         MVI   0(RE),FF                                                         
*                                                                               
         CLI   LSJOBMIN,C' '       TEST JOB HAS A MINIMUM                       
         BH    *+12                                                             
         OI    LSTINDS,LSTISEQ     NO - DO READ SEQUENTIAL NEXT                 
         B     GETPROY                                                          
         MVC   KEYJOB,LSJOBMIN     YES - READ HIGH FOR FIRST JOB                
*                                                                               
GETPROY  OI    LSTINDS,LSTIPRO     SET HAVE PRODUCT RECORD                      
         B     EXITY                                                            
*                                                                               
GETPRON  MVI   KEYJOB,FF           SET KEY TO GET NEXT PRODUCT                  
         B     EXITY                                                            
         DROP  P                                                                
         SPACE 1                                                                
***********************************************************************         
* ADD JOB TO LIST                                                     *         
***********************************************************************         
         SPACE 1                                                                
GETJOB   NTR1  ,                   * JOB RECORD *                               
         TM    ACTKSTAT,ACTSCLOS                                                
         BO    GETJOBN                                                          
         BAS   RE,GETACT                                                        
         BNE   GETJOBN                                                          
*                                                                               
         MVC   LJOBOFFC,ACOFFC     SET OFFICE CODE                              
         CLC   LJOBOFFC,BCSPACES                                                
         BNE   *+10                                                             
         MVC   LJOBOFFC,LPROOFFC                                                
         CLC   LJOBOFFC,BCSPACES                                                
         BNE   *+10                                                             
         MVC   LJOBOFFC,LCLIOFFC                                                
*                                                                               
         MVI   TLJLEV,TLJLJOBQ     SET UP TSAR RECORD                           
         GOTO1 INITSAR,TLSTD                                                    
         GOTO1 SETJOB,BOPARM,TLSTD,AIO1                                         
*                                                                               
         TM    LSTINDS,LSTIPRO     TEST HAVE PRODUCT TOTAL                      
         BZ    GETJOBX                                                          
         GOTO1 UPDTOT,BOPARM,PROTSAR,TLSTD,0                                    
         OI    LSTINDS,LSTIPROY                                                 
         TM    LSTINDS,LSTICLI     TEST HAVE CLIENT TOTAL                       
         BZ    GETJOBX                                                          
         GOTO1 (RF),(R1),CLITSAR,,                                              
         OI    LSTINDS,LSTICLIY                                                 
*                                                                               
GETJOBX  MVI   ACTKACT+L'ACTKACT,FF SET TO GET NEXT JOB                         
         B     EXITY                                                            
*                                                                               
GETJOBN  MVI   ACTKACT+L'ACTKACT,FF SET TO GET NEXT JOB                         
         B     EXITN                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO INITIALIZE TSAR RECORD                                   *         
*                                                                     *         
* NTRY: R1 = A(TSAR RECORD)                                           *         
***********************************************************************         
         SPACE 1                                                                
INITSAR  NTR1  ,                                                                
         LR    R3,R1                                                            
X        USING TLSTD,R3                                                         
         MVC   X.TLRLEN,=AL2(TLJOBLNQ)                                          
         MVC   X.TLKSES,TWASESNL                                                
         MVC   X.TLKJOB,ACTKACT                                                 
         MVC   X.TLDA,ACTKDA                                                    
         LA    R0,TLJAMTSN                                                      
         LA    RF,X.TLJAMTS                                                     
         ZAP   0(L'TLJAMTS,RF),BCPZERO                                          
         LA    RF,L'TLJAMTS(RF)                                                 
         BCT   R0,*-10                                                          
         B     EXITY                                                            
         DROP  X                                                                
         SPACE 1                                                                
***********************************************************************         
* GET CLIENT/PRODUCT/JOB AND TEST SECURITY                            *         
***********************************************************************         
         SPACE 1                                                                
GETACT   NTR1  ,                                                                
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1                                                          
         LA    RF,ACTRFST-ACTRECD(RF)                                           
         GOTO1 AGETELS,BOPARM,(RF),0                                            
         L     RF,AOFFBLK                                                       
         OI    OFFACTRL-OFFALD(RF),OFFACCNV                                     
         GOTO1 ATSTSEC                                                          
         B     EXIT                                                             
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* PUT SAVED CLIENT TSAR RECORD BACK                                   *         
***********************************************************************         
         SPACE 1                                                                
PUTCLI   NTR1  ,                                                                
         TM    LSTINDS,LSTICLI+LSTICLIY                                         
         BNO   PUTCLIX                                                          
         GOTO1 ATSARIO,BOPARM,('TSAADD',CLITSAR)                                
PUTCLIX  NI    LSTINDS,FF-LSTICLI-LSTICLIY                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* PUT SAVED PRODUCT TSAR RECORD BACK                                  *         
***********************************************************************         
         SPACE 1                                                                
PUTPRO   NTR1  ,                                                                
         TM    LSTINDS,LSTIPRO+LSTIPROY                                         
         BNO   PUTPROX                                                          
         GOTO1 ATSARIO,BOPARM,('TSAADD',PROTSAR)                                
PUTPROX  NI    LSTINDS,FF-LSTIPRO-LSTIPROY                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LAST FOR SCREEN                                                     *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST  TM    SVINDS,SVIALC       TEST IF ALLOCATE SCREEN CALLED               
         BZ    *+12                                                             
         NI    SVINDS,FF-SVIALC    YES - RESET FLAG AND EXIT                    
         B     EXITY                                                            
*                                                                               
         MVC   BCCLICOD,SVENTCLI   RESTORE ENTRY VALUES                         
         MVC   BCPROCOD,SVENTPRO                                                
         MVC   BCJOBCOD,SVENTJOB                                                
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMN                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISCLM   L     R2,AIO1                                                          
         USING ACTRECD,R2                                                       
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     DISCOD              JOB CODE                                     
         B     DISNAM              JOB NAME                                     
         B     DISBAL              JOB BALANCE                                  
         B     DISCRS              JOB CREDITS                                  
         B     DISDRS              JOB DEBITS                                   
         B     DISNET              ALLOCATED NET                                
         B     DISCOM              ALLOCATED COMMISSION                         
         B     DISWOFT             WRITE-OFFS (TIME)                            
         B     DISWOFC             WRITE-OFFS (COST)                            
         B     DISRECT             WRITE-OFF RECOVERIS (TIME)                   
         B     DISRECC             WRITE-OFF RECOVERIES (COST)                  
         B     DISXFR              TRANSFERS                                    
         B     DISINT              INTERNAL INVOICE (PENDING)                   
         SPACE 1                                                                
***********************************************************************         
* DISPLAY JOB CODE                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISCOD   MVC   FVIFLD(L'ACTKACT),ACTKACT                                        
         GOTO1 UPDJOB,BOPARM,TLSTD,ACTRECD                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY JOB NAME                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISNAM   XR    R0,R0                                                            
         LA    RF,ACTRFST                                                       
         USING NAMELD,RF                                                        
DISNAM02 CLI   NAMEL,0                                                          
         BE    EXIT                                                             
         CLI   NAMEL,NAMELQ                                                     
         BNE   DISNAM04                                                         
         XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         EX    RE,*+4                                                           
         MVC   FVIFLD(0),NAMEREC                                                
         B     EXIT                                                             
DISNAM04 IC    R0,NAMLN                                                         
         AR    RF,R0                                                            
         B     DISNAM02                                                         
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY JOB BALANCE                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISBAL   ZAP   BODUB1,TLJDR        SHOW BALANCE                                 
         SP    BODUB1,TLJCR                                                     
         CURED BODUB1,(15,FVIFLD),2,MINUS=YES                                   
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY JOB CREDITS                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISCRS   CURED TLJCR,(15,FVIFLD),2,MINUS=YES                                    
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY JOB DEBITS                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISDRS   CURED TLJDR,(15,FVIFLD),2,MINUS=YES                                    
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY NET AMOUNT                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISNET   DS    0H                                                               
         CURED TLJNET,(15,FVIFLD),2,MINUS=YES                                   
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY COMMISSION AMOUNT                                           *         
***********************************************************************         
         SPACE 1                                                                
DISCOM   DS    0H                                                               
         CURED TLJCOM,(15,FVIFLD),2,MINUS=YES                                   
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY WRITE-OFFS (TIME)                                           *         
***********************************************************************         
         SPACE 1                                                                
DISWOFT  DS    0H                                                               
         CURED TLJWOFT,(15,FVIFLD),2,MINUS=YES                                  
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY WRITE-OFFS (COST)                                           *         
***********************************************************************         
         SPACE 1                                                                
DISWOFC  DS    0H                                                               
         CURED TLJWOFC,(15,FVIFLD),2,MINUS=YES                                  
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY RECOVERIES (TIME)                                           *         
***********************************************************************         
         SPACE 1                                                                
DISRECT  DS    0H                                                               
         CURED TLJRECT,(15,FVIFLD),2,MINUS=YES                                  
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY RECOVERIES (COST)                                           *         
***********************************************************************         
         SPACE 1                                                                
DISRECC  DS    0H                                                               
         CURED TLJRECC,(15,FVIFLD),2,MINUS=YES                                  
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY TRANSFERS                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISXFR   DS    0H                                                               
         CURED TLJXFR,(15,FVIFLD),2,MINUS=YES                                   
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY INTERNAL INVOICES                                           *         
***********************************************************************         
         SPACE 1                                                                
DISINT   DS    0H                                                               
         CURED TLJINT,(15,FVIFLD),2,MINUS=YES                                   
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE LINE SELECTION                                             *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   SLL   R1,2                                                             
         B     *(R1)                                                            
         B     VALALL              ALLOCATE                                     
         B     VALWOF              WRITE-OFF                                    
         B     VALXFR              TRANSFER                                     
         B     VALINS              ADVANCE                                      
         B     VALDRA              DRAFT                                        
         B     VALUPD              UPDATE                                       
*                                                                               
VALALL   DS    0H                                                               
VALWOF   DS    0H                                                               
VALXFR   DS    0H                                                               
VALINS   DS    0H                                                               
VALDRA   DS    0H                                                               
VALUPD   DS    0H                                                               
         GOTO1 ASETUP,BOPARM,TLKJOB,0,0                                         
         BNE   EXITN                                                            
         OI    CSINDSG1,CSINDSET                                                
         OI    SVINDS,SVIALC                                                    
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE CLIENT                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALCLI   NTR1  ,                                                                
         TM    BASCLIH+FHIID,FHIIVA                                             
         BO    EXITY                                                            
         NI    BASPROH+FHIID,FF-FHIIVA                                          
         NI    BASJOBH+FHIID,FF-FHIIVA                                          
*                                                                               
         MVI   FVMINL,1                                                         
         MVC   FVMAXL,BCCLILEN                                                  
         GOTO1 AFVAL,BASCLIH                                                    
         BNE   EXITN                                                            
*                                                                               
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   ACTKACT,FVIFLD                                                   
         GOTO1 AIO,IOACCDIR+IOREAD+IO1                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INCLI)                                           
         B     EXITN                                                            
         TM    ACTKSTAT,ACTSLOCK                                                
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$CLILK)                                           
         B     EXITN                                                            
         TM    ACTKSTAT,ACTSCLOS                                                
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTCL)                                           
         B     EXITN                                                            
*                                                                               
         GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
         L     R2,AIO1             TEST CLIENT IS LOCKED                        
         LA    R1,ACTRFST                                                       
         USING LOKELD,R1                                                        
         XR    RF,RF                                                            
VCLI02   CLI   LOKEL,0                                                          
         BE    VCLI04                                                           
         CLI   LOKEL,LOKELQ                                                     
         BE    *+12                                                             
         IC    RF,LOKLN                                                         
         BXH   R1,RF,VCLI02                                                     
         TM    LOKSTAT,LOKSLOCK                                                 
         BZ    VCLI04                                                           
         MVC   FVMSGNO,=AL2(AE$CLILK)                                           
         B     EXITN                                                            
*                                                                               
VCLI04   MVC   LSCLI,FVIFLD                                                     
*                                                                               
VALCLIY  OI    BASCLIH+FHIID,FHIIVA                                             
         OI    BASCLIH+FHOID,FHOITR                                             
         B     EXITY                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRODUCT                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALPRO   NTR1  ,                                                                
         TM    BASPROH+FHIID,FHIIVA                                             
         BO    EXITY                                                            
         NI    BASJOBH+FHIID,FF-FHIIVA                                          
*                                                                               
         MVC   LSPROMIN,BCSPACES   SET DEFAULT MIN/MAX                          
         MVC   LSPROMAX,BCEFFS                                                  
         MVI   LSPROIND,0                                                       
*                                                                               
         GOTO1 AFVAL,BASPROH                                                    
         BNE   VALPROY             OKAY IF FIELD IS BLANK                       
*                                                                               
         XR    RF,RF                                                            
         IC    RF,BCPROLEN                                                      
         XR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         SR    RF,RE                                                            
         GOTO1 VALRNG,BOPARM,(RF),LSPROMIN,LSPROMAX                             
         BL    EXITN                                                            
         BNH   *+8                                                              
         OI    LSPROIND,LSPROISI   SET SINGLE VALUE ENTERED                     
*                                                                               
VALPROY  OI    BASPROH+FHIID,FHIIVA                                             
         OI    BASPROH+FHOID,FHOITR                                             
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE JOB                                                        *         
***********************************************************************         
         SPACE 1                                                                
VALJOB   NTR1  ,                                                                
         TM    BASJOBH+FHIID,FHIIVA                                             
         BO    EXITY                                                            
*                                                                               
         MVC   LSJOBMIN,BCSPACES   SET DEFAULT MIN/MAX                          
         MVC   LSJOBMAX,BCEFFS                                                  
         GOTO1 AFVAL,BASJOBH                                                    
         BNE   VALJOBY             OKAY IF FIELD IS EMPTY                       
*                                                                               
         XR    RF,RF                                                            
         IC    RF,BCJOBLEN                                                      
         XR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         SR    RF,RE                                                            
         GOTO1 VALRNG,BOPARM,(RF),LSJOBMIN,LSJOBMAX                             
         BL    EXITN                                                            
*                                                                               
VALJOBY  OI    BASJOBH+FHIID,FHIIVA                                             
         OI    BASJOBH+FHOID,FHOITR                                             
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE RANGE                                           *         
*                                                                     *         
* NTRY: P1 = MAXIMUM LENGTH OF INPUT                                  *         
*       P2 = A(MINIMUM FIELD)                                         *         
*       P3 = A(MAXIMUM FIELD)                                         *         
* EXIT: CC = EQUAL IF VALID RANGE ENTERED                             *         
*       CC = HIGH IF VALID SINGLE VALUE ENTERED                       *         
*       CC = LOW IF ERROR (FIELD TOO LONG)                            *         
***********************************************************************         
         SPACE 1                                                                
VALRNG   NTR1  ,                                                                
         LM    R1,R3,0(R1)                                                      
*                                                                               
         XR    R0,R0               SEARCH FOR THE DASH                          
         IC    R0,FVILEN                                                        
         LA    RF,FVIFLD                                                        
         CLI   0(RF),C'-'                                                       
         BE    VRNG02                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
*                                                                               
         CLM   R1,1,FVILEN         TEST SINGLE VALUE TOO LONG                   
         BL    VALRNGN                                                          
         IC    RE,FVXLEN                                                        
         EX    RE,*+4              COPY TO MINIMUM                              
         MVC   0(0,R2),FVIFLD                                                   
         EX    RE,*+4              COPY TO MAXIMUM                              
         MVC   0(0,R3),FVIFLD                                                   
         B     EXITH                                                            
*                                                                               
VRNG02   LA    RE,FVIFLD           COPY LEFT HAND SIDE                          
         SR    RE,RF                                                            
         LPR   RE,RE                                                            
         BZ    VRNG04                                                           
         CR    R1,RE                                                            
         BL    VALRNGN                                                          
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),FVIFLD                                                   
*                                                                               
VRNG04   LR    RE,R0               COPY RIGHT HAND SIDE                         
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BZ    VRNG06                                                           
         CR    R1,RE                                                            
         BL    VALRNGN                                                          
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R3),1(RF)                                                    
*                                                                               
VRNG06   DS    0H                                                               
         B     EXITY                                                            
*                                                                               
VALRNGN  MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE JOB TSAR RECORD                                   *         
*                                                                     *         
* NTRY: P1 = A(TSAR RECORD)                                           *         
*       P2 = A(JOB RECORD)                                            *         
***********************************************************************         
         SPACE 1                                                                
UPDJOB   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
J        USING TLSTD,R2                                                         
         USING ACTRECD,R3                                                       
*                                                                               
         LA    RE,ACTRFST                                                       
         USING JCBELD,RE                                                        
         XR    RF,RF                                                            
UJOB02   CLI   JCBEL,0                                                          
         BE    EXIT                                                             
         CLI   JCBEL,JCBELQ                                                     
         BE    *+12                                                             
         IC    RF,JCBLN                                                         
         BXH   RE,RF,UJOB02                                                     
*                                                                               
         CLC   J.TLJSEQ,JCBSEQ     COMPARE SEQUENCE NUMBERS                     
         BE    EXIT                NO CHANGE - JOB HASN'T CHANGED               
*                                                                               
         MVC   JOBTSAR,J.TLSTD     SAVE OLD RECORD                              
*                                  UPDATE TSAR RECORD                           
         GOTO1 SETJOB,BOPARM,J.TLSTD,ACTRECD                                    
*                                                                               
         GOTO1 KEYOUT,ACTKEY       SET KEYCLI/KEYPRO/KEYJOB                     
*                                                                               
P        USING TLSTD,PROTSAR       UPDATE PRODUCT TOTAL                         
         MVC   KEYJOB,BCSPACES                                                  
         MVI   KEYJOB,FF                                                        
         GOTO1 KEYIN,BOWORK1                                                    
         XC    P.TLKEY,P.TLKEY                                                  
         MVC   P.TLKSES,TWASESNL                                                
         MVC   P.TLKJOB,BOWORK1+(ACTKACT-ACTKEY)                                
         GOTO1 ATSARIO,BOPARM,('TSARDH',P.TLSTD)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 UPDTOT,BOPARM,P.TLSTD,J.TLSTD,JOBTSAR                            
         GOTO1 ATSARIO,BOPARM,('TSAPUT',P.TLSTD)                                
         DROP  P                                                                
*                                                                               
         TM    LSPROIND,LSPROISI   TEST SINGLE PRODUCT                          
         BO    UPDJOBX                                                          
C        USING TLSTD,CLITSAR       NO - UPDATE CLIENT TOTAL                     
         MVC   KEYJOB,BCSPACES                                                  
         MVC   KEYPRO,BCSPACES                                                  
         MVI   KEYPRO,FF                                                        
         GOTO1 KEYIN,BOWORK1                                                    
         XC    C.TLKEY,C.TLKEY                                                  
         MVC   C.TLKSES,TWASESNL                                                
         MVC   C.TLKJOB,BOWORK1+(ACTKACT-ACTKEY)                                
         GOTO1 ATSARIO,BOPARM,('TSARDH',C.TLSTD)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 UPDTOT,BOPARM,C.TLSTD,J.TLSTD,JOBTSAR                            
         GOTO1 ATSARIO,BOPARM,('TSAPUT',C.TLSTD)                                
         DROP  C                                                                
*                                                                               
UPDJOBX  B     EXIT                                                             
         SPACE 1                                                                
         DROP   J,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET JOB INFO ON TSAR RECORD                              *         
*                                                                     *         
* NTRY : P1 = A(TSAR RECORD)                                          *         
*        P2 = A(JOB RECORD)                                           *         
***********************************************************************         
         SPACE 1                                                                
SETJOB   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
T        USING TLSTD,R2                                                         
         USING ACTRECD,R3                                                       
*                                                                               
         LA    R0,TLJAMTSN                                                      
         LA    RF,T.TLJAMTS                                                     
         ZAP   0(L'TLJAMTS,RF),BCPZERO                                          
         LA    RF,L'TLJAMTS(RF)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         LA    RE,ACTRFST                                                       
         XR    RF,RF                                                            
SJOB02   CLI   0(RE),0                                                          
         BE    SJOB10                                                           
         USING ABLELD,RE                                                        
         CLI   ABLEL,ABLELQ                                                     
         BNE   SJOB04                                                           
         AP    TLJDR,ABLDR                                                      
         AP    TLJCR,ABLCR                                                      
         B     SJOB06                                                           
         USING SCIELD,RE                                                        
SJOB04   CLI   SCIEL,SCIELQ                                                     
         BNE   SJOB06                                                           
         CLI   SCITYPE,SCITT99S    TEST UNPOSTED BILLING                        
         BNE   SJOB08                                                           
         AP    TLJCR,SCIAMNT       ADD TO JOB DEBITS                            
         B     SJOB08                                                           
         USING JCBELD,RE                                                        
SJOB06   CLI   JCBEL,JCBELQ                                                     
         BNE   SJOB08                                                           
         MVC   TLJSEQ,JCBSEQ       SAVE SEQUENCE NUMBER                         
         DROP  RE                                                               
*                                                                               
SJOB08   IC    RF,1(RE)                                                         
         BXH   RE,RF,SJOB02                                                     
*                                                                               
SJOB10   LA    R4,PENTAB                                                        
         USING PENTABD,R4                                                       
SJOB12   CLI   PENTABD,EOT                                                      
         BE    SJOB20                                                           
*                                                                               
         LA    RE,ACTRFST          FIND SCIEL ON RECORD                         
         USING SCIELD,RE                                                        
         XR    RF,RF                                                            
SJOB14   CLI   SCIEL,0                                                          
         BE    SJOB18                                                           
         CLI   SCIEL,SCIELQ                                                     
         BNE   *+14                                                             
         CLC   SCITYPE,PENTYPE                                                  
         BE    *+12                                                             
         IC    RF,SCILN                                                         
         BXH   RE,RF,SJOB14                                                     
*                                                                               
         LH    RF,PENAMNT                                                       
         LA    R1,T.TLSTD(RF)                                                   
         ZAP   0(L'TLJAMTS,R1),SCIAMNT                                          
         ICM   RF,3,PENADMN                                                     
         BZ    SJOB18                                                           
         LA    R1,T.TLSTD(RF)                                                   
         ZAP   0(L'TLJAMTS,R1),SCIADMN                                          
         DROP  RE                                                               
*                                                                               
SJOB18   LA    R4,PENTABL(R4)                                                   
         B     SJOB12                                                           
         DROP  R4                                                               
*                                                                               
SJOB20   B     EXIT                                                             
         DROP  T,R3                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE TOTAL TSAR RECORD                                 *         
*                                                                     *         
* NTRY: P1 = A(CLIENT/PRODUCT TSAR RECORD)                            *         
*       P2 = A(NEW JOB TSAR RECORD)                                   *         
*       P3 = A(OLD JOB TSAR RECORD) / 0                               *         
***********************************************************************         
         SPACE 1                                                                
UPDTOT   NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
T        USING TLSTD,R2            R2=A(TOTAL TSAR RECORD)                      
N        USING TLSTD,R3            R3=A(NEW JOB TSAR RECORD)                    
O        USING TLSTD,R4            R4=A(OLD JOB TSAR RECORD)                    
*                                                                               
         LA    R0,TLJAMTSN                                                      
         LA    R1,T.TLJAMTS                                                     
         LA    RE,N.TLJAMTS                                                     
         LA    RF,O.TLJAMTS                                                     
UTOT02   AP    0(L'TLJAMTS,R1),0(L'TLJAMTS,RE)                                  
         LTR   R4,R4                                                            
         BZ    *+10                                                             
         SP    0(L'TLJAMTS,R1),0(L'TLJAMTS,RF)                                  
         LA    R1,L'TLJAMTS(R1)                                                 
         LA    RF,L'TLJAMTS(RF)                                                 
         LA    RE,L'TLJAMTS(RE)                                                 
         BCT   R0,UTOT02                                                        
*                                                                               
UPDTOTX  B     EXIT                                                             
         SPACE 1                                                                
         DROP  T,N,O                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT CLIENT/PRODUCT/JOB FROM ACTKACT                  *         
*                                                                     *         
* NTRY: R1 = A(ACCOUNT KEY)                                           *         
* EXIT: KEYCLI = CLIENT CODE                                          *         
*       KEYPRO = PRODUCT CODE                                         *         
*       KEYJOB = JOB CODE                                             *         
***********************************************************************         
         SPACE 1                                                                
KEYOUT   NTR1  ,                                                                
         USING ACTRECD,R1                                                       
         MVC   KEY,BCSPACES        SPACERISE CLIENT/PRODUCT/JOB                 
*                                                                               
         XR    RF,RF               COPY CLIENT CODE                             
         IC    RF,BCCLILEN                                                      
         LR    R0,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   KEYCLI(0),ACTKACT                                                
*                                                                               
         LA    R2,ACTKACT+1(RF)    COPY PRODUCT CODE                            
         IC    RF,BCPROLEN                                                      
         SR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   KEYPRO(0),0(R2)                                                  
*                                                                               
         LA    R2,1(R2,RF)         COPY JOB CODE                                
         IC    R0,BCPROLEN                                                      
         IC    RF,BCJOBLEN                                                      
         SR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   KEYJOB(0),0(R2)                                                  
*                                                                               
         B     EXIT                                                             
         DROP  R1                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO PUT CLIENT/PRODUCT/JOB INTO ACTKACT                      *         
*                                                                     *         
* NTRY: R1 = A(ACCOUNT KEY)                                           *         
*       KEYCLI = CLIENT CODE                                          *         
*       KEYPRO = PRODUCT CODE                                         *         
*       KEYJOB = JOB CODE                                             *         
* EXIT: ACCOUNT KEY UPDATED                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYIN    NTR1  ,                                                                
         USING ACTRECD,R1                                                       
         MVC   ACTKACT,BCSPACES    SPACERISE ACCOUNT CODE                       
*                                                                               
         XR    RF,RF               COPY CLIENT CODE                             
         IC    RF,BCCLILEN                                                      
         LR    R0,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   ACTKACT(0),KEYCLI                                                
*                                                                               
         LA    R2,ACTKACT+1(RF)    COPY PRODUCT CODE                            
         IC    RF,BCPROLEN                                                      
         SR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R2),KEYPRO                                                   
*                                                                               
         LA    R2,1(R2,RF)         COPY JOB CODE                                
         IC    R0,BCPROLEN                                                      
         IC    RF,BCJOBLEN                                                      
         SR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R2),KEYJOB                                                   
*                                                                               
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DEFAULT COLUMN LIST                                                 *         
***********************************************************************         
         SPACE 1                                                                
DEFCLM   DS    0XL1                                                             
         DC    AL1(JLI#JBN)                                                     
         DC    AL1(JLI#JBB)                                                     
         DC    AL1(EOT)                                                         
DEFCLML  EQU   *-DEFCLM                                                         
         SPACE 1                                                                
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PENDING AMOUNTS TABLE                                               *         
***********************************************************************         
         SPACE 1                                                                
PENTABD  DSECT                                                                  
PENTYPE  DS    AL1                 SCITYPE EQUATE                               
         DS    AL1                 N/D                                          
PENAMNT  DS    H                   DISP. TO SCIAMNT AMOUNT                      
PENADMN  DS    H                   DISP. TO SCIADMN AMOUNT                      
PENTABL  EQU   *-PENTABD                                                        
         SPACE 1                                                                
         PUSH  USING                                                            
         DROP                                                                   
         USING TLSTD,0                                                          
CLB1D    CSECT                                                                  
*                                                                               
PENTAB   DS    0H                                                               
         DC    AL1(SCITCBAP,0),S(TLJNET,TLJCOM)                                 
         DC    AL1(SCITCBWP,0),S(TLJWOFT,TLJWOFC)                               
         DC    AL1(SCITCBRP,0),S(TLJRECT,TLJRECC)                               
         DC    AL1(SCITCBTP,0),S(TLJXFR,0)                                      
         DC    AL1(SCITCBIP,0),S(TLJINT,0)                                      
PENTABX  DC    AL1(EOT)                                                         
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
LWORKD   DSECT                                                                  
RETURN   DS    A                                                                
DMCB     DS    6A                                                               
WORK     DS    XL100                                                            
LCLIOFFC DS    CL2                 CLIENT OFFICE CODE                           
LPROOFFC DS    CL2                 PRODUCT OFFICE CODE                          
LJOBOFFC DS    CL2                 JOB OFFICE CODE                              
*                                                                               
LSTINDS  DS    XL1                 LIST INDICATOR BYTE                          
LSTISEQ  EQU   X'80'               DO SEQUENTIAL READ NEXT, NOT RDHIGH          
LSTICLI  EQU   X'40'               HAVE SAVED CLIENT TOTAL                      
LSTICLIY EQU   X'20'               FOUND JOB FOR CLIENT                         
LSTIPRO  EQU   X'10'               HAVE SAVED PRODUCT TOTAL                     
LSTIPROY EQU   X'08'               FOUND JOB FOR PRODUCT                        
*                                                                               
SVOPS    DS    XL(LSFLTOPL)        SAVED OPTIONS                                
*                                                                               
KEY      DS    0CL18               EXTRACTED CLIENT/PRODUCT/JOB                 
KEYCLI   DS    CL5                 CLIENT CODE                                  
KEYPRO   DS    CL5                 PRODUCT CODE                                 
KEYJOB   DS    CL8                 JOB CODE                                     
*                                                                               
CLITSAR  DS    XL(TLJOBLNQ+L'TLNUM) SAVED CLIENT TSAR RECORD                    
PROTSAR  DS    XL(TLJOBLNQ+L'TLNUM) SAVED PRODUCT TSAR RECORD                   
JOBTSAR  DS    XL(TLJOBLNQ+L'TLNUM) SAVED JOB TSAR RECORD                       
         DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* OPTIONS TABLE                                                       *         
***********************************************************************         
         SPACE 1                                                                
CLB1D    CSECT                                                                  
OPTTAB   DS    0X                                                               
*                                  DISPLAY=COLUMN CODES                         
         DC    AL2(UC8DSP-TWAD,UC3DSP-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,64,0)                                            
         DC    AL1((*-OPTTAB)/OPTTABL)                                          
         DC    AL2(OPTDISQ,0)                                                   
         DC    CL4'+'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  JOB=ABC-XYZ                                  
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
* VALIDATE OPTIONS                                                  *           
*********************************************************************           
         SPACE 1                                                                
         DROP  R6,R7,R8,RB                                                      
         DS    0D                                                               
VALOPT   NMOD1 0,**VALO**                                                       
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         LR    R4,R2               R4=A(OPTTAB ENTRY)                           
         USING OPTTABD,R4                                                       
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *(RF)                                                            
         SPACE 1                                                                
VALX     XIT1  ,                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INCLUDED DSECTS                                                     *         
***********************************************************************         
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
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
* GEGENEXC                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENEXC                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* GEGENCUR                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENCUR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
*                                                                               
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBE6D                                                       
*                                                                               
         ORG   OSVALS                                                           
*                                                                               
SVENTCLI DS    XL12                ENTRY CLIENT                                 
SVENTPRO DS    XL12                ENTRY PRODUCT                                
SVENTJOB DS    XL12                ENTRY JOB                                    
*                                                                               
SVINDS   DS    XL1                 INDICATOR BYTE                               
SVIALC   EQU   X'80'               ALLOCATE CALLED                              
*                                                                               
SVAMPEND DS    PL8                 AMOUNT PENDING                               
*                                                                               
         ORG   OSVALS+OSVALSL                                                   
*                                                                               
LSVALSD  DSECT                                                                  
         ORG   LSOVER                                                           
LSCLI    DS    CL5                 CLIENT CODE                                  
LSPROMIN DS    CL5                 MINIMUM PRODUCT                              
LSPROMAX DS    CL5                 MAXIMUM PRODUCT CODE                         
LSPROIND DS    XL1                 PRODUCT INDICATOR                            
LSPROISI EQU   X'80'               PRODUCT IS SINGLE VALUE (NOT RANGE)          
LSJOBMIN DS    CL8                 MINIMUM JOB CODE                             
LSJOBMAX DS    CL8                 MAXIMUM JOB CODE                             
         DS    (L'LSOVER-(*-LSOVER))X                                           
*                                                                               
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKJOB   DS    CL12                JOB CODE                                     
         SPACE 1                                                                
CLB1D    CSECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'083ACCLB1D   08/16/00'                                      
         END                                                                    
