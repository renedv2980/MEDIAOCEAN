*          DATA SET ACCLB22    AT LEVEL 007 AS OF 08/16/00                      
*PHASE T62122A                                                                  
CLB22    TITLE '- BILL PROGRAM UPDATE FEE ADJUSTMENTS'                          
CLB22    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CB22**,RR=RE                                                 
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING MIXLST,LSMIXLST                                                  
         USING PBLKD,R7                                                         
         USING POSTVALS,PBPOSTV                                                 
         L     RC,AOVERWRK                                                      
         USING FWORKD,RC                                                        
*                                                                               
         L     R4,PBADATAB         DA TABLE                                     
*                                                                               
UPDF100  ST    R4,PBLSTDA          SAVE A(CURRENT TABLE ENTRY)                  
         CLI   0(R4),FF            FIN                                          
         BE    UPDF900                                                          
         TM    L'TRNKDA(R4),TYPSFEE                                             
         BZ    UPDF800             NOT A FEE ADJUSTMENT                         
         MVC   IODAOVER,0(R4)                                                   
         LA    R1,IOGET+IOACCMST+IO3                                            
         CLI   PBMODE,PBLIVEQ                                                   
         BNE   *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         CLI   PBMODE,PBDRAFTQ     TEST DRAFT                                   
         BE    UPDF105                                                          
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
UPDF105  L     R2,AIO3             RESET R2=A(FEEADJ TRANSACTION)               
         USING TRNELD,TRNRFST                                                   
*                                                                               
         MVC   FANC,BCSPACES                                                    
         CLC   =C'SI',TRNKULC      IF INCOME GET ANALYSIS ACCOUNT CODE          
         BNE   UPDF112                                                          
         USING ACTRECD,IOKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCULA,TRNKCULC                                                
         GOTO1 AGETACT,0                                                        
         BE    UPDF110                                                          
         CLI   PBMODE,PBDRAFTQ                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
UPDF110  EQU   *                                                                
*&&UK*&& MVC   FANC(L'ACCOST),ACCOST                                            
*&&US                                                                           
         ICM   RF,15,ACASPA                                                     
         BZ    *+10                                                             
         MVC   FANC,SPAAULA-SPAELD(RF)                                          
*&&                                                                             
*                                                                               
UPDF112  MVC   POSTBTMC,PBFEEMC                                                 
         MVC   POSTBTRF,PBFEEREF                                                
         MVI   POSTTYPE,8                                                       
         MVC   POSTACT,TRNKULA                                                  
         MVC   POSTACTN,BCJOBNAM                                                
         MVC   POSTCAC,TRNKCULC                                                 
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
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
         MVI   PBPTRS,FF                                                        
         XC    POSTNARR,POSTNARR                                                
         SR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         SH    RF,=Y(TRNNARR-TRNELD+1)                                          
         BM    *+14                                                             
         EX    RF,*+4                                                           
         MVC   POSTNARR(0),TRNNARR                                              
*                                                                               
         MVC   FOFFC,BCSPACES      CLEAR SAVED FINANCIAL OFFICE                 
         LA    R3,TRNELD                                                        
         USING PTAELD,R3                                                        
UPDF120  CLI   PTAEL,0                                                          
         BE    UPDF140                                                          
         CLI   PTAEL,PTAELQ                                                     
         BE    UPDF130                                                          
         CLI   PTAEL,SPAELQ                                                     
         BE    UPDF130                                                          
         CLI   PTAEL,SCIELQ                                                     
         BE    UPDF130                                                          
         CLI   PTAEL,APEELQ                                                     
         BE    UPDF130                                                          
         CLI   PTAEL,FFTELQ                                                     
         BNE   UPDF122                                                          
         CLI   FFTTYPE-FFTELD(R3),FFTTOFFC                                      
         BNE   UPDF122                                                          
         MVC   FOFFC,FFTDATA-FFTELD(R3)                                         
UPDF122  SR    RE,RE                                                            
         IC    RE,PTALN                                                         
         AR    R3,RE                                                            
         B     UPDF120                                                          
UPDF130  IC    RE,PTALN            COPY SPA/SCI/PTA/APEEL TO NEW ITEM           
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BOELEM(0),PTAEL                                                  
         GOTO1 AADDXTRA                                                         
         B     UPDF122                                                          
*                                                                               
UPDF140  GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
*                                                                               
         L     R2,AIO3                                                          
         CLI   PBMODE,PBDRAFTQ     TEST IF UPDATING FILE                        
         BE    UPDF160                                                          
         TM    TRNRSTA2-TRNRECD(RE),TRNSBILP                                    
         BNO   UPDF160                                                          
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
UPDF160  MVI   POSTSTAT,0          CLEAR STATUS - CREDIT                        
         MVC   POSTOFFC,CSOFFICE                                                
         CLC   FOFFC,BCSPACES      TEST ANY OVERRIDE FINANCIAL OFFICE           
         BNH   *+10                                                             
         MVC   POSTOFFC,FOFFC                                                   
         MVC   POSTACT,TRNKULC                                                  
         CLI   POSTACT,C'S'        TEST CONTRA WAS IN UNIT S                    
         BE    UPDF170                                                          
         MVC   POSTACT,BCSPACES    CLEAR POSTING ACCOUNT                        
         LA    RF,TRNRFST                                                       
         SR    R0,R0                                                            
         USING APEELD,RF                                                        
UPDF162  CLI   APEEL,0             FIND ANALYSIS POINTER ELEMENT                
         BNE   *+6                                                              
         DC    H'0'                ANALYSIS POINTER TO SK/SI MISSING            
         CLI   APEEL,APEELQ                                                     
         BE    UPDF166                                                          
UPDF164  IC    R0,APELN                                                         
         AR    RF,R0                                                            
         B     UPDF162                                                          
UPDF166  CLI   APENUM,1            TEST SINGLE ENTRY                            
         BNE   UPDF164                                                          
         LA    RE,APENTRY                                                       
         USING APENTRY,RE                                                       
         CLC   =C'SI',APENACT      TEST SI POINTER                              
         BE    *+14                                                             
         CLC   =C'SK',APENACT      TEST SK POINTER                              
         BNE   UPDF164                                                          
         DROP  RF                                                               
         SR    RF,RF               EXTRACT POINTER ACCOUNT                      
         IC    RF,APENLEN                                                       
         SH    RF,=Y(APENACT-APENLEN+1)                                         
         EX    RF,*+4                                                           
         MVC   POSTACT(0),APENACT                                               
         DROP  RE                                                               
UPDF170  MVC   POSTCAC,TRNKCULA    FULL JOB FOR SK CONTRA                       
         CLC   =C'SK',POSTACT                                                   
         BE    UPDF200                                                          
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
*                                                                               
         USING SCIELD,BOELEM                                                    
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGRSS                                                 
         ZAP   SCIAMNT,BCPZERO                                                  
         GOTO1 AADDXTRA                                                         
*&&US                                                                           
*                                  FUDGE ADDRESS OF PTAEL                       
         LA    R3,PBFEEMP-(PTAMOA-PTAELD)                                       
         GOTO1 AADDWMDT,BOPARM,PTAELD,BCJOBCOD  ADD MEDIA XFER ELEMENT          
*&&                                                                             
*                                  SET UP ANALYSIS CODES FOR APEEL              
         LA    RE,PBPTRS                                                        
         LA    RF,ACTKULA          RF=A(U/L/CLI/PRO/JOB)                        
         STCM  RF,15,0(RE)                                                      
         MVI   0(RE),APENSDR       JOB POSTING WAS A DEBIT                      
         LA    RE,4(RE)                                                         
         CLC   FANC,BCSPACES       TEST ANY COSTING CODE                        
         BNH   UPDF180                                                          
         LA    RF,BCCMPPRF+(PPRCOSTU-PPRELD)                                    
         STCM  RF,15,0(RE)                                                      
         MVI   0(RE),APENSDR       1C POSTING IS A DEBIT                        
         LA    RE,4(RE)                                                         
         MVC   FCA(L'ULREV),ULREV                                               
         MVC   FCA+L'ULREV(L'FANC),FANC                                         
         LA    RF,FCA                                                           
         STCM  RF,15,0(RE)                                                      
         MVI   0(RE),0             REVENUE POSTING IS CREDIT                    
         LA    RE,4(RE)                                                         
UPDF180  MVI   0(RE),X'FF'                                                      
*                                                                               
UPDF200  DS    0H                                                               
*&&US                                                                           
         GOTO1 AADDRFL,TRNKWORK    ADD WORKCODE FILTER ELEMENT                  
*&&                                                                             
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         MVI   PBPTRS,FF                                                        
         CLC   =C'SK',POSTACT      NO COSTING FOR SK                            
         BE    UPDF800                                                          
*                                                                               
         CLC   FANC,BCSPACES       TEST ANY ANALYSIS ACCOUNT                    
         BNH   UPDF800                                                          
         MVC   POSTACT,BCCMPPRF+(PPRCOSTU-PPRELD)                               
         GOTO1 ASETACTN,BOPARM,POSTACT,POSTACTN                                 
         MVC   POSTCAC,BCSPACES                                                 
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'ACTKCPY(ACTKACT-ACTKUNT),ULREV                         
         MVC   POSTCAC+(ACTKACT-ACTKEY)(L'FANC),FANC                            
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         MVI   POSTSTAT,TRNSDR                                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BNE   EXIT                                                             
         MVC   POSTACT,POSTCAC+(ACTKUNT-ACTKEY)                                 
         MVC   POSTCAC,BCCMPPRF+(PPRCOST-PPRELD)                                
         XC    POSTACTN,POSTCACN   SWAP ACCOUNT/CONTRA                          
         XC    POSTCACN,POSTACTN                                                
         XC    POSTACTN,POSTCACN                                                
         MVI   POSTSTAT,0                                                       
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BNE   EXIT                                                             
*                                                                               
UPDF800  L     R4,PBLSTDA          GET NEXT FROM D/A LIST                       
         LA    R4,L'TRNKDA+L'TRNKSTA2(R4)                                       
         B     UPDF100                                                          
*                                                                               
UPDF900  B     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC=HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC=LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC=EQUAL                                 
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
ULREV    DC    C'12'                                                            
ACCMST   DC    C'ACCMST '                                                       
         SPACE 1                                                                
***********************************************************************         
* GENERAL EQUATES                                                     *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* LOCAL W/S                                                           *         
***********************************************************************         
         SPACE 1                                                                
FWORKD   DSECT                                                                  
*                                                                               
FANC     DS    CL(L'ACTKACT)       FEEADJ INCOME COSTING CODE                   
FOFFC    DS    CL2                 FININCIAL OFFICE                             
FCA      DS    CL(L'ACTKULA)       FEE COST ACCOUNT (ULREV+UPDFEEAN)            
*                                                                               
FWORKL   EQU   *-FWORKD                                                         
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
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORK                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACCLB22   08/16/00'                                      
         END                                                                    
