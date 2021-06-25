*          DATA SET ACCLB20    AT LEVEL 006 AS OF 08/16/00                      
*PHASE T62120A                                                                  
CLB20    TITLE '- BILL PROGRAM UPDATE WRITE-OFFS/RECOVERIES'                    
CLB20    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CB20**,RR=RE                                                 
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING MIXLST,LSMIXLST                                                  
         USING PBLKD,R7                                                         
         USING POSTVALS,PBPOSTV                                                 
         L     RC,AOVERWRK                                                      
         USING WWORKD,RC                                                        
*                                                                               
         MVC   WTYPES,TYPTYPES-TYPLSTD(R1)                                      
*                                                                               
         L     R4,PBADATAB                                                      
UPDW100  ST    R4,PBLSTDA          SAVE FOR NEXT                                
         CLI   0(R4),FF                                                         
         BE    UPDW500             ALL DONE                                     
         TM    L'TRNKDA(R4),TRNSWOFP                                            
         BZ    UPDW900             NO WRITE-OFF PENDING                         
         MVC   IODAOVER,0(R4)                                                   
         LA    R1,IOGET+IOACCMST+IO1                                            
         CLI   PBMODE,PBLIVEQ                                                   
         BNE   *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
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
UPDW210  SR    R0,R0                                                            
         IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         CLI   PTAEL,0                                                          
         BE    UPDW900                                                          
         CLI   PTAEL,PTAELQ                                                     
         BNE   UPDW210                                                          
UPDW220  TM    PTASTAT1,PTASPEND   TEST ACTIVITY PENDING                        
         BNO   UPDW210                                                          
         CLI   PTATYPE,PTATWOF     TEST WRITE-OFF ELEMENT                       
         BNE   UPDW225                                                          
         TM    WTYPES,PBWOFQ       CHECK WRITE-OFFS BEING UPDATED               
         BZ    UPDW210                                                          
         B     UPDW230                                                          
UPDW225  CLI   PTATYPE,PTATWOFR    TEST RECOVERY ELEMENT                        
         BNE   UPDW210                                                          
         TM    WTYPES,PBRECQ       CHECK RECOVERIES BEING UPDATED               
         BZ    UPDW210                                                          
*                                  UPDATE PTAEL AND RECORD                      
UPDW230  NI    PTASTAT1,FF-PTASPEND                                             
         MVC   PTAMOA,PBWOFMP      SET W-OFF MONTH                              
         CLI   PTATYPE,PTATWOF                                                  
         BE    *+10                                                             
         MVC   PTAMOA,PBRECMP      SET RECVR MONTH                              
*&&US*&& MVC   PTADATE,BCTODAYC    SET TODAY FOR NARR AND DISPLAY               
         NI    TRNRSTA2,FF-TRNSWOFP                                             
         CLI   PBMODE,PBDRAFTQ     TEST DRAFT                                   
         BE    UPDW240                                                          
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('TRSELQ',AIO1),0,0                  
         CLI   BODMCB+12,0                                                      
         BNE   UPDW232                                                          
         L     RE,BODMCB+12                                                     
         OI    TRSSTAT3-TRSELD(RE),TRSSNBIL                                     
         CLI   PTATYPE,PTATWOFR    TEST THIS IS A RECOVERY                      
         BNE   UPDW232                                                          
         NI    TRSSTAT3-TRSELD(RE),FF-TRSSNBIL                                  
UPDW232  GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('TRXELQ',AIO1),0,0                  
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
UPDW240  LA    R2,IOKEY                                                         
         USING PTARECD,R2                                                       
         XC    PTAKEY,PTAKEY                                                    
         MVI   PTAKTYP,PTAKTYPQ                                                 
         MVC   PTAKCPY,CUABIN                                                   
         MVC   PTAKJOB,BCJOBCOD                                                 
         MVC   PTAKSEQN,PTASEQN                                                 
         L     R1,=A(IO8)                                                       
         LA    R1,IOREAD+IOACCDIR(R1)                                           
         CLI   PBMODE,PBLIVEQ                                                   
         BNE   *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=A(IO8)                                                       
         LA    R1,IOGET+IOACCMST(R1)                                            
         CLI   PBMODE,PBLIVEQ                                                   
         BNE   *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
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
         BNE   UPDW260                                                          
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
         MVC   POSTCACN,BCPRONAM                                                
         MVI   POSTSTAT,0          THEN POST CREDIT                             
         MP    POSTAMNT,=P'-1'     AND REVERSE SIGN                             
UPDW260  MVC   POSTTYPE,TRNTYPE                                                 
         OI    POSTSTAT,TRNSNOCM                                                
         CLI   PTATYPE,PTATWOF                                                  
         BNE   UPDW262                                                          
         MVC   POSTBTRF,PBWOFREF   SET W-OFF REFERENCE/MONTH                    
         MVC   POSTBTMC,PBWOFMC                                                 
*&&US                                                                           
         CLI   POSTTYPE,POSTWOFC   SET FLAG FOR COST/TIME                       
         BNE   *+12                                                             
         OI    PBINDS1,PBIWOFC                                                  
         B     *+8                                                              
         OI    PBINDS1,PBIWOFT                                                  
*&&                                                                             
         B     UPDW264                                                          
UPDW262  MVC   POSTBTRF,PBRECREF   SET RECVR REFERENCE/MONTH                    
         MVC   POSTBTMC,PBRECMC                                                 
*&&US                                                                           
         CLI   POSTTYPE,POSTWOFC   SET FLAG FOR COST/TIME                       
         BNE   *+12                                                             
         OI    PBINDS1,PBIRECC                                                  
         B     *+8                                                              
         OI    PBINDS1,PBIRECT                                                  
*&&                                                                             
*                                                                               
UPDW264  GOTO1 BLDWONAR,BOPARM,TRNELD,PTAELD   BUILD NARRATIVE                  
*                                                                               
         MVI   PBPTRS,FF                                                        
         MVI   WFILT,0                                                          
         BAS   RE,BLDPTRS          BUILD ANALYSIS POINTERS                      
         L     RE,POSTXTRA         CLEAR EXTRA ELEMENT BLOCK                    
         MVI   0(RE),0                                                          
*&&US                                                                           
         CLC   =C'SI',POSTACT      TEST GOING TO INCOME                         
         BNE   UPDW266                                                          
         GOTO1 AADDWMDT,BOPARM,PTAELD,BCJOBCOD  ADD MEDIA XFER EL               
UPDW266  L     R1,AIO1                                                          
         LA    R1,TRNKWORK-TRNRECD(R1)                                          
         GOTO1 AADDRFL             ADD WORKCODE FILTER ELEMENT                  
*&&                                                                             
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCJOBCOD),BCJOBCOD                  
*                                  GET WC/DATE/REF FROM ORIGINAL                
         L     RF,AIO1                                                          
         USING TRNRECD,RF                                                       
X        USING TRNELD,TRNRFST                                                   
         MVC   POSTOFFC,X.TRNOFFC                                               
         MVC   POSTDATE,TRNKDATE                                                
         MVC   POSTREF,TRNKREF                                                  
         MVC   POSTACT,POSTCAC+L'TRNKCPY                                        
         MVC   POSTCAC,TRNKCULC                                                 
         GOTO1 AXTRAELS,BOPARM,X.TRNELD,PTAELD                                  
         DROP  X,RF                                                             
*                                                                               
         PUSH USING                                                             
         USING TRSELD,BOELEM       ADD TRSELD                                   
         XC    TRSELD(TRSLNQ),TRSELD                                            
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         OI    TRSSTAT3,TRSSNBIL   NOT BILLABLE ON A21                          
         GOTO1 AADDXTRA                                                         
*                                                                               
         USING SPAELD,BOELEM       ADD WRITE-OFF SPAEL                          
         XC    SPAELD(SPALNQ),SPAELD                                            
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATWOFF                                                 
         MVC   SPAAULA,PTAWEXPA                                                 
         GOTO1 AADDXTRA                                                         
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
         GOTO1 AADDXTRA                                                         
*&&                                                                             
*                                                                               
         ZAP   POSTAMNT,PTANET                                                  
         MVI   POSTSTAT,TRNSDR                                                  
         L     RF,AIO1                                                          
         TM    (TRNRFST-TRNRECD)+(TRNSTAT-TRNELD)(RF),TRNSNOCM                  
         BNO   *+8                                                              
         OI    POSTSTAT,TRNSNOCM   -DR TO JOB GETS SAME STATUS                  
         MP    POSTAMNT,=P'-1'                                                  
         MVI   PBPTRS,FF           NO ANALYSIS POINTERS                         
         GOTO1 ASETACTN,BODMCB,POSTCUL,POSTCACN                                 
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         L     RE,POSTXTRA         CLEAR EXTRA ELEMENT BLOCK                    
         MVI   0(RE),0                                                          
         MVC   POSTOFFC,TRNOFFC                                                 
         MVC   POSTDATE,TRNDATE    RESET REF/DATE FROM W/O INPUT                
         MVC   POSTREF,TRNREF                                                   
*&&US                                                                           
*        CLI   TRNTYPE,57          TEST TIME                                    
*        BNE   UPDW280                                                          
         L     RE,AIO1                                                          
         CLC   =C'SK',TRNKULC-TRNKEY(RE)                                        
         BNE   *+14                                                             
         MVC   POSTACT,TRNKULC-TRNKEY(RE)                                       
         B     UPDW270                                                          
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('SPDELQ',AIO1),(2,=C'SK')           
         CLI   12(R1),0                                                         
         BNE   UPDW280                                                          
         L     RF,12(R1)                                                        
         USING SPDELD,RF                                                        
         MVC   POSTACT,BCSPACES                                                 
         IC    RE,SPDLN                                                         
         SH    RE,=Y(SPDLN1Q+1)                                                 
         EX    RE,*+4                                                           
         MVC   POSTACT(0),SPDACCS                                               
         DROP  RF                                                               
UPDW270  MVC   POSTCAC(L'CUABIN),CUABIN                                         
         MVC   POSTCAC+L'CUABIN(L'BCCPYPRD),BCCPYPRD                            
         MVC   POSTCAC+L'CUABIN+L'BCCPYPRD(L'BCJOBCOD),BCJOBCOD                 
         MVC   POSTCACN,BCJOBNAM                                                
         ZAP   POSTAMNT,PTANET                                                  
         MVI   POSTSTAT,TRNSDR                                                  
         MVI   PBPTRS,FF                                                        
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         MVC   POSTACT(2),=C'SI'                                                
         MVC   POSTCAC+L'CUABIN+L'BCCPYPRD(L'BCPROCOD),BCPROCOD                 
         MVC   POSTCACN,BCPRONAM                                                
         MVI   POSTSTAT,0                                                       
         MVI   WFILT,WOIAPESK                                                   
         BAS   RE,BLDPTRS          BUILD ANALYSIS POINTERS                      
         GOTO1 AADDWMDT,BOPARM,PTAELD,BCJOBCOD ADD MEDIA XFER EL                
         L     R1,AIO1                                                          
         LA    R1,TRNKWORK-TRNRECD(R1)                                          
         GOTO1 AADDRFL             ADD WORKCODE FILTER ELEMENT                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
*&&                                                                             
         EJECT                                                                  
**********************************************************************          
*              ANALYSIS WRITE-OFF POSTINGS                           *          
**********************************************************************          
         SPACE 1                                                                
UPDW280  CLI   PTALN,PTAWLN2Q                                                   
         BNE   UPDW300                                                          
         MVI   POSTSTAT,TRNSDR                                                  
         MVC   POSTACT,PTAWEXPA                                                 
         MVC   POSTCAC(L'TRNKCPY),CUABIN                                        
         MVC   POSTCAC+L'CUABIN(L'TRNKULA),PTAWWOFA                             
         ZAP   POSTAMNT,PTANET                                                  
         CLC   =C'SI',POSTACT                                                   
         BNE   UPDW286                                                          
         MVI   POSTSTAT,0                                                       
         MP    POSTAMNT,=P'-1'                                                  
*&&US                                                                           
         GOTO1 AADDWMDT,BOPARM,PTAELD,BCJOBCOD ADD MEDIA XFER EL                
*&&                                                                             
         L     R1,AIO1                                                          
         LA    R1,TRNKWORK-TRNRECD(R1)                                          
         GOTO1 AADDRFL             ADD WORKCODE FILTER ELEMENT                  
UPDW286  MVI   PBPTRS,FF           NO ANALYSIS POINTERS                         
         GOTO1 ASETACTN,BODMCB,POSTCUL,POSTCACN                                 
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         L     RE,POSTXTRA         CLEAR EXTRA ELEMENT BLOCK                    
         MVI   0(RE),0                                                          
*                                                                               
         XC    POSTACT,POSTCAC+1                                                
         XC    POSTCAC+1(L'POSTACT),POSTACT                                     
         XC    POSTACT,POSTCAC+1                                                
         MVI   POSTSTAT,0                                                       
         ZAP   POSTAMNT,PTANET                                                  
         MVI   PBPTRS,FF           NO ANALYSIS POINTERS                         
         CLC   =C'SI',POSTACT                                                   
         BNE   UPDW288                                                          
*&&US                                                                           
         GOTO1 AADDWMDT,BOPARM,PTAELD,BCJOBCOD ADD MEDIA XFER EL                
*&&                                                                             
         L     R1,AIO1                                                          
         LA    R1,TRNKWORK-TRNRECD(R1)                                          
         GOTO1 AADDRFL             ADD WORKCODE FILTER ELEMENT                  
UPDW288  GOTO1 ASETACTN,BODMCB,POSTCUL,POSTCACN                                 
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         L     RE,POSTXTRA         CLEAR EXTRA ELEMENT BLOCK                    
         MVI   0(RE),0                                                          
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
*              MAKE DEPARTMENT/COSTING/PERSON POSTINGS               *          
**********************************************************************          
         SPACE 1                                                                
UPDW300  LA    R2,WOTAB                                                         
         USING WOTABD,R2                                                        
UPDW310  CLI   WOTABD,EOT          TEST END-OF-TABLE                            
         BE    UPDW380                                                          
         GOTO1 TESTWO,BOPARM,WOTABD  TEST POSTING VALID                         
         BNE   UPDW370                                                          
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
         BO    UPDW320                                                          
         GOTO1 ASETACTN,BODMCB,POSTCUL,POSTCACN                                 
         B     UPDW330                                                          
*                                                                               
UPDW320  MVC   POSTCACN,BCSPACES                                                
         MVI   POSTCAC,C'*'                                                     
         TM    WOINDS1,WOICAN2P    TEST CONTRA NAME FROM 2P ACCOUNT             
         BZ    UPDW322                                                          
         MVC   BOWORK1(L'POSTCAC),POSTCAC                                       
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),(SPAELQ,AIO8),              *        
               (1,=AL1(SPATW2PA))                                               
         CLI   12(R1),0                                                         
         BNE   UPDW330                                                          
         L     RF,12(R1)                                                        
         MVC   POSTCAC(L'CUABIN),CUABIN                                         
         MVC   POSTCAC+L'CUABIN(L'SPAAULA),SPAAULA-SPAELD(RF)                   
         GOTO1 ASETACTN,BODMCB,POSTCUL,POSTCACN                                 
         MVC   POSTCAC,BOWORK1                                                  
         B     UPDW330                                                          
UPDW322  TM    WOINDS1,WOICANCL    TEST CONTRA NAME FROM CLIENT                 
         BZ    UPDW330                                                          
         MVC   POSTCACN,BCCLINAM                                                
*                                                                               
UPDW330  GOTO1 ABLDTRN,BOPARM,POSTVALS                                          
         BH    EXIT                                                             
*                                                                               
UPDW370  LA    R2,WOTABL(R2)                                                    
         B     UPDW310                                                          
         DROP  R2                                                               
*                                                                               
UPDW380  GOTO1 AXFRTMS,BOPARM,AIO1,(C'W',PTAELD),POSTVALS                       
         B     UPDW210             MAY BE ANOTHER WOFF PTAEL (REC)              
*                                                                               
UPDW900  L     R4,PBLSTDA          GET NEXT FROM D/A LIST                       
         LA    R4,L'TRNKDA+L'TRNKSTA2(R4)                                       
         B     UPDW100                                                          
*                                                                               
UPDW500  B     EXITY                                                            
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
*              BUILD WRITE-OFF ANLAYSIS POINTERS                      *         
*                                                                     *         
*              - PTA RECORD IN IO8                                    *         
***********************************************************************         
         SPACE 1                                                                
BLDPTRS  NTR1  ,                                                                
         LA    R3,PBPTRS           R3=A(LIST OF ANALYSIS POINTERS)              
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
         MVC   BCBYTE1,WFILT      SET CALLERS FILTERS                           
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
         B     EXIT                                                             
         DROP  R4                                                               
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
         LA    R2,POSTNARR                                                      
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
         EDIT  (B2,PTAHOURS),(8,(R2)),2,ALIGN=LEFT,DUB=BODUB1,         *        
               WRK=BOWORK1                                                      
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
         B     EXIT                                                             
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
         B     EXIT                                                             
TESTWONO LTR   RB,RB               SET CC=NOT EQUAL                             
         B     EXIT                                                             
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
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
         SPACE 1                                                                
***********************************************************************         
* GENERAL EQUATES                                                     *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
ALL      EQU   0                                                                
         EJECT                                                                  
***********************************************************************         
* WRITE-OFF ANALYSIS TABLE                                            *         
***********************************************************************         
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
         SPACE 1                                                                
CLB20    CSECT                                                                  
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
***********************************************************************         
* LOCAL W/S                                                           *         
***********************************************************************         
         SPACE 1                                                                
WWORKD   DSECT                                                                  
*                                                                               
WTYPES   DS    XL1                 PBTYPES EQUATE (PBWOFQ / PBRECQ)             
WFILT    DS    XL1                 FILTER BITS FOR APEEL                        
*                                                                               
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
**PAN#1  DC    CL21'006ACCLB20   08/16/00'                                      
         END                                                                    
