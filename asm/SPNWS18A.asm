*          DATA SET SPNWS18A   AT LEVEL 029 AS OF 07/17/02                      
*PHASE T20718A,*                                                                
         TITLE 'BWS18 T20718  BUYER''S WORK SHEET - NETWORK OVERLAY'            
* PWES 024                                                                      
* EJOR 023 21DEC00                                                              
T20718   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1250,T20718**,RA,RR=RE                                           
*                                                                               
         USING TWAD,R5                                                          
         USING SAVAREA,R6                                                       
         USING WORKD,R7                                                         
         LR    R1,RC                                                            
         L     RC,APALOCAL                                                      
         USING LOCALD,RC                                                        
         ST    R1,LAWORK           A(EXTRA WORKING STORAGE)                     
         ST    RB,APBASE1                                                       
         ST    RB,APNTRYA                                                       
         ST    RD,APWORKA                                                       
         ST    RE,APRELO                                                        
*                                                                               
         LA    R2,IOKEY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         CLI   APMODE,APMVALK        VALIDATE KEY                               
         BE    VALKEY                                                           
         CLI   APMODE,APMVALR        VALIDATE RECORD                            
         BE    VALREC                                                           
         CLI   APMODE,APMDISR        DISPLAY RECORD                             
         BE    DISREC                                                           
         CLI   APMODE,APMDELR        DELETE RECORD                              
         BE    DELREC                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   GOTO1 AVALMED,NETMEDH     VALIDATE MEDIA                               
         BNE   VALKX                                                            
         GOTO1 AVALBYR,NETBYRH     VALIDATE BUYER                               
         BNE   VALKX                                                            
         OC    BYRPW,BYRPW         CHECK BUYER'S PASSWORD                       
         BZ    VALK2                                                            
         GOTO1 AVALPWD                                                          
         BNE   VALKX                                                            
*                                                                               
VALK2    GOTO1 AVALCAM,NETCAMH     VALIDATE CAMPAIGN                            
         BNE   VALKX                                                            
         GOTO1 AGETCLT,CMPCLTC     GET CLIENT                                   
         BE    *+16                                                             
         LA    R1,NETCAMH                                                       
         ST    R1,FVADDR                                                        
         B     VALKX                                                            
         GOTO1 AGETPRD,CMPPRDN     GET PRODUCT                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETEST,CMPESTN     GET ESTIMATE                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AVALMKT,NETMKTH     VALIDATE MARKET                              
         BE    VALK3                                                            
         CLC   FVMSGNO,=AL2(FVFNONE) IF MISSING,                                
         BNE   VALKX                                                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 AVALMKT,BWSKY2H       VALIDATE FROM KEY2 FIELD                   
         BE    *+18                                                             
         CLC   FVMSGNO,=AL2(FVFNONE)                                            
         BE    VALK9                                                            
         B     VALKX                                                            
         MVC   NETMKT(L'QMKT),QMKT   AND PLACE IN MARKET FIELD                  
         OI    NETMKTH+6,FVOXMT                                                 
*                                                                               
VALK3    CLI   APACTN,ACTADD       ACTION=ADD                                   
         BNE   *+12                                                             
         OI    APINDS,APIOKADD     YES                                          
         B     VALKX                                                            
         TM    TWAINDS,TWAITSIN    NO-TEST TSAR INITIALIZED                     
         BZ    VALKX                                                            
         XC    TSARREC(TSPLRECL),TSARREC YES-TEST RECORDS EXIST                 
         LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
         MVI   TRECTYP,TRECSPL                                                  
         MVC   TAGYMD,BAGYMD                                                    
         MVC   TBYR,BBYR                                                        
         MVC   TCAM,BCAM                                                        
         MVC   TMKT,BMKT                                                        
         MVC   TKEYSAVE,TKEY                                                    
         MVI   TSARACT,TSARDH                                                   
         GOTO1 ATSAR,TREC                                                       
         BE    VALK4                                                            
         CLC   FVMSGNO,=AL2(FVTMR)     TEST EOF                                 
         BE    VALK6                                                            
         CLC   FVMSGNO,=AL2(FVFERNF)   TEST EXACT KEY NOT FOUND                 
         BE    VALK4                                                            
         DC    H'0'                                                             
*                                                                               
VALK4    CLC   TKEY(TTYPE-TKEY),TKEYSAVE                                        
         BNE   VALK6                                                            
         OI    APINDS,APIOKDIS+APIOKDEL  OK TO DISPLAY AND DELETE               
*                                                                               
VALK6    MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALKX                                                            
*                                                                               
VALK9    LA    R1,NETMKTH          MISSING MARKET                               
         ST    R1,FVADDR                                                        
*                                                                               
VALKX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALREC   MVI   LFLAG,0                                                          
         MVI   LFLAGPRO,0                                                       
         BAS   RE,DELETE           DELETE ALL CURRENT TSAR RECORDS              
         MVC   SVAGYMD,BAGYMD      SAVE AGENCY/MEDIA                            
         XC    SVSELDOL(4*NMAXWKS),SVSELDOL                                     
         XC    SVNETDOL(4*NMAXWKS),SVNETDOL                                     
         XC    SVSELDEM(4*NMAXWKS),SVSELDEM                                     
         XC    SVSPLDEM(4*NMAXWKS),SVSPLDEM                                     
         XC    SVNETDEM(4*NMAXWKS),SVNETDEM                                     
         XC    SVNSPDEM(4*NMAXWKS),SVNSPDEM                                     
         XC    SPILLSTA(8*NMAXSTA),SPILLSTA                                     
         XC    NETSTA(8*NMAXSTA),NETSTA                                         
*                                                                               
         CLI   CMPPGRPN,0          TEST PRODUCT GROUP                           
         BE    VALR1A                                                           
         MVC   SVPRD,BPRD          (CAN ONLY BE POL X'FF' IF PGRP)              
         BAS   RE,GETPLIST         GET CLIENT RECORD PRODUCT LIST (IO3)         
         BAS   RE,FSTPRD           GET FIRST PRODUCT IN GROUP (IN BPRD)         
*<<TEMP>>BNE   VALR200                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALR1A   BAS   RE,INITSBLK         INITIALIZE SPOTBLOCK                         
*                                                                               
         XC    LAESTIM,LAESTIM                                                  
         OC    CMPELIST,CMPELIST   TEST CAMPAIGN ESTIMATE LIST                  
         BZ    VALR1                                                            
         LA    R1,CMPELIST         YES                                          
         ST    R1,LAESTIM                                                       
         LA    R1,L'CMPELIST(R1)                                                
         ST    R1,LAESTIMX                                                      
         MVC   SVEST,BEST                                                       
*                                                                               
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
* 1- WEEKLY SPILL FROM SELECTIVE TV SCHEDULING IN NWS                           
* 2- SELECTIVE TV ALREADY PURCHASED IN BUY                                      
* 3- NETWORK SCHEDULED IN BUY / NETWORK SPILL FROM OTHER MARKETS IN BUY         
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>          
         SPACE                                                                  
VALR1    OI    LFLAG,LSPILLQ+LNETQ+LSELCTQ  READ SPILL/NWK/SELECT               
         MVI   LFLAGPRO,LSPILLQ    START WITH SPILL                             
*                                                                               
*                                  *** SKIP BUY SPILL                           
         BAS   RE,GETSPILL         *** AND GET NWS SPILL                        
         B     VALR100             ***                                          
*                                                                               
VALR2    XC    BUYKEY,BUYKEY       BUILD BUY RECORD KEY                         
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,BCLT                                                     
         MVC   BUYKPRD,BPRD                                                     
         CLI   CMPPRD1,0           TEST PIGGYBACK PAIR                          
         BE    *+10                                                             
         MVC   BUYKPRD,CMPPRD1     YES-USE FIRST PRD OF PAIR                    
         MVC   BUYMSTA(2),BMKT                                                  
*                                                                               
VALR4    LA    R1,DIRHI+IO1                                                     
         B     VALR6+4                                                          
*                                                                               
VALR6    LA    R1,DIRSQ+IO1        READ NEXT BUY RECORD                         
         GOTO1 AIO                                                              
         LA    R2,IOKEY                                                         
         CLC   BUYKEY(BUYMSTA+2-BUYKEY),IOKEYSAV                                
         BNE   VALR100                                                          
         CLC   BUYKEST,BEST        CHECK THE ESTIMATE                           
         BE    VALR8                                                            
         BH    *+20                                                             
         MVC   BUYKEST,BEST        LOW-SKIP TO ESTIMATE                         
         XC    BUYKBUY,BUYKBUY                                                  
         B     VALR4                                                            
         MVC   BUYKEST(4),XFF      HIGH-SKIP TO NEXT STATION                    
         B     VALR4                                                            
*                                                                               
VALR8    EQU   *                                                                
*        TM    LFLAGPRO,LSPILLQ    TEST SPILL ONLY                              
*        BZ    *+12                                                             
*        CLI   BUYKBUY,X'80'       YES-TEST SPILL POINTER                       
*        BNE   VALR6                                                            
         GOTO1 AIO,FILGET1         GET THE RECORD                               
         BNE   VALR6                                                            
*                                                                               
         TM    LFLAGPRO,LSELCTQ    SELECTIVE COULD BE IN NWS AND BUY            
         BZ    *+16                                                             
         BAS   RE,CHKTRANS         CHECK IF TRANSFERRED FROM NWS TO BUY         
         TM    LSELFLAG,LSELIGNQ                                                
         BNZ   VALR6               DON'T WANT IT                                
*                                                                               
         L     R2,AIOAREA1                                                      
         MVC   SBBMKT,BUYMSTA      SET ORIGINATING MARKET                       
*                                                                               
VALR10   GOTO1 VSPOTBUY,APPARM,SBLOCK   *** CALL SPOTBUY ***                    
*                                                                               
         L     R3,SBACHUNK                                                      
         USING SCHUNKD,R3                                                       
         XC    LSLNS,LSLNS         BUILD LIST OF SPOT LENGTHS                   
*                                                                               
VALR12   OC    SCNEXT,SCNEXT                                                    
         BZ    VALR18                                                           
         ZIC   RE,SCSLN1                                                        
         SR    RF,RF                                                            
         ICM   RF,1,SCSLN2                                                      
         BZ    *+6                                                              
         AR    RE,RF                                                            
         LA    R0,L'LSLNS                                                       
         LA    R1,LSLNS                                                         
*                                                                               
VALR14   CLI   0(R1),0                                                          
         BNE   *+12                                                             
         STC   RE,0(R1)                                                         
         B     VALR16                                                           
         CLM   RE,1,0(R1)                                                       
         BE    VALR16                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VALR14                                                        
         DC    H'0'                                                             
*                                                                               
VALR16   L     R3,SCNEXT                                                        
         B     VALR12                                                           
*                                                                               
VALR18   LA    R8,LSLNS            ACCUMULATE DEMOS AND DOLLARS                 
         LA    R9,L'LSLNS          FOR EACH SPOT LENGTH                         
*                                                                               
VALR20   XC    LDEMS(NMAXWKS*4),LDEMS                                           
         XC    LDOLS(NMAXWKS*4),LDOLS                                           
         CLI   0(R8),0                                                          
         BE    VALR50                                                           
         L     R3,SBACHUNK                                                      
*                                                                               
VALR25   OC    SCNEXT,SCNEXT                                                    
         BZ    VALR50                                                           
         CLI   CMPPRD1,0           TEST PIGGYBACKS                              
         BNE   VALR30              - YES                                        
*>>>>                                                                           
         CLI   BPRD,X'FF'          POL - SKIP PIG CHECK                         
         BE    *+14                                                             
         OC    SCPRD2,SCPRD2       SINGLE PRODUCT, IS SPOT FOR A P/B            
         BNZ   VALR40              YES - DONT INCLUDE (GOALS DOESN'T!)          
*>>>>                                                                           
         CLC   SCPRD1,BPRD         NO-CHECK THE PRODUCT                         
         BE    VALR35                                                           
         CLI   SCPRD1,X'FE'        TEST UNALLOCATED                             
         BNE   VALR40                                                           
         CLI   SBEUNALL,C'Y'       YES-ARE WE ALLOWING THEM?                    
         BE    VALR35                                                           
         B     VALR40                                                           
*                                                                               
VALR30   CLC   SCPRD1,CMPPRD1      YES-TEST CHUNK IS FOR PIGGYBACK PAIR         
         BNE   VALR40                                                           
         CLC   SCPRD2,CMPPRD2                                                   
         BNE   VALR40                                                           
*                                                                               
VALR35   ZIC   RE,SCSLN1           TEST CHUNK IS FOR CURRENT SPOT LEN           
         SR    RF,RF                                                            
         ICM   RF,1,SCSLN2                                                      
         BZ    *+6                                                              
         AR    RE,RF                                                            
         CLM   RE,1,0(R8)                                                       
         BNE   VALR40                                                           
         L     RE,SBADATE          FIND THE WEEK                                
         L     R0,SBNDATES                                                      
         SR    R1,R1                                                            
         CLC   SCDATE,2(RE)                                                     
         BNH   *+18                                                             
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-18                                                          
         DC    H'0'                                                             
         LA    RF,LDEMS(R1)        DEMO VALUE                                   
         L     RE,SCDEMOS                                                       
         A     RE,0(RF)                                                         
         ST    RE,0(RF)                                                         
         CLC   SBBMKT,SBEMKT       TEST SPILL                                   
         BNE   VALR40              NO-NO DOLLARS                                
         LA    RF,LDOLS(R1)        DOLLAR VALUE                                 
         L     RE,SCGROSS                                                       
         A     RE,0(RF)                                                         
         ST    RE,0(RF)                                                         
*                                                                               
VALR40   L     R3,SCNEXT           NEXT CHUNK                                   
         B     VALR25                                                           
*                                                                               
VALR50   OC    SBACONT,SBACONT     ANY BUY CONTINUATION?                        
         BNZ   VALR10               YES - GO GET THE REST                       
         CLI   LSLNS,0                                                          
         BE    VALR90                                                           
*                                                                               
         TM    LFLAGPRO,LSELCTQ    SELECTIVE COULD BE IN NWS AND BUY            
         BZ    VALR55                                                           
         TM    LSELFLAG,LSELMINQ   ANYTHING TO MINUS OUT ?                      
         BZ    VALR55                                                           
         BAS   RE,MINUSNWS                                                      
         XC    LSELFLAG,LSELFLAG  ENSURE DON'T REMOVE AGAIN                     
*                                                                               
VALR55   OC    LDEMS(NMAXWKS*4),LDEMS    TEST ANY DATA                          
         BNZ   *+14                                                             
         OC    LDOLS(NMAXWKS*4),LDOLS                                           
         BZ    VALR80                                                           
         GOTO1 TOTAL,LDEMS         YES-TOTAL THE DEMOS AND DOLLARS              
         ST    R1,LDEMTOT                                                       
*>>>     LA    R4,SVSPLDEM         <<< CAN'T GET SPILL HERE                     
*>>>     TM    LFLAGPRO,LSPILLQ    <<< IT IS DONE FROM NWS NOT                  
*>>>     BO    VALR60              <<< BUY RECORDS                              
         LA    R4,SVSELDEM                                                      
         TM    LFLAGPRO,LSELCTQ                                                 
         BNZ   VALR60                                                           
         LA    R4,SVNETDEM                                                      
         CLC   SBBMKT,SBEMKT                                                    
         BE    VALR60                                                           
         LA    R4,SVNSPDEM                                                      
*                                                                               
VALR60   GOTO1 ACCUM,APPARM,LDEMS,(R4)                                          
         XC    LDOLTOT,LDOLTOT                                                  
         TM    LFLAGPRO,LSELCTQ     WANT SELECTIVE TV DOLLARS                   
         BNZ   VALR65                                                           
         CLC   SBBMKT,SBEMKT                                                    
         BNE   VALR70                                                           
         GOTO1 TOTAL,LDOLS                                                      
         ST    R1,LDOLTOT                                                       
         GOTO1 ACCUM,APPARM,LDOLS,SVNETDOL                                      
         B     VALR70                                                           
*                                                                               
VALR65   GOTO1 TOTAL,LDOLS                                                      
         ST    R1,LDOLTOT                                                       
         GOTO1 ACCUM,APPARM,LDOLS,SVSELDOL                                      
*                                                                               
VALR70   XC    TSARREC(TSPLRECL),TSARREC  BUILD A TSAR RECORD                   
         LA    R4,TSARREC                                                       
         USING TREC,R4                                                          
         MVC   TRECL,=Y(TSPLRECL)                                               
         MVI   TRECTYP,TRECSPL                                                  
         MVC   TAGYMD,SVAGYMD                                                   
         MVC   TBYR,BBYR                                                        
         MVC   TCAM,BCAM                                                        
         MVC   TMKT,BMKT                                                        
         MVI   TTYPE,C'L'          TYPE=SELECTIVE                               
         TM    LFLAGPRO,LSELCTQ                                                 
         BNZ   VALR75                                                           
         MVI   TTYPE,C'N'          TYPE=NETWORK                                 
         CLC   SBEMKT,SBBMKT                                                    
         BE    *+8                                                              
         MVI   TTYPE,C'S'          TYPE=NETWORK SPILL                           
VALR75   MVC   TDPT,BDDAYPT                                                     
         MVC   TSLN,0(R8)                                                       
         BAS   RE,ADDREC           ADD TO TSAR                                  
*                                                                               
VALR80   LA    R8,1(R8)            NEXT SPOT LENGTH                             
         BCT   R9,VALR20                                                        
*                                                                               
VALR90   TM    LFLAGPRO,LSELCTQ                                                 
         BNZ   *+8                                                              
         BAS   RE,SAVESTA          SAVE SPILL STATIONS                          
         B     VALR6               NEXT BUY RECORD                              
*                                                                               
VALR100  TM    LFLAGPRO,LSPILLQ    TEST JUST DONE SPOT SPILL                    
         BZ    VALR105                                                          
         NI    LFLAG,255-LSPILLQ   YES                                          
VALR105  TM    LFLAG,LSELCTQ       TEST SELECTIVE BUYS WANTED                   
         BZ    VALR110                                                          
         TM    LFLAGPRO,LSELCTQ    JUST DONE ?                                  
         BNZ   VALR110             - YEP                                        
         MVI   LFLAGPRO,LSELCTQ    DO SELECTIVE                                 
         NI    LFLAG,255-LSELCTQ                                                
         B     VALR2                                                            
*                                                                               
VALR110  TM    LFLAG,LNETQ         TEST NETWORK BUYS WANTED                     
         BZ    VALR120                                                          
         TM    LFLAGPRO,LNETQ      JUST DONE ?                                  
         BNZ   VALR120             - YEP                                        
         MVI   LFLAGPRO,LNETQ      DO NETWORK                                   
         NI    BAGYMD,X'F0'        SWITCH MEDIA TO NETWORK                      
         OI    BAGYMD,X'03'                                                     
         B     VALR2                                                            
*                                                                               
VALR120  ICM   R1,15,LAESTIM       TEST CAMPAIGN ESTIMATE LIST                  
         BZ    VALR170                                                          
         C     R1,LAESTIMX         YES-                                         
         BNL   VALR160                                                          
         CLI   0(R1),0                                                          
         BE    VALR160                                                          
*                                                                               
         CLC   0(2,R1),=AL2(CESTALLQ)   TEST ALL ESTIMATES REQUIRED             
         BE    VALR150                                                          
         MVC   BEST,0(R1)          GO BACK AND DO FOR NEXT ESTIMATE             
         LA    R1,1(R1)            IN THE LIST                                  
         ST    R1,LAESTIM                                                       
VALR130  MVC   BAGYMD,SVAGYMD                                                   
         OI    LFLAG,LSPILLQ+LNETQ+LSELCTQ                                      
         MVI   LFLAGPRO,0                                                       
         B     VALR100                                                          
*                                                                               
*                              <<< START OF 'ALL' ESTIMATES CODE >>>            
VALR150  CLC   BEST,SVEST          HANDLE 'ALL' ESTIMATES                       
         BNE   VALR152                                                          
         MVI   BEST,0              JUST DONE CAMPAIGN EST, PRIME ALL            
VALR152  CLI   BEST,255                                                         
         BNL   VALR160             DONE 1-255                                   
         SR    R1,R1                                                            
         IC    R1,BEST                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BEST             TRY NEXT POSSIBLE ESTIMATE NUMBER            
         CLC   BEST,SVEST                                                       
         BE    VALR152             ALREADY DONE CAMPAIGN ESTIMATE               
*                                  VALIDATE FOR CAMPAIGN ON THE FLY             
         LA    R2,IOKEY            BUILD KEY OF ESTIMATE RECORD                 
         USING ESTHDRD,R2                                                       
         XC    EKEY,EKEY                                                        
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,BEST                                                     
         GOTO1 AIO,FILRD+IO1                                                    
         BNE   VALR152                                                          
*                                                                               
         TM    CLTIND2,CLTIEST     TEST CAMP AND EST DATES MUST MATCH           
         BZ    VALR155                                                          
         GOTO1 VDATCON,APPARM,(3,CMPST),APWORK   YES-                           
         GOTO1 (RF),(R1),(3,CMPND),APWORK+6                                     
         SR    R1,R1                                                            
         L     R2,AIOAREA1                                                      
         CLC   ESTART,APWORK                                                    
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
         CLC   EEND,APWORK+6                                                    
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
         LTR   R1,R1               TEST START OR END MATCH                      
         BZ    VALR152             NO-ERROR                                     
         OC    CMPCCAM,CMPCCAM     YES-TEST COMPANION CAMPAIGN                  
         BNZ   VALR155             YES-OK                                       
         CHI   R1,2                NO-BOTH MUST MATCH                           
         BNE   VALR152             NOT VALID/FOR CAM PERIOD ETC                 
VALR155  LA    R2,IOKEY            RESTORE R2 FOR BUYKREC                       
         B     VALR130             DO GOALS FOR THIS ESTIMATE                   
*                              >>> END OF 'ALL' ESTIMATES CODE <<<              
*                                                                               
VALR160  MVC   BEST,SVEST          RESTORE CAMPAIGN ESTIMATE                    
*                                                                               
VALR170  CLI   CMPPGRPN,0          TEST PRODUCT GROUP                           
         BE    VALR200                                                          
         BAS   RE,NXTPRD           GET NEXT PRODUCT IN GROUP (IN BPRD)          
         BNE   VALR190                                                          
         BAS   RE,INITSBLK         RE-INITIALIZE SPOTBLOCK FOR THIS PRD         
*                                                                               
         XC    LAESTIM,LAESTIM                                                  
         OC    CMPELIST,CMPELIST   TEST CAMPAIGN ESTIMATE LIST                  
         BZ    VALR180                                                          
         LA    R1,CMPELIST         YES - RESET SO PROCESS ALL ESTS              
         ST    R1,LAESTIM                                                       
         LA    R1,L'CMPELIST(R1)                                                
         ST    R1,LAESTIMX                                                      
         MVC   SVEST,BEST                                                       
VALR180  B     VALR130             ROUND AGAIN FOR THIS PRODUCT                 
*                                                                               
VALR190  MVC   BPRD,SVPRD          RESTORE CAMPAIGN PRODUCT                     
*                                                                               
VALR200  XC    LDEMS(NMAXWKS*4),LDEMS   FORMAT TO SCREEN                        
         XC    LDOLS(NMAXWKS*4),LDOLS                                           
         GOTO1 ACCUM,APPARM,SVNETDOL,LDOLS     ACCUM TOTAL GROSS DOLS           
         GOTO1 ACCUM,APPARM,SVSELDOL,LDOLS                                      
         LA    R2,NETLN1H                                                       
         LA    R5,SVSELDEM                                                      
         LA    R6,LDOLS                                                         
         LA    R8,LDEMS                                                         
         L     R9,SBADATE                                                       
         L     R0,SBNDATES                                                      
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         MVI   EBLOUT,L'SNET                                                    
         MVI   EBOPT,X'20'                                                      
*                                                                               
VALR210  OI    6(R2),FVOXMT                                                     
         LA    R2,NETLN1-NETLN1H(R2)                                            
         XC    0(L'NETLN1,R2),0(R2)                                             
         USING SCREEND,R2                                                       
         GOTO1 VDATCON,APPARM,(2,(R9)),(4,SWEEK)                                
         LA    R1,SSELCT           SELECTIVE DEMOS                              
         ST    R1,EBAOUT                                                        
         ST    R5,EBAIN                                                         
         MVI   EBDECS,1                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R1,SSPILL           SPILL DEMOS                                  
         ST    R1,EBAOUT                                                        
         LA    R1,SVSPLDEM-SVSELDEM(R5)                                         
         ST    R1,EBAIN                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R1,SNET             NETWORK DEMOS                                
         ST    R1,EBAOUT                                                        
         LA    R1,SVNETDEM-SVSELDEM(R5)                                         
         ST    R1,EBAIN                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R1,SNETSPL                                                       
         ST    R1,EBAOUT           NETWORK SPILL                                
         LA    R1,SVNSPDEM-SVSELDEM(R5)                                         
         ST    R1,EBAIN                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         L     R1,0(R5)            DEMO TOTAL                                   
         A     R1,SVSPLDEM-SVSELDEM(R5)                                         
         A     R1,SVNETDEM-SVSELDEM(R5)                                         
         A     R1,SVNSPDEM-SVSELDEM(R5)                                         
         ST    R1,0(R8)                                                         
         LA    R1,SDEMTOT                                                       
         ST    R1,EBAOUT                                                        
         ST    R8,EBAIN                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R1,SCOST                                                         
         ST    R1,EBAOUT           DOLLARS                                      
         ST    R6,EBAIN                                                         
         MVI   EBDECS,2                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R2,NETLN2H-NETLN1(R2)                                            
         LA    R5,4(R5)                                                         
         LA    R6,4(R6)                                                         
         LA    R8,4(R8)                                                         
         LA    R9,4(R9)                                                         
         BCT   R0,VALR210          NEXT WEEK                                    
         XC    LDOLS(NMAXWKS*4),LDOLS                                           
*                                                                               
         L     R5,ATWA                                                          
         GOTO1 TOTAL,LDEMS         DEMO AND COST TOTALS                         
         ST    R1,LDEMTOT                                                       
         GOTO1 TOTAL,SVNETDOL                                                   
         ST    R1,SVNETDOT                                                      
         ST    R1,LDOLTOT                                                       
         GOTO1 TOTAL,SVSELDOL                                                   
         ST    R1,SVSELDOT                                                      
         A     R1,LDOLTOT                                                       
         ST    R1,LDOLTOT                                                       
         GOTO1 TOTAL,SVSELDEM                                                   
         ST    R1,SVSELTOT                                                      
         GOTO1 TOTAL,SVSPLDEM                                                   
         ST    R1,SVSPLTOT                                                      
         GOTO1 TOTAL,SVNETDEM                                                   
         ST    R1,SVNETTOT                                                      
         GOTO1 TOTAL,SVNSPDEM                                                   
         ST    R1,SVNSPTOT                                                      
         OI    6(R2),FVOXMT        FORMAT TOTAL LINE                            
         LA    R2,NETLN1-NETLN1H(R2)                                            
         MVC   SWEEK,=C'*ALL*'                                                  
         LA    R1,SVSELTOT                                                      
         ST    R1,EBAIN                                                         
         LA    R1,SSELCT                                                        
         ST    R1,EBAOUT                                                        
         MVI   EBDECS,1                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R1,SVSPLTOT                                                      
         ST    R1,EBAIN                                                         
         LA    R1,SSPILL                                                        
         ST    R1,EBAOUT                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R1,SVNETTOT                                                      
         ST    R1,EBAIN                                                         
         LA    R1,SNET                                                          
         ST    R1,EBAOUT                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R1,SVNSPTOT                                                      
         ST    R1,EBAIN                                                         
         LA    R1,SNETSPL                                                       
         ST    R1,EBAOUT                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R1,LDEMTOT                                                       
         ST    R1,EBAIN                                                         
         LA    R1,SDEMTOT                                                       
         ST    R1,EBAOUT                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R1,LDOLTOT                                                       
         ST    R1,EBAIN                                                         
         LA    R1,SCOST                                                         
         ST    R1,EBAOUT                                                        
         MVI   EBDECS,2                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
         LA    R3,NMAXWKS          CLEAR REMAINING LINES                        
         S     R3,SBNDATES                                                      
         BNP   VALR230                                                          
*                                                                               
VALR220  LA    R2,NETLN2H-NETLN1(R2)                                            
         OI    6(R2),FVOXMT                                                     
         LA    R2,NETLN1-NETLN1H(R2)                                            
         XC    0(L'NETLN1,R2),0(R2)                                             
         BCT   R3,VALR220                                                       
*                                                                               
VALR230  XC    TSARREC(TSPLRECL),TSARREC  BUILD TSAR RECORD FOR TOTALS          
         LA    R4,TSARREC                                                       
         USING TREC,R4                                                          
         MVC   TRECL,=Y(TSPLRECL)                                               
         MVI   TRECTYP,TRECSPL                                                  
         MVC   TAGYMD,SVAGYMD                                                   
         MVC   TBYR,BBYR                                                        
         MVC   TCAM,BCAM                                                        
         MVC   TMKT,BMKT                                                        
         MVI   TTYPE,C'N'          ADD NETWORK TOTAL RECORD                     
         MVC   TDEMTOT,SVNETTOT                                                 
         MVC   TDEMS(4*NMAXWKS),SVNETDEM                                        
         MVC   TCOST,SVNETDOT                                                   
         MVC   TDOLS(4*NMAXWKS),SVNETDOL                                        
         MVI   TSARACT,TSAADD                                                   
         GOTO1 ATSAR,TREC                                                       
         BE    VALR240                                                          
         DC    H'0'                                                             
*                                                                               
VALR240  XC    TCOST,TCOST                                                      
         XC    TDOLS(4*NMAXWKS),TDOLS                                           
         OC    SVNSPTOT,SVNSPTOT                                                
         BZ    VALR245                                                          
         MVI   TTYPE,C'S'          ADD NETWORK SPILL TOTAL RECORD               
         MVC   TDEMTOT,SVNSPTOT                                                 
         MVC   TDEMS(4*NMAXWKS),SVNSPDEM                                        
         GOTO1 ATSAR,TREC                                                       
         BE    VALR245                                                          
         DC    H'0'                                                             
*                                                                               
VALR245  OC    SVSPLTOT,SVSPLTOT                                                
         BZ    VALR250                                                          
         MVI   TTYPE,C'T'          ADD TV SPILL TOTAL RECORD                    
         MVC   TDEMTOT,SVSPLTOT                                                 
         MVC   TDEMS(4*NMAXWKS),SVSPLDEM                                        
         GOTO1 ATSAR,TREC                                                       
         BE    VALR250                                                          
         DC    H'0'                                                             
*                                                                               
VALR250  OC    SVSELTOT,SVSELTOT                                                
         BZ    VALR255                                                          
         MVI   TTYPE,C'L'          ADD SELECTIVE TV TOTAL RECORD                
         MVC   TDEMTOT,SVSELTOT                                                 
         MVC   TDEMS(4*NMAXWKS),SVSELDEM                                        
         MVC   TCOST,SVSELDOT                                                   
         MVC   TDOLS(4*NMAXWKS),SVSELDOL                                        
         GOTO1 ATSAR,TREC                                                       
         BE    VALR255                                                          
         DC    H'0'                                                             
*                                                                               
VALR255  XC    NETCMT,NETCMT       COMMENT LINE FOR SPILL STATIONS              
         OI    NETCMTH+6,FVOXMT                                                 
         LA    R8,SPILLSTA                                                      
         LA    RF,NMAXSTA                                                       
         LA    R2,APELEM                                                        
         XC    APELEM,APELEM                                                    
*                                                                               
VALR260  OC    0(8,R8),0(R8)                                                    
         BZ    VALR270                                                          
         MVC   0(4,R2),0(R8)                                                    
         LA    R2,3(R2)                                                         
         CLI   0(R2),C' '                                                       
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         LA    R8,8(R8)                                                         
         BCT   RF,VALR260                                                       
*                                                                               
VALR270  LA    R8,NETSTA                                                        
         LA    RF,NMAXSTA                                                       
*                                                                               
VALR280  OC    0(8,R8),0(R8)                                                    
         BZ    VALR290                                                          
         MVC   0(4,R2),0(R8)                                                    
         LA    R2,3(R2)                                                         
         CLI   0(R2),C' '                                                       
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'/'                                                       
         LA    R2,1(R2)                                                         
         MVC   0(4,R2),4(R8)                                                    
         LA    R2,3(R2)                                                         
         CLI   0(R2),C' '                                                       
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         LA    R8,8(R8)                                                         
         BCT   RF,VALR280                                                       
*                                                                               
VALR290  LA    R1,APELEM                                                        
         SR    R2,R1                                                            
         BNP   VALRX                                                            
         BCTR  R2,0                                                             
         BCTR  R2,0                                                             
         LA    R1,L'NETCMT-1                                                    
         CR    R2,R1                                                            
         BNH   *+6                                                              
         LR    R2,R1                                                            
         EX    R2,*+4                                                           
         MVC   NETCMT(0),APELEM                                                 
         OI    NETCMTH+6,FVOXMT                                                 
*                                                                               
VALRX    MVC   BAGYMD,SVAGYMD                                                   
         MVC   CMPDATSP(2),LCMPST                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NWS SPILL FROM CAMPAIGN/MARKET HEADER RECORD                    *         
***********************************************************************         
         SPACE 1                                                                
GETSPILL NTR1  ,                                                                
         XC    LDOLTOT,LDOLTOT     CLEAR DOLLARS                                
         XC    LDOLS(NMAXWKS*4),LDOLS                                           
         XC    IOKEY,IOKEY         BUILD HEADER KEY                             
         LA    R2,IOKEY                                                         
         USING BWHRECD,R2                                                       
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,SVAGYMD                                                 
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         MVC   BWHKCAM,BCAM                                                     
         MVC   BWHKMKT,BMKT                                                     
         GOTO1 AIO,DIRHI+IO1                                                    
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSX                                                            
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                   
         BNE   GETSX                                                            
         GOTO1 AIO,FILGET1                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         LA    R3,BWHEL                                                         
*                                                                               
GETS2    CLI   0(R3),0                                                          
         BE    GETS10                                                           
         CLI   0(R3),SPLELCDQ                                                   
         BNE   GETS4                                                            
         USING SPLELD,R3                                                        
         XC    LDEMS(NMAXWKS*4),LDEMS                                           
         ZIC   RE,SPLELLN                                                       
         SH    RE,=Y(SPLDEMS-SPLELD)                                            
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BM    GETS4                                                            
         EX    RE,*+4                                                           
         MVC   LDEMS(0),SPLDEMS                                                 
         GOTO1 TOTAL,LDEMS                                                      
         ST    R1,LDEMTOT                                                       
         GOTO1 ACCUM,APPARM,LDEMS,SVSPLDEM                                      
*                                                                               
         XC    TSARREC(TSPLRECL),TSARREC  BUILD A TSAR RECORD                   
         LA    R4,TSARREC                                                       
         USING TREC,R4                                                          
         MVC   TRECL,=Y(TSPLRECL)                                               
         MVI   TRECTYP,TRECSPL                                                  
         MVC   TAGYMD,SVAGYMD                                                   
         MVC   TBYR,BBYR                                                        
         MVC   TCAM,BCAM                                                        
         MVC   TMKT,BMKT                                                        
         MVI   TTYPE,C'T'          TYPE=TV SPILL                                
         MVC   TDPT,SPLDPT                                                      
         MVC   TSLN,SPLSLN                                                      
         BAS   RE,ADDREC           ADD SPILL RECORD TO TSAR                     
*                                                                               
         XC    APWORK,APWORK       SAVE SPILL STATION                           
         MVC   APWORK(4),SPLSTA                                                 
         LA    R8,SPILLSTA                                                      
         LA    RF,NMAXSTA                                                       
*                                                                               
GETS3    OC    0(8,R8),0(R8)                                                    
         BNZ   *+14                                                             
         MVC   0(8,R8),APWORK                                                   
         B     GETS4                                                            
         CLC   0(8,R8),APWORK                                                   
         BE    GETS4                                                            
         LA    R8,8(R8)                                                         
         BCT   RF,GETS3                                                         
         DC    H'0'                                                             
*                                                                               
GETS4    ZIC   R0,1(R3)            NEXT SPILL ELEMENT                           
         AR    R3,R0                                                            
         B     GETS2                                                            
*                                                                               
GETS10   MVC   BCMSEQ,BWHKSEQ      ENSURE SET FOR SELECTIVE TV EXTRACT          
         XC    LCCMSEQ,LCCMSEQ                                                  
         OC    CMPCCAM,CMPCCAM                                                  
         BZ    GETSX                                                            
         XC    IOKEY,IOKEY         ALSO GET ANY COMPANION CAMPAIGN SEQ          
         LA    R2,IOKEY                                                         
         USING BWHRECD,R2                                                       
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,SVAGYMD                                                 
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         MVC   BWHKCAM,CMPCCAM                                                  
         MVC   BWHKMKT,BMKT                                                     
         GOTO1 AIO,DIRHI+IO1                                                    
         BNE   GETSX                                                            
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                   
         BNE   GETSX                                                            
         MVC   LCCMSEQ,BWHKSEQ                                                  
GETSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SAVE SPILL STATIONS                                                 *         
***********************************************************************         
         SPACE 1                                                                
SAVESTA  LR    R0,RE                                                            
         L     R2,AIOAREA1                                                      
         USING BUYRECD,R2                                                       
         XC    APWORK,APWORK                                                    
         LA    R8,SPILLSTA                                                      
         TM    LFLAGPRO,LSPILLQ    TEST TV SPILL                                
         BO    SAVES4                                                           
         CLC   SBEMKT,SBBMKT       NETWORK-TEST FOR SPILL                       
         BE    SAVESX              NO                                           
         LA    R8,NETSTA           YES-LOOK FOR CANADIAN NETWORK                
         LA    R1,BDELEM               STATION ELEMENT                          
         SR    RF,RF                                                            
*                                                                               
SAVES2   CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),X'68'                                                      
         BE    *+14                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     SAVES2                                                           
         MVC   APWORK+4(4),2(R1)                                                
*                                                                               
SAVES4   GOTO1 VMSUNPK,APPARM,BUYMSTA,APFULL,APDUB                              
         MVC   APWORK(4),APDUB                                                  
         LA    RF,NMAXSTA          ADD STATION(/NETWORK) TO STATION             
*                                  TABLES                                       
SAVES6   OC    0(8,R8),0(R8)                                                    
         BNZ   *+14                                                             
         MVC   0(8,R8),APWORK                                                   
         B     SAVESX                                                           
         CLC   APWORK(8),0(R8)                                                  
         BE    SAVESX                                                           
         LA    R8,8(R8)                                                         
         BCT   RF,SAVES6                                                        
         DC    H'0'                                                             
*                                                                               
SAVESX   LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE SPOT BLOCK                                               *         
***********************************************************************         
         SPACE 1                                                                
INITSBLK NTR1  ,                                                                
         LA    RE,SBLOCK                                                        
         LA    RF,SBLOCKX-SBLOCK                                                
         XCEF  ,                                                                
         MVC   SBCOMFAC,ACOM                                                    
         MVC   SBAIO1,AIOAREA1                                                  
         MVC   SBUSERID,CUUSER                                                  
         LA    R1,CMPDATSP                                                      
         ST    R1,SBADATE          CAMPAIGN DATES                               
         MVC   LCMPST,CMPDATSP                                                  
         TM    CMPOPTS,CAMOWKS     TEST NON-CONTIGUOUS FLIGHT WEEKS             
         BO    INIT2                                                            
         GOTO1 VDATCON,APPARM,(3,CMPST),(2,CMPDATSP)   NO-MAKE SURE OF          
*                                                         START DATE            
INIT2    MVC   SBNDATES+3(1),CMPNWKS                                            
         MVC   SBACHUNK,AIOAREA2                                                
         MVC   SBASPTTB,LAWORK                                                  
         MVC   SBLSPTTB,=F'10000'                                               
         MVC   SBQPRD,QPRD                                                      
         MVC   SBQBPRD,BPRD                                                     
         MVC   SBEDEMOS(3),ESTDEMS                                              
         MVI   SBENDEM,1                                                        
         MVC   SBEMKT,BMKT                                                      
         MVI   SBEDEMTY,C'P'                                                    
         MVI   SBEBYSLN,C'Y'                                                    
         MVC   SBEPRD,BPRD         CAMPAIGN PRODUCT                             
         OI    SBEFLAG2,SBESPLBY   TELL SPOTBUY TO SPLIT BIG BUYS               
         CLI   CMPPRD1,0           TEST PIGGYBACKS                              
         BE    *+18                                                             
         MVC   SBEPRD,CMPPRD1      YES-PASS FIRST PIGGYBACK PRODUCT             
INIT5    MVI   SBESPLIT,C'N'           AND DON'T SPLIT PAIRS                    
         B     INITX                                                            
         CLI   BPRD,X'FF'          NO-TEST CAMPAIGN PRODUCT = POL               
*        BNE   INITX                                                            
*>>>                                                                            
         BNE   INIT5               (WANT TO DETECT P/B SPOTS SGL BRD)           
*>>>                                                                            
         MVI   SBEUNALL,C'Y'       YES-INCLUDE UNALLOCATED SPOTS                
*                                                                               
INITX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TOTAL ROUTINE                                                       *         
* INPUT  : R1=A(VALUES BY WEEK)                                       *         
* OUTPUT : R1=TOTAL                                                   *         
***********************************************************************         
         SPACE 1                                                                
TOTAL    LR    R0,RE                                                            
         SR    RE,RE                                                            
         LA    RF,NMAXWKS                                                       
         A     RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   RF,*-8                                                           
         LR    R1,RE                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISREC   XC    SVNETTOT,SVNETTOT                                                
         XC    SVNSPTOT,SVNSPTOT                                                
         XC    SVSPLTOT,SVSPLTOT                                                
         XC    SVSELTOT,SVSELTOT                                                
         XC    LDOLTOT,LDOLTOT                                                  
         XC    SVNETDEM(4*NMAXWKS),SVNETDEM                                     
         XC    SVNSPDEM(4*NMAXWKS),SVNSPDEM                                     
         XC    SVSPLDEM(4*NMAXWKS),SVSPLDEM                                     
         XC    SVSELDEM(4*NMAXWKS),SVSELDEM                                     
         XC    LDOLS(4*NMAXWKS),LDOLS                                           
         XC    TSARREC(TSPLRECL),TSARREC                                        
         LA    R4,TSARREC                                                       
         USING TREC,R4                                                          
         MVI   TRECTYP,TRECSPL                                                  
         MVC   TAGYMD,BAGYMD                                                    
         MVC   TBYR,BBYR                                                        
         MVC   TCAM,BCAM                                                        
         MVC   TMKT,BMKT                                                        
         MVI   TTYPE,C'N'                                                       
         MVC   TKEYSAVE,TKEY                                                    
         MVI   TSARACT,TSARDH                                                   
         GOTO1 ATSAR,TREC          READ NETWORK TOTAL RECORD                    
         BNE   DISR2                                                            
         MVC   SVNETDEM(4*NMAXWKS),TDEMS                                        
         MVC   SVNETTOT,TDEMTOT                                                 
         MVC   LDOLS(4*NMAXWKS),TDOLS                                           
         MVC   LDOLTOT,TCOST                                                    
*                                                                               
DISR2    MVC   TKEY,TKEYSAVE                                                    
         MVC   FVFERN,=AL2(FVFOK)                                               
         MVI   TTYPE,C'L'                                                       
         MVI   TSARACT,TSARDH                                                   
         GOTO1 ATSAR,TREC          READ SELECTIVE TOTAL RECORD                  
         BNE   DISR2A                                                           
         MVC   SVSELDEM(4*NMAXWKS),TDEMS                                        
         MVC   SVSELTOT,TDEMTOT                                                 
         GOTO1 ACCUM,APPARM,TDOLS,LDOLS  ADD DOLLARS TO NETWORK                 
         L     R1,LDOLTOT                                                       
         A     R1,TCOST                                                         
         ST    R1,LDOLTOT                                                       
*                                                                               
DISR2A   MVC   TKEY,TKEYSAVE                                                    
         MVC   FVFERN,=AL2(FVFOK)                                               
         MVI   TTYPE,C'S'                                                       
         MVI   TSARACT,TSARDH                                                   
         GOTO1 ATSAR,TREC          READ NETWORK SPILL TOTAL RECORD              
         BNE   *+16                                                             
         MVC   SVNSPDEM(4*NMAXWKS),TDEMS                                        
         MVC   SVNSPTOT,TDEMTOT                                                 
*                                                                               
         MVC   TKEY,TKEYSAVE                                                    
         MVC   FVFERN,=AL2(FVFOK)                                               
         MVI   TTYPE,C'T'                                                       
         MVI   TSARACT,TSARDH                                                   
         GOTO1 ATSAR,TREC          READ TV SPILL TOTAL RECORD                   
         BNE   *+16                                                             
         MVC   SVSPLDEM(4*NMAXWKS),TDEMS                                        
         MVC   SVSPLTOT,TDEMTOT                                                 
*                                                                               
         MVC   FVFERN,=AL2(FVFOK)                                               
         BAS   RE,INITSBLK         INITIALIZE SPOT BLOCK                        
         LA    R2,NETLN1H                                                       
         LA    R3,LDOLS            OUTPUT VALUES TO SCREEN                      
         LA    R5,SVSELDEM                                                      
         L     R9,SBADATE                                                       
         L     R0,SBNDATES                                                      
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         MVI   EBLOUT,L'SDEMTOT                                                 
         MVI   EBOPT,X'20'                                                      
*                                                                               
DISR4    OI    6(R2),FVOXMT                                                     
         LA    R2,NETLN1-NETLN1H(R2)                                            
         XC    0(L'NETLN1,R2),0(R2)                                             
         USING SCREEND,R2                                                       
         GOTO1 VDATCON,APPARM,(2,(R9)),(4,SWEEK)                                
         ST    R5,EBAIN                                                         
         L     R8,0(R5)                                                         
         LA    R1,SSELCT                                                        
         ST    R1,EBAOUT                                                        
         MVI   EBDECS,1                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R1,SVSPLDEM-SVSELDEM(R5)                                         
         ST    R1,EBAIN                                                         
         A     R8,0(R1)                                                         
         LA    R1,SSPILL                                                        
         ST    R1,EBAOUT                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R1,SVNETDEM-SVSELDEM(R5)                                         
         ST    R1,EBAIN                                                         
         A     R8,0(R1)                                                         
         LA    R1,SNET                                                          
         ST    R1,EBAOUT                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R1,SVNSPDEM-SVSELDEM(R5)                                         
         ST    R1,EBAIN                                                         
         A     R8,0(R1)                                                         
         LA    R1,SNETSPL                                                       
         ST    R1,EBAOUT                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R1,SDEMTOT                                                       
         ST    R1,EBAOUT                                                        
         ST    R8,APFULL                                                        
         LA    R1,APFULL                                                        
         ST    R1,EBAIN                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R1,SCOST                                                         
         ST    R1,EBAOUT                                                        
         ST    R3,EBAIN                                                         
         MVI   EBDECS,2                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
DISR6    LA    R2,NETLN2H-NETLN1(R2)                                            
         LA    R3,4(R3)                                                         
         LA    R5,4(R5)                                                         
         LA    R9,4(R9)                                                         
         BCT   R0,DISR4            NEXT WEEK                                    
*                                                                               
         L     R5,ATWA                                                          
         OI    6(R2),FVOXMT        FORMAT TOTAL LINE                            
         LA    R2,NETLN1-NETLN1H(R2)                                            
         XC    0(L'NETLN1,R2),0(R2)                                             
         MVC   SWEEK,=C'*ALL*'                                                  
         L     R8,SVSPLTOT                                                      
         LA    R1,SVSPLTOT                                                      
         ST    R1,EBAIN                                                         
         LA    R1,SSPILL                                                        
         ST    R1,EBAOUT                                                        
         MVI   EBDECS,1                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         A     R8,SVNETTOT                                                      
         LA    R1,SVNETTOT                                                      
         ST    R1,EBAIN                                                         
         LA    R1,SNET                                                          
         ST    R1,EBAOUT                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         A     R8,SVNSPTOT                                                      
         LA    R1,SVNSPTOT                                                      
         ST    R1,EBAIN                                                         
         LA    R1,SNETSPL                                                       
         ST    R1,EBAOUT                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         A     R8,SVSELTOT                                                      
         LA    R1,SVSELTOT                                                      
         ST    R1,EBAIN                                                         
         LA    R1,SSELCT                                                        
         ST    R1,EBAOUT                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         ST    R8,APFULL                                                        
         LA    R1,APFULL                                                        
         ST    R1,EBAIN                                                         
         LA    R1,SDEMTOT                                                       
         ST    R1,EBAOUT                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         LA    R1,LDOLTOT                                                       
         ST    R1,EBAIN                                                         
         LA    R1,SCOST                                                         
         ST    R1,EBAOUT                                                        
         MVI   EBDECS,2                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
         LA    R3,NMAXWKS          CLEAR REMAINING LINES                        
         S     R3,SBNDATES                                                      
         BNP   DISRX                                                            
*                                                                               
DISR8    LA    R2,NETLN2H-NETLN1(R2)                                            
         OI    6(R2),FVOXMT                                                     
         LA    R2,NETLN1-NETLN1H(R2)                                            
         XC    0(L'NETLN1,R2),0(R2)                                             
         BCT   R3,DISR8                                                         
*                                                                               
DISRX    XC    NETCMT,NETCMT       COMMENT LINE FOR SPILL STATIONS              
         OI    NETCMTH+6,FVOXMT                                                 
DISRX1   MVC   CMPDATSP(2),LCMPST                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DELETE RECORD                                                       *         
***********************************************************************         
         SPACE 1                                                                
DELREC   BAS   RE,DELETE                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A RECORD TO TSAR                                     *         
***********************************************************************         
         SPACE 1                                                                
ADDREC   NTR1  ,                                                                
         LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
         MVC   TKEYSAVE,TKEY                                                    
         MVI   TSARACT,TSARDH      SEE IF ALREADY EXISTS                        
         GOTO1 ATSAR,TREC                                                       
         BE    ADD2                YES                                          
         MVC   TKEY,TKEYSAVE       NO-MOVE BACK THE KEY                         
         MVC   TCOST,LDOLTOT       AND MOVE THE VALUES                          
         MVC   TDEMTOT,LDEMTOT                                                  
         MVC   TDOLS(4*NMAXWKS),LDOLS                                           
         MVC   TDEMS(4*NMAXWKS),LDEMS                                           
         MVI   TSARACT,TSAADD      ADD TSAR RECORD                              
         B     ADD4                                                             
*                                                                               
ADD2     ICM   R1,15,TCOST         ADD IN THIS RECORD'S VALUES                  
         A     R1,LDOLTOT                                                       
         STCM  R1,15,TCOST                                                      
         ICM   R1,15,TDEMTOT                                                    
         A     R1,LDEMTOT                                                       
         STCM  R1,15,TDEMTOT                                                    
         GOTO1 ACCUM,APPARM,LDOLS,TDOLS                                         
         GOTO1 (RF),(R1),LDEMS,TDEMS                                            
         MVI   TSARACT,TSAWRT      PUT THE RECORD BY KEY                        
*                                                                               
ADD4     GOTO1 ATSAR,TREC                                                       
         BE    ADDX                                                             
         DC    H'0'                                                             
*                                                                               
ADDX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ACCUMULATE WEEKLY VALUES TO TOTAL WEEKLY VALUES                     *         
* PARM1=A(WEEKLY VALUES)                                              *         
* PARM2=A(TOTAL WEEKLY VALUES)                                        *         
***********************************************************************         
         SPACE 1                                                                
ACCUM    NTR1  ,                                                                
         LM    RE,RF,0(R1)                                                      
         LA    R0,NMAXWKS                                                       
*                                                                               
ACCUM2   OC    0(4,RE),0(RE)                                                    
         BZ    ACCUM4                                                           
         L     R1,0(RF)                                                         
         A     R1,0(RE)                                                         
         ST    R1,0(RF)                                                         
*                                                                               
ACCUM4   LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,ACCUM2                                                        
*                                                                               
ACCUMX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE ALL CURRENT TSAR RECORDS                          *         
***********************************************************************         
         SPACE 1                                                                
DELETE   NTR1  ,                                                                
         TM    TWAINDS,TWAITSIN    TEST TSAR INITIALIZED                        
         BZ    DELX                                                             
         LA    R4,TSARREC          YES-READ ALL RECORDS                         
         USING TRECD,R4                                                         
*                                                                               
DEL2     XC    TSARREC(TSPLRECL),TSARREC                                        
         MVI   TRECTYP,TRECSPL                                                  
         MVC   TAGYMD,BAGYMD                                                    
         MVC   TBYR,BBYR                                                        
         MVC   TCAM,BCAM                                                        
         MVC   TMKT,BMKT                                                        
         MVC   TKEYSAVE,TKEY                                                    
         MVI   TSARACT,TSARDH                                                   
         GOTO1 ATSAR,TREC                                                       
         BE    DEL4                                                             
         CLC   FVMSGNO,=AL2(FVTMR)     TEST EOF                                 
         BE    DELX                                                             
         CLC   FVMSGNO,=AL2(FVFERNF)   TEST EXACT KEY NOT FOUND                 
         BE    DEL4                                                             
         DC    H'0'                                                             
*                                                                               
DEL4     CLC   TKEY(TTYPE-TKEY),TKEYSAVE  TEST RECORD FOUND                     
         BNE   DELX                                                             
         MVI   TSARACT,TSADEL      YES-DELETE                                   
         GOTO1 ATSAR,TREC                                                       
         BE    DEL2                                                             
         DC    H'0'                                                             
*                                                                               
DELX     MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ CLIENT HEADER TO GET LIST OF PRODUCTS                          *         
* CLIENT HEADER IS READ INTO IOAREA3                                  *         
* NOTE - THIS IS BASED ON A ROOT ROUTINE WHICH OVERLAY CANNOT CALL    *         
***********************************************************************         
         SPACE 1                                                                
GETPLIST LR    R0,RE                                                            
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CLTHDRD,R2                                                       
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         DROP  R2                                                               
         GOTO1 AIO,FILRD+IO3                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET FIRST/NEXT PRODUCT FOR PRODUCT GROUP PROCESSING                 *         
* RETURNS: CC EQ - PRODUCT FOUND AND RETURNED IN BPRD                 *         
*          CC NE - ALL PRODUCTS HAVE BEEN FOUND                       *         
* NOTE - THIS IS BASED ON A ROOT ROUTINE WHICH OVERLAY CANNOT CALL    *         
***********************************************************************         
         SPACE 1                                                                
FSTPRD   LR    R0,RE                                                            
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY            READ PRODUCT GROUP PASSIVES                  
         USING PRGRECD,R2                                                       
         MVC   PRGPTYP,=X'0D81'                                                 
         MVC   PRGPAGMD,BAGYMD                                                  
         MVC   PRGPCLT,BCLT                                                     
         MVC   PRGPID,CMPPGRP                                                   
         MVC   PRGPGRP,CMPPGRP+1                                                
         LA    R1,DIRHI+IO1                                                     
         B     NXTPRD2                                                          
*                                                                               
NXTPRD   LR    R0,RE               GET NEXT PRODUCT                             
         MVC   IOKEY,SVPGKEY                                                    
         GOTO1 AIO,DIRHI+IO1       RE-ESTABLISH READ SEQUENCE                   
         LA    R1,DIRSQ+IO1                                                     
*                                                                               
NXTPRD2  MVC   BPRD,CMPPGRPN                                                    
         NI    BPRD,X'7F'                                                       
         LA    R8,PRGPGRP-PRGKEY-1 SET R8 FOR EXECUTED COMPARE                  
         CLI   CMPPGRPN,X'81'                                                   
         BE    NXTPRD4                                                          
         LA    R8,1(R8)                                                         
         CLI   BPRD,3                                                           
         BNE   NXTPRD4                                                          
         LA    R8,1(R8)                                                         
*                                                                               
NXTPRD4  DS    0H                                                               
         GOTO1 AIO                                                              
         BNE   NXTPRD7                                                          
         EX    R8,*+8                                                           
         B     *+10                                                             
         CLC   PRGKEY(0),IOKEYSAV                                               
         BNE   NXTPRD7                                                          
         CLI   CMPPGRPN,X'81'                                                   
         BNE   NXTPRD6                                                          
         MVC   BPRD,PRGPGRP                                                     
         NI    BPRD,X'F0'                                                       
         CLC   BPRD,CMPPGRP+1                                                   
         BE    NXTPRD6                                                          
         BH    NXTPRD7                                                          
         LA    R1,DIRSQ+IO1                                                     
         B     NXTPRD4                                                          
*                                                                               
NXTPRD6  L     R1,AIOAREA3         PRODUCT'S BEEN FOUND                         
         LA    R1,CLIST-CLTHDR(R1)                                              
         CLC   0(3,R1),PRGPPRD     GET PRODUCT CODE FROM CLIENT HEADER          
         BE    NXTPRD8                                                          
         LA    R1,4(R1)                                                         
         CLI   0(R1),C'A'                                                       
         BNL   *-18                                                             
         DC    H'0'                                                             
*                                                                               
NXTPRD7  MVI   BPRD,0              ALL PRODUCTS FOUND                           
         B     NXTPRDX                                                          
*                                                                               
NXTPRD8  MVC   BPRD,3(R1)          RETURN PRODUCT CODE IN BPRD                  
*                                                                               
NXTPRDX  MVC   SVPGKEY,IOKEY       SAVE CURRENT PRODUCT GROUP                   
         LR    RE,R0                                                            
         CLI   BPRD,0                                                           
         BE    *+8                                                              
         CR    RE,RE                                                            
         BR    RE                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK BUY TRANSFER STATUS                                                     
* IF BUY WAS TRANSFERRED FROM NWS WE DO NOT WANT TO INCLUDE IT WHEN WE          
* ACCUMULATE THE SELECTIVE TO BE REDUCED FROM GOALS (IT IS TAKEN INTO           
* ACCOUNT IN THE ACHIEVED SO WE DO NOT WANT TO DOUBLE COUNT)                    
* HOWEVER - CHANGES MADE TO BUY AFTER TRANSFER MUST BE ACCOUNTED FOR            
* SO IF BUY CHANGED SINCE TRANSFER WE ACCUMULATE THE NWS COST/GRP TO            
* MINUS OUT FROM THE BUY VALUES (NET RESULT IS +/- DIFF)                        
*                                                                               
* ENTRY - BUY RECORD IN I/O1                                                    
* EXIT  - LSELFLAG SET, LMDEMS/LMDOLS CONTAIN ANY VALUES TO MINUS OUT           
* NOTE  - USES IOAREA2 (SBCHUNK WILL CRAP OVER LATER)                           
*       - CALLED PRIOR TO BUILDING SLEN TAB AND DOES NOT TAKE INTO              
*         ACCOUNT SLENS SINCE WE NOT GONNA GET DIFFERENT LENS ANYWAY            
*         AS NOT SPLIITING PIGS (EXCEPT IF POL WHICH CANT HAVE PIGS!)           
***********************************************************************         
         SPACE 1                                                                
CHKTRANS NTR1  ,                                                                
         XC    LSELFLAG,LSELFLAG   INITIALISE TO KEEP BUY                       
         MVC   SVBUYKEY,IOKEY      SAVE INCASE WE DO I/O                        
         L     R2,AIOAREA1                                                      
         USING BUYRECD,R2                                                       
         LA    R3,BDELEM                                                        
CHKT10   CLI   0(R3),0             FIND BUY DETAIL ELEM                         
         BNE   *+6                                                              
         DC    H'0'                GOTTA BE ONE                                 
         CLI   0(R3),BDCODEQ                                                    
         BE    CHKT15                                                           
         SR    RF,RF                                                            
         IC    RF,1(R3)            NEXT ELEMENT                                 
         AR    R3,RF                                                            
         B     CHKT10                                                           
CHKT15   TM    BDSTAT-BDELEM(R3),BDSBWSTQ   NWS TRANSFER                        
         BZ    CHKTKEEP            NOT TRANSFERRED SO INCLUDE                   
*                                                                               
* ENSURE WE TRANSFERRED IT - IF NOT THEN AGENCY PROBABLY NOT STICKING           
* TO RULES OF SETTING UP BUYER=CLIENT CODE & ONLY 1 CAMPAIGN PER C/P/E          
* IN WHICH CASE TREAT AS NOT TRANSFERRED SINCE NOT IN OUR ACHIEVED              
         LA    R1,BDELEM                                                        
         SR    RF,RF               FIND BWS TRANSFER ELEMENT                    
CHKT20   CLI   0(R1),0                                                          
*        BNE   *+6                                                              
*        DC    H'0'                HOW WAS THIS TRANSFERRED THEN ?              
         BE    CHKTKEEP            ABOVE TRAP HIT 11DEC01, JUST INCLUDE         
         CLI   0(R1),BWSCODEQ                                                   
         BE    *+14                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     CHKT20                                                           
         CLC   BWSBYR-BWSELEM(L'BWSBYR,R1),QBYR                                 
         BNE   CHKTKEEP            WE (BUYER) NOT TRANSFER SO INCLUDE           
* CHECK BOTH THIS AND ANY COMPANION CAMPAIGN - ACHIEVED DOES                    
         MVC   APHALF,BCAM                                                      
         XC    APHALF,=X'FFFF'                                                  
         CLC   BWSCAM-BWSELEM(L'BWSCAM,R1),APHALF                               
         BE    CHKT22              WE (CAMPGN) TRANSFER, CONTINUE               
         OC    CMPCCAM,CMPCCAM                                                  
         BZ    CHKTKEEP            NO COMPANION SO INCLUDE                      
         MVC   APHALF,CMPCCAM                                                   
         XC    APHALF,=X'FFFF'                                                  
         CLC   BWSCAM-BWSELEM(L'BWSCAM,R1),APHALF                               
         BNE   CHKTKEEP            COMPANION NOT TRANSFER SO INCLUDE            
*                                                                               
CHKT22   CLI   BDWHY,0             SEE IF SUBSEQUENTLY CHANGED IN =BUY          
         BNE   *+8                 ( CANNOT RELY ON WHAT LAST WAS AS            
         CLI   BDWHY2,0            ( MAY HAVE BEEN OTHER CHANGES PRIOR          
         BNE   *+8                 ( WHICH AFFECTED COST/GRPS                   
         CLI   BDWHY3,0                                                         
         BE    CHKTDROP            NOT CHANGED, DON'T INCLUDE                   
*                               CHANGED - KEEP BUT MINUS OUT NWS VALUES         
         LR    R4,R2               R4=A(BUY RECORD)                             
         XC    IOKEY,IOKEY         MATCH BUY TO NWS 'PARTNER' (CAN'T            
         LA    R2,IOKEY            SEE HOW TO DO VIA MINIO, DO MYSELF!)         
         USING BWDRECD,R2                                                       
         MVI   BWDKTYP,BWDKTYPQ                                                 
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BAGYMD                                                  
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,BCMSEQ                                                   
         OC    CMPCCAM,CMPCCAM     WAS THIS VIA COMPANION CAMPAIGN              
         BZ    CHKT23                                                           
         XC    APHALF,=X'FFFF'                                                  
         CLC   APHALF,CMPCCAM                                                   
         BNE   CHKT23                                                           
         MVC   BWDKSEQ,LCCMSEQ     YES - USE IT'S SEQ                           
CHKT23   LA    R1,DIRHI+IO2                                                     
         B     *+8                                                              
CHKT25   LA    R1,DIRSQ+IO2                                                     
         GOTO1 AIO                                                              
         CLC   IOKEY(BWDKEL-BWDRECD),IOKEYSAV                                   
         BNE   CHKTKEEP            CAN'T MATCH BUY IN NWS SO INCLUDE            
         GOTO1 AIO,FILGET2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA2                                                      
         LA    R1,BWDEL                                                         
         SR    RF,RF                                                            
         SR    R3,R3                                                            
*                                                                               
CHKT30   CLI   0(R1),0             LOCATE MATCHING NWS TRANSFER ELEM            
         BNE   CHKT32                                                           
         CLI   1(R1),BWDELCDQ      DETAIL ELEM FOLLOW / OR EOR                  
         BNE   CHKT25              EOR - SEE IF CONTINUATION RECORD             
         LA    R1,1(R1)            DETAIL - NEXT CLUSTER                        
CHKT32   CLI   0(R1),BWDELCDQ      DETAIL ELEM                                  
         BNE   *+10                                                             
         LR    R3,R1               R3=A(CURRENT DETAIL ELEM)                    
         B     CHKT35                                                           
         CLI   0(R1),BTRELCDQ      TRANSFER ELEM                                
         BNE   *+14                                                             
         CLC   BTRLINE-BTREL(L'BTRLINE,R1),BUYKBUY-BUYRECD(R4)                  
         BE    CHKT40              FOUND MATCHING TRANSFER ELEM                 
CHKT35   IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     CHKT30                                                           
*                                                                               
* MATCHED BUY TO NWS - ACCUMULATE NWS POINTS AND DOLLARS                        
CHKT40   LTR   R3,R3               MUST HAVE CORRESPONDING BWDEL                
         BNZ   *+6                                                              
         DC    H'0'                MINIO NOT ADD PROPERLY                       
*                                                                               
         XC    LMDEMS(NMAXWKS*4),LMDEMS                                         
         XC    LMDOLS(NMAXWKS*4),LMDOLS                                         
         LR    R1,R3               KEEP R3=A(BWDEL)                             
         SR    RF,RF                                                            
*                                                                               
CHKT50   CLI   0(R1),0             FIND DEMOS ELEMENT                           
         BE    CHKTKEEP            NOT FOUND - IN THAT CASE KEEP BUY            
         CLI   0(R1),DMOELCDQ                                                   
         BE    CHKT55                                                           
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     CHKT50                                                           
CHKT55   SR    R4,R4               R4=RATING                                    
         ICM   R4,7,DMODEMO-DMOEL+5(R1)                                         
         B     CHKT65                                                           
*                                                                               
CHKT60   CLI   0(R1),0             FIND SPOTS PER WEEK ELEMENT                  
         BE    CHKTKEEP            NOT FOUND - IN THAT CASE KEEP BUY            
         CLI   0(R1),SPWELCDQ                                                   
         BE    CHKT70                                                           
CHKT65   IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     CHKT60                                                           
*                                                                               
         USING SPWEL,R1                                                         
CHKT70   SR    RE,RE                                                            
         IC    RE,SPWELLN                                                       
         AR    RE,R1                                                            
         ST    RE,APFULL           APFULL = A(END OF SPOTS ELEM)                
*                                                                               
         LTR   R4,R4               ACCUMULATE NWS GRPS                          
         BZ    CHKT80              ZERO RATING                                  
         LA    R0,NMAXWKS                                                       
         LA    R8,SPWPERWK                                                      
         LA    R9,LMDEMS                                                        
CHKT72   SR    RF,RF                                                            
         ICM   RF,1,0(R8)                                                       
         BZ    CHKT74                                                           
         MR    RE,R4               SPOTS X RATING                               
         ST    RF,0(R9)                                                         
CHKT74   LA    R8,1(R8)            NEXT WEEK                                    
         CLM   R8,7,APFULL+1                                                    
         BNL   CHKT80                                                           
         LA    R9,4(R9)                                                         
         BCT   R0,CHKT72                                                        
*                                  ACCUMULATE NWS COST                          
CHKT80   ICM   R4,15,BWDCOST1      R2 STILL=A(BWDEL)                            
         BZ    CHKT100             ZERO COST                                    
         OC    BWDEFDT2,BWDEFDT2   TEST EFFECTIVE DATES                         
         BZ    CHKT81                                                           
         GOTO1 VDATCON,APPARM,(3,BWDEFDT2),(2,APDUB)  YES-APDUB(2)=             
         OC    BWDEFDT3,BWDEFDT3                          EFF DATE 2            
         BZ    CHKT81                                     APDUB+2(2)=           
         GOTO1 VDATCON,APPARM,(3,BWDEFDT3),(2,APDUB+2)    EFF DATE 3            
*                                                                               
CHKT81   LA    R0,NMAXWKS                                                       
         LA    R8,SPWPERWK                                                      
         LA    R9,LMDOLS                                                        
         LA    R1,CMPDATSP                                                      
CHKT82   SR    RF,RF                                                            
         ICM   RF,1,0(R8)                                                       
         BZ    CHKT84                                                           
         ICM   R4,15,BWDCOST1                                                   
         OC    BWDEFDT2,BWDEFDT2   TEST EFF DATE 2                              
         BZ    CHKT83                                                           
         CLC   2(2,R1),APDUB       YES-TEST DATE IN THIS WEEK                   
         BL    CHKT83                                                           
         ICM   R4,15,BWDCOST2      YES-SWITCH TO EFF COST 2                     
         OC    BWDEFDT3,BWDEFDT3   TEST EFF DATE 3                              
         BZ    CHKT83                                                           
         CLC   2(2,R1),APDUB+2     YES-TEST DATE IN THIS WEEK                   
         BL    CHKT83                                                           
         ICM   R4,15,BWDCOST3      YES-SWITCH TO EFF COST 3                     
*                                                                               
CHKT83   LTR   R4,R4                                                            
         BZ    CHKT84                                                           
         MR    RE,R4               SPOTS X COST                                 
         ST    RF,0(R9)                                                         
CHKT84   LA    R8,1(R8)            NEXT WEEK                                    
         CLM   R8,7,APFULL+1                                                    
         BNL   CHKT100                                                          
         LA    R9,4(R9)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,CHKT82                                                        
*                                                                               
CHKT100  OI    LSELFLAG,LSELMINQ   FLAG VALUES TO MINUS OUT (KEEP BUY)          
         B     CHKTKEEP                                                         
CHKTDROP OI    LSELFLAG,LSELIGNQ   FLAG TO IGNORE BUY                           
CHKTKEEP CLC   IOKEY,SVBUYKEY      CHECK I/O DONE                               
         BE    CHKTXIT                                                          
         MVC   IOKEY,SVBUYKEY                                                   
         GOTO1 AIO,DIRHI+IO1       RE-ESTABLISH BUY READ SEQUENCE               
CHKTXIT  XIT1  ,                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MINUS NWS VALUES FROM SELECTIVE BUY VALUES               *         
***********************************************************************         
         SPACE 1                                                                
MINUSNWS NTR1  ,                                                                
         LA    RE,LMDOLS                                                        
         LA    RF,LDOLS                                                         
         LA    R0,NMAXWKS                                                       
*                                                                               
MINUS2   OC    0(4,RE),0(RE)                                                    
         BZ    MINUS4                                                           
         L     R1,0(RF)                                                         
         S     R1,0(RE)       YES - THIS COULD GO MINUS                         
         ST    R1,0(RF)                                                         
*                                                                               
MINUS4   LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,MINUS2                                                        
*                                                                               
         LA    RE,LMDEMS                                                        
         LA    RF,LDEMS                                                         
         LA    R0,NMAXWKS                                                       
*                                                                               
MINUS6   OC    0(4,RE),0(RE)                                                    
         BZ    MINUS8                                                           
         L     R1,0(RF)                                                         
         S     R1,0(RE)       YES - THIS COULD GO MINUS                         
         ST    R1,0(RF)                                                         
*                                                                               
MINUS8   LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,MINUS6                                                        
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
ENOTV    MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
         EJECT                                                                  
XFF      DC    XL8'FFFFFFFFFFFFFFFF'                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
LOCALD   DSECT                                                                  
*                                                                               
LAESTIM  DS    A                                                                
LAESTIMX DS    A                                                                
LAWORK   DS    A                                                                
*                                                                               
LDEMTOT  DS    F                                                                
LDOLTOT  DS    F                                                                
LDEMS    DS    (NMAXWKS)XL4                                                     
LDOLS    DS    (NMAXWKS)XL4                                                     
*                                                                               
LMDEMS   DS    (NMAXWKS)XL4    TRANSFERRED BUY VALUES TO MINUS OUT              
LMDOLS   DS    (NMAXWKS)XL4                                                     
*                                                                               
SVSELDEM DS    (NMAXWKS)XL4    WEEKLY DEMO ACCUMS                               
SVSPLDEM DS    (NMAXWKS)XL4                                                     
SVNETDEM DS    (NMAXWKS)XL4                                                     
SVNSPDEM DS    (NMAXWKS)XL4                                                     
*                                                                               
SVSELDOL DS    (NMAXWKS)XL4    WEEKLY DOLLAR ACCUMS                             
SVNETDOL DS    (NMAXWKS)XL4                                                     
*                                                                               
SVSELTOT DS    XL4             DEMO TOTALS                                      
SVSPLTOT DS    XL4                                                              
SVNETTOT DS    XL4                                                              
SVNSPTOT DS    XL4                                                              
*                                                                               
SVSELDOT DS    XL4             DOLLAR TOTALS                                    
SVNETDOT DS    XL4                                                              
*                                                                               
LFLAG    DS    X                                                                
LNETQ    EQU   X'80'           NETWORK/NETWORK SPILL                            
LSPILLQ  EQU   X'40'           SPILL                                            
LSELCTQ  EQU   X'20'           SELECTIVE TV ALREADY PURCHASED                   
LFLAGPRO DS    X               CURRENT LFLAG SETTING PROCESSING                 
*                                                                               
LSELFLAG DS    X               SELECTIVE BUY FLAG                               
LSELIGNQ EQU   X'80'           - IGNORE THIS BUY                                
LSELMINQ EQU   X'40'           - NEEDS VALUES MINUSING OUT                      
*                                                                               
LSLNS    DS    XL4                                                              
LCMPST   DS    XL2                                                              
LCCMSEQ  DS    XL(L'BCMSEQ)    COMPANION CAMPAIGN SEQ                           
SVAGYMD  DS    XL1                                                              
SVEST    DS    XL1                 CAMPAIGN ESTIMATE                            
SVPRD    DS    XL1                 CAMPAIGN PRODUCT                             
SVPGKEY  DS    XL(L'IOKEY)                                                      
SVBUYKEY DS    XL(L'IOKEY)                                                      
*                                                                               
TKEYSAVE DS    XL(L'TKEY)                                                       
*                                                                               
TSARREC  DS    (TSPLRECL)X         TSAR RECORD                                  
*                                                                               
SPILLSTA DS    (NMAXSTA)XL8                                                     
NETSTA   DS    (NMAXSTA)XL8                                                     
NMAXSTA  EQU   10                                                               
*                                                                               
* DDEBLOCK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
         EJECT                                                                  
* SPOTBLOCK                                                                     
         DS    0D                                                               
       ++INCLUDE SPOTBLOCK                                                      
         EJECT                                                                  
***********************************************************************         
* SCREEN LINE DSECT                                                   *         
***********************************************************************         
         SPACE 1                                                                
SCREEND  DSECT                                                                  
*                                                                               
SWEEK    DS    CL5                                                              
         DS    CL3                                                              
SSELCT   DS    CL10                                                             
         DS    CL2                                                              
SSPILL   DS    CL10                                                             
         DS    CL2                                                              
SNET     DS    CL10                                                             
         DS    CL2                                                              
SNETSPL  DS    CL10                                                             
         DS    CL2                                                              
SDEMTOT  DS    CL10                                                             
         DS    CL2                                                              
SCOST    DS    CL10                                                             
         EJECT                                                                  
* SPNWSWRK                                                                      
*        PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPNWSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
BUYRECD  DSECT                                                                  
         SPACE 1                                                                
* SPGENBUY                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
ESTHDRD  DSECT                                                                  
         SPACE 1                                                                
* SPGENEST                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENCLT                                                                      
CLTHDRD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
* SPGENPRG                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENPRG                                                       
         PRINT ON                                                               
* SPNWSCAM                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSCAM                                                       
         PRINT ON                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSF3D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029SPNWS18A  07/17/02'                                      
         END                                                                    
