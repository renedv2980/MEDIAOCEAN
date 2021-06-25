*          DATA SET SPNWS03    AT LEVEL 035 AS OF 02/26/07                      
*PHASE T20703C,*                                                                
         TITLE 'NWS03 T20703 - BUYERS WORK SHEET - CAMPAIGN OVERLAY'            
T20703   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20703**,RA,R6,RR=RE                                           
         USING TWAD,R5             R5=A(TWA)                                    
******** USING SAVAREA,R6 ******** R6=A(SAVE AREA) ********                     
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RD,APWORKA                                                       
         ST    RB,APNTRYA                                                       
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         LA    R2,IOKEY            R2=A(CAMPAIGN RECORD KEY)                    
         USING CAMRECD,R2                                                       
*                                                                               
         LA    R0,AXTRAN           SET EXTENTION ROUTINES                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         L     R1,=A(EXTRA)                                                     
         A     R1,APRELO                                                        
         ST    R1,AXTRA(RE)                                                     
         STC   RF,AXTRA(RE)                                                     
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     DELRECA                                                          
         B     RESRECA                                                          
         B     VALSEL                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         DC    AL4(0)              APMVALS MODE IS INVALID                      
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     LSTSCR                                                           
         B     VALREQ                                                           
         B     PRTREP                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   XC    CAMKEY,CAMKEY                                                    
         MVI   CAMKTYP,CAMKTYPQ                                                 
         MVI   CAMKSUB,CAMKSUBQ                                                 
*                                                                               
         XC    CPNMDN,CPNMDN       VALIDATE MEDIA                               
         OI    CPNMDNH+6,FVOXMT                                                 
         GOTO1 AVALMED,CPNMEDH                                                  
         BNE   VALKEYX                                                          
         MVC   CPNMDN,MEDNM                                                     
         MVC   CAMKAGMD,BAGYMD                                                  
*                                                                               
         XC    CPNBYN,CPNBYN       VALIDATE BUYER                               
         OI    CPNBYNH+6,FVOXMT                                                 
         GOTO1 AVALBYR,CPNBYRH                                                  
         BNE   VALKEYX                                                          
         MVC   CPNBYN,BYRNM                                                     
         MVC   CAMKBYR,BBYR                                                     
         OC    CAMKAGMD,BBYRMASK     APPLY AGY/MED MASK                         
*                                                                               
         XC    SCFULL,SCFULL       VALIDATE CAMPAIGN NUMBER                     
         CLI   APACTN,ACTADD                                                    
         BE    VALKEY4                                                          
         MVI   FVMINL,1            REQUIRED FIELD                               
         GOTO1 AFVAL,CPNNUMH                                                    
         BNE   VALKEYX                                                          
         TM    FVIIND,FVINUM                                                    
         BZ    VALKEY2                                                          
         OC    SCFULL,SCFULL       TEST CAMPAIGN NUMBER ZERO                    
         BZ    EIIF                                                             
         OC    SCFULL(2),SCFULL                                                 
         BNZ   EIIF                                                             
         B     VALKEY4                                                          
*                                                                               
VALKEY2  ZIC   R1,FVXLEN           TEST LATEST CAMPAIGN REQUESTED               
         EX    R1,CAMINP1                                                       
         BE    *+12                                                             
         EX    R1,CAMINP2                                                       
         BNE   EIIF                                                             
         MVC   SCFULL,EFFS                                                      
*                                                                               
VALKEY4  OC    CAMKCAM,SCFULL+2                                                 
         BZ    *+10                                                             
         XC    CAMKCAM,EFFS                                                     
         LA    R1,DIRHID+IO1                                                    
*                                                                               
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                 READ HIGH FOR CAMPAIGN KEY                   
         BL    VALKEYX                                                          
         MVC   APRECKEY,IOKEY                                                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         BE    VALKEY6                                                          
         TM    IOERR,IOEDEL                                                     
         BNZ   VALKEY6                                                          
         MVI   APINDS,APIOKADD                                                  
         MVC   APRECKEY,IOKEYSAV                                                
         XC    LCAMP,LCAMP                                                      
         B     VALKEYX                                                          
*                                                                               
VALKEY6  CLC   CAMKEY(CAMKCAM-CAMKEY),IOKEYSAV                                  
         BNE   VALKEY8                                                          
         OC    SCFULL,SCFULL                                                    
         BZ    VALKEY8                                                          
         CLC   SCFULL,EFFS                                                      
         BE    *+14                                                             
         CLC   CAMKEY(CAMKREST-CAMKEY),IOKEYSAV                                 
         BNE   VALKEY8                                                          
         MVC   WCAMP,CAMKCAM                                                    
         LA    R1,FILGET1                                                       
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BE    *+14                TEST OK OR DELETED                           
         TM    IOERR,IOERRS-IOEDEL                                              
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   APRECDA,IODA                                                     
         MVI   APINDS,APIOKDIS+APIOKCHA                                         
         TM    CAMKCNTL,X'80'      TEST RECORD IS DELETED                       
         BZ    *+12                                                             
         MVI   APINDS,APIOKRES+APIOKDIS                                         
         B     VALKEYX                                                          
         GOTO1 AGETCAM,WCAMP       GET CAMPAIGN DETAILS                         
*                                                                               
         LA    R3,IOKEY            CHECK IF ANY DETAILS EXIST                   
         USING BWHRECD,R3                                                       
         XC    BWHKEY,BWHKEY                                                    
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,BAGYMD                                                  
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         MVC   BWHKCAM,WCAMP                                                    
         GOTO1 AIO,DIRHI                                                        
         BE    *+12                                                             
         OI    APINDS,APIOKDEL                                                  
         B     VALKEYX                                                          
         CLC   BWHKEY(BWHKMKT-BWHKEY),IOKEYSAV                                  
         BE    *+8                                                              
         OI    APINDS,APIOKDEL                                                  
         B     VALKEYX                                                          
         DROP  R3                                                               
*                                                                               
VALKEY8  MVI   APINDS,APIOKADD                                                  
         MVC   APRECKEY,IOKEYSAV                                                
         XC    LCAMP,LCAMP                                                      
         CLC   CAMKEY(CAMKCAM-CAMKEY),IOKEYSAV                                  
         BNE   VALKEYX                                                          
         MVC   LCAMP,CAMKCAM                                                    
         XC    LCAMP,EFFS                                                       
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALREC   MVC   IOKEY,APRECKEY                                                   
         OC    BYRPW,BYRPW         TEST BUYER PASSWORD REQUIRED                 
         BZ    VALREC2                                                          
         GOTO1 AVALPWD             YES-VALIDATE IT                              
         BNE   VALRECX                                                          
*                                                                               
VALREC2  CLI   APACTN,ACTADD       TEST ACTION=ADD                              
         BNE   VALREC4                                                          
         SR    R1,R1                                                            
         ICM   R1,3,LCAMP                                                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,CAMKCAM                                                     
         XC    CAMKCAM,EFFS                                                     
         XC    SVCSTART,SVCSTART                                                
         XC    SVCEND,SVCEND                                                    
         XC    SVCINDS,SVCINDS                                                  
         XC    SVCAMWKS,SVCAMWKS                                                
         XC    SVCAMOPT,SVCAMOPT                                                
         XC    SVCCAM,SVCCAM                                                    
         XC    SVCIDR,SVCIDR                                                    
         B     VALREC5                                                          
*                                                                               
VALREC4  CLI   APACTN,ACTCHA     TEST ACTION=CHANGE                             
         BNE   VALREC5                                                          
         L     R2,AIOAREA1       YES-                                           
         MVC   SVCCLT,CAMCLT     SAVE OLD CLIENT (FOR PASSIVE KEYS)             
         MVC   SVCPRD,CAMPRD     SAVE OLD PRODUCT (FOR PASSIVE KEYS)            
         MVC   SVCEST,CAMEST     SAVE OLD ESTIMATE (FOR PASSIVE KEYS)           
         MVC   SVCSTART,CAMSTRT  SAVE OLD CAMPAIGN DATES (IF ANY)               
         MVC   SVCEND,CAMEND                                                    
         MVC   SVCINDS,CAMINDS   SAVE INDICATORS                                
         MVC   SVCAMWKS,CAMWKS                                                  
         MVC   SVCAMOPT,CAMOPT                                                  
         MVC   SVCCAM,CAMCCAM    SAVE COMPANION CAMPAIGN                        
*                                                                               
         XC    SVCIDR,SVCIDR                                                    
         CLI   CAMELLN,CAMELLNQ  DO WE HAVE AN EXPANDED DESC ELEMENT?           
         BNH   VALREC5                                                          
         MVC   SVCIDR,CAMIDRNM   YES, SAVE IDR NAME                             
*                                                                               
VALREC5  L     R2,AIOAREA1                                                      
         LR    R0,R2                                                            
         LA    R1,2000                                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   CAMKEY,IOKEY                                                     
*                                                                               
         LA    R0,CAMFSTEL+CAMELLNQ+1-CAMRECD                                   
         OC    SVCIDR,SVCIDR                                                    
         BZ    *+8                                                              
         LA    R0,CAMFSTEL+CAMELLQ2+1-CAMRECD    LONGER IF IDR OR PC            
         STCM  R0,3,CAMLEN                                                      
*                                                                               
         CLI   APACTN,ACTCHA                                                    
         BNE   *+10                                                             
         MVC   CAMINDS,SVCINDS                                                  
         XC    CPNCNM,CPNCNM       CLIENT                                       
         OI    CPNCNMH+6,FVOXMT                                                 
         GOTO1 AVALCLT,CPNCLTH                                                  
         BNE   VALRECX                                                          
         MVC   CAMCLT,BCLT                                                      
         MVC   CPNCNM,CLTNM                                                     
*                                                                               
         XC    CPNPNM,CPNPNM       PRODUCT                                      
         OI    CPNPNMH+6,FVOXMT                                                 
         GOTO1 AVALPRD,CPNPRDH                                                  
         BNE   VALRECX                                                          
         MVC   CAMPRD,BPRD                                                      
         MVC   CPNPNM,PRDNM                                                     
*                                                                               
         XC    CAMPGRP,CAMPGRP                                                  
         MVI   CAMPGRPN,0                                                       
         XC    CPNPGN,CPNPGN                                                    
         OI    CPNPGNH+6,FVOXMT                                                 
         CLI   CPNPGRH+5,0         PRODUCT GROUP                                
         BE    VALREC6                                                          
         MVI   FVMINL,2                                                         
         MVI   FVMAXL,4                                                         
         GOTO1 AFVAL,CPNPGRH                                                    
         BNE   VALRECX                                                          
         CLI   BPRD,X'FF'          PRODUCT MUST BE POL                          
         BNE   EPOL                                                             
         GOTO1 AVALPGR             VALIDATE PRODUCT GROUP                       
         BNE   VALRECX                                                          
         CLC   CMPPGRP,CAMPGRP     TEST PRODUCT GROUP CHANGE                    
         BE    VALREC6                                                          
         XC    QEST,QEST           YES-ESTIMATE MUST BE RE-VALIDATED            
*                                                                               
VALREC6  MVC   CMPPGRP,CAMPGRP                                                  
         MVC   CMPPGRPN,CAMPGRPN                                                
*                                                                               
         XC    CPNENM,CPNENM       ESTIMATE                                     
         OI    CPNENMH+6,FVOXMT                                                 
         GOTO1 AVALEST,CPNESTH                                                  
         BNE   VALRECX                                                          
         MVC   CAMEST,BEST                                                      
         MVC   CPNENM,ESTNM                                                     
*                                                                               
         CLC   CMPPRDN,CAMPRD      HAS PRODUCT CHANGED?                         
         BNE   *+14                                                             
         CLC   CMPESTN,CAMEST      HAS ESTIMATE CHANGED?                        
         BE    VALREC7             NO                                           
         GOTO1 ACHKTRN             YES, MAKE SURE BUY NOT TRANSFERRED           
         BNE   VALRECX                                                          
*                                                                               
VALREC7  MVI   CAMELCD,CAMELCDQ                                                 
         MVI   CAMELLN,CAMELLNQ                                                 
         OC    SVCIDR,SVCIDR       DID WE HAVE A IDR NAME BEFORE?               
         BZ    VALREC7E                                                         
         MVI   CAMELLN,CAMELLQ2    YES, STILL EXPANDED DESC ELEM                
         LA    R0,CAMFSTEL+CAMELLQ2+1-CAMRECD                                   
         STCM  R0,3,CAMLEN                                                      
VALREC7E MVC   CAMPRDC,QPRD                                                     
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,CPNDATH+5                                                   
         BZ    VALREC8                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CPNDAT(0),=C'ESTIMATE'                                           
         BNE   VALREC8                                                          
         GOTO1 VDATCON,APPARM,(0,ESTST),(3,CAMSTRT)                             
         GOTO1 (RF),(R1),(0,ESTND),(3,CAMEND)                                   
         XC    CPNDAT,CPNDAT                                                    
         OI    CPNDATH+6,FVOXMT                                                 
         GOTO1 (RF),(R1),(3,CAMSTRT),(5,CPNDAT)                                 
         MVI   CPNDAT+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,CAMEND),(5,CPNDAT+9)                                
         MVC   APWORK(6),ESTST                                                  
         MVC   APWORK+6(6),ESTND                                                
         LA    R1,CPNDATH                                                       
         ST    R1,FVADDR                                                        
         B     VALREC10                                                         
*                                                                               
VALREC8  MVI   FVMINL,1                                                         
         GOTO1 AVALDAT,CPNDATH                                                  
         BNE   VALRECX                                                          
         OC    APWORK+6(6),APWORK+6                                             
         BZ    EEDM                                                             
*                                                                               
         CLC   APWORK+0(6),ESTST                                                
         BL    EDNC                                                             
         CLC   APWORK+6(6),ESTND                                                
         BH    EDNC                                                             
         CLC   APWORK+0(6),APWORK+6                                             
         BH    ESGE                                                             
         TM    CLTIND2,CLTIEST     TEST DATES MUST MATCH ESTIMATE DATES         
         BZ    VALREC9                                                          
         SR    R1,R1               YES-                                         
         CLC   APWORK(6),ESTST                                                  
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
         CLC   APWORK+6(6),ESTND                                                
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
         LTR   R1,R1               TEST START OR END MATCH EST DATES            
         BZ    ECMPEST             NO-ERROR                                     
         CLI   CPNCCNH+5,0         YES-TEST COMPANION CAMPAIGN                  
         BNE   VALREC9             YES                                          
         CHI   R1,2                 NO-BOTH MUST MATCH                          
         BNE   ECMPEST                                                          
*                                                                               
VALREC9  DS    0H                                                               
         GOTO1 VDATCON,APPARM,(0,APWORK),(3,CAMSTRT)                            
         GOTO1 (RF),(R1),(0,APWORK+6),(3,CAMEND)                                
*                                                                               
VALREC10 TM    CAMINDS,CAMISKD+CAMIGOAL TEST SPOTS SCHEDULED OR BWS             
         BZ    VALREC13                 GOAL RECORDS ADDED                      
         CLI   APACTN,ACTCHA       AND ACTION=CHANGE                            
         BNE   VALREC13                                                         
         CLI   CPNFW1H+5,0         AND NOT NON-CONTIGUOUS WEEKS                 
         BNE   VALREC13                                                         
         LA    R0,2                                                             
         CLC   CAMSTRT,SVCSTART    YES-TEST CAMPAIGN START CHANGED              
         BE    VALRC12A                                                         
         TM    CAMINDS,CAMISKD     YES-TEST SPOTS SCHEDULED                     
         BO    ECMPDAT                                                          
****     BZ    *+12                                                             
****     TM    SVCAMOPT,CAMODLY    YES-INVALID FOR DAILY SKED                   
****     BO    ECMPDAT                                                          
         MVC   APWORK+12(6),APWORK     NEW START                                
         GOTO1 VDATCON,APPARM,(3,SVCSTART),APWORK+18  OLD START                 
*                                                                               
VALRC10A CLC   APWORK+12(6),APWORK+18  COMPARE NEW TO OLD                       
***      BL    *+16                                                             
***      MVC   APWORK+12(6),APWORK+18  HIGH-SWITCH THE DATES                    
***      MVC   APWORK+18(6),APWORK          SO THAT LOWER IS FIRST              
         BH    VALREC13            IF NEW END DATE IS HIGHER, OKAY              
         GOTO1 VGETDAY,APPARM,APWORK+12,APFULL   GET DAY OF LOWER               
         ZIC   R4,0(R1)                                                         
         SR    RE,RE                                                            
         ICM   RE,1,ESTOWSDY                                                    
         BZ    VALREC11                                                         
         LA    R4,1(R4)                                                         
         SR    R4,RE                                                            
         BZ    VALRCEDT                                                         
         BP    VALREC11                                                         
         LPR   R4,R4                                                            
         B     VALREC12                                                         
*                                                                               
VALREC11 LNR   R4,R4                                                            
         LA    R4,7(R4)                                                         
         LTR   R4,R4                                                            
         BZ    VALRCEDT                                                         
*                                                                               
VALREC12 GOTO1 VADDAY,APPARM,APWORK+12,APWORK+24,(R4)  GET SUNDAY               
         CLC   APWORK+18(6),APWORK+24   TEST HIGHER DATE AFTER SUNDAY           
         BNH   VALRC12A                 NO - CHECK END DATE                     
*&&DO                                                                           
VALRC12A TM    CAMINDS,CAMIGOAL    TEST BWS GOAL RECORDS ADDED                  
         BZ    VALREC13                                                         
*&&                                                                             
VALRCEDT CHI   R0,2                                                             
         BE    EDAT                                                             
         B     ECENDDAT                                                         
*                                                                               
VALRC12A CLC   CAMEND,SVCEND       AND END DATE CHANGED                         
         BE    VALREC13                                                         
         MVC   APWORK+12(6),APWORK+6   NEW END                                  
         GOTO1 VDATCON,APPARM,(3,SVCEND),APWORK+18  OLD END                     
         BCT   R0,VALRC10A            TEST END WEEK HAS CHANGED                 
*                                                                               
VALREC13 SR    R8,R8               COMPANION CAMPAIGN                           
         GOTO1 AFVAL,CPNCCNH                                                    
         BE    VALRC13A                                                         
         OC    SVCCAM,SVCCAM       MISSING-TEST CURRENTLY HAS ONE               
         BZ    VALRC13E                                                         
         MVC   APHALF,SVCCAM       YES-READ CURRENT COMPANION                   
         XC    APHALF,EFFS                                                      
         B     VALRC13B                                                         
*                                                                               
VALRC13A LA    R8,1                                                             
         TM    FVIIND,FVINUM       TEST NUMERIC                                 
         BZ    EIIF                                                             
         OC    SCFULL,SCFULL       TEST CAMPAIGN NON ZERO                       
         BZ    EIIF                                                             
         OC    SCFULL(2),SCFULL    AND NOT TOO LARGE                            
         BNZ   EIIF                                                             
         OC    SVCCAM,SVCCAM       TEST ALREADY HAS A COMPANION                 
         BZ    *+14                                                             
         CLC   SVCCAM,SCFULL+2     YES-IF IT'S NOT THE SAME,                    
         BNE   ECHAC                   USER MUST DELETE BEFORE CHANGE           
         MVC   CAMCCAM,SCFULL+2                                                 
         MVC   APHALF,SCFULL+2                                                  
         XC    APHALF,EFFS                                                      
         CLC   APHALF,CAMKCAM      CHECK IT'S NOT POINTING TO ITSELF            
         BE    EIIF                                                             
*                                                                               
VALRC13B XC    IOKEY,IOKEY         READ COMPANION CAMPAIGN RECORD               
         MVC   IOKEY(CAMKCAM-CAMKEY),CAMKEY                                     
         LA    R2,IOKEY                                                         
         MVC   CAMKCAM,APHALF                                                   
         GOTO1 AIO,DIRHI+IO2                                                    
         BNE   VALRECX                                                          
         CLC   IOKEY(CAMKREST-CAMKEY),IOKEYSAV                                  
         BNE   ECAM                                                             
         GOTO1 AIO,FILGETU2                                                     
         BNE   VALRECX                                                          
         L     R2,AIOAREA2                                                      
         LTR   R8,R8               TEST REMOVING COMPANIONS                     
         BNZ   *+14                                                             
         XC    CAMCCAM,CAMCCAM     YES                                          
         B     VALRC13C                                                         
*                                                                               
         CLC   CAMCLT,BCLT         NO-CHECK SAME CLT/PRD/EST                    
         BNE   ECCAM                                                            
         CLC   CAMPRD,BPRD                                                      
         BNE   ECCAM                                                            
         CLC   CAMEST,BEST                                                      
         BNE   ECCAM                                                            
         CLC   CAMPGRP,CMPPGRP     CHECK PRODUCT GROUP THE SAME                 
         BNE   ECCAM                                                            
         CLC   CAMPGRPN,CMPPGRPN                                                
         BNE   ECCAM                                                            
         L     R1,AIOAREA1         CHECK DATES DON'T OVERLAP                    
         CLC   CAMSTRT,CAMEND-CAMRECD(R1)                                       
         BH    *+14                                                             
         CLC   CAMEND,CAMSTRT-CAMRECD(R1)                                       
         BNL   ECAMD                                                            
         MVC   APHALF,CAMKCAM-CAMRECD(R1)                                       
         XC    APHALF,EFFS                                                      
*                                                                               
         OC    CAMCCAM,CAMCCAM     TEST COMPANION CMPN HAS COMPANION            
         BZ    *+18                                                             
         CLC   APHALF,CAMCCAM      YES-IT MUST BE THIS ONE                      
         BNE   ECAMCOM                                                          
         B     VALRC13D                                                         
*                                                                               
         MVC   CAMCCAM,APHALF      POINT COMPANION BACK TO THIS ONE             
******   BRAS  RE,CAMNMWKS         SOME FIELDS ARE NOT SET CORRECTLY TO         
******                                  USE CAMNMWKS                            
         TM    CAMOPT,CAMOWKS      COMPANION HAS NON-CONTIGUOUS WEEKS?          
         BNZ   VALRC13C            YES, THEN IT CAN'T HAVE MORE THAN 14         
         GOTO1 VDATCON,APPARM,(X'13',CAMSTRT),(X'05',APWORK)                    
         GOTO1 VPERVAL,APPARM,(17,APWORK),(X'20',AIOAREA3)                      
         L     R1,AIOAREA3                                                      
         USING PERVALD,R1                                                       
         CLC   PVALNWKS,=AL2(14)                                                
         BH    ECCAM               COMP CAMPS CAN'T HAVE MORE THAN 14           
         DROP  R1                                                               
*                                                                               
VALRC13C GOTO1 AIO,FILPUT2         AND PUT                                      
         BE    VALRC13D                                                         
         DC    H'0'                                                             
*                                                                               
VALRC13D L     R2,AIOAREA1                                                      
*                                                                               
VALRC13E GOTO1 AFVAL,CPNLENH                                                    
         BNE   VALREC15                                                         
         TM    FVIIND,FVINUM       TEST NUMERIC                                 
         BZ    EFNN                                                             
         OC    SCFULL(3),SCFULL                                                 
         BNZ   EISL                                                             
         OC    SCFULL,SCFULL                                                    
         BZ    EISL                                                             
*&&DO                                                                           
         LA    RE,SLNTAB                                                        
VALREC14 CLI   0(RE),0                                                          
         BE    EISL                                                             
         CLC   0(1,RE),SCFULL+3                                                 
         BE    *+12                                                             
         LA    RE,L'SLNTAB(RE)                                                  
         B     VALREC14                                                         
         MVC   CAMSLN,0(RE)                                                     
*&&                                                                             
         MVC   CAMSLN,SCFULL+3     WE'RE GOOD                                   
*&&DO                                                                           
VALREC14 DS    0H                                                               
         L     R1,VSLNTAB          PIONT TO SLNTAB                              
         LH    RE,0(R1)            GET ENTRY LENGTH                             
         L     RF,2(R1)            GET DSPL TO EOT                              
         AR    RF,R1               RF POINTS TO EOT                             
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         SR    R0,R0                                                            
         LA    R0,C'T'                                                          
         CLI   QMED,C'T'                                                        
         BE    VALRC14C                                                         
         CLI   QMED,C'N'                                                        
         BE    VALRC14C                                                         
         CLI   QMED,C'C'                                                        
         BE    VALRC14C                                                         
         CLI   QMED,C'R'                                                        
         BE    VALRC14C                                                         
         CLI   QMED,C'X'                                                        
         BE    VALRC14C                                                         
         DC    H'0'                                                             
*                                                                               
VALRC14C CLC   =C'00',0(R1)        FIND DEFAULT ENTRY                           
         BE    VALRC14G                                                         
         CLC   CUAALF,0(R1)        MATCH AGY ALPHA                              
         BNE   *+14                                                             
VALRC14G CLM   R0,1,2(R1)          MATCH MEDIA                                  
         BE    VALRC14K                                                         
*                                                                               
         BXLE  R1,RE,VALRC14C      NEXT ENTRY                                   
         DC    H'0'                                                             
*                                                                               
VALRC14K AHI   R1,4                POINT BEYOND TABLE ID                        
         SR    RE,RE                                                            
         IC    RE,SCFULL+3         GET SLN                                      
         AR    RE,RE               X 2                                          
         AR    RE,R1               POINT TO ENTRY                               
         CLI   1(RE),0             TEST SLN VALID                               
         BE    EISL                                                             
         MVC   CAMSLN,1(RE)                                                     
*&&                                                                             
*                                                                               
VALREC15 L     RE,ATWA                                                          
         AHI   RE,SVESLN-TWAD                                                   
         CLI   0(RE),0                                                          
         BE    VALRC15A                                                         
         CLC   CAMSLN,0(RE)                                                     
         BE    VALRC15A                                                         
         XR    R1,R1                                                            
         IC    R1,0(RE)                                                         
         CVD   R1,APDUB                                                         
         UNPK  APWORK(3),APDUB                                                  
         OI    APWORK+2,X'F0'                                                   
         B     ONESPTLN                                                         
*                                                                               
VALRC15A GOTO1 AFVAL,CPNPIGH                                                    
         BL    VALREC18                                                         
         BH    VALRECX                                                          
         GOTO1 VSCANNER,APPARM,CPNPIGH,(2,AIOAREA3),C',=/-'                     
         CLI   4(R1),2                                                          
         BNE   EIIF                                                             
         L     R3,AIOAREA3                                                      
         CLI   0(R3),2                                                          
         BL    EIIF                                                             
         CLI   0(R3),5                                                          
         BH    EIIF                                                             
         MVC   APFULL,SPACES                                                    
         LA    R1,12(R3)                                                        
         LA    RE,2                                                             
         NI    CAMINDS,255-CAMIFR1                                              
*                                                                               
         CLI   0(R3),3                                                          
         BH    EPNV                NO LONGER HAVE BRAND 2 PAYS ALL              
*                                                                               
VALRC15D EX    RE,*+4                                                           
         MVC   APFULL(0),0(R1)                                                  
         CLC   =C'POL',APFULL      DISSALLOW PIGGY PRD=POL                      
         BE    EPNV                                                             
         CLC   QPRD,=C'POL'        TEST MASTER PRODUCT = POL                    
         BE    VALREC16            YES                                          
         CLC   QPRD,APFULL         NO-THEN 1ST PIGGY MUST MATCH MASTER          
         BNE   EPRD                   PRODUCT                                   
         MVC   CAMPPRD1(1),CAMPRD                                               
*                                                                               
VALREC16 LA    R4,IOKEY                                                         
         USING PRDHDRD,R4                                                       
         XC    PKEY,PKEY                                                        
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,BCLT                                                     
         MVC   APDUB(3),QPRD       SAVE QPRD IN APDUB                           
         CLC   QPRD,=C'POL'                                                     
         BNE   VALREC17                                                         
         MVC   PKEYPRD,APFULL                                                   
         GOTO1 AIO,FILRD2                                                       
         BNE   EPNV                                                             
         L     R4,AIOAREA2                                                      
         MVC   CAMPPRD1(1),PCODE+1                                              
*                                                                               
         MVC   QPRD,APFULL         CAMPAIGN PRODUCT POL, BUT WE NEED            
         BNE   VALRC16A              THE PIGGYBACK PAIR'S 1ST PRODUCT           
VALRC16A XC    QEST,QEST                                                        
         GOTO1 AVALEST,CPNESTH                                                  
         BNE   VALRECX                                                          
         MVC   QPRD,=C'POL'        RESTORE THIS VALUE                           
*                                                                               
VALREC17 LA    R1,CPNPIGH                                                       
         ST    R1,FVADDR                                                        
         CLI   1(R3),2                                                          
         BL    EIIF                                                             
         CLI   1(R3),5                                                          
         BH    EIIF                                                             
         MVC   APFULL,SPACES                                                    
         LA    R1,22(R3)                                                        
         LA    RE,2                                                             
         CLI   1(R3),3                                                          
         BNH   VALRC17A                                                         
         CLI   22(R3),C'('                                                      
         BNE   EPNV                                                             
         OI    CAMINDS,CAMIFR1     FREE RIDER WHERE BRAND 1 PAYS ALL            
         LA    R1,1(R1)                                                         
         CLI   1(R3),5                                                          
         BNE   *+16                                                             
         CLI   26(R3),C')'                                                      
         BNE   EPNV                                                             
         B     VALRC17A                                                         
         CLI   25(R3),C')'                                                      
         BNE   EPNV                                                             
         BCTR  RE,0                                                             
*                                                                               
VALRC17A EX    RE,*+4                                                           
         MVC   APFULL(0),0(R1)                                                  
         CLC   =C'POL',APFULL      DISSALLOW PIGGY PRD=POL                      
         BE    EPNV                                                             
         LA    R4,IOKEY                                                         
         MVC   PKEYPRD,APFULL                                                   
         GOTO1 AIO,FILRD2                                                       
         BNE   EPNV                                                             
         L     R4,AIOAREA2                                                      
         MVC   CAMPPRD2(1),PCODE+1                                              
         CLC   CAMPPRD1(1),CAMPPRD2   PIGGIES MUST BE DIFFERENT                 
         BE    EPNV                                                             
         MVC   QPRD,APFULL         VALIDATE ESTIMATE FOR PRD2                   
         XC    QEST,QEST                                                        
         GOTO1 AVALEST,CPNESTH                                                  
         BNE   VALRECX                                                          
         MVC   QPRD,APDUB          RE-VALIDATE ESTIMATE FOR MASTER PRD          
         XC    QEST,QEST                                                        
         GOTO1 AVALEST,CPNESTH                                                  
         BNE   VALRECX                                                          
         DROP  R4                                                               
*                                                                               
         LA    R1,CPNPIGH                                                       
         ST    R1,FVADDR                                                        
         TM    34(R3),X'80'                                                     
         BZ    EIIF                                                             
         L     R4,36(R3)                                                        
         CHI   R4,255                                                           
         BH    EIIF                                                             
         STC   R4,APFLAG                                                        
***      BAS   RE,VALPRDSL                                                      
         MVC   CAMPLEN1(1),APFLAG                                               
         TM    35(R3),X'80'                                                     
         BZ    EIIF                                                             
         L     RF,40(R3)                                                        
         CHI   RF,255                                                           
         BH    EIIF                                                             
         STC   RF,APFLAG                                                        
***      BAS   RE,VALPRDSL                                                      
         MVC   CAMPLEN2(1),APFLAG                                               
         AR    R4,RF                                                            
         XC    APFULL,APFULL                                                    
         MVC   APFULL+3(1),CAMSLN                                               
         CLI   CAMSLN,0                                                         
         BNE   *+12                                                             
         STC   R4,CAMSLN                                                        
         B     *+12                                                             
         C     R4,APFULL                                                        
         BNE   EIIF                                                             
         B     VALREC18                                                         
*                                                                               
*&&DO                                                                           
VALPRDSL LA    R1,VALPRDT                                                       
VALPRDS2 CLI   0(R1),0                                                          
         BE    EIIF                                                             
         CLC   APFLAG,0(R1)                                                     
         BER   RE                                                               
         LA    R1,1(R1)                                                         
         B     VALPRDS2                                                         
VALPRDT  DC    AL1(10,15,20,30,40,45,50,60,75,90,120),AL1(0)                    
         SPACE 1                                                                
*&&                                                                             
*                                                                               
VALREC18 GOTO1 AFVAL,CPNCANH                                                    
         BNE   *+10                                                             
         MVC   CAMNAME,FVIFLD                                                   
*                                                                               
         CLI   CPNUPGH+5,0                                                      
         BE    VALRC19C                                                         
         MVI   APFLAG,X'D8'                                                     
         GOTO1 AVALUPG,CPNUPGH                                                  
         BNE   VALRECX                                                          
         MVC   CAMUPFIL(CAMINPUT-CAMUPFIL),APWORK                               
         MVC   CAMINPUT,CPNUPG                                                  
         MVC   CAMUPUT,APWORK+16                                                
         MVC   CAMUSHR,APWORK+17                                                
         MVC   CAMFRBKL,APWORK+18  FROM BOOK LIST                               
         TM    CAMFRBK+1,BTY2CHAR  2 CHARACTER BOOKTYPE?                        
         BNO   VALRC19C             - NOPE                                      
         MVC   CAMFRBKT,QBOOKTYP                                                
         MVI   CAMELLN,CAMELLQ2    YES, STILL EXPANDED DESC ELEM                
         LA    R0,CAMFSTEL+CAMELLQ2+1-CAMRECD                                   
         STCM  R0,3,CAMLEN                                                      
*                                                                               
****  THIS WAS PREVIOUSLY IN VALREC40!!  MOVED HERE FOR CAMBKTPS!               
VALRC19C XC    CAMBOOKS,CAMBOOKS   VALIDATE BOOKS                               
         XC    CAMBKTPS,CAMBKTPS                                                
         GOTO1 AFVAL,CPNBKSH                                                    
         BNE   VALRC19X                                                         
         XC    APWORK,APWORK       XL12 FOR BOOKS, SO USE +16 FOR TYPE          
         L     R4,ACPARMA                                                       
         L     R4,16(R4)           ADDRESS OF COMFACS                           
*                                                                               
         GOTO1 VBOOKVAL,APPARM,(CLTSRC,FVIHDR),(4,APWORK),             X        
               (C'B',VSCANNER),APWORK+16,(C'C',(R4))                            
         CLI   4(R1),1             1-4 BOOKS ALLOWED                            
         BL    EBKS                                                             
         CLI   4(R1),4                                                          
         BH    EBKS                                                             
         ZIC   R0,4(R1)                                                         
         LA    R4,APWORK           BOOKS                                        
         LA    R1,APWORK+16        BOOK TYPES                                   
         LA    R8,CAMBOOKS         WHERE TO STORE THE CAMP BOOKS                
         LA    R9,CAMBKTPS         STORING CAMP BOOK TYPES                      
*                                                                               
VALRC19E MVC   0(2,R8),1(R4)                                                    
         STCM  R1,15,APFULL                                                     
         LR    RF,R1                                                            
         XC    APWORK+64(2),APWORK+64                                           
         GOTO1 AGETBKTY,APPARM,(C'A',0),APWORK+64,(RF)                          
         PRINT NOGEN                                                            
*                                                                               
         GOTO1 AGETBKTY,APPARM,(C'C',APWORK+64),1(R8),0(R9)  BOOK BITS          
         ICM   R1,15,APFULL                                                     
*                                                                               
VALRC19G LA    R4,3(R4)                                                         
         LA    R8,2(R8)                                                         
         LA    R1,1(R1)                                                         
         LA    R9,1(R9)                                                         
         BCT   R0,VALRC19E                                                      
         OC    CAMBKTPS,CAMBKTPS                                                
         BZ    VALRC19X                                                         
         MVI   CAMELLN,CAMELLQ2    YES, STILL EXPANDED DESC ELEM                
         LA    R0,CAMFSTEL+CAMELLQ2+1-CAMRECD                                   
         STCM  R0,3,CAMLEN                                                      
*                                                                               
VALRC19X BAS   RE,VALUP2           VALIDATE 2ND UPGRADE AND DATE                
         BNE   VALRECX                                                          
*                                                                               
         GOTO1 AFVAL,CPNCOMH                                                    
         BNE   VALREC20                                                         
         LA    R3,APELEM                                                        
         USING CCMEL,R3                                                         
         MVI   CCMELCD,CCMELCDQ                                                 
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CCMCOM(0),FVIFLD                                                 
         LA    R1,3(R1)                                                         
         STC   R1,CCMELLN                                                       
         GOTO1 AADDELS,CAMRECD     ADD COMMENT ELEMENT TO RECORD                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALREC20 MVI   CAMOPT,0            CLEAR OPTIONS                                
         MVI   CAMOPT2,0                                                        
         MVI   CAMDPOPT,0                                                       
         MVI   CAMRSVC,0                                                        
         TM    ESTIND,ESTIDLY      DAILY ESTIMATE ?                             
         BZ    *+8                                                              
         OI    CAMOPT,CAMODLY      YES-MAKE THE CAMPAIGN DAILY                  
         GOTO1 AFVAL,CPNOPTH                                                    
         BNE   VALREC33                                                         
         GOTO1 VSCANNER,APPARM,(58,FVIHDR),(7,AIOAREA3),C',=,='                 
         SR    R3,R3                                                            
         ICM   R3,1,4(R1)                                                       
         BZ    EIIF                                                             
         L     R4,AIOAREA3                                                      
         MVI   FVINDX,1                                                         
*                                                                               
VALREC21 CLC   =C'SIDUP',12(R4)    SIDUP OPTION                                 
         BNE   VALREC22                                                         
         CLI   22(R4),C'Y'                                                      
         BE    VALREC32                                                         
         OI    CAMOPT,CAMONSU                                                   
         CLI   22(R4),C'N'                                                      
         BE    VALREC32                                                         
*                                                                               
VALREC22 CLC   12(7,R4),AUTOADJ      AUTOADJ OPTION                             
         BNE   VALREC23                                                         
         CLC   =C'IMP',22(R4)                                                   
         BNE   *+12                                                             
         OI    CAMOPT,CAMOAIMP                                                  
         B     VALREC32                                                         
         CLC   22(3,R4),ALL                                                     
         BNE   *+12                                                             
         OI    CAMOPT,CAMOAALL                                                  
         B     VALREC32                                                         
         CLC   =C'TGT',22(R4)                                                   
         BNE   *+12                                                             
         OI    CAMOPT,CAMOATGT                                                  
         B     VALREC32                                                         
         CLI   22(R4),C'N'                                                      
         BNE   VALREC31                                                         
         OI    CAMOPT,CAMOANO                                                   
         B     VALREC32                                                         
*                                                                               
VALREC23 CLC   =C'SUBDPT ',12(R4)    SUBDPT OPTION                              
         BNE   VALREC24                                                         
         CLI   1(R4),3                                                          
         BH    VALREC31                                                         
         MVC   CAMDPOPT,22(R4)                                                  
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,EXSDMAS                                                       
         BE    VALREC32                                                         
         EX    RE,EXSDSEP                                                       
         BE    VALREC32                                                         
         EX    RE,EXSDNO                                                        
         BE    VALREC32                                                         
         B     VALREC31                                                         
*                                                                               
EXSDMAS  CLC   22(0,R4),MAS                                                     
EXSDSEP  CLC   22(0,R4),SEP                                                     
EXSDNO   CLC   22(0,R4),=C'NO '                                                 
*                                                                               
VALREC24 CLC   12(3,R4),SVC        RATING SERVICE OVERRIDE                      
         BNE   VALREC25                                                         
         MVC   CAMRSVC,22(R4)                                                   
         CLI   1(R4),1                                                          
         BL    VALREC31                                                         
         CLI   1(R4),3                                                          
         BH    VALREC31                                                         
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,EXARB                                                         
         BE    VALREC32                                                         
         EX    RE,EXNSI                                                         
         BE    VALREC32                                                         
         B     VALREC31                                                         
*                                                                               
VALREC25 CLC   =C'DAILY',12(R4)    DAILY SCHEDULING OPTION                      
         BNE   VALREC26                                                         
         OI    CAMOPT,CAMODLY                                                   
         CLI   1(R4),0                                                          
         BE    *+16                                                             
         CLI   22(R4),C'N'         ACCEPT DAILY=N                               
         BNE   *+8                                                              
         NI    CAMOPT,255-CAMODLY                                               
         CLC   22(3,R4),=C'SEP'    OPTION TO TRANSFER DAYS AS                   
         BNE   VALREC32            SEPARATE BUYLINES                            
         OI    CAMOPT2,CAMOSDLY                                                 
         B     VALREC32                                                         
*                                                                               
VALREC26 ZIC   RE,0(R4)            CANADIAN ESTIMATE LIST OPTION                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'ESTS '                                               
         BNE   VALREC27                                                         
         ICM   RE,1,1(R4)                                                       
         BZ    EIIF                                                             
         MVC   APWORK(80),SPACES                                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),22(R4)                                                 
         GOTO1 VSCANNER,APPARM,(C'C',APWORK),(12,AIOAREA4),C',=/='              
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    EIIF                                                             
         CLI   4(R1),L'CMPELIST                                                 
         BH    EIIF                                                             
         L     RF,AIOAREA4                                                      
         LA    RE,APELEM                                                        
         XC    APELEM,APELEM                                                    
         USING CESEL,RE                                                         
         MVI   CESELCD,CESELCDQ                                                 
         LA    RE,CESTS                                                         
         MVI   FVSUBX,1                                                         
         MVC   APBYTE,BEST                                                      
         DROP  RE                                                               
* FIRST CHECK FOR 'ALL' - MUST BE ONLY INPUT                                    
         CLI   4(R1),1                                                          
         BNE   VALRC26A                                                         
         TM    2(RF),X'40'                                                      
         BZ    VALRC26A                                                         
         CLI   1(RF),0                                                          
         BNE   EIIF                                                             
         CLI   0(RF),3                                                          
         BNE   EIIF                                                             
         CLC   12(3,RF),=C'ALL'                                                 
         BNE   EIIF                                                             
         MVC   0(L'CESTALL,RE),=AL2(CESTALLQ)                                   
         AHI   RE,2                                                             
         B     VALRC26D                                                         
*                                                                               
VALRC26A CLI   1(RF),0                                                          
         BNE   EIIF                                                             
         TM    2(RF),X'80'                                                      
         BZ    EIIF                                                             
         OC    4(3,RF),4(RF)                                                    
         BNZ   EIIF                                                             
         LA    R1,7(RF)                                                         
         CLC   APBYTE,0(R1)                                                     
         BE    EIIF                                                             
         STM   RE,RF,APDUB                                                      
         GOTO1 AGETEST                                                          
         BNE   VALRECX                                                          
         L     RE,APDUB                                                         
         LA    RF,APELEM+(CESTS-CESEL)                                          
*                                                                               
VALRC26B CR    RF,RE                                                            
         BNL   VALRC26C                                                         
         CLC   BEST,0(RF)                                                       
         BE    EIIF                                                             
         LA    RF,1(RF)                                                         
         B     VALRC26B                                                         
*                                                                               
VALRC26C L     RF,APDUB+4                                                       
         MVC   0(1,RE),BEST                                                     
         LA    RE,1(RE)                                                         
         LA    RF,32(RF)                                                        
         ZIC   R1,FVSUBX                                                        
         LA    R1,1(R1)                                                         
         STC   R1,FVSUBX                                                        
         BCT   R0,VALRC26A                                                      
*                                                                               
VALRC26D LA    R1,APELEM                                                        
         SR    RE,R1                                                            
         STC   RE,APELEM+1                                                      
         GOTO1 AADDELS,CAMRECD                                                  
         GOTO1 AGETEST,APBYTE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   FVSUBX,0                                                         
         B     VALREC32                                                         
*                                                                               
VALREC27 CLC   12(6,R4),=C'GOALS ' OVERRIDE MEDIA FOR GOALS                     
         BNE   VALREC28                                                         
         CLI   1(R4),1                                                          
         BNE   EIIF                                                             
         MVC   CAMGOALS,22(R4)                                                  
         B     VALREC32                                                         
*                                                                               
VALREC28 CLC   =C'IDR',12(R4)                                                   
         BNE   VALRC28A                                                         
         TM    CLTIND,CLTIBIMG     ID=MKTGRP?                                   
         BZ    EIIF                                                             
         CLI   CLTMGRID,C'A'       SEE  CIDR  IN  SPBUY07                       
         BL    EIIF                                                             
         CLI   CLTMGRID,C'K'                                                    
         BH    EIIF                                                             
         CLI   1(R4),4                                                          
         BNH   EIIF                                                             
         CLI   1(R4),6                                                          
         BH    EIIF                                                             
         B     VALRC28X                                                         
****                                                                            
VALRC28A CLC   =C'PUR',12(R4)                                                   
         BNE   VALREC29                                                         
         XC    IOKEY,IOKEY                                                      
         LA    R1,IOKEY                                                         
         USING PRPKEY,R1                                                        
         MVI   PRPKTYP,PRPKTYPQ                                                 
         MVI   PRPKSUB,PRPKSUBQ                                                 
         MVC   PRPKAGY,CUAALF                                                   
         MVC   PRPKMED,QMED                                                     
         MVC   PRPCODE,22(R4)                                                   
         OC    PRPCODE,SPACES                                                   
         DROP  R1                                                               
         GOTO1 AIO,FILRD2                                                       
         BNE   EPURP                                                            
         OI    CAMOPT2,CAMOPURP    WE HAVE A PURPOSE CODE                       
****                                                                            
VALRC28X CLI   CAMELLN,CAMELLQ2    IS IT ALREADY AN EXPANDED ELEMENT?           
         BNL   VALRC28Z            YES                                          
         XC    APELEM,APELEM                                                    
         ZIC   R1,CAMELLN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),CAMEL     COPY THE DESCRIPTION ELEMENT                 
         GOTO1 VRECUP,APPARM,(C'S',AIOAREA1),CAMEL,0    DEL OLD ONE             
         MVI   APELEM+1,CAMELLQ2    EXPANDED DESCRIPTION ELEMENT                
         GOTO1 VRECUP,APPARM,(C'S',AIOAREA1),APELEM,CAMEL                       
VALRC28Z MVC   CAMIDRNM,22(R4)     COULD BE IDR OR PURPOSE                      
         B     VALREC32                                                         
*                                                                               
VALREC29 CLC   =C'NONET',12(R4)    SUPPRESS NET DOWN                            
         BNE   VALRC29A                                                         
         OI    CAMOPT,CAMONOND                                                  
         B     VALREC32                                                         
*                                                                               
VALRC29A CLC   =C'BUYER',12(R4)    TRANSFER BUYER NAME                          
         BNE   VALREC30                                                         
         OI    CAMOPT2,CAMOBYRN                                                 
         B     VALREC32                                                         
*                                                                               
VALREC30 CLC   =C'F94',12(R4)       F94 OPTION                                  
         BNE   VALREC31                                                         
         CLI   1(R4),1                                                          
         BL    VALREC31                                                         
         CLI   1(R4),3                                                          
         BH    VALREC31                                                         
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,EXARB                                                         
         BNE   *+12                                                             
         OI    CAMOPT2,CAMOF94A                                                 
         B     VALREC32                                                         
         EX    RE,EXNSI                                                         
         BNE   VALREC31                                                         
         OI    CAMOPT2,CAMOF94N                                                 
         B     VALREC32                                                         
*                                                                               
VALREC31 B     EIIF                                                             
*                                                                               
VALREC32 LA    R4,80(R4)                                                        
         ZIC   RE,FVINDX                                                        
         LA    RE,1(RE)                                                         
         STC   RE,FVINDX                                                        
         BCT   R3,VALREC21                                                      
*                                                                               
VALREC33 XC    IOKEY,IOKEY         READ B0 PROFILE                              
         MVC   IOKEY(4),=C'S0B0'                                                
         MVC   IOKEY+4(2),CUAALF                                                
         MVC   IOKEY+6(1),QMED                                                  
         MVC   IOKEY+7(3),QCLT                                                  
         GOTO1 VGETPROF,APPARM,IOKEY,APWORK,VDMGR                               
         OC    APWORK(16),APWORK   TEST PROFILE FOUND                           
         BZ    VALREC34                                                         
         CLI   APWORK+9,C'Y'       TEST PURPOSE CODE REQUIRED                   
         BNE   VALREC34                                                         
         TM    CAMOPT2,CAMOPURP                                                 
         BZ    PURPMISS                                                         
*                                                                               
VALREC34 XC    CAMSCHEM,CAMSCHEM                                                
         MVI   FVMAXL,3                                                         
         GOTO1 AFVAL,CPNSCHH       VALIDATE SID SCHEME                          
         BH    VALRECX                                                          
         BL    *+10                                                             
         MVC   CAMSCHEM,FVIFLD                                                  
*                                                                               
         GOTO1 AFVAL,CPNPERH       VALIDATE SID PERIODS                         
         BH    VALRECX                                                          
         BE    VALREC36                                                         
         OC    CAMSCHEM,CAMSCHEM   MISSING-TEST SCHEME SPECIFIED                
         BNZ   EMIF                YES-PERIODS ARE MISSING                      
         B     VALREC40                                                         
*                                  SID PERIODS ARE ENTERED-                     
VALREC36 OC    CAMSCHEM,CAMSCHEM   TEST SCHEME SPECIFIED                        
         BNZ   VALREC37            YES                                          
         MVC   CAMSCHEM,ALL        NO-DEFAULT TO 'ALL'                          
         XC    IOKEY,IOKEY         READ SID PROFILE                             
         MVC   IOKEY(4),=C'S0SI'                                                
         MVC   IOKEY+4(2),CUAALF                                                
         MVC   IOKEY+6(1),QMED                                                  
         GOTO1 VGETPROF,APPARM,IOKEY,APWORK,VDMGR                               
         OC    APWORK(16),APWORK   TEST PROFILE FOUND                           
         BZ    VALREC37                                                         
         CLI   APWORK+2,C'N'       TEST 'ALL' IS NOT THE DEFAULT                
         BNE   VALREC37                                                         
         LA    RE,CPNSCHH          YES-SCHEME IS MISSING                        
         ST    RE,FVADDR                                                        
         B     EMIF                                                             
*                                                                               
VALREC37 GOTO1 VSCANNER,APPARM,FVIHDR,(12,AIOAREA2),C',=,='                     
         CLI   4(R1),12                                                         
         BH    EIIF                                                             
         SR    R9,R9                                                            
         ICM   R9,1,4(R1)                                                       
         BZ    EIIF                                                             
         L     R3,AIOAREA2                                                      
         LA    R4,CAMPERS                                                       
         LA    R8,1                                                             
*                                                                               
VALREC38 CLI   0(R3),0                                                          
         BE    VALREC39                                                         
         LA    RF,SRBLKLN                                                       
         XCEF  SRBLK,(RF)                                                       
         MVC   SRASIR,AIOAREA3                                                  
         MVC   SRACOM,ACOM                                                      
         MVC   SRACLPAC,VCLPACK                                                 
         MVC   SRAMSUNP,VMSUNPK                                                 
         MVC   SRADYUNP,VDAYUNPK                                                
         MVC   SRAUNTIM,VUNTIME                                                 
         MVC   SRSELSCH,CAMSCHEM                                                
         MVC   SRSELAGY,QAGY                                                    
         MVC   SRSELAM,BAGYMD                                                   
         MVC   SRSELMED,QMED                                                    
         MVC   SRSELSLN,CAMSLN                                                  
         MVC   SRSELPER,12(R3)                                                  
*****    MVC   SRSELCTY,APROF7                                                  
         MVI   SRSELCTY,C'U'                                                    
         TM    APROFBTS,A00CANAD   TEST CANADIAN                                
         BZ    *+8                  - NOPE                                      
         MVI   SRSELCTY,C'C'                                                    
*                                                                               
         GOTO1 VRANSID,APPARM,SRBLK                                             
         CLI   SRERROR,SRNOSCH                                                  
         BE    ESCH                                                             
         CLI   SRERROR,SRNOPER                                                  
         BE    EPER                                                             
         MVC   0(4,R4),12(R3)                                                   
         LA    R4,4(R4)                                                         
*                                                                               
VALREC39 LA    R3,32(R3)                                                        
         LA    R8,1(R8)                                                         
         STC   R8,FVINDX                                                        
         BCT   R9,VALREC38                                                      
         MVI   FVINDX,0                                                         
*                                                                               
VALREC40 DS    0H                  MOVED TO VALRC19C!!!                         
******                                                                          
VALREC44 GOTO1 AFVAL,CPNADJH       VALIDATE ADJACENCY CODE                      
         BH    VALRECX                                                          
         BE    VALRC44A            WE HAVE INPUT                                
         CLI   CLTPROF+9,C'0'      NO INPUT, DO WE NEED ANY?                    
         BNE   EMIF                          YES                                
         B     VALREC45                      NO                                 
*                                                                               
VALRC44A GOTO1 AVALADJ,FVIFLD                                                   
         BNE   VALRECX                                                          
         MVC   CAMADJ,QADJCD                                                    
*                                                                               
VALREC45 GOTO1 AFVAL,CPNSL1H       VALIDATE SPOT LENGTH EQUIVALENCES            
         BH    VALRECX                                                          
         BL    VALREC46                                                         
         GOTO1 AVALSLEQ                                                         
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   VALRECX                                                          
*                                                                               
VALREC46 MVI   APBYTE,1                                                         
         XC    CAMWKS,CAMWKS                                                    
         CLI   CPNUP2H+5,0         TEST 2ND UPGRADE GIVEN                       
         BE    *+8                                                              
         MVI   FVMINL,1            YES-FLIGHT DATES ARE MANDATORY               
         GOTO1 AFVAL,CPNFW1H       VALIDATE NON-CONTINUOUS WEEKS                
         BH    VALRECX                                                          
         BL    VALREC54                                                         
         OI    CAMOPT,CAMOWKS      INDICATE NON-CONTINUOUS WEEKS                
         GOTO1 VDATCON,APPARM,(3,CAMSTRT),(0,APELEM)                            
         GOTO1 VDATCON,(R1),(3,CAMEND),(0,APELEM+6)                             
         XC    LFLST,LFLST                                                      
         LA    R9,CAMWKS                                                        
         LA    RE,NMAXWKS                                                       
         TM    CAMOPT,CAMODLY      TEST DAILY SCHEDULING                        
         BZ    *+8                                                              
         LA    RE,2                YES-LIMIT TO TWO WEEKS                       
         SLL   RE,2                                                             
         AR    RE,R9                                                            
         ST    RE,LFULL            LFULL=END OF WEEK LIST                       
*                                                                               
VALREC47 GOTO1 VSCANNER,APPARM,FVIHDR,(14,AIOAREA3),C',=,-'                     
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    EIIF                                                             
         L     R3,AIOAREA3                                                      
         LA    R8,1                                                             
*                                                                               
VALREC48 SR    R5,R5                                                            
         GOTO1 VDATVAL,APPARM,(0,12(R3)),APWORK                                 
         OC    0(4,R1),0(R1)                                                    
         BNZ   VALREC49                                                         
         GOTO1 VDATVAL,APPARM,(1,12(R3)),APWORK                                 
         OC    0(4,R1),0(R1)                                                    
         BZ    VALREC59                                                         
*                                                                               
VALREC49 CLC   APWORK(2),=C'00'                                                 
         BNE   VALREC50                                                         
         MVC   APWORK(2),APELEM                                                 
         CLC   APELEM(2),APELEM+6                                               
         BE    VALREC50                                                         
         CLC   APWORK+2(4),APELEM+2                                             
         BNL   VALREC50                                                         
         MVC   APWORK(2),APELEM+6                                               
*                                                                               
VALREC50 CLC   APWORK(6),APELEM                                                 
         BL    VALREC58                                                         
         CLC   APWORK(6),APELEM+6                                               
         BH    VALREC58                                                         
         LA    R4,APWORK                                                        
         BRAS  RE,GETMON                                                        
         OC    LFLST,LFLST                                                      
         BZ    *+14                                                             
         MVC   APWORK(6),APWORK+12                                              
         B     *+16                                                             
         MVC   LFLSTMON,APWORK+12  SAVE FLIGHT START MONDAY                     
         MVC   LFLST,APWORK        SAVE FLIGHT START DATE                       
         C     R9,LFULL                                                         
         BL    *+16                                                             
         TM    CAMOPT,CAMODLY                                                   
         BZ    EPTL                                                             
         B     EPTL2                                                            
         GOTO1 VDATCON,APPARM,(0,APWORK),(2,(R9))                               
         GOTO1 VADDAY,(R1),APWORK+12,APWORK+18,6                                
         CLC   APWORK+18(6),APELEM+6                                            
         BNH   *+10                                                             
         MVC   APWORK+18(6),APELEM+6                                            
         GOTO1 VDATCON,(R1),(0,APWORK+18),(2,2(R9))                             
         MVC   LFLND,APWORK+18     SAVE FLIGHT END DATE                         
         GOTO1 VADDAY,(R1),APWORK+12,APELEM,7                                   
         LA    R9,4(R9)                                                         
         CLI   1(R3),0             TEST FOR -NW                                 
         BE    VALREC53            NO                                           
         LTR   R5,R5               YES-TEST VALIDATED YET                       
         BNZ   VALREC52            YES                                          
         SR    RE,RE               NO-THEN VALIDATE IT                          
         LA    RF,23(R3)                                                        
         CLI   1(R3),2                                                          
         BE    VALREC51                                                         
         LA    RE,1                                                             
         LA    RF,24(R3)                                                        
         CLI   1(R3),3                                                          
         BNE   VALREC58                                                         
*                                                                               
VALREC51 CLI   0(RF),C'W'                                                       
         BNE   VALREC58                                                         
         CLI   22(R3),C'1'                                                      
         BL    VALREC58                                                         
         CLI   22(R3),C'9'                                                      
         BH    VALREC58                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,22(0,R3)                                                   
         TM    APDUB+7,X'0F'                                                    
         BNO   VALREC58                                                         
         CVB   R5,APDUB            R5=N'CONSECUTIVE WEEKS                       
*                                                                               
VALREC52 MVC   APWORK(6),APELEM    APWORK(6)=MONDAY OF NEXT WEEK                
         BCT   R5,VALREC50                                                      
*                                                                               
VALREC53 LA    R3,32(R3)                                                        
         LA    R8,1(R8)                                                         
         BCT   R0,VALREC48                                                      
         L     R5,ATWA             RESTORE R5=A(TWA)                            
*                                                                               
VALREC54 CLI   APBYTE,1                                                         
         BNE   VALREC55                                                         
         MVI   APBYTE,2                                                         
         GOTO1 AFVAL,CPNFW2H       VALIDATE 2ND FLIGHT WEEKS LINE               
         BH    VALRECX                                                          
         BL    VALREC55                                                         
         TM    CAMOPT,CAMOWKS                                                   
         BZ    EIIF                                                             
         B     VALREC47                                                         
*                                                                               
VALREC55 GOTO1 AFVAL,CPNFNDH       VALIDATE FLIGHT END DATE                     
         BH    VALRECX                                                          
         BL    VALREC60                                                         
         TM    CAMOPT,CAMOWKS                                                   
         BZ    EIIF                                                             
         GOTO1 VDATVAL,APPARM,(0,CPNFND),APWORK                                 
         OC    0(4,R1),0(R1)                                                    
         BNZ   VALREC56                                                         
         GOTO1 VDATVAL,APPARM,(1,CPNFND),APWORK                                 
         OC    0(4,R1),0(R1)                                                    
         BZ    EIDAT                                                            
*                                                                               
VALREC56 CLC   APWORK(2),=C'00'                                                 
         BNE   VALREC57                                                         
         MVC   APWORK(2),LFLND                                                  
         CLC   APWORK+2(4),LFLND+2                                              
         BNH   VALREC57                                                         
         MVC   APWORK(2),LFLSTMON                                               
*                                                                               
VALREC57 SHI   R9,4                                                             
         GOTO1 VDATCON,APPARM,(0,APWORK),(2,APHALF)                             
         CLC   APHALF,2(R9)                                                     
         BH    EIIF                                                             
         CLC   APHALF,0(R9)                                                     
         BL    EIIF                                                             
         MVC   2(2,R9),APHALF                                                   
         MVC   LFLND,APWORK                                                     
         B     VALREC60                                                         
*                                                                               
VALREC58 STC   R8,FVINDX                                                        
         B     EIIF                                                             
VALREC59 STC   R8,FVINDX                                                        
         B     EIDAT                                                            
*                                                                               
VALREC60 CLI   APACTN,ACTCHA       TEST ACTION=CHANGE                           
         BNE   VALREC62                                                         
         TM    CAMINDS,CAMISKD+CAMIGOAL AND SPOTS SCHEDULED OR GOALS            
         BZ    VALREC62                                                         
         CLC   CAMWKS,SVCAMWKS     AND CHANGE IN FLIGHT WEEKS                   
         BNE   EFLWK               YES-ERROR                                    
*                                                                               
VALREC62 TM    CAMOPT,CAMOWKS      TEST NON-CONTIGUOUS WEEKS                    
         BZ    VALREC64                                                         
         CLI   CPNUP2H+5,0         YES-TEST 2ND UPGRADE                         
         BE    VALREC66                                                         
         OC    CAMWKS+4(2),CAMWKS+4    YES-CHECK AT LEAST 2 FLIGHT WKS          
         BZ    EUPDT                                                            
         CLC   CMPU2DAT,CAMWKS+2   CHECK EFFECTIVE DATE AFTER 1ST WEEK          
         BNH   EUPDT                                                            
         CLC   LUPDATE,LFLND       CHECK EFFECTIVE DATE WITHIN FLIGHT           
         BH    EUPDT                                                            
         B     VALREC66                                                         
*                                                                               
VALREC64 GOTO1 VDATCON,APPARM,(3,CAMSTRT),(0,APWORK) GET STRT/END DATES         
         GOTO1 VDATCON,(R1),(3,CAMEND),(0,APWORK+6)                             
********                                                                        
         LA    R4,53               NEW DAILY LIMIT FOR SONY                     
********                                                                        
         TM    CAMOPT,CAMODLY      TEST DAILY SKED                              
         BO    VALREC65                                                         
         LA    R4,APWORK           NO-GET MONDAYS OF START/END WEEKS            
         BRAS  RE,GETMON                                                        
         MVC   APWORK(6),APWORK+12                                              
         LA    R4,APWORK+6                                                      
         BRAS  RE,GETMON                                                        
         MVC   APWORK+6(6),APWORK+12                                            
********                                                                        
         LA    R4,365              NEW WEEKLY DAY LIMIT FOR SONY                
********                                                                        
*                                                                               
VALREC65 GOTO1 VPERVERT,APPARM,APWORK,APWORK+6   CANNOT BE MORE THAN            
         CLM   R4,3,8(R1)                        13 OR 2 WEEKS DIFF             
         BNL   VALREC66                                                         
         LA    R1,CPNDATH                                                       
         ST    R1,FVADDR                                                        
         TM    CAMOPT,CAMODLY                                                   
         BO    EPTL2                                                            
         B     EPTL                                                             
*                                                                               
VALREC66 LA    R1,FILADD1          ADD/PUT THE CAMPAIGN RECORD                  
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,FILPUT1                                                       
         GOTO1 AIO                                                              
         MVC   SAVEDA,IODA         SAVE D/A FOR PASSIVE KEYS                    
*                                                                               
         MVC   LCAMP,CAMKCAM                                                    
         XC    LCAMP,EFFS                                                       
         SR    R1,R1                                                            
         ICM   R1,3,LCAMP                                                       
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  CPNNUM,APDUB                                                     
         OI    CPNNUMH+6,X'80'                                                  
*                                                                               
         MVC   CMPNM,CAMNAME       SET CURRENT CAMPAIGN VALUES                  
         MVC   CMPCLTC,CAMCLT                                                   
         MVC   CMPPRDN,CAMPRD                                                   
         MVC   CMPESTN,CAMEST                                                   
         MVC   CMPST,CAMSTRT                                                    
         MVC   CMPND,CAMEND                                                     
         MVC   CMPPRDC,CAMPRDC                                                  
         MVC   CMPUF,CAMUPFIL                                                   
         MVC   CMPUP,CAMUPGRD                                                   
         MVC   CMPFB,CAMFRBK                                                    
         CLI   CAMELLN,CAMELLQ2    EXTENDED LENGTH?                             
         BNE   *+10                 - NOPE                                      
         MVC   CMPFBTP,CAMFRBKT    CMPFRBK BOOK TYPE                            
         MVC   CMPUPIN,CAMINPUT                                                 
         MVC   CMPUPUT,CAMUPUT                                                  
         MVC   CMPUSHR,CAMUSHR                                                  
         MVC   CMPSLN,CAMSLN                                                    
         MVC   CMPOPTS,CAMOPT                                                   
         MVC   CMPOPTS2,CAMOPT2                                                 
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT                               
         BNZ   VALREC67                                                         
         TM    CMPOPTS,CAMOANO                                                  
         BO    VALREC67                                                         
         CLI   CLTBWPRO+3,C'I'                                                  
         BNE   *+8                                                              
         OI    CMPOPTS,CAMOAIMP                                                 
         CLI   CLTBWPRO+3,C'A'                                                  
         BNE   *+8                                                              
         OI    CMPOPTS,CAMOAALL                                                 
         CLI   CLTBWPRO+3,C'T'                                                  
         BNE   VALREC67                                                         
         OI    CMPOPTS,CAMOATGT                                                 
*                                                                               
VALREC67 MVC   CMPDPOPT,CAMDPOPT                                                
         CLI   CMPDPOPT,0                                                       
         BNE   *+10                                                             
         MVC   CMPDPOPT,CLTBWPRO+4                                              
         MVC   CMPRSVC,CAMRSVC                                                  
         MVC   CLTSRC,CLTSRCDF     SET SERVICE TO CLIENT'S DEFAULT,             
         CLI   CMPRSVC,0           UNLESS RATING SERVICE OVERRIDE               
         BE    *+10                                                             
         MVC   CLTSRC,CMPRSVC                                                   
         MVC   CMPBOOKS,CAMBOOKS                                                
         CLI   CAMELLN,CAMELLNQ    OLD LENGTH?                                  
         BNH   *+10                 - NOPE                                      
         MVC   CMPBKTPS,CAMBKTPS    - YUP, EXTENDED LENGTH                      
         MVC   CMPPRD1,CAMPPRD1                                                 
         MVC   CMPPRD2,CAMPPRD2                                                 
         MVC   CMPLEN1,CAMPLEN1                                                 
         MVC   CMPLEN2,CAMPLEN2                                                 
         MVC   CMPSCHEM,CAMSCHEM                                                
         MVC   CMPPERS,CAMPERS                                                  
         MVC   CMPADJ,CAMADJ                                                    
         MVC   CMPGOALS,CAMGOALS                                                
         MVI   CMPIND,0                                                         
         TM    CAMINDS,CAMISKD                                                  
         BZ    *+8                                                              
         OI    CMPIND,CMPISKD                                                   
         TM    CAMINDS,CAMIGOAL                                                 
         BZ    *+8                                                              
         OI    CMPIND,CMPIGOAL                                                  
         TM    CAMINDS,CAMIFR1                                                  
         BZ    *+8                                                              
         OI    CMPIND,CMPIFR1                                                   
*                                                                               
         TM    CAMOPT,CAMODLY+CAMOWKS  TEST DAILY SKED AND FLIGHT WKS           
         BNO   *+10                                                             
         MVC   CMPWKDLY,CAMWKS     YES-SAVE FLIGHT WEEKS FOR DAILY SKED         
*                                                                               
         BRAS  RE,CAMNMWKS         SET CMPNWKS AND CMPDATSP                     
         CLI   CPNCCNH+5,0         ANY COMPANION CAMPAIGN?                      
         BE    VALREC84            NONE, NO LIMIT OF 14 TO # OF WKS             
         CLI   CMPNWKS,14          YES, CURRENTLY ONLY 14 WEEKS ALLOWED         
         BNH   VALREC84              FOR COMPANION AND PARTNER                  
         LA    R1,CPNCCNH          POINT TO COMPANION CAMP FIELD                
         ST    R1,FVADDR                                                        
         B     ECCAM               COMP CAMPS CAN'T HAVE MORE THAN 14           
*                                                                               
VALREC84 LR    R3,R5               BUILD LIST OF DISPLAY DATES                  
         AHI   R3,CMPDATSD+6-TWAD                                               
         LR    R4,R5                                                            
         AHI   R4,CMPDATSP+4-TWAD                                               
*                                                                               
VALREC86 CLI   0(R4),X'FF'                                                      
         BE    VALREC88                                                         
         GOTO1 VDATCON,APPARM,(2,(R4)),(0,(R3))                                 
         LA    R3,6(R3)                                                         
         MVC   CMPNDMNP,0(R4)      END MONDAY                                   
         LA    R4,4(R4)                                                         
         B     VALREC86                                                         
*                                                                               
VALREC88 MVI   0(R3),X'FF'         MARK EOL                                     
*                                                                               
         XC    IOKEY,IOKEY         READ SID PROFILE                             
         MVC   IOKEY(4),=C'S0SI'                                                
         MVC   IOKEY+4(2),CUAALF                                                
         MVC   IOKEY+6(1),QMED                                                  
         CLC   CMPSCHEM,ALL                                                     
         BE    *+10                                                             
         MVC   IOKEY+7(3),CMPSCHEM                                              
         GOTO1 VGETPROF,APPARM,IOKEY,APWORK,VDMGR                               
         OC    APWORK(16),APWORK   TEST PROFILE FOUND                           
         BZ    VALREC90                                                         
         CLI   APWORK,C'Y'         YES - TEST COMPETITION USER                  
         BNE   VALREC90                                                         
         OI    CMPIND,CMPICMPU           YES                                    
*                                                                               
VALREC90 LA    R3,CAMEL                                                         
         SR    R0,R0                                                            
         XC    APDUB,APDUB                                                      
*                                                                               
VALREC92 CLI   0(R3),0                                                          
         BE    VALREC93                                                         
         LA    R1,APDUB                                                         
         CLI   0(R3),CEQELCDQ                                                   
         BE    *+16                                                             
         LA    R1,APDUB+4                                                       
         CLI   0(R3),CESELCDQ                                                   
         BNE   *+8                                                              
         ST    R3,0(R1)                                                         
*                                                                               
         CLI   1(R3),0             NO LENGTH IN THIS ELEMENT?                   
         BNE   *+12                                                             
         MVI   0(R3),0             NONE, SHOULDN'T BE AN ELEMENT CODE           
         B     VALREC93                                                         
*                                                                               
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     VALREC92                                                         
*                                                                               
VALREC93 ICM   R3,15,APDUB                                                      
         BZ    VALREC94                                                         
         USING CEQEL,R3                                                         
         ZIC   RF,CEQELLN                                                       
         SH    RF,=Y(CEQSLN-CEQEL-1)                                            
         BM    VALREC94                                                         
         LA    RE,L'CMPSLEQU-1                                                  
         CR    RF,RE                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         EX    RF,*+4                                                           
         MVC   CMPSLEQU,CEQSLN                                                  
         DROP  R3                                                               
*                                                                               
VALREC94 XC    CMPELIST,CMPELIST                                                
         ICM   R3,15,APDUB+4                                                    
         BZ    VALREC96                                                         
         USING CESEL,R3                                                         
         ZIC   RE,CESELLN                                                       
         SH    RE,=Y(CESTS-CESEL)                                               
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   CMPELIST(0),CESTS                                                
         DROP  R3                                                               
*                                                                               
VALREC96 MVC   QCAM,CPNNUM                                                      
         MVC   BCAM,CAMKCAM                                                     
*                                                                               
         BRAS  RE,CHEKLIST                                                      
*                                                                               
         CLI   APACTN,ACTADD     ACTION=ADD?                                    
         BNE   VALREC97          NO                                             
         GOTO1 =A(ADDPASIV),RR=APRELO  -YES...ADD PASSIVE POINTERS              
VALREC97 CLI   APACTN,ACTCHA     ACTION=CHANGE?                                 
         BNE   VALRECX           NO                                             
*                                                                               
         CLI   CMPDSTDT,0          ARE WE DISPLACED INTO CAMPAIGN WKS?          
         BE    *+8                                                              
         MVI   CMPDSTDT,0          YES, HAVE USER START FROM CMP START          
         CLC   CAMCLT,SVCCLT     CLIENT CHANGED?                                
         BNE   VALREC98          YES                                            
         CLC   CAMPRD,SVCPRD     PRODUCT CHANGED?                               
         BNE   VALREC98          YES                                            
         CLC   CAMEST,SVCEST     ESTIMATE CHANGED?                              
         BE    VALRECX           NO                                             
*                                                                               
VALREC98 GOTO1 =A(ADDPASIV),RR=APRELO  -YES...CHANGE PASSIVE POINTERS           
*                                                                               
VALRECX  B     EXIT                                                             
         SPACE 2                                                                
EXARB    CLC   22(0,R4),ARB                                                     
EXNSI    CLC   22(0,R4),NSI                                                     
         EJECT                                                                  
***********************************************************************         
* VALIDATE SECOND UPGRADE AND EFFECTIVE DATE                          *         
***********************************************************************         
         SPACE 1                                                                
VALUP2   XC    CMPU2DAT(CMPUSHR2+L'CMPUSHR2-CMPU2DAT),CMPU2DAT                  
         XC    CMPFB2L,CMPFB2L                                                  
*                                                                               
         LR    R0,RE                                                            
*                                                                               
         CLI   CPNUP2H+5,0         TEST FOR 2ND UPGRADE                         
         BE    VALU2                                                            
         MVI   APFLAG,X'D8'        YES-VALIDATE                                 
         GOTO1 AVALUPG,CPNUP2H                                                  
         BNE   VALUX                                                            
         XC    APELEM,APELEM       BUILD 2ND UPGRADE ELEMENT                    
         LA    R4,APELEM                                                        
         USING CUPEL,R4                                                         
         MVI   CUPELCD,CUPELCDQ                                                 
         MVI   CUPELLN,CUPELLNQ                                                 
         MVC   CUPFILE(CUPINPUT-CUPFILE),APWORK                                 
         MVC   CUPINPUT,CPNUP2                                                  
         MVC   CUPPUT,APWORK+16                                                 
         MVC   CUPSHR,APWORK+17                                                 
         MVC   CUPFRBKL,APWORK+18                                               
         TM    CUPFRBK+1,BTY2CHAR  2 CHARACTER BOOKTYPE?                        
         BNO   VALU1E               - NOPE                                      
         MVC   CUPFRBKT,QBOOKTYP                                                
         MVI   CUPELLN,CUPELLQ2    YES, STILL EXPANDED DESC ELEM                
         ICM   R1,3,CAMLEN                                                      
         AHI   R1,CUPELLQ2-CUPELLNQ                                             
         STCM  R1,3,CAMLEN                                                      
*                                                                               
VALU1E   MVC   CMPUF2,CUPFILE      SAVE 2ND UPGRADE VALUES                      
         MVC   CMPUP2,CUPGRADE                                                  
         MVC   CMPFB2,CUPFRBK                                                   
         MVC   CMPFB2TP,CUPFRBKT                                                
         MVC   CMPFB2L,CUPFRBKL                                                 
         MVC   CMPUPIN2,CUPINPUT                                                
         MVC   CMPUPUT2,CUPPUT                                                  
         MVC   CMPUSHR2,CUPSHR                                                  
         MVI   FVMINL,1                                                         
*                                                                               
VALU2    GOTO1 AVALDAT,CPNUPDH     VALIDATE EFFECTIVE DATE                      
         BNE   VALUX                                                            
         OC    APWORK(6),APWORK    TEST DATE GIVEN                              
         BZ    VALUX               NO-THEREFORE THERE'S NO UPGRADE              
         LA    R1,CPNUP2H                                                       
         CLI   5(R1),0             YES-TEST FOR UPGRADE                         
         BE    VALU9               MISSING                                      
         MVC   LUPDATE,APWORK                                                   
         GOTO1 VDATCON,APPARM,APWORK,(2,CMPU2DAT)                               
         MVC   CUPDATE,CMPU2DAT                                                 
         GOTO1 AADDELS,CAMRECD                                                  
         LA    R1,CPNUPGH          MAKE SURE FIRST UPGRADE GIVEN                
         CLI   5(R1),0                                                          
         BNE   VALUX                                                            
*                                                                               
VALU9    MVC   FVMSGNO,=AL2(FVFNONE)   MISSING UPGRADE                          
         ST    R1,FVADDR                                                        
*                                                                               
VALUX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD KEY                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   GOTO1 ADISPKEY                                                         
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECORD                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         TWAXC CPNCLTH                                                          
         OC    BYRPW,BYRPW         TEST BUYER PASSWORD REQUIRED                 
         BZ    DISREC2                                                          
         GOTO1 AVALPWD             YES - VALIDATE PASSWORD                      
         BNE   VALRECX                                                          
*                                                                               
DISREC2  MVC   APFULL(L'CAMKCAM),CAMKCAM                                        
         XC    APFULL(L'CAMKCAM),EFFS                                           
         SR    R1,R1                                                            
         ICM   R1,3,APFULL                                                      
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  CPNNUM,APDUB                                                     
         OI    CPNNUMH+6,FVOXMT                                                 
*                                                                               
         GOTO1 AGETCLT,CAMCLT                                                   
         BE    DISREC3                                                          
         CLC   FVMSGNO,=AL2(FVSECLOK)                                           
         BE    VALRECX                                                          
         B     DISREC4                                                          
*                                                                               
DISREC3  MVC   CPNCLT,QCLT                                                      
         CLC   CPNCNM,CLTNM                                                     
         BE    DISREC4                                                          
         MVC   CPNCNM,CLTNM                                                     
         OI    CPNCNMH+6,FVOXMT                                                 
*                                                                               
DISREC4  MVI   BPRD,0                                                           
         GOTO1 AGETPRD,CAMPRD                                                   
         BNE   DISREC5                                                          
         MVC   CPNPRD,QPRD                                                      
         CLC   CPNPNM,PRDNM                                                     
         BE    DISREC5                                                          
         MVC   CPNPNM,PRDNM                                                     
         OI    CPNPNMH+6,FVOXMT                                                 
*                                                                               
DISREC5  OC    CAMPGRP,CAMPGRP     TEST PRODUCT GROUP                           
         BZ    DISREC6                                                          
         GOTO1 ADISPGR             YES-DISPLAY PRODUCT GROUP                    
         BNE   DISRECX                                                          
*                                                                               
DISREC6  GOTO1 AGETEST,CAMEST                                                   
         BNE   DISRECX                                                          
         MVC   CPNEST,QEST                                                      
         CLC   CPNENM,ESTNM                                                     
         BE    *+14                                                             
         MVC   CPNENM,ESTNM                                                     
         OI    CPNENMH+6,FVOXMT                                                 
*                                                                               
         GOTO1 VDATCON,APPARM,(3,CAMSTRT),(5,CPNDAT)                            
         MVI   CPNDAT+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,CAMEND),(5,CPNDAT+9)                                
*                                                                               
DISREC7  OC    CAMCCAM,CAMCCAM     COMPANION CAMPAIGN                           
         BZ    DISREC8                                                          
         EDIT  CAMCCAM,(5,CPNCCN),ALIGN=LEFT                                    
*                                                                               
DISREC8  SR    R0,R0                                                            
         ICM   R0,1,CAMSLN                                                      
         BZ    DISREC9                                                          
         EDIT  (R0),(3,CPNLEN),ALIGN=LEFT                                       
*                                                                               
DISREC9  MVC   CPNCAN,CAMNAME                                                   
*                                                                               
         MVC   CPNUPG,CAMINPUT     UPGRADE                                      
*                                                                               
         LA    R3,CAMFSTEL         LOCATE THE ELEMENTS                          
         SR    R0,R0                                                            
         XC    LACOMEL,LACOMEL                                                  
         XC    LACEQEL,LACEQEL                                                  
         XC    LACUPEL,LACUPEL                                                  
         XC    LACESEL,LACESEL                                                  
*                                                                               
DISREC10 CLI   0(R3),0                                                          
         BE    DISREC12                                                         
         LA    R1,LACOMEL                                                       
         CLI   0(R3),CCMELCDQ                                                   
         BE    DISREC11                                                         
         LA    R1,LACEQEL                                                       
         CLI   0(R3),CEQELCDQ                                                   
         BE    DISREC11                                                         
         LA    R1,LACUPEL                                                       
         CLI   0(R3),CUPELCDQ                                                   
         BE    DISREC11                                                         
         LA    R1,LACESEL                                                       
         CLI   0(R3),CESELCDQ                                                   
         BNE   DISREC11+4                                                       
*                                                                               
DISREC11 ST    R3,0(R1)                                                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DISREC10                                                         
*                                                                               
DISREC12 ICM   R3,15,LACUPEL       TEST 2ND UPGRADE                             
         BZ    DISREC13                                                         
         USING CUPEL,R3            YES-FORMAT 2ND UPGRADE                       
         MVC   CPNUP2,CUPINPUT                                                  
         GOTO1 VDATCON,APPARM,(2,CUPDATE),(8,CPNUPD) AND EFFECTIVE DATE         
*                                                                               
DISREC13 ICM   R3,15,LACOMEL                                                    
         BZ    DISREC14                                                         
         USING CCMEL,R3                                                         
         ZIC   R1,CCMELLN                                                       
         LA    R0,CCMCOM+1-CCMEL                                                
         SR    R1,R0                                                            
         EX    R1,*+4                                                           
         MVC   CPNCOM(0),CCMCOM                                                 
*                                                                               
DISREC14 MVC   CPNPIG(15),SPACES                                                
         MVC   SVBPRD,BPRD                                                      
         GOTO1 AGETPRD,CAMPPRD1                                                 
         BNE   DISREC16                                                         
         OI    CPNPIGH+6,FVOXMT                                                 
         LA    R4,CPNPIG                                                        
         MVC   0(3,R4),QPRD                                                     
*                                                                               
         BAS   RE,GETSPAC                                                       
*                                                                               
         MVI   0(R4),C'-'                                                       
         GOTO1 AGETPRD,CAMPPRD2                                                 
         BNE   DISREC16                                                         
         TM    CAMINDS,CAMIFR1                                                  
         BZ    *+12                                                             
         MVI   1(R4),C'('                                                       
         LA    R4,1(R4)                                                         
         MVC   1(3,R4),QPRD                                                     
*                                                                               
         BAS   RE,GETSPAC                                                       
*                                                                               
         TM    CAMINDS,CAMIFR1                                                  
         BZ    *+12                                                             
         MVI   0(R4),C')'                                                       
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'/'                                                       
         SR    R0,R0                                                            
         ICM   R0,1,CAMPLEN1                                                    
         BZ    DISREC16                                                         
         EDIT  (R0),(3,1(R4)),ALIGN=LEFT                                        
*                                                                               
         BAS   RE,GETSPAC                                                       
*                                                                               
         MVI   0(R4),C'-'                                                       
         SR    R0,R0                                                            
         ICM   R0,1,CAMPLEN2                                                    
         BZ    DISREC16                                                         
         EDIT  (R0),(3,1(R4)),ALIGN=LEFT                                        
         B     DISREC16                                                         
*                                                                               
GETSPAC  LA    R4,CPNPIG                                                        
         LA    RF,15                                                            
*                                                                               
GETSPAC2 CLI   0(R4),X'40'                                                      
         BER   RE                                                               
         LA    R4,1(R4)                                                         
         BCT   RF,GETSPAC2                                                      
         DS    H'0'                                                             
*                                                                               
DISREC16 MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   BPRD,SVBPRD                                                      
         LA    R4,CPNOPT                                                        
         CLI   CAMOPT,0                                                         
         BE    DISREC20                                                         
         TM    CAMOPT,CAMONSU                                                   
         BZ    *+14                                                             
         MVC   0(8,R4),=C'SIDUP=N,'                                             
         LA    R4,8(R4)                                                         
         TM    CAMOPT,CAMOAIMP+CAMOAALL+CAMOATGT+CAMOANO                        
         BZ    DISREC18                                                         
         MVC   0(10,R4),AUTOADJ                                                 
         LR    R1,R4                                                            
         LA    R4,10(R4)                                                        
         TM    CAMOPT,CAMOANO                                                   
         BO    DISREC18                                                         
         MVC   8(4,R1),ALL                                                      
         LA    R4,2(R4)                                                         
         TM    CAMOPT,CAMOAALL                                                  
         BO    DISREC18                                                         
         MVC   8(3,R1),=C'IMP'                                                  
         TM    CAMOPT,CAMOAIMP                                                  
         BO    DISREC18                                                         
         MVC   8(3,R1),=C'TGT'                                                  
*                                                                               
DISREC18 TM    CAMOPT,CAMODLY      DAILY SCHEDULING                             
         BZ    DISREC19                                                         
         MVC   0(5,R4),=C'DAILY'                                                
         LA    R4,5(R4)                                                         
         TM    CAMOPT2,CAMOSDLY                                                 
         BZ    *+14                                                             
         MVC   0(4,R4),=C'=SEP'                                                 
         LA    R4,4(R4)                                                         
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
DISREC19 TM    CAMOPT,CAMONOND     SUPPRESS NET DOWN                            
         BZ    DISREC20                                                         
         MVC   0(6,R4),=C'NONET,'                                               
         LA    R4,6(R4)                                                         
*                                                                               
DISREC20 CLI   CAMDPOPT,0                                                       
         BE    DISREC23                                                         
         LA    R1,11                                                            
         MVC   0(7,R4),=C'SUBDPT='                                              
         MVC   7(4,R4),MAS                                                      
         CLI   CAMDPOPT,C'M'                                                    
         BE    DISREC22                                                         
         MVC   7(4,R4),SEP                                                      
         CLI   CAMDPOPT,C'S'                                                    
         BE    DISREC22                                                         
         MVC   7(4,R4),=C'NO, '                                                 
         LA    R1,10                                                            
         CLI   CAMDPOPT,C'N'                                                    
         BE    DISREC22                                                         
         DC    H'0'                                                             
DISREC22 LA    R4,0(R1,R4)                                                      
*                                                                               
DISREC23 CLI   CAMRSVC,0                                                        
         BE    DISRC23B                                                         
         MVC   0(4,R4),SVC                                                      
         MVC   4(4,R4),ARB                                                      
         CLI   CAMRSVC,C'A'                                                     
         BE    DISRC23A                                                         
         MVC   4(4,R4),NSI                                                      
         CLI   CAMRSVC,C'N'                                                     
         BE    DISRC23A                                                         
         DC    H'0'                                                             
DISRC23A LA    R4,8(R4)                                                         
*                                                                               
DISRC23B ICM   R8,15,LACESEL       CANADIAN ESTIMATE LIST                       
         BZ    DISRC23E                                                         
         MVC   0(5,R4),=C'ESTS='                                                
         LA    R4,5(R4)                                                         
         ZIC   R9,1(R8)                                                         
         AR    R9,R8                                                            
         LA    R8,CESTS-CESEL(R8)                                               
         SR    R1,R1                                                            
* FIRST CHECK FOR ALL ESTS                                                      
         CLC   0(L'CESTALL,R8),=AL2(CESTALLQ)                                   
         BNE   DISRC23C                                                         
         MVC   0(4,R4),=C'ALL,'                                                 
         LA    R4,4(R4)                                                         
         B     DISRC23E                                                         
*                                                                               
DISRC23C IC    R1,0(R8)                                                         
         CVD   R1,APDUB                                                         
         LA    RE,2                                                             
         CHI   R1,100                                                           
         BNL   DISRC23D                                                         
         BCTR  RE,0                                                             
         CHI   R1,10                                                            
         BNL   DISRC23D                                                         
         BCTR  RE,0                                                             
*                                                                               
DISRC23D LR    RF,RE                                                            
         SLL   RF,4                                                             
         LA    RF,7(RF)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  0(0,R4),APDUB                                                    
         AR    R4,RE                                                            
         OI    0(R4),X'F0'                                                      
         MVI   1(R4),C'/'                                                       
         LA    R4,2(R4)                                                         
         LA    R8,1(R8)                                                         
         CR    R8,R9                                                            
         BL    DISRC23C                                                         
         BCTR  R4,0                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
DISRC23E CLI   CAMGOALS,0                                                       
         BE    DISRC23F                                                         
         MVC   0(6,R4),=C'GOALS='                                               
         MVC   6(1,R4),CAMGOALS                                                 
         MVI   7(R4),C','                                                       
         LA    R4,8(R4)                                                         
*                                                                               
DISRC23F TM    CAMOPT2,CAMOBYRN                                                 
         BZ    DISRC23G                                                         
         MVC   0(5,R4),=C'BUYER'                                                
         MVI   5(R4),C','                                                       
         LA    R4,6(R4)                                                         
*                                                                               
DISRC23G TM    CAMOPT2,CAMOF94A+CAMOF94N                                        
         BZ    DISRC23H                                                         
         MVC   0(7,R4),=C'F94=ARB'                                              
         TM    CAMOPT2,CAMOF94A                                                 
         BO    *+10                                                             
         MVC   4(3,R4),=C'NSI'                                                  
         MVI   7(R4),C','                                                       
         LA    R4,8(R4)                                                         
*                                                                               
DISRC23H CLI   CAMELLN,CAMELLNQ    EXPANDED DESC ELEMENT?                       
         BNH   DISREC24                                                         
         OC    CAMIDRNM,CAMIDRNM   YES, DO WE HAVE IDR NAME?                    
         BZ    DISREC24                                                         
         MVC   0(4,R4),=C'IDR='    YES                                          
         TM    CAMOPT2,CAMOPURP                                                 
         BZ    *+10                                                             
         MVC   0(4,R4),=C'PUR='    YES                                          
         MVC   4(L'CAMIDRNM,R4),CAMIDRNM                                        
*                                                                               
         LA    R4,4(R4)            FIT AS MUCH AS WE CAN WITHOUT SPACES         
         CLI   0(R4),C' '                                                       
         BNH   *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
*                                                                               
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
DISREC24 DS    0H                                                               
         BCTR  R4,0                BACK UP                                      
         CLI   0(R4),C','                                                       
         BNE   DISREC25                                                         
         MVI   0(R4),C' '          AND BLANK LAST COMMA                         
*                                                                               
DISREC25 OC    CAMBOOKS,CAMBOOKS   BOOKS                                        
         BZ    DISREC28                                                         
         LA    R0,4                                                             
         LA    R3,CAMBOOKS                                                      
         LA    R4,CPNBKS                                                        
         LA    R9,CAMBKTPS         CAMBOOKS BOOKTYPES                           
         MVI   APWORK+2,1                                                       
*                                                                               
DISREC26 OC    0(2,R3),0(R3)                                                    
         BZ    DISREC27                                                         
         MVC   APWORK(2),0(R3)                                                  
         NI    APWORK+1,X'FF'-BTYBITSQ  REMOVE ANY SPECIAL TYPE                 
         GOTO1 VDATCON,APPARM,(3,APWORK),(6,(R4))                               
         TM    1(R3),BTYBITSQ      SPECIAL TYPE?                                
         BZ    DSREC26R                                                         
*                                                                               
****  NOTE:  R9 IS USED ONLY WHEN BTY2CHAR IS ON                                
         GOTO1 AGETBKTY,APPARM,(C'B',1(R3)),APFULL,0(R9)                        
         CLI   0(R1),X'FF'                                                      
         BE    DSREC26R            NOT A SPECIAL TYPE                           
         MVC   6(4,R4),=C'( ),'                                                 
         MVC   7(1,R4),APFULL                                                   
         TM    1(R3),BTY2CHAR      2 CHARACTER BOOKTYPE?                        
         BNO   DSREC26Q                                                         
         CLI   APFULL+1,C' '       IS THE 2 CHARACTER A BLANK OR LOWER?         
         BNH   DSREC26Q             - YUP                                       
         MVC   7(2,R4),APFULL                                                   
         MVC   9(2,R4),=C'),'                                                   
         LA    R4,11(R4)           2 CHARACTER                                  
         B     DSREC26T                                                         
*                                                                               
DSREC26Q LA    R4,10(R4)           NON-2 CHARACTER                              
         B     DSREC26T            SO WE NOW HAVE IE:  JL98O,NV98O              
*                                                                               
DSREC26R MVI   6(R4),C','                                                       
         LA    R4,7(R4)                                                         
DSREC26T LA    R3,2(R3)                                                         
         LA    R9,1(R9)            BUMP THE BOOK TYPE                           
         BCT   R0,DISREC26                                                      
DISREC27 BCTR  R4,0                                                             
         MVI   0(R4),C' '                                                       
*                                                                               
DISREC28 CLI   CAMADJ,0            PROGRAM ADJACENCY CODE                       
         BE    DISREC30                                                         
         MVC   CPNADJ(1),CAMADJ                                                 
         CLI   CAMADJ,C'A'                                                      
         BNL   DISREC30                                                         
         UNPK  APFULL(3),CAMADJ(2)                                              
         MVC   CPNADJ,APFULL                                                    
         CLI   CPNADJ+1,C'0'                                                    
         BNE   DISREC30                                                         
         MVI   CPNADJ+1,C' '                                                    
*                                                                               
DISREC30 MVC   CPNSCH,CAMSCHEM     SID SCHEME                                   
*                                                                               
         OC    CAMPERS,CAMPERS     SID PERIODS                                  
         BNZ   *+14                                                             
         XC    CPNSCH,CPNSCH                                                    
         B     DISREC35                                                         
         LA    R0,12                                                            
         LA    R3,CAMPERS                                                       
         LA    R4,CPNPER                                                        
*                                                                               
DISREC32 OC    0(4,R3),0(R3)                                                    
         BZ    DISREC34                                                         
         MVC   0(4,R4),0(R3)                                                    
         LA    R4,1(R4)                                                         
         LA    RE,3                                                             
         CLI   0(R4),C' '                                                       
         BE    *+12                                                             
         LA    R4,1(R4)                                                         
         BCT   RE,*-12                                                          
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
DISREC34 LA    R3,4(R3)                                                         
         BCT   R0,DISREC32                                                      
         BCTR  R4,0                                                             
         MVI   0(R4),C' '                                                       
*                                                                               
DISREC35 ICM   R4,15,LACEQEL       SPOT LENGTH EQUIVALENCES                     
         BZ    DISREC37                                                         
         LA    RF,5                                                             
         USING CEQEL,R4                                                         
         LA    R3,CPNSL1H                                                       
         ZIC   R8,CEQELLN                                                       
         AR    R8,R4                                                            
         SH    R8,=Y(CEQSLN-CEQEL)                                              
*                                                                               
DISREC36 LA    R1,L'FVIHDR(R3)                                                  
         EDIT  CEQSLN,(3,(R1)),ALIGN=LEFT                                       
         LA    R3,CPNSE1H-CPNSL1H(R3)                                           
         LA    R1,L'FVIHDR(R3)                                                  
         EDIT  CEQEQUIV,(4,(R1)),ALIGN=LEFT                                     
         LA    R4,L'CEQSLN+L'CEQEQUIV(R4)                                       
         CR    R4,R8                                                            
         BNL   DISREC37                                                         
         LA    R3,CPNSL2H-CPNSE1H(R3)                                           
         BCT   RF,DISREC36                                                      
*                                                                               
DISREC37 TM    CAMOPT,CAMOWKS      TEST NON-CONTIGUOUS FLIGHT WEEKS             
         BZ    DISREC44                                                         
         LA    R3,CAMWKS                                                        
         LA    R0,NMAXWKS                                                       
         LA    R4,CPNFW1                                                        
         LA    R8,L'CPNFW1-6(R4)                                                
*                                                                               
DISREC38 OC    0(4,R3),0(R3)                                                    
         BZ    DISREC40                                                         
         GOTO1 VDATCON,APPARM,(2,(R3)),(4,(R4))                                 
         MVI   5(R4),C','                                                       
         LA    R3,4(R3)                                                         
         LA    R4,6(R4)                                                         
         CR    R4,R8                                                            
         BNH   DISREC39                                                         
         BCTR  R4,0                                                             
         MVI   0(R4),C' '                                                       
         LA    R4,CPNFW2                                                        
         LA    R8,L'CPNFW2-6(R4)                                                
DISREC39 BCT   R0,DISREC38                                                      
*                                                                               
DISREC40 LA    R1,CPNFW2                                                        
         CR    R4,R1                                                            
         BE    DISREC42                                                         
         BCTR  R4,0                                                             
         CLI   0(R4),C','                                                       
         BNE   DISREC42                                                         
         MVI   0(R4),C' '                                                       
*                                                                               
DISREC42 BCTR  R3,0                FLIGHT END DATE                              
         BCTR  R3,0                                                             
         GOTO1 VDATCON,APPARM,(2,(R3)),(5,CPNFND)                               
*                                                                               
DISREC44 BRAS  RE,CHEKLIST                                                      
*                                                                               
DISRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS                                          *         
***********************************************************************         
         SPACE 1                                                                
VALSEL0  NTR1  ,                                                                
VALSEL   LA    R0,LSTL1H                                                        
         ST    R0,APPARM+0                                                      
         LA    R1,LSTL2H                                                        
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
*                                                                               
VALSELCH L     R1,ALSM                                                          
         USING LSMD,R1                                                          
         CLC   LSMUOPT,INOPTI      TEST CHANGE OF OPTIONS                       
         BE    *+10                                                             
         XC    LSMUKEY,LSMUKEY     WIPE OUT THE INITIAL KEY                     
         DROP  R1                                                               
*                                                                               
VALSEL0A LA    R2,APRECKEY                                                      
         XC    APRECKEY,APRECKEY                                                
         XC    LSTMDN,LSTMDN                                                    
         OI    LSTMDNH+6,FVOXMT                                                 
         GOTO1 AVALMED,LSTMEDH     MEDIA                                        
         BNE   VALSELX                                                          
         MVC   LSTMDN,MEDNM                                                     
         MVI   CAMKTYP,CAMKTYPQ                                                 
         MVI   CAMKSUB,CAMKSUBQ                                                 
         MVC   CAMKAGMD,BAGYMD                                                  
         OI    SELKIND,SELKMED                                                  
*                                                                               
         LA    R1,LSTBYRH          BUYER                                        
         ST    R1,FVADDR                                                        
         CLI   5(R1),0                                                          
         BNE   VALSEL1                                                          
         TM    4(R1),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         MVI   BBYRMASK,X'08'      START FROM MOST RECENT BUYER                 
         TM    SELKIND,SELKBYR     "LATER BUYERS GET LISTED FIRST"              
         BO    EMIF                                                             
         B     VALSEL2                                                          
*                                                                               
VALSEL1  GOTO1 AVALBYR,LSTBYRH                                                  
         BNE   VALSELX                                                          
         MVC   CAMKBYR,BBYR                                                     
         OI    SELKIND,SELKBYR     HAVE A BUYER TO FILTER LIST FOR              
*                                                                               
VALSEL2  OC    CAMKAGMD,BBYRMASK   IF MORE THAN 255 BUYERS                      
         OI    4(R1),X'20'         PREVIOUSLY VALIDATED BUYER FLD               
*                                                                               
         XC    SCFULL,SCFULL       VALIDATE START CAMPAIGN NUMBER               
         GOTO1 AFVAL,LSTNUMH                                                    
         BH    VALKEYX                                                          
         BE    *+16                                                             
         TM    SELKIND,SELKNUM                                                  
         BO    EMIF                                                             
         B     VALSEL5                                                          
         TM    SELKIND,SELKBYR                                                  
         BZ    EBNS                                                             
         TM    FVIIND,FVINUM                                                    
         BZ    VALSEL3                                                          
         OC    SCFULL,SCFULL       TEST CAMPAIGN NUMBER ZERO                    
         BZ    EIIF                                                             
         OC    SCFULL(2),SCFULL                                                 
         BNZ   EIIF                                                             
         B     VALSEL4                                                          
*                                                                               
VALSEL3  ZIC   R1,FVXLEN           TEST LATEST CAMPAIGN REQUESTED               
         EX    R1,CAMINP1                                                       
         BE    *+12                                                             
         EX    R1,CAMINP2                                                       
         BNE   EIIF                                                             
         MVC   SCFULL,EFFS                                                      
*                                                                               
VALSEL4  OC    CAMKCAM,SCFULL+2                                                 
         BZ    *+10                                                             
         XC    CAMKCAM,EFFS                                                     
         OI    SELKIND,SELKNUM                                                  
*                                                                               
VALSEL5  XC    LSTCNM,LSTCNM       CLIENT                                       
         OI    LSTCNMH+6,FVOXMT                                                 
         LA    R1,LSTCLTH                                                       
         ST    R1,FVADDR                                                        
         CLI   5(R1),0                                                          
         BNE   VALSEL5A                                                         
         TM    NWSFLAG,NWSFCLST    X'80' - USES CLIENT LIST?                    
         BO    EMIF                YES, NEED A CLIENT                           
         TM    SELKIND,SELKCLT                                                  
         BO    EMIF                                                             
         B     VALSEL6                                                          
*                                                                               
VALSEL5A TM    SELKIND,SELKMED                                                  
         BZ    EMNS                                                             
         GOTO1 AVALCLT                                                          
         BNE   VALSELX                                                          
         MVC   LSTCNM,CLTNM                                                     
         MVC   CAMKCLT,BCLT                                                     
         OI    SELKIND,SELKCLT                                                  
*                                                                               
VALSEL6  XC    LSTPNM,LSTPNM                                                    
         OI    LSTPNMH+6,FVOXMT                                                 
         LA    R1,LSTPRDH                                                       
         ST    R1,FVADDR                                                        
         CLI   5(R1),0                                                          
         BNE   *+16                                                             
         TM    SELKIND,SELKPRD                                                  
         BO    EMIF                                                             
         B     VALSEL8                                                          
         TM    SELKIND,SELKCLT                                                  
         BZ    ECNS                                                             
         GOTO1 AVALPRD                                                          
         BNE   VALSELX                                                          
         MVC   LSTPNM,PRDNM                                                     
         MVC   CAMKPRD,BPRD                                                     
         OI    SELKIND,SELKPRD                                                  
*                                                                               
VALSEL8  XC    LSTENM,LSTENM                                                    
         OI    LSTENMH+6,FVOXMT                                                 
         LA    R1,LSTESTH                                                       
         ST    R1,FVADDR                                                        
         CLI   5(R1),0                                                          
         BNE   *+16                                                             
         TM    SELKIND,SELKEST                                                  
         BO    EMIF                                                             
         B     VALSEL10                                                         
         TM    SELKIND,SELKPRD                                                  
         BZ    EPNS                                                             
         GOTO1 AVALEST                                                          
         BNE   VALSELX                                                          
         MVC   LSTENM,ESTNM                                                     
         MVC   CAMKEST,BEST                                                     
         OI    SELKIND,SELKEST                                                  
*                                                                               
VALSEL10 MVC   SELKEY,APRECKEY                                                  
         TM    SELKIND,SELKNUM                                                  
         BZ    VALSEL20                                                         
         XC    CAMKCLT,CAMKCLT                                                  
         MVI   CAMKPRD,0                                                        
         MVI   CAMKEST,0                                                        
*                                                                               
***          NEW DATE FILTER  (REQUIRES BUYER FILTER)                           
***                                     MHC  06/03/03                           
VALSEL20 OC    INFDATES,INFDATES   ANY DATE FILTER??                            
         BZ    VALSELX              - NOPE                                      
         CLC   INFDATES(3),INFDATES+3   COMPARE START AND END DATE              
         BH    EBADDAT              - START IS AFTER END, NO GOOD!!             
         TM    SELKIND,SELKBYR     DO WE HAVE A BUYER FILTER??                  
         BO    VALSEL22             - YUP WE GOT IT!                            
         NI    BBYRMASK,X'FF'-X'08'   TAKE OFF THE MASK                         
         B     ENOBYR               - NOPE, ERROR, NEED BUYER FILTER            
VALSEL22 OI    SELKIND,SELKDAT      - WE GOT A DATE FILTER!                     
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETSEL0  NTR1  ,                                                                
GETSEL   MVC   IOKEY,APRECKEY                                                   
         TM    APINDS,APILRERD     TEST READ THEN READ SEQUENTIAL               
         BZ    GETSEL2                                                          
         GOTO1 AIO,DIRRD                                                        
         BE    GETSEL4                                                          
         B     GETSEL9                                                          
*                                                                               
GETSEL2  TM    APINDS,APILNSEQ     TEST READ HIGH                               
         BO    GETSEL4                                                          
         LA    R1,DIRHI+IO1                                                     
         B     *+8                                                              
*                                                                               
GETSEL4  LA    R1,DIRSQ+IO1        GET FIRST/NEXT DIRECTORY RECORD              
         GOTO1 AIO                                                              
         BNE   GETSEL9                                                          
         LA    R2,IOKEY                                                         
         CLC   CAMKEY(CAMKBYR-CAMKEY),SELKEY                                    
         BNE   GETSEL9                                                          
         TM    SELKIND,SELKMED                                                  
         BZ    *+14                                                             
         CLC   CAMKAGMD,SELKEY+(CAMKAGMD-CAMKEY)                                
         BNE   GETSEL4                                                          
         TM    SELKIND,SELKBYR                                                  
         BZ    *+14                                                             
         CLC   CAMKBYR,SELKEY+(CAMKBYR-CAMKEY)                                  
         BNE   GETSEL9                                                          
         MVC   APRECKEY,IOKEY                                                   
         GOTO1 AIO,FILGET1                                                      
         BE    *+14                TEST OK OR DELETED                           
         TM    IOERR,IOERRS-IOEDEL                                              
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     R2,IOADDR                                                        
         TM    SELKIND,SELKCLT                                                  
         BZ    *+14                                                             
         CLC   CAMCLT,SELKEY+(CAMKCLT-CAMKEY)                                   
         BNE   GETSEL4                                                          
         TM    SELKIND,SELKPRD                                                  
         BZ    *+14                                                             
         CLC   CAMPRD,SELKEY+(CAMKPRD-CAMKEY)                                   
         BNE   GETSEL4                                                          
         TM    SELKIND,SELKEST                                                  
         BZ    *+14                                                             
         CLC   CAMEST,SELKEY+(CAMKEST-CAMKEY)                                   
         BNE   GETSEL4                                                          
*                                                                               
         TM    SELKIND,SELKDAT     WE GOT A DATE FILTER?                        
         BZ    GETSEL8              - NOPE                                      
*                                                                               
         USING LSMD,RE                                                          
         L     RE,ALSM             LOAD ADDRESS OF LSM                          
         CLC   LSMUOPT,INOPTS      TEST CHANGE OF OPTIONS                       
         BNE   GETSEL               - NOPE, START OVER                          
         DROP  RE                                                               
*                                                                               
         CLC   INFEND,CAMSTRT      SEE IF CAMPAIGN DATES INTERSECTS...          
         BL    GETSEL4                                                          
         CLC   CAMEND,INFSTART     ...FILTER DATES                              
         BL    GETSEL4                                                          
*                                                                               
GETSEL8  MVC   APRECDA,IODA                                                     
         B     GETSELX                                                          
*                                                                               
GETSEL9  TM    SELKIND,SELKBYR     HAVE A BUYER FILTER?                         
         BNZ   GETSEL9A            YES, THEN WE'RE DEFINITELY DONE              
         TM    BBYRMASK,X'08'      STILL LOOKING AT BUYER PAST 255?             
         BZ    GETSEL9A                                                         
         BAS   RE,VALSEL0          GO BACK AND LOOK AT 1ST 255 BUYERS           
         MVC   IOKEY,APRECKEY                                                   
         MVC   CAMKAGMD,BAGYMD                                                  
         NI    BBYRMASK,X'FF'-X'08'                                             
         MVI   BBYR,0                                                           
         MVC   SELKEY,IOKEY                                                     
         GOTO1 AIO,DIRHI+IO1                                                    
         MVC   APRECKEY,IOKEY                                                   
         NI    APINDS,X'FF'-(APILRERD+APILNSEQ)                                 
         B     GETSEL                                                           
*                                                                               
GETSEL9A MVI   APMODE,APMEOFS                                                   
         NI    LSTBYRH+4,X'FF'-X'20'   INVALIDATE THE BUYER FIELD               
         TM    SELKIND,SELKBYR                                                  
         BO    GETSELX                                                          
         MVI   BBYR,0                                                           
*                                                                               
GETSELX  CLI   APMODE,APMEOFS                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   GOTO1 ADISPSEL                                                         
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LAST FOR LIST SCREEN                                                *         
***********************************************************************         
LSTSCR   MVI   APMODE,APMPFKS      ENABLE PROGRAM FUNCTION KEYS                 
         B     EXIT                                                             
***********************************************************************         
* VALIDATE REQUEST SCREEN                                             *         
***********************************************************************         
VALREQ   GOTO1 AVREQ                                                            
         B     EXIT                                                             
***********************************************************************         
* PRINT THE REPORT                                                    *         
***********************************************************************         
PRTREP   GOTO1 =A(PRINTREP),RR=APRELO                                           
         B     EXIT                                                             
***********************************************************************         
* DELETE THE PASSIVE KEYS                                             *         
***********************************************************************         
DELRECA  GOTO1 =A(DELREC),RR=APRELO                                             
         B     EXIT                                                             
***********************************************************************         
* RESTORE THE PASSIVE KEYS                                            *         
***********************************************************************         
RESRECA  GOTO1 =A(RESREC),RR=APRELO                                             
         B     EXIT                                                             
         EJECT                                                                  
EMIF     MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXIT                                                             
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
EFNN     MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXIT                                                             
EISL     MVC   FVMSGNO,=AL2(FVISLN)                                             
         B     EXIT                                                             
EDNC     MVC   FVMSGNO,=AL2(FVDNEP)                                             
         B     EXIT                                                             
EEDM     MVC   FVMSGNO,=AL2(FVNOEDT)                                            
         B     EXIT                                                             
ESGE     MVC   FVMSGNO,=AL2(FVSTENDT)                                           
         B     EXIT                                                             
EPTL     MVC   FVMSGNO,=AL2(FVPTL)                                              
         B     EXIT                                                             
EPTL2    MVC   FVMSGNO,=AL2(FVPTL2)                                             
         B     EXIT                                                             
EBNS     MVC   FVMSGNO,=AL2(FVNOBYR)                                            
         B     EXIT                                                             
EMNS     MVC   FVMSGNO,=AL2(FVNOMED)                                            
         B     EXIT                                                             
ECNS     MVC   FVMSGNO,=AL2(FVNOCLT)                                            
         B     EXIT                                                             
EPNS     MVC   FVMSGNO,=AL2(FVNOPRD)                                            
         B     EXIT                                                             
EBKS     MVC   FVMSGNO,=AL2(FVNOBKS)                                            
         B     EXIT                                                             
ECAM     MVC   FVMSGNO,=AL2(FVICAM)                                             
         B     EXIT                                                             
ECCAM    MVC   FVMSGNO,=AL2(FVICCAM)                                            
         B     EXIT                                                             
ECAMD    MVC   FVMSGNO,=AL2(FVCAMDT)                                            
         B     EXIT                                                             
ECAMCOM  MVC   FVMSGNO,=AL2(FVCAMCOM)                                           
         B     EXIT                                                             
ECHAC    MVC   FVMSGNO,=AL2(FVCHACOM)                                           
         B     EXIT                                                             
ENOBYR   MVC   FVMSGNO,=AL2(1192)  NEED BYR FILTER IF USING DATE FILTER         
         LA    R1,CPNBYRH                                                       
         B     ECURSOR                                                          
EBADDAT  MVC   FVMSGNO,=AL2(1193)   DATE RANGE IS NO GOOD!!                     
         LA    R1,BWSOPTH                                                       
         B     ECURSOR                                                          
ESCH     MVC   FVMSGNO,=AL2(FVISCHM)                                            
         LA    R1,CPNSCHH                                                       
         B     ECURSOR                                                          
EPER     MVC   FVMSGNO,=AL2(FVIPERN)                                            
         B     EXIT                                                             
EPNV     MVC   FVMSGNO,=AL2(FVIPRD)                                             
         B     EXIT                                                             
EPURP    MVC   FVMSGNO,=AL2(1135)                                               
         B     EXIT                                                             
PURPMISS MVC   FVMSGNO,=AL2(1136)                                               
         B     EXIT                                                             
EIDAT    MVC   FVMSGNO,=AL2(FVIDAT)                                             
         B     EXIT                                                             
ECMPEST  MVC   FVMSGNO,=AL2(FVCMPEST)                                           
         B     EXIT                                                             
*                                                                               
EDAT     MVC   FVMSGNO,=AL2(FVCSTWK)                                            
         B     ERRDAT                                                           
ECENDDAT MVC   FVMSGNO,=AL2(FVCNDWK)                                            
         B     *+10                                                             
ECMPDAT  MVC   FVMSGNO,=AL2(FVCMPDAT)                                           
ERRDAT   LA    R1,CPNDATH                                                       
         B     ECURSOR                                                          
*                                                                               
EFLWK    MVC   FVMSGNO,=AL2(FVCFLWK)                                            
         LA    R1,CPNFW1H                                                       
         B     ECURSOR                                                          
EPRD     MVC   FVMSGNO,=AL2(FVIPRDPB)                                           
         LA    R1,CPNPRDH                                                       
         B     ECURSOR                                                          
EPOL     MVC   FVMSGNO,=AL2(FVIPRDPL)                                           
         LA    R1,CPNPRDH                                                       
         B     ECURSOR                                                          
ONESPTLN LA    RE,APPARM           DEFINE GETTEXT BLOCK                         
         USING GETTXTD,RE                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTLTXT,3                                                         
         LA    RF,APWORK                                                        
         STCM  RF,7,GTATXT                                                      
         MVC   GTMSGNO,=AL2(FVI1SPLN)                                           
         MVI   GTMTYP,GTMERR                                                    
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         B     EXIT                                                             
         DROP  RE                                                               
EUPDT    MVC   FVMSGNO,=AL2(FVIEFDT)                                            
         LA    R1,CPNUPDH                                                       
ECURSOR  ST    R1,FVADDR                                                        
*        B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
CAMINP1  CLC   FVIFLD(0),=C'LAST  '                                             
CAMINP2  CLC   FVIFLD(0),=C'LATEST'                                             
*                                                                               
REPDESCL DC    CL13'CAMPAIGN LIST'                                              
*                                                                               
ALL      DC    CL4'ALL,'                                                        
MAS      DC    CL4'MAS,'                                                        
SEP      DC    CL4'SEP,'                                                        
ARB      DC    CL4'ARB,'                                                        
NSI      DC    CL4'NSI,'                                                        
SVC      DC    CL4'SVC='                                                        
AUTOADJ  DC    CL10'AUTOADJ=N,'                                                 
*                                                                               
***TAB   DS    0XL1                                                             
***    ++INCLUDE SPSLNTAB                                                       
         DC    AL1(0)                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
EFFS     DC    X'FFFFFFFF'                                                      
SPACES   DC    CL80' '                                                          
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS UP CMPNWKS AND CMPDATSD                                     
***********************************************************************         
CAMNMWKS NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R5               XC    CMPDATSD,CMPDATSD                      
         AHI   RE,CMPDATSD-TWAD    XC    CMPDATSP,CMPDATSP                      
         LHI   RF,L'CMPDATSD+L'CMPDATSP                                         
         LR    R0,RE                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 VDATCON,APPARM,(3,CAMSTRT),(0,LCAMSTRT) CAMP START               
         TM    CAMOPT,CAMOWKS      TEST NON-CONTIGUOUS FLIGHT WEEKS             
         BO    CNWKS20             YES                                          
         TM    CAMOPT,CAMODLY      NO-TEST DAILY SCHEDULE                       
         BZ    CNWKS70             NO                                           
         MVC   APWORK(6),LCAMSTRT  YES-BUILD LIST OF DAYS                       
*                                                                               
         LR    RE,R5                                                            
         AHI   RE,CMPDATSD-TWAD                                                 
         MVC   0(6,RE),LCAMSTRT                                                 
*                                                                               
         LA    R4,LCAMSTRT                                                      
         BRAS  RE,GETMON                                                        
         MVC   CMPSTMON,APWORK+12  CAMPAIGN START MONDAY                        
         GOTO1 VDATCON,APPARM,CMPSTMON,(2,CMPSTMNP)                             
         GOTO1 (RF),(R1),(3,CAMEND),APWORK+12                                   
*                                                                               
         LR    R4,R5                                                            
         AHI   R4,CMPDATSP-TWAD                                                 
*                                                                               
CNWKS10  CLC   APWORK(6),APWORK+12                                              
         BH    CNWKS60                                                          
         GOTO1 VDATCON,APPARM,APWORK,(2,(R4))                                   
         MVC   2(2,R4),0(R4)                                                    
         MVC   CMPNDMNP,0(R4)      END DAY                                      
         GOTO1 VADDAY,(R1),APWORK,APWORK+6,1                                    
         MVC   APWORK(6),APWORK+6                                               
         LA    R4,4(R4)                                                         
         B     CNWKS10                                                          
*                                                                               
CNWKS20  MVC   CMPFLSTM,LFLSTMON   FLIGHT START MONDAY                          
         MVC   CMPFLND,LFLND       FLIGHT END                                   
*                                                                               
         TM    CAMOPT,CAMODLY                                                   
         BO    CNWKS30                                                          
         LR    RE,R5                                                            
         AHI   RE,CMPDATSP-TWAD                                                 
         MVC   0(L'CAMWKS,RE),CAMWKS      FLIGHT WEEKS                          
         B     CNWKS50                                                          
*                                                                               
CNWKS30  LA    R3,CAMWKS           DAILY SCHEDULE                               
*                                                                               
         LR    R4,R5                                                            
         AHI   R4,CMPDATSP-TWAD                                                 
*                                                                               
         LA    R9,2                                                             
*                                                                               
CNWKS40  OC    0(4,R3),0(R3)                                                    
         BZ    CNWKS50                                                          
         MVC   0(2,R4),0(R3)                                                    
         MVC   2(2,R4),0(R3)                                                    
         LA    R0,6                                                             
*                                                                               
CNWKS43  GOTO1 VDATCON,APPARM,(2,(R4)),(0,APWORK)                               
         GOTO1 VADDAY,(R1),APWORK,APWORK+6,1                                    
         GOTO1 VDATCON,(R1),(0,APWORK+6),(2,APHALF)                             
         CLC   APHALF,2(R3)                                                     
         BH    CNWKS46                                                          
         LA    R4,4(R4)                                                         
         MVC   0(2,R4),APHALF                                                   
         MVC   2(2,R4),APHALF                                                   
         BCT   R0,CNWKS43                                                       
*                                                                               
CNWKS46  LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R9,CNWKS40                                                       
*                                                                               
CNWKS50  LR    RE,R5                                                            
         AHI   RE,CMPDATSD-TWAD                                                 
         MVC   0(6,RE),LFLST   FLIGHT START                                     
*                                                                               
         GOTO1 VDATCON,APPARM,(0,LFLSTMON),(2,CMPSTMNP) FLIGHT STRT MON         
         MVC   CMPNDMNP,CMPSTMNP   END MONDAY PACKED                            
         LA    R4,LCAMSTRT                                                      
         BRAS  RE,GETMON                                                        
         MVC   CMPSTMON,APWORK+12    CAMPAIGN START MONDAY                      
*                                                                               
CNWKS60  LR    R1,R5                                                            
         AHI   R1,CMPDATSP-TWAD                                                 
*                                                                               
         SR    RE,RE                                                            
         OC    0(4,R1),0(R1)                                                    
         BZ    *+16                                                             
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         B     *-18                                                             
         MVI   0(R1),X'FF'                                                      
         STC   RE,CMPNWKS          TOTAL N'WEEKS (OR DAYS) FOR                  
         B     CNWKSX              WEEKS ARE READY FOR SCHEDULING               
*                                                                               
CNWKS70  GOTO1 VDATCON,APPARM,(3,CAMEND),(0,LCAMEND)                            
         MVC   APPARM+16(4),VGTBROAD    BUILD LIST OF PACKED DATES              
         MVC   APPARM+20(4),VADDAY                                              
         MVC   APPARM+24(4),VGETDAY                                             
         MVC   APPARM+28(4),VDATCON                                             
         XC    APELEM,APELEM                                                    
         CLI   ESTOWSDY,0                                                       
         BE    *+10                                                             
         MVC   APELEM+8(1),ESTOWSDY                                             
*                                                                               
         LR    RE,R5                                                            
         AHI   RE,CMPDATSP-TWAD                                                 
         ST    RE,APPARM+4                                                      
         MVI   APPARM+4,5                                                       
*                                                                               
         GOTO1 VMOBILE,APPARM,(53,LCAMSTRT),,APPARM+16,APELEM                   
         MVC   CMPNWKS,APPARM      NUMBER OF WEEKS                              
*                                                                               
         LR    RE,R5                                                            
         AHI   RE,CMPDATSP-TWAD                                                 
         ST    RE,APPARM                                                        
         MVI   APPARM,2                                                         
*                                                                               
         GOTO1 VDATCON,APPARM,,(0,CMPSTMON) START MONDAY                        
*                                                                               
         LR    RE,R5                                                            
         AHI   RE,CMPDATSP-TWAD                                                 
         MVC   CMPSTMNP,0(RE)      START MONDAY PACKED                          
         MVC   CMPNDMNP,0(RE)      END MONDAY PACKED                            
*                                                                               
         LR    RE,R5                                                            
         AHI   RE,CMPDATSD-TWAD                                                 
         MVC   0(6,RE),LCAMSTRT    START DATE                                   
*                                                                               
CNWKSX   J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RESTORE RECORD VALUES WHEN CAMPAIGN/CHANGE IS SELECTED   *         
* FROM A WORK RECORD LIST/SELECT SCREEN                               *         
***********************************************************************         
         SPACE 1                                                                
CHEKLIST NTR1  BASE=*,LABEL=*                                                   
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT IS ACTIVE                   
         BZ    CHKLSX                                                           
         L     R1,ALSM                                                          
         USING LSMD,R1                                                          
         CLI   LSMLREC,RECWRK      AND LISTING WORK RECORDS                     
         BNE   CHKLSX                                                           
         LH    RE,LSMRDSP          YES-FIX RECORD VALUES SO THAT THEY           
         LA    RE,0(R1,RE)             DON'T CHANGE                             
         USING LSMRTAB,RE                                                       
         MVC   APRECKEY,LSMRKEY                                                 
         MVC   APRECDA,LSMRDA                                                   
         MVC   APRECID,LSMRDA                                                   
         MVC   APRECNUM,LSMRNUM                                                 
         DROP  R1,RE                                                            
*                                                                               
CHKLSX   J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET MONDAY                                                         
* INPUT  : R4 = A(DATE)                                                         
* OUTPUT : APWORK+12(6) = MONDAY DATE OF WEEK OF INPUT DATE                     
***********************************************************************         
GETMON   NTR1  BASE=*,LABEL=*                                                   
         MVC   APWORK+12(6),0(R4)                                               
         GOTO1 VGETDAY,APPARM,(R4),APFULL                                       
         CLC   APFULL(3),=C'   '                                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,APPARM                                                        
         SR    RF,RF                                                            
         ICM   RF,1,ESTOWSDY                                                    
         BZ    GETMON2                                                          
         SR    RF,RE                                                            
         BZ    GETMONX                                                          
         BM    *+8                                                              
         SHI   RF,7                                                             
         LR    RE,RF                                                            
         B     GETMON4                                                          
*                                                                               
GETMON2  BCTR  RE,0                                                             
         LNR   RE,RE                                                            
         BZ    EXIT                                                             
*                                                                               
GETMON4  ST    RE,APPARM+8                                                      
         GOTO1 VADDAY,APPARM,(R4),APWORK+12                                     
*                                                                               
GETMONX  B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTENTION ROUTINES                                                            
***********************************************************************         
EXTRA    NMOD1 0,**BW3X**,RA                                                    
         L     RC,APALOCAL                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     VREQ                                                             
         B     VALPGR                                                           
         B     DISPGR                                                           
         B     VALSLEQU                                                         
         B     DISPKEY                                                          
         B     DISPSEL                                                          
         B     CHKTRN                                                           
         B     CHKDAR                                                           
*                                                                               
XIT      CLC   FVMSGNO,=AL2(FVFOK)                                              
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE REQUEST SCREEN                                             *         
***********************************************************************         
         SPACE 1                                                                
VREQ     L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
         MVI   REPCLASS,C'B'       CLASS B REPORT                               
         LA    R2,APRECKEY                                                      
         XC    APRECKEY,APRECKEY                                                
         MVI   CAMKTYP,CAMKTYPQ                                                 
         MVI   CAMKSUB,CAMKSUBQ                                                 
         CLI   REPREQH+5,0         ANY REQUESTOR INPUT?                         
         BNE   VALREQ1                                                          
         MVC   REPREQ,QBYR         NO, SET IT TO THE BUYER CODE                 
         CLC   REPREQ,SPACES                                                    
         BL    VALREQ1                                                          
         MVI   REPREQH+5,L'QBYR                                                 
*                                                                               
VALREQ1  MVI   FVMINL,1            SET MINIMUM INPUT LENGTH                     
         GOTO1 AFVAL,REPREQH                                                    
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
         CLI   ASONOFF,ASON        TEST ONLINE                                  
         BNE   VALREQ2                                                          
*                                                                               
         GOTO1 AVALWHEN,REPWENH    VALIDATE WHEN                                
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALDEST,REPDIDH    VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALOTYP,REPOUTH    VALIDATE OUTPUT TYPE                         
         BNE   VALREQX                                                          
*                                                                               
VALREQ2  XC    REPMDN,REPMDN                                                    
         OI    REPMDNH+6,FVOXMT                                                 
         GOTO1 AVALMED,REPMEDH     VALIDATE MEDIA                               
         BNE   VALREQX                                                          
         MVC   REPMDN,MEDNM                                                     
         MVC   CAMKAGMD,BAGYMD                                                  
         MVI   SELKIND,SELKMED                                                  
*                                                                               
         XC    REPBYN,REPBYN                                                    
         OI    REPBYNH+6,FVOXMT                                                 
         CLI   REPBYRH+5,0         TEST BUYER FILTER INPUT                      
         BE    VALREQ3                                                          
         GOTO1 AVALBYR,REPBYRH     VALIDATE BUYER                               
         BNE   VALREQX                                                          
         MVC   REPBYN,BYRNM                                                     
         MVC   CAMKBYR,BBYR                                                     
         OI    SELKIND,SELKBYR                                                  
         OC    CAMKAGMD,BBYRMASK                                                
*                                                                               
VALREQ3  XC    REPCNM,REPCNM                                                    
         OI    REPCNMH+6,FVOXMT                                                 
         CLI   REPCLTH+5,0         TEST CLIENT FILTER INPUT                     
         BNE   VALREQ3A                                                         
*****                                                                           
         TM    NWSFLAG,NWSFCLST    X'80' - USES CLIENT LIST?                    
         BNO   VALREQ4                                                          
         LA    R1,REPCLTH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(FVFNONE)   NEED CAMPAIGN IF USES CLT LIST           
         B     VALREQX                                                          
*                                                                               
VALREQ3A GOTO1 AVALCLT,REPCLTH     VALIDATE CLIENT                              
         BNE   VALREQX                                                          
         MVC   REPCNM,CLTNM                                                     
         MVC   CAMKCLT,BCLT                                                     
         OI    SELKIND,SELKCLT                                                  
*                                                                               
VALREQ4  XC    REPPNM,REPPNM                                                    
         OI    REPPNMH+6,FVOXMT                                                 
         CLI   REPPRDH+5,0         TEST PRODUCT FILTER INPUT                    
         BE    VALREQ5                                                          
         GOTO1 AVALPRD,REPPRDH     VALIDATE PRODUCT                             
         BNE   VALREQX                                                          
         TM    SELKIND,SELKCLT                                                  
         BZ    VALREQ9                                                          
         MVC   REPPNM,PRDNM                                                     
         MVC   CAMKPRD,BPRD                                                     
         OI    SELKIND,SELKPRD                                                  
*                                                                               
VALREQ5  XC    REPENM,REPENM                                                    
         OI    REPENMH+6,FVOXMT                                                 
         CLI   REPESTH+5,0         TEST ESTIMATE FILTER INPUT                   
         BE    VALREQ6                                                          
         GOTO1 AVALEST,REPESTH     VALIDATE ESTIMATE                            
         BNE   VALREQX                                                          
         TM    SELKIND,SELKPRD                                                  
         BZ    VALREQ9                                                          
         MVC   REPENM,ESTNM                                                     
         MVC   CAMKEST,BEST                                                     
         OI    SELKIND,SELKEST                                                  
*                                                                               
VALREQ6  TM    SELKIND,SELKBYR+SELKCLT+SELKPRD+SELKEST   ANY FILTERS?           
         BNZ   VALREQ8              - YUP, WE GOT SOME                          
         CLC   REPWEN(2),=C'OV'    DO WE HAVE AN OVERNIGHT JOB?                 
         BNE   VALREQ10             - NO FILTER, ERROR!                         
*                                                                               
VALREQ8  MVC   SELKEY,APRECKEY     SET SELECT KEY                               
         MVC   REPDESC,REPDESCL                                                 
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0          SET NO FOOTLINES REQUIRED                    
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS          SET A(SPEC POOL)                             
         LA    R0,REPHOOK                                                       
         ST    R0,REPAUSR          SET A(REPORT HOOK)                           
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALREQX                                                          
*                                                                               
VALREQ9  MVC   FVMSGNO,=AL2(FVFNOTV)   INVALID INPUT                            
         B     VALREQX                                                          
*                                                                               
VALREQ10 MVC   FVMSGNO,=AL2(1216)  NEED AT LEAST 1 FILTER FOR SOON/NOW          
*                                                                               
VALREQX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRODUCT GROUP                                              *         
***********************************************************************         
         SPACE 1                                                                
VALPGR   XC    IOKEY,IOKEY         READ PRODUCT GROUP DEFINITION REC            
         LA    R3,IOKEY                                                         
         USING PRGRECD,R3                                                       
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD,BAGYMD                                                  
         MVC   PRGKCLT,BCLT                                                     
         MVC   PRGKID,FVIFLD                                                    
         GOTO1 AIO,DIRHI+IO2                                                    
         BNE   VALPGR9                                                          
         CLC   PRGKEY(PRGKGRP+2-PRGKEY),IOKEYSAV                                
         BNE   VALPGR9                                                          
         GOTO1 AIO,FILGET2                                                      
         BNE   VALPGR9                                                          
         L     R3,AIOAREA2                                                      
         SR    R0,R0                                                            
         LA    R4,PRGEL            FIND PRDGRP BREAK DESCRIPTION                
*                                                                               
VALPGR2  CLI   0(R4),0                                                          
         BE    VALPGR9                                                          
         CLI   0(R4),1                                                          
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALPGR2                                                          
         USING PRGEL01,R4                                                       
         ZIC   RF,FVILEN                                                        
         BCTR  RF,0                RF=N'PGRP DIGITS ENTERED                     
         STC   RF,CAMPGRPN                                                      
         ZIC   RE,PRGBK1LN         L'BREAK1                                     
         ZIC   R1,PRGBK2LN                                                      
         CR    RF,RE               COMPARE TO N'DIGITS ENTERED                  
         BNE   VALPGR4                                                          
         LTR   R1,R1               EQUAL-TEST FOR 2ND BREAK LEVEL               
         BZ    VALPGR6                                                          
         OI    CAMPGRPN,X'80'      YES-X'80' BIT INDICATES THIS                 
         B     VALPGR6                                                          
*                                                                               
VALPGR4  AR    RE,R1               PLUS L'BREAK2                                
         CR    RE,RF               COMPARE TO N'DIGITS ENTERED                  
         BNE   VALPGR9                                                          
*                                                                               
VALPGR6  MVC   CAMPGRP(1),FVIFLD   PRODUCT GROUP ID                             
         BCTR  RE,0                TEST REST FOR NUMERICS                       
         XC    APFULL,APFULL                                                    
         EX    RE,*+4                                                           
         MVZ   APFULL(0),FVIFLD+1                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   APFULL(0),=C'000'                                                
         BNE   VALPGR9                                                          
         OC    FVIFLD+1(3),=C'000' PRODUCT GROUP DIGITS                         
         PACK  APDUB,FVIFLD+1(3)                                                
         MVC   CAMPGRP+1(2),APDUB+6                                             
         NI    CAMPGRP+2,X'F0'                                                  
         BAS   RE,GETPGR           READ PRODUCT GROUP RECORD                    
         B     VALPGRX                                                          
*                                                                               
VALPGR9  MVC   FVMSGNO,=AL2(FVIGRP)   INVALID GROUP                             
*                                                                               
VALPGRX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY PRODUCT GROUP AND NAME                                      *         
***********************************************************************         
         SPACE 1                                                                
DISPGR   MVC   CPNPGR(1),CAMPGRP   PRODUCT GROUP ID                             
         MVC   APDUB+6(2),CAMPGRP+1 DISPLAY THE DIGITS                          
         OI    APDUB+7,X'0F'                                                    
         UNPK  APFULL(3),APDUB                                                  
         MVC   APBYTE,CAMPGRPN                                                  
         NI    APBYTE,X'7F'                                                     
         SR    RE,RE                                                            
         ICM   RE,1,APBYTE                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   CPNPGR+1(0),APFULL                                               
         OI    CPNPGRH+6,FVOXMT                                                 
         BAS   RE,GETPGR           DISPLAY PRODUCT GROUP NAME                   
*                                                                               
DISPGRX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET PRODUCT GROUP RECORD AND DISPLAY THE PRODUCT GROUP NAME         *         
* INPUT  : CAMPGRP=CAMPAIGN'S PRODUCT GROUP                           *         
* OUTPUT : CC EQ - OK                                                 *         
*          CC NE - ERROR                                              *         
***********************************************************************         
         SPACE 1                                                                
GETPGR   NTR1  ,                                                                
         XC    IOKEY,IOKEY                                                      
         LA    R3,IOKEY            READ PRODUCT GROUP RECORD                    
         USING PRGRECD,R3                                                       
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD,BAGYMD                                                  
         MVC   PRGKCLT,BCLT                                                     
         MVC   PRGKID,CAMPGRP                                                   
         MVC   PRGKGRP,CAMPGRP+1                                                
         GOTO1 AIO,DIRHI+IO2                                                    
         BNE   GETPGR9                                                          
         MVC   APBYTE,CAMPGRPN                                                  
         NI    APBYTE,X'7F'                                                     
         LA    RE,PRGKGRP-PRGKEY-1 SET RE FOR EXECUTED COMPARE                  
         CLI   CAMPGRPN,X'81'                                                   
         BE    GETPGR2                                                          
         LA    RE,1(RE)                                                         
         CLI   APBYTE,3                                                         
         BNE   GETPGR2                                                          
         LA    RE,1(RE)                                                         
*                                                                               
GETPGR2  EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   PRGKEY(0),IOKEYSAV                                               
         BNE   GETPGR9                                                          
         CLI   CAMPGRPN,X'81'                                                   
         BNE   GETPGR4                                                          
         MVC   APBYTE,PRGKGRP                                                   
         NI    APBYTE,X'F0'                                                     
         CLC   APBYTE,CAMPGRP+1                                                 
         BNE   GETPGR9                                                          
*                                                                               
GETPGR4  DS    0H                                                               
         GOTO1 AIO,FILGET2                                                      
         BNE   GETPGR9                                                          
         L     R3,AIOAREA2                                                      
         SR    R0,R0               FIND BREAK NAMES ELEMENT                     
         LA    R4,PRGEL                                                         
*                                                                               
GETPGR6  CLI   0(R4),0                                                          
         BE    GETPGR9                                                          
         CLI   0(R4),X'10'                                                      
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GETPGR6                                                          
         USING PRGEL10,R4                                                       
         LA    R1,PRGNAM1          EXTRACT BREAK 1 NAME                         
         TM    CAMPGRPN,X'80'      TEST FIRST LEVEL OF TWO                      
         BO    *+18                YES                                          
         CLC   PRGNAM2,BLANKS      NO-TEST FOR 2ND LEVEL                        
         BNH   *+8                 NO                                           
         LA    R1,PRGNAM2          YES-USE BREAK 2 NAME                         
         MVC   CPNPGN,0(R1)                                                     
         OI    CPNPGNH+6,FVOXMT                                                 
         B     GETPGRX                                                          
*                                                                               
GETPGR9  MVC   FVMSGNO,=AL2(FVIGRP)   INVALID GROUP                             
*                                                                               
GETPGRX  CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE SPOT LENGTH EQUIVALENCE FIELDS                             *         
***********************************************************************         
         SPACE 1                                                                
VALSLEQU DS    0H                                                               
         MVI   APBYTE,0                                                         
         XC    APELEM,APELEM                                                    
         LA    R4,APELEM                                                        
         USING CEQEL,R4                                                         
         MVI   CEQELCD,CEQELCDQ                                                 
         LA    R0,5                                                             
         LA    R3,CPNSL1H                                                       
*                                                                               
VALE2    LR    R1,R3                                                            
         GOTO1 AFVAL               VALIDATE SPOT LENGTH                         
         BH    VALEX                                                            
         BL    VALE6                                                            
         TM    FVIIND,FVINUM                                                    
         BZ    VALEISL                                                          
         OC    SCFULL(3),SCFULL                                                 
         BNZ   VALEISL                                                          
         OC    SCFULL,SCFULL                                                    
         BZ    VALEISL                                                          
         LA    RE,LENTAB                                                        
*                                                                               
VALE4    CLI   0(RE),0                                                          
         BE    VALEISL                                                          
         CLC   0(1,RE),SCFULL+3                                                 
         BE    *+12                                                             
         LA    RE,L'LENTAB(RE)                                                  
         B     VALE4                                                            
         MVC   CEQSLN,0(RE)                                                     
         LA    R3,CPNSE1H-CPNSL1H(R3)                                           
         LR    R1,R3               VALIDATE EQUIVALENCE                         
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL                                                            
         BNE   VALEX                                                            
         TM    FVIIND,FVINUM                                                    
         BZ    VALEFNN                                                          
         OC    SCFULL,SCFULL                                                    
         BZ    VALEIIF                                                          
         MVC   CEQEQUIV,SCFULL+2                                                
         CLC   CEQEQUIV,=H'1000'                                                
         BNE   *+8                                                              
         MVI   APBYTE,1                                                         
         LA    R3,CPNSL2H-CPNSE1H(R3)                                           
         LA    R4,L'CEQSLN+L'CEQEQUIV(R4)                                       
         BCT   R0,VALE2                                                         
*                                                                               
VALE6    CLI   APBYTE,0            TEST BASE SPOT LENGTH DEFINED                
         BE    VALE99                                                           
         LA    R1,APELEM           ELEMENT LENGTH                               
         LA    R4,CEQSLN-CEQEL(R4)                                              
         SR    R4,R1                                                            
         STC   R4,APELEM+1                                                      
         GOTO1 AADDELS,CAMRECD     ADD THE ELEMENT                              
         BE    VALEX                                                            
         DC    H'0'                                                             
*                                                                               
VALEISL  MVC   FVMSGNO,=AL2(FVISLN)                                             
         B     VALEX                                                            
*                                                                               
VALEFNN  MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALEX                                                            
*                                                                               
VALEIIF  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALEX                                                            
*                                                                               
VALE99   MVC   FVMSGNO,=AL2(FVNOBSL)     NO BASE SPOT LENGTH DEFINED            
         LA    R1,CPNSE1H                                                       
         ST    R1,FVADDR                                                        
*                                                                               
VALEX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAYS THE RECORD KEY                                                       
***********************************************************************         
DISPKEY  LA    R2,APRECKEY                                                      
         GOTO1 AGETMED,CAMKAGMD                                                 
         BNE   *+10                                                             
         MVC   CPNMED,QMED                                                      
         LA    R1,CAMKBYR                                                       
         ICM   R1,8,=C'B'                                                       
         GOTO1 AGETBYR                                                          
         MVC   CPNBYR,QBYR                                                      
         MVC   CPNNUM,QCAM                                                      
         CLI   CAMKTYP,CAMKTYPQ                                                 
         BNE   DSPKYX                                                           
         CLI   CAMKSUB,CAMKSUBQ                                                 
         BNE   DSPKYX                                                           
         MVC   LCAMP,CAMKCAM                                                    
         XC    LCAMP,EFFS                                                       
         SR    R1,R1                                                            
         ICM   R1,3,LCAMP                                                       
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  CPNNUM,APDUB                                                     
DSPKYX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAYS THE LIST/SELECT RECORD                                               
***********************************************************************         
DISPSEL  L     R2,AIOAREA1         R2=A(RECORD)                                 
         L     R3,APPARM                                                        
         ZIC   R1,0(R3)                                                         
         LA    R3,L'FVIHDR(R1,R3)  R3=A(OUTPUT DISPLAY LINE)                    
         USING LISTD,R3                                                         
         TM    SELKIND,SELKBYR     ANY BUYER FILTER?                            
         BO    DISPSEL2            YES                                          
*                                                                               
         CLC   CAMKBYR,BBYR        CHANGE OF BUYER?                             
         BNE   DISPSEL1            YES                                          
         MVC   APBYTE,BAGYMD                                                    
         OC    APBYTE,BBYRMASK                                                  
         CLC   CAMKAGMD,APBYTE     ALSO A CHANGE IF MEDIA IS DIFFERENT          
         BE    DISPSEL2                                                         
*                                                                               
DISPSEL1 MVI   BBYRMASK,0                                                       
         TM    CAMKAGMD,X'08'      THIS BUYER # PAST THE 1ST 255 BYRS?          
         BZ    *+8                                                              
         OI    BBYRMASK,X'08'                                                   
*                                                                               
         LA    R1,CAMKBYR          YES-GET BUYER                                
         ICM   R1,8,=C'B'                                                       
         GOTO1 AGETBYR                                                          
*                                                                               
DISPSEL2 MVC   LISTBYR,QBYR                                                     
         MVC   LCAMP,CAMKCAM       CAMPAIGN                                     
         XC    LCAMP,EFFS                                                       
         SR    R1,R1                                                            
         ICM   R1,3,LCAMP                                                       
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  LISTCAM,APDUB                                                    
         MVC   LISTCNM,CAMNAME                                                  
         GOTO1 AGETCLT,CAMCLT      CLIENT                                       
         BNE   *+10                                                             
         MVC   LISTCLT,QCLT                                                     
         GOTO1 AGETPRD,CAMPRD      PRODUCT                                      
         BNE   *+10                                                             
         MVC   LISTPRD,QPRD                                                     
         GOTO1 AGETEST,CAMEST      ESTIMATE                                     
         BNE   *+10                                                             
         MVC   LISTEST,QEST                                                     
         GOTO1 VDATCON,APPARM,(3,CAMSTRT),(5,LISTDAT)                           
         MVI   LISTDAT+8,C'-'                                                   
         GOTO1 (RF),(R1),(3,CAMEND),(5,LISTDAT+9)                               
*                                                                               
         XC    LISTSLN,LISTSLN                                                  
         SR    R0,R0                                                            
         ICM   R0,1,CAMSLN                                                      
         BZ    DISPSELX                                                         
         EDIT  (R0),(3,LISTSLN),ALIGN=LEFT                                      
*                                                                               
         CLI   CAMPPRD1,0                                                       
         BE    DISPSELX                                                         
*                                                                               
         LA    R8,LISTSLN                                                       
         LA    RF,3                                                             
*                                                                               
DISPSEL4 CLI    0(R8),X'40'                                                     
         BE    DISPSEL6                                                         
         CLI   0(R8),X'00'                                                      
         BE    DISPSEL6                                                         
         LA    R8,1(R8)                                                         
         BCT   RF,DISPSEL4                                                      
*                                                                               
DISPSEL6 MVI   0(R8),C'/'                                                       
         LA    R8,1(R8)                                                         
*                                                                               
         GOTO1 AGETPRD,CAMPPRD1                                                 
         BNE   *+10                                                             
         MVC   0(3,R8),QPRD                                                     
*                                                                               
         LA    R8,LISTSLN                                                       
         LA    RF,8                                                             
*                                                                               
DISPSEL8 CLI   0(R8),X'40'                                                      
         BE    DISPSEL9                                                         
         CLI   0(R8),X'00'                                                      
         BE    DISPSEL9                                                         
         LA    R8,1(R8)                                                         
         BCT   RF,DISPSEL8                                                      
*                                                                               
DISPSEL9 MVI   0(R8),C'-'                                                       
         LA    R8,1(R8)                                                         
         GOTO1 AGETPRD,CAMPPRD2                                                 
         BNE   *+10                                                             
         MVC   0(3,R8),QPRD                                                     
*                                                                               
DISPSELX MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LOOK FOR TRANSFER ELEMENTS                                          *         
***********************************************************************         
CHKTRN   XC    IOKEY,IOKEY                                                      
         XC    DARKEY,DARKEY                                                    
         LA    R2,IOKEY                                                         
         USING BWHRECD,R2                                                       
         XC    BWHKEY,BWHKEY       READ WORKSHEET HEADER RECORD                 
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,BAGYMD                                                  
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         MVC   BWHKCAM,WCAMP                                                    
*                                                                               
         GOTO1 AIO,DIRHI+IO2                                                    
*                                                                               
CHKTRN2  CLC   BWHKEY(BWHKMKT-BWHKEY),IOKEYSAV                                  
         BNE   CHKTRNX             NO MORE TO CHECK                             
         MVC   HEADKEY,IOKEY       SAVE KEY FOR SEQUENTIAL READ                 
*                                                                               
         LA    R3,DETKEY                                                        
         USING BWDRECD,R3                                                       
         XC    BWDKEY,BWDKEY       READ WORKSHEET DETAIL RECORD                 
         MVI   BWDKTYP,BWDKTYPQ                                                 
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BAGYMD                                                  
         OC    BWDKAGMD,BBYRMASK                                                
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,BWHKSEQ   <==  FROM THE HEADER KEY                       
         MVI   BWDKELCD,BWDELCDQ   START AT FIRST ELEMENT                       
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'DETKEY),DETKEY                                           
         LA    R1,MINHI2                                                        
         B     CHKTRN4+4                                                        
*                                                                               
CHKTRN4  LA    R1,MINSEQ2                                                       
         GOTO1 AMIN                                                             
         BNE   CHKTRN10                                                         
         CLC   IOKEY(BWDKELST-BWDKEY),DETKEY                                    
         BNE   CHKTRN10                                                         
*                                                                               
         L     R2,AIOAREA2                                                      
         LA    R4,BWHFSTEL                                                      
         SR    R0,R0                                                            
*                                                                               
CHKTRN6  CLI   0(R4),0             END OF CLUSTER                               
         BE    CHKTRN4             YES, GET NEXT DETAIL RECORD                  
         CLI   0(R4),BWDELCDQ      NO, DETAIL DESC ELEMENT (X'01')?             
         BNE   *+14                                                             
         MVC   QSTA,BWDSTA-BWDEL(R4)   YES, SAVE THE STATION IT WAS FOR         
         B     CHKTRN6A                                                         
*                                                                               
         CLI   0(R4),BTRELCDQ      NO, TRANSFER ELEMENT?                        
         BE    CHKTRN8             YES, RETURN WITH ERROR                       
CHKTRN6A IC    R0,1(R4)            NO, KEEP LOOKING                             
         AR    R4,R0                                                            
         B     CHKTRN6                                                          
*                                                                               
CHKTRN8  CLC   CMPESTN,BEST        ESTIMATE CHANGE INITIATED THIS?              
         BE    CHKTRN9                NO, PRD CHANGE INITIATED THIS             
         MVC   FVMSGNO,=AL2(FVTRANS)  YES, THEN ERROR OUT                       
         LA    R1,CPNESTH                                                       
         ST    R1,FVADDR           POINT TO ESTIMATE FIELD                      
         B     CHKTRNX                                                          
*******                                                                         
CHKTRN9  MVC   DETKEY,IOKEY                                                     
         CLI   CMPPRDN,X'FF'          POL PRODUCT?                              
         BNE   CHKTRN9A                                                         
         MVC   FVMSGNO,=AL2(FVTRANS)  YES, THEN ERROR OUT                       
         LA    R1,CPNPRDH                                                       
         ST    R1,FVADDR              POINT TO PRODUCT FIELD                    
         B     CHKTRNX                                                          
*                                                                               
CHKTRN9A GOTO1 VMSPACK,APPARM,=C'0000',QSTA,BMKT   BSTA ALSO                    
         GOTO1 ACHKDAR             USING THE PREVIOUS PRD  CMPPRDN              
         MVC   IOKEY,DETKEY                                                     
         BE    CHKTRN6A                                                         
         B     CHKTRNX                                                          
*******                                                                         
CHKTRN10 MVC   IOKEY,HEADKEY       RESTORE HEADER KEY                           
         GOTO1 AIO,DIRHI+IO2       RE-READ HEADER                               
         GOTO1 AIO,DIRSQ+IO2       READ SEQUENTIAL                              
         LA    R2,IOKEY            READDRESS R2                                 
         B     CHKTRN2                                                          
*                                                                               
CHKTRNX  B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* LOOK FOR DARE RECORD                                                *         
***********************************************************************         
CHKDAR   LA    R2,IOKEY            BUILD DARE PASSIVE POINTER BY CLT            
         USING DOKEY,R2                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   DCKTYPE,DCKTYPQ                                                  
         MVI   DCKSUBTY,DCKSTYPQ                                                
         MVC   DCKAGMD,BAGYMD                                                   
         MVC   DCKCLT,BCLT                                                      
         MVC   DCKPRD,CMPPRDN                                                   
         MVC   DCKEST,BEST                                                      
         MVC   DCKSTA,BSTA                                                      
         CLI   CMPPRD1,0           ANY PIGGYBACK PRODUCT?                       
         BE    *+10                                                             
         MVC   DCKPRD2,CMPPRD2     YES                                          
*                                                                               
         CLC   IOKEY(DCKFLTNM-DOKEY),DARKEY  BEEN THRU THIS DARE KEY?           
         BE    CHKDARX                       YES, SKIP THIS CHECK               
         MVC   DARKEY,IOKEY                  NO, GO THRU THEM ALL               
*                                                                               
         GOTO1 AIO,DIRHI+IO3                                                    
         B     CHKDAR05                                                         
CHKDAR00 GOTO1 AIO,DIRSQ+IO3                                                    
*                                                                               
CHKDAR05 CLC   IOKEY(DCKFLTNM-DOKEY),IOKEYSAV                                   
         BNE   CHKDARX                                                          
*                                                                               
         GOTO1 AIO,FILGET+IO3                                                   
         L     R6,AIOAREA3                                                      
         LA    R6,DORFRST-DOKEY(R6)                                             
         XR    R0,R0                                                            
CHKDAR10 CLI   0(R6),0                                                          
         BE    CHKDAR00            END OF THIS REC, CK FOR FLIGHTED             
         CLI   0(R6),DOXMTELQ      X'11' - TRANSMISSION ELEMENT?                
         BE    CHKDAR20                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CHKDAR10                                                         
*                                                                               
CHKDAR20 MVC   FVMSGNO,=AL2(796)   CAN'T CHANGE - BUYS HAVE BEEN DARE'D         
         LA    R1,CPNPRDH                                                       
         ST    R1,FVADDR           POINT TO ESTIMATE FIELD                      
*                                                                               
CHKDARX  B     XIT                                                              
         EJECT                                                                  
LENTAB   DS    0XL1                                                             
         DC    AL1(5,10,15,20,30,40,45,50,60,75,90,120),AL1(0)                  
         EJECT                                                                  
REPSPEC  DS    0X                  ** REPORT SPEC POOL **                       
         SPEC  H1,1,RUN                                                         
         SPEC  H1,60,C'CAMPAIGN LIST'                                           
         SPEC  H2,60,C'-------------'                                           
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,1,C'MEDIA'                                                    
         SPEC  H5,1,C'BUYER'                                                    
         SPEC  HOOK,1              MEDIA/BUYER HOOK                             
         SPEC  M1,1,CL22'CAMPAIGN NUMBER && NAME'                               
         SPEC  M2,1,22C'-'                                                      
         SPEC  M1,29,CL18'CLIENT CODE && NAME'                                  
         SPEC  M2,29,18C'-'                                                     
         SPEC  M1,54,CL19'PRODUCT CODE && NAME'                                 
         SPEC  M2,54,19C'-'                                                     
         SPEC  M1,79,C'EST'                                                     
         SPEC  M2,79,C'NUM'                                                     
         SPEC  M2,83,C'--BUYING PERIOD--'                                       
         SPEC  M1,101,C'SPT'                                                    
         SPEC  M2,101,C'LEN'                                                    
         SPEC  M1,105,C'UPGRADE EXPRESSION'                                     
         SPEC  M2,105,C'------------------'                                     
         SPEC  END                                                              
         EJECT                                                                  
BLANKS   DC    CL32' '                                                          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DELETE RECORD                                                       *         
***********************************************************************         
         USING CAMRECD,R2                                                       
DELREC   NTR1  BASE=*,LABEL=*                                                   
         MVC   IOKEY,APRECKEY                                                   
         OI    CAMKCNTL,X'80'                                                   
         GOTO1 AIO,DIRWRT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         OI    CAMCNTL,X'80'                                                    
         GOTO1 AIO,FILPUT1                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,BLDPASE6          BUILD PASSIVE E6 KEY IN IOKEY               
         GOTO1 AIO,DIRHI                                                        
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(L'CAMKEY),IOKEYSAV                                         
         BNE   DELREC10             NOTHING TO DELETE                           
         LA    R2,IOKEY                                                         
         OI    CAMKCNTL,X'80'       DELETE PASSIVE KEY                          
         GOTO1 AIO,DIRWRT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DELREC10 BAS   RE,BLDPASE8          BUILD PASSIVE E8 KEY IN IOKEY               
         GOTO1 AIO,DIRHI                                                        
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(L'CAMKEY),IOKEYSAV                                         
         BNE   DELRECX              NOTHING TO DELETE                           
         LA    R2,IOKEY                                                         
         OI    CAMKCNTL,X'80'       DELETE PASSIVE KEY                          
         GOTO1 AIO,DIRWRT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DELRECX  B     EXIT                                                             
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* RESTORE RECORD                                                      *         
***********************************************************************         
         USING CAMRECD,R2                                                       
RESREC   NTR1  BASE=*,LABEL=*                                                   
         MVC   IOKEY,APRECKEY                                                   
         NI    CAMKCNTL,X'FF'-X'80'                                             
         GOTO1 AIO,DIRWRT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         NI    CAMCNTL,X'FF'-X'80'                                              
         GOTO1 AIO,FILPUT1                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,BLDPASE6          BUILD PASSIVE E6 KEY IN IOKEY               
         GOTO1 AIO,DIRHID           READ HIGH FOR DELETES                       
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(L'CAMKEY),IOKEYSAV                                         
         BNE   RESREC10             NOTHING TO RESTORE                          
         LA    R2,IOKEY                                                         
         NI    CAMKCNTL,X'FF'-X'80' RESTORE PASSIVE KEY                         
         GOTO1 AIO,DIRWRT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RESREC10 BAS   RE,BLDPASE8          BUILD PASSIVE E8 KEY IN IOKEY               
         GOTO1 AIO,DIRHID           READ HIGH FOR DELETES                       
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(L'CAMKEY),IOKEYSAV                                         
         BNE   RESRECX              NOTHING TO RESTORE                          
         LA    R2,IOKEY                                                         
         NI    CAMKCNTL,X'FF'-X'80' RESTORE PASSIVE KEY                         
         GOTO1 AIO,DIRWRT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RESRECX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ADD PASSIVE POINTERS (E6,E8)                                        *         
***********************************************************************         
ADDPASIV NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIOAREA1       GET THE RECORD                                 
         USING CAMRECD,R2                                                       
*                                                                               
         CLI   APACTN,ACTCHA     ACTION=CHANGE?                                 
         BNE   ADDP05            NO...MUST BE ADD                               
***                                                                             
* MUST REMOVE OLD PASSIVE PTR ON A CHANGE SINCE THE PASSIVE KEY                 
* CONTAINS DATA THAT CAN BE CHANGED IN THE RECORD                               
***                                                                             
         XC    IOKEY,IOKEY       BUILD OLD E6 PASSIVE                           
         LA    R3,IOKEY                                                         
E6       USING CAMPKEY,R3        ** CLT/PRD/EST PASSIVE POINTER **              
         MVI   E6.CAMPKTYP,CAMPKTYQ X'0D'                                       
         MVI   E6.CAMPKSUB,CAMPKSBQ X'E6'                                       
         MVC   E6.CAMPKAM,CAMKAGMD  A/M                                         
         MVC   E6.CAMPKCLT,SVCCLT   OLD CLIENT                                  
         MVC   E6.CAMPKPRD,SVCPRD   OLD PRODUCT                                 
         MVC   E6.CAMPKEST,SVCEST   OLD ESTIMATE                                
         MVC   E6.CAMPKBYR,CAMKBYR  BUYER CODE                                  
         MVC   E6.CAMPKCAM,CAMKCAM  CAMPAIGN NUMBER                             
         DROP  E6                                                               
*                                                                               
         GOTO1 AIO,DIRHI         READ HIGH                                      
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(L'CAMKEY),IOKEYSAV  DOES RECORD EXIST?                     
         BNE   ADDP01                    NO                                     
         LA    R2,IOKEY                                                         
         OI    CAMKCNTL,X'80'                                                   
         GOTO1 AIO,DIRWRT                WRITE BACK                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ADDP01   L     R2,AIOAREA1       GET THE RECORD                                 
         XC    IOKEY,IOKEY       BUILD OLD E8 PASSIVE                           
         LA    R3,IOKEY                                                         
E8       USING CMP2KEY,R3        ** BYR/CLT/PRD/EST PASSIVE PTR **              
         MVI   E8.CMP2KTYP,CMP2KTYQ X'0D'                                       
         MVI   E8.CMP2KSUB,CMP2KSBQ X'E8'                                       
         MVC   E8.CMP2KAM,CAMKAGMD  A/M                                         
         MVC   E8.CMP2KBYR,CAMKBYR  BUYER CODE                                  
         MVC   E8.CMP2KCLT,SVCCLT   OLD CLIENT                                  
         MVC   E8.CMP2KPRD,SVCPRD   OLD PRODUCT                                 
         MVC   E8.CMP2KEST,SVCEST   OLD ESTIMATE                                
         MVC   E8.CMP2KCAM,CAMKCAM  CAMPAIGN NUMBER                             
         DROP  E8                                                               
*                                                                               
         GOTO1 AIO,DIRHI         READ HIGH                                      
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(L'CAMKEY),IOKEYSAV  DOES RECORD EXIST?                     
         BNE   ADDP05                    NO                                     
         LA    R2,IOKEY                                                         
         OI    CAMKCNTL,X'80'                                                   
         GOTO1 AIO,DIRWRT                WRITE BACK                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ADDP05   BAS   RE,BLDPASE6       BUILD E6 PASSIVE PTR IN IOKEY                  
         BAS   RE,ADDP50         ADD THE E6 PASSIVE PTR                         
*                                                                               
ADDP10   BAS   RE,BLDPASE8       BUILD E6 PASSIVE PTR IN IOKEY                  
         BAS   RE,ADDP50         ADD THE E6 PASSIVE PTR                         
*                                                                               
ADDPX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ALL IS OK...WE CAN NOW ADD THE PASSIVE POINTER                      *         
***********************************************************************         
         USING CAMRECD,R2                                                       
ADDP50   NTR1                                                                   
         GOTO1 AIO,DIRHID        READ HIGH FOR DELETES                          
         BNL   *+6               LOW = HARD DISK ERROR                          
         DC    H'0'                                                             
         CLC   IOKEY(L'CAMKEY),IOKEYSAV  DOES RECORD EXIST?                     
         BNE   ADDP100                   NO                                     
         LA    R2,IOKEY                                                         
         TM    CAMKCNTL,X'80'            DELETED?                               
         BO    *+6                       YES...RESTORE IT                       
         DC    H'0'                      HOW CAN THIS EXIST?                    
         NI    CAMKCNTL,X'FF'-X'80'      RESTORE                                
         LA    R1,DIRWRT                 WRITE BACK EXISTING PTR                
         B     *+14                                                             
ADDP100  MVC   IOKEY,IOKEYSAV                                                   
         LA    R1,DIRADD                 ADD NEW PTR                            
         MVC   IOKEY+14(4),SAVEDA        DISK ADDRESS                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD PASSIVE X'E6' POINTER -- R2 = A(REC)                          *         
***********************************************************************         
BLDPASE6 DS    0H                                                               
         L     R2,AIOAREA1          GET RECORD                                  
         USING CAMRECD,R2                                                       
         XC    IOKEY,IOKEY       ADD E6 PASSIVE                                 
         LA    R3,IOKEY                                                         
E6R      USING CAMPKEY,R3        ** CLT/PRD/EST PASSIVE POINTER **              
         MVI   E6R.CAMPKTYP,CAMPKTYQ X'0D'                                      
         MVI   E6R.CAMPKSUB,CAMPKSBQ X'E6'                                      
         MVC   E6R.CAMPKAM,CAMKAGMD  A/M                                        
         MVC   E6R.CAMPKCLT,CAMCLT   CLIENT                                     
         MVC   E6R.CAMPKPRD,CAMPRD   PRODUCT                                    
         MVC   E6R.CAMPKEST,CAMEST   ESTIMATE                                   
         MVC   E6R.CAMPKBYR,CAMKBYR  BUYER CODE                                 
         MVC   E6R.CAMPKCAM,CAMKCAM  CAMPAIGN NUMBER                            
         BR    RE                                                               
         DROP  E6R,R2                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD PASSIVE X'E8' POINTER -- R2 = A(REC)                          *         
***********************************************************************         
BLDPASE8 DS    0H                                                               
         L     R2,AIOAREA1          GET RECORD                                  
         USING CAMRECD,R2                                                       
         XC    IOKEY,IOKEY       ADD E8 PASSIVE                                 
         LA    R3,IOKEY                                                         
E8R      USING CMP2KEY,R3        ** BYR/CLT/PRD/EST PASSIVE PTR **              
         MVI   E8R.CMP2KTYP,CMP2KTYQ X'0D'                                      
         MVI   E8R.CMP2KSUB,CMP2KSBQ X'E8'                                      
         MVC   E8R.CMP2KAM,CAMKAGMD  A/M                                        
         MVC   E8R.CMP2KBYR,CAMKBYR  BUYER CODE                                 
         MVC   E8R.CMP2KCLT,CAMCLT   CLIENT                                     
         MVC   E8R.CMP2KPRD,CAMPRD   PRODUCT                                    
         MVC   E8R.CMP2KEST,CAMEST   ESTIMATE                                   
         MVC   E8R.CMP2KCAM,CAMKCAM  CAMPAIGN NUMBER                            
         BR    RE                                                               
         DROP  E8R,R2                                                           
         EJECT                                                                  
         DROP  RA                                                               
***********************************************************************         
* PRINT THE REPORT                                                    *         
***********************************************************************         
         USING CAMRECD,R2                                                       
PRINTREP NTR1  BASE=*,LABEL=*                                                   
         L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
         MVI   LMED,0                                                           
         MVI   LBYR,0                                                           
         MVI   APINDS,APILFLST                                                  
*                                                                               
PRTREP2  BAS   RE,GETSEL0          GET FIRST/NEXT RECORD                        
         BE    PRTREPX                                                          
         MVI   APINDS,APILNSEQ                                                  
         L     R2,IOADDR           POINT TO CAMPAIGN RECORD                     
*                                                                               
         CLC   CAMKAGMD,LMED       TEST CHANGE OF MEDIA                         
         BE    PRTREP4                                                          
         GOTO1 AGETMED,CAMKAGMD                                                 
         OI    REPHEADI,REPHFRCE                                                
         MVC   LMED,CAMKAGMD                                                    
*                                                                               
PRTREP4  CLC   CAMKBYR,LBYR        TEST CHANGE OF BUYER                         
         BE    PRTREP6                                                          
         OI    REPHEADI,REPHFRCE                                                
         LA    R1,CAMKBYR                                                       
         ICM   R1,8,=C'B'                                                       
         GOTO1 AGETBYR                                                          
         MVC   LBYR,CAMKBYR                                                     
*                                                                               
PRTREP6  MVC   WCAMP,CAMKCAM       FORMAT PRINT LINE                            
         XC    WCAMP,EFFS                                                       
         SR    R1,R1                                                            
         ICM   R1,3,WCAMP                                                       
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  REPPBCH,APDUB                                                    
         MVC   REPPBNM,CAMNAME                                                  
         GOTO1 AGETCLT,CAMCLT                                                   
         BNE   *+16                                                             
         MVC   REPPCLT,QCLT                                                     
         MVC   REPPCNM,CLTNM                                                    
         GOTO1 AGETPRD,CAMPRD                                                   
         BNE   *+16                                                             
         MVC   REPPPRD,QPRD                                                     
         MVC   REPPPNM,PRDNM                                                    
         GOTO1 AGETEST,CAMEST                                                   
         BNE   *+10                                                             
         MVC   REPPEST,QEST                                                     
         GOTO1 VDATCON,APPARM,(3,CAMSTRT),(5,REPPDAT)                           
         MVI   REPPDAT+8,C'-'                                                   
         GOTO1 (RF),(R1),(3,CAMEND),(5,REPPDAT+9)                               
         SR    R0,R0                                                            
         ICM   R0,1,CAMSLN                                                      
         BZ    PRTREP8                                                          
         EDIT  (R0),(3,REPPSLN)                                                 
PRTREP8  MVC   REPPUPGD,CAMINPUT                                                
*                                                                               
         GOTO1 VREPORT,REPD        PRINT A LINE                                 
         CLI   CAMPPRD1,0                                                       
         BE    PRTREP2                                                          
*                                                                               
         MVC   REPPPRD(15),SPACES                                               
         GOTO1 AGETPRD,CAMPPRD1                                                 
         BNE   PRTREP2                                                          
         MVC   REPPPRD(3),QPRD                                                  
*                                                                               
         BAS   RE,PRTREP20                                                      
*                                                                               
         MVI   0(R4),C'-'                                                       
         GOTO1 AGETPRD,CAMPPRD2                                                 
         BNE   PRTREP2                                                          
         MVC   1(3,R4),QPRD                                                     
*                                                                               
         BAS   RE,PRTREP20                                                      
*                                                                               
         MVI   0(R4),C'/'                                                       
         SR    R0,R0                                                            
         ICM   R0,1,CAMPLEN1                                                    
         BZ    PRTREP2                                                          
         EDIT  (R0),(3,1(R4)),ALIGN=LEFT                                        
*                                                                               
         BAS   RE,PRTREP20                                                      
*                                                                               
         MVI   0(R4),C'-'                                                       
         SR    R0,R0                                                            
         ICM   R0,1,CAMPLEN2                                                    
         BZ    PRTREP2                                                          
         EDIT  (R0),(3,1(R4)),ALIGN=LEFT                                        
*                                                                               
         GOTO1 VREPORT,REPD        PRINT A LINE                                 
         B     PRTREP2                                                          
*                                                                               
PRTREP20 LA    R4,REPPPRD                                                       
         LA    RF,15                                                            
*                                                                               
PRTREP22 CLI   0(R4),X'40'                                                      
         BER   RE                                                               
         LA    R4,1(R4)                                                         
         BCT   RF,PRTREP22                                                      
         DS    H'0'                                                             
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         SPACE 1                                                                
REPHOOK  MVC   REPH4+09(L'QMED),QMED                                            
         MVC   REPH4+14(L'MEDNM),MEDNM                                          
         MVC   REPH5+09(L'QBYR),QBYR                                            
         MVC   REPH5+14(L'BYRNM),BYRNM                                          
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
LOCALD   DSECT                                                                  
*                                                                               
AXTRA    DS    0F               ** EXTENTION ROUTINE ADDRESSES **               
AVREQ    DS    A                                                                
AVALPGR  DS    A                                                                
ADISPGR  DS    A                                                                
AVALSLEQ DS    A                                                                
ADISPKEY DS    A                                                                
ADISPSEL DS    A                                                                
ACHKTRN  DS    A                                                                
ACHKDAR  DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
DUB      DS    D                                                                
LFULL    DS    F                                                                
LACOMEL  DS    A                                                                
LACEQEL  DS    A                                                                
LACUPEL  DS    A                                                                
LACESEL  DS    A                                                                
WORK     DS    CL20                                                             
LMED     DS    XL1                                                              
LBYR     DS    XL1                                                              
LCAMP    DS    XL2                                                              
WCAMP    DS    XL2                                                              
HEADKEY  DS    CL44                HEADER KEY SAVE AREA                         
DETKEY   DS    CL44                DETAIL KEY BUILD AREA                        
DARKEY   DS    CL44                DARE KEY BUILD AREA                          
*                                                                               
SELKIND  DS    XL1                 SELECT INDICATORS                            
SELKMED  EQU   X'80'                                                            
SELKBYR  EQU   X'40'                                                            
SELKCLT  EQU   X'20'                                                            
SELKPRD  EQU   X'10'                                                            
SELKEST  EQU   X'08'                                                            
SELKNUM  EQU   X'04'                                                            
SELKDAT  EQU   X'02'                - WE HAVE A DATE FILTER                     
SELKEY   DS    XL32                SELECT KEY                                   
SVCSTART DS    XL3                                                              
SVCEND   DS    XL3                                                              
SVCINDS  DS    XL1                                                              
SVCAMWKS DS    XL(L'CAMWKS)                                                     
SVCAMOPT DS    XL1                                                              
SVCCAM   DS    XL(L'CAMCCAM)                                                    
SVCIDR   DS    CL6                                                              
SVCCLT   DS    XL(L'CAMCLT)        OLD CLT (FOR PASSIVE KEY DELETES)            
SVCPRD   DS    XL(L'CAMPRD)        OLD PRD (FOR PASSIVE KEY DELETES)            
SVCEST   DS    XL(L'CAMEST)        OLD EST (FOR PASSIVE KEY DELETES)            
SAVEDA   DS    XL4                                                              
LFLSTMON DS    CL6                                                              
LFLST    DS    CL6                                                              
LFLND    DS    CL6                                                              
LCAMSTRT DS    CL6                                                              
LCAMEND  DS    CL6                                                              
LUPDATE  DS    CL6                 2ND UPGRADE EFFECTIVE DATE                   
SVBPRD   DS    XL1                                                              
         EJECT                                                                  
       ++INCLUDE SPRANSIDD                                                      
         SPACE 2                                                                
LOCALX   EQU   *                                                                
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT DISPLAY LINE **               
         DS    CL1                                                              
LISTBYR  DS    CL3                 BUYER CODE                                   
         DS    CL2                                                              
LISTCAM  DS    CL5                 CAMPAIGN NUMBER                              
         DS    CL1                                                              
LISTCNM  DS    CL20                CAMPAIGN NAME                                
         DS    CL1                                                              
LISTCLT  DS    CL3                 CLIENT CODE                                  
         DS    CL1                                                              
LISTPRD  DS    CL3                 PRODUCT CODE                                 
         DS    CL1                                                              
LISTEST  DS    CL3                 ESTIMATE CODE                                
         DS    CL1                                                              
LISTDAT  DS    CL17                BUYING PERIOD                                
         DS    CL1                                                              
LISTSLN  DS    CL11                SPOT LENGTH                                  
         EJECT                                                                  
* SPNWSWRK                                                                      
*        PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
*        PRINT ON                                                               
         EJECT                                                                  
REPD     DSECT                     ** REPORT LINE LAYOUT **                     
         ORG   REPP1                                                            
REPPBCH  DS    CL6                 CAMPAIGN NUMBER                              
         DS    CL1                                                              
REPPBNM  DS    CL20                CAMPAIGN NAME                                
         DS    CL1                                                              
REPPCLT  DS    CL3                 CLIENT CODE                                  
         DS    CL1                                                              
REPPCNM  DS    CL20                CLIENT NAME                                  
         DS    CL1                                                              
REPPPRD  DS    CL3                 PRODUCT CODE                                 
         DS    CL1                                                              
REPPPNM  DS    CL20                PRODUCT NAME                                 
         DS    CL1                                                              
REPPEST  DS    CL3                 ESTIMATE NUMBER                              
         DS    CL1                                                              
REPPDAT  DS    CL17                BUYING PERIOD                                
         DS    CL1                                                              
REPPSLN  DS    CL3                 SPOT LENGTH                                  
         DS    CL1                                                              
REPPUPGD DS    CL28                                                             
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSFDD                                                       
         EJECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSEDD                                                       
         EJECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSDDD                                                       
         ORG                                                                    
         EJECT                                                                  
* SPGENPURP                                                                     
*        PRINT OFF                                                              
       ++INCLUDE SPGENPURP                                                      
*        PRINT ON                                                               
         SPACE 1                                                                
* SPGENPRD                                                                      
PRDHDRD  DSECT                                                                  
*        PRINT OFF                                                              
       ++INCLUDE SPGENPRD                                                       
*        PRINT ON                                                               
         SPACE 1                                                                
* SPGENPRG                                                                      
*        PRINT OFF                                                              
       ++INCLUDE SPGENPRG                                                       
*        PRINT ON                                                               
         SPACE 1                                                                
* SPNWSCAM                                                                      
       ++INCLUDE SPNWSCAM                                                       
         SPACE 1                                                                
CAMRECD  DSECT                                                                  
         ORG   CAMKREST                                                         
CAMKCLT  DS    XL2                                                              
CAMKPRD  DS    XL1                                                              
CAMKEST  DS    XL1                                                              
         EJECT                                                                  
* SPNWSHDR                                                                      
       ++INCLUDE SPNWSHDR                                                       
* FAGETTXTD                                                                     
       ++INCLUDE FAGETTXTD                                                      
* SPGENDRORD                                                                    
       ++INCLUDE SPGENDRORD                                                     
* DDPERVALD                                                                     
       ++INCLUDE DDPERVALD                                                      
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035SPNWS03   02/26/07'                                      
         END                                                                    
