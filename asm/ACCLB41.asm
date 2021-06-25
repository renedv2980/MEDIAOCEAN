*          DATA SET ACCLB41    AT LEVEL 081 AS OF 08/16/00                      
*PHASE T62141A                                                                  
CLB41    TITLE '- BILL PROGRAM ROUTINES 2'                                      
CLB41    CSECT                                                                  
         PRINT NOGEN                                                            
ROUT     NMOD1 0,**CB41**,RR=R8                                                 
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
*                                                                               
         SRL   RF,24               RF = A(ROUTINE, SIZE OF LOCAL W/S)           
         SLL   RF,3                                                             
         LA    RF,AROUT(RF)                                                     
         A     R8,0(RF)            R8 = A(ROUTINE)                              
*                                                                               
         ICM   R3,15,4(RF)         TEST FOR W/S REQUIRED                        
         BZ    ROUT02                                                           
         BCTR  R3,0                                                             
         SRL   R3,3                ENSURE DOUBLE WORD LENGTH                    
         LA    R3,1(R3)                                                         
         SLL   R3,3                                                             
         L     RF,4(RD)            EXTEND W/S                                   
         AR    RD,R3                                                            
         ST    RD,8(RF)                                                         
         ST    RF,4(RD)                                                         
         LR    R2,RC               CLEAR W/S                                    
         XR    RF,RF                                                            
         MVCL  R2,RE                                                            
*                                                                               
ROUT02   DS    0H                  BRANCH TO ROUTINE                            
         BR    R8                                                               
*                                                                               
AROUT    DS    0A                  ROUTINES / LOCAL W/S SIZE                    
         DC    A(SETUP,SUWORKL)                                                 
         DC    A(UPDJOB,UJWORKL)                                                
         DC    A(TSARIO,TIWORKL)                                                
         DC    A(EDTAMT,EAWORKL)                                                
         DC    A(SAVOWS,SOWORKL)                                                
         DC    A(VALBAT,VBWORKL)                                                
*                                                                               
ROUTL    MVI   BCDUB,0             SET CC LOW                                   
         B     ROUTCC                                                           
ROUTH    MVI   BCDUB,2             SET CC HIGH                                  
         B     ROUTCC                                                           
ROUTE    MVI   BCDUB,1             SET CC EQUAL                                 
ROUTCC   CLI   BCDUB,1                                                          
*                                                                               
ROUTX    XIT1  ,                   EXIT WITH CC SET                             
         SPACE 1                                                                
LTORG    DS    0H                  SPACE FOR LITERALS                           
         ORG   CLB41+X'180'                                                     
LTORGX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET UP GLOBAL VALUES FOR NEW JOB                         *         
*                                                                     *         
* NTRY: P1 BYTE 0 = 0 TO SET UP EVERTHING                             *         
*                   X'80' ON TO SET UP CLIENT VALUES                  *         
*                   X'40' ON TO SET UP PRODUCT VALUES                 *         
*                   X'20' ON TO SET UP JOB VALUES                     *         
*                   X'10' ON TO SET UP CURRENCY                       *         
*                   X'08' ON TO SET UP EXCHANGE RATE                  *         
*                   X'04' ON TO SET UP FORMAT CODE                    *         
*             1-3 = CLIENT/PRODUCT/JOB CODE                           *         
*                                                                     *         
*       P2 BYTE 0 = X'80' ON TO SUPPRESS JOB LOCKED WARNING           *         
*                   X'40' ON IF CURRENCY DOES NOT DEPEND ON JOB       *         
*             1-3 = A(OVERRIDE CURRENCY) OR 0                         *         
*                                                                     *         
*       P3        = A(OVERRIDE EXCHANGE RATE) OR 0                    *         
***********************************************************************         
         SPACE 1                                                                
         USING SUWORKD,RC                                                       
SETUP    DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         MVC   SUIPARM,0(R1)                                                    
         CLI   SUIPVAL,0           DEFAULT IS TO VALIDATE EVERYTHING            
         BNE   *+8                                                              
         MVI   SUIPVAL,FF                                                       
         XR    RF,RF                                                            
         ICM   RF,7,SUIPAJOB       GET JOB CODE                                 
         BZ    *+10                                                             
         MVC   SUJOB,0(RF)                                                      
         L     R4,AGOPBLK          R4=A(GETOPT BLOCKS)                          
         USING GOBLOCKD,R4                                                      
         USING GOBBLCKD,GOBLOCKD+(JOBBLK-GOPBLK)                                
*                                                                               
KEY      USING ACTRECD,IOKEY       IOKEY=CLIENT/PRODUCT/JOB KEY                 
         MVC   KEY.ACTKEY,BCSPACES                                              
         MVC   KEY.ACTKCPY,CUABIN                                               
         MVC   KEY.ACTKUNT(L'BCCPYPRD),BCCPYPRD                                 
         SPACE 1                                                                
***********************************************************************         
* - SET UP CLIENT VALUES                                              *         
***********************************************************************         
         SPACE 1                                                                
SETCLI   TM    SUIPVAL,SUIPVCLI                                                 
         BZ    SETCLIX                                                          
         XC    BCCLI(BCCLIL),BCCLI CLEAR CURRENT VALUES                         
         IC    RE,BCCLILEN         GET CLIENT RECORD                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   KEY.ACTKACT(0),SUJOB                                             
         GOTO1 AGETACT,0                                                        
         BNE   SETUPL                                                           
*                                                                               
         L     R3,AIO1             TEST CLIENT IS LOCKED                        
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         USING LOKELD,R3                                                        
         XR    RF,RF                                                            
SETCLI02 CLI   LOKEL,0                                                          
         BE    SETCLI04                                                         
         CLI   LOKEL,LOKELQ                                                     
         BE    *+12                                                             
         IC    RF,LOKLN                                                         
         BXH   R3,RF,SETCLI02                                                   
         TM    LOKSTAT,LOKSLOCK                                                 
         BZ    SETCLI04                                                         
         MVC   FVMSGNO,=AL2(AE$CLILK)                                           
         B     SETUPL                                                           
         DROP  R3                                                               
*                                                                               
SETCLI04 MVC   BCCLICOD,KEY.ACTKACT SET CLIENT VALUES                           
         MVC   BCCLINAM,ACNAME                                                  
         MVC   BCCLIOFF,ACOFFC                                                  
         MVC   BCCLIDA,ACDA                                                     
         ICM   RF,15,ACAPPR                                                     
         MVC   BCCLIPRF,0(RF)                                                   
*                                                                               
         XC    BCWORK,BCWORK       GET PROGRAM PROFILE FOR CLIENT               
         MVI   BCWORK+00,C'A'-X'40'                                             
         MVC   BCWORK+01(3),=C'CB1'                                             
         MVC   BCWORK+05(2),BCCPYPRD                                            
         MVC   BCWORK+07(3),BCCLICOD                                            
         MVC   BCWORK+12(2),TWAAGY                                              
         GOTO1 VGETPROF,BCPARM,BCWORK,BCBPROF1,VDMGR                            
         MVI   BCWORK+03,C'2'                                                   
         GOTO1 (RF),(R1),,BCBPROF2 GET PAGE 2 ALSO                              
*                                                                               
SETCLIX  DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* - SET UP PRODUCT VALUES                                             *         
***********************************************************************         
         SPACE 1                                                                
SETPRO   TM    SUIPVAL,SUIPVPRO                                                 
         BZ    SETPROX                                                          
         XC    BCPRO(BCPROL),BCPRO                                              
         IC    RE,BCPROLEN         GET PRODUCT RECORD                           
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   KEY.ACTKACT(0),SUJOB                                             
         GOTO1 AGETACT,0                                                        
         BNE   SETUPL                                                           
         MVC   BCPROCOD,KEY.ACTKACT SET PRODUCT VALUES                          
         MVC   BCPRONAM,ACNAME                                                  
         MVC   BCPROOFF,ACOFFC                                                  
         MVC   BCPRODA,ACDA                                                     
         XC    BCPROPRF,BCPROPRF                                                
         ICM   RF,15,ACAPPR                                                     
         BZ    *+10                                                             
         MVC   BCPROPRF,0(RF)                                                   
SETPROX  DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* - SET UP JOB VALUES                                                 *         
***********************************************************************         
         SPACE 1                                                                
SEJOB    TM    SUIPVAL,SUIPVJOB                                                 
         BZ    SETJOBX                                                          
         CLC   SUJOB,BCJOBCOD      TEST OLD JOB=NEW JOB                         
         BE    *+8                                                              
         NI    CSINDSG1,FF-CSWARNED  NO - TURN OFF WARNING BIT                  
         XC    BCJOB(BCJOBL),BCJOB                                              
         XC    CSBILNUM,CSBILNUM                                                
         MVC   KEY.ACTKACT,SUJOB   GET JOB RECORD                               
         GOTO1 AGETACT,0                                                        
         BNE   SETUPL                                                           
         MVC   BCJOBCOD,KEY.ACTKACT SET JOB VALUES                              
         MVC   BCJOBNAM,ACNAME                                                  
         MVC   BCJOBOFF,ACOFFC                                                  
         MVC   BCJOBDA,ACDA                                                     
         XC    BCJOBPRF,BCJOBPRF                                                
         ICM   RF,15,ACAPPR                                                     
         BZ    *+10                                                             
         MVC   BCJOBPRF,0(RF)                                                   
         TM    ACSTAT1,RSTSACIC    TEST JOB IS CLOSED                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$JOBCL)                                           
         B     SETUPL                                                           
         TM    ACSTAT1,RSTSACIL    TEST JOB IS LOCKED                           
         BZ    SETJOB02                                                         
         CLI   P#LOKJOB,C'P'       TEST LOCK NOT ALLOWED                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$JOBLK)                                           
         B     SETUPL                                                           
         CLI   P#LOKJOB,C'W'       TEST LOCK WARNING REQUIRED                   
         BNE   SETJOB02                                                         
         TM    CSINDSG1,CSWARNED   TEST WARNING GIVEN                           
         BO    SETJOB02                                                         
         OI    CSINDSG1,CSWARNED   NO - GIVE IT                                 
         TM    SUIPINDS,SUIPINWN   TEST WARNING NOT REQUIRED                    
         BO    SETJOB02                                                         
         MVC   FVMSGNO,=AL2(AE$LKENT)                                           
         B     SETUPL                                                           
         DROP  KEY                                                              
*                                                                               
SETJOB02 GOTO1 ACMPPRF             MERGE PROFILES                               
         LA    RE,GOBBLCKD         CALL GETOPT FOR JOB                          
         ST    RE,GOABEXT                                                       
         GOTO1 AGETOPT,BOPARM,AIO1                                              
         CLI   GOBILTYP,PPRBCLIT                                                
         BNE   *+8                                                              
         OI    BCJOBSTA,BCJOBSCB   SET CLIENT BILLING                           
*&&US                                                                           
         NI    LSSTAT1,FF-LSSSEPTY                                              
         CLC   GOTWO,BCSPACES      TEST SEPERATING TIME/OOPS                    
         BNH   *+8                                                              
         OI    LSSTAT1,LSSSEPTY                                                 
*&&                                                                             
*                                                                               
         L     R3,AIO1                                                          
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         USING JOBELQ,R3                                                        
         XR    RF,RF                                                            
SETJOB04 CLI   0(R3),0                                                          
         BE    SETJOB20                                                         
         CLI   0(R3),JOBELQ                                                     
         BE    SETJOB08                                                         
         CLI   0(R3),AFCELQ                                                     
         BE    SETJOB10                                                         
         CLI   0(R3),JCBELQ                                                     
         BE    SETJOB12                                                         
SETJOB06 IC    RF,1(R3)                                                         
         BXH   R3,RF,SETJOB04                                                   
*                                                                               
         USING JOBELD,R3                                                        
SETJOB08 TM    JOBSTA1,JOBSXJOB    TEST EXPENSE JOB                             
         BZ    SETJOB06                                                         
         MVC   FVMSGNO,=AL2(AE$CUEXJ)                                           
         B     SETUPL                                                           
         USING AFCELD,R3                                                        
SETJOB10 CLC   AFCCURR,BCSPACES    TEST AMOUNT PENDING                          
         BNH   SETJOB06                                                         
         OI    BCJOBSTA,BCJOBPEN                                                
         MVC   CSBILCUR,AFCCURR    YES - SET CURRENCY CODE/RATE                 
         MVC   CSEXCVAL,AFCX                                                    
         MVI   CSTYPEXC,0                                                       
         XC    CSDATEXC,CSDATEXC                                                
         B     SETJOB06                                                         
         USING JCBELD,R3                                                        
SETJOB12 TM    BCJOBSTA,BCJOBPEN   TEST AMOUNT PENDING                          
         BZ    SETJOB06                                                         
         MVC   CSTYPEXC,JCBXTYP    SET EXCHANGE RATE TYPE                       
         MVC   CSDATEXC,JCBXDAT    & DATE SET                                   
         B     SETJOB06                                                         
         DROP  R3                                                               
*                                                                               
SETJOB20 TM    BCCPYST1,CPYSOROE   TEST COMPANY USES OFFICES                    
         BZ    SETJOB22                                                         
         MVC   CSOFFICE,BCJOBOFF                                                
         CLC   CSOFFICE,BCSPACES                                                
         BH    SETJOB22                                                         
         MVC   CSOFFICE,BCPROOFF                                                
         CLC   CSOFFICE,BCSPACES                                                
         BH    SETJOB22                                                         
         MVC   CSOFFICE,BCCLIOFF                                                
*                                                                               
SETJOB22 L     R1,AOFFBLK          TEST OFFICE SECURITY                         
         USING OFFALD,R1                                                        
         MVC   OFFAREC,AIO1                                                     
         MVI   OFFAOPOS,LDGOPROF                                                
         MVC   OFFAOFFC,CSOFFICE                                                
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         BE    SETBNO                                                           
         MVC   FVMSGNO,=AL2(AE$SECLK)                                           
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(L'BCJOBCOD),BCJOBCOD                                      
         B     SETUPL                                                           
         DROP  R1                                                               
         SPACE 1                                                                
***********************************************************************         
*  - SET BILL NUMBER ORIGIN                                           *         
***********************************************************************         
         SPACE 1                                                                
SETBNO   LA    R2,IOKEY                                                         
         CLC   CSOFFICE,BCSPACES   TEST OFFICE RESOLVED                         
         BNH   SETBNO04                                                         
*                                                                               
         USING OGRRECD,R2          READ PRODUCTION OFFICE RECORD                
         XC    OGRKEY,OGRKEY                                                    
         MVI   OGRKTYP,OGRKTYPQ                                                 
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY,CUABIN                                                   
         MVC   OGRKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   OGRKOFC,CSOFFICE                                                 
         GOTO1 AIO,IOREAD+IOACCDIR+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         L     R1,AIO2                                                          
         LA    R1,OGRRFST-OGRRECD(R1)                                           
         USING BNCELD,R1                                                        
SETBNO02 CLI   BNCEL,BNCELQ                                                     
         BNE   *+12                                                             
         OI    BCJOBSTA,BCJOBBNO   SET BILL NUMBER FROM OFFICE                  
         B     SETBNO10                                                         
         IC    R0,BNCLN                                                         
         AR    R1,R0                                                            
         CLI   BNCEL,0                                                          
         BNE   SETBNO02                                                         
*                                                                               
         USING LDGRECD,R2                                                       
SETBNO04 MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN                                                   
         MVC   LDGKUNT(L'BCCPYPRD),BCCPYPRD                                     
         GOTO1 AIO,IOREAD+IOACCDIR+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         L     R1,AIO2                                                          
         LA    R1,LDGRFST-LDGRECD(R1)                                           
         USING PMDELD,R1                                                        
SETBNO06 CLI   PMDEL,PMDELQ                                                     
         BNE   *+12                                                             
         OI    BCJOBSTA,BCJOBBNL   SET BILL NUMBER FROM LEDGER                  
         B     SETBNO10                                                         
         IC    R0,PMDLN                                                         
         AR    R1,R0                                                            
         CLI   PMDEL,0                                                          
         BNE   SETBNO06                                                         
*                                                                               
         USING PMDRECD,R2                                                       
SETBNO10 MVC   PMDKEY,BCSPACES                                                  
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUABIN                                                   
         SR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         LA    RE,BCJOBCOD(RE)                                                  
         MVC   PMDKMED,0(RE)                                                    
         GOTO1 AIO,IOREAD+IOACCDIR+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         L     R1,AIO2                                                          
         LA    R1,PMDRFST-PMDRECD(R1)                                           
         USING PMDELD,R1                                                        
SETBNO12 CLI   PMDEL,PMDELQ                                                     
         BNE   SETBNO14                                                         
         TM    BCJOBSTA,BCJOBBNO+BCJOBBNL                                       
         BNZ   *+8                 BILL ORIGIN ALREADY SET                      
         OI    BCJOBSTA,BCJOBBNM   SET BILL NUMBER FROM MEDIA                   
         MVC   LSUMEDNM,PMDDESC    SET MEDIA NAME FOR BILLING SOURCE            
         MVC   LSUMEDIN,PMDCOM1    SET MEDIA RECORD INCOME ACCOUNT              
         B     SETBNOX                                                          
SETBNO14 IC    R0,PMDLN                                                         
         AR    R1,R0                                                            
         CLI   PMDEL,0                                                          
         BNE   SETBNO12                                                         
*                                                                               
         OI    BCJOBSTA,BCJOBBNA   SET BILL NUMBER MUST BE AUTO/MANUAL          
*                                                                               
SETBNOX  DS    0H                                                               
         DROP  R1,R2                                                            
*                                                                               
SETJOBX  DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* - SET CURRENCY CODE                                                 *         
***********************************************************************         
         SPACE 1                                                                
SETCUR   TM    SUIPINDS,SUIPICNJ   TEST NO JOB                                  
         BO    *+12                                                             
         TM    BCJOBSTA,BCJOBPEN   TEST ALLCOATION PENDING                      
         BO    SETCUR02                                                         
         TM    BCCPYST6,CPYSFBIL   TEST FOREIGN CURRENCY BILLING                
         BO    *+14                                                             
         MVC   CSBILCUR,CSCPYCUR                                                
         B     SETCUR02                                                         
         TM    SUIPVAL,SUIPVCUR    TEST VALIDATING CURRENCY                     
         BZ    SETCURX                                                          
         XR    RF,RF                                                            
         ICM   RF,7,SUIPACUR                                                    
         BZ    *+10                                                             
         MVC   CSBILCUR,0(RF)                                                   
         CLC   CSBILCUR,BCSPACES   TEST CALLER SET CURRENCY                     
         BH    SETCUR02                                                         
*                                                                               
         MVC   CSBILCUR,GOBILCUR   DEFAULT TO GETOPT CURRENCY                   
         CLC   CSBILCUR,BCSPACES                                                
         BH    SETCUR02                                                         
         MVC   CSBILCUR,CSCPYCUR   OR TO COMPANY CURRENCY                       
*                                                                               
SETCUR02 CLC   CSBILCUR,CSCURBIL+(CURTCUR-CURTABD)                              
         BE    SETCURX             TEST ALREADY HAVE CURRENCY TABLE             
         MVC   CSCURBIL,CSCURCPY                                                
         CLC   CSBILCUR,CSCPYCUR   TEST CURRENCY TABLE IS COMPANY'S             
         BE    SETCURX             NO - BUILD IT                                
         GOTO1 AGETCUR,BCPARM,(X'A0',CSBILCUR),CSCURBIL,,CSMINEXC,     *        
               CSMAXEXC                                                         
         BNE   SETUPL                                                           
*                                                                               
SETCURX  DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* - SET EXCHANGE RATE                                                 *         
***********************************************************************         
         SPACE 1                                                                
SETEXC   CLC   CSBILCUR,CSCPYCUR   TEST BILLING=AGENCY CURRENCY                 
         BNE   SETEXC02                                                         
         XC    CSEXCVAL,CSEXCVAL   YES - SET 1 : 1 RATE                         
         MVI   CSEXCRAT+2,X'10'                                                 
         MVI   CSTYPEXC,0                                                       
         B     SETEXCX                                                          
*                                                                               
SETEXC02 TM    SUIPINDS,SUIPICNJ   TEST NO JOB                                  
         BO    *+12                                                             
         TM    BCJOBSTA,BCJOBPEN   TEST ALLOCATION PENDING                      
         BO    SETEXCX                                                          
         TM    SUIPVAL,SUIPVEXC    TEST VALIDATING EXCHANGE RATE                
         BZ    SETEXCX                                                          
         XC    CSEXCVAL,CSEXCVAL                                                
         XR    RF,RF                                                            
         ICM   RF,7,SUIPAEXC                                                    
         BZ    *+10                                                             
         MVC   CSEXCVAL,0(RF)                                                   
         OC    CSEXCVAL,CSEXCVAL   TEST CALLER SET EXCHANGE RATE                
         BZ    SETEXC06                                                         
         CLC   CSEXCRAT,CSMINEXC   TEST MIM/MAX VALUES                          
         BL    SETEXC04                                                         
         CLC   CSEXCRAT,CSMAXEXC                                                
         BH    SETEXC04                                                         
         MVI   CSTYPEXC,JCBXINPQ                                                
         B     SETEXC08                                                         
SETEXC04 MVC   FVMSGNO,=AL2(AE$EXRNV)                                           
         GOTO1 AEDTRAT,SUPARM,(L'FVXTRA,FVXTRA),CSMINEXC,CSMAXEXC               
         B     SETUPL                                                           
*                                                                               
SETEXC06 LA    R2,BCWORK                                                        
         USING GEXCD,R2            BUILD KEY OF EXCHANGE RECORD                 
         XC    GEKEY,GEKEY                                                      
         MVC   GEKAGY,CUAALF                                                    
         MVC   GEKCURF,CSCPYCUR                                                 
         MVC   GEKCURT,CSBILCUR                                                 
         MVC   GEKACT,BCCLICOD                                                  
         MVC   GEKPEND,BCTODAYC                                                 
         LA    RF,X'20'            SET TO GET ACCOUNTING RATE                   
         TM    BCCPYST6,CPYSFTXR                                                
         BZ    *+8                                                              
         LA    RF,X'0C'(RF)        SET FT RATES ARE PERMITTED                   
         GOTO1 VGETCUR,BCPARM,((RF),GEXCD),ACOM                                 
         CLI   0(R1),0                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$EXCNF)                                           
         B     SETUPL                                                           
         L     R2,0(R1)                                                         
         MVC   CSEXCIND,GESTAT                                                  
         MVC   CSEXCRAT,GEXRATE                                                 
         MVC   CSEXCSHF,GEXSHFT                                                 
         CLI   GEKSYS,GEKSFTQ      TEST FT-RATE                                 
         BNE   *+12                                                             
         MVI   CSTYPEXC,JCBXFTQ                                                 
         B     SETEXC08                                                         
         MVI   CSTYPEXC,JCBXAGYQ   SET AGENCY RATE                              
         CLC   GEKACT,BCCLICOD     TEST CLIENT RATE                             
         BNE   SETEXC08                                                         
         MVI   CSTYPEXC,JCBXCLIQ                                                
         DROP  R2                                                               
SETEXC08 MVC   CSDATEXC,BCTODAYC                                                
*                                                                               
SETEXCX  DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* - SET FORMAT CODE                                                   *         
***********************************************************************         
         SPACE 1                                                                
SETFMT   TM    SUIPVAL,SUIPSFMT    TEST SETTING FORMAT CODE                     
         BZ    SETFMTX                                                          
         MVC   CSFORMAT,GOBILFRM   USE FORMAT CODE FROM GETOPT                  
         CLI   CSFORMAT,0                                                       
         BNE   SETFMTX                                                          
         PUSH  USING                                                            
         USING PBCRECD,IOKEY       IF N/D USE FIRST FORMAT ON RECORD            
         XC    PBCKEY,PBCKEY                                                    
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVI   PBCKSUB,PBCKCONQ                                                 
         GOTO1 AIO,IOHIGH+IOACCDIR                                              
         BNE   SETFMT02                                                         
         CLC   PBCKEY(PBCKFMT-PBCRECD),IOKEYSAV                                 
         BNE   SETFMT02                                                         
         MVC   CSFORMAT,PBCKFMT                                                 
         B     SETFMTX                                                          
         POP   USING                                                            
*                                                                               
SETFMT02 MVC   FVMSGNO,=AL2(AE$FMTNA)                                           
         B     SETUPL              NO FOMRAT RECORD ON FILE FOR COMPANY         
*                                                                               
SETFMTX  DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* RETURN TO CALLER                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETUPX   B     ROUTE                                                            
*                                                                               
SETUPL   B     ROUTL                                                            
         SPACE 1                                                                
         DROP  R4,RC                                                            
         SPACE 1                                                                
***********************************************************************         
* SETUP LOCAL W/S                                                     *         
***********************************************************************         
         SPACE 1                                                                
SUWORKD  DSECT                                                                  
SUIPARM  DS    0XL(SUIPARML)       * INPUT PARAMATERS *                         
SUIPVAL  DS    XL1                 VALIDATE INDICATOR BYTE                      
SUIPVCLI EQU   X'80'               VALIDATE CLIENT CODE                         
SUIPVPRO EQU   X'40'               VALIDATE PRODUCT CODE                        
SUIPVJOB EQU   X'20'               VALIDATE JOB CODE                            
SUIPVCUR EQU   X'10'               VALIDATE CURRENCY                            
SUIPVEXC EQU   X'08'               VALIDATE EXCHANGE RATE                       
SUIPSFMT EQU   X'04'               SET FORMAT CODE                              
SUIPAJOB DS    AL3                 A(JOB)                                       
SUIPINDS DS    XL1                 INDICATOR BYTE                               
SUIPINWN EQU   X'80'               NO WARNING IF JOB IS LOCKED                  
SUIPICNJ EQU   X'40'               CURRENCY DOES NOT DEPEND ON JOB              
SUIPACUR DS    AL3                 A(OVERRIDE CURRENCY)                         
         DS    XL1                                                              
SUIPAEXC DS    AL3                 A(OVERRIDE EXCHANGE RATE)                    
SUIPARML EQU   *-SUIPVAL                                                        
*                                                                               
SUPARM   DS    6A                                                               
SUPL8    DS    PL8                                                              
*                                                                               
SUAAFC   DS    A                   A(AFCEL ON JOB RECORD)                       
SUJOB    DS    CL12                JOB CODE                                     
SUCUR    DS    CL3                 CURRENCY CODE                                
SUEXC    DS    XL(L'CSEXCVAL)      EXCHANGE RATE                                
*                                                                               
SUPROF   DS    CL16                                                             
*                                                                               
SUWORKL  EQU   *-SUWORKD                                                        
CLB41    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE JOB AFTER ITEM HAS BEEN UPDATED                   *         
*                                                                     *         
* NTRY: P1 BYTE 0 = C'U' FOR AN UPDATED ITEM                          *         
*             1-3 = A(TRANSACTION RECORD)                             *         
*       P2        = A(JOB RECORD)                                     *         
*       P3        = A(OLD PRO-RATA BLOCK)                             *         
*                 = 0 IF RECORD HAS BEEN ADDED                        *         
*       P4        = A(NEW PRO-RATA BLOCK)                             *         
*       LSTLST    = CURRENT TSAR LIST RECORD FOR TRANSACTION          *         
*  OR                                                                 *         
* NTRY: P1 BYTE 0 = C'C' TO CLEAR AMOUNT AFTER UPDATE                 *         
*               3 = SCITYPE                                           *         
*       P2        = A(JOB RECORD)                                     *         
*                                                                     *         
*  OR                                                                 *         
* NTRY: P1 BYTE 0 = C'D' TO DISPLAY JOB DETAILS (ON FOOTLINES)        *         
*             1-3 = A(SCITYPE LIST)                                   *         
*       P2        = A(JOB RECORD)                                     *         
*       P3        = A(1ST FOOTLINE OF 2)                              *         
*                                                                     *         
* EXIT: P1        = A(JCBEL IN JOB RECORD)                            *         
*       P1 BYTE 0 = SCIEL TYPE IF ERROR                               *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING UJWORKD,RC                                                       
UPDJOB   DS    0H                                                               
         USING *,R8                                                             
         MVC   UJPARMS,0(R1)                                                    
         ST    R1,UJAR1                                                         
*                                                                               
         CLI   UJPACT,UJPADIS      TEST DISPLAY ACTION                          
         BE    UPDDIS                                                           
*                                                                               
         L     RE,UJPAJOB          SAVE JOB                                     
         XR    RF,RF                                                            
         ICM   RF,3,ACTRLEN-ACTRECD(RE)                                         
         LA    R0,UJSAVJOB                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING JCBELD,UJJCBEL                                                   
         XC    JCBELD(JCBLNQ),JCBELD                                            
         MVI   JCBEL,JCBELQ                                                     
         MVI   JCBLN,JCBLNQ                                                     
*                                                                               
         L     R3,UJPAJOB          SEARCH FOR EXISTING JCBEL                    
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         XR    RF,RF                                                            
UPDJOB02 CLI   0(R3),0                                                          
         BE    UPDJOB04                                                         
         CLI   0(R3),JCBELQ                                                     
         BE    *+12                                                             
         IC    RF,1(R3)                                                         
         BXH   R3,RF,UPDJOB02                                                   
         MVC   JCBELD(JCBLNQ),0(R3)                                             
*                                                                               
UPDJOB04 NI    JCBINDS1,FF-JCBIPEND                                             
*                                                                               
         LA    R2,UPDTAB                                                        
         USING UPDTABD,R2          R2=A(UPDATE TABLE)                           
*                                                                               
UPDJOB12 L     R3,UJPAJOB          LOCATE SCIEL ON JOB                          
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         USING SCIELD,R3                                                        
         XR    RF,RF                                                            
UPDJOB14 CLI   SCIEL,0                                                          
         BE    UPDJOB16                                                         
         CLI   SCIEL,SCIELQ                                                     
         BNE   *+14                                                             
         CLC   SCITYPE,UPDTYPE                                                  
         BE    UPDJOB18                                                         
         IC    RF,SCILN                                                         
         BXH   R3,RF,UPDJOB14                                                   
UPDJOB16 LA    R3,UJWORK           ADD ELEMENT IF NOT ON RECORD                 
         XC    SCIELD(SCILN2Q),SCIELD                                           
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN2Q                                                    
         MVC   SCITYPE,UPDTYPE                                                  
         ZAP   SCIAMNT,BCPZERO                                                  
         ZAP   SCIADMN,BCPZERO                                                  
         GOTO1 VHELLO,UJPARM,(C'P',ACCMST),UJPAJOB,SCIELD                       
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,16(R1)                                                        
*                                                                               
UPDJOB18 LH    R4,UPDJCB                                                        
         LA    R4,JCBELD(R4)       R4=A(COUNTER FIELD)                          
*                                                                               
         CLI   UJPACT,UJPACLR      TEST CLEARING                                
         BNE   UPDJOB30                                                         
         TM    UPDINDS,UPDINJCB                                                 
         BO    UPDJOB50                                                         
         CLC   SCITYPE,UJPCTYPE    MATCH ON SCITYPE                             
         BNE   UPDJOB50                                                         
         ZAP   SCIAMNT,BCPZERO     ZEROISE TOTALS                               
         ZAP   SCIADMN,BCPZERO                                                  
*                                                                               
         TM    UPDINDS,UPDIAST     TEST REDUCE ASTDRAFT AMOUNT                  
         BZ    UPDJOB20                                                         
         L     R1,UJPAJOB                                                       
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING ASTELD,R1                                                        
         XR    RF,RF                                                            
         CLI   ASTEL,ASTELQ                                                     
         BE    *+12                                                             
         IC    RF,ASTLN                                                         
         BXH   R1,RF,*-12                                                       
         ICM   RF,7,ASTDRAFT                                                    
         SH    RF,0(R4)                                                         
         BNM   *+6                                                              
         XR    RF,RF                                                            
         STCM  RF,7,ASTDRAFT                                                    
         DROP  R1                                                               
*                                                                               
UPDJOB20 XC    0(2,R4),0(R4)       CLEAR JCBELD AMOUNT                          
         B     UPDJOB50                                                         
*                                                                               
UPDJOB30 ZAP   UJTOTBEF,BCPZERO    CLEAR BEFORE/AFTER TOTALS                    
         ZAP   UJTOTAFT,BCPZERO                                                 
*                                                                               
         LH    RE,UPDAAMNT         RE=DISP. TO SCIAMNT AMOUNT                   
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LH    RE,UPDFAMNT                                                      
         LTR   RE,RE                                                            
         BZ    UPDJOB36                                                         
         CLI   UPDAMNTS,0          TEST TLXSTAT FILTER                          
         BE    UPDJOB31                                                         
         MVC   BCBYTE1,UPDAMNTS                                                 
         NC    BCBYTE1,TLXSTAT                                                  
         BZ    UPDJOB36                                                         
UPDJOB31 CLI   UPDAMNTN,0          TEST 'NOT' TLXSTAT FILTER                    
         BE    UPDJOB32                                                         
         MVC   BCBYTE1,UPDAMNTN                                                 
         NC    BCBYTE1,TLXSTAT                                                  
         BNZ   UPDJOB36                                                         
UPDJOB32 ICM   R1,15,UJPAOLD                                                    
         BZ    UPDJOB34                                                         
         AR    R1,RE                                                            
         SP    SCIAMNT,0(8,R1)                                                  
         AP    UJTOTBEF,0(8,R1)                                                 
UPDJOB34 L     R1,UJPANEW                                                       
         AR    R1,RE                                                            
         AP    SCIAMNT,0(8,R1)                                                  
         AP    UJTOTAFT,0(8,R1)                                                 
*                                                                               
UPDJOB36 LH    RE,UPDAADMN         RE=DISP. TO SCIADMN AMOUNT                   
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LH    RE,UPDFADMN                                                      
         LTR   RE,RE                                                            
         BZ    UPDJOB42                                                         
         CLI   UPDADMNS,0          TEST TLXSTAT FILTER                          
         BE    UPDJOB37                                                         
         MVC   BCBYTE1,UPDADMNS                                                 
         NC    BCBYTE1,TLXSTAT                                                  
         BZ    UPDJOB42                                                         
UPDJOB37 CLI   UPDADMNN,0          TEST 'NOT' TLXSTAT FILTER                    
         BE    UPDJOB38                                                         
         MVC   BCBYTE1,UPDADMNN                                                 
         NC    BCBYTE1,TLXSTAT                                                  
         BNZ   UPDJOB42                                                         
UPDJOB38 ICM   R1,15,UJPAOLD                                                    
         BZ    UPDJOB40                                                         
         AR    R1,RE                                                            
         SP    SCIADMN,0(8,R1)                                                  
         AP    UJTOTBEF,0(8,R1)                                                 
UPDJOB40 L     R1,UJPANEW                                                       
         AR    R1,RE                                                            
         AP    SCIADMN,0(8,R1)                                                  
         AP    UJTOTAFT,0(8,R1)                                                 
*                                                                               
UPDJOB42 TM    UPDINDS,UPDINJCB                                                 
         BO    UPDJOB50                                                         
         NI    UJINDS,FF-(UJIFR0+UJITO0)                                        
         CP    UJTOTBEF,BCPZERO    TEST ITEM WAS ZERO BEFORE                    
         BNE   *+8                                                              
         OI    UJINDS,UJIFR0                                                    
         CP    UJTOTAFT,BCPZERO    TEST ITEM IS ZERO KNOW                       
         BNE   *+8                                                              
         OI    UJINDS,UJITO0                                                    
*                                                                               
         TM    UJINDS,UJIFR0+UJITO0                                             
         BNM   UPDJOB50            TEST CHANGE IN ZERO STATUS                   
         ICM   RE,3,0(R4)          YES - ADJUST COUNTER                         
         TM    UJINDS,UJITO0                                                    
         BZ    UPDJOB44                                                         
         BCTR  RE,0                                                             
         STCM  RE,3,0(R4)                                                       
         B     UPDJOB46                                                         
UPDJOB44 LA    RE,1(RE)                                                         
         STCM  RE,3,0(R4)                                                       
         TM    UPDINDS,UPDIMAX     TEST MAXIMUM NUMBER OF ITEMS                 
         BZ    UPDJOB46                                                         
         CLC   UJMAXPEN,0(R4)                                                   
         BNL   UPDJOB46                                                         
         MVC   FVMSGNO,=AL2(AE$MXPEN)                                           
         B     UPDJOBN                                                          
*                                                                               
UPDJOB46 TM    UPDINDS,UPDITMS     TEST TMS LOCK TO BE UPDATED                  
         BZ    UPDJOB50                                                         
         XC    UJWORK,UJWORK                                                    
         PUSH  USING                                                            
         USING PTAELD,UJWORK                                                    
         ZAP   PTANET,UJTOTAFT                                                  
         GOTO1 AXFRTMS,UJPARM,(C'L',UJPATRN),PTAELD,0                           
         BNH   UPDJOB50                                                         
         MVC   FVMSGNO,=AL2(AE$NOTMS)                                           
         B     UPDJOBN                                                          
         POP   USING                                                            
*                                                                               
UPDJOB50 TM    UPDINDS,UPDINJCB    TEST NO JCBEL                                
         BO    UPDJOB58                                                         
         OC    0(2,R4),0(R4)       TEST ANYTHING PENDING                        
         BNZ   UPDJOB52                                                         
         TM    UPDINDS,UPDIDNTP    TEST DON'T TEST PENDING AMOUNTS              
         BO    UPDJOB58                                                         
         CP    SCIAMNT,BCPZERO                                                  
         BNE   UPDJOB52                                                         
         CP    SCIADMN,BCPZERO                                                  
         BE    UPDJOB58                                                         
UPDJOB52 OI    JCBINDS1,JCBIPEND                                                
         DROP  R3                                                               
*                                                                               
UPDJOB58 LA    R2,UPDTABL(R2)      BUMP R2 TO NEXT TABLE ENTRY                  
         CLI   UPDTABD,EOT         TEST E-O-T                                   
         BNE   UPDJOB12                                                         
*                                                                               
         L     R3,UJPAJOB          GET AFC ELEMENT                              
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         USING AFCELD,R3                                                        
         XR    RF,RF                                                            
UPDJOB62 CLI   AFCEL,0                                                          
         BE    UPDJOB64                                                         
         CLI   AFCEL,AFCELQ                                                     
         BE    UPDJOB66                                                         
         IC    RF,AFCLN                                                         
         BXH   R3,RF,UPDJOB62                                                   
UPDJOB64 LA    R3,UJWORK           ELEMENT NOT ON FILE - ADD IT                 
         XC    AFCELD(AFCLNQ),AFCELD                                            
         MVI   AFCEL,AFCELQ                                                     
         MVI   AFCLN,AFCLNQ                                                     
         ZAP   AFCRATE,BCPZERO                                                  
         GOTO1 VHELLO,UJPARM,(C'P',ACCMST),UJPAJOB,AFCELD                       
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,16(R1)                                                        
*                                                                               
UPDJOB66 TM    JCBINDS1,JCBIPEND   TEST ANYTHING PENDING                        
         BZ    UPDJOB68                                                         
         MVC   AFCCURR,CSBILCUR    YES - SET CURRENCY/EXCHANGE RATE             
         MVC   AFCX,CSEXCVAL                                                    
         MVC   JCBXTYP,CSTYPEXC                                                 
         MVC   JCBXDAT,CSDATEXC                                                 
         B     UPDJOB70                                                         
UPDJOB68 XC    AFCCURR,AFCCURR     NO - CLEAR CURRENCY/EXCHANGE RATE            
         XC    AFCX,AFCX                                                        
         MVI   JCBXTYP,0                                                        
         XC    JCBXDAT,JCBXDAT                                                  
         DROP  R3                                                               
*                                                                               
UPDJOB70 ICM   RE,3,JCBSEQ         UPDATE ACTIVITY SEQUENCE NUMBER              
         LA    RE,1(RE)                                                         
         STCM  RE,3,JCBSEQ                                                      
         MVC   JCBPERS,CUPASS      UPDATE PERSON UPDATING                       
*                                                                               
         L     R3,UJPAJOB          SEARCH FOR EXISTING JCBEL                    
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         XR    RF,RF                                                            
UPDJOB72 CLI   0(R3),0                                                          
         BE    UPDJOB74                                                         
         CLI   0(R3),JCBELQ                                                     
         BE    *+12                                                             
         IC    RF,1(R3)                                                         
         BXH   R3,RF,UPDJOB72                                                   
         MVC   0(JCBLNQ,R3),JCBELD COPY IN NEW ELEMENT                          
         B     UPDJOB76                                                         
*                                                                               
UPDJOB74 GOTO1 VHELLO,UJPARM,(C'P',ACCMST),UJPAJOB,JCBELD                       
         CLI   12(R1),0            ADD NEW ELEMENT                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,16(R1)                                                        
*                                                                               
UPDJOB76 L     R1,UJAR1            RETURN CALLER A(JCB ELEMENT)                 
         ST    R3,0(R1)                                                         
*                                                                               
         CLI   UJPACT,UJPAUPD      TEST ACTION UPDATE OR CLEAR                  
         BNE   UPDJOBX                                                          
         GOTO1 ASUBTOT,UJPARM,(C'U',UJPAOLD),UJPANEW                            
*                                                                               
UPDJOBX  B     ROUTE                                                            
*                                                                               
UPDJOBN  LA    RE,UJSAVJOB         RESTORE JOB RECORD AS IT WAS                 
         XR    RF,RF                                                            
         ICM   RF,3,ACTRLEN-ACTRECD(RE)                                         
         L     R0,UJPAJOB                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         L     R1,UJAR1            RETURN CALLER SCIEL ERROR TYPE               
         XC    0(4,R1),0(R1)                                                    
         MVC   0(1,R1),UPDTYPE                                                  
         B     ROUTL                                                            
*                                                                               
UJMAXPEN DC    AL2(100)            MAXIMUM NUMBER OF PENDING ITEMS              
         SPACE 1                                                                
***********************************************************************         
* DISPLAY JOB DETAILS ON FOOTLINES                                    *         
***********************************************************************         
         SPACE 1                                                                
UPDDIS   L     R3,UJPAFOOT         R3=A(FIRST FOOTLINE)                         
         USING FOOTLIND,R3                                                      
         OI    FOOTLINH+FHOID,FHOITR                                            
         MVC   FOOTLIN,BCSPACES                                                 
         LA    R3,FOOTLIN                                                       
         DROP  R3                                                               
         L     R4,UJPALST          R4=A(LIST OF SCIEL TYPES)                    
         LA    R4,0(R4)                                                         
UDIS02   CLI   0(R4),EOT           TEST END OF LIST                             
         BE    UDIS20                                                           
         LA    R2,UPDTAB                                                        
         USING UPDTABD,R2          LOCATE UPDATE TABLE ENTRY                    
UDIS04   CLC   UPDTYPE,0(R4)                                                    
         BE    UDIS10                                                           
         LA    R2,UPDTABL(R2)                                                   
         CLI   UPDTABD,EOT                                                      
         BNE   UDIS04                                                           
         B     UDIS18                                                           
*                                                                               
UDIS10   ZAP   UJDUB1,BCPZERO                                                   
         ZAP   UJDUB2,BCPZERO                                                   
         L     R1,UJPAJOB                                                       
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING SCIELD,R1           LOCATE SCIEL ELEMENT                         
         XR    RF,RF                                                            
UDIS12   CLI   SCIEL,0                                                          
         BE    UDIS14                                                           
         CLI   SCIEL,SCIELQ                                                     
         BNE   *+14                                                             
         CLC   SCITYPE,UPDTYPE                                                  
         BE    *+12                                                             
         IC    RF,SCILN                                                         
         BXH   R1,RF,UDIS12                                                     
         ZAP   UJDUB1,SCIAMNT                                                   
         ZAP   UJDUB2,SCIADMN                                                   
         DROP  R1                                                               
*                                                                               
UDIS14   XR    RF,RF               SET UP UPDEDT PARMS FOR DESCRIPTION          
         ICM   RF,3,UPDDWRD1                                                    
         LA    R0,TWAD(RF)                                                      
         ICM   RF,3,UPDDWRD2                                                    
         BZ    *+8                                                              
         LA    RF,TWAD(RF)                                                      
         GOTO1 ,UJPARM,(UPDLWRD1,(R0)),(UPDLWRD2,(RF))                          
         TM    UPDINDS,UPDISUM     TEST SUM AMOUNTS                             
         BZ    *+10                                                             
         AP    UJDUB1,UJDUB2                                                    
         XR    RF,RF                                                            
         TM    UPDINDS,UPDIBOTH    TEST DISPLAY BOTH AMOUNTS                    
         BZ    *+8                                                              
         LA    RF,UJDUB2                                                        
         GOTO1 UPDEDT,(R1),,,UJDUB1,(RF)                                        
*                                                                               
UDIS18   LA    R4,1(R4)            BUMP TO NEXT SCIEL                           
         B     UDIS02                                                           
         DROP  R2                                                               
*                                                                               
UDIS20   L     R3,UJPAFOOT         R3=A(SECOND FOOTLINE)                        
         LA    R3,FOOTLINL(R3)                                                  
         USING FOOTLIND,R3                                                      
         OI    FOOTLINH+FHOID,FHOITR                                            
         MVC   FOOTLIN,BCSPACES                                                 
         LA    R3,FOOTLIN                                                       
         DROP  R3                                                               
*                                                                               
         ZAP   UJDUB1,BCPZERO                                                   
         ZAP   UJDUB2,BCPZERO                                                   
         L     R1,UJPAJOB                                                       
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         XR    RF,RF                                                            
UDIS22   CLI   0(R1),0                                                          
         BE    UDIS30                                                           
         USING ABLELD,R1                                                        
         CLI   ABLEL,ABLELQ                                                     
         BNE   UDIS24                                                           
         AP    UJDUB1,ABLDR        JOB TOTAL CHARGES                            
         AP    UJDUB2,ABLCR        JOB TOTAL BILLING                            
         B     UDIS28                                                           
         USING SCIELD,R1                                                        
UDIS24   CLI   SCIEL,SCIELQ                                                     
         BNE   UDIS28                                                           
         CLI   SCITYPE,SCITT99S    TEST UNPOSTED BILLING                        
         BNE   UDIS28                                                           
         AP    UJDUB2,SCIAMNT      ADD TO JOB CREDITS                           
         DROP  R1                                                               
UDIS28   IC    RF,1(R1)                                                         
         BXH   R1,RF,UDIS22                                                     
*                                                                               
UDIS30   LH    R4,=Y(BSDICT-TWAD)  DISPLAY CHARGES/BILLED/BILLABLE              
         LA    R4,TWAD(R4)                                                      
         USING BSDICT,R4                                                        
         GOTO1 UPDEDT,UJPARM,(L'LC@CHGS,LC@CHGS),0,(1,UJDUB1),0                 
         GOTO1 (RF),(R1),(L'LC@BLD,LC@BLD),0,(1,UJDUB2),0                       
         SP    UJDUB1,UJDUB2                                                    
*&&UK*&& GOTO1 (RF),(R1),(L'LC@JOBB,LC@JOBB),0,(1,UJDUB1),0                     
*&&US*&& GOTO1 (RF),(R1),(L'LC@BLB,LC@BLB),0,(1,UJDUB1),0                       
         DROP  R4                                                               
*                                                                               
UPDDISX  B     ROUTE                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO EDIT DESCRIPTION = AMOUNT (SEE UEPARMS)                  *         
*                                                                     *         
* NTRY: R3 = A(OUPTUT POSITION)                                       *         
* EXIT: R3 = A(NEXT OUTPUT POSITION)                                  *         
***********************************************************************         
         SPACE 1                                                                
UPDEDT   NTR1  ,                                                                
         MVC   UEPARMS,0(R1)                                                    
*                                                                               
         XR    RF,RF               COPY WORD 1                                  
         IC    RF,UEPLWRD1                                                      
         BCTR  RF,0                                                             
         XR    RE,RE                                                            
         ICM   RE,7,UEPAWRD1                                                    
         EX    RF,*+4                                                           
         MVC   0(0,R3),0(RE)                                                    
         AR    R3,RF                                                            
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         ICM   RF,1,UEPLWRD2       COPY WORD 2                                  
         BZ    UEDT02                                                           
         BCTR  RF,0                                                             
         XR    RE,RE                                                            
         ICM   RE,7,UEPAWRD2                                                    
         EX    RF,*+4                                                           
         MVC   2(0,R3),0(RE)                                                    
         LA    R3,2(R3,RF)                                                      
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
*                                                                               
UEDT02   MVC   1(1,R3),BCEQUAL     OUTPUT '=' AMOUNT 1                          
         LA    R3,2(R3)                                                         
         GOTO1 UPDFMT,UJPARM,UEPAAMT1                                           
         ICM   R0,15,UEPAAMT2      OUTPUT '/' AMOUNT 2                          
         BZ    UPDEDTX                                                          
         MVC   0(1,R3),BCSLASH                                                  
         LA    R3,1(R3)                                                         
         GOTO1 (RF),(R1),(R0)                                                   
*                                                                               
UPDEDTX  LA    R3,1(R3)                                                         
         XIT1  REGS=(R3)                                                        
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO FORMAT AMOUNT                                            *         
*                                                                     *         
* NTRY: P1 BYTE 0 = NON-ZERO TO CONVERT IF NOT AGENCY CURRENCY        *         
*             1-3 = A(PL8 NUMBER)                                     *         
*       R3        = A(OUPTUT POSITION)                                *         
* EXIT: R3        = A(NEXT OUTPUT POSITION)                           *         
***********************************************************************         
         SPACE 1                                                                
UPDFMT   NTR1  ,                                                                
         XR    RF,RF                                                            
         ICM   RF,7,1(R1)                                                       
         ZAP   UFDUB,0(8,RF)                                                    
         CLI   0(R1),0                                                          
         BE    UFMT02                                                           
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    UFMT02                                                           
         EXCHP UFDUB,CSEXCVAL,DUB=UFDUB,WRK=UJWORK                              
*                                                                               
UFMT02   CURED UFDUB,(14,0(R3)),CSCURBIL,ALIGN=LEFT,ZERO=NOBLANK,      *        
               MINUS=YES,DMCB=UJPARM                                            
         AR    R3,R0                                                            
*                                                                               
UPDFMTX  XIT1  REGS=(R3)                                                        
         SPACE 1                                                                
         POP   USING                                                            
         SPACE 1                                                                
***********************************************************************         
* UPDJOB LOCAL W/S                                                    *         
***********************************************************************         
         SPACE 1                                                                
UJWORKD  DSECT                                                                  
*                                                                               
UJPARMS  DS    0XL16                                                            
UJPACT   DS    0XL1                ACTION                                       
UJPAUPD  EQU   C'U'                UPDATE JOB FOR ITEM                          
UJPACLR  EQU   C'C'                CLEAR ALLOCAION ON JOB                       
UJPADIS  EQU   C'D'                DISPLAY JOB DETAILS                          
UJPALST  DS    0A                  A(SCIEL TYPE LIST (DISPLAY))                 
UJPATRN  DS    A                   A(TRANSACTION RECORD FOR ITEM)               
         ORG   *-1                                                              
UJPCTYPE DS    XL1                 SCITYPE (FOR ACTION CLEAR)                   
UJPAJOB  DS    A                   A(JOB RECORD)                                
UJPAFOOT DS    0A                  A(FOOTLINE (DISPLAY))                        
UJPAOLD  DS    A                   A(OLD PRO-RATA BLOCK)                        
UJPANEW  DS    A                   A(NEW PRO-RATA BLOCK)                        
         ORG   UJPARMS+L'UJPARMS                                                
UJAR1    DS    A                   A(CALLER'S R1)                               
UJDUB1   DS    D                                                                
UJDUB2   DS    D                                                                
*                                                                               
UJPARM   DS    6A                                                               
UJJCBEL  DS    XL(JCBLNQ)          COPY OF JCBELD                               
*                                                                               
UJINDS   DS    XL1                 INDICATOR BYTE                               
UJITO0   EQU   X'80'               CHANGED TO ZERO                              
UJIFR0   EQU   X'40'               CHANGED FROM ZERO                            
UJTOTBEF DS    PL8                 TOTAL BEFORE UPDATE                          
UJTOTAFT DS    PL8                 TOTAL AFTER UPDATE                           
UJWORK   DS    CL80                                                             
*                                                                               
         DS    0A                                                               
UEPARMS  DS    0XL16               UPDFMT INPUT PARMS                           
UEPLWRD1 DS    AL1                 L(WORD 1)                                    
UEPAWRD1 DS    AL3                 A(WORD 1)                                    
UEPLWRD2 DS    AL1                 L(WORD 2) OR 0                               
UEPAWRD2 DS    AL3                 A(WORD 2)                                    
UEPAAMT1 DS    A                   A(PL8 AMOUNT 1)                              
UEPAAMT2 DS    A                   A(PL8 AMOUNT 2)                              
         ORG   UEPARMS+L'UEPARMS                                                
*                                                                               
UFDUB    DS    D                                                                
*                                                                               
UJSAVJOB DS    XL2048              SAVED JOB RECORD                             
         DS    0X                                                               
UJWORKL  EQU   *-UJWORKD                                                        
         SPACE 1                                                                
***********************************************************************         
* UPDATE TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
UPDTABD  DSECT                                                                  
UPDTYPE  DS    XL1                 SCIEL TYPE FOR AMOUNTS                       
UPDINDS  DS    XL1                 INDICATOR BYTE                               
UPDITMS  EQU   X'80'               LOCK/UNLOCK TMS                              
UPDIDNTP EQU   X'40'               DO NOT TEST PENDING AMOUNTS                  
UPDIMAX  EQU   X'20'               MAXIMUM NUMBER OF PENDING ITEMS              
UPDISUM  EQU   X'10'               DISPLAY SUM OF AMOUNTS                       
UPDIBOTH EQU   X'08'               DISPLAY BOTH AMOUNTS                         
UPDINJCB EQU   X'04'               NO JCBELD COUNTER                            
UPDIAST  EQU   X'02'               REDUCE ASTDRAFT WHEN CLEARING                
UPDAMNTS DS    XL1                 TLXSTAT FILTER FOR SCIAMNT                   
UPDADMNS DS    XL1                 TLXSTAT FILTER FOR SCIADMN                   
UPDAMNTN DS    XL1                 'NOT' TLXSTAT FILTER FOR SCIAMNT             
UPDADMNN DS    XL1                 'NOT' TLXSTAT FILTER FOR SCIADMN             
UPDAAMNT DS    AL2                 AGENCY CURRENCY DISP. TO SCIAMNT             
UPDFAMNT DS    AL2                 FOREIGN CURRENCY DISP. TO SCIAMNT            
UPDAADMN DS    AL2                 AGENCY CURRENCY DISP. TO SCIADMN             
UPDFADMN DS    AL2                 FOREIGN CURRENCT DISP. TO SCIADMN            
UPDJCB   DS    AL2                 DISP. TO JCBELD COUNTER                      
UPDLWRD1 DS    AL1                 LENGTH OF DESCRIPTION WORD 1                 
UPDDWRD1 DS    AL2                 DISP. TO DESCRIPTION WORD 1                  
UPDLWRD2 DS    AL1                 LENGTH OF DESCRIPTION WORD 2                 
UPDDWRD2 DS    AL2                 DISP. TO DESCRIPTION WORD 2                  
UPDTABL  EQU   *-UPDTABD                                                        
         SPACE 1                                                                
         PUSH  USING                                                            
         USING PRORATAD,0                                                       
         USING JCBELD,PRORATAD                                                  
CLB41    CSECT                                                                  
UPDTAB   DS    0H                                                               
*                                                                               
         DC    AL1(SCITCBAP,UPDIBOTH)                                           
         DC    AL1(0,0,TLXSFEEA,TLXSFEEA)                                       
         DC    S(PP$AALLO,PP$FALLO,PP$ACOMM,PP$FCOMM,JCBALL)                    
         DC    AL1(L'LC@ALCTD),AL2(LC@ALCTD-TWAD)                               
         DC    AL1(0),AL2(0)                                                    
*                                                                               
         DC    AL1(SCITCBAP,UPDINJCB)                                           
         DC    AL1(0,0,TLXSFEEA,TLXSFEEA)                                       
         DC    S(0,0,PP$WUAMT,PP$WUAMT,0)                                       
         DC    AL1(0),AL2(0)                                                    
         DC    AL1(0),AL2(0)                                                    
*                                                                               
         DC    AL1(SCITCBWP,UPDITMS+UPDIMAX+UPDISUM)                            
         DC    AL1(TLXSHOUR,TLXSCOST,0,0)                                       
         DC    S(PP$AWOFF,PP$FWOFF,PP$AWOFF,PP$FWOFF,JCBWOF)                    
         DC    AL1(L'LC@DRAFT),AL2(LC@DRAFT-TWAD)                               
         DC    AL1(L'LC@WRTFS),AL2(LC@WRTFS-TWAD)                               
*                                                                               
         DC    AL1(SCITCBRP,UPDITMS+UPDIMAX+UPDISUM)                            
         DC    AL1(TLXSHOUR,TLXSCOST,0,0)                                       
         DC    S(PP$AWOFR,PP$FWOFR,PP$AWOFR,PP$FWOFR,JCBRCV)                    
         DC    AL1(L'LC@DRAFT),AL2(LC@DRAFT-TWAD)                               
*&&US*&& DC    AL1(L'LC@RCVR),AL2(LC@RCVR-TWAD)                                 
*&&UK*&& DC    AL1(L'LC@WRTB),AL2(LC@WRTB-TWAD)                                 
*                                                                               
         DC    AL1(SCITCBTP,UPDITMS+UPDIMAX)                                    
         DC    AL1(0,0,0,0)                                                     
         DC    S(PP$AXFER,PP$FXFER,0,0,JCBXFR)                                  
         DC    AL1(L'LC@DRAFT),AL2(LC@DRAFT-TWAD)                               
         DC    AL1(L'LC@XFR),AL2(LC@XFR-TWAD)                                   
*                                                                               
         DC    AL1(SCITCBIP,UPDIAST)                                            
         DC    AL1(TLXSFEEA,0,0,0)                                              
         DC    S(PA$NET,PA$NET,0,0,JCBFEE)                                      
         DC    AL1(L'LC@DRAFT),AL2(LC@DRAFT-TWAD)                               
         DC    AL1(L'LC@FEE),AL2(LC@FEE-TWAD)                                   
*                                                                               
UPDTABX  DC    AL1(EOT)                                                         
         DS    0H                                                               
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INTERFACE WITH TSAR                                      *         
*                                                                     *         
* NTRY - R1 = TSAR ACTION VALUE (TSAR INITIALISED IF NECESSARY)       *         
*     OR R1 = A(TSAR ACTION VALUE, A(RECORD))                         *         
*        X'80' BIT OF TSAR ACTION IS OFF                              *         
*           = RETURN ERROR IF FILE FULL ON ADD                        *         
* EXIT - CC = EQUAL IF OK                                             *         
*        CC = LOW IF END-OF-FILE REACHED                              *         
*        CC = HIGH IF RECORD NOT FOUND FOR READ HIGH                  *         
***********************************************************************         
         SPACE 1                                                                
         USING TIWORKD,RC                                                       
TSARIO   DS    0H                                                               
         USING *,R8                                                             
         ST    R1,TIPARM           SAVE ACTION OR A(PARAMETER)                  
         MVC   TITSACTN,TIPARM+3   SAVE CALLER'S ACTION                         
         LA    R2,LSTLST                                                        
         OC    TIPARM(3),TIPARM    TEST ACTION,A(RECORD) PASSED                 
         BZ    *+14                                                             
         MVC   TITSACTN,0(R1)                                                   
         ICM   R2,7,1(R1)                                                       
TI       USING TLSTD,R2            R2=A(CURRENT LIST ENTRY)                     
         TM    TITSACTN,TITSFULL   TEST RETURN ON FULL FILE                     
         BO    *+12                                                             
         OI    TITSACTN,TITSFULL                                                
         OI    TIINDS1,TIIRFULL                                                 
*                                                                               
         L     R3,ATSABLK                                                       
         USING TSARD,R3            R3=A(TSAR BLOCK)                             
         LA    R0,TI.TLREC                                                      
         ST    R0,TSAREC           SET A(RECORD)                                
*                                                                               
         TM    BCTSINDS,BCTSIRES   TEST ALREADY RESTORED                        
         BNZ   TSARIO04                                                         
         MVC   TSACOM,ACOM         SET A(COMFACS)                               
         MVI   TSKEYL,L'TLKEY      SET KEY LENGTH                               
         MVI   TSRECI,TSRVAR       SET VARIABLE LENGTH                          
         MVC   TSRECL,=Y(TLMAXLNQ) SET MAXIMUM RECORD LENGTH                    
         MVI   TSPAGN,TSPEXPN      SET NUMBER OF TEMPEST PAGES                  
         MVI   TSACTN,TSAINI       SET INITIALISE                               
         MVI   TSINDS,TSIALLOC     SET TO ALLOCATE FROM TEMPEST                 
         CLI   TITSACTN,TSASAV     TEST SAVE                                    
         BE    TSARIOX                                                          
         TM    BCTSINDS,BCTSIINI   TEST TEMPEST BUFFER INITIALISED              
         BZ    TSARIO02                                                         
         MVI   TSACTN,TSARES       SET RESTORE                                  
         MVC   TSPAGL,BCTSLOWP     SET LOW PAGE NUMBER                          
         MVC   TSPAGN,BCTSNUMP     SET NUMBER OF PAGES ALLOCATED                
*                                                                               
TSARIO02 GOTO1 VTSAR,TSARD         CALL TO INITIALISE/RESTORE                   
         BNE   TSARIOAB            ABEND                                        
         MVC   BCTSLOWP,TSPAGL     SAVE LOW TSAR PAGE NUMBER                    
         MVC   BCTSNUMP,TSPAGN     SAVE NUMBER OF PAGES ALLOCATED               
         OI    BCTSINDS,BCTSIINI+BCTSIRES                                       
*                                                                               
TSARIO04 MVC   TSACTN,TITSACTN     SET ACTION NUMBER                            
         CLI   TSACTN,TSAINI       TEST EXPLICIT INITIALISE                     
         BE    TSARIOX                                                          
         CLI   TSACTN,TSARES       TEST EXPLICIT RESTORE                        
         BE    TSARIOX                                                          
         CLI   TSACTN,TSASAV       TEST SAVE                                    
         BNE   TSARIO06                                                         
         NI    BCTSINDS,FF-BCTSIRES                                             
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TSARIO06 MVC   TSRNUM,TI.TLNUM                                                  
         GOTO1 VTSAR,TSARD                                                      
         MVC   TI.TLNUM,TSRNUM     SET RECORD LIST NUMBER                       
         BE    TSARIO10                                                         
         CLI   TSACTN,TSARDH       TEST READ-HIGH/NEXT                          
         BE    *+12                                                             
         CLI   TSACTN,TSANXT                                                    
         BNE   TSARIO08                                                         
         TM    TSERRS,TSEEOF       RETURN CC=LOW FOR END-OF-FILE ERROR          
         BO    ROUTL                                                            
         TM    TSERRS,TSERNF       RETURN CC=HIGH IF RECORD NOT FOUND           
         BO    ROUTH                                                            
         DC    H'0'                                                             
*                                                                               
TSARIO08 CLI   TSACTN,TSAADD       TEST ADDING                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TSERRS,TSEEOF       TEST END-OF-FILE ERROR                       
         BO    *+6                                                              
         DC    H'0'                                                             
         TM    TIINDS1,TIIRFULL    YES - TEST WANT TO RETURN WITH THAT          
         BO    ROUTL                                                            
         DC    H'0'                                                             
*                                                                               
TSARIO10 CLI   TSACTN,TSAADD       TEST ADDING                                  
         BNE   TSARIO14                                                         
         TM    TI.TLKSES,X'F0'     TEST FOR NTRSES LEVEL                        
         BNZ   TSARIO12                                                         
         ICM   RE,3,BCTSHIGH       YES - INCREMENT GLOBAL HIGH RECORD #         
         LA    RE,1(RE)                                                         
         STCM  RE,3,BCTSHIGH                                                    
         MVC   CSHIRECN,BCTSHIGH                                                
         B     TSARIOX                                                          
TSARIO12 CLI   TI.TLKSES,TLKSTEMP  TEST ADDING TEMPORARY RECORD                 
         BL    TSARIOX                                                          
         OI    LSINDS1,LSITEMP     YES - SET FLAG SO GETS DELETED LATER         
         B     TSARIOX                                                          
*                                                                               
TSARIO14 CLI   TSACTN,TSADEL       TEST DELETING                                
         BNE   TSARIOX                                                          
         TM    TI.TLKSES,X'F0'     TEST FOR NTRSES LEVEL                        
         BNZ   TSARIOX                                                          
         ICM   RE,3,BCTSHIGH       YES - DECREMENT GLOBAL HIGH RECORD #         
         BCTR  RE,0                                                             
         STCM  RE,3,BCTSHIGH                                                    
         CLC   CSHIRECN,BCTSHIGH                                                
         BNH   TSARIOX                                                          
         MVC   CSHIRECN,BCTSHIGH                                                
*                                                                               
TSARIOX  B     ROUTE                                                            
*                                                                               
TSARIOAB LA    R1,BASACTH          ABEND IF INITIALISE/RESTORE FAILS            
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$ISUTS)                                           
         OI    CSINDSG1,CSINDUNW   SET TO UNWIND VIA $ABEND                     
         L     RD,BCSVRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                  EXIT TO ROOT CALL POINT                      
*                                                                               
         DROP  R3,RC,TI                                                         
         SPACE 1                                                                
TIWORKD  DSECT                     ** TSARIO LOCAL W/S **                       
TIPARM   DS    A                   CALLING PARAMETER                            
TITSACTN DS    XL1                 SAVED TSAR ACTION NUMBER                     
TITSFULL EQU   X'80'               DON'T RETURN ERROR IF BUFFER FULL            
TIINDS1  DS    XL1                 INDICATOR BYTE                               
TIIRFULL EQU   X'80'               RETURN ON ON TSERRS,TSEEOF ERROR             
TIWORKL  EQU   *-TIWORKD                                                        
CLB41    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EDIT OUT AGENCY/FOREIGN CURRENCY AMOUNT FOR COLUMN       *         
*                                                                     *         
* NTRY: P1 = A(PL8 AGENCY CURRENCY AMOUNT)                            *         
*       P2 = A(PL8 FOREIGN CURRENCY AMOUNT) OR 0 IF NOT CURR.         *         
***********************************************************************         
         SPACE 1                                                                
         USING EAWORKD,RC                                                       
EDTAMT   DS    0H                                                               
         USING *,R8                                                             
         LM    R2,R3,0(R1)                                                      
         LTR   R3,R3               TEST CURRENCY                                
         BNZ   EDTAMT02                                                         
         L     R1,ACLMDATA                                                      
         XR    RF,RF                                                            
         IC    RF,CLMFWDTH-CLMTABD(R1)                                          
         CURED (P8,(R2)),((RF),FVIFLD),2,MINUS=YES,ZERO=NOBLANK,       *        
               DMCB=EADMCB                                                      
         ORG   *-2                                                              
         L     RE,ALINNTRY                                                      
         TM    LININDS-LINTABD(RE),LINIINP                                      
         BZ    *+8                                                              
         OI    0(R1),X'20'         ALIGN=LEFT IF INPUT COLUMN                   
         BASR  RE,RF                                                            
         B     ROUTX                                                            
*                                                                               
EDTAMT02 CLC   CSCPYCUR,CSBILCUR   TEST FOREIGN CURRENCY                        
         BE    *+6                                                              
         LR    R2,R3                                                            
         L     R1,ACLMDATA                                                      
         XR    RF,RF                                                            
         IC    RF,CLMFWDTH-CLMTABD(R1)                                          
         CURED (P8,(R2)),((RF),FVIFLD),CSCURBIL,MINUS=YES,ZERO=NOBLANK,*        
               DMCB=EADMCB                                                      
         ORG   *-2                                                              
         L     RE,ALINNTRY                                                      
         TM    LININDS-LINTABD(RE),LINIINP                                      
         BZ    *+8                                                              
         OI    0(R1),X'20'         ALIGN=LEFT IF INPUT COLUMN                   
         BASR  RE,RF                                                            
         B     ROUTX                                                            
         DROP  RC                                                               
         SPACE 1                                                                
EAWORKD  DSECT                     ** EDTAMT LOCAL W/S **                       
EADMCB   DS    6A                                                               
EAWORKL  EQU   *-EAWORKD                                                        
CLB41    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SAVE/RESTORE OVERLAY W/S ON TSAR RECORDS                 *         
*                                                                     *         
* NTRY: P1 BYTE 0 = C'S' TO SAVE OVERLAY W/S                          *         
*                   C'R' TO RESTORE OVERLAY W/S                       *         
*             1-3 = L(OVERLAY W/S)                                    *         
*       P2        = A(OVERLAY W/S)                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING SOWORKD,RC                                                       
SAVOWS   DS    0H                                                               
         USING *,R8                                                             
         MVC   SOPARMS,0(R1)                                                    
         L     R2,SOPAWS           R2=A(W/S)                                    
         LH    R3,SOPLWS           R3=L(W/S)                                    
*                                                                               
K        USING TLKEY,SOKEY                                                      
R        USING TLSTD,SOTLST                                                     
*                                                                               
         CLI   SOPACT,SOPARES      TEST RESTORING                               
         BE    RESOWS                                                           
         CLI   SOPACT,SOPASAV      TEST SAVING                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    K.TLKEY,K.TLKEY                                                  
         MVC   K.TLKSES,TWASESNL                                                
         OI    K.TLKSES,TLKSWS                                                  
*                                                                               
SOWS02   LA    R4,512              R4=L'DATA TO BE SAVED THIS TIME              
         CR    R4,R3                                                            
         BNH   *+6                                                              
         LR    R4,R3                                                            
*                                                                               
         MVC   R.TLKEY,K.TLKEY     TEST TSAR RECORD ALREADY SAVED               
         MVI   SOBYTE,TSAPUT                                                    
         GOTO1 ATSARIO,SOPARM,('TSARDH',R.TLSTD)                                
         BE    *+14                                                             
         MVI   SOBYTE,TSAADD                                                    
         MVC   R.TLKEY,K.TLKEY                                                  
*                                                                               
         LR    RE,R2               COPY DATA                                    
         LR    RF,R4                                                            
         LA    R0,R.TLDATA                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         LA    RF,L'TLRLEN+L'TLKEY(R4)                                          
         STH   RF,R.TLRLEN                                                      
         GOTO1 ATSARIO,SOPARM,(SOBYTE,R.TLSTD)                                  
*                                                                               
         IC    RE,K.TLKSRT                                                      
         LA    RE,1(RE)                                                         
         STC   RE,K.TLKSRT                                                      
*                                                                               
         AR    R2,R4               BUMP R4 ALONG                                
         SR    R3,R4               BUMP LENGTH LEFT DOWN                        
         BP    SOWS02                                                           
*                                                                               
SAVOWSX  B     ROUTX                                                            
         SPACE 1                                                                
RESOWS   XC    R.TLKEY,R.TLKEY                                                  
         MVC   R.TLKSES,TWASESNL                                                
         OI    R.TLKSES,TLKSWS                                                  
*                                                                               
ROWS02   LA    R4,512              R4=L'DATA TO BE RESTORED THIS TIME           
         CR    R4,R3                                                            
         BNH   *+6                                                              
         LR    R4,R3                                                            
*                                                                               
         GOTO1 ATSARIO,SOPARM,('TSARDH',R.TLSTD)                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    RE,R2               COPY DATA                                    
         LR    RF,R4                                                            
         LA    R0,R.TLDATA                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         IC    RE,R.TLKSRT                                                      
         LA    RE,1(RE)                                                         
         STC   RE,R.TLKSRT                                                      
*                                                                               
         AR    R2,R4               BUMP R2 ALONG                                
         SR    R3,R4               BUMP LENGTH LEFT DOWN                        
         BP    ROWS02                                                           
*                                                                               
RESOWSX  B     ROUTX                                                            
         SPACE 1                                                                
         DROP  K,R,RC                                                           
         SPACE 1                                                                
SOWORKD  DSECT                                                                  
SOPARMS  DS    0XL8                                                             
SOPACT   DS    XL1                 ACTION CODE                                  
SOPASAV  EQU   C'S'                SAVE                                         
SOPARES  EQU   C'R'                RESTORE                                      
         DS    XL1                 N/D                                          
SOPLWS   DS    H                   L(W/S)                                       
SOPAWS   DS    X                   A(W/S)                                       
SOPARM   DS    6A                                                               
*                                                                               
SOBYTE   DS    XL1                                                              
*                                                                               
SOKEY    DS    XL(L'TLKEY)                                                      
SOTLST   DS    XL(L'TLST)                                                       
SOWORKL  EQU   *-SOWORKD                                                        
CLB41    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE BATCH INPUT DETAILS                             *         
*                                                                     *         
* NTRY: P1 BYTE 0 = C'V' TO VALIDATE FIELDS ONLY                      *         
*                   C'D' IF DRAFT                                     *         
*                   C'U' TO UPDATE                                    *         
*               3 = PBTYPES EQUATE                                    *         
*       P2        = A(BATCH REF# FIELD HEADER)                        *         
*       P3        = A(BATCH MONTH FIELD HEADER)                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING VBWORKD,RC                                                       
         USING PBPARMS,VBPBPARM                                                 
VALBAT   DS    0H                                                               
         USING *,R8                                                             
         MVC   VBPARMS,0(R1)                                                    
*                                                                               
         L     R7,ATYPLST                                                       
         USING TYPLSTD,R7                                                       
VBAT02   CLC   TYPTYPES,VBTYPES    FIND TABLE ENTRY FOR EQUATE                  
         BE    VBAT04                                                           
         LA    R7,TYPLSTL(R7)                                                   
         CLI   TYPLSTD,EOT                                                      
         BNE   VBAT02                                                           
         DC    H'0'                                                             
*                                                                               
VBAT04   MVC   PBTYPES,TYPTYPES                                                 
         XR    RF,RF                                                            
         ICM   RF,3,TYPPBLK                                                     
         LA    RF,PBPARMS(RF)                                                   
         USING PBBATCHD,RF                                                      
         MVC   PBAREFH,VBPAREFH                                                 
         MVC   PBAMOAH,VBPAMOAH                                                 
         DROP  RF                                                               
         L     R3,VBPAREFH                                                      
         USING REFFLDD,R3          R3=A(REFERENCE FIELD)                        
         L     R4,VBPAMOAH                                                      
         USING MOAFLDD,R4          R4=A(MOA FIELD)                              
*                                                                               
         CLI   VBPACT,C'V'                                                      
         BE    VBAT10                                                           
         CLI   VBPACT,C'D'                                                      
         BE    VBAT20                                                           
         CLI   VBPACT,C'U'                                                      
         BE    VBAT30                                                           
         DC    H'0'                                                             
*                                                                               
VBAT10   DS    0H                  VALIDATE FIELDS ONLY                         
*                                                                               
         TM    REFFLDH+FHIID,FHIIVA TEST BOTH FIELDS ALREADY VALIDATED          
         BZ    *+12                                                             
         TM    MOAFLDH+FHIID,FHIIVA                                             
         BO    ROUTE                                                            
         OI    BCINDS2,BCIHLDLP    HOLD CURRENT LIST PAGE                       
*                                                                               
         CLI   REFFLDH+FHILD,0     TEST FIELDS ARE EMPTY                        
         BNE   *+12                                                             
         CLI   MOAFLDH+FHILD,0                                                  
         BE    VBATVAL                                                          
*                                                                               
         MVI   PBMODE,PBVALQ                                                    
         GOTO1 APOSTIT,BCPARM,PBPARMS                                           
         BNE   VBATINV                                                          
*                                                                               
VBATVAL  DS    0H                  RETURN WITH FIELDS VALID                     
         OI    REFFLDH+FHIID,FHIIVA                                             
         OI    MOAFLDH+FHIID,FHIIVA                                             
         B     ROUTE                                                            
*                                                                               
VBATINV  DS    0H                  RETURN WITH FIELDS INVALID                   
         NI    REFFLDH+FHIID,FF-FHIIVA                                          
         NI    MOAFLDH+FHIID,FF-FHIIVA                                          
         B     ROUTL                                                            
         SPACE 1                                                                
VBAT20   MVI   PBMODE,PBDRAFTQ     SET DRAFT MODE                               
         B     VBAT40                                                           
         SPACE 1                                                                
VBAT30   MVI   PBMODE,PBLIVEQ      SET LIVE MODE                                
         SPACE 1                                                                
VBAT40   DS    0H                                                               
         CLI   REFFLDH+FHILD,0     TEST FIELDS ARE EMPTY                        
         BNE   VBAT42                                                           
         CLI   MOAFLDH+FHILD,0                                                  
         BNE   VBAT42              YES - NTRSES TO DRAFT/UPDATE SCREEN          
         LA    R1,=AL1(RECBIL,ACTDRA,3,0,0,0)                                   
         CLI   PBMODE,PBLIVEQ                                                   
         BNE   *+8                                                              
         LA    R1,=AL1(RECBIL,ACTUPD,3,0,0,0)                                   
         GOTO1 ANTRSES                                                          
*                                                                               
VBAT42   TM    LSINDS1,LSILINUP    TEST 'OWT INPUT TO LIST                      
         BZ    VBAT44                                                           
         MVC   FVOMTYP,GTMINF      YES - DON'T DO ANYTHING                      
         MVC   FVMSGNO,=AL2(AI$ACTOK)   AS IT GETS CONFUSED                     
         B     ROUTE                                                            
*                                                                               
VBAT44   DS    0H                                                               
         MVC   PBTYPES,TYPTYPES                                                 
*        LA    RF,?????                                                         
*        ST    RF,PBATWAX                                                       
         GOTO1 APOSTIT,BCPARM,PBPARMS                                           
         B     ROUTX                                                            
         SPACE 1                                                                
         POP   USING                                                            
         SPACE 1                                                                
REFFLDD  DSECT                     ** REFERENCE FIELD **                        
REFFLDH  DS    XL8                                                              
REFFLD   DS    CL4                                                              
*                                                                               
MOAFLDD  DSECT                     ** MOA FIELD **                              
MOAFLDH  DS    XL8                                                              
MOAFLD   DS    CL6                                                              
*                                                                               
VBWORKD  DSECT                     ** VALBAT LOCAL W/S **                       
VBPARMS  DS    0XL12               INPUT PARAMETERS                             
VBPACT   DS    XL1                 ACTION                                       
         DS    AL2                                                              
VBTYPES  DS    XL1                 PBTYPES EQUATE                               
*                                                                               
VBPAREFH DS    A                   A(BATCH REF# FIELD HEADER)                   
VBPAMOAH DS    A                   A(MOA FIELD HEADER)                          
         ORG   VBPARMS+L'VBPARMS                                                
*                                                                               
VBPARM   DS    6A                                                               
*                                                                               
VBPBPARM DS    XL(L'PBPARMS)                                                    
*                                                                               
VBWORKL  EQU   *-VBWORKD                                                        
CLB41    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         ORG   LTORG                                                            
         LTORG                                                                  
         SPACE 1                                                                
DMREAD   DC    C'DMREAD  '                                                      
DMWRITE  DC    C'DMWRT   '                                                      
TEMPSTR  DC    C'TEMPSTR '                                                      
ACCMST   DC    C'ACCMST  '                                                      
ADDATEND DC    C'ADD=END'                                                       
TRNORDER DC    C'**'                                                            
PZERO    DC    PL1'0'                                                           
PL8ZERO  DC    PL8'0'                                                           
PMINUS1  DC    PL1'-1'                                                          
EXPUL    DC    C'SE'               EXPENSE UNIT/LEDGER                          
INCUL    DC    C'SI'               INCOME UNIT/LEDGER                           
INCSUSUL DC    C'SK'               INCOME SUSPENSE UNIT/LEDGER                  
         SPACE 1                                                                
         DS    (LTORGX-*)X                                                      
         ORG                                                                    
         SPACE 1                                                                
***********************************************************************         
* EQUATES                                                             *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
ALL      EQU   0                                                                
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDGETHELPD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGETHELPD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* GEGENCUR                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENCUR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* GEGENEXC                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENEXC                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACCATCALLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* ACCLBWORK                                                                     
       ++INCLUDE ACCLBWORK                                                      
* ACCLBCOLS                                                                     
       ++INCLUDE ACCLBCOLS                                                      
         SPACE 1                                                                
CLB41    CSECT                     ALLOW EASIER RE-LOAD OF PHASE                
         ORG   CLB41+(((*-CLB41)/512)+1)*512                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'081ACCLB41   08/16/00'                                      
         END                                                                    
