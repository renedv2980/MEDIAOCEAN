*          DATA SET ACCLB13    AT LEVEL 050 AS OF 08/16/00                      
*PHASE T62113A                                                                  
CLB13    TITLE '- BILL PROGRAM - AUTO ALLOCATE/UNALLOCATE - NEW VSN'            
CLB13    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB13**,R7,R6,RR=RE                                           
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
         B     EXITY               DISPLAY (SUB-) TOTAL ROUTINE                 
         B     EXITY               DISPLAY SCREEN TOTAL ROUTINE                 
         B     PFKRTN              PF KEY ROUTINES                              
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
* - SET OPTIONS (FOR ALLOCATE SCREEN) ON FIRST PASS                   *         
*   VALIDATE OPTIONS ON SUBSEQUENT PASSES                             *         
* - (PASSED JOB OPTIONS SET IN LSTFRST)                               *         
*   VALIDATE PASSED JOB OPTIONS ON FIRST PASS                         *         
*   VALIDATE ALL JOB OPTIONS ON SUBSEQUENT PASSES                     *         
***********************************************************************         
         SPACE 1                                                                
SCRFRST  MVI   LSPASS,C'Y'         SET TO PASS OPTIONS TO NEXT SESSION          
*                                                                               
         TM    BCINDS1,BCINACT     TEST FIRST TIME                              
         BO    *+12                                                             
         TM    BCINDS2,BCINTRS                                                  
         BZ    SFRST10                                                          
*                                                                               
         MVC   AOVEROUT,ALSVALS    SAVE DEFAULT OPTIONS                         
         GOTO1 AVALOPT,BOPARM,OPTTAB,(X'40',0),0                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVDEFOPS,LSOPS                                                   
         XC    LSOPS(LSOPL),LSOPS                                               
*                                                                               
         MVC   SVBILCUR,CSCPYCUR   USE COMPANY CURR AS DEFAULT                  
         MVC   SVCURBIL,CSCURCPY                                                
         OC    CSBILCUR,CSBILCUR   TEST BILLING CURRENCY                        
         BZ    *+16                                                             
         MVC   SVBILCUR,CSBILCUR                                                
         MVC   SVCURBIL,CSCURBIL                                                
         OC    CSEXCVAL,CSEXCVAL   TEST EXCHANGE RATE                           
         BZ    *+16                                                             
         MVC   SVEXCVAL,CSEXCVAL                                                
         MVC   SVEXCTYP,CSTYPEXC                                                
*                                                                               
         MVI   SVJOBMAX,FF         SET MIN/MAX JOB CODES                        
         MVC   SVJOBMAX+1(L'SVJOBMAX-1),SVJOBMAX                                
         XR    RF,RF                                                            
         CLC   BCCLICOD,BCSPACES                                                
         BNH   SFRST02                                                          
         MVC   SVJOBMIN,BCCLICOD                                                
         MVC   SVENTCLI,BCCLICOD                                                
         IC    RF,BCCLILEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SVJOBMAX(0),BCCLICOD                                             
         CLC   BCPROCOD,BCSPACES                                                
         BNH   SFRST02                                                          
         MVC   SVJOBMIN,BCPROCOD                                                
         MVC   SVENTPRO,BCPROCOD                                                
         IC    RF,BCPROLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SVJOBMAX(0),BCPROCOD                                             
         CLC   BCJOBCOD,BCSPACES                                                
         BNH   SFRST02                                                          
         MVC   SVJOBMIN,BCJOBCOD                                                
         MVC   SVENTJOB,BCJOBCOD                                                
         IC    RF,BCJOBLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SVJOBMAX(0),BCJOBCOD                                             
SFRST02  LA    RE,SVJOBMAX         ENSURE REST OF KEY SET TO FFS                
         LA    RF,L'SVJOBMAX-1(RE)                                              
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         MVI   0(RF),FF                                                         
         BCT   RF,*-12                                                          
*                                                                               
         MVC   LCLIOFFC,BCCLIOFF   START OFF WITH A DECENT OFFICE               
         MVC   LPROOFFC,BCPROOFF                                                
         MVC   LJOBOFFC,BCJOBOFF                                                
*                                                                               
         BAS   RE,DISJBO                                                        
*                                                                               
SFRST10  MVC   LLSOPS,LSOPS        SAVE CURRENT ALLOCATION OPTIONS              
         XC    LSOPS(LSOPL),LSOPS                                               
         L     RE,=A(VALOPT)       VALIDATE OPTIONS FOR ALLOCATE SCREEN         
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,ALSVALS                                                 
*                                                                               
         XC    BOWORK2,BOWORK2                                                  
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         OC    BOWORK2(L'GOCBDAAU),GOCBDAAU-GOBBLOCK(RF)                        
         BNZ   *+10                                                             
         MVC   BOWORK2(DEFCLML),DEFCLM                                          
         GOTO1 AVALOPT,BOPARM,OPTTAB,BOWORK2,0                                  
         BE    *+14                                                             
         MVC   LSOPS(LSOPL),LLSOPS RESTORE PREVIOUS OPTIONS                     
         B     EXITL                                                            
         NI    SVINDS,FF-SVIDEFOP                                               
         CLC   SVDEFOPS,LSOPS      TEST OPTIONS ARE DEFAULT                     
         BNE   *+8                                                              
         OI    SVINDS,SVIDEFOP                                                  
*                                                                               
         MVC   LSVOPTS,SVOPTS      SAVE CURRENT JOB OPTIONS                     
         XC    SVOPTS,SVOPTS                                                    
         TM    BCCPYST6,CPYSFBIL   TEST FOREIGN CURRENCY BILLING                
         BO    *+10                                                             
         MVC   SVBILCUR,CSCPYCUR   NO - KEEP CURRENCY CODE                      
         L     RE,=A(VALJBO)       VALIDATE JOB OPTIONS FOR THIS SCREEN         
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,AALJBOH                                                    
         LA    RE,OSVALS                                                        
         ST    RE,AOVEROUT                                                      
         NI    SVINDS,FF-(SVICINP+SVIXINP)                                      
         GOTO1 AVALOPT,BOPARM,JBOTAB,0,0                                        
         BE    *+14                                                             
         MVC   SVOPTS,LSVOPTS                                                   
         B     EXITL                                                            
*                                                                               
         BAS   RE,DISJBO           RE-DISPLAY VALID JOB OPTIONS                 
         CLC   LSVOPTS(SVOPTSL1),SVOPTS                                         
         BNE   EXITH                                                            
         CLC   LLSOPS,LSOPS        TEST CHANGE IN TRANSACTION OPTIONS           
         BE    EXITY                                                            
*                                                                               
         OI    SVINDS,SVIFLTD      SET EVERYTHING FILTERED                      
         MVC   TLNUM,CSPSRECN      UPDATE LIST                                  
SFRST12  ICM   RE,3,TLNUM                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,3,TLNUM                                                       
         CLC   TLNUM,CSHIRECN                                                   
         BH    SFRST20                                                          
         GOTO1 ATSARIO,TSAGET                                                   
         MVC   IODAOVER,TLDA                                                    
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 JOBSET,BOPARM,('LSETIREF',AIO1)                                  
         GOTO1 ATSARIO,TSAPUT                                                   
         B     SFRST12                                                          
*                                                                               
SFRST20  OI    BCINDS2,BCIHLDLP    HOLD CURRENT LIST PAGE                       
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST                                                      *         
* - BUILD KEY FOR FIRST RECORD TO BE READ FOR TSAR LIST               *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING ACTRECD,IOKEY                                                    
LSTFRST  OI    SVINDS,SVIFLTD      SET EVERYTHING FILTERED                      
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   ACTKACT,SVJOBMIN                                                 
         CLC   SVJOBMIN,C' '                                                    
         BH    EXITY                                                            
         MVI   ACTKACT,FF                                                       
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* GET FIRST/NEXT RECORD FOR TSAR LIST                                 *         
***********************************************************************         
         SPACE 1                                                                
GETNEXT  MVI   ACTKEY+L'ACTKCULA,FF   BUMP TO NEXT JOB                          
GETFRST  LA    R1,IOHIGH+IOACCDIR+IO1                                           
         B     GET04                                                            
GET02    LA    R1,IOSEQ+IOACCDIR+IO1                                            
GET04    GOTO1 AIO                                                              
         BNE   EXITN                                                            
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEYSAV                                  
         BNE   EXITN                                                            
         CLC   ACTKACT,SVJOBMIN    MATCH ON INPUT KEY                           
         BL    EXITN                                                            
         CLC   ACTKACT,SVJOBMAX                                                 
         BH    EXITN                                                            
         TM    ACTKSTAT,ACTSABLP   TEST BALANCE ELEMENT                         
         BO    GET06                                                            
         GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
         LA    RF,LCLIOFFC                                                      
         SR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         LA    RE,ACTKACT(RE)      RE=A(1ST PRODUCT CHAR)                       
         CLI   0(RE),C' '          TEST THIS IS A PRODUCT                       
         BNH   *+8                                                              
         LA    RF,LPROOFFC         YES - SET PRODUCT OFFICE                     
         MVC   0(L'LCLIOFFC,RF),ACOFFC                                          
         MVC   LJOBOFFC,LPROOFFC   SET JOB OFFICE FROM PRODUCT                  
         CLC   LJOBOFFC,BCSPACES                                                
         BNE   GET02                                                            
         MVC   LJOBOFFC,LCLIOFFC   SET JOB OFFICE FROM CLIENT                   
         B     GET02                                                            
*                                                                               
GET06    CLC   ACTKEY+ACTKEND(L'ACTKEY-ACTKEND),BCSPACES                        
         BH    GETNEXT                                                          
         TM    ACTKSTAT,ACTSCLOS   TEST JOB IS CLOSED                           
         BO    GETNEXT                                                          
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 JOBSET,BOPARM,('LSETI1ST',AIO1)                                  
         BNE   GETNEXT                                                          
*                                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* LAST FOR SCREEN                                                     *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST  TM    SVINDS,SVIALL       TEST IF ALLOCATE SCREEN CALLED               
         BZ    *+12                                                             
         NI    SVINDS,FF-SVIALL    YES - RESET FLAG AND EXIT                    
         B     EXITY                                                            
*                                                                               
         MVC   BCCLICOD,SVENTCLI   RESTORE ENTRY VALUES                         
         MVC   BCPROCOD,SVENTPRO                                                
         MVC   BCJOBCOD,SVENTJOB                                                
*                                                                               
         TM    SVINDS,SVIFLTD      TEST ALL JOBS FILTERED                       
         BO    EXITY                                                            
         CLI   FVOMTYP,GTMINF                                                   
         BNE   EXITY                                                            
         CLC   FVMSGNO,=AL2(AI$SJBFL)                                           
         BE    EXITY                                                            
         CLC   FVXTRA,BCSPACES                                                  
         BH    EXITY                                                            
         LH    RF,=Y(LC@PF4FL-TWAD)                                             
         LA    RF,TWAD(RF)                                                      
         MVC   FVXTRA,0(RF)                                                     
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* PF KEY ROUTINES                                                               
***********************************************************************         
         SPACE 1                                                                
PFKRTN   CLI   BCPFKEY,PFKACFMQ                                                 
         BE    CONFIRM                                                          
         CLI   BCPFKEY,PFKAFLTQ                                                 
         BE    CONFIRM                                                          
         CLI   BCPFKEY,PFKAGRPQ                                                 
         BE    GROUP                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
***********************************************************************         
* CONFIRM AUTO-ALLOCATION/AUTO-UNALLOCATION                           *         
***********************************************************************         
         SPACE 1                                                                
CONFIRM  MVC   TLNUM,CSPSRECN      GET TSAR RECORD                              
CFM04    XC    SVNXRECN,SVNXRECN                                                
         ICM   RE,3,TLNUM                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,3,TLNUM                                                       
         CLC   TLNUM,CSHIRECN                                                   
         BH    CFM10                                                            
         GOTO1 ATSARIO,TSAGET                                                   
*                                                                               
         LA    R1,LAUTFLTQ                                                      
         CLI   BCPFKEY,PFKAFLTQ                                                 
         BE    CFM06                                                            
         LA    R1,LAUTALCQ                                                      
         CLI   CSACT,ACTAULC                                                    
         BNE   CFM06                                                            
         LA    R1,LAUTUNAQ                                                      
CFM06    GOTO1 AUTO,(R1)                                                        
         BNE   EXITN                                                            
         GOTO1 ATSARIO,TSAPUT      WRITE BACK TSAR RECORD                       
         CLC   TLNUM,CSHIRECN      TEST END OF LIST RECORDS                     
         BNL   CFM10                                                            
*                                                                               
         GOTO1 VGETFACT,BOPARM,0   GET A(SYSTEM INFO BLOCK)                     
         L     R1,BOPARM                                                        
         USING FACTSD,R1           TEST WITHIN 90% OF 5000 IOS                  
         CLC   FATIOCNT,=Y(4500)                                                
         BL    CFM04                                                            
         MVC   SVNXRECN,TLNUM                                                   
         MVC   FVMSGNO,=AL2(AI$HIJPR)                                           
         CLI   BCPFKEY,PFKAFLTQ                                                 
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(AI$SJBFL)                                           
         MVI   FVOMTYP,GTMINF                                                   
*        GOTO1 AGETGEN                                                          
*        XC    FVMSGNO,FVMSGNO                                                  
*        MVI   FVMSGNO+1,X'FE'                                                  
         B     EXITY                                                            
         DROP  R1                                                               
*                                                                               
CFM10    OI    SVINDS,SVIFLTD      SET ALL JOBS FILTERED                        
         MVC   FVMSGNO,=AL2(AI$AJBFL)  ALL JOBS FILTERED                        
         CLI   BCPFKEY,PFKAFLTQ                                                 
         BE    EXITY                                                            
         MVC   FVMSGNO,=AL2(AI$ALCAL)  ALL CHARGES ALLOCATED                    
         CLI   CSACT,ACTAALC                                                    
         BE    EXITY                                                            
         MVC   FVMSGNO,=AL2(AI$ALCUL)  ALL CHARGES UNALLOCATED                  
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* GET NEXT REFERENCE NUMBER TO BE ATTACHED TO GROUP OF ALLOCATED JOBS *         
***********************************************************************         
         SPACE 1                                                                
GROUP    MVC   FVMSGNO,=AL2(AE$UEAGN)  UNABLE TO ESTABLISH GROUP NUMBER         
         LA    R3,IOKEY                                                         
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BNE   EXITN                                                            
         GOTO1 AIO,IOGET+IOLOCK+IOACCMST+IO1                                    
         BNE   EXITN                                                            
         L     R3,AIO1                                                          
         LA    R2,ACTRFST                                                       
*                                                                               
         XR    R0,R0                                                            
GRP02    CLI   0(R2),0                                                          
         BE    GRP08                                                            
         CLI   0(R2),XXPELQ        EXTRA EXTRA PROFILE ELEMENT                  
         BE    GRP04                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GRP02                                                            
*                                                                               
         USING XXPELD,R2                                                        
GRP04    CLI   XXPLN,XXPLN1Q                                                    
         BH    *+8                                                              
         BAS   RE,EXTXXP           EXTENDED ELEMENT                             
         LH    R1,XXPLAGN          HIGHEST ALLOCATION GROUP NO.                 
         CH    R1,=X'7FFF'                                                      
         BNL   EXITN                                                            
         LA    R1,1(R1)            INCREMENT                                    
         STH   R1,XXPLAGN                                                       
         STCM  R1,3,SVAGN                                                       
         B     GRP10                                                            
         DROP  R2,R3                                                            
*                                                                               
GRP08    XC    BOELEM,BOELEM       ADD EXTRA EXTRA PROFILE ELEMENT              
         PUSH  USING                                                            
         USING XXPELD,BOELEM                                                    
         MVI   XXPEL,XXPELQ                                                     
         MVI   XXPLN,XXPLN2Q                                                    
         LH    R0,=H'1'                                                         
         STH   R0,XXPLAGN                                                       
         STCM  R0,3,SVAGN                                                       
         GOTO1 VHELLO,BOPARM,(C'P',=C'ACCMST'),AIO1,BOELEM,0                    
         CLI   12(R1),0                                                         
         BNE   EXITN                                                            
         POP   USING                                                            
*                                                                               
GRP10    GOTO1 AIO,IOPUT+IOACCMST+IO1  WRITE BACK UPDATED LEDGER RECORD         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TLNUM,CSPSRECN      GET TSAR RECORD                              
GRP12    ICM   RE,3,TLNUM                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,3,TLNUM                                                       
         CLC   TLNUM,CSHIRECN                                                   
         BH    GRP26                                                            
         GOTO1 ATSARIO,TSAGET                                                   
*                                                                               
         MVC   IODAOVER,TLDA       GET JOB RECORD                               
         GOTO1 AIO,IOGET+IOLOCK+IOACCMST+IO1                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1                                                          
         USING ACTRECD,R3                                                       
         LA    R2,ACTRFST                                                       
         XR    R0,R0                                                            
GRP14    CLI   0(R2),0                                                          
         BE    GRP18                                                            
         CLI   0(R2),XXPELQ                                                     
         BE    GRP16                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GRP14                                                            
*                                                                               
         USING XXPELD,R2                                                        
GRP16    TM    XXPLN,XXPLN1Q                                                    
         BH    *+12                                                             
         BAS   RE,EXTXXP           EXTENDED ELEMENT                             
         BNE   GRP20                                                            
         MVC   XXPJAGN,SVAGN                                                    
         MVC   TLAAGN,SVAGN        REFRESH LIST RECORD                          
         B     GRP22                                                            
         DROP  R2,R3                                                            
*                                                                               
GRP18    XC    BOELEM,BOELEM       ADD EXTRA EXTRA PROFILE ELEMENT              
         PUSH  USING                                                            
         USING XXPELD,BOELEM                                                    
         MVI   XXPEL,XXPELQ                                                     
         MVI   XXPLN,XXPLN2Q                                                    
         MVC   XXPJAGN,SVAGN                                                    
         GOTO1 VHELLO,BOPARM,(C'P',=C'ACCMST'),AIO1,BOELEM,0                    
         CLI   12(R1),0                                                         
         BNE   GRP20                                                            
         MVC   TLAAGN,SVAGN                                                     
         B     GRP22                                                            
GRP20    MVC   FVMSGNO,=AL2(AE$UAAGN)  UNABLE TO ATTACH GROUP NUMBER            
         B     EXITN                                                            
         POP   USING                                                            
*                                                                               
GRP22    GOTO1 AIO,IOPUT+IOACCMST+IO1  WRITE BACK UPDATED JOB RECORD            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 ATSARIO,TSAPUT      WRITE BACK TSAR RECORD                       
         B     GRP12                                                            
*                                                                               
GRP26    GOTO1 VGETFACT,BOPARM,0   GET A(SYSTEM INFO BLOCK)                     
         LA    R1,BOPARM                                                        
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,=AL2(AI$GNATC)                                           
         MVI   GTMTYP,GTMINF                                                    
         XR    R0,R0                                                            
         ICM   R0,3,SVAGN          SET A(SUBSTITUTION PARAMETERS)               
         CVD   R0,BODUB1                                                        
         UNPK  BOWORK1+1(5),BODUB1                                              
         OI    BOWORK1+5,X'F0'                                                  
         MVI   BOWORK1,6                                                        
         MVI   BOWORK1+6,0                                                      
         LA    R0,BOWORK1                                                       
         STCM  R0,7,GTASUBST                                                    
         LA    R0,BASMSG                                                        
         STCM  R0,7,GTAOUT                                                      
         OI    GT1INDS,GT1OWRK                                                  
         MVI   GTMAXL,L'BASMSG                                                  
         GOTO1 VGETTXT             RESOLVE MESSAGE                              
         XC    FVMSGNO,FVMSGNO                                                  
         MVI   FVMSGNO+1,X'FE'                                                  
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* EXTENDED EXTRA EXTRA PROFILE ELEMENT                                *         
***********************************************************************         
         SPACE 1                                                                
         USING XXPELD,R2                                                        
EXTXXP   ST    RE,RETURN                                                        
         XC    BOELEM,BOELEM                                                    
         MVC   BOELEM(XXPLN1Q),XXPEL                                            
         MVI   BOELEM+(XXPLN-XXPELD),XXPLN2Q                                    
         GOTO1 VHELLO,BOPARM,(C'D',=C'ACCMST'),('XXPELQ',AIO1),0,0              
         CLI   12(R1),0                                                         
         BNE   EXTXXPN                                                          
         GOTO1 VHELLO,BOPARM,(C'P',=C'ACCMST'),AIO1,BOELEM,0                    
         CLI   12(R1),0                                                         
         BNE   EXTXXPN                                                          
EXTXXPY  L     RE,RETURN                                                        
         CR    RB,RB                                                            
         BR    RE                                                               
EXTXXPN  L     RE,RETURN                                                        
         LTR   RB,RB                                                            
         BR    RE                                                               
         DROP  R2                                                               
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
         B     DISUNA              UNALLOCATED                                  
         B     DISANT              ALLOCATED NET                                
         B     DISACO              ALLOCATED COMMISSION                         
*&&UK*&& B     DISEXR              EXCHANGE RATE                                
*&&UK*&& B     DISGRP              GROUP                                        
         SPACE 1                                                                
***********************************************************************         
* DISPLAY JOB CODE                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISCOD   TM    SVINDS,SVIFLTD      TEST EVERYTHING IS FILTERED                  
         BZ    DISCOD02                                                         
         GOTO1 JOBSET,BOPARM,ACTRECD                                            
         TM    SVINDS,SVIFLTD      TEST JOB DETAILS NEED UPDATING               
         BO    DISCOD10                                                         
         GOTO1 AUTO,LAUTFLTQ       YES - UPDATE THEM                            
         OI    SVINDS,SVIFLTD                                                   
         B     DISCOD10                                                         
*                                                                               
DISCOD02 GOTO1 JOBSET,BOPARM,ACTRECD                                            
*                                                                               
DISCOD10 MVC   FVIFLD(L'ACTKACT),ACTKACT                                        
         NI    TLINDS1,FF-TLIHIGH                                               
         LA    RE,TLAIALL          HIGHLIGHT LINE IF FULLY                      
         CLI   CSACT,ACTAALC         ALLOCATED/CLEARED                          
         BE    *+8                                                              
         LA    RE,TLAICLR                                                       
         EX    RE,*+8                                                           
         BZ    EXIT                                                             
         TM    TLAINDS,0                                                        
         OI    TLINDS1,TLIHIGH                                                  
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY JOB NAME                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISNAM   XR    R0,R0                                                            
         LA    RF,ACTRFST                                                       
         USING NAMELD,RF                                                        
DNAM02   CLI   NAMEL,0                                                          
         BE    EXIT                                                             
         CLI   NAMEL,NAMELQ                                                     
         BNE   DNAM04                                                           
         XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         EX    RE,*+4                                                           
         MVC   FVIFLD(0),NAMEREC                                                
         B     EXIT                                                             
DNAM04   IC    R0,NAMLN                                                         
         AR    RF,R0                                                            
         B     DNAM02                                                           
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY UNALLOCATED                                                 *         
***********************************************************************         
         SPACE 1                                                                
*ISUNA   TM    TLAINDS,TLAIUPD+TLAIJOB                                          
*        BZ    EXIT                                                             
DISUNA   CURED TLAUNA,(13,FVIFLD),TLACTAB,MINUS=YES                             
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY ALLOCATED NET                                               *         
***********************************************************************         
         SPACE 1                                                                
*ISANT   TM    TLAINDS,TLAIUPD+TLAIJOB                                          
*        BZ    EXIT                                                             
DISANT   CURED TLAANT,(13,FVIFLD),TLACTAB,MINUS=YES                             
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY ALLOCATED COMMISSION                                        *         
***********************************************************************         
         SPACE 1                                                                
*ISACO   TM    TLAINDS,TLAIUPD+TLAIJOB                                          
*        BZ    EXIT                                                             
DISACO   CURED TLAACO,(13,FVIFLD),TLACTAB,MINUS=YES                             
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY EXCHANGE RATE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISEXR   CLC   TLACUR,CSCPYCUR     TEST FOR COMPANY CURRENCY                    
         BE    EXIT                                                             
         GOTO1 AEDTRAT,DMCB,(10,FVIFLD),TLAXRAT,0                               
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY GROUP                                                       *         
***********************************************************************         
         SPACE 1                                                                
DISGRP   OC    TLAAGN,TLAAGN                                                    
         BZ    EXIT                                                             
         XR    R0,R0                                                            
         ICM   R0,3,TLAAGN                                                      
         CVD   R0,BODUB1                                                        
         UNPK  BOWORK1(5),BODUB1                                                
         OI    BOWORK1+4,X'F0'                                                  
         MVC   FVIFLD+1(5),BOWORK1                                              
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE LINE SELECTION                                             *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   SLL   R1,2                                                             
         B     *(R1)                                                            
         B     VALALL              ALLOCATE                                     
         B     VALCLR              CLEAR                                        
*                                                                               
VALALL   GOTO1 ASETUP,BOPARM,TLAJOB,TLACUR,TLAXVAL                              
         BNE   EXITN                                                            
         OI    CSINDSG1,CSINDSET                                                
         OI    SVINDS,SVIALL                                                    
         B     EXITY                                                            
*                                                                               
VALCLR   GOTO1 AUTO,LAUTUNAQ                                                    
         BNE   EXITN                                                            
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO AUTO(UN)ALLOCATE                                         *         
*                                                                     *         
* NTRY: R1 = LAUTALCQ TO ALLOCATE                                     *         
*          = LAUTUNAQ TO UNALLOCATE                                   *         
*          = LAUTFLTQ TO FILTER                                       *         
***********************************************************************         
         SPACE 1                                                                
AUTO     NTR1  ,                                                                
         STC   R1,LAUTACT                                                       
*                                                                               
         GOTO1 ASETUP,BOPARM,TLAJOB,(X'80',TLACUR),TLAXVAL                      
         BNE   EXITN                                                            
         MVC   IODAOVER,BCJOBDA                                                 
         LH    R1,=Y(IOGET+IOACCMST+IO4)                                        
         CLI   LAUTACT,LAUTFLTQ                                                 
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 JOBSET,BOPARM,AIO4                                               
*                                                                               
         TM    TLAINDS,TLAIUPD TEST TSAR RECORD UP TO DATE                      
         BZ    AUTO02                                                           
         CLI   LAUTACT,LAUTFLTQ    YES - ALREADY FILTERED                       
         BE    EXITY                                                            
         CLI   LAUTACT,LAUTALCQ                                                 
         BNE   *+12                                                             
         TM    TLAINDS,TLAIALL TEST ALREADY FULLY ALLOCATED                     
         BO    EXITY                                                            
         CLI   LAUTACT,LAUTUNAQ                                                 
         BNE   *+12                                                             
         TM    TLAINDS,TLAICLR TEST ALREADY FULLY UNALLOCATED                   
         BO    EXITY                                                            
*                                                                               
AUTO02   MVC   LSAVLST,TLSTD       COPY JOB TSAR RECORD                         
JOB      USING TLSTD,LSAVLST                                                    
         ZAP   JOB.TLAUNA,BCPZERO  ZEROISE TOTALS                               
         ZAP   JOB.TLAANT,BCPZERO                                               
         ZAP   JOB.TLAACO,BCPZERO                                               
         MVI   JOB.TLAINDS,TLAIALL+TLAICLR                                      
*                                                                               
K        USING TRNRECD,IOKEY       READ FOR TRANSACTION RECORDS                 
         MVC   K.TRNKEY,BCSPACES                                                
         MVC   K.TRNKCPY,CUABIN                                                 
         MVC   K.TRNKUNT(L'BCCPYPRD),BCCPYPRD                                   
         MVC   K.TRNKACT,JOB.TLAJOB                                             
         OI    LSINDS1,LSIBLST     SET BUILDING LIST                            
*                                                                               
         LA    R1,IOHIGH+IOACCDIR+IO2                                           
         B     *+8                                                              
AUTO04   LA    R1,IOSEQ+IOACCDIR+IO2                                            
         GOTO1 AIO                                                              
         BNE   AUTO30                                                           
         CLC   K.TRNKEY(TRNKWORK-TRNKEY),IOKEYSAV                               
         BNE   AUTO30                                                           
         CLC   K.TRNKWORK,=C'99'   TEST LAST FOR THIS JOB                       
         BNL   AUTO30                                                           
         OC    K.TRNKDATE,K.TRNKDATE ENSURE WE HAVE TRANSACTION RECORD          
         BZ    AUTO04                                                           
         CLC   K.TRNKDATE,BCSPACES                                              
         BE    AUTO04                                                           
         GOTO1 ASETTRN,BOPARM,(C'D',K.TRNRECD)                                  
         BNE   AUTO04                                                           
*                                                                               
         LA    R1,IOGET+IOACCMST+IO2  GET TRANSACTION RECORD                    
         CLI   LAUTACT,LAUTFLTQ                                                 
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
R        USING TRNRECD,R2                                                       
         USING TRNELD,R.TRNRFST                                                 
*&&UK                                                                           
         CLI   P#UNAUTH,C'Y'       TEST PREVENT ALLOC. OF UNAUTH ITEMS          
         BNE   *+12                                                             
         TM    TRNSTAT,TRNSAUTH                                                 
         BZ    AUTO04                                                           
*&&                                                                             
         GOTO1 ASETTRN,BOPARM,(C'M',R.TRNRECD)                                  
         BNE   AUTO28                                                           
*                                                                               
         CLI   LAUTACT,LAUTFLTQ    TEST FILTERING ONLY                          
         BE    AUTO24                                                           
         MVC   LTRNRSTA,R.TRNRSTA                                               
         CLI   LAUTACT,LAUTALCQ    TEST ALLOCATING                              
         BE    AUTO08                                                           
         CLI   LAUTACT,LAUTUNAQ    TEST UNALLOCATING                            
         BE    AUTO10                                                           
         DC    H'0'                                                             
*                                                                               
AUTO08   BAS   RE,SETAMTS          ALLOCATE                                     
         CP    LNVBL,BCPZERO       NON-ZERO AMOUNT MUST BE AVAILABLE            
         BE    AUTO24                                                           
*                                                                               
         LA    RE,LSPRATA          SAVE ORIGINAL PRO-RATA BLOCK                 
         LA    RF,LSPRATAS-LSPRATA                                              
         LA    R0,LSPRATAS                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO2),('PTASCASH',LSPRATA),   X        
               (0,LNVBL),(0,LCVBL)                                              
         BE    AUTO20                                                           
         MVC   FVMSGNO,=AL2(AE$UUTXR)  UNABLE TO UPDATE TRANSACTION             
         OI    LAUTINDS,LAUTIERR                                                
         B     AUTO30                                                           
*                                                                               
AUTO10   BAS   RE,SETAMTS          UNALLOCATE                                   
         TM    PG$STAT,PG$REVS                                                  
         BO    AUTO24              DON'T UNALL REVERSAL ALLOCATION              
         CP    LALLO,BCPZERO       NET OR COMM PENDING MUST BE NONZERO          
         BNE   *+14                                                             
         CP    LCOMM,BCPZERO                                                    
         BE    AUTO24                                                           
*                                                                               
         LA    RE,LSPRATA          SAVE ORIGINAL PRO-RATA BLOCK                 
         LA    RF,LSPRATAS-LSPRATA                                              
         LA    R0,LSPRATAS                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         ZAP   BODUB1,BCPZERO                                                   
         ZAP   BODUB2,BCPZERO                                                   
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO2),('PTASCASH',LSPRATA),   X        
               (1,BODUB1),(1,BODUB2)                                            
         BE    AUTO20                                                           
         MVC   FVMSGNO,=AL2(AE$UUTXR)  UNABLE TO UPDATE TRANSACTION             
         OI    LAUTINDS,LAUTIERR                                                
         B     AUTO30                                                           
*                                                                               
AUTO20   GOTO1 AUPDJOB,BOPARM,(C'U',AIO2),AIO4,LSPRATAS,LSPRATA                 
         BE    *+12                                                             
         OI    LAUTINDS,LAUTIERR                                                
         B     AUTO30                                                           
         GOTO1 AIO,IOPUT+IOACCMST+IO2  WRITE BACK UPDATED TX RECORD             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   LTRNRSTA,R.TRNRSTA  TEST FOR CHANGE IN STATUS AREA               
         BE    AUTO24                                                           
         GOTO1 AIO,IOACCDIR+IOREAD  UPDATE DIRECTORY RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   K.TRNKSTA,R.TRNRSTA                                              
         GOTO1 AIO,IOACCDIR+IOWRITE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
AUTO24   BAS   RE,SETAMTS          ADJUST AMOUNTS                               
         AP    JOB.TLAUNA,LNVBL                                                 
         AP    JOB.TLAANT,LALLO                                                 
         AP    JOB.TLAACO,LCOMM                                                 
         CP    LNVBL,BCPZERO       TEST UNALLCOATED AMOUNT                      
         BE    *+8                                                              
         NI    JOB.TLAINDS,FF-TLAIALL                                           
         CP    LALLO,BCPZERO       TEST ALLOCATED AMOUNT                        
         BE    *+8                                                              
         NI    JOB.TLAINDS,FF-TLAICLR                                           
         CP    LCOMM,BCPZERO                                                    
         BE    *+8                                                              
         NI    JOB.TLAINDS,FF-TLAICLR                                           
*                                                                               
AUTO28   GOTO1 AIO,IOREAD+IOACCDIR RE-READ TO ESTABLISH SEQUENCE                
         BE    AUTO04                                                           
         DC    H'0'                                                             
         DROP  K,R                                                              
*                                                                               
AUTO30   NI    LSINDS1,FF-LSIBLST                                               
         MVC   TLSTD(L'LSAVLST),LSAVLST                                         
         DROP  JOB                 RESTORE JOB TSAR RECORD                      
*                                                                               
         CLI   LAUTACT,LAUTFLTQ                                                 
         BE    AUTO32                                                           
         LH    R1,=Y(IO4)                                                       
         GOTO1 AIO,IOPUT+IOACCMST(R1)  WRITE BACK UPDATED JOB RECORD            
         BE    *+6                                                              
         DC    H'0'                                                             
AUTO32   OI    TLAINDS,TLAIUPD                                                  
         L     R3,AIO4                                                          
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         USING JCBELD,R3           UPDATE SEQUENCE NUMBER                       
         XR    RF,RF                                                            
AUTO34   CLI   JCBEL,0                                                          
         BE    AUTO36                                                           
         CLI   JCBEL,JCBELQ                                                     
         BE    *+12                                                             
         IC    RF,JCBLN                                                         
         BXH   R3,RF,AUTO34                                                     
         MVC   TLASEQ,JCBSEQ                                                    
         DROP  R3                                                               
*                                                                               
AUTO36   TM    LAUTINDS,LAUTIERR                                                
         BZ    EXITY                                                            
         MVI   TLAINDS,0                                                        
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET NET/COMMISSION AVAILABLE/ALLOCATED                   *         
***********************************************************************         
         SPACE 1                                                                
SETAMTS  CLC   CSCPYCUR,TLACUR     TEST AGENCY/FOREIGN CURRENCY                 
         BNE   SETAMTSF                                                         
         ZAP   LNVBL,PM$ANVBL                                                   
         ZAP   LCVBL,PM$ACVBL                                                   
         ZAP   LALLO,PP$AALLO                                                   
         ZAP   LCOMM,PP$ACOMM                                                   
         BR    RE                                                               
SETAMTSF ZAP   LNVBL,PM$FNVBL                                                   
         ZAP   LCVBL,PM$FCVBL                                                   
         ZAP   LALLO,PP$FALLO                                                   
         ZAP   LCOMM,PP$FCOMM                                                   
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SET LIST RECORD DATA FOR JOB RECORD                                 *         
*                                                                     *         
* NTRY: P1 BYTE 0 = X'80' ON IF SETTING UP FOR 1ST TIME               *         
*                   X'40' ON TO REFRESH EVEN IF SEQ. NO THE SAME      *         
*             1-3 = A(JOB RECORD)                                     *         
***********************************************************************         
         SPACE 1                                                                
JOBSET   NTR1  ,                                                                
         MVC   LSETINDS,0(R1)                                                   
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
         USING ACTRECD,R2          R2=A(JOB RECORD)                             
*                                                                               
         LA    R4,ACTRFST          GET JOB CLIENT BILLING ELEMENT               
         USING JCBELD,R4                                                        
         XR    RF,RF                                                            
JSET02   CLI   JCBEL,0                                                          
         BE    JSET04                                                           
         CLI   JCBEL,JCBELQ                                                     
         BE    JSET06                                                           
         IC    RF,JCBLN                                                         
         BXH   R4,RF,JSET02                                                     
JSET04   XC    BOELEM,BOELEM                                                    
         LA    R4,BOELEM                                                        
JSET06   TM    LSETINDS,LSETI1ST+LSETIREF  TEST 1ST TIME/REFRESHING             
         BNZ   *+14                                                             
         CLC   TLASEQ,JCBSEQ       TEST JOB HAS BEEN UPDATED                    
         BE    JOBSETY                                                          
         MVC   TLASEQ,JCBSEQ                                                    
*                                                                               
         MVC   TLAJOB,ACTKACT                                                   
         ZAP   TLAUNA,BCPZERO                                                   
         ZAP   TLAANT,BCPZERO                                                   
         ZAP   TLAACO,BCPZERO                                                   
         XC    TLACUR,TLACUR                                                    
         XC    TLACTAB,TLACTAB                                                  
         XC    TLAXVAL,TLAXVAL                                                  
         XC    TLAAGN,TLAAGN                                                    
         MVI   TLAINDS,0                                                        
*                                                                               
         LA    R3,ACTRFST                                                       
         XR    R0,R0                                                            
JSET10   CLI   0(R3),0                                                          
         BE    JSET30                                                           
         CLI   0(R3),AFCELQ                                                     
         BE    JSET14                                                           
         CLI   0(R3),JOBELQ                                                     
         BE    JSET16                                                           
         CLI   0(R3),XXPELQ                                                     
         BE    JSET18                                                           
         CLI   0(R3),PPRELQ                                                     
         BE    JSET22                                                           
JSET12   IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     JSET10                                                           
*                                                                               
         USING AFCELD,R3                                                        
JSET14   CLC   AFCCURR,BCSPACES                                                 
         BNH   JSET12                                                           
         MVC   TLAXVAL,AFCX                                                     
         MVC   TLACUR,AFCCURR      CURRENCY CODE FOR LIST RECD                  
         MVC   TLACTAB,SVCURBIL                                                 
         CLC   TLACUR,SVBILCUR                                                  
         BE    JSET12                                                           
         GOTO1 VBLDCUR,BOPARM,AFCCURR,TLACTAB,ACOM                              
         B     JSET12                                                           
*                                                                               
         USING JOBELD,R3                                                        
JSET16   TM    JOBSTA1,JOBSXJOB    TEST EXPENSE JOB                             
         BO    JOBSETN             EXCLUDE FROM THE LIST                        
         B     JSET12                                                           
*                                                                               
         USING XXPELD,R3                                                        
JSET18   OC    SVGRPC,SVGRPC       TEST FILTERING BY GROUP                      
         BZ    JSET20                                                           
         TM    XXPLN,XXPLN2Q                                                    
         BL    JOBSETN                                                          
         CLC   XXPJAGN,SVGRPC      EXCLUDE IF GROUP NOT REQUIRED                
         BNE   JOBSETN                                                          
         B     *+12                                                             
JSET20   TM    XXPLN,XXPLN2Q                                                    
         BL    JSET12                                                           
         MVC   TLAAGN,XXPJAGN      ALOCATION GROUP NUMBER FOR LIST RECD         
         B     JSET12                                                           
*                                                                               
         USING PPRELD,R3                                                        
JSET22   CLC   PPRGAOFF,BCSPACES                                                
         BNH   JSET12                                                           
         MVC   LJOBOFFC,PPRGAOFF                                                
         B     JSET12                                                           
         DROP  R3                                                               
*                                                                               
JSET30   OC    TLACUR,TLACUR       TEST CURRENCY TABLE ENTRY DEFINED            
         BNZ   JSET32                                                           
         MVC   TLACUR,SVBILCUR     USE BILLING CURR AS DEFAULT                  
         MVC   TLACTAB,SVCURBIL                                                 
         MVC   TLAXVAL,SVEXCVAL                                                 
JSET32   CLC   SVBILCUR,TLACUR     EXCLUDE IF JOB NOT IN BILLING CURR           
         BNE   JOBSETN                                                          
*                                                                               
         GOTO1 JOBTOT,ACTRECD                                                   
         CP    TLAUNA,BCPZERO      EXIT NOW IF JOB BALANCE IS ZERO              
         BNE   JSET34                                                           
         CP    TLAANT,BCPZERO      AND NO ALLOCATION IS PENDING                 
         BNE   JSET34                                                           
         OC    JCBALL,JCBALL                                                    
         BZ    JOBSETN                                                          
*                                                                               
JSET34   TM    TLAINDS,TLAIJOB     TEST JOB TOTAL OKAY                          
         BO    *+8                                                              
         NI    SVINDS,FF-SVIFLTD   NO - FILTERING REQUIRED                      
         TM    LSETINDS,LSETI1ST   FINISHED IF NOT 1ST TIME                     
         BZ    JOBSETY                                                          
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1           CALL OFFAL TO TEST OFFIC                     
         LA    RE,ACTRECD                                                       
         ST    RE,OFFAREC                                                       
         MVI   OFFAOPOS,LDGOPROF                                                
         MVC   OFFAOFFC,LJOBOFFC                                                
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         BNE   JOBSETN                                                          
         DROP  R1                                                               
*                                                                               
         OC    TLAAGN,TLAAGN       TEST ALLOCATION GROUP NUMBER SET             
         BNZ   *+14                                                             
         OC    SVGRPC,SVGRPC       TEST FILTERING BY GROUP                      
         BNZ   JOBSETN                                                          
*                                                                               
         GOTO1 AGETOPT,BOPARM,ACTRECD                                           
         L     RF,AGOPBLK          TEST BILLING TYPE                            
         CLI   GOBILTYP-GOBLOCK(RF),PPRBCLIT                                    
         BNE   JOBSETN                                                          
*                                                                               
JOBSETY  B     EXITY                                                            
*                                                                               
JOBSETN  B     EXITN                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* SET TOTALS FOR WHOLE JOB                                            *         
*                                                                     *         
* NTRY: R1 = A(JOB RECORD)                                            *         
***********************************************************************         
         SPACE 1                                                                
JOBTOT   NTR1  ,                                                                
         ZAP   TLAUNA,BCPZERO                                                   
         ZAP   TLAANT,BCPZERO                                                   
         ZAP   TLAACO,BCPZERO                                                   
*                                                                               
         LR    R2,R1                                                            
         USING ACTRECD,R2                                                       
         LA    R2,ACTRFST                                                       
         ZAP   SVAMPEND,BCPZERO                                                 
*                                                                               
         XR    R0,R0                                                            
JTOT02   CLI   0(R2),0                                                          
         BE    JTOT20                                                           
         CLI   0(R2),ABLELQ                                                     
         BE    JTOT06                                                           
         CLI   0(R2),SCIELQ                                                     
         BE    JTOT08                                                           
JTOT04   IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     JTOT02                                                           
*                                                                               
         USING ABLELD,R2                                                        
JTOT06   ZAP   TLAUNA,ABLDR        JOB BALANCE : TOTAL CHARGES -                
         SP    TLAUNA,ABLCR        TOTAL BILLING = TOTAL BILLABLE               
         B     JTOT04                                                           
*                                                                               
         USING SCIELD,R2                                                        
JTOT08   CLI   SCITYPE,SCITCBAP    ALLOCATION PENDING                           
         BNE   JTOT10                                                           
         AP    SVAMPEND,SCIAMNT                                                 
         ZAP   TLAANT,SCIAMNT      ALLOCATED NET FOR LIST RECD                  
         ZAP   TLAACO,SCIADMN      ALLOCATED COMMISSION FOR LIST RECD           
JTOT10   CLI   SCITYPE,SCITCBWP    WRITE-OFFS PENDING                           
         BE    JTOT12                                                           
         CLI   SCITYPE,SCITCBTP    TRANSFERS PENDING                            
         BE    JTOT12                                                           
         CLI   SCITYPE,SCITCBRP    RECOVERIES PENDING                           
         BE    JTOT12                                                           
         CLI   SCITYPE,SCITCBIP    B/TYPE 8 PENDING                             
         BE    JTOT12                                                           
         CLI   SCITYPE,SCITT99S    BILLING NOT IN ABLCR                         
         BNE   JTOT04                                                           
         SP    TLAUNA,SCIAMNT                                                   
         B     JTOT04                                                           
*                                                                               
JTOT12   AP    SVAMPEND,SCIAMNT                                                 
         CLI   SCILN,SCILN2Q                                                    
         BNE   JTOT04                                                           
         AP    SVAMPEND,SCIADMN                                                 
         B     JTOT04                                                           
         DROP  R2                                                               
*                                                                               
JTOT20   CLC   TLACUR,CSCPYCUR     TEST JOB BALANCE NEEDS CONVERSION            
         BE    JTOT22                                                           
         EXCHP TLAUNA,TLAXVAL,DUB=BOPL81                                        
         ZAP   TLAUNA,BOPL81                                                    
JTOT22   SP    TLAUNA,SVAMPEND     UNALLOCATED NET FOR LIST RECD                
*                                                                               
         MVI   TLAINDS,0                                                        
         TM    BCCPYST6,CPYSFBIL   TEST FOREIGN CURRENCY BILLING                
         BO    JOBTOTX                                                          
         TM    SVINDS,SVIDEFOP     TEST TRANSACTION OPTIONS = DEFAULT           
         BZ    JOBTOTX                                                          
         OI    TLAINDS,TLAIJOB     YES - AMOUNTS ARE OKAY                       
         CP    TLAUNA,BCPZERO      TEST UNALLOCATED = ZERO                      
         BNE   *+8                                                              
         OI    TLAINDS,TLAIALL     YES - JOB FULLY ALLOCATED (PROBABLY)         
         CP    TLAANT,BCPZERO      TEST ALLOCATED = ZERO                        
         BNE   JOBTOTX                                                          
         CP    TLAACO,BCPZERO                                                   
         BNE   JOBTOTX                                                          
         OI    TLAINDS,TLAICLR     YES - JOB FULLY CLEARED (PROBABLY)           
*                                                                               
JOBTOTX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY JOB OPTIONS                                      *         
***********************************************************************         
         SPACE 1                                                                
DISJBO   NTR1  ,                                                                
         MVC   AALJBO,BCSPACES                                                  
         LA    RF,AALJBO                                                        
*                                                                               
         LH    RE,=Y(UC@JOB-TWAD)  INSERT JOB OPTION                            
         LA    RE,TWAD(RE)                                                      
         MVC   0(L'UC@JOB,RF),0(RE)                                             
         LA    RF,L'UC@JOB-1(RF)                                                
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   1(1,RF),BCEQUAL                                                  
         LA    RF,2(RF)                                                         
         CLI   SVJOBMIN,C' '                                                    
         BH    *+12                                                             
         MVI   0(RF),C'?'                                                       
         B     DISJBO02                                                         
         MVC   0(L'SVJOBMIN,RF),SVJOBMIN                                        
         LA    RF,L'SVJOBMIN-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R1,SVJOBMAX                                                      
         LA    RE,L'SVJOBMAX-1(R1)                                              
         CLI   0(RE),FF                                                         
         BL    *+8                                                              
         BCT   RE,*-8                                                           
         SR    RE,R1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SVJOBMIN(0),SVJOBMAX                                             
         BE    DISJBO02                                                         
         MVI   1(RF),C'-'                                                       
         MVC   2(L'SVJOBMAX,RF),SVJOBMAX                                        
         LA    RE,L'SVJOBMAX+2(RF)                                              
         LA    RF,L'SVJOBMAX+1(RF)                                              
         CLI   0(RF),FF                                                         
         BL    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   1(0,RF),BCSPACES                                                 
*                                                                               
DISJBO02 TM    BCCPYST6,CPYSFBIL   TEST FOREIGN CURRENCY                        
         BZ    DISJBO10                                                         
         MVC   1(1,RF),BCCOMMA     INSERT CURRENCY OPTION                       
         LA    RF,2(RF)                                                         
         LH    RE,=Y(UC3CURRY-TWAD)                                             
         LA    RE,TWAD(RE)                                                      
         MVC   0(L'UC3CURRY,RF),0(RE)                                           
         LA    RF,L'UC3CURRY+1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   1(1,RF),BCEQUAL                                                  
         LA    RF,2(RF)                                                         
         OC    SVBILCUR,SVBILCUR                                                
         BNZ   DISJBO04                                                         
         MVI   0(RF),C'?'                                                       
         LA    RF,1(RF)                                                         
         B     DISJBO10                                                         
DISJBO04 MVC   0(L'SVBILCUR,RF),SVBILCUR                                        
         LA    RF,L'SVBILCUR(RF)                                                
*                                                                               
         CLC   SVBILCUR,CSCPYCUR     INSERT EXCHANGE RATE OPTION                
         BE    DISJBO10                                                         
         MVC   0(1,RF),BCCOMMA                                                  
         LA    RF,1(RF)                                                         
         LH    RE,=Y(UC@RATE-TWAD)                                              
         LA    RE,TWAD(RE)                                                      
         MVC   0(L'UC@RATE,RF),0(RE)                                            
         LA    RF,L'UC@RATE+1(RF)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   1(1,RF),BCEQUAL                                                  
         LA    RF,2(RF)                                                         
         OC    SVEXCRAT,SVEXCRAT                                                
         BNZ   DISJBO06                                                         
         MVI   0(RF),C'?'                                                       
         LA    RF,1(RF)                                                         
         B     DISJBO10                                                         
DISJBO06 ST    RF,BOFULL1                                                       
         MVC   WORK,BCSPACES                                                    
         GOTO1 AEDTRAT,DMCB,(10,WORK),SVEXCRAT,0                                
         LA    RE,WORK+10                                                       
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         LA    RE,1(RE)                                                         
         LA    RF,WORK                                                          
         SR    RE,RF                                                            
         L     RF,BOFULL1                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),WORK                                                     
         LA    RF,1(RE,RF)                                                      
*                                                                               
DISJBO10 DS    0H                                                               
*&&UK                                                                           
         OC    SVGRPE,SVGRPE       INSERT ALLOCATION GROUP NUMBER               
         BZ    DISJBOX                                                          
         MVC   0(1,RF),BCCOMMA                                                  
         LA    RF,1(RF)                                                         
         LH    RE,=Y(UC@GROUP-TWAD)                                             
         LA    RE,TWAD(RE)                                                      
         MVC   0(L'UC@GROUP,RF),0(RE)                                           
         LA    RF,L'UC@GROUP+1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   1(1,RF),BCEQUAL                                                  
         LA    RF,2(RF)                                                         
         MVC   0(L'SVGRPE,RF),SVGRPE                                            
         LA    RF,L'SVGRPE-1(RF)                                                
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,1(RF)                                                         
*&&                                                                             
DISJBOX  LA    RE,AALJBO                                                        
         SR    RF,RE                                                            
         STC   RF,AALJBOH+FHILD                                                 
         OI    AALJBOH+FHOID,FHOITR                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DEFAULT COLUMN LIST                                                 *         
***********************************************************************         
         SPACE 1                                                                
DEFCLM   DS    0XL1                                                             
         DC    AL1(AAL#JBN)                                                     
         DC    AL1(AAL#UNA)                                                     
         DC    AL1(AAL#ANT)                                                     
         DC    AL1(AAL#ACO)                                                     
*&&UK*&& DC    AL1(AAL#EXR)                                                     
*&&UK*&& DC    AL1(AAL#GRP)                                                     
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
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
LWORKD   DSECT                                                                  
RETURN   DS    A                                                                
DMCB     DS    6A                                                               
WORK     DS    XL100                                                            
LLSTREF  DS    CL1                 LIST RECORDS REFRESH INDICATOR               
LCLIOFFC DS    CL2                 CLIENT OFFICE CODE                           
LPROOFFC DS    CL2                 PRODUCT OFFICE CODE                          
LJOBOFFC DS    CL2                 JOB OFFICE CODE                              
LAUTACT  DS    XL1                 AUTO ROUTINE ACTION                          
LAUTALCQ EQU   C'A'                AUTO-ALLOCATE                                
LAUTUNAQ EQU   C'U'                AUTO-UNALLOCATE                              
LAUTFLTQ EQU   C'P'                APPLY TRANSACTION FILTERS                    
LAUTINDS DS    XL1                 AUTO ROUTINE INDICATOR                       
LAUTIERR EQU   X'80'               ERROR WHILST (UN)ALLOCATING                  
LSETINDS DS    XL1                 JOBSET ROUTINE INDICATOR                     
LSETI1ST EQU   X'80'               SETTING DETAILS FOR FIRST TIME               
LSETIREF EQU   X'40'               REFRESH DETAILS                              
LTRNRSTA DS    XL(L'TRNRSTA)       ORIGINAL TRANSACTION RECORD STATUS           
LOPTJOB  DS    CL(L'TRNKACT)       SAVED JOB FOR LAST GETOPT CALL               
LOPTWC   DS    CL(L'TRNKWORK)      SAVED W/C FOR LAST GETOPT CALL               
LLSOPS   DS    XL(LSOPL)           SAVED ALLOCTION OPTIONS                      
LSVOPTS  DS    XL(SVOPTSL2)        SAVED CURRENT JOB OPTIONS                    
LNVBL    DS    PL8                 NET AVAILABLE                                
LCVBL    DS    PL8                 COMMISSION AVAILABLE                         
LALLO    DS    PL8                 NET ALLOCATED                                
LCOMM    DS    PL8                 COMMISSION ALLOCATED                         
LSAVLST  DS    XL256               SAVED LIST RECORD                            
         DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* OPTIONS TABLE                                                       *         
***********************************************************************         
         SPACE 1                                                                
CLB13    CSECT                                                                  
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
***********************************************************************         
* JOB OPTIONS TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
JBOTAB   DS    0X                                                               
*                                  JOB=ABC-XYZ                                  
         DC    AL2(UC@JOB-TWAD,UC@JOB-TWAD)                                     
         DC    AL1(OPTNRTN+OPTREQD,0)                                           
         DC    AL1(0,0,0,0,0,1,25,L'SVJOB)                                      
         DC    AL1((*-JBOTAB)/OPTTABL)                                          
         DC    AL2(1,SVJOB-OSVALS)                                              
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*&&UK                              CUR=ABC                                      
         DC    AL2(UC3CURRY-TWAD,UC3CURRY-TWAD)                                 
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,3,L'SVBILCUR)                                    
         DC    AL1((*-JBOTAB)/OPTTABL)                                          
         DC    AL2(2,SVBILCUR-OSVALS)                                           
         DC    AL1(OPTDEFQ),AL3(0)                                              
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  RATE=XXXXX                                   
         DC    AL2(UC@RATE-TWAD,UC@RATE-TWAD)                                   
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,10,L'SVEXCVAL)                                   
         DC    AL1((*-JBOTAB)/OPTTABL)                                          
         DC    AL2(3,SVEXCVAL-OSVALS)                                           
         DC    AL1(OPTDEFQ),AL3(0)                                              
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  GROUP=XXXXX                                  
         DC    AL2(UC@GROUP-TWAD,UC@GROUP-TWAD)                                 
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,5,L'SVGRP)                                       
         DC    AL1((*-JBOTAB)/OPTTABL)                                          
         DC    AL2(4,SVGRP-OSVALS)                                              
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*&&                                                                             
JBOTABX  DC    AL1(EOT)                                                         
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
         LTORG                                                                  
         SPACE 1                                                                
*********************************************************************           
* VALIDATE JOB OPTIONS                                              *           
*********************************************************************           
         SPACE 1                                                                
         DROP                                                                   
         DS    0D                                                               
VALJBO   NMOD1 0,**VJBO**                                                       
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         LR    R4,R2               R4=A(OPTTAB ENTRY)                           
         USING OPTTABD,R4                                                       
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     VALOJOB             1 JOB=ABC-XYZ                                
         B     VALOCUR             2 CUR=ABC                                    
         B     VALORATE            3 RATE=XXXXX                                 
         B     VALOGRP             4 GROUP=XXXXX                                
         SPACE 1                                                                
VALX     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE JOB=ABC-XYZ                                                *         
***********************************************************************         
         SPACE 1                                                                
VALOJOB  LA    R3,BCWORK                                                        
         USING SVJOB,R3                                                         
         MVI   SVJOBMAX,FF                                                      
         MVC   SVJOBMAX+1(L'SVJOBMAX-1),SVJOBMAX                                
         MVC   SVJOBMIN,BCSPACES                                                
*                                                                               
         LA    R2,FVIFLD           R2=A(INPUT)                                  
         XR    RE,RE                                                            
         IC    RE,FVILEN           RE=L'INPUT                                   
         CH    RE,=H'1'                                                         
         BH    VJOB02                                                           
         CLI   0(R2),C'-'                                                       
         BE    VJOBN                                                            
         CLI   0(R2),C'?'                                                       
         BE    VJOBN                                                            
VJOB02   LR    RF,RE                                                            
         LR    R1,R2               R1=A(INPUT)                                  
         CLI   0(R1),C'-'                                                       
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   RE,*-12                                                          
*                                                                               
         SR    RF,RE               COPY MINIMUM CODE                            
         BZ    VJOB04                                                           
         LR    R0,RF               SAVE MINIMUM INPUT LENGTH                    
         BL    VJOBN                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SVJOBMIN(0),0(R2)                                                
         LTR   RE,RE               TEST PARTITION CHARACTER FOUND               
         BNZ   VJOB04                                                           
         EX    RF,*+4                                                           
         MVC   SVJOBMAX(0),0(R2)                                                
         B     VJOB06                                                           
*                                                                               
VJOB04   BCT   RE,*+8                                                           
         B     VJOB06                                                           
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   SVJOBMAX(0),1(R1)                                                
*                                                                               
VJOB06   CLI   SVJOBMIN,C' '       TEST START/END KEYS SET                      
         BNH   VJOBN                                                            
         CLI   SVJOBMAX,FF                                                      
         BE    VJOBN                                                            
*                                                                               
         XR    RF,RF               TEST MINIMUM CLIENT LENGTH INPUT             
         IC    RF,BCCLILEN                                                      
         CR    RF,R0                                                            
         BH    VJOBN                                                            
         BCTR  RF,0                TEST START/END CLIENTS MATCH                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SVJOBMIN(0),SVJOBMAX                                             
         BNE   VJOBN                                                            
*                                                                               
         LA    RE,SVJOBMAX         TEST START KEY BEFORE END KEY                
         LA    RF,L'SVJOBMAX-1(RE)                                              
         CLI   0(RF),FF                                                         
         BL    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SVJOBMIN(0),SVJOBMAX                                             
         BH    VJOBN                                                            
*                                                                               
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
         IC    RE,BCCLILEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ACTKACT(0),SVJOBMIN                                              
         GOTO1 AIO,IOACCDIR+IOREAD+IO1                                          
         BNE   VJOBN                                                            
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1             TEST CLIENT IS LOCKED                        
         LA    R1,ACTRFST                                                       
         USING LOKELD,R1                                                        
         XR    RF,RF                                                            
VJOB08   CLI   LOKEL,0                                                          
         BE    VJOBX                                                            
         CLI   LOKEL,LOKELQ                                                     
         BE    *+12                                                             
         IC    RF,LOKLN                                                         
         BXH   R1,RF,VJOB08                                                     
         TM    LOKSTAT,LOKSLOCK                                                 
         BZ    VJOBX                                                            
         MVC   FVMSGNO,=AL2(AE$CLILK)                                           
         B     VJOBX                                                            
         DROP  R1,R2                                                            
*                                                                               
VJOBN    MVC   FVMSGNO,=AL2(AE$INJOB)                                           
VJOBX    B     VALX                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CUR=ABC                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALOCUR  CLI   FVIFLD,OPTDEFQ      DEFAULT IS COMPANY CURRENCY                  
         BNE   *+10                                                             
         MVC   FVIFLD(L'CSCPYCUR),CSCPYCUR                                      
*                                                                               
         MVC   SVBILCUR,FVIFLD                                                  
         CLC   SVBILCUR,SVCURBIL   TEST ALREADY HAVE CURRENCY DETAILS           
         BE    VCUR06                                                           
*                                                                               
         CLC   CSCPYCUR,FVIFLD     TEST COMPANY CURRENCY SPECIFIED              
         BNE   VCUR04                                                           
         MVC   SVCURBIL,CSCURCPY                                                
         B     VCURX                                                            
*                                                                               
VCUR04   GOTO1 AGETCUR,BOPARM,(X'A0',SVBILCUR),SVCURBIL,,SVMINEXC,     *        
               SVMAXEXC                                                         
         BNE   VCURX                                                            
         MVC   CSBILCUR,SVBILCUR                                                
         MVC   CSCURBIL,SVCURBIL                                                
         MVI   SVDEFTYP,0          SET NO DEFAULT EXCHANGE RATE                 
         GOTO1 ASETUP,BOPARM,(X'08',0),(X'40',0),0                              
         BNE   *+16                                                             
         MVC   SVDEFRAT,CSEXCRAT                                                
         MVC   SVDEFTYP,CSTYPEXC                                                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VCUR06   TM    SVINDS,SVIXINP      TEST EXCHANGE RATE VALIDATED                 
         BZ    VCURX                                                            
         BAS   RE,TSTRATE                                                       
*                                                                               
VCURX    OI    SVINDS,SVICINP                                                   
         MVC   BCWORK(L'SVBILCUR),SVBILCUR                                      
         B     VALX                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RATE=XXXXX                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALORATE CLI   FVIFLD,OPTDEFQ      TEST SETTING DEFAULT                         
         BNE   VRATE02                                                          
         CLC   SVBILCUR,CSCPYCUR   TEST COMPANY CURRENCY                        
         BE    VRATEX                                                           
         MVC   SVEXCRAT,SVDEFRAT                                                
         MVC   SVEXCTYP,SVDEFTYP                                                
         CLI   SVEXCTYP,0          TEST HAD DEFAULT VALUE                       
         BNE   VRATE04                                                          
         MVC   FVMSGNO,=AL2(AE$EXCNF)                                           
         B     VRATEX                                                           
*                                                                               
VRATE02  GOTO1 AVALAMT,BOPARM,(X'85',FVIHDR),(6,BOWORK1)                        
         BNE   VRATEN                                                           
         SRP   BOWORK1(6),1,0                                                   
         MVC   SVEXCRAT,BOWORK1                                                 
         OC    SVEXCRAT,SVEXCRAT   TEST ZERO AMOUNT                             
         BZ    VRATEN                                                           
         MVI   SVEXCTYP,JCBXINPQ   SET USER DEFINED RATE                        
         CLC   SVEXCRAT,SVDEFRAT   UNLESS IS THE DEFAULT RATE                   
         BNE   *+10                                                             
         MVC   SVEXCTYP,SVDEFTYP                                                
*                                                                               
VRATE04  TM    SVINDS,SVICINP      TEST HAVE CURRENCY CODE                      
         BZ    VRATEX                                                           
         BAS   RE,TSTRATE                                                       
         B     VRATEX                                                           
*                                                                               
VRATEN   MVC   FVMSGNO,=AL2(AE$INEXR)                                           
VRATEX   OI    SVINDS,SVIXINP                                                   
         MVC   BCWORK(L'SVEXCVAL),SVEXCVAL                                      
         B     VALX                                                             
         EJECT                                                                  
***********************************************************************         
* TEST RATE IN VALID RANGE                                            *         
***********************************************************************         
         SPACE 1                                                                
TSTRATE  NTR1  ,                                                                
         XR    RE,RE                                                            
         IC    RE,CSCURCPY+(CURTDECP-CURTABD)                                   
         XR    RF,RF                                                            
         IC    RF,SVCURBIL+(CURTDECP-CURTABD)                                   
         SR    RF,RE                                                            
         STC   RF,SVEXCSHF         SET AGENCY --> LOCAL SHIFT VALUE             
*                                                                               
         CLC   SVMINEXC,SVEXCRAT   TEST RATE IN VALID RANGE                     
         BH    *+14                                                             
         CLC   SVMAXEXC,SVEXCRAT                                                
         BNL   TSTRATEX                                                         
         MVC   FVMSGNO,=AL2(AE$EXRNV)                                           
         GOTO1 AEDTRAT,BOPARM,(L'FVXTRA,FVXTRA),SVMINEXC,SVMAXEXC               
TSTRATEX CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALX                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE GROUP=XXXXX                                                *         
***********************************************************************         
         SPACE 1                                                                
VALOGRP  LA    R3,BCWORK                                                        
         USING SVGRP,R3                                                         
         XC    SVGRP,SVGRP                                                      
         LA    RE,FVIFLD                                                        
         XR    RF,RF                                                            
         IC    RF,FVILEN                                                        
VGRP02   TM    0(RE),X'F0'        NUMERIC                                       
         BO    VGRP04                                                           
         CLI   0(RE),X'40'        SPACES ALLOWED                                
         BNE   VGRPN                                                            
         OI    0(RE),X'F0'                                                      
VGRP04   LA    RE,1(RE)                                                         
         BCT   RF,VGRP02                                                        
         MVI   SVGRPE,X'F0'                                                     
         MVC   SVGRPE+1(L'SVGRPE-1),SVGRPE                                      
         LA    RE,FVIFLD                                                        
         IC    RF,FVILEN                                                        
         LA    R1,L'SVGRPE                                                      
         SR    R1,RF                                                            
         LA    R1,SVGRPE(R1)                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)                                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,0(0,RE)                                                   
         CVB   RF,BODUB1                                                        
         STCM  RF,3,SVGRPC                                                      
         B     VGRPX                                                            
*                                                                               
VGRPN    MVC   FVMSGNO,=AL2(AE$INAGN)                                           
VGRPX    B     VALX                                                             
         DROP  R3                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INCLUDED DSECTS                                                     *         
***********************************************************************         
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKC                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBCOLS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBCOLSC                                                     
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
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBEDD                                                       
         ORG   OSVALS                                                           
*                                                                               
*                                                                               
SVOPTS   DS    0XL(SVOPTSL2)       ** JOB OPTIONS **                            
*                                                                               
SVJOB    DS    0XL(SVJOBL)                                                      
SVJOBMIN DS    CL12                MINIMUM JOB CODE                             
SVJOBMAX DS    CL12                MAXIMUM JOB CODE                             
SVJOBL   EQU   *-SVJOBMIN                                                       
*                                                                               
SVEXCVAL DS    0XL7                                                             
SVEXCIND DS    XL1                 EXCHANGE INDICATOR                           
SVEXCRAT DS    PL5                 EXCHANGE RATE                                
SVEXCSHF DS    XL1                 EXCHANGE SHIFT                               
SVEXCTYP DS    XL1                 EXCHANGE RATE TYPE                           
*                                                                               
SVBILCUR DS    CL3                 CURRENCY CODE                                
*                                                                               
SVOPTSL1 EQU   *-SVJOB             END OF PASSED JOB OPTIONS                    
*                                                                               
SVGRP    DS    0XL(SVGRPL)                                                      
SVGRPE   DS    CL5                 GROUP (EBCDIC)                               
SVGRPC   DS    XL2                 GROUP (COMPRESSED)                           
SVGRPL   EQU   *-SVGRPE                                                         
*                                                                               
SVOPTSL2 EQU   *-SVJOB             END OF VALIDATED JOB OPTIONS                 
*                                                                               
SVCURBIL DS    XL(L'CSCURCPY)      CURRENCY TABLE ENTRY                         
SVDEFRAT DS    PL5                 DEFAULT EXCHANGE RATE                        
SVDEFTYP DS    XL1                 DEFAULT EXCHANGE RATE TYPE                   
SVMINEXC DS    PL5                 MIN VALID EXCHANGE RATE                      
SVMAXEXC DS    PL5                 MAX VALID EXCHANGE RATE                      
*                                                                               
SVENTCLI DS    XL12                ENTRY CLIENT                                 
SVENTPRO DS    XL12                ENTRY PRODUCT                                
SVENTJOB DS    XL12                ENTRY JOB                                    
*                                                                               
SVINDS   DS    XL1                 INDICATOR BYTE                               
SVICINP  EQU   X'80'               CURRENCY CODE INPUT                          
SVIXINP  EQU   X'40'               EXCHANGE RATE INPUT                          
SVIALL   EQU   X'20'               ALLOCATE SCREEN CALLED                       
SVIDEFOP EQU   X'10'               TRANSACTION OPTIONS ARE THE DEFAULT          
SVIFLTD  EQU   X'08'               ALL JOBS HAVE BEEN FILTERED                  
*                                                                               
SVNXRECN DS    XL2                 NEXT LIST RECORD TO BE PROCESSED             
SVAGN    DS    XL2                 ALLOCATION GROUP NUMBER                      
SVAMPEND DS    PL8                 AMOUNT PENDING                               
*                                                                               
SVDEFOPS DS    XL(LSFLTOPL)        DEFAULT OPTION VALUES                        
         ORG   OSVALS+OSVALSL                                                   
         SPACE 1                                                                
CLB13    CSECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050ACCLB13   08/16/00'                                      
         END                                                                    
