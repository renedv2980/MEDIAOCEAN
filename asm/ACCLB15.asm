*          DATA SET ACCLB15    AT LEVEL 135 AS OF 08/16/00                      
*PHASE T62115A                                                                  
CLB15    TITLE '- TRANSFER LIST - NEW VERSION'                                  
CLB15    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLB15*,R8,R7,R6,CLEAR=YES,RR=RE                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK         RC=A(LOCAL WORKING STORAGE)                  
         USING OVERWRK,RC                                                       
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
         EJECT                                                                  
***********************************************************************         
* FIRST FOR THIS SCREEN                                               *         
***********************************************************************         
         SPACE 1                                                                
SCRFRST  MVC   BASJOBC,BCJOBCOD                                                 
         MVC   BASJOBN,BCJOBNAM                                                 
         L     RE,=A(ALOVAL)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,ALSVALS    SET OPTION OUTPUT BASE ADDRESS               
         MVC   SFLTOPS,LSOPS       SAVE FILTERING OPTIONS FOR COMPARE           
         XC    LSOPS,LSOPS         CLEAR ALL OPTIONS                            
         XC    LSDIS,LSDIS         CLEAR DISPLAY COLUMNS                        
*                                                                               
         L     RF,AIO1             ENSURE OPTIONS READ FOR THIS LEVEL           
         USING TRNRECD,RF                                                       
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'TRNKUNT+L'TRNKLDG),BCCPYEL+(CPYPROD-CPYELD)            
         MVC   TRNKACT,BCJOBCOD                                                 
         DROP  RF                                                               
         GOTO1 AGETOPT,BODMCB,AIO1                                              
         XC    BOWORK2,BOWORK2                                                  
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         OC    BOWORK2(L'GOCBDXFR),GOCBDXFR-GOBBLOCK(RF)                        
         BNZ   *+10                                                             
         MVC   BOWORK2(DEFCLML),DEFCLM                                          
         GOTO1 AVALOPT,BOPARM,ALOTAB,BOWORK2,0                                  
         BNE   EXITL               ERROR                                        
*                                                                               
         CLC   SFLTOPS,LSOPS                                                    
         BE    EXITY                                                            
         B     EXITH               FILT OPTIONS CHANGED - RESTART               
         SPACE 1                                                                
***********************************************************************         
* LAST FOR THIS SCREEN - R1=A(FIRST FOOT)                             *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST  LR    R2,R1               DISPLAY JOB INFO                             
         LA    RF,=AL1(SCITCBTP,SCITCBAP,SCITCBWP,EOT)                          
         GOTO1 AUPDJOB,BOPARM,(C'D',(RF)),AIO4,FOOTLINL(R2)                     
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT TABLE                                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   SLL   R1,2                                                             
         B     *(R1)                                                            
         B     VALYES                                                           
         B     VALCLR                                                           
         B     VALYES              ALLOCATE AS FOR 'YES'                        
*                                                                               
VALYES   TM    TLXPEND,TLXPREV     TEST FOR REVERSAL ALLOC PENDING              
         BO    EXITN                                                            
         CP    PM$ANVBL,BCPZERO    TEST ANYTHING AVAILABLE                      
         BE    EXITN                                                            
         GOTO1 AFNDCLM,BOPARM,=AL1(PRO#XAMT)                                    
         BNE   VALYES02            TEST AMOUNT FIELD INPUT TO                   
         L     RF,0(R1)                                                         
         TM    FHIID(RF),FHIIVA                                                 
         BZ    EXITY                                                            
*                                                                               
VALYES02 LA    RF,PM$ANVBL                                                      
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    RF,PM$FNVBL                                                      
         GOTO1 XFRTRN,BOPARM,('PTATTRFT',AIO1),('PTASCASH',LSPRATA),   *        
               (RF),0                                                           
         BNE   EXITN                                                            
         B     EXITY                                                            
*                                                                               
VALCLR   TM    TLXPEND,TLXPXFR                                                  
         BNO   EXITN               SELECT ACTION INVALID                        
         ZAP   BODUB1,BCPZERO                                                   
         GOTO1 XFRTRN,BOPARM,('PTATTRFT',AIO1),('PTASCASH',LSPRATA),   *        
               (1,BODUB1),0                                                     
         BNE   EXITN                                                            
         L     RF,0(R1)            DELETE TRANSFER ELEMENT                      
         MVI   0(RF),FF                                                         
         GOTO1 VHELLO,BODMCB,(C'D',ACCMST),('FF',AIO1),0                        
         CLI   12(R1),0                                                         
         BE    EXITY                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST FOR VALIDATE LINE - R1=A(LINE)                                *         
***********************************************************************         
         SPACE 1                                                                
VALFRST  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE COLUMN - R1 CONTAINS COLUMN ROUTINE NUMBER                 *         
***********************************************************************         
         SPACE 1                                                                
VALCLM   SLL   R1,2                                                             
         SR    R4,R4                                                            
         ICM   R4,3,CSSELACT                                                    
         AR    R4,RA                                                            
*                                                                               
VALCLMA  B     VALCLMA+4(R1)                                                    
         DC    XL4'00'    00                                                    
         DC    XL4'00'    01       NET AVAILABLE                                
         B     VALXFR     02       TRANSFER AMOUNT                              
         B     VALTFT     03       TRANSFER TO DETAILS                          
         B     VALOVR     04       OVERRIDE DETAILS                             
         DC    XL4'00'    05       HOURS                                        
         DC    XL4'00'    06       COMMISSION AVAILABLE                         
         DC    XL4'00'    07       COMMISSION TRANSFERRED                       
         EJECT                                                                  
***********************************************************************         
* VALIDATE TRANSFER AMOUNT                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
         USING SUBINPH,R4                                                       
VALXFR   L     R2,AIO1                                                          
*                                                                               
         OI    FVIFLD,X'40'        ENSURE U/C                                   
         CLC   FVIFLD(1),OV@HOURS  TEST HOURS TRANSFER                          
         BNE   VALX40                                                           
         ICM   RF,1,FVXLEN         DECREASE INPUT LENGTHS                       
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$IVHRS)                                           
         B     EXITN                                                            
         STC   RF,FVILEN                                                        
         BCTR  RF,0                                                             
         STC   RF,FVXLEN                                                        
         MVC   FVIFLD(L'FVIFLD-1),FVIFLD+1                                      
         MVI   FVIFLD+(L'FVIFLD-1),C' '                                         
         GOTO1 AVALHRS,BODMCB,0,LSPRATA                                         
         BNE   EXITN                                                            
         GOTO1 XFRTRN,BOPARM,('PTATTRFT',AIO1),('PTASHOUR',LSPRATA),   X        
               (1,BODUB1),0                                                     
         BNE   EXITN                                                            
         B     EXITY                                                            
*                                                                               
VALX40   XR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         LA    RE,FVIFLD-1(RF)                                                  
         MVI   LINPCNT,C'N'                                                     
         CLI   0(RE),C'%'                                                       
         BNE   *+10                                                             
         BCTR  RF,0                REDUCE LENGTH FOR CASHVAL                    
         MVI   LINPCNT,C'Y'                                                     
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   0(R1),FF                                                         
         BE    EXITN               INPUT MUST BE STRAIGHT CASH                  
         ZAP   BODUB1,BODMCB+4(8)  SET CASH AMOUNT ALLOCATED                    
         CLI   LINPCNT,C'N'                                                     
         BE    VALX50                                                           
         ZAP   BODUB2,PM$ANVBL     NET AVAILABLE                                
         CLC   CSCPYCUR,CSBILCUR   TEST IF AGENCY CURRENCY                      
         BE    *+10                                                             
         ZAP   BODUB2,PM$FNVBL     FOREIGN NET AVAILABLE                        
         XC    BODUB1,BODUB1                                                    
         MP    BODUB1(2*L'BODUB1),BODMCB+4(8)                                   
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         ZAP   BODUB1,BODUB2                                                    
         B     VALX55                                                           
*                                                                               
VALX50   CP    BODUB1,BCPZERO      TEST IF AMOUNT IS ZERO                       
         BNE   VALX55              USE INPUT AMOUNT                             
         CP    PP$AXFER,BCPZERO    IF PREVIOUS AMOUNT WAS NON-ZERO              
         BNE   VALX55              THEN USE THE ZERO INPUT                      
         ZAP   BODUB1,PM$ANVBL     ELSE USE AMOUNT AVAILABLE                    
         CLC   CSCPYCUR,CSBILCUR   TEST IF AGENCY CURRENCY                      
         BE    *+10                                                             
         ZAP   BODUB1,PM$FNVBL     FOREIGN NET AVAILABLE                        
VALX55   GOTO1 XFRTRN,BOPARM,('PTATTRFT',AIO1),('PTASCASH',LSPRATA),   X        
               (1,BODUB1),0                                                     
         BNE   EXITN                                                            
         B     EXITY                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL ALLTRN FOR TRANSFER AMOUNT                          *         
*                                                                     *         
* NTRY: P1 = ALLTRN PARAMETER LIST                                    *         
***********************************************************************         
         SPACE 1                                                                
XFRTRN   NTR1  ,                                                                
         GOTO1 AALLTRN                                                          
         BNE   EXITN                                                            
         CP    PP$AXFER,BCPZERO                                                 
         BE    EXITY                                                            
         L     R3,0(R1)                                                         
         USING PTAELD,R3                                                        
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         CLC   LSTOWC,BCSPACES     SET WORK-CODE TO OPTION                      
         BNH   *+14                                                             
         MVC   PTATWRK,LSTOWC                                                   
         B     XFRTRN02                                                         
         CLC   PTATWRK,BCSPACES    OR CURRENT VALUE                             
         BH    XFRTRN02                                                         
         MVC   PTATWRK,TRNKWORK    OR TRANSACTION W/C                           
*                                                                               
XFRTRN02 CLC   LSTOJOB,BCSPACES    SET JOB TO OPTION                            
         BNH   *+14                                                             
         MVC   PTATJOB,LSTOJOB                                                  
         B     XFRTRN04                                                         
         CLC   PTATJOB,BCSPACES    OR CURRENT VALUE                             
         BH    XFRTRN04                                                         
         MVC   PTATJOB,TRNKACT     OR TRANSACTION ACCOUNT CODE                  
*                                                                               
XFRTRN04 XR    RF,RF               TEST FOR ALLOCATED SUB-ACTION                
         ICM   RF,3,CSSELCUR                                                    
         BZ    XFRTRNX                                                          
         A     RF,AOVERSEL                                                      
         USING SELTABD,RF                                                       
         CLC   SELTDSPN,=Y(UC@ALLOC-TWAD)                                       
         BNE   XFRTRNX                                                          
         OI    PTASTAT2,PTASXISA                                                
*                                                                               
XFRTRNX  B     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE TRANSFER TO WORKCODE/JOB                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
         USING SUBINPH,R4                                                       
VALTFT   L     R2,AIO1                                                          
         BAS   RE,GETPTAX          GET PENDING PTA ELEMENT                      
         BE    VTFT20                                                           
         CLI   FVILEN,0                                                         
         BE    EXITY                                                            
         TM    FVIIND,FVIVAL                                                    
         BO    EXITY                                                            
         MVC   FVMSGNO,=AL2(AE$ININP)                                           
         B     EXITN                                                            
         USING PTAELD,R3                                                        
VTFT20   CLI   FVILEN,0            TEST ANY INPUT                               
         BE    EXITY                                                            
*                                                                               
VTFT40   CLI   FVIFLD,C'/'         TEST FIRST INPUT IS SEPARATOR                
         BE    VTFT60              DEAL WITH JOB                                
         LA    RF,IOKEY                                                         
         USING WCORECD,RF                                                       
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   WCOKWRK,FVIFLD                                                   
         CLC   WCOKWRK,TRNKWORK    DO NOT READ IF SAME WORKCODE                 
         BE    VTFT50                                                           
         DROP  RF                                                               
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     EXITN                                                            
VTFT50   MVC   PTATWRK,FVIFLD                                                   
         OI    LSINDS1,LSIUPREC                                                 
*                                                                               
VTFT60   LA    RF,FVIFLD                                                        
         SR    RE,RE                                                            
         IC    RE,FVILEN                                                        
         CLI   0(RF),C'/'                                                       
         BE    VTFT70                                                           
         LA    RF,1(RF)                                                         
         BCT   RE,*-12                                                          
         CLI   FVILEN,L'PTATWRK    TEST LENGTH OF INPUT                         
         BE    EXITY                                                            
         MVC   FVMSGNO,=AL2(AE$FLDTL)                                           
         B     EXITN               TOO LONG FOR A WORKCODE                      
VTFT70   LA    RF,1(RF)                                                         
         BCT   RE,*+8                                                           
         B     EXITY               NOTHING AFTER THE SEPARATOR                  
*                                                                               
         LR    R1,RF               READ FOR JOB                                 
         USING ACTRECD,IOKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   SCRJOB,BCSPACES                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   SCRJOB(0),0(R1)                                                  
         CLC   TRNKACT,SCRJOB                                                   
         BE    VTFT90              DO NOT READ SAME JOB                         
         IC    RE,BCCLILEN         READ CLIENT FIRST                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ACTKACT(0),SCRJOB                                                
         MVC   FVXTRA,BCSPACES                                                  
         L     RE,AIO1             SAVE IO1                                     
         XR    RF,RF                                                            
         ICM   RF,3,TRNRLEN-TRNRECD(RE)                                         
         AH    RF,=Y(L'IODA+L'IOWORK)                                           
         SH    RE,=Y(L'IODA+L'IOWORK)                                           
         L     R0,AIO3                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 AGETACT,0                                                        
         BNE   VTFT84                                                           
         ICM   RF,15,ACAPPR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   SCROFFC,(PPRGAOFF-PPRELD)(RF)                                    
         MVC   SCRCOST,(PPRCOST-PPRELD)(RF)                                     
         SR    RE,RE                                                            
         IC    RE,BCPROLEN         READ PRODUCT                                 
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ACTKACT(0),SCRJOB                                                
         GOTO1 AGETACT,0                                                        
         BNE   VTFT84                                                           
         ICM   RF,15,ACAPPR                                                     
         BZ    VTFT72                                                           
         CLC   (PPRGAOFF-PPRELD)(L'PPRGAOFF,RF),BCSPACES                        
         BNH   *+10                                                             
         MVC   SCROFFC,(PPRGAOFF-PPRELD)(RF)                                    
         OC    (PPRCOST-PPRELD)(L'PPRCOST,RF),(PPRCOST-PPRELD)(RF)              
         BZ    VTFT72                                                           
         MVC   SCRCOST,(PPRCOST-PPRELD)(RF)                                     
VTFT72   SR    RE,RE                                                            
         IC    RE,BCJOBLEN         READ JOB                                     
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ACTKACT(0),SCRJOB                                                
         GOTO1 AGETACT,0                                                        
         BNE   VTFT84                                                           
         ICM   RF,15,ACAPPR                                                     
         BZ    VTFT74                                                           
         CLC   (PPRGAOFF-PPRELD)(L'PPRGAOFF,RF),BCSPACES                        
         BNH   *+10                                                             
         MVC   SCROFFC,(PPRGAOFF-PPRELD)(RF)                                    
         OC    (PPRCOST-PPRELD)(L'PPRCOST,RF),(PPRCOST-PPRELD)(RF)              
         BZ    VTFT74                                                           
         MVC   SCRCOST,(PPRCOST-PPRELD)(RF)                                     
VTFT74   LA    R0,JOBELQ                                                        
         GOTO1 VHELLO,BODMCB,(C'G',=C'ACCMST'),((R0),AIO1),0,0                  
         CLI   12(R1),0                                                         
         BNE   VTFT76                                                           
         L     RE,12(R1)                                                        
         TM    JOBSTA1-JOBELD(RE),JOBSXJOB                                      
         BNO   VTFT76                                                           
         MVC   FVMSGNO,=AL2(AE$CUEXJ)                                           
         B     VTFT84                                                           
VTFT76   EQU   *                                                                
*&&US*&& TM    CSBSECL,CPYBSOFF                                                 
*&&US*&& BNO   VTFT82              DO NOT CHECK OFFICE SECURITY                 
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1           CALL OFFAL TO TEST OFFIC                     
         MVC   OFFAREC,AIO1                                                     
         MVI   OFFAOPOS,LDGOPROF                                                
         MVC   OFFAOFFC,SCROFFC                                                 
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         BE    VTFT82                                                           
         MVC   FVMSGNO,=AL2(AE$SECLK)                                           
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(L'BCJOBCOD),ACTKACT                                       
         B     EXITN                                                            
                                                                                
*                                                                               
VTFT82   TM    ACSTAT1,RSTSACIC                                                 
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTCL)                                           
         B     VTFT84                                                           
         TM    ACSTAT1,RSTSACIL                                                 
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     VTFT84                                                           
         TM    ACBSTAT,ACBSABAL                                                 
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$WLACT)                                           
         B     VTFT84                                                           
*                                                                               
*&&US*&& TM    CSBSECL,CPYBSOFF                                                 
*&&US*&& BNO   VTFT86              DO NOT CHECK OFFICE SECURITY                 
         MVC   ACTKEY,BCSPACES     READ 1C ACCOUNT FOR OFFICE CHECK             
         MVC   ACTKCULA,SCRCOST                                                 
         GOTO1 AGETACT,0                                                        
         BE    VTFT86                                                           
*                                                                               
VTFT84   MVC   FVXTRA(L'ACTKULA),IOKEYSAV+(ACTKUNT-ACTKEY)                      
         LA    R2,EXITN                                                         
         B     *+8                                                              
VTFT86   LA    R2,VTFT90                                                        
         L     RE,AIO3             RESTORE IO1                                  
         XR    RF,RF                                                            
         ICM   RF,3,L'IODA+L'IOWORK+(TRNRLEN-TRNRECD)(RE)                       
         AH    RF,=Y(L'IODA+L'IOWORK)                                           
         L     R0,AIO1                                                          
         SH    R0,=Y(L'IODA+L'IOWORK)                                           
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         BR    R2                                                               
*                                                                               
VTFT90   MVC   PTATJOB,SCRJOB                                                   
         B     EXITY                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE OVERRIDE DETAILS                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
         USING SUBINPH,R4                                                       
VALOVR   L     R2,AIO1                                                          
         BAS   RE,GETPTAX          GET PENDING PTA ELEMENT                      
         BE    VOVR10                                                           
         CLI   FVILEN,0                                                         
         BE    EXITY                                                            
         TM    FVIIND,FVIVAL                                                    
         BO    EXITY                                                            
         MVC   FVMSGNO,=AL2(AE$ININP)                                           
         B     EXITN                                                            
*                                                                               
         USING PTAELD,R3                                                        
VOVR10   CLI   FVILEN,0            TEST ANY INPUT                               
         BE    EXITY               NO - GOOD                                    
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
*                                                                               
         MVI   FVINDX,2                                                         
         CLI   FVIFLD,C'/'         TEST ANY COMM OVERRIDE                       
         BE    VOVR40              NO - HANDLE SK ACCOUNT                       
         MVI   FVINDX,1                                                         
         NI    PTASTAT2,FF-PTASXCOM                                             
         CLI   FVIFLD,C'N'         ** TEMP **                                   
         BE    VOVR20                                                           
         OI    PTASTAT2,PTASXCOM                                                
         CLI   FVIFLD,C'C'         ** TEMP **                                   
         BNE   EXITN                                                            
         TM    TLXSTAT,TLXSHOUR                                                 
         BZ    VOVR20                                                           
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITN                                                            
VOVR20   OI    LSINDS1,LSIUPREC                                                 
*                                                                               
VOVR40   MVI   FVINDX,2                                                         
         LA    RF,FVIFLD                                                        
         SR    RE,RE                                                            
         IC    RE,FVILEN                                                        
         CLI   0(RF),C'/'                                                       
         BE    *+16                                                             
         LA    RF,1(RF)                                                         
         BCT   RE,*-12                                                          
         B     EXITY                                                            
         LA    RF,1(RF)                                                         
         BCT   RE,*+8                                                           
         B     EXITN                                                            
         CLC   =C'SK',TRNKULC      CANNOT OVERRIDE IF NOT SK CONTRA             
         BNE   EXITN                                                            
*                                                                               
VOVR60   LR    R1,RF               READ FOR SKACCOUNT                           
         LA    RF,IOKEY                                                         
         USING ACTRECD,RF                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),=C'SK'                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ACTKACT(0),0(R1)                                                 
         CLC   ACTKEY(ACTKEND),TRNKEY                                           
         BE    VOVR80              DO NOT READ SAME ACCOUNT                     
         MVC   FVXTRA,BCSPACES                                                  
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     VOVR75                                                           
         LA    RF,IOKEY                                                         
         TM    ACTKSTAT,ACTSLOCK                                                
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     VOVR75                                                           
         TM    ACTKSTAT,ACTSCLOS                                                
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTCL)                                           
         B     VOVR75                                                           
         TM    ACTKSTAT,ACTSABLP                                                
         BO    VOVR80                                                           
         MVC   FVMSGNO,=AL2(AE$WLACT)                                           
         B     VOVR75                                                           
         DROP  RF                                                               
*                                                                               
VOVR75   MVC   FVXTRA(L'ACTKULA),IOKEYSAV+(ACTKUNT-ACTKEY)                      
         B     EXITN                                                            
*                                                                               
VOVR80   CLI   PTALN,PTATLN2Q      TEST LONG ELEMENT                            
         BE    VOVR90                                                           
         SR    RE,RE                                                            
         IC    RE,PTALN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BOELEM(0),PTAEL     SAVE ELEMENT                                 
         MVI   PTAEL,FF            DELETE SHORT ELEMENT                         
         GOTO1 VHELLO,BODMCB,(C'D',ACCMST),('FF',AIO1),0                        
         CLI   BODMCB+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   BOELEM+(PTALN-PTAELD),PTATLN2Q                                   
         GOTO1 (RF),(R1),(C'P',ACCMST),AIO1,BOELEM                              
         CLI   BODMCB+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,BODMCB+16                                                     
VOVR90   MVC   PTATSKAC,IOKEY+(ACTKACT-ACTKEY)                                  
         OI    LSINDS1,LSIUPREC                                                 
         B     EXITY                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* LAST FOR VALIDATE LINE                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         USING PTAELD,R3                                                        
VALLAST  TM    LSINDS1,LSICLMIN+LSIUPREC                                        
         BZ    EXITY                                                            
         L     R2,AIO1                                                          
         BAS   RE,GETPTAX          GET PENDING PTA ELEMENT                      
         BE    *+10                                                             
         SR    R3,R3                                                            
         B     VALL10                                                           
         CP    PTANET,BCPZERO      IF ZERO BYPASS CHANGE TESTS                  
         BE    VALL10                                                           
*                                                                               
         CLC   PTATWRK,TRNKWORK    WC/JOB/COMMSTAT/SKACC MUST CHANGE            
         BNE   VALL10                                                           
         CLC   PTATJOB,TRNKACT                                                  
         BNE   VALL10                                                           
         LA    RE,X'10'            SET BO                                       
         TM    PTASTAT2,PTASXCOM   TEST TRANSFER IS COMMISSIONABLE              
         BO    *+8                 YES - BRANCH IF ORIGINAL IS NOT              
         LA    RE,X'80'            SET BZ                                       
         TM    TRNSTAT,TRNSNOCM    TEST ORIGINAL IS NON-COMMISIONABLE           
         EX    RE,*+4                                                           
         NOP   VALL10                                                           
         CLI   PTALN,PTATLN2Q                                                   
         BNE   *+14                                                             
         CLC   PTATSKAC,TRNKACT    TEST SK ACCOUNT HAS CHANGED                  
         BNE   VALL10                                                           
         MVC   FVMSGNO,=AL2(AE$WJSSK)                                           
         B     EXITN                                                            
*                                                                               
VALL10   OI    LSINDS1,LSIUPREC                                                 
         LTR   R3,R3                                                            
         BZ    VALLX                                                            
         CP    PTANET,BCPZERO                                                   
         BNE   VALLX                                                            
         MVI   PTAEL,FF            IF NO TRANSFER AMOUNT DELETE ELEM            
         GOTO1 VHELLO,BODMCB,(C'D',ACCMST),('FF',AIO1),0                        
         CLI   BODMCB+12,0                                                      
         BE    VALLX                                                            
         DC    H'0'                                                             
VALLX    B     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMN - R1 CONTAINS COLUMN ROUTINE NUMBER                  *         
***********************************************************************         
         SPACE 1                                                                
DISCLM   ST    R2,ALINTAB          A(CURRENT COLUMN ENTRY)                      
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     EXITN      00       DISPLAY FIRST ROUTINE                        
         B     DISNET     01       NET AVAILABLE                                
         B     DISXFR     02       TRANSFER AMOUNT                              
         B     DISTFT     03       TRANSFER TO DETAILS                          
         B     DISOVR     04       OVERRIDE DETAILS                             
         B     DISHRS     05       HOURS                                        
         B     DISCMNA    06       COMMISSION AVAILABLE                         
         B     DISCMNX    07       COMMISSION TRANSFERRED                       
         EJECT                                                                  
***********************************************************************         
* DISPLAY NET AMOUNT AVAILABLE                                        *         
***********************************************************************         
         SPACE 1                                                                
DISNET   NI    FVIHDR+FHATD,FF-FHATHI                                           
         CP    PP$AXFER,BCPZERO                                                 
         BE    *+8                                                              
         OI    FVIHDR+FHATD,FHATHI                                              
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY TRANSFER AMOUNT PENDING                                     *         
***********************************************************************         
         SPACE 1                                                                
DISXFR   NI    FVIHDR+FHATD,FF-FHATHI                                           
         CP    PP$AXFER,BCPZERO                                                 
         BE    *+8                                                              
         OI    FVIHDR+FHATD,FHATHI                                              
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY TRANSFER TO WORKCODE/JOB                                    *         
***********************************************************************         
         SPACE 1                                                                
DISTFT   NI    FVIHDR+FHATD,FF-FHATHI                                           
         ZAP   BODUB1,PP$AXFER     TEST TRANSFER PENDING                        
         BZ    EXIT                NO - NO WORKCODE TO SHOW                     
         BAS   RE,GETPTAX          GET PENDING PTA ELEMENT                      
         BNE   EXIT                COULDN'T FIND - GET OUT                      
         USING PTAELD,R3                                                        
         MVC   FVIFLD(L'PTATWRK),PTATWRK                                        
         MVI   FVIFLD+L'PTATWRK,C'/'                                            
         MVC   FVIFLD+L'PTATWRK+1(L'PTATJOB),PTATJOB                            
         OI    FVIHDR+FHATD,FHATHI                                              
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY OVERRIDE DETAILS                                            *         
***********************************************************************         
         SPACE 1                                                                
DISOVR   NI    FVIHDR+FHATD,FF-FHATHI                                           
         ZAP   BODUB1,PP$AXFER     TEST TRANSFER PENDING                        
         BZ    EXITY               NO - NO WORKCODE TO SHOW                     
         BAS   RE,GETPTAX          GET PENDING PTA ELEMENT                      
         BNE   EXITY               COULDN'T FIND - GET OUT                      
         USING PTAELD,R3                                                        
         MVI   FVIFLD,C'C'                                                      
         TM    PTASTAT2,PTASXCOM                                                
         BO    *+8                                                              
         MVI   FVIFLD,C'N'                                                      
         CLI   PTALN,PTATLN2Q                                                   
         BNE   DOVR20                                                           
         MVI   FVIFLD+1,C'/'                                                    
         MVC   FVIFLD+2(L'PTATSKAC),PTATSKAC                                    
DOVR20   OI    FVIHDR+FHATD,FHATHI                                              
         B     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY HOURS AVAILABLE                                             *         
***********************************************************************         
         SPACE 1                                                                
DISHRS   NI    FVIHDR+FHATD,FF-FHATHI                                           
         CP    PP$AXFER,BCPZERO                                                 
         BZ    *+8                                                              
         OI    FVIHDR+FHATD,FHATHI                                              
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY COMMISSION AVAILABLE                                        *         
***********************************************************************         
         SPACE 1                                                                
DISCMNA  NI    FVIHDR+FHATD,FF-FHATHI                                           
         CP    PP$AXFER,BCPZERO                                                 
         BZ    *+8                                                              
         OI    FVIHDR+FHATD,FHATHI                                              
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY COMMISSION TRANSFERRED                                      *         
***********************************************************************         
         SPACE 1                                                                
DISCMNX  EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIND PENDING TRANSFER ELEMENT                                       *         
***********************************************************************         
         SPACE 1                                                                
GETPTAX  L     R3,AIO1             FIND WRITE-OFF ACTIVITY ELEMENT              
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         SR    R0,R0                                                            
         USING PTAELD,R3                                                        
GXPTA10  CLI   PTAEL,0                                                          
         BE    GXPTAN                                                           
         CLI   PTAEL,PTAELQ                                                     
         BE    GXPTA30                                                          
GXPTA20  IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         B     GXPTA10                                                          
GXPTA30  CLI   PTATYPE,PTATTRFT    TEST TRANSFER-TO                             
         BNE   GXPTA20                                                          
         TM    PTASTAT1,PTASPEND   TEST PENDING                                 
         BNO   GXPTA20                                                          
*                                                                               
GXPTAY   CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
GXPTAN   LTR   RB,RB                                                            
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
LSTFRST  LA    R2,IOKEY            INITIALISE KEY                               
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'TRNKUNT+L'TRNKLDG),BCCPYEL+(CPYPROD-CPYELD)            
         MVC   TRNKACT,BCJOBCOD                                                 
         MVC   CSBSECL,BCCPYEL+(CPYBSEC-CPYELD)                                 
         MVCDD FVIFLD(3),AC#TODAY                                               
         GOTO1 VDICTAT,BCPARM,C'SU  ',FVIFLD,0                                  
         GOTO1 VBMONVAL,BCPARM,(3,FVIFLD),(34,ACOM),(CULANG,BOWORK1),  X        
               (CUABIN,0)                                                       
         MVC   CSBSECL,0(R1)                                                    
         GOTO1 VDICTAT,BCPARM,C'LU  ',OVDUCI,OVDUCO                             
         B     EXIT                                                             
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
*        CLC   BCJOBCOD,TRNKACT    TEST SAME JOB                                
*        BNE   EXITN               MUST HAVE REACHED THE END                    
         GOTO1 AIO,(R1)                                                         
         BNE   EXITN                                                            
         CLC   TRNKCULA,IOKEYSAV+TRNKCULA-TRNKEY                                
         BNE   EXITN                                                            
         CLI   TRNKSTYP,47         DON'T ALLOW TRANSFER OF EP                   
         BE    GETNEXT                                                          
         CLC   TRNKDATE,BCSPACES                                                
         BE    GETNEXT             NOT TRANSACTION RECORD                       
         GOTO1 ASETTRN,BOPARM,(C'D',IOKEY)                                      
         BNE   GETNEXT                                                          
         TM    TLXVAL,TLXVXFR                                                   
         BZ    GETNEXT                                                          
*                                                                               
         GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETN130  GOTO1 ASETTRN,BOPARM,(C'M',AIO1)                                       
         BNE   GETNEXT                                                          
*                                                                               
GETNOK   B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* CALCULATE COMMISSION AMOUNT - BODUB1 CONTAINS NET AMOUNT            *         
***********************************************************************         
         SPACE 1                                                                
SETCOM   NTR1                                                                   
         MVI   BOBYTE1,0           0 MEANS INCOME CALCULATED                    
         BAS   RE,GETCOM           CALCULATE COMMISSION AMOUNT                  
*                                                                               
         LA    RE,LSLIN                                                         
         USING LINTABD,RE                                                       
SETC10   CLI   LINTABD,EOT         TEST IF COMMISSION FIELD INPUT               
         BE    SETCX                                                            
         TM    LININDS,LINIINP                                                  
         BNO   SETC15              NOT AN INPUT FIELD                           
         SR    R3,R3                                                            
         IC    R3,LINHDR                                                        
         AR    R3,R4                                                            
         USING FHD,R3                                                           
         TM    FHII,FHIIVA         DO NOT BOTHER IF PREV VALIDATED              
         BNO   SETC20                                                           
SETC15   LA    RE,LINTABL(RE)                                                   
         B     SETC10                                                           
SETC20   L     R3,ACLMHEAD                                                      
         AH    R3,LINCLM                                                        
         USING CLMTABD,R3                                                       
         CLI   CLMCHAR,C'F'        COMMISSION COLUMN                            
         BNE   SETC15                                                           
         ZAP   BODUB2,BCPZERO      DO NOT ALLOCATE CALCULATED COMM              
         MVI   BOBYTE1,1           1 MEANS INCOME INPUT                         
SETCX    B     EXIT                                                             
         DROP  R3                                                               
         DROP  RE                                                               
*                                                                               
GETCOM   L     RF,AGOPBLK          CALC COMM ON ALLOCATED NET AMT.              
         ZAP   BOPL81(16),BODUB1                                                
         SRP   BOPL81(16),2,0                                                   
         MP    BOPL81(16),GOAGYCOM-GOBLOCKD(L'GOAGYCOM,RF)                      
         SRP   BOPL81(16),64-8,5                                                
         ZAP   BODUB2,BOPL81(16)                                                
         BR    RE                                                               
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
ACCMST   DC    C'ACCMST  '                                                      
*                                                                               
DEFCLM   EQU   *                                                                
*        DC    AL1(XFR#AMTO)                                                    
         DC    AL1(XFR#TFTO)                                                    
         DC    AL1(XFR#OVRO)                                                    
         DC    AL1(EOT)                                                         
DEFCLML  EQU   *-DEFCLM                                                         
*                                                                               
FF       EQU   X'FF'                                                            
***********************************************************************         
* OPTION TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
ALOTAB   DS    0X                                                               
*                                  DISPLAY=COLUMN CODES                         
         DC    AL2(UC8DSP-TWAD,UC3DSP-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,L'LSDISLST,L'LSDISLST)                           
         DC    AL1((*-ALOTAB)/OPTTABL)                                          
         DC    AL2(OPTDISQ,LSDIS-LSVALSD)                                       
         DC    CL4'+'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  GENERAL TRANSACTION OPTIONS                  
         DC    AL2(TRNOPTQ)                                                     
*                                                                               
*                                  SWITCH=Y/N                                   
         DC    AL2(UC8SWCH-TWAD,UC3SWCH-TWAD)                                   
         DC    AL1(OPTNRTN+OPTDFLTO,0)                                          
         DC    AL1(0,0,0,0,0,1,4,L'LSSWITCH)                                    
         DC    AL1((*-ALOTAB)/OPTTABL)                                          
         DC    AL2(OPTYNQ,LSSWITCH-LSVALSD)                                     
         DC    CL4'N'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  TOJOB=CLIPROJOB                              
         DC    AL2(UC@TOJOB-TWAD,UC@TOJOB-TWAD)                                 
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,L'LSTOJOB,L'LSTOJOB)                             
         DC    AL1((*-ALOTAB)/OPTTABL)                                          
         DC    AL2(1,LSTOJOB-LSVALSD)                                           
         DC    XL4'00'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  TOWC=WC                                      
         DC    AL2(UC@TOWC-TWAD,UC@TOWC-TWAD)                                   
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,L'LSTOWC,L'LSTOWC)                               
         DC    AL1((*-ALOTAB)/OPTTABL)                                          
         DC    AL2(2,LSTOWC-LSVALSD)                                            
         DC    XL4'00'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
ALOTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
* OVERLAY UPPER CASE DICTIONARY                                     *           
*********************************************************************           
         SPACE 1                                                                
OVDUCI   DS    0X                                                               
         DCDDL AC#HOURS,3,LABEL=OV@HOURS                                        
         DC    X'00'                                                            
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
         B     VALTOJOB            VALIDATE TO-JOB CODE                         
         B     VALTOWC             VALIDATE TO-WORKCODE                         
         SPACE 1                                                                
VALX     XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE TOJOB=CLIPROJOB                                            *         
***********************************************************************         
         SPACE 1                                                                
VALTOJOB XR    RE,RE                                                            
         IC    RE,FVILEN                                                        
         BCTR  RE,0                                                             
         LA    RF,IOKEY                                                         
         USING ACTRECD,RF                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
         EX    RE,*+4                                                           
         MVC   ACTKACT(0),FVIFLD                                                
         EX    RE,*+4                                                           
         MVC   BCWORK(L'ACTKACT),ACTKACT                                        
         CLC   ACTKACT,BCJOBCOD                                                 
         BNE   *+14                SAME JOB IS INVALID                          
         MVC   FVMSGNO,=AL2(AE$INJOB)                                           
         B     VALX                                                             
         MVC   FVXTRA,BCSPACES                                                  
         DROP  RF                                                               
         GOTO1 AGETACT,0                                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INJOB)                                           
         B     VALX                                                             
         GOTO1 VHELLO,BOPARM,(C'G',=C'ACCMST '),('JOBELQ',AIO1),0               
         CLI   12(R1),0                                                         
         BNE   VTOJOB02                                                         
         L     RE,12(R1)                                                        
         TM    JOBSTA1-JOBELD(RE),JOBSXJOB                                      
         BNO   VTOJOB02                                                         
         MVC   FVMSGNO,=AL2(AE$CUEXJ)                                           
         B     VALX                                                             
VTOJOB02 TM    ACBSTAT,ACBSCLSE                                                 
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$JOBCL)                                           
         B     VALX                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$JOBLK)                                           
         B     VALX                                                             
         TM    ACINDS1,ACIPRJOB                                                 
         BO    VALX                                                             
         MVC   FVMSGNO,=AL2(AE$WLACT)                                           
         B     VALX                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE TOWC=WORKCODE                                              *         
***********************************************************************         
         SPACE 1                                                                
VALTOWC  XR    RE,RE                                                            
         IC    RE,FVILEN                                                        
         BCTR  RE,0                                                             
         LA    RF,IOKEY                                                         
         USING WCORECD,RF                                                       
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'BCCPYPRD),BCCPYPRD                                     
         EX    RE,*+4                                                           
         MVC   WCOKWRK(0),FVIFLD                                                
         EX    RE,*+4                                                           
         MVC   BCWORK(0),FVIFLD                                                 
         DROP  RF                                                               
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    VALX                                                             
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     VALX                                                             
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
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
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
       ++INCLUDE ACCLBWORK                                                      
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF1D                                                       
         ORG   OSVALS                                                           
SCRJOB   DS    CL12                SAVED TO JOB CODE                            
SCRCOFFC DS    CL2                 TO JOB CLIENT OFFICE                         
SCRPOFFC DS    CL2                 TO JOB PRODUCT OFFICE                        
SCRJOFFC DS    CL2                 TO JOB JOB OFFICE                            
SCROFFC  DS    CL(L'PPRGAOFF)      TO JOB COMPOSITE OFFICE                      
SCRCOST  DS    CL(L'PPRCOST)       TO JOB COMPOSITE COSTING ACCOUNT             
LINPCNT  DS    CL1                                                              
LINEINDS DS    XL1                 LINE INDICATORS                              
LINENETV EQU   X'80'               NET COLUMN INPUT (IGNORE HOURS)              
LINEHRSV EQU   X'40'               HOURS COLUMN INPUT (IGNORE NET)              
*                                                                               
OVDUCO   DS    0X                                                               
         DSDDL PRINT=YES                                                        
*                                                                               
         ORG   OSVALS+OSVALSL                                                   
*                                                                               
         SPACE 1                                                                
WORKD    DSECT                                                                  
         ORG   OVERWRK                                                          
SFLTOPS  DS    CL(LSFLTOPL)        SAVED FILTERING OPTIONS                      
ALINTAB  DS    A                                                                
*                                                                               
SCANBYTE DS    CL1                                                              
SCANBLK  DS    5CL(SCBLKLQ)                                                     
*                                                                               
         DS    XL((OVERWRKL)-(*-SFLTOPS))                                       
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'135ACCLB15   08/16/00'                                      
         END                                                                    
