*          DATA SET ACCLB18    AT LEVEL 085 AS OF 08/16/00                      
*PHASE T62118A                                                                  
CLB18    TITLE '- REVERSE'                                                      
CLB18    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB18**,R8,R7,R6,CLEAR=YES,RR=RE                              
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
         TM    BCINDS2,BCINTRS     WHEN FIRST FOR LIST                          
         BZ    *+10                SAVE DA OF SELECTED BILL                     
         MVC   SCR99DA,TLDA                                                     
         XC    SCR99CNT,SCR99CNT                                                
SCRF010  L     RE,=A(ALOVAL)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,ALSVALS    SET OUTPUT BASE ADDRESS                      
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
         L     RE,AGOPBLK                                                       
         L     RE,GOABEXT-GOBLOCK(RE)                                           
         XC    BOWORK2,BOWORK2                                                  
         LA    RF,BOWORK2                                                       
         SR    R0,R0                                                            
SCRF018  LA    RF,BOWORK2                                                       
         OC    0(L'GOCBDREV,RF),GOCBDREV-GOBBLOCK(RE)                           
         BNZ   *+8                                                              
         LA    RF,DEFCLMR                                                       
         ICM   RF,8,=X'80'                                                      
SCRF020  GOTO1 AVALOPT,BOPARM,ALOTAB,(RF),(R0)                                  
         BE    SCRF022                                                          
         MVC   LSOPS(L'SFLTOPS),LSOPS   RESTORE OLD FILTERS IF ERROR            
         B     EXITL                                                            
*                                                                               
SCRF022  CLC   SFLTOPS,LSOPS                                                    
         BE    EXITY                                                            
         B     EXITH               FILT OPTIONS CHANGED - RESTART               
         EJECT                                                                  
***********************************************************************         
* LAST FOR THIS SCREEN - R1=A(FIRST FOOT)                             *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST  LR    R2,R1               DISPLAY JOB INFO                             
         LA    RF,=AL1(SCITCBAP,SCITCBWP,SCITCBTP,EOT)                          
         GOTO1 AUPDJOB,BOPARM,(C'D',(RF)),AIO4,FOOTLINL(R2)                     
*                                                                               
         OC    SCR99CNT,SCR99CNT   ANY CHANGE TO COUNT THIS SCREEN?             
         BZ    SCRLASTX                                                         
         MVC   IODAOVER,SCR99DA    GET BILL POSTING                             
         GOTO1 AIO,IOGET+IOLOCK+IOACCMST+IO3                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO3                                                          
         LA    RF,TRNRFST-TRNRECD(RF)                                           
         SR    R0,R0                                                            
         USING RATELD,RF                                                        
SCRL02   IC    R0,RATLN                                                         
         AR    RF,R0                                                            
         CLI   RATEL,0                                                          
         BE    SCRL06                                                           
         CLI   RATEL,RATETAXQ                                                   
         BNE   SCRL02                                                           
         NI    RATRATE,FF-X'80'                                                 
         ICM   RE,15,SCR99CNT      THIS SCREEN COUNT OF REVERSALS               
         AH    RE,RATRATE          ADD EXISTING COUNT                           
         STCM  RE,3,RATRATE                                                     
         B     SCRL08                                                           
SCRL06   LA    RF,BOELEM                                                        
         MVI   RATEL,TRNELQ+1      ENSURE NEW ELEMENT GOES AFTER TRNEL          
         MVI   RATLN,RATLNQ                                                     
         MVC   RATRATE,SCR99CNT+2  THIS SCREEN COUNT OF REVERSALS               
         GOTO1 VHELLO,BODMCB,(C'P',=C'ACCMST '),AIO3,BOELEM,0,0                 
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,16(R1)           PICK-UP A(ELEMENT) IN RECORD                 
         MVI   RATEL,RATETAXQ      AND SET CORRECT ELEMENT CODE                 
SCRL08   SR    RE,RE                                                            
         ICM   RE,3,CSHIRECN       GET HIGH TSAR RECORD NUMBER                  
         SR    R0,R0                                                            
         ICM   R0,3,CSPSRECN       HIGH TSAR PREVIOUS SESSION                   
         SR    RE,R0                                                            
         CLM   RE,3,RATRATE        TEST EVERYTHING MARKED FOR REVERSAL          
         BNE   *+8                                                              
         OI    RATRATE,X'80'                                                    
         MVC   IODAOVER,SCR99DA    WRITE BACK UPDATED BILL POSTING              
         GOTO1 AIO,IOPUT+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SCRLASTX B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT TABLE                                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   SLL   R1,2                                                             
         B     *(R1)                                                            
         B     VALYES                                                           
         B     VALCLR                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE YES                                                        *         
***********************************************************************         
         SPACE 1                                                                
VALYES   CP    PP$AALLO,BCPZERO    TEST ANY PENDING ALLOCATION                  
         BE    VYES02                                                           
         TM    PG$STAT,PG$REVS     TEST PENDING ALLOC IS REVERSAL               
         BO    VYES02              YES - ADD THIS ALLOCATION TO IT              
         ZAP   BODUB1,BCPZERO      CLEAR NON-REVERSAL ALLOCATION                
         ZAP   BODUB2,BCPZERO                                                   
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO1),('PTASCASH',LSPRATA),   X        
               (1,BODUB1),(1,BODUB2)                                            
         BNE   EXITN                                                            
*                                                                               
VYES02   BAS   RE,GETPTREV         GET PTAEL FOR REVERSAL                       
         USING PTAELD,R3                                                        
         TM    PTASTAT1,PTASREVD   TEST ALREADY REVERSED                        
         BO    EXITN                                                            
         OI    PTASTAT1,PTASREVD   SET REVERSAL STATUS ON ORIG BILL             
         ZAP   BODUB1,PTANET       SET NET AND COMMISSION                       
         ZAP   BODUB2,PTARCOM                                                   
         CLC   CSCPYCUR,CSBILCUR   TEST FOREIGN CURRENCY BILL                   
         BE    *+16                                                             
         ZAP   BODUB1,PTANETF      SET FOREIGN NET AND COMMISSION               
         ZAP   BODUB2,PTARFCOM                                                  
         MP    BODUB1,=P'-1'                                                    
         MP    BODUB2,=P'-1'                                                    
         DROP  R3                                                               
*                                                                               
         LA    R0,PTASCASH+PTASREVS                                             
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO1),((R0),LSPRATA),         X        
               (0,BODUB1),(0,BODUB2)                                            
         BNE   EXITN                                                            
         ICM   RF,15,SCR99CNT      INCREMENT SCREEN COUNT OF REVERSALS          
         LA    RF,1(RF)                                                         
         STCM  RF,15,SCR99CNT                                                   
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* CLEAR PENDING ALLOCATION                                            *         
***********************************************************************         
         SPACE 1                                                                
VALCLR   TM    TLXPEND,TLXPREV                                                  
         BNO   EXITN                                                            
         CP    PP$AALLO,BCPZERO                                                 
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NOALL)                                           
         BE    EXITN               NOTHING TO CLEAR                             
*                                                                               
         ZAP   BODUB1,BCPZERO                                                   
         ZAP   BODUB2,BCPZERO                                                   
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO1),('PTASCASH',LSPRATA),   X        
               (1,BODUB1),(1,BODUB2)                                            
         BNE   EXITN                                                            
         L     R3,AIO1             REMOVE REVERSAL PENDING STATUS               
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         SR    R0,R0                                                            
VCLR02   IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         USING PTAELD,R3                                                        
         CLI   PTAEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PTAEL,PTAELQ                                                     
         BNE   VCLR02                                                           
         CLI   PTATYPE,PTATRAL                                                  
         TM    PTASTAT1,PTASPEND                                                
         BNO   VCLR02                                                           
         NI    PTASTAT1,FF-PTASREVS                                             
*                                                                               
         BAS   RE,GETPTREV         REMOVE REVERSAL STAT FROM ORIGINAL           
         NI    PTASTAT1,FF-PTASREVD                                             
         DROP  R3                                                               
         ICM   RF,15,SCR99CNT                                                   
         BCTR  RF,0                                                             
         STCM  RF,15,SCR99CNT      REDUCE SCREEN COUNT OF REVERSALS             
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR VALIDATE LINE - R1=A(LINE)                                *         
***********************************************************************         
         SPACE 1                                                                
VALFRST  B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE COLUMN - R1 CONTAINS COLUMN ROUTINE NUMBER                 *         
***********************************************************************         
         SPACE 1                                                                
VALCLM   B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR VALIDATE LINE                                              *         
***********************************************************************         
         SPACE 1                                                                
VALLAST  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMN - R1 CONTAINS COLUMN ROUTINE NUMBER                  *         
***********************************************************************         
         SPACE 1                                                                
DISCLM   SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     EXITN       00      FIRST TIME CALL                              
         B     DISNET      01      NET AVAILABLE                                
         B     DISALLC     02      AMOUNT ALLOCATED                             
         B     DISCMN      03      COMMISSION                                   
         B     DISHRS      04      HOURS                                        
         EJECT                                                                  
***********************************************************************         
* DISPLAY NET (UNALLOCATED) AMOUNT                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING PTAELD,R3                                                        
DISNET   BAS   RE,GETPTREV                                                      
         ZAP   BODUB1,BCPZERO                                                   
         TM    PTASTAT1,PTASREVD   TEST ALREADY MARKED FOR REVERSAL             
         BO    DISAMNT             THEN SHOW ZERO AVAILABLE                     
         ZAP   BODUB1,PTANET                                                    
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+10                                                             
         ZAP   BODUB1,PTANETF                                                   
         MP    BODUB1,=P'-1'                                                    
         B     DISAMNT                                                          
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY AMOUNT ALLOCATED                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING PTAELD,R3                                                        
DISALLC  BAS   RE,GETPTREV                                                      
         ZAP   BODUB1,BCPZERO                                                   
         TM    PTASTAT1,PTASREVD   TEST THIS BILL REVERSED                      
         BZ    DISAMNT             NO - SHOW ZERO ALLOCATED                     
         ZAP   BODUB1,PTANET       SHOW ORIG BILL AMOUNT AS ALLOCATED           
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+10                                                             
         ZAP   BODUB1,PTANETF                                                   
         MP    BODUB1,=P'-1'                                                    
         B     DISAMNT                                                          
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY COMMISSION (UNALLOCATED) AMOUNT                             *         
***********************************************************************         
         SPACE 1                                                                
         USING PTAELD,R3                                                        
DISCMN   BAS   RE,GETPTREV                                                      
         ZAP   BODUB1,BCPZERO                                                   
         TM    PTASTAT1,PTASREVD   TEST REVERSAL ALREADY ALLOCATED              
         BO    DISAMNT             SHOW ZERO COMMISSION AVAILABLE               
         ZAP   BODUB1,PTARCOM                                                   
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+10                                                             
         ZAP   BODUB1,PTARFCOM                                                  
         MP    BODUB1,=P'-1'                                                    
         B     DISAMNT                                                          
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY HOURS                                                       *         
***********************************************************************         
         SPACE 1                                                                
DISHRS   ZAP   BODUB2,PM$HRVBL     SET NUMBER OF HOURS AVAILABLE                
*                                                                               
         USING PTAELD,R3                                                        
         BAS   RE,GETPTREV                                                      
         ZAP   BODUB2,BCPZERO                                                   
         TM    PTASTAT1,PTASREVD   TEST REVERSED                                
         BO    DISH200             DISPLAY ZERO HOURS AVAILABLE                 
         SR    RE,RE                                                            
         ICM   RE,3,PTAHOURS       DISPLAY HOURS ON ORIGINAL BILL               
         CVD   RE,BODUB2                                                        
*                                                                               
DISH200  GOTO1 AEDTAMT,BOPARM,BODUB2,0                                          
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY CURRENCY AMOUNT                                             *         
***********************************************************************         
         SPACE 1                                                                
DISAMNT  GOTO1 AEDTAMT,BOPARM,BODUB1,BODUB1                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* FIND MATCHIN PTAEL                                                  *         
***********************************************************************         
         SPACE 1                                                                
GETPTREV L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST                                                       
         USING PTAELD,R3                                                        
         SR    R0,R0                                                            
GETP10   IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   PTAEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PTAEL,PTAELQ                                                     
         BNE   GETP10                                                           
         CLI   PTATYPE,PTATRAL                                                  
         TM    PTASTAT1,PTASPEND                                                
         BO    GETP10                                                           
         CLC   PTARBLNO,CSBILNUM                                                
         BNE   GETP10                                                           
         BR    RE                                                               
         DROP  R3,R2                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
LSTFRST  LA    R2,IOKEY            INITIALISE KEY                               
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,BCJOBCOD                                                 
*                                                                               
         B     EXITY                                                            
         DROP  R2                                                               
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
         GOTO1 AIO,(R1)                                                         
         BNE   EXITN                                                            
         CLC   TRNKCULA,IOKEYSAV+TRNKCULA-TRNKEY                                
         BNE   EXITN                                                            
         CLC   TRNKDATE,BCSPACES   NOT TRANSACTION RECORD                       
         BE    GETNEXT                                                          
*                                                                               
GETN100  GOTO1 ASETTRN,BOPARM,(C'D',TRNRECD)                                    
         BNE   GETNEXT             FILTER DIRECTORY RECORD                      
*                                                                               
         GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         USING TRNELD,TRNRFST                                                   
*                                                                               
GETN110  LA    RE,TRNELD                                                        
         SR    R0,R0                                                            
         USING PTAELD,RE                                                        
GETN120  CLI   PTAEL,0             TEST E-O-R                                   
         BE    GETNEXT                                                          
         CLI   PTAEL,PTAELQ        FIND ACTIVITY ELEMENT                        
         BNE   GETN140                                                          
         CLI   PTATYPE,PTATRAL     CHECK ALLOCATED TO BILL                      
         BNE   GETN140                                                          
         TM    PTASTAT1,PTASPEND   TEST IF STILL PENDING UPDATE                 
         BO    GETN140                                                          
         CLC   PTARBLNO,CSBILNUM   CHECK MATCHES BILL BEING REVERSED            
         BNE   GETN140                                                          
         TM    PTASTAT1,PTASREVU   TEST REVERSAL OF BILL IS UPDATED             
         BO    GETNEXT                                                          
         B     GETN150                                                          
GETN140  IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     GETN120                                                          
         DROP  RE                                                               
*                                  GENERAL FILTERS                              
GETN150  GOTO1 ASETTRN,BOPARM,(C'M',TRNRECD)                                    
         BNE   GETNEXT                                                          
*                                                                               
GETNEXTX B     EXITY                                                            
         DROP  R2                                                               
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
*                                                                               
DEFCLMR  EQU   *                                                                
         DC    AL1(PRO#NET)                                                     
         DC    AL1(PRO#COM)                                                     
         DC    AL1(PRO#ALC)                                                     
         DC    AL1(EOT)                                                         
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
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
*&&UK                                                                           
*                                  SWITCH=Y/N                                   
         DC    AL2(UC8SWCH-TWAD,UC3SWCH-TWAD)                                   
         DC    AL1(OPTNRTN+OPTDFLTO,0)                                          
         DC    AL1(0,0,0,0,0,1,4,L'LSSWITCH)                                    
         DC    AL1((*-ALOTAB)/OPTTABL)                                          
         DC    AL2(OPTYNQ,LSSWITCH-LSVALSD)                                     
         DC    CL4'N'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*&&                                                                             
*&&US                                                                           
*                                  SWITCH=Y/N (REVERSE)                         
         DC    AL2(UC8SWCH-TWAD,UC3SWCH-TWAD)                                   
         DC    AL1(OPTNRTN+OPTDFLTO,0)                                          
         DC    AL1(0,0,0,ACTREV,0,1,4,L'LSSWITCH)                               
         DC    AL1((*-ALOTAB)/OPTTABL)                                          
         DC    AL2(OPTNYQ,LSSWITCH-LSVALSD)                                     
         DC    CL4'N'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*&&                                                                             
*                                                                               
ALOTABX  DC    AL1(EOT)                                                         
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
         SPACE 1                                                                
VALX     XMOD1                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE ACCLBCOLSC                                                     
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
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKC                                                     
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF1D                                                       
         SPACE 1                                                                
         ORG   OSVALS                                                           
SCR99DA  DS    A                   SAVED DA OF 99 POSTING WHEN ACTREV           
SCR99CNT DS    A                   COUNT OF REVERSED ITEMS THIS SCREEN          
*                                                                               
SFLTOPS  DS    CL(LSFLTOPL)        SAVED FILTERING OPTIONS                      
         ORG   OSVALS+OSVALSL                                                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
         ORG   OVERWRK                                                          
         DS    0X                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'085ACCLB18   08/16/00'                                      
         END                                                                    
