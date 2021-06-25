*          DATA SET ACCLB18B   AT LEVEL 121 AS OF 12/22/99                      
*PHASE T62118B                                                                  
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
         MVC   BASBILL,CSBILNUM                                                 
         TM    BCINDS2,BCINTRS     WHEN FIRST FOR LIST                          
         BZ    *+10                SAVE DA OF SELECTED BILL                     
         MVC   SCR99DA,TLDA                                                     
         XC    SCR99CNT,SCR99CNT                                                
*                                                                               
         L     RE,AIO4             A(JOB RECORD)                                
         SR    R0,R0                                                            
         XC    RJOBCURR,RJOBCURR                                                
         LA    RE,ACTRFST-ACTRECD(RE)                                           
SCRF002  CLI   0(RE),0             TEST E-O-R                                   
         BE    SCRF010                                                          
         CLI   0(RE),AFCELQ        AFCCURR=CURR OF PENDING ACTIVITY             
         BE    SCRF004                                                          
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     SCRF002                                                          
SCRF004  CLC   AFCCURR-AFCELD(,RE),BCSPACES                                     
         BNH   SCRF010                                                          
         MVC   RJOBCURR,AFCCURR-AFCELD(RE)                                      
*                                                                               
SCRF010  L     RE,=A(ALOVAL)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,ALSVALS    SET OUTPUT BASE ADDRESS                      
         MVC   SFLTOPS,LSOPS       SAVE FILTERING OPTIONS FOR COMPARE           
         XC    LSOPS,LSOPS         CLEAR ALL OPTIONS                            
         XC    LSCLM,LSCLM         CLEAR DISPLAY COLUMNS                        
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
VALYES   BAS   RE,GETPTREV         GET PTAEL FOR REVERSAL                       
         USING PTAELD,R3                                                        
COP      USING PTAELD,R4                                                        
         GOTO1 CHGPRO,BOPARM,(R3),(R4),LSPRATAS                                 
*                                                                               
         CP    PP$AALLO,BCPZERO    TEST ANY PENDING ALLOCATION                  
         BE    VYES02                                                           
         TM    PG$STAT,PG$REVS     TEST PENDING ALLOC IS REVERSAL               
         BO    VYES02              YES - ADD THIS ALLOCATION TO IT              
         ZAP   BODUB1,BCPZERO      CLEAR NON-REVERSAL ALLOCATION                
         ZAP   BODUB2,BCPZERO                                                   
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO1),('PTASCASH',LSPRATA),   X        
               (1,BODUB1),(1,BODUB2),0                                          
         BNE   EXITN                                                            
         BAS   RE,GETPTREV         ALLTRN MAY HAVE CHANGED THE RECORD           
*                                                                               
VYES02   TM    PTASTAT1,PTASREVD   TEST ALREADY REVERSED                        
         BO    EXITN                                                            
         OI    PTASTAT1,PTASREVD   SET REVERSAL STATUS ON ORIG BILL             
         ZAP   BODUB1,COP.PTANET   SET NET AND COMMISSION                       
         ZAP   BODUB2,COP.PTARCOM                                               
         CLC   CSCPYCUR,CSBILCUR   TEST FOREIGN CURRENCY BILL                   
         BE    VYES03                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    VYES03                                                           
         ZAP   REVAGNET,BODUB1     AGENCY CURRENCY AMOUNT TO REVERSE            
         ZAP   REVAGCOM,BODUB1                                                  
         MP    REVAGNET,=P'-1'                                                  
         MP    REVAGCOM,=P'-1'                                                  
         ZAP   BODUB1,COP.PTANETF  SET FOREIGN NET AND COMMISSION               
         ZAP   BODUB2,COP.PTARFCOM                                              
VYES03   MP    BODUB1,=P'-1'                                                    
         MP    BODUB2,=P'-1'                                                    
*                                                                               
         LA    R0,PTASCASH+PTASREVS                                             
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO1),((R0),LSPRATA),         X        
               (0,BODUB1),(0,BODUB2),REVAGNET                                   
         BNE   EXITN                                                            
         BAS   RE,GETPTREV         ALLTRN MAY HAVE CHANGED THE RECORD           
         TM    PTASTAT2,PTASWRUP   TEST REVERSING A WRITE-UP                    
         BNO   VYES04                                                           
         L     RE,BOPARM           GET A(NEW PTAEL)                             
NEW      USING PTAELD,RE                                                        
         OI    NEW.PTASTAT2,PTASWRUP                                            
         ZAP   NEW.PTAWUAMT,PTAWUAMT                                            
         MP    NEW.PTAWUAMT,=P'-1'                                              
         SR    RF,RF                                                            
         ICM   RF,3,PTAWUHRS                                                    
         LCR   RF,RF                                                            
         STCM  RF,3,NEW.PTAWUHRS                                                
         XR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+18                FOREIGN CURRENCY NOT IN USE                  
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 APRORATA,BODMCB,AIO1,AGOPBLK,ACOM,(R0),LSPRATA,0                 
*                                                                               
VYES04   ICM   RF,15,SCR99CNT      INCREMENT SCREEN COUNT OF REVERSALS          
         LA    RF,1(RF)                                                         
         STCM  RF,15,SCR99CNT                                                   
         BAS   RE,GETPTREV         UPDATE PRO-RATA BLOCK                        
         GOTO1 CHGPRO,BOPARM,(R3),(R4),LSPRATA                                  
         B     EXITY                                                            
         DROP  R3,NEW,COP                                                       
         EJECT                                                                  
***********************************************************************         
* CLEAR PENDING ALLOCATION                                            *         
***********************************************************************         
         SPACE 1                                                                
VALCLR   BAS   RE,GETPTREV                                                      
         GOTO1 CHGPRO,BOPARM,(R3),(R4),LSPRATAS                                 
COP      USING PTAELD,R4                                                        
*                                                                               
         TM    TLXPEND,TLXPREV                                                  
         BNO   EXITN                                                            
         CP    PP$AALLO,BCPZERO                                                 
         BNE   VCLR02                                                           
         CP    PP$ACOMM,BCPZERO                                                 
         BNE   VCLR02                                                           
         MVC   FVMSGNO,=AL2(AE$NOALL)                                           
         BE    EXITN               NOTHING TO CLEAR                             
*                                                                               
VCLR02   ZAP   BODUB1,COP.PTANET   SET NET AND COMMISSION                       
         ZAP   BODUB2,COP.PTARCOM                                               
         CLC   CSCPYCUR,CSBILCUR   TEST FOREIGN CURRENCY BILL                   
         BE    VCLR03                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    VCLR03                                                           
         ZAP   REVAGNET,BODUB1     AGENCY CURRENCY AMOUNT TO CLEAR              
         ZAP   REVAGCOM,BODUB1                                                  
         ZAP   BODUB1,COP.PTANETF  SET FOREIGN NET AND COMMISSION               
         ZAP   BODUB2,COP.PTARFCOM                                              
VCLR03   GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO1),('PTASCASH',LSPRATA),   X        
               BODUB1,BODUB2,REVAGNET                                           
         BNE   EXITN                                                            
         L     R3,AIO1             REMOVE REVERSAL PENDING STATUS               
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         SR    R0,R0                                                            
VCLR04   IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         USING PTAELD,R3                                                        
         CLI   PTAEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PTAEL,PTAELQ                                                     
         BNE   VCLR04                                                           
         CLI   PTATYPE,PTATRAL                                                  
         TM    PTASTAT1,PTASPEND                                                
         BNO   VCLR04                                                           
         CP    PTANET,BCPZERO                                                   
         BNE   *+8                                                              
         NI    PTASTAT1,FF-PTASREVS                                             
*                                                                               
         BAS   RE,GETPTREV         REMOVE REVERSAL STAT FROM ORIGINAL           
         NI    PTASTAT1,FF-PTASREVD                                             
         DROP  R3                                                               
         ICM   RF,15,SCR99CNT                                                   
         BCTR  RF,0                                                             
         STCM  RF,15,SCR99CNT      REDUCE SCREEN COUNT OF REVERSALS             
         BAS   RE,GETPTREV         UPDATE PRO-RATA BLOCK                        
         GOTO1 CHGPRO,BOPARM,(R3),(R4),LSPRATA                                  
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  COP                                                              
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
DISCLM   L     R3,APTA                                                          
         USING PTAELD,R3                                                        
         L     R4,ACOPPTA                                                       
COP      USING PTAELD,R4                                                        
*                                                                               
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     DISFRST     00      FIRST TIME CALL                              
         B     DISNET      01      NET AVAILABLE                                
         B     EXIT        02      UNDEFINED                                    
         B     DISCMN      03      COMMISSION                                   
         B     DISHRS      04      HOURS                                        
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR DISPLAY LINE                                              *         
***********************************************************************         
         SPACE 1                                                                
DISFRST  DS    0H                                                               
*&&UK                                                                           
         TM    BCCPYST7,CPYSSCNV   TEST CONVERTED FOR 2ND CURRENCY              
         BZ    DFRST02                                                          
         CLC   CSBILCUR,BCCPYSEC   TEST BILLING IN 2ND CURRENCY                 
         BNE   DFRST02                                                          
         TM    LSINDS2,LSTOBACC    TEST RECORD IS CONVERTED                     
         BO    DFRST02             NO - CONVERT IT                              
         GOTO1 VTOBACCO,BOPARM,('TOBAACVS',BCWORK),AIO1,ACOM,0,0                
         OI    LSINDS2,LSTOBACC                                                 
*&&                                                                             
*                                                                               
DFRST02  BAS   RE,GETPTREV                                                      
         ST    R3,APTA                                                          
         ST    R4,ACOPPTA                                                       
         GOTO1 CHGPRO,BOPARM,PTAELD,COP.PTAELD,LSPRATA                          
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY NET (UNALLOCATED) AMOUNT                                    *         
***********************************************************************         
         SPACE 1                                                                
DISNET   ZAP   BODUB1,BCPZERO                                                   
         TM    PTASTAT1,PTASREVD   TEST ALREADY MARKED FOR REVERSAL             
         BO    DISAMNT             THEN SHOW ZERO AVAILABLE                     
         ZAP   BODUB1,COP.PTANET                                                
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    DISNET02                                                         
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    DISNET02                                                         
         ZAP   BODUB1,COP.PTANETF                                               
DISNET02 MP    BODUB1,=P'-1'                                                    
         B     DISAMNT                                                          
         SPACE 1                                                                
***********************************************************************         
* DISPLAY COMMISSION (UNALLOCATED) AMOUNT                             *         
***********************************************************************         
         SPACE 1                                                                
DISCMN   ZAP   BODUB1,BCPZERO                                                   
         TM    PTASTAT1,PTASREVD   TEST REVERSAL ALREADY ALLOCATED              
         BO    DISAMNT             SHOW ZERO COMMISSION AVAILABLE               
         ZAP   BODUB1,COP.PTARCOM                                               
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    DISCMN02                                                         
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    DISCMN02                                                         
         ZAP   BODUB1,COP.PTARFCOM                                              
DISCMN02 MP    BODUB1,=P'-1'                                                    
         B     DISAMNT                                                          
         SPACE 1                                                                
***********************************************************************         
* DISPLAY HOURS                                                       *         
***********************************************************************         
         SPACE 1                                                                
DISHRS   ZAP   BODUB2,BCPZERO                                                   
         TM    PTASTAT1,PTASREVD   TEST REVERSED                                
         BO    DISH200             DISPLAY ZERO HOURS AVAILABLE                 
         SR    RE,RE                                                            
         ICM   RE,3,COP.PTAHOURS   DISPLAY HOURS ON ORIGINAL BILL               
         CVD   RE,BODUB2                                                        
*                                                                               
DISH200  GOTO1 AEDTAMT,BOPARM,BODUB2,0                                          
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R3,COP                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY CURRENCY AMOUNT                                             *         
***********************************************************************         
         SPACE 1                                                                
DISAMNT  GOTO1 AEDTAMT,BOPARM,BODUB1,BODUB1                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* FIND MATCHING PTAEL                                                 *         
***********************************************************************         
         SPACE 1                                                                
GETPTREV ST    RE,OVSVRE                                                        
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST                                                       
         USING PTAELD,R3                                                        
         SR    R0,R0                                                            
         SR    R4,R4                                                            
         CLC   TRNKWORK,TRNORDER   ORDER - COPY PTAEL AND MASSAGE               
         BNE   GETP10                                                           
         GOTO1 AMCHORD,BOPARM,(C'I',(R3)),AIO2                                  
         L     R4,AIO2             FIND MATCHING COPIED/MASSAGED PTAEL          
COP      USING PTAELD,R4                                                        
GETP02   CLI   COP.PTAEL,0         TEST MASSAGED PTAEL NOT FOUND                
         BE    GETP10                                                           
         CLI   COP.PTAEL,PTAELQ                                                 
         BNE   GETP06                                                           
         CLI   COP.PTATYPE,PTATRAL                                              
         BNE   GETP06                                                           
         TM    COP.PTASTAT1,PTASPEND                                            
         BO    GETP06                                                           
         CLC   COP.PTARBLNO,CSBILNUM                                            
         BE    GETP10              R4=A(MATCHING COPIED/MASSAGED PTAEL)         
GETP06   IC    R0,COP.PTALN                                                     
         AR    R4,R0                                                            
         B     GETP02                                                           
*                                                                               
GETP10   IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         CLI   PTAEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PTAEL,PTAELQ                                                     
         BNE   GETP10                                                           
         CLI   PTATYPE,PTATRAL                                                  
         BNE   GETP10                                                           
         TM    PTASTAT1,PTASPEND                                                
         BO    GETP10                                                           
         CLC   PTARBLNO,CSBILNUM                                                
         BNE   GETP10                                                           
         LTR   R4,R4               ORDER - R4=A(COPIED/MASSAGED PTAEL)          
         BNZ   *+6                                                              
         LR    R4,R3               ELSE - R4=A(ORIGINAL PTAEL)                  
         L     RE,OVSVRE                                                        
         BR    RE                                                               
         DROP  R3,R2,COP                                                        
         EJECT                                                                  
***********************************************************************         
* FUDGE TO CHANGE PRORATA BLOCK TO CONTAIN BILL AMOUNTS               *         
*                                                                     *         
* NTRY: P1 = A(PTAELD)                                                *         
*       P2 = A(COPIED PTAELD)                                         *         
*       P3 = A(PRO-RATA BLOCK TO UPDATE)                              *         
* THIS IS SO THAT WORKCODE TOTALS / SCREEN TOTALS ARE CORRECT         *         
***********************************************************************         
         SPACE 1                                                                
CHGPRO   NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         USING PTAELD,R2                                                        
COP      USING PTAELD,R3                                                        
         USING PRORATAD,R4                                                      
*                                                                               
         ZAP   BILLNET,COP.PTANET  SET NET/COMMISSION BILL AMOUNTS              
         MP    BILLNET,=P'-1'                                                   
         ZAP   BILLCOM,COP.PTARCOM                                              
         MP    BILLCOM,=P'-1'                                                   
         XR    RE,RE                                                            
         ICM   RE,3,COP.PTAHOURS                                                
         CVD   RE,BILLHRS                                                       
         ZAP   BILLNETF,BCPZERO                                                 
         ZAP   BILLCOMF,BCPZERO                                                 
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    CPRO02                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    CPRO02                                                           
         ZAP   BILLNETF,COP.PTANETF                                             
         MP    BILLNETF,=P'-1'                                                  
         ZAP   BILLCOMF,COP.PTARFCOM                                            
         MP    BILLCOMF,=P'-1'                                                  
*                                                                               
CPRO02   TM    PTASTAT1,PTASREVD   TEST IS PENDING REVERSAL                     
         BO    CPRO04                                                           
         ZAP   PM$ANVBL,BILLNET    NO - SET AVAILABLE TO BILL AMOUNTS           
         ZAP   PM$FNVBL,BILLNETF                                                
         ZAP   PM$ACVBL,BILLCOM                                                 
         ZAP   PM$FCVBL,BILLCOMF                                                
         ZAP   PM$HRVBL,BILLHRS                                                 
         B     CHGPROX                                                          
*                                                                               
CPRO04   DS    0H                  YES - SET AVAILABLE TO ZERO                  
         ZAP   PM$ANVBL,BCPZERO                                                 
         ZAP   PM$FNVBL,BCPZERO                                                 
         ZAP   PM$ACVBL,BCPZERO                                                 
         ZAP   PM$FCVBL,BCPZERO                                                 
         ZAP   PM$HRVBL,BCPZERO                                                 
*                                                                               
CHGPROX  B     EXITY                                                            
         DROP  R2,COP,R4                                                        
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
*&&UK                                                                           
         TM    PTASTAT2,PTASOXFR                                                
         BO    GETN140                                                          
*&&                                                                             
         TM    PTASTAT1,PTASREVU   TEST REVERSAL OF BILL IS UPDATED             
         BO    GETNEXT                                                          
         OC    RJOBCURR,RJOBCURR   TEST ANY ACTIVITY PENDING ON JOB             
         BZ    *+14                NO                                           
         CLC   PTACUR,RJOBCURR     TEST MATCH WITH PENDING ACTIVITY             
         BNE   GETNEXT                                                          
         CLC   PTACUR,BCCPYSEC     TEST ALLOCATION IN SECOND CURRENCY           
         BNE   GETN150                                                          
         CLC   CSBILCUR,CSCPYCUR   TEST BILLING CURR AGENCY PRIMARY             
         BNE   GETN150                                                          
         MVC   CSBILCUR,PTACUR     THEN ASSUME THIS IS THE RIGHT ONE            
         B     GETN150                                                          
GETN140  IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     GETN120                                                          
         DROP  RE                                                               
*                                  GENERAL FILTERS                              
GETN150  GOTO1 ASETTRN,BOPARM,(C'M',TRNRECD)                                    
         BNE   GETNEXT                                                          
         BAS   RE,GETPTREV                                                      
         CLC   TRNORDER,TRNKWORK                                                
         BNE   GETNEXTX                                                         
         CLI   0(R4),0             TEST MASSAGED PTAEL FOUND                    
         BE    GETNEXT             NO - MUST BE FULLY MATCHED ORDER             
*                                                                               
GETNEXTX GOTO1 CHGPRO,BOPARM,(R3),(R4),LSPRATA                                  
         B     EXITY                                                            
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
         DC    AL1(0,0,0,0,0,1,LSCLMMAX,LSCLMMAX)                               
         DC    AL1((*-ALOTAB)/OPTTABL)                                          
         DC    AL2(OPTDISQ,0)                                                   
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
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBE2D                                                       
         SPACE 1                                                                
         ORG   OSVALS                                                           
SCR99DA  DS    A                   SAVED DA OF 99 POSTING WHEN ACTREV           
SCR99CNT DS    A                   COUNT OF REVERSED ITEMS THIS SCREEN          
OVSVRE   DS    A                   SAVED RE                                     
*                                                                               
SFLTOPS  DS    CL(LSFLTOPL)        SAVED FILTERING OPTIONS                      
         ORG   OSVALS+OSVALSL                                                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
         ORG   OVERWRK                                                          
APTA     DS    A                                                                
ACOPPTA  DS    A                                                                
*                                                                               
         DS    0D                                                               
BILLNET  DS    PL8                 ITEM NET AMOUNT (USED BY CHGPRO)             
BILLNETF DS    PL8                 ITEM FOREIGN NET AMOUNT                      
BILLCOM  DS    PL8                 ITEM COMMISSION AMOUNT                       
BILLCOMF DS    PL8                 ITEM FOREIGN COMMISSION AMOUNT               
BILLHRS  DS    PL8                 ITEM HOURS                                   
*                                                                               
REVAGNET DS    PL8                 AGENCY NET AMOUNT (FOREIGN BILL)             
REVAGCOM DS    PL8                 AGENCY COMM AMOUNT (FOREIGN)                 
*                                                                               
RJOBCURR DS    CL3                                                              
         DS    0X                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'121ACCLB18B  12/22/99'                                      
         END                                                                    
