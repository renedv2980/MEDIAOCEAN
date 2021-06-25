*          DATA SET ACCLB02    AT LEVEL 053 AS OF 08/16/00                      
*PHASE T62102A                                                                  
CLB02    TITLE '- TRANSACTION LIST ROUTINES - NEW VERSION'                      
CLB02    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLB2**,R8,R7,R6,CLEAR=YES,RR=RE                              
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
SCRFRST  MVI   SCRINDS,0                                                        
         OC    LSTALNET,LSTALNET                                                
         BNZ   SCRF002                                                          
         ZAP   LSTALNET,BCPZERO                                                 
         ZAP   LSTNVBL,BCPZERO                                                  
SCRF002  AP    LSTNVBL,LSTALNET                                                 
*                                                                               
         ZAP   LSTALNET,BCPZERO    MODIFY LIST NET AVAILABLE BY                 
         L     R1,AIO4             ACTIVITY SINCE LAST BUILT                    
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING SCIELD,R1                                                        
         XR    RF,RF                                                            
SCRF004  CLI   SCIEL,0                                                          
         BE    SCRF010                                                          
         CLI   SCIEL,SCIELQ                                                     
         BNE   SCRF008                                                          
         CLI   SCITYPE,SCITCBAP                                                 
         BNE   *+14                                                             
         AP    LSTALNET,SCIAMNT                                                 
         B     SCRF008                                                          
         CLI   SCITYPE,SCITCBWP                                                 
         BNE   *+20                                                             
         AP    LSTALNET,SCIAMNT                                                 
         AP    LSTALNET,SCIADMN                                                 
         B     SCRF008                                                          
         CLI   SCITYPE,SCITCBTP                                                 
         BNE   *+10                                                             
         AP    LSTALNET,SCIAMNT                                                 
SCRF008  IC    RF,SCILN                                                         
         BXH   R1,RF,SCRF004                                                    
         DROP  R1                                                               
SCRF010  SP    LSTNVBL,LSTALNET                                                 
*                                                                               
         MVC   BASJOBC,BCJOBCOD                                                 
         MVC   BASJOBN,BCJOBNAM                                                 
         L     RE,=A(ALOVAL)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,ALSVALS    SET OUTPUT BASE ADDRESS                      
         MVC   SFLTOPS,LSOPS       SAVE FILTERING OPTIONS FOR COMPARE           
*     ?? CLI   LSPASS,C'Y'         TEST PASSING OPTIONS FOR NEW SESSION         
*     ?? BE    *+10                                                             
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
         CLI   CSACT,ACTALC                                                     
         BNE   SCRF016                                                          
         LA    R0,DUPCLM                                                        
         OC    0(L'GOCBDALL,RF),GOCBDALL-GOBBLOCK(RE)                           
         BNZ   *+8                                                              
         LA    RF,DEFCLMA                                                       
         B     SCRF020                                                          
SCRF016  CLI   CSACT,ACTWOF                                                     
         BNE   SCRF018                                                          
         OC    0(L'GOCBDWOF,RF),GOCBDWOF-GOBBLOCK(RE)                           
         BNZ   *+8                                                              
         LA    RF,DEFCLMW                                                       
         B     SCRF020                                                          
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
SCRLAST  LR    R2,R1               DISPLAY UPDATED JOB INFO                     
         LA    RF,=AL1(SCITCBAP,SCITCBWP,SCITCBTP,EOT)                          
         CLI   CSACT,ACTWOF                                                     
         BNE   *+8                                                              
         LA    RF,=AL1(SCITCBWP,SCITCBAP,SCITCBTP,EOT)                          
         GOTO1 AUPDJOB,BOPARM,(C'D',(RF)),AIO4,FOOTLINL(R2)                     
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT TABLE                                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   MVI   LINEINDS,LINENOWO   PRE-SET NO WRITE-OFF                         
         SLL   R1,2                                                             
         B     *(R1)                                                            
         B     VALYES                                                           
         B     VALCLR                                                           
         B     VALWOFF                                                          
         B     VALDEL                                                           
         SPACE 1                                                                
***********************************************************************         
* SUB-ACTION 'YES'                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALYES   TM    TLXPEND,TLXPREV                                                  
         BO    EXITN                                                            
         CLI   CSACT,ACTWOF                                                     
         BE    VWOFF02                                                          
         CP    PM$ANVBL,BCPZERO    TEST AMOUNT AVAILABLE                        
         BE    EXITN                                                            
         BAS   RE,INPAMT           TEST AMOUNT FIELD HAS INPUT                  
         BE    EXITY                                                            
         TM    SCRINDS,SCRIMNET    TEST MULTI-INPUT TO NET FIELD                
         BO    EXITY                                                            
         ZAP   BODUB1,PM$ANVBL     NO - ALLOCATE FULL AMOUNT                    
         ZAP   BODUB2,PM$ACVBL                                                  
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+16                                                             
         ZAP   BODUB1,PM$FNVBL                                                  
         ZAP   BODUB2,PM$FCVBL                                                  
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO1),('PTASCASH',LSPRATA),   *        
               BODUB1,BODUB2                                                    
         BNE   EXITN                                                            
         OI    LSINDS1,LSIUPREC                                                 
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* SUB-ACTION 'WRITE-OFF'                                              *         
***********************************************************************         
         SPACE 1                                                                
VALWOFF  CLI   CSACT,ACTWOF        NOT ON ACTION WRITE-OFF                      
         BE    EXITN                                                            
VWOFF02  TM    TLXSTAT,TLXSORD     CAN'T WRITE-OFF ORDERS                       
         BO    EXITN                                                            
         CP    PM$ANVBL,BCPZERO    TEST AMOUNT AVAILABLE                        
         BE    EXITN                                                            
         OI    LINEINDS,LINEWOFF                                                
         NI    LINEINDS,FF-LINENOWO                                             
         BAS   RE,INPAMT           TEST AMOUNT FIELD HAS INPUT                  
         BE    EXITY                                                            
         ZAP   BODUB1,PM$ANVBL                                                  
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+10                                                             
         ZAP   BODUB1,PM$FNVBL                                                  
         GOTO1 WOFTRN,BOPARM,('PTATWOF',AIO1),LSPRATA,BODUB1,0                  
         BNE   EXITN                                                            
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* SUB-ACTION 'CLEAR'                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALCLR   CLI   CSACT,ACTWOF        TEST IF WRITE-OFF PENDING                    
         BNE   VALC10                                                           
         TM    TLXPEND,TLXPWOF                                                  
         BNO   EXITN                                                            
         ZAP   BODUB1,BCPZERO                                                   
         GOTO1 WOFTRN,BOPARM,('PTATWOF',AIO1),LSPRATA,(1,BODUB1),0              
         BNE   EXITN                                                            
         B     EXITY                                                            
*                                                                               
VALC10   TM    TLXPEND,TLXPALL     TEST FOR ALLOCATION PENDING                  
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NOALL)                                           
         B     EXITN                                                            
         ZAP   BODUB1,BCPZERO                                                   
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO1),('PTASCASH',LSPRATA),   *        
               (1,BODUB1),(1,BODUB1)                                            
         BNE   EXITN                                                            
         OI    LSINDS1,LSIUPREC                                                 
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* SUB-ACTION 'DELETE'                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
VALDEL   TM    TLXVAL,TLXVDEL                                                   
         BZ    EXITN               CANNOT DELETE RECORD                         
         ZAP   BODUB1,BCPZERO                                                   
         ZAP   BODUB2,BCPZERO                                                   
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO1),('PTASCASH',LSPRATA),   X        
               (1,BODUB1),(1,BODUB2)                                            
         BNE   EXITN                                                            
*                                                                               
         L     R3,AIO4                                                          
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         USING JCBELD,R3           REDUCE JOB ADVANCES COUNT                    
         XR    RF,RF                                                            
VDEL02   CLI   JCBEL,JCBELQ                                                     
         BE    VDEL04                                                           
         CLI   JCBEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    RF,JCBLN                                                         
         BXH   R3,RF,VDEL02                                                     
VDEL04   ICM   RE,3,JCBADV         REDUCE JOB ADVANCES COUNT                    
         BCTR  RE,0                                                             
         STCM  RE,3,JCBADV                                                      
         DROP  R3                                                               
*                                                                               
         L     R2,AIO1                                                          
         OI    TRNRSTAT,TRNSDELT                                                
         OI    LSINDS1,LSIUPREC    SET TO UPDATE RECORD                         
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST WHETHER AMOUNT COLUMN HAS BEEN INPUT TO             *         
***********************************************************************         
         SPACE 1                                                                
INPAMT   NTR1  ,                                                                
         LA    R4,=AL1(PRO#NET,PRO#ALC,PRO#WTF,PRO#HRS,EOT)                     
         L     RF,AFNDCLM                                                       
         LA    R1,BOPARM                                                        
IAMT02   GOTO1 (RF),(R1),(R4)                                                   
         BNE   IAMT08                                                           
         L     R3,0(R1)                                                         
         TM    FHIID(R3),FHIIVA                                                 
         BZ    EXITY                                                            
IAMT08   LA    R4,1(R4)                                                         
         CLI   0(R4),EOT                                                        
         BNE   IAMT02                                                           
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR VALIDATE LINE - R1=A(LINE)                                *         
***********************************************************************         
         SPACE 1                                                                
VALFRST  OC    CSSELCUR,CSSELCUR                                                
         BNZ   VFRST02                                                          
         MVI   LINEINDS,LINENOWO   PRE-SET NO WRITE-OFF                         
         CLI   CSACT,ACTWOF                                                     
         BNE   VFRST02                                                          
         OI    LINEINDS,LINEWOFF                                                
*                                                                               
VFRST02  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE COLUMN - R1 CONTAINS COLUMN ROUTINE NUMBER                 *         
***********************************************************************         
         SPACE 1                                                                
VALCLM   SLL   R1,2                                                             
         SR    R4,R4                                                            
         ICM   R4,3,CSSELACT                                                    
         LA    R4,TWAD(R4)                                                      
*                                                                               
VALCLMA  B     VALCLMA+4(R1)                                                    
         DC    XL4'00'  00                                                      
         B     VALNET   01         NET AVAILABLE                                
         B     VALWOF   02         WRITE-OFF AMOUNT                             
         B     VALALL   03         AMOUNT ALLOCATED                             
         B     VALCMN   04         COMMISSION AMOUNT                            
         B     VALHRS   05         HOURS                                        
         B     VALEXP   06         EXPENSE ACCOUNT                              
         B     VALOFF   07         OFFICE CODE                                  
         B     VALPAR   08         PARAGRAPH CODE                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE WRITE-OFF AMOUNT                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
VALWOF   TM    LINEINDS,LINEHRSV                                                
         BO    EXITY               HOURS ALREADY VALIDATED                      
         NI    LINEINDS,FF-LINENOWO                                             
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         XR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   0(R1),FF                                                         
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN               INPUT MUST BE STRAIGHT CASH                  
         OI    LINEINDS,LINENETV   SET NET VALIDATED                            
         ZAP   BODUB1,BODMCB+4(8)                                               
         GOTO1 WOFTRN,BOPARM,('PTATWOF',AIO1),('PTASCASH',LSPRATA),    *        
               (1,BODUB1),0                                                     
         BNE   EXITN                                                            
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT TO NET (AVAILABLE) COLUMN - INPUT IS INCREMENTAL     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
         USING SUBINPH,R4                                                       
VALNET   ICM   RF,15,LSAMIFEL      TEST MULTI ACTION                            
         BZ    VALN25                                                           
         USING MIFELD,RF                                                        
         CLC   MIF#LO,TLNUM                                                     
         BNE   VALN15                                                           
*                                  VALIDATE INPUT AMOUNT                        
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   0(R1),FF                                                         
         BE    EXITN               INPUT MUST BE STRAIGHT CASH                  
         ZAP   LSTREMR,BCPZERO     CLEAR SAVED REMAINDER                        
         ZAP   LSTALLC,BODMCB+4(8) SET ALLOCATION AMOUNT                        
         CP    LSTNVBL,BCPZERO     TEST NET AVAILABLE IS ZERO                   
         BE    EXITN               YES - CANNOT SPREAD ALLOCATION               
*                                                                               
VALN15   OI    SCRINDS,SCRIMNET    SET MULTI INPUT TO NET FIELD                 
*                                                                               
         LA    RF,PM$ANVBL         FIND NET AMOUNT AVAILABLE                    
         CLC   CSCPYCUR,CSBILCUR   SAVE REMAINDER EACH TIME FOR NEXT            
         BE    *+8                                                              
         LA    RF,PM$FNVBL                                                      
         ZAP   BOPL81(16),0(L'PM$ANVBL,RF)                                      
         MP    BOPL81(2*L'BOPL81),LSTALLC                                       
         SRP   BOPL81(2*L'BOPL81),2,0                                           
         DP    BOPL81(2*L'BOPL81),LSTNVBL                                       
         AP    BOPL81,LSTREMR                                                   
         MVC   BOWORK1(L'BOPL81),BOPL81                                         
         SRP   BOPL81,64-2,5                                                    
         ZAP   BODUB1,BOPL81       ALLOCATED AMOUNT                             
         SRP   BOPL81,2,0                                                       
         SP    BOWORK1(L'BOPL81),BOPL81                                         
         MVC   LSTREMR,BOWORK1     SAVE THIS REMAINDER FOR NEXT                 
         BAS   RE,SETCOM           CALCULATE COMMISSION                         
         B     VALN70                                                           
*                                                                               
VALN25   TM    LINEINDS,LINEWOFF   TEST WRITING OFF                             
         BO    VALN80                                                           
*                                                                               
VALN60   TM    LINEINDS,LINEHRSV                                                
         BO    EXITY               HOURS ALREADY VALIDATED                      
         TM    TLXPEND,TLXPREV                                                  
         BO    EXITN               REVERSAL PENDING - INVALID                   
         L     R2,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   0(R1),FF                                                         
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN               INPUT MUST BE STRAIGHT CASH                  
         ZAP   BODUB1,BODMCB+4(8)  SET CASH AMOUNT ALLOCATED                    
         BAS   RE,SETCOM           CALCULATE COMM IF NOT OVERRIDDEN             
*                                                                               
VALN70   LA    R0,PTASCASH                                                      
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO1),((R0),LSPRATA),         X        
               (0,BODUB1),(0,BODUB2)                                            
         BNE   EXITN                                                            
         OI    LINEINDS,LINENETV   SET NET VALIDATED                            
         B     EXITY                                                            
*                                  NET COLUMN FOR WRITE-OFF LIST                
VALN80   SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         TM    LINEINDS,LINEHRSV                                                
         BO    EXITY               HOURS ALREADY VALIDATED                      
         NI    LINEINDS,FF-LINENOWO                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   0(R1),FF                                                         
         BNE   *+14                INPUT MUST BE STRAIGHT CASH                  
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN               INPUT MUST BE STRAIGHT CASH                  
         OI    LINEINDS,LINENETV   SET NET VALIDATED                            
         ZAP   BODUB1,BODMCB+4(8)                                               
         GOTO1 WOFTRN,BOPARM,('PTATWOF',AIO1),('PTASCASH',LSPRATA),    *        
               (0,BODUB1),0                                                     
         BNE   EXITN                                                            
         B     EXITY                                                            
*                                                                               
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT TO ALLOCATED COLUMN - INPUT RESETS ANY CURRENT VALUE *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
         USING SUBINPH,R4                                                       
VALALL   L     R2,AIO1             R2=A(TRANSACTION)                            
         SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   0(R1),FF                                                         
         BNE   *+14                INPUT MUST BE STRAIGHT CASH                  
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN               INPUT MUST BE STRAIGHT CASH                  
         ZAP   BODUB1,BODMCB+4(8)  SET CASH AMOUNT ALLOCATED                    
VALA04   BAS   RE,SETCOM           CALCULATE COMM IF NOT OVERRIDDEN             
         LA    R0,1                SET TO RESET INCOME AMOUNT                   
         CLI   BOBYTE1,0                                                        
         BE    *+6                                                              
         SR    R0,R0               SET TO ADD INCOME AMOUNT                     
*                                                                               
VALA30   LA    R3,PP$AALLO         TAKE OUT PREVIOUS ALLOCATION                 
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R3,PP$FALLO                                                      
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO1),('PTASCASH',LSPRATA),   X        
               (1,BODUB1),((R0),BODUB2)                                         
         BE    VALA40                                                           
         B     EXITN                                                            
*                                                                               
VALA40   B     EXITY                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE COMMISSION - INPUT IS INCREMENTAL                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
VALCMN   L     R2,AIO1                                                          
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   0(R1),FF                                                         
         BNE   *+14                INPUT MUST BE STRAIGHT CASH                  
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN                                                            
         ZAP   BODUB2,BODMCB+4(8)  SET COMMISSION AMOUNT                        
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO1),('PTASCOVR',LSPRATA),   X        
               0,(0,BODUB2)                                                     
         BNE   EXITN                                                            
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE HOURS                                                      *         
***********************************************************************         
         SPACE 1                                                                
VALHRS   TM    LINEINDS,LINENETV                                                
         BO    EXITY               NET ALREADY VALIDATED                        
         GOTO1 AVALHRS,BODMCB,0,LSPRATA,0                                       
         BNE   EXITN                                                            
         TM    LINEINDS,LINEWOFF                                                
         BZ    VALH20                                                           
         GOTO1 WOFTRN,BOPARM,('PTATWOF',AIO1),('PTASHOUR',LSPRATA),    *        
               (0,BODUB1),0                                                     
         BNE   EXITN                                                            
         NI    LINEINDS,FF-LINENOWO                                             
         OI    LINEINDS,LINEHRSV                                                
         B     EXITY                                                            
*                                                                               
         USING TRNRECD,R2                                                       
VALH20   L     R2,AIO1                                                          
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO1),('PTASHOUR',LSPRATA),   X        
               (0,BODUB1),0                                                     
         BNE   EXITN                                                            
         LA    RF,PP$AALLO                                                      
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    RF,PP$FALLO                                                      
         ZAP   BODUB1,0(L'PP$AALLO,RF)   SET CURENT NET ALLOCATION              
*        AH    RF,=Y(LSPRATAS-LSPRATA)                                          
*        SP    BODUB1,0(L'PP$AALLO,RF)   REMOVE PREVIOUS AMOUNT                 
         BAS   RE,SETCOM           CALCULATE COMM IF NOT OVERRIDDEN             
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO1),(0,LSPRATA),            X        
               0,(1,BODUB2)                                                     
*                                                                               
         OI    LINEINDS,LINEHRSV   SET HRS VALIDATED                            
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE COMMISSION RATE                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
VALCOMR  L     R2,AIO1                                                          
         CLI   FVILEN,0                                                         
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXPENSE ACCOUNT                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
VALEXP   L     R2,AIO1                                                          
         BAS   RE,GETWPTA                                                       
         BNE   EXITN                                                            
         LR    R3,RF                                                            
         USING PTAELD,R3                                                        
         LA    RF,X'80'                                                         
         TM    TLXSTAT,TLXSULSI                                                 
         BZ    *+8                                                              
         LA    RF,X'40'(RF)                                                     
         GOTO1 AVALWEX,BOPARM,((RF),0)                                          
         BNE   EXITN                                                            
         MVC   PTAWEXPA,ACCODE+(ACTKUNT-ACTKCPY)                                
         DROP  R3                                                               
*                                                                               
         CLI   CUCTRY,CTRYHOL      TEST HOLLAND                                 
         BNE   VALEXP06                                                         
         CLI   BCBYTE1,0           TEST HAVE TAX TYPE                           
         BNE   VALEXP02                                                         
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('FFTELQ',AIO5),            *        
               (1,=AL1(FFTTTAXI))                                               
         CLI   12(R1),0                                                         
         BE    VALEXP06                                                         
         DC    H'0'                                                             
*                                                                               
VALEXP02 GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('FFTELQ',AIO5),            *        
               (1,=AL1(FFTTTAXI))                                               
         CLI   12(R1),0            TEST FFTELD ON PTA RECORD                    
         BNE   VALEXP04                                                         
         L     R3,12(R1)                                                        
         USING FFTELD,R3                                                        
         CLC   BCBYTE1,FFTDATA     TEST CHANGE OF CODE                          
         BE    VALEXP06                                                         
         MVC   FFTDATA(1),BCBYTE1                                               
         B     VALEXP06                                                         
*                                                                               
VALEXP04 LA    R3,BOELEM           ADD FFT ELEMENT                              
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTDATA+1-FFTELD                                           
         MVI   FFTTYPE,FFTTTAXI                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,1                                                        
         MVC   FFTDATA(1),BCBYTE1                                               
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO5,FFTELD                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
VALEXP06 OI    LSINDS1,LSIUPREC+LSIUPPTA                                        
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE OFFICE CODE                                                *         
***********************************************************************         
         SPACE 1                                                                
VALOFF   CLI   CSACT,ACTWOF        TEST MAIN ACTION IS WRITE-OFF                
         BE    VALO02                                                           
         SR    RE,RE               OR SUB-ACT IS WRITE-OFF                      
         ICM   RE,3,CSSELCUR                                                    
         A     RE,AOVERSEL                                                      
         CLC   =Y(UC@WRTF-TWAD),SELTDSPN-SELTABD(RE)                            
         BNE   EXITN               IF NOT THEN OFFICE NOT ALLOWED               
VALO02   SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
*        EX    RF,*+4                                                           
*        MVC   WOFOFFC,FVIFLD                                                   
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAGRAPH CODE INPUT                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
VALPAR   CP    PP$AALLO,BCPZERO    TEST ANYTHING PENDING                        
         BNE   VALP04                                                           
         CP    PP$ACOMM,BCPZERO                                                 
         BNE   VALP04                                                           
         CLI   FVILEN,0            NO CODE ALLOWED IF NO PENDING                
         BNE   EXITN                                                            
         B     EXITY                                                            
VALP04   L     R2,AIO1                                                          
         SR    R0,R0                                                            
         LA    R3,TRNRFST                                                       
         USING PTAELD,R3                                                        
VALP06   CLI   PTAEL,0             FIND PTAEL FOR ALLOCATION PENDING            
         BE    EXIT                                                             
         CLI   PTAEL,PTAELQ                                                     
         BNE   VALP08                                                           
         CLI   PTATYPE,PTATRAL                                                  
         BNE   VALP08                                                           
         TM    PTASTAT1,PTASPEND                                                
         BO    VALP10                                                           
VALP08   IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         B     VALP06                                                           
VALP10   MVI   PTARCODE,0                                                       
         OI    LSINDS1,LSIUPREC                                                 
         CLI   FVILEN,0                                                         
         BE    EXITY                                                            
         TM    FVIIND,FVINUM       TEST NUMERIC INPUT                           
         BNO   EXITN                                                            
         CLC   BCFULL,=A(200)                                                   
         BH    EXITN                                                            
         MVC   PTARCODE,BCFULL+3                                                
         B     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* LAST FOR VALIDATE LINE                                              *         
***********************************************************************         
         SPACE 1                                                                
VALLAST  TM    LSINDS1,LSIUPPTA    TEST UPDATING PTA RECORD                     
         BZ    EXITY                                                            
         BAS   RE,GETWPTA                                                       
         BNE   EXITY                                                            
         LR    R3,RF               R3=A(PTAEL)                                  
         USING PTAELD,R3                                                        
         CLI   PTAWEXPA,C' '                                                    
         BH    EXITY                                                            
         MVC   FVMSGNO,=AL2(AE$CUFWO)                                           
         B     EXITN               NO EXPENSE ACCOUNT                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL ALLTRN FOR WRITE-OFF AMOUNT                         *         
*                                                                     *         
* NTRY: R1 = A(ALLTRN PARAMETER LIST)                                 *         
***********************************************************************         
         SPACE 1                                                                
WOFTRN   NTR1  ,                                                                
         TM    LSINDS1,LSIWOPTA    TEST HAVE PTA RECORD                         
         BO    WOFTRN02                                                         
         LR    R0,R1               NO - GET IT                                  
         GOTO1 AGETPTA,BODMCB,AIO1,AIO5,LSPRATA,0                               
         LR    R1,R0                                                            
*                                                                               
WOFTRN02 GOTO1 AALLTRN                                                          
         BNE   EXITN                                                            
         OI    LSINDS1,LSIUPPTA                                                 
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMN - R1 CONTAINS COLUMN ROUTINE NUMBER                  *         
***********************************************************************         
         SPACE 1                                                                
DISCLM   SLL   R1,2                ALL DONE BY GENERAL ROUTINES                 
         B     *+4(R1)                                                          
         B     EXITN       00      FIRST TIME CALL                              
         B     EXITN       01      NET AVAILABLE                                
         B     EXITN       02      WRITE-OFF AMOUNT                             
         B     EXITN       03      AMOUNT ALLOCATED                             
         B     EXITN       04      COMMISSION AMOUNT                            
         B     EXITN       05      HOURS                                        
         B     EXITN       06      EXPENSE ACCOUNT                              
         B     EXITN       07      OFFICE CODE                                  
         B     EXITN       08      PARAGRAPH CODE                               
         EJECT                                                                  
***********************************************************************         
* FIND PENDING WRITE-OFF PTAEL - RETURN A(PTAEL) IN RF                *         
***********************************************************************         
         SPACE 1                                                                
GETWPTA  L     RF,AIO1             FIND WRITE-OFF ACTIVITY ELEMENT              
         LA    RF,TRNRFST-TRNRECD(RF)                                           
         SR    R0,R0                                                            
         USING PTAELD,RF                                                        
GWPTA10  CLI   PTAEL,0                                                          
         BE    GWPTAN                                                           
         CLI   PTAEL,PTAELQ                                                     
         BE    GWPTA30                                                          
GWPTA20  IC    R0,PTALN                                                         
         AR    RF,R0                                                            
         B     GWPTA10                                                          
GWPTA30  CLI   PTATYPE,PTATWOF     TEST WRITE-OFF                               
         BNE   GWPTA20                                                          
         TM    PTASTAT1,PTASPEND   TEST PENDING                                 
         BNO   GWPTA20                                                          
*                                                                               
GWPTAY   CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
GWPTAN   LTR   RB,RB                                                            
         BR    RE                                                               
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY CURRENCY AMOUNT                                             *         
***********************************************************************         
         SPACE 1                                                                
DISAMNT  GOTO1 AEDTAMT,BOPARM,BODUB1,BODUB1                                     
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
LSTFRST  ZAP   LSTNVBL,BCPZERO     INIT LIST NET AVAILABLE TOTAL                
*                                                                               
         LA    R2,IOKEY            INITIALISE KEY                               
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
         GOTO1 ASETTRN,BOPARM,(C'D',TRNRECD)                                    
         BNE   GETNEXT             FILTER DIRECTORY RECORD                      
         LA    RE,TLXVALL          TEST VALID TO ALLOCATE/WRITE-OFF             
         CLI   CSACT,ACTWOF                                                     
         BNE   *+8                                                              
         LA    RE,TLXVWOF                                                       
         EX    RE,*+8                                                           
         BZ    GETNEXT                                                          
         TM    TLXVAL,0                                                         
*                                                                               
         GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         USING TRNELD,TRNRFST                                                   
*                                  GENERAL FILTERS                              
GETN150  GOTO1 ASETTRN,BOPARM,(C'M',TRNRECD)                                    
         BNE   GETNEXT                                                          
*                                                                               
GETNEXTX LA    RF,PM$ANVBL         ROLL UP NET AVAILABLE AMOUNTS                
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    RF,PM$FNVBL                                                      
         AP    LSTNVBL,0(L'PM$ANVBL,RF)                                         
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CALCULATE COMMISSION AMOUNT - BODUB1 CONTAINS NET AMOUNT            *         
***********************************************************************         
         SPACE 1                                                                
SETCOM   NTR1                                                                   
         MVI   BOBYTE1,0           0 MEANS INCOME CALCULATED                    
         ZAP   BODUB2,BCPZERO      SET ZERO COMMISSION                          
         TM    TLXSTAT,TLXSNOCM                                                 
         BO    SETCX               NON-COMMISSIONABLE ITEM                      
         BAS   RE,GETCOM           CALCULATE COMMISSION AMOUNT                  
*                                                                               
         GOTO1 AFNDCLM,BOPARM,=AL1(PRO#COM)                                     
         BNE   SETCX               TEST COMMISSION COLUM INPUT TO               
         L     RF,0(R1)                                                         
         TM    FHIID(RF),FHIIVA                                                 
         BO    SETCX                                                            
         ZAP   BODUB2,BCPZERO      DO NOT ALLOCATE CALCULATED COMM              
         MVI   BOBYTE1,1           1 MEANS INCOME INPUT                         
SETCX    B     EXIT                                                             
*                                                                               
GETCOM   L     RF,AGOPBLK          CALC COMM ON ALLOCATED NET AMT.              
         ZAP   BODUB2,BCPZERO      SET ZERO COMMISSION                          
         TM    TLXSTAT,TLXSNOCM                                                 
         BOR   RE                  NON-COMMISSIONABLE ITEM                      
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
ACCMST   DC    C'ACCMST  '                                                      
TRNBILL  DC    C'99'                                                            
TRNORDER DC    C'**'                                                            
*                                                                               
DEFCLMA  EQU   *                                                                
         DC    AL1(PRO#NETO)                                                    
         DC    AL1(PRO#HRSO)                                                    
         DC    AL1(PRO#ALC)                                                     
         DC    AL1(EOT)                                                         
DEFCLMW  EQU   *                                                                
*&&UK                                                                           
         DC    AL1(PRO#NETO)                                                    
         DC    AL1(PRO#WTF)                                                     
         DC    AL1(PRO#ALC)                                                     
         DC    AL1(EOT)                                                         
*&&                                                                             
*&&US                                                                           
         DC    AL1(PRO#NET)                                                     
         DC    AL1(PRO#HRSO)                                                    
         DC    AL1(PRO#WTFO)                                                    
         DC    AL1(EOT)                                                         
*&&                                                                             
         DC    X'00'                                                            
DEFCLMR  EQU   *                                                                
         DC    AL1(PRO#NETO)                                                    
         DC    AL1(PRO#COM)                                                     
         DC    AL1(PRO#ALC)                                                     
         DC    AL1(EOT)                                                         
DUPCLM   EQU   *                                                                
         DC    AL1(PRO#NET)                                                     
         DC    AL1(PRO#ALC)                                                     
         DC    AL2(AE$NBOTH)                                                    
         DC    AL1(EOT)                                                         
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
*                                  SWITCH=Y/N (ALLOCATE)                        
         DC    AL2(UC8SWCH-TWAD,UC3SWCH-TWAD)                                   
         DC    AL1(OPTNRTN+OPTDFLTO,0)                                          
         DC    AL1(0,0,0,ACTALC,0,1,4,L'LSSWITCH)                               
         DC    AL1((*-ALOTAB)/OPTTABL)                                          
         DC    AL2(OPTNYQ,LSSWITCH-LSVALSD)                                     
         DC    CL4'Y'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  SWITCH=Y/N (WRITE-OFF)                       
         DC    AL2(UC8SWCH-TWAD,UC3SWCH-TWAD)                                   
         DC    AL1(OPTNRTN+OPTDFLTO,0)                                          
         DC    AL1(0,0,0,ACTWOF,0,1,4,L'LSSWITCH)                               
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
       ++INCLUDE ACCLBWORKC                                                     
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF1D                                                       
         SPACE 1                                                                
         ORG   OSVALS                                                           
LSTNVBL  DS    PL8                 LIST NET AVAILABLE TOTAL                     
LSTALLC  DS    PL8                 ALLOCATION AMOUNT FOR SPREADING              
LSTREMR  DS    PL8                 SAVED REMAINDER FOR PRECISION                
LSTALNET DS    PL8                 JOB NET ALLOCATION AT LSTFRST                
LINEINDS DS    XL1                 LINE INDICATORS                              
LINENETV EQU   X'80'               NET COLUMN INPUT (IGNORE HOURS)              
LINEHRSV EQU   X'40'               HOURS COLUMN INPUT (IGNORE NET)              
LINENOWO EQU   X'20'               NO WRITE-OFF TO UPDATE                       
LINEWOFF EQU   X'10'               WRITING OFF                                  
SCRINDS  DS    XL1                 SCREEN INDICATORS                            
SCRIMNET EQU   X'80'               MULTI INPUT TO NET COLUMN                    
*                                                                               
SFLTOPS  DS    CL(LSFLTOPL)        SAVED FILTERING OPTIONS                      
         ORG   OSVALS+OSVALSL                                                   
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053ACCLB02   08/16/00'                                      
         END                                                                    
