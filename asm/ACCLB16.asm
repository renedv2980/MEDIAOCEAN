*          DATA SET ACCLB16    AT LEVEL 242 AS OF 08/16/00                      
*PHASE T62116A                                                                  
CLB16    TITLE '- WRITE OFF RECOVERY LIST - NEW VERSION'                        
CLB16    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLB16*,R8,R7,R6,CLEAR=YES,RR=RE                              
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
         B     EXITY               FIRST FOR VALIDATE THIS LINE                 
         B     EXITY               VALIDATE COLUMN                              
         B     EXITY               LAST FOR VALIDATE THIS LINE                  
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
*                                                                               
         XC    BOWORK2,BOWORK2                                                  
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         OC    BOWORK2(L'GOCBDRCV),GOCBDRCV-GOBBLOCK(RF)                        
         BNZ   *+10                                                             
         MVC   BOWORK2(DEFCLML),DEFCLM                                          
         SR    R0,R0                                                            
         GOTO1 AVALOPT,BOPARM,ALOTAB,BOWORK2,(R0)                               
         BNE   EXITL               ERROR                                        
*                                                                               
         CLC   SFLTOPS,LSOPS       TEST CHANGE OF OPTIONS                       
         BNE   EXITH                                                            
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR THIS SCREEN - R1=A(FIRST FOOT)                             *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST  LR    R2,R1               DISPLAY JOB INFO                             
         LA    RF,=AL1(SCITCBRP,SCITCBWP,SCITCBAP,EOT)                          
         GOTO1 AUPDJOB,BOPARM,(C'D',(RF)),AIO4,FOOTLINL(R2)                     
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT TABLE                                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   SLL   R1,2                                                             
         B     *(R1)                                                            
         B     VALYES                                                           
         B     VALCLR                                                           
*                                                                               
VALYES   CLI   TLRNWOF,1                                                        
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$USZWR) USE ZOOMWR WHEN > 1 WRITE OFF             
         B     EXITN                                                            
*                                                                               
         L     R2,AIO1             R2=A(TRANSACTION RECORD)                     
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST          R3=A(FIRST ELEMENT ON TRANSACTION)           
         USING PTAELD,R3                                                        
         XR    R0,R0                                                            
VALY10   CLI   PTAEL,0             END OF RECORD?                               
         BE    VALY30                                                           
         CLI   PTAEL,PTAELQ        PROD TRANSACTION ACTIVITY ELEMENT?           
         BE    *+14                                                             
*                                                                               
VALY20   IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         B     VALY10                                                           
*                                                                               
         CLI   PTATYPE,PTATWOF     WRITE OFF?                                   
         BNE   VALY20                                                           
         TM    PTASTAT1,PTASPEND   PENDING ACTIVITY?                            
         BO    VALY20                                                           
         LR    RF,R3               KEEP TRACK OF LATEST WRITE-OFF               
         B     VALY20                                                           
*                                                                               
VALY30   LR    R3,RF               R3=A(LATEST WRITE-OFF PTAEL)                 
         MVC   BOHALF1,PTASEQN                                                  
         DROP  R3                                                               
*                                  ADD RECOVERY PENDING PTAEL                   
         GOTO1 AGETPTA,BOPARM,AIO1,AIO5,LSPRATA,('PTATWOFR',BOHALF1)            
         BNE   EXITN                                                            
*                                                                               
         CP    PP$AWOFF,BCPZERO    DRAFT WRITE-OFFS?                            
         BNE   VALY60                                                           
         BAS   RE,GETTRX                                                        
         USING TRXELD,R3                                                        
         OI    TRXSTA2,TRXSWOFP    SET WRITE-OFF (RECOVERY) PENDING             
         OI    TRNRSTA2,TRNSWOFP                                                
         DROP   R3                                                              
*                                                                               
VALY60   OI    LSINDS1,LSIUPREC                                                 
         B     EXITY                                                            
         DROP  R2                                                               
*                                                                               
VALCLR   L     R2,AIO1             R2=A(TRANSACTION RECORD)                     
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST          R3=A(FIRST ELEMENT ON TRANSACTION)           
         USING PTAELD,R3                                                        
         XR    R0,R0                                                            
VALC10   CLI   PTAEL,0             END OF RECORD?                               
         BE    VALC30                                                           
         CLI   PTAEL,PTAELQ        PROD TRANSACTION ACTIVITY ELEMENT?           
         BE    *+14                                                             
*                                                                               
VALC20   IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         B     VALC10                                                           
*                                                                               
         CLI   PTATYPE,PTATWOFR    WRITE-OFF RECOVERY?                          
         BNE   VALC20                                                           
         TM    PTASTAT1,PTASPEND   PENDING ACTIVITY?                            
         BNO   VALC20                                                           
         MVI   PTAEL,FF                                                         
         B     VALC20                                                           
*                                                                               
VALC30   GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('FF',AIO1),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XR    R0,R0               UPDATE PRORATA BLOCK                         
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BOPARM,AIO1,AGOPBLK,ACOM,(R0),LSPRATA,0                 
*                                                                               
         CP    PP$AWOFF,BCPZERO    DRAFT WRITE-OFFS?                            
         BNE   VALC60                                                           
         BAS   RE,GETTRX                                                        
         USING TRXELD,R3                                                        
         NI    TRXSTA2,FF-TRXSWOFP SET WRITE-OFF (RECOVERY) PENDING             
         NI    TRNRSTA2,FF-TRNSWOFP                                             
         DROP   R3                                                              
*                                                                               
VALC60   OI    LSINDS1,LSIUPREC                                                 
         B     EXITY                                                            
         DROP  R2                                                               
         SPACE 1                                                                
GETTRX   NTR1  ,                                                                
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST                                                       
         XR    RF,RF                                                            
         USING TRXELD,R3                                                        
GETTRX02 CLI   TRXEL,TRXELQ                                                     
         BE    GETTRXX                                                          
         CLI   TRXEL,0                                                          
         BE    *+12                                                             
         IC    RF,TRXLN                                                         
         BXH   R3,RF,GETTRX02                                                   
         LA    R3,BOELEM                                                        
         XC    TRXELD(TRXLN1Q),TRXELD                                           
         MVI   TRXEL,TRXELQ                                                     
         MVI   TRXLN,TRXLN1Q                                                    
         MVC   TRXSTA2,TRNRSTA2                                                 
         NI    TRXSTA2,TRXSXFRP+TRXSWOFP+TRXSBILP                               
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),TRNRECD,TRXELD                       
         CLI   12(R1),0                                                         
         BE     *+6                                                             
         DC     H'0'                                                            
         L      R3,16(R1)                                                       
*                                                                               
GETTRXX  XIT1  REGS=(R3)                                                        
         DROP  R2,R3                                                            
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
VALCLM   DC    H'0'                THERE ARE NO OPEN COLUMNS                    
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
         B     EXITN               WORKCODE                                     
         B     DISNWOF             NUMBER OF WRITE OFFS                         
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY NUMBER OF UPDATED WRITE-OFFS                                *         
***********************************************************************         
         SPACE 1                                                                
DISNWOF  CURED TLRNWOF,(3,FVIFLD),0,DMCB=BODMCB                                 
         B     EXIT                                                             
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
         CLC   TRNKDATE,BCSPACES                                                
         BE    GETNEXT             NOT TRANSACTION RECORD                       
*                                  GENERAL FILTERS                              
         GOTO1 ASETTRN,BOPARM,(C'D',TRNRECD)                                    
         BNE   GETNEXT                                                          
*                                                                               
         GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         USING TRNELD,TRNRFST                                                   
*                                                                               
GETN100  GOTO1 ASETTRN,BOPARM,(C'M',TRNRECD)                                    
         BNE   GETNEXT                                                          
         CP    PA$WOFAM,BCPZERO    ONLY SHOW UPDATED WRITE-OFFS                 
         BE    GETNEXT                                                          
*                                                                               
         LA    R3,TRNRFST                                                       
         USING PTAELD,R3                                                        
GETN700  CLI   PTAEL,0                                                          
         BE    GETNOKX                                                          
         CLI   PTAEL,PTAELQ                                                     
         BE    GETN720                                                          
GETN710  SR    R0,R0                                                            
         IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         B     GETN700                                                          
*                                                                               
GETN720  TM    PTASTAT1,PTASPEND   PENDING?                                     
         BO    GETN710                                                          
         CLI   PTATYPE,PTATWOF     WRITE OFF?                                   
         BNE   GETN730                                                          
         SR    RF,RF                                                            
         IC    RF,TLRNWOF                                                       
         LA    RF,1(RF)                                                         
         STC   RF,TLRNWOF                                                       
         B     GETN710                                                          
GETN730  CLI   PTATYPE,PTATWOFR    WRITE OFF RECOVERY?                          
         BNE   GETN710                                                          
         SR    RF,RF                                                            
         IC    RF,TLRNWOF                                                       
         BCTR  RF,0                                                             
         STC   RF,TLRNWOF                                                       
         B     GETN710                                                          
*                                                                               
GETNOKX  B     EXITY                                                            
         DROP  R3                                                               
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
ACCMST   DC    C'ACCMST '                                                       
*                                                                               
DEFCLM   EQU   *                                                                
         DC    AL1(PRO#UWO)                                                     
         DC    AL1(PRO#PRCV)                                                    
         DC    AL1(PRO#HRS)                                                     
         DC    AL1(RCV#NWO)                                                     
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
TWAD     DSECT                                                                  
         ORG   OSVALS                                                           
SFLTOPS  DS    CL(LSFLTOPL)        SAVED FILTERING OPTIONS                      
         ORG   SFLTOPS                                                          
*                                                                               
       ++INCLUDE ACCLBCOLSC                                                     
         SPACE 1                                                                
WORKD    DSECT                                                                  
         ORG   OVERWRK                                                          
*                                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'242ACCLB16   08/16/00'                                      
         END                                                                    
