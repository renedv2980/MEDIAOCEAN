*          DATA SET ACCLB55    AT LEVEL 089 AS OF 08/16/00                      
*PHASE T62155A                                                                  
CLB55    TITLE '- PC COMMS - TRANSACTION LISTS'                                 
CLB55    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB55**,R8,RR=RE                                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         ST    RE,BORELO                                                        
         L     RC,ALINK                                                         
         USING LINKD,RC                                                         
*                                                                               
         CLI   LINKMODE,ROUTSN                                                  
         BNL   EXITY                                                            
         XR    RF,RF                                                            
         IC    RF,LINKMODE                                                      
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     SETMAP              SET A(MAP TABLE)                             
         B     RCVFST              FIRST FOR RECEIVE                            
         B     RCVHDRF             FIRST FOR MAP HEADER RECEIVE                 
         B     RCVDATA             DATA RECEIVE                                 
         B     RCVHDRL             LAST FOR MAP HEADER RECEIVE                  
         B     RCVLST              LAST FOR RECEIVE                             
         B     SND                 SEND                                         
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITY    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SET A(MAP TABLE)                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETMAP   DS    0H                                                               
         LA    RF,MAPTAB           SET A(MAP TABLE)                             
         ST    RF,AMAPTAB                                                       
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR RECEIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
RCVFST   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR MAP HEADER RECEIVE                                        *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRF  DS    0H                                                               
         XC    KEYSAVE,KEYSAVE                                                  
         MVC   WOACC,BCSPACES                                                   
         MVC   WOREF,BCSPACES                                                   
         XC    WODATE,WODATE                                                    
         MVC   WODEP,BCSPACES                                                   
         MVC   WOSTF,BCSPACES                                                   
         MVC   WONAR,BCSPACES                                                   
         MVI   WBACT,0                                                          
         XC    RATE,RATE                                                        
         XC    CURR,CURR                                                        
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ELEMENT DATA RECEIVE                                                *         
***********************************************************************         
         SPACE 1                                                                
RCVDATA  DS    0H                                                               
         ICM   RF,15,ORCVDATA                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* RECEIVE JOB CODE                                                    *         
***********************************************************************         
*                                                                               
RCVJOB   MVC   THISJOB,DATA        RECEIVE JOB KEY                              
         B     EXITY                                                            
*                                                                               
***********************************************************************         
* RECEIVE DISK ADDRESS                                                *         
***********************************************************************         
*                                                                               
RCVDA    MVC   TRANDA,DATA         RECEIVE DISK ADDRESS                         
         B     EXITY                                                            
*                                                                               
RCVALCOM ZAP   ALLOCCOM,DATA(L'ALLOCCOM)   ALLOCATED COMMISSION                 
         B     EXITY                                                            
***********************************************************************         
* RECEIVE ALLOCATION / TRANSFER / WRITE OFF (& RECOVERY)              *         
* IO3=TRANSACTION ACCMST                                              *         
* IO4=USED FOR PTAREC                                                 *         
* IO5=JOB RECORD ACCMST                                               *         
***********************************************************************         
                                                                                
RCVAXW   L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING PRORATAD,LSPRATA                                                 
         USING TLSTD,LSTLST                                                     
                                                                                
         MVC   IODAOVER,TRANDA              GET THE TRANSACTION RECORD          
         GOTO1 AIO,IO3+IOACCMST+IOGETRUP    ACCMST INTO IO3                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,AIO3                                                          
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'TRNKCULA),0(R1)                                          
         L     R1,=A(IO5+IOACCDIR+IOREAD)                                       
         GOTO1 AIO                          JOB RECORD INTO IO5                 
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=A(IO5+IOACCMST+IOGETRUP)                                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETUP,BODMCB,IOKEY+3,0,0                                        
                                                                                
         MVC   CSEXCRAT,RATE                                                    
         CLC   CURR,BCSPACES                                                    
         BNH   *+10                                                             
         MVC   CSBILCUR,CURR                                                    
         XR    R0,R0                        CALL PRORATA                        
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,AIO3,AGOPBLK,ACOM,(X'80',(R0)),LSPRATA, X        
               CSCPYCUR                                                         
                                                                                
         LA    R0,LSPRATAS                  COPY LSPRATA TO LSPRATAS            
         L     R1,=A(PR$LNQ)                                                    
         LA    RE,LSPRATA                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         XC    IOKEY,IOKEY         GET TRANSACTION ACCDIR                       
         L     R1,AIO3                                                          
         MVC   IOKEY(L'TRNKEY),0(R1)                                            
         GOTO1 AIO,IO2+IOACCDIR+IOREAD                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 ASETTRN,BOPARM,(C'S',IOKEY),AIO3,LSPRATA                         
                                                                                
***********************************************************************         
* WRITE OFF RECOVERY                                                  *         
***********************************************************************         
                                                                                
         L     R1,AMDEL                                                         
         USING MDELD,R1                                                         
         CLC   MDCODE,=AL2(MCWOREFQ)        TEST DOING A RECOVERY               
         BNE   RREGALL                      NO, TRY REGULAR ALLOCATION          
         CLI   WBACT,1                      CLEARING A PENDING RECOV. ?         
         BE    RWBCLEAR                                                         
                                                                                
         L     R2,AIO3             R2=A(TRANSACTION RECORD)                     
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST          R3=A(FIRST ELEMENT ON TRANSACTION)           
         USING PTAELD,R3                                                        
         XR    R0,R0                                                            
RCVWB10  CLI   PTAEL,0             END OF RECORD?                               
         BE    RCVWB30                                                          
         CLI   PTAEL,PTAELQ        PROD TRANSACTION ACTIVITY ELEMENT?           
         BE    *+14                                                             
*                                                                               
RCVWB20  IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         B     RCVWB10                                                          
*                                                                               
         CLI   PTATYPE,PTATWOF     WRITE OFF?                                   
         BNE   RCVWB20                                                          
         TM    PTASTAT1,PTASPEND   PENDING ACTIVITY?                            
         BO    RCVWB20                                                          
         CLC   PTAWREF,DATA        MATCH ON W/O REF                             
         BNE   RCVWB20                                                          
*                                                                               
RCVWB30  MVC   BOHALF1,PTASEQN                                                  
         DROP  R2,R3                                                            
*                                  ADD RECOVERY PENDING PTAEL                   
         GOTO1 AGETPTA,BODMCB,AIO3,AIO4,PRORATAD,('PTATWOFR',BOHALF1)           
         BNE   EXITN                                                            
                                                                                
         BAS   RE,GETTRX                                                        
         OI    TRXSTA2-TRXELD(R3),TRXSWOFP  SET RECOVERY PENDING                
         L     R1,AIO3                                                          
         OI    TRNRSTA2-TRNRECD(R1),TRNSWOFP                                    
         B     RAXWUPD                      WRITE BACK JOB                      
                                                                                
***********************************************************************         
* CLEAR PENDING RECOVERIES                                            *         
***********************************************************************         
                                                                                
RWBCLEAR L     R3,AIO3                                                          
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         USING PTAELD,R3                                                        
         XR    R0,R0                                                            
         XR    R4,R4                                                            
RWBCLR10 CLI   PTAEL,0             END OF RECORD?                               
         BE    RWBCLR30                                                         
         CLI   PTAEL,PTAELQ                                                     
         BE    *+14                                                             
*                                                                               
RWBCLR20 IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         B     RWBCLR10                                                         
*                                                                               
         CLI   PTATYPE,PTATWOFR    WRITE-OFF RECOVERY?                          
         BNE   RWBCLR20                                                         
         TM    PTASTAT1,PTASPEND   PENDING ACTIVITY?                            
         BNO   RWBCLR20                                                         
         LA    R4,1(R4)            COUNT # OF PENDING RECOVERIES                
         CLC   PTAWREF,DATA                                                     
         BNE   RWBCLR20                                                         
         MVI   PTAEL,FF                                                         
         BCTR  R4,0                                                             
         B     RWBCLR20                                                         
*                                                                               
RWBCLR30 GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('FF',AIO3),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
         XR    R0,R0               UPDATE PRORATA BLOCK                         
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    RWBCLR32                                                         
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    RWBCLR32                                                         
         LA    R0,CSEXCVAL                                                      
RWBCLR32 GOTO1 APRORATA,BOPARM,AIO3,AGOPBLK,ACOM,(R0),LSPRATA,0                 
                                                                                
         CP    PP$AWOFF,BCPZERO    DRAFT WRITE-OFFS?                            
         BNE   RAXWUPD                                                          
         LTR   R4,R4               ANY MORE RECOVERIES PENDING ?                
         BNZ   RAXWUPD                                                          
         BAS   RE,GETTRX                                                        
         NI    TRXSTA2-TRXELD(R3),FF-TRXSWOFP                                   
         L     R1,AIO3                                                          
         NI    TRNRSTA2-TRNRECD(R1),FF-TRNSWOFP                                 
         B     RAXWUPD                                                          
                                                                                
***********************************************************************         
* REGULAR ALLOCATION, WRITE OFF AND TRANSFERS                         *         
***********************************************************************         
                                                                                
RREGALL  DS    0H                                                               
         TM    TLXPEND,TLXPREV                                                  
         BO    EXITN               CANT ALLOCATE IF PENDING REVERSAL            
                                                                                
         LA    R2,PTASPEND+PTASCASH         SET ALLOCATING CASH                 
         LA    RF,PTATRAL                                                       
         CLC   MDCODE,=AL2(MCALLOCQ)        REGULAR ALLOCATION ?                
         BE    RREGAL10                                                         
         LA    RF,PTATTRFT                                                      
         CLC   MDCODE,=AL2(MCXFRQ)          TRANSFER ?                          
         BE    RREGAL10                                                         
         LA    RF,PTATWOF                                                       
         CLC   MDCODE,=AL2(MCWOQ)           WRITE OFF ?                         
         BE    RREGAL10                                                         
                                                                                
         LA    R2,PTASPEND+PTASHOUR         SET ALLOCATING HOURS                
         LA    RF,PTATRAL                                                       
         CLC   MDCODE,=AL2(MCALLHRQ)        REGULAR ALLOCATION ?                
         BE    RREGAL10                                                         
         LA    RF,PTATTRFT                                                      
         CLC   MDCODE,=AL2(MCXFRHRQ)        TRANSFER ?                          
         BE    RREGAL10                                                         
         LA    RF,PTATWOF                                                       
         CLC   MDCODE,=AL2(MCWOFHRQ)        WRITE OFF ?                         
         BE    RREGAL10                                                         
         DC    H'0'                                                             
         DROP  R1                                                               
                                                                                
RREGAL10 GOTO1 AALLTRN,BODMCB,((RF),AIO3),((R2),PRORATAD),(1,DATA),    X        
               (1,ALLOCCOM)                                                     
         BNE   EXITN                                                            
                                                                                
***********************************************************************         
* SET WRITE OFF DETAILS                                               *         
***********************************************************************         
                                                                                
RWODET   L     R1,AMDEL                       TEST IF DOING A WRITE OFF         
         CLC   =AL2(MCWOQ),MDCODE-MDELD(R1)          CASH                       
         BE    *+14                                                             
         CLC   =AL2(MCWOFHRQ),MDCODE-MDELD(R1)       OR HOURS                   
         BNE   RAXWUPD                                                          
                                                                                
         GOTO1 AGETPTA,BODMCB,AIO3,AIO4,PRORATAD,0                              
         BL    EXITN                                                            
         L     R2,0(R1)                                                         
         USING PTAELD,R2                                                        
         CLC   WOACC,BCSPACES              WO ACCOUNT UPLOADED?                 
         BNH   *+10                                                             
         MVC   PTAWEXPA,WOACC              YES,INSERT                           
         CLI   PTAWEXPA,C' '               TEST IF ANY SET                      
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$CUFWO)      CANT USE FLD WITHOUT ACCOUNT         
         B     EXITN                                                            
                                                                                
         MVI   FVXLEN,L'PTAWEXPA           SET UP FVAL TO FOOL VALWEX           
         MVI   FVILEN,L'PTAWEXPA+1                                              
         MVI   FVIFLD,C'*'                 OVERIDE UNIT/LEDGER                  
         MVC   FVIFLD+1(L'PTAWEXPA),PTAWEXPA                                    
         L     R1,AMDEL                                                         
         CLC   =AL2(MCWOQ),MDCODE-MDELD(R1) TEST CASH WO                        
         BE    *+8                                                              
         LA    RF,X'40'                     SET TO VALIDATE FOR TIME            
         GOTO1 AVALWEX,BODMCB,((RF),0)                                          
         BNE   EXITN                                                            
                                                                                
         OC    WODATE,WODATE                                                    
         BZ    *+10                                                             
         MVC   PTAWDAT,WODATE              SET WRITE OFF DATE                   
         CLC   WOREF,BCSPACES                                                   
         BNH   *+10                                                             
         MVC   PTAWREF,WOREF               AND REFERENCE                        
         DROP  R2                                                               
                                                                                
         CLI   WONARLEN,0                   ANY NARRATIVE RECEIVED?             
         BE    RWODET10                                                         
         L     R2,AIO4                      R2=A(PTAREC)                        
         USING TRNRECD,R2                                                       
         XC    BOELEM,BOELEM                                                    
         LA    R3,BOELEM                                                        
         USING TRNELD,R3                                                        
         IC    RE,TRNRFST+(TRNLN-TRNELD)                                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TRNELD(0),TRNRFST                                                
                                                                                
         MVC   TRNNARR,WONAR                                                    
         XR    RF,RF                                                            
         IC    RF,WONARLEN                                                      
         LA    RF,TRNLN1Q(RF)             SET ELEMENT LENGTH                    
         STC   RF,TRNLN                                                         
                                                                                
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('TRNELQ',TRNRECD),0                 
         MVI   TRNEL,1             ADD ELEMENT AT START OF RECORD               
         GOTO1 (RF),(R1),(C'P',ACCMST),TRNRECD,TRNELD                           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRNRFST,TRNELQ                                                   
         DROP  R2,R3                                                            
                                                                                
RWODET10 XC    BODMCB,BODMCB                                                    
         GOTO1 AWOPPTA,BODMCB,AIO3,AIO4,WODEP,WOSTF                             
         ORG   *-2                                                              
         CLC   WODEP,BCSPACES                                                   
         BNE   *+10                                                             
         XC    8(4,R1),8(R1)                                                    
         CLC   WOSTF,BCSPACES                                                   
         BNE   *+10                                                             
         XC    12(4,R1),12(R1)                                                  
         CLC   =PL8'0',DATA        IF CLEARING DON'T SEND DEP/STAFF             
         BNE   *+10                                                             
         XC    8(8,R1),8(R1)                                                    
         BASR  RE,RF                                                            
         BL    EXITN                                                            
                                                                                
         GOTO1 APUTPTA,BODMCB,AIO3,AIO4                                         
         BL    EXITN                                                            
                                                                                
***********************************************************************         
* TRANSACTION / JOB UPDATE                                            *         
***********************************************************************         
                                                                                
RAXWUPD  GOTO1 AIO,IO3+IOACCMST+IOPUT      WRITE BACK TRANSACTION MST           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    IOKEY,IOKEY                 GET TRANSACTION ACCDIR               
         L     R1,AIO3                                                          
         MVC   IOKEY(L'TRNKEY),0(R1)                                            
         GOTO1 AIO,IO2+IOACCDIR+IORDUP                                          
                                                                                
K        USING TRNRECD,IOKEY               COPY STATUS                          
         L     RE,AIO3                                                          
         MVC   K.TRNKSTAT,TRNRSTAT-TRNRECD(RE)                                  
         MVC   K.TRNKSTA2,TRNRSTA2-TRNRECD(RE)                                  
         GOTO1 AIO,IO2+IOACCDIR+IOWRITE    WRITE BACK TRANSACTION DIR           
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  K                                                                
                                                                                
         GOTO1 AUPDJOB,BOPARM,(C'U',AIO3),AIO5,LSPRATAS,LSPRATA                 
                                                                                
         L     R1,=A(IO5+IOACCMST+IOPUT)                                        
         GOTO1 AIO                        WRITE BACK JOB RECORD                 
         BE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R5                                                               
**********************************************************************          
*GETTRX    DATA SET ACCLB16B   AT LEVEL 243 AS OF 29/12/98                      
**********************************************************************          
         SPACE 1                                                                
GETTRX   NTR1  ,                                                                
         L     R2,AIO3                                                          
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
***********************************************************************         
* RECEIVE TRANSFER TO INFORMATION                                     *         
***********************************************************************         
                                                                                
RCVXFRTO CLC   DATA(L'ACTKACT),BCSPACES                                         
         BE    RXFRTO30                                                         
         L     R1,AMDEL                                                         
         CLC   =AL2(MCTOJOBQ),MDCODE-MDELD(R1)   IS IT TO JOB?                  
         BE    RXFRTO10                                                         
*                                                                               
         LA    RF,IOKEY                                                         
         USING WCORECD,RF                                                       
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   WCOKWRK,DATA                                                     
         DROP  RF                                                               
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    RXFRTO30                                                         
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     EXITN                                                            
                                                                                
RXFRTO10 MVC   CSBSECL,BCCPYEL+(CPYBSEC-CPYELD)                                 
         MVCDD BOWORK2(3),AC#TODAY SET LOCK FOR TRANSFERS                       
         GOTO1 VDICTAT,BOPARM,C'SU  ',BOWORK2,0                                 
         GOTO1 VBMONVAL,(R1),(3,BOWORK2),('POSTTRNF',ACOM),            *        
               (CULANG,BOWORK1),(CUABIN,0)                                      
         MVC   CSBSECL,0(R1)                                                    
*                                                                               
         LA    RF,IOKEY                                                         
         USING ACTRECD,RF                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   ACTKACT,DATA                                                     
         GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('JOBELQ',AIO1),0                    
         CLI   12(R1),0                                                         
         BNE   RXFRTO20                                                         
*                                                                               
         L     RE,12(R1)                                                        
         TM    JOBSTA1-JOBELD(RE),JOBSXJOB                                      
         BNO   RXFRTO20                                                         
         MVC   FVMSGNO,=AL2(AE$CUEXJ)                                           
         B     EXITN                                                            
*                                                                               
RXFRTO20 TM    ACBSTAT,ACBSCLSE                                                 
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTCL)                                           
         B     EXITN                                                            
         TM    ACBSTAT,ACBSLOCK                                                 
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     EXITN                                                            
         TM    ACINDS1,ACIPRJOB                                                 
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$WLACT)                                           
         B     EXITN                                                            
         DROP  RF                                                               
*                                                                               
RXFRTO30 MVC   IODA,TRANDA                                                      
         GOTO1 AIO,IO3+IOACCMST+IOGETRUP                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
*                                                                               
         LA    R2,TRNRFST-TRNRECD(R2)                                           
         USING PTAELD,R2                                                        
         XR    R0,R0                                                            
RXFRTO40 CLI   PTAEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PTAEL,PTAELQ                                                     
         BNE   *+8                                                              
         CLI   PTATYPE,PTATTRFT                                                 
         BNE   *+12                                                             
         TM    PTASTAT1,PTASPEND                                                
         BO    RXFRTO50                                                         
         IC    R0,PTALN                                                         
         AR    R2,R0                                                            
         B     RXFRTO40                                                         
*                                                                               
RXFRTO50 L     R1,AMDEL                                                         
         CLC   =AL2(MCTOJOBQ),MDCODE-MDELD(R1)   IS IT TO JOB?                  
         BNE   *+14                                                             
         MVC   PTATJOB,DATA                                                     
         B     *+10                                                             
         MVC   PTATWRK,DATA                                                     
*&&UK                                                                           
         L     RF,AIO3                                                          
         USING TRNRECD,RF                                                       
         CLC   TRNKWORK,PTATWRK  TEST CHANGE IN WORK-CODE                       
         BNE   RXFRTO60                                                         
         CLC   TRNKACT,PTATJOB   TEST CHANGE IN JOB                             
         BNE   RXFRTO60                                                         
         MVC   FVMSGNO,=AL2(AE$WCJBD)                                           
         B     EXITN                                                            
         DROP  RF                                                               
*&&                                                                             
RXFRTO60 GOTO1 AIO,IO3+IOACCMST+IOPUT                                           
         BE    EXITY                                                            
         DC    H'0'                                                             
*                                                                               
***********************************************************************         
* RECEIVE WRITE OFF DETAILS                                           *         
***********************************************************************         
*                                                                               
RCVWODAT MVC   WODATE,DATA                                                      
         B     EXITY                                                            
                                                                                
RCVWBACT MVC   WBACT,DATA          1=CLEAR PENDING WRITE BACK                   
         B     EXITY                                                            
                                                                                
RCVWOREF XR    RE,RE                                                            
         ICM   RE,3,DATALENX                                                    
         BM    EXITY                                                            
         EX    RE,*+4                                                           
         MVC   WOREF(0),DATA                                                    
         OC    WODATE,WODATE   IF WEVE GOT DATE MUST BE SETTING DETAILS         
         BNZ   EXITY                                                            
         B     RCVAXW          ELSE THIS IS THE REF FOR WRITE BACK              
                                                                                
RCVWOACC XR    RE,RE                                                            
         ICM   RE,3,DATALENX                                                    
         BM    EXITY                                                            
         EX    RE,*+4                                                           
         MVC   WOACC(0),DATA                                                    
         B     EXITY                                                            
                                                                                
RCVWODEP XR    RE,RE                                                            
         ICM   RE,3,DATALENX                                                    
         BM    EXITY                                                            
         EX    RE,*+4                                                           
         MVC   WODEP(0),DATA                                                    
         B     EXITY                                                            
                                                                                
RCVWOSTF XR    RE,RE                                                            
         ICM   RE,3,DATALENX                                                    
         BM    EXITY                                                            
         EX    RE,*+4                                                           
         MVC   WOSTF(0),DATA                                                    
         B     EXITY                                                            
                                                                                
RCVWONAR MVC   WONARLEN,DATALEN+1  (DATALEN IS 2 BYTES)                         
         MVC   WONAR,DATA                                                       
         B     EXITY                                                            
         EJECT                                                                  
                                                                                
RCVNARR  MVC   IODA,TRANDA                                                      
         GOTO1 AIO,IO3+IOACCMST+IOGETRUP                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         USING TRNRECD,R2                                                       
                                                                                
         XC    BOELEM,BOELEM                                                    
         LA    R3,BOELEM                                                        
         USING TRNELD,R3                                                        
         ZIC   RE,TRNRFST+(TRNLN-TRNELD)                                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TRNELD(0),TRNRFST                                                
*                                                                               
         MVC   TRNNARR,DATA                                                     
         XR    RF,RF               FIND LENGTH OF NARRATIVE                     
         ICM   RF,1,DATALEN+1                                                   
         LA    RF,TRNLN1Q(RF)      SET ELEMENT LENGTH                           
         STC   RF,TRNLN                                                         
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('TRNELQ',TRNRECD),0                 
         MVI   TRNEL,1             ADD ELEMENT AT START OF RECORD               
         GOTO1 (RF),(R1),(C'P',ACCMST),TRNRECD,TRNELD                           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRNRFST,TRNELQ                                                   
*                                                                               
         GOTO1 AIO,IO3+IOACCMST+IOPUT                                           
         BE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* RECEIVE DRAFT / UPDATE / AUTOFORM ACTION                            *         
* BREF = CL4 BATCH REFERENCE                                          *         
* BMON = CL12 DATE EXPRESSION                                         *         
* DATA(1) = ACTION                                                    *         
***********************************************************************         
         SPACE 1                                                                
RCVDRUP  DS    0H                                                               
         GOTO1 ASETUP,BODMCB,THISJOB,0,0                                        
         XR    RF,RF                                                            
         MVC   THISACT,DATA                                                     
         ICM   RF,1,THISACT                                                     
         CLM   RF,1,=AL1(RCVDUAM)                                               
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     RCVDUACT(RF)                                                     
                                                                                
RCVDUACT DS    0XL4                                                             
         B     EXITY               0 - ????                                     
         B     XFRDR               1 - DRAFT TRANSFER                           
         B     XFRUP               2 - UPDATE TRANSFER                          
         B     WOFDR               3 - DRAFT WRITE-OFF                          
         B     WOFUP               4 - UPDATE WRITE-OFF                         
         B     WBADR               5 - DRAFT WRITE BACK                         
         B     WBAUP               6 - UPDATE WRITE BACK                        
         B     FEEDR               7 - DRAFT INTERNAL INVOICE                   
         B     FEEUP               8 - UPDATE INTERNAL INVOICE                  
RCVDUAM  EQU   (*-RCVDUACT)/L'RCVDUACT                                          
                                                                                
XFRDR    LA    R0,C'D'                                                          
         B     *+8                                                              
XFRUP    LA    R0,C'U'                                                          
         LA    RF,PBXFRQ                                                        
         B     RCVDP10                                                          
*                                                                               
WOFDR    LA    R0,C'D'                                                          
         B     *+8                                                              
WOFUP    LA    R0,C'U'                                                          
         LA    RF,PBWOFQ                                                        
         B     RCVDP10                                                          
*                                                                               
WBADR    LA    R0,C'D'                                                          
         B     *+8                                                              
WBAUP    LA    R0,C'U'                                                          
         LA    RF,PBRECQ                                                        
         B     RCVDP10                                                          
*                                                                               
FEEDR    LA    R0,C'D'                                                          
         B     *+8                                                              
FEEUP    LA    R0,C'U'                                                          
         LA    RF,PBFEEQ                                                        
*                                                                               
RCVDP10  GOTO1 AVALBAT,BOPARM,((R0),(RF)),BREFH,BMONH                           
         B     EXIT                EXIT WITH CC SET                             
         SPACE 1                                                                
***********************************************************************         
* RECEIVE BATCH REFERENCE                                             *         
***********************************************************************         
         SPACE 1                                                                
RCVBREF  DS    0H                                                               
         MVC   BREFH+5(1),DATALEN+1                                             
         MVC   BREF,DATA                                                        
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* RECEIVE BATCH MONTH                                                 *         
***********************************************************************         
         SPACE 1                                                                
RCVBMON  DS    0H                                                               
         MVC   BMONH+5(1),DATALEN+1                                             
         MVC   BMON,DATA                                                        
         B     EXITY                                                            
***********************************************************************         
* RECEIVE EXCHANGE RATE AND CURRENCY CODE                             *         
***********************************************************************         
         SPACE 1                                                                
RCVRATE  DS    0H                                                               
         MVC   RATE,DATA                                                        
         B     EXITY                                                            
RCVCUR   DS    0H                                                               
         MVC   CURR,DATA                                                        
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* LAST FOR MAP HEADER RECEIVE                                         *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRL  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR RECEIVE                                                    *         
***********************************************************************         
         SPACE 1                                                                
RCVLST   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* SEND                                                                *         
***********************************************************************         
         SPACE 1                                                                
SND      DS    0H                                                               
         ICM   RF,15,OSND                                                       
         BNZR  RF                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SEND TRANSACTION LIST                                               *         
***********************************************************************         
                                                                                
SNDTRL   EQU   *                                                                
         CLI   THISACT,0                                                        
         BNZ   SNDDET                                                           
                                                                                
         PUSH  USING                                                            
         L     R3,ALSVALS                                                       
         USING LSVALSD,R3                                                       
         USING PRORATAD,LSPRATA                                                 
         GOTO1 ASETUP,BODMCB,THISJOB,0,0                                        
K        USING TRNRECD,IOKEY                                                    
         MVC   K.TRNKEY,BCSPACES                                                
         MVC   K.TRNKCPY,CUABIN                                                 
         MVC   K.TRNKUNT(L'BCCPYPRD),BCCPYPRD                                   
         MVC   K.TRNKACT,THISJOB                                                
                                                                                
         GOTO1 AIO,IO1+IOACCDIR+IOHI                                            
         BE    STRN20                                                           
         DC    H'0'                                                             
                                                                                
STRN10   GOTO1 AIO,IO1+IOACCDIR+IOSEQ                                           
         BE    *+6                                                              
         DC    H'0'                                                             
STRN20   CLC   K.TRNKACT,THISJOB     CHECK SAME JOB                             
         BNE   STRNX                                                            
                                                                                
* NEED TO CALL GETOPT FOR WC LEVEL COMM RATE                                    
         CLC   K.TRNKWORK,TRNKWORK-TRNRECD+IOKEYSAV                             
         BE    STRN28                                                           
         GOTO1 AGETOPT,BODMCB,(X'80',K.TRNRECD)                                 
         GOTO1 AIO,IO1+IOACCDIR+IOREAD RE-ESTABLISH READ SEQUENCE               
                                                                                
STRN28   MVI   BCBYTE1,0                                                        
         CLC   K.TRNKREF,BCSPACES                                               
         BNH   STRN10                                                           
         TM    K.TRNKSTAT,TRNSREVS                                              
         BNZ   STRN10                                                           
         TM    K.TRNKSTAT,TRNSDRFT                                              
         BNO   *+8                                                              
         OI    BCBYTE1,TRNSDRFT    SET TO SAY DRAFT FOUND                       
         TM    K.TRNKSTA2,TRNSEXCL                                              
         BO    STRN10                                                           
         CLC   K.TRNKWORK,=C'99'     DONE WHEN REACH 99'S                       
         BNL   STRNX                                                            
                                                                                
         GOTO1 AIO,IO1+IOACCMST+IOGET                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1                                                          
         CLI   TRNRFST-TRNRECD(RF),TRNELQ                                       
         BNE   STRN10                                                           
                                                                                
         MVC   CSEXCRAT,RATE       USE EXCHANGE RATE SENT                       
         XR    R0,R0                                                            
         CLC   CURR,BCSPACES                                                    
         BNH   *+10                                                             
         MVC   CSBILCUR,CURR                                                    
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,AIO1,AGOPBLK,ACOM,(X'80',(R0)),PRORATAD,X        
               CSCPYCUR                                                         
                                                                                
*        CP    PA$NETUB,BCPZERO    ANYTHING UNBILLED ?                          
*        BNE   *+14                                                             
*        CP    PA$WOFAM,BCPZERO    NO, ANY UPDATED WO?                          
*        BE    STRN10              NO, SO SKIP                                  
                                                                                
         CLC   PG$BLCUR,BCSPACES   TEST ANY PRIOR ACTIVITY                      
         BNH   STRN30                                                           
         CLC   CSBILCUR,PG$BLCUR   TEST CURRENCY OF PRIOR ACTIVITY              
         BE    STRN30              MATCHES - OK                                 
         CP    PA$NETBL,BCPZERO    IF PRIOR BILLING COMES TO ZERO OK            
         BNE   STRN10                                                           
         CP    PA$COMBL,BCPZERO                                                 
         BNE   STRN10                                                           
                                                                                
STRN30   L     R2,AIO1                                                          
         LA    R2,TRNRFST-TRNRECD(R2)                                           
         XR    RF,RF                                                            
         USING TRNELD,R2                                                        
         TM    BCBYTE1,TRNSDRFT    WAS IT DRAFT?                                
         BNO   *+12                                                             
         CLI   TRNTYPE,99          ONLY DRAFT 99'S PLEASE                       
         BNE   STRN10                                                           
         TM    TRNSTAT,TRNSNOCM                                                 
         BNO   *+8                                                              
         OI    BCBYTE1,TRNSNOCM                                                 
         USING TRXELD,R2                                                        
STRN40   CLI   TRXEL,0                                                          
         BE    STRN60                                                           
         CLI   TRXEL,TRXELQ                                                     
         BE    STRN50                                                           
         IC    RF,TRXLN                                                         
         BXH   R2,RF,STRN40                                                     
STRN50   TM    TRXSTA1,TRXSXALC    TEST EXCLUDE FROM BILL ALLOCATION            
         BO    STRN10                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
* SEND REGULAR TRANSACTION DETAILS                                    *         
***********************************************************************         
                                                                                
STRN60   GOTO1 ASNDHDR,BODMCB,13   SEND TRANSACTION INFO                        
         GOTO1 ASNDDATA,BODMCB,1,K.TRNKDA                                       
         GOTO1 (RF),(R1),2,K.TRNKWORK                                           
         GOTO1 (RF),(R1),3,(L'TRNKULC,K.TRNKULC)                                
         GOTO1 (RF),(R1),4,K.TRNKDATE                                           
         GOTO1 (RF),(R1),5,K.TRNKREF                                            
         GOTO1 (RF),(R1),10,PM$ANVBL                                            
         GOTO1 (RF),(R1),11,PM$FNVBL                                            
         GOTO1 (RF),(R1),12,PP$AALLO                                            
         GOTO1 (RF),(R1),13,PP$FALLO                                            
         GOTO1 (RF),(R1),14,PP$AWOFF                                            
         GOTO1 (RF),(R1),15,PP$FWOFF                                            
         GOTO1 (RF),(R1),16,PP$AXFER                                            
         GOTO1 (RF),(R1),17,PP$FXFER                                            
         GOTO1 (RF),(R1),24,PM$HRVBL    HOURS AVAILBALE                         
         GOTO1 (RF),(R1),25,PP$HRSB     HOURS PENDING ALLOC                     
         GOTO1 (RF),(R1),26,PP$HRSX     HOURS PENDING XFR                       
         GOTO1 (RF),(R1),44,PM$ACVBL                                            
         GOTO1 (RF),(R1),45,PM$FCVBL                                            
         L     R2,AGOPBLK                                                       
         GOTO1 (RF),(R1),46,GOAGYCOM-GOBLOCKD(R2)                               
         GOTO1 (RF),(R1),47,PP$ACOMM                                            
         GOTO1 (RF),(R1),48,PP$FCOMM                                            
         TM    PG$STAT,PG$REVS     TEST IF REVERSAL ALLOCATION                  
         BNO   *+8                                                              
         OI    BCBYTE1,PG$REVS                                                  
         GOTO1 (RF),(R1),39,BCBYTE1    STATUS                                   
         L     R2,AIO1             SEND DOWN TRANSACTION NARRATIVE              
         LA    R2,TRNRFST-TRNRECD(R2)                                           
         USING TRNELD,R2                                                        
         CLI   TRNLN,TRNLN1Q                                                    
         BNH   STRN65                                                           
         XR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         SHI   RF,TRNLN1Q                                                       
         GOTO1 ASNDDATA,BODMCB,52,((RF),TRNNARR)                                
         DROP  R2,K                                                             
***********************************************************************         
* SEND TRANSFER, WRITE OFF AND RECOVERY DETAILS                       *         
***********************************************************************         
                                                                                
* DONT SEND DEFAULT WO ACC                                                      
*        L     R1,AGOPBLK                                                       
*        L     R1,GOABEXT-GOBLOCKD(R1)                                          
*        LA    R0,GOEXPOAC+1-GOBBLOCK(R1)                                       
* COULD BE GOEXPTAC IF TIME WC                                                  
*        GOTO1 ASNDDATA,BODMCB,34,(R0)      DEFAULT WO ACC                      
                                                                                
STRN65   L     R2,AIO1                                                          
         LA    R2,TRNRFST-TRNRECD(R2)                                           
STRN70   XR    R0,R0                                                            
         USING PTAELD,R2                                                        
STRN80   CLI   PTAEL,0                                                          
         BE    STRN10                                                           
         CLI   PTAEL,PTAELQ                                                     
         BNE   STRN90                                                           
         CLI   PTATYPE,PTATTRFT                                                 
         BE    STRN100                                                          
         CLI   PTATYPE,PTATWOF                                                  
         BE    STRN110                                                          
*        CLI   PTATYPE,PTATWOFR                                                 
*        BE    STRN160                                                          
STRN90   IC    R0,PTALN                                                         
         AR    R2,R0                                                            
         B     STRN80                                                           
                                                                                
STRN100  TM    PTASTAT1,PTASPEND                                                
         BNO   STRN90                                                           
         GOTO1 ASNDDATA,BODMCB,30,(L'PTATJOB,PTATJOB)                           
         GOTO1 (RF),(R1),31,(L'PTATWRK,PTATWRK)                                 
         B     STRN90                                                           
                                                                                
STRN110  TM    PTASTAT1,PTASPEND   TEST WO PENDING                              
         BO    STRN150                                                          
         CLC   PTACUR,CSBILCUR                                                  
         BE    *+14                                                             
         CLC   PTACUR,CSCPYCUR                                                  
         BNE   STRN90                                                           
         L     R4,AIO1             NO, IS THERE MATCHING UPDATED WB             
         LA    R4,TRNRFST-TRNRECD(R4)                                           
         MVI   BCBYTE1,0                                                        
PTA      USING PTAELD,R4                                                        
STRN120  CLI   PTA.PTAEL,0                                                      
         BE    STRN140               CHECKED ALL SO ADD UPDATED WO              
         CLI   PTA.PTAEL,PTAELQ                                                 
         BNE   STRN130                                                          
         CLI   PTA.PTATYPE,PTATWOFR  IS IT A WB?                                
         BNE   STRN130                                                          
         CLC   PTA.PTASEQN,PTASEQN   IS IT SAME SEQ # AS WO ?                   
         BNE   STRN130                                                          
         TM    PTA.PTASTAT1,PTASPEND IS IT UPDATED ?                            
         BNO   STRN90                SKIP THIS WO/WB                            
         MVI   BCBYTE1,1             SET RECOVERY PENDING                       
         B     STRN140                                                          
STRN130  IC    R0,PTA.PTALN                                                     
         AR    R4,R0                                                            
         B     STRN120                                                          
                                                                                
STRN140  GOTO1 ASNDDATA,BODMCB,20,PTANET    UPDATED WO DETAILS                  
         GOTO1 (RF),(R1),21,PTANETF                                             
         GOTO1 (RF),(R1),32,PTAWREF                                             
         GOTO1 (RF),(R1),33,PTAWDAT                                             
         XR    R4,R4                                                            
         LH    R4,PTAHOURS                  UPDATED HOURS WO                    
         CVD   R4,BODUB1                                                        
         GOTO1 (RF),(R1),27,BODUB1+(L'BODUB1-L'PP$HRSW)                         
         GOTO1 (RF),(R1),34,(L'PTAWEXPA,PTAWEXPA)                               
         GOTO1 SPTAREC,PTASEQN              SEND DETAILS FROM PTAREC            
         GOTO1 ASNDDATA,BODMCB,22,BCBYTE1   SEND WHETHER WB PENDING             
         B     STRN90                                                           
                                                                                
STRN150  GOTO1 ASNDDATA,BODMCB,32,PTAWREF   PENDING WO DETAILS                  
         GOTO1 (RF),(R1),33,PTAWDAT                                             
         GOTO1 (RF),(R1),34,(L'PTAWEXPA,PTAWEXPA)                               
         GOTO1 (RF),(R1),27,PP$HRSW     HOURS PENDING WO                        
         GOTO1 SPTAREC,PTASEQN     SEND DETAILS FROM PTAREC                     
         CLI   PTALN,PTAWLN2Q      ANY ANALYSIS A/C ?                           
         BNE   STRN90                                                           
         GOTO1 ASNDDATA,BODMCB,37,PTAWWACT                                      
         B     STRN90                                                           
                                                                                
*TRN160  TM    PTASTAT1,PTASPEND                                                
*        BNO   STRN90                                                           
*        MVI   BCBYTE1,1                                                        
*        GOTO1 ASNDDATA,BODMCB,22,BCBYTE1                                       
*        B     STRN90                                                           
         DROP  R2                                                               
                                                                                
STRNX    B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* SEND DETAILS FROM PTAREC                                            *         
***********************************************************************         
                                                                                
SPTAREC  NTR1  ,                   R1=A(PTASEQN)                                
         MVC   KEYSAVE,IOKEY                                                    
         LA    R2,IOKEY                                                         
         USING PTARECD,R2                                                       
         XC    PTAKEY,PTAKEY                                                    
         MVI   PTAKTYP,PTAKTYPQ                                                 
         MVC   PTAKCPY,KEYSAVE                                                  
         MVC   PTAKJOB,KEYSAVE+TRNKACT-TRNRECD                                  
         MVC   PTAKSEQN,0(R1)                                                   
         GOTO1 AIO,IO2+IOACCMST+IOREAD                                          
         BNE   SPTAEX                                                           
         L     R2,AIO2                                                          
         LA    R2,TRNRFST-TRNRECD(R2)                                           
         USING TRNELD,R2                                                        
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC     H'0'                                                            
         CLI   TRNLN,TRNLN1Q                                                    
         BE    SPTA10                                                           
         XR    R3,R3                                                            
         IC    R3,TRNLN                                                         
         SH    R3,=Y(TRNLN1Q)                                                   
         GOTO1 ASNDDATA,BODMCB,38,((R3),TRNNARR)                                
                                                                                
         USING SPAELD,R2                                                        
SPTA10   CLI   SPAEL,0                                                          
         BE    SPTAEX                                                           
         CLI   SPAEL,SPAELQ                                                     
         BE    *+14                                                             
SPTA12   IC    R0,SPALN                                                         
         AR    R2,R0                                                            
         B     SPTA10                                                           
                                                                                
         CLI   SPATYPE,SPATW2PA                                                 
         BNE   SPTA20                                                           
         GOTO1 ASNDDATA,BODMCB,36,(L'SPAAACT,SPAAACT)                           
         B     SPTA12                                                           
                                                                                
SPTA20   CLI   SPATYPE,SPATW2DA                                                 
         BNE   SPTA12                                                           
         GOTO1 ASNDDATA,BODMCB,35,(L'SPAAACT-1,SPAAACT+1)  SKIP OFFICE          
         B     SPTA12                                                           
                                                                                
SPTAEX   MVC   IOKEY,KEYSAVE       RESTORE READ SEQUENCE                        
         GOTO1 AIO,IO1+IOACCDIR+IOREAD                                          
         BE    EXIT                                                             
         DC    H'0'                                                             
         DROP  R2                                                               
***********************************************************************         
* SEND PQ ID AND LIVE BILL NUMBER                                     *         
***********************************************************************         
         SPACE 1                                                                
SNDDET   DS    0H                                                               
         L     R7,AREP                                                          
         USING REPD,R7                                                          
         GOTO1 ASNDHDR,BODMCB,13                                                
         MVC   THISPQID,REPSUBID                                                
         MVI   THISPQID+L'REPSUBID,C','                                         
         LA    RF,THISPQID+L'REPSUBID+1                                         
         EDIT  (2,REPREPNO),(5,(RF)),ALIGN=LEFT                                 
         GOTO1 ASNDDATA,BODMCB,43,THISPQID                                      
         B     EXITY                                                            
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* MAP TABLE                                                           *         
***********************************************************************         
         SPACE 1                                                                
MAPTAB   DS    0X                                                               
         SPACE 1                                                                
*                                  ** TRANSACTION LIST **                       
JBEL     DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#TRL)         ELEMENT CODE                                 
         DC    AL2(JBELX+1-JBEL)   DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDTRL-CLB55)   SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(255)            MAPPING CODE                                 
         DC    CL5'JOB  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKACT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVJOB-CLB55)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'DA'             TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKDA)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVDA-CLB55)    RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'WC'             TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKWORK)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'CONTR'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'DATE'           TEXT IDENTIFIER                              
         DC    AL1(MDTDTQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKDATE)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE                                 
         DC    CL5'REF'            TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKREF)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(06)             MAPPING CODE                                 
         DC    CL5'AMNT'           TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNAMNT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(10)             MAPPING CODE                                 
         DC    CL5'NETAL'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PM$ANVBL)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(11)             MAPPING CODE                                 
         DC    CL5'NETAF'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PM$FNVBL)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(MCALLOCQ)       MAPPING CODE                                 
         DC    CL5'ALLOC'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PP$AALLO)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVAXW-CLB55)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(13)             MAPPING CODE                                 
         DC    CL5'ALLOF'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PP$FALLO)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(MCWOQ)          MAPPING CODE                                 
         DC    CL5'WO'             TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PP$AWOFF)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVAXW-CLB55)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(15)             MAPPING CODE                                 
         DC    CL5'WOF'            TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PP$FWOFF)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(MCXFRQ)         MAPPING CODE                                 
         DC    CL5'XFR'            TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PP$AXFER)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVAXW-CLB55)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(17)             MAPPING CODE                                 
         DC    CL5'XFRF'           TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PP$FXFER)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(20)             MAPPING CODE                                 
         DC    CL5'UPWO'           TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PTANET)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(21)             MAPPING CODE                                 
         DC    CL5'UPWOF'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PTANETF)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(22)             MAPPING CODE                                 
         DC    CL5'PEWB'           TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(24)             MAPPING CODE                                 
         DC    CL5'AVHRS'          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PM$HRVBL)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(MCALLHRQ)       MAPPING CODE                                 
         DC    CL5'ALHRS'          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PP$HRSB)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVAXW-CLB55)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(MCXFRHRQ)       MAPPING CODE                                 
         DC    CL5'XFRHR'          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PP$HRSX)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVAXW-CLB55)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(MCWOFHRQ)       MAPPING CODE                                 
         DC    CL5'WOFHR'          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PP$HRSW)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVAXW-CLB55)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(MCTOJOBQ)       MAPPING CODE                                 
         DC    CL5'TOJOB'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVXFRTO-CLB55) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(31)             MAPPING CODE                                 
         DC    CL5'TOWRK'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVXFRTO-CLB55)  RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(MCWOREFQ)       MAPPING CODE                                 
         DC    CL5'WOREF'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PTAWREF)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVWOREF-CLB55) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(33)             MAPPING CODE                                 
         DC    CL5'WODAT'          TEXT IDENTIFIER                              
         DC    AL1(MDTDTQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PTAWDAT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVWODAT-CLB55) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(34)             MAPPING CODE                                 
         DC    CL5'WOACC'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVWOACC-CLB55) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(MCWODEPQ)       MAPPING CODE                                 
         DC    CL5'WODEP'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVWODEP-CLB55) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(MCWOSTFQ)       MAPPING CODE                                 
         DC    CL5'WOSTF'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVWOSTF-CLB55) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(37)             MAPPING CODE                                 
         DC    CL5'WOANL'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PTAWWACT)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(MCWONARQ)       MAPPING CODE                                 
         DC    CL5'WONAR'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVWONAR-CLB55) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(39)             MAPPING CODE                                 
         DC    CL5'STAT '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(40)             MAPPING CODE                                 
         DC    CL5'DUACT'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVDRUP-CLB55)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(41)             MAPPING CODE                                 
         DC    CL5'BTREF'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(4)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVBREF-CLB55)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(42)             MAPPING CODE                                 
         DC    CL5'BTMON'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(12)             DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVBMON-CLB55)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(43)             MAPPING CODE                                 
         DC    CL5'PQID'           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(9)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(44)             MAPPING CODE                                 
         DC    CL5'COMAA'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PM$ACVBL)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(45)             MAPPING CODE                                 
         DC    CL5'COMAF'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PM$FCVBL)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(46)             MAPPING CODE                                 
         DC    CL5'CRATE'          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'GOAGYCOM)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(47)             MAPPING CODE                                 
         DC    CL5'COMPA'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PP$ACOMM)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVALCOM-CLB55) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(48)             MAPPING CODE                                 
         DC    CL5'COMPF'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PP$FCOMM)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(49)             MAPPING CODE                                 
         DC    CL5'EXCHR'          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'CSEXCRAT)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVRATE-CLB55)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(50)             MAPPING CODE                                 
         DC    CL5'CRNCY'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'CSBILCUR)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVCUR-CLB55)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(51)             MAPPING CODE                                 
         DC    CL5'WBACT'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVWBACT-CLB55) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(52)             MAPPING CODE                                 
         DC    CL5'NARR '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVNARR-CLB55)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
JBELX    DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
MAPTABX  DC    X'00'               E-O-T                                        
*                                  MAP CODE EQUATES                             
BREFH    DC    XL1'0C'             DUMMY FLD HEADER                             
         DC    XL7'00'                                                          
BREF     DC    CL4' '              BATCH REF                                    
*                                                                               
BMONH    DC    XL1'0E'             DUMMY FLD HEADER                             
         DC    XL7'00'                                                          
BMON     DC    CL6' '              BATCH  MONTH                                 
         DC    X'00'                                                            
*                                                                               
WORK     EQU   BOWORK1                                                          
DUB      EQU   BODUB1                                                           
                                                                                
MCALLOCQ EQU   12                  PENDING ALLOCATION AMOUNT                    
MCWOQ    EQU   14                  PENDING WRITE OFF AMOUNT                     
MCXFRQ   EQU   16                  PENDING TRANSFER AMOUNT                      
MCALLHRQ EQU   25                  PENDING ALLOCATION - HOURS                   
MCXFRHRQ EQU   26                  PENDING TRANSFER - HOURS                     
MCWOFHRQ EQU   27                  PENDING WRITE OFF - HOURS                    
MCTOJOBQ EQU   30                  TRANSFER TO JOB                              
MCWOREFQ EQU   32                  WRITE OFF REFERENCE                          
MCWODEPQ EQU   35                  WRITE OFF DEPARTMENT                         
MCWOSTFQ EQU   36                  WRITE OFF STAFF                              
MCWONARQ EQU   38                  WRITE OFF NARRATIVE                          
         EJECT                                                                  
* ACCLBWORK                                                                     
*        PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
*        PRINT ON                                                               
         SPACE 1                                                                
***********************************************************************         
* SAVED WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   OSVALS                                                           
         DS    (L'OSVALS-(*-OSVALS))X                                           
         EJECT                                                                  
* ACCLBLINK                                                                     
       ++INCLUDE ACCLBLINK                                                      
         SPACE 1                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
LINKD    DSECT                                                                  
         ORG   OVRWS                                                            
THISJOB  DS    CL(L'TRNKACT)       CURRENT JOB                                  
KEYSAVE  DS    XL(L'IOKEY)                                                      
TRANDA   DS    XL(L'TRNKDA)        CURRENT TRANSACTION DISK ADDR                
WODATE   DS    PL(L'PTAWDAT)       WRITE OFF DATE                               
WOREF    DS    CL(L'PTAWREF)       WRITE OFF REFERENCE IF NOT DEFAULT           
WOACC    DS    CL(L'PTAWEXPA)      WRITE OFF ACCOUNT                            
WODEP    DS    CL(L'SPAAACT)       WRITE OFF DEPARTMENT                         
WOSTF    DS    CL(L'SPAAACT)       WRITE OFF STAFF                              
WONAR    DS    CL(L'TRNNARR)       WRITE OFF NARRATIVE                          
WBACT    DS    XL1                 W/B ACTION, 1=CLEAR PENDING W/B              
ALLOCCOM DS    PL(L'PP$ACOMM)      ALLOCATED COMMISSION                         
WONARLEN DS    XL1                 WO NARRATIVE LENGTH                          
THISACT  DS    XL1                                                              
THISPQID DS    CL9                 PRINT QUEUE ID                               
RATE     DS    XL(L'CSEXCRAT)                                                   
CURR     DS    CL(L'CSBILCUR)                                                   
*                                                                               
UWPBPARM DS    XL(PBPARMSL)                                                     
*                                                                               
         DS    (L'OVRWS-(*-OVRWS))X                                             
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'089ACCLB55   08/16/00'                                      
         END                                                                    
