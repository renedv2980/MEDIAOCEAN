*          DATA SET ACMRK41    AT LEVEL 095 AS OF 03/18/15                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 045076.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE T61641A                                                                  
ACMRK41 TITLE 'ROOT VALIDATION ROUTINES'                                        
* JFOX 077 OVERLAY CREATED FROM "ROUT" NMOD IN ROOT                             
ACMRK41  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MK41**,RA,R9,R8                                              
         L     RC,4(RD)                                                         
         L     RC,68(RC)                                                        
         USING WORKD,RC                                                         
         USING TWAD,R6                                                          
         USING SAVED,R7                                                         
         L     R5,AIOCNTL                                                       
         USING IOCNTLD,R5                                                       
         SRL   RF,32-8             BRANCH INDEX HELD IN HOB RF                  
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     GETACC              READ ACCOUNT                                 
         B     GETLDG              READ LEDGER/PROCESS LEDGER TABLE             
         B     IOEXEC              IO EXECUTIVE                                 
         B     OVRSCR              OVERLAY SCREEN                               
         B     PROFILE             EXTRACT TYPE/ACTION PROFILE                  
         B     PRTCLO              CLOSE REPORT, ETC.                           
         B     PRTINI              INITIALISE REPORT, PRINT FRONT PAGE          
         B     REPTRN              REPORT TRANSACTION POSTING                   
         B     REPINI              INITIALISE TRANSACTION REPORTING             
         B     TCLOSE              TEST CLOSED TRANSACTION                      
         B     TSARADD             ADD A TSAR RECORD                            
         B     TSARGET             GET A TSAR RECORD                            
         B     XMTSCR              TRANSMIT ALL FIELDS ON SCREEN                
                                                                                
ROUTL    MVI   DUB,0               SET CC LOW                                   
         B     ROUTCC                                                           
ROUTH    MVI   DUB,2               SET CC HIGH                                  
         B     ROUTCC                                                           
ROUTE    MVI   DUB,1               SET CC EQUAL                                 
ROUTCC   CLI   DUB,1                                                            
                                                                                
ROUTX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ESTABLISH LEDGER AND ACCOUNT VALUES FOR AN ACCOUNT AND   *         
* TEST FOR SECURITY.                                                  *         
* NTRY - KEY CONTAINS ACCOUNT RECORD KEY                              *         
*        R1=A(LIST OF VALID LEDGERS), OR                              *         
*        R1=0                                                         *         
*        GETIND=0, OR                                                 *         
*        GETIABLQ BIT ON - ACCOUNT BALANCE ELEMENT REQUIRED           *         
*        GETICCBQ BIT ON - GIVE ERROR FOR CLIENT BILLING JOB          *         
***********************************************************************         
                                                                                
GETACC   LTR   R1,R1               TEST A(LEDGER LIST PASSED)                   
         BZ    GETACC06                                                         
         MVC   FVXTRA(L'LEDGTUL),KEY+(ACTKUNT-ACTKEY)                           
         ICM   RF,3,0(R1)          NO DISPLACEMENT MUST BE BAD                  
         BZ    GETACCIL                                                         
         A     RF,OVERLAY          RF=A(VALID UNIT/LEDGER TABLE)                
         LR    R0,RF               R0=TABLE SAVE ADDRESS                        
         XR    RE,RE               RE=NUMBER OF ENTRIES IN TABLE                
GETACC02 CLI   0(RF),EOT           TEST E-O-T                                   
         BE    GETACC04                                                         
         LA    RE,1(RE)                                                         
         CLC   0(L'LEDGTUL,RF),KEY+(ACTKUNT-ACTKEY)                             
         BE    GETACC06                                                         
         LA    RF,L'LEDGTUL(RF)                                                 
         B     GETACC02                                                         
                                                                                
GETACC04 CH    RE,=H'1'            TEST LIST CONTAINS ONE ENTRY                 
         BNE   GETACCIL                                                         
         LR    RF,R0               YES - USE LIST UNIT/LEDGER                   
         MVC   WORK(L'ACTKACT),KEY+(ACTKUNT-ACTKEY)                             
         MVC   KEY+(ACTKUNT-ACTKEY)(L'LEDGTUL),0(RF)                            
         MVC   KEY+(ACTKACT-ACTKEY)(L'ACTKACT),WORK                             
                                                                                
GETACC06 GOTO1 AGETLDG,0           ESTABLISH THE LEDGER                         
         BNE   ROUTH                                                            
         ICM   R2,15,RECALDGT                                                   
         USING LEDGTABD,R2         R2=A(LEDGER TABLE ENTRY)                     
*&&US*&& TM    BATCHSEC,CPYBSSEC   TEST LEDGER SECURITY OVERRIDE                
*&&US*&& BZ    *+14                                                             
         CLC   TWAAUTH+1(1),LEDGTSEC                                            
         BL    GETACCSL                                                         
         MVC   FVXTRA(L'ACTKCULA-1),KEY+(ACTKUNT-ACTKEY)                        
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BNE   GETACCIA                                                         
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AACCELS                                                          
         TM    GETIND,GETIABLQ     TEST ACCOUNT BALANCE ELEMENT REQ'D           
         BZ    GETACC08                                                         
         OC    RECABLEL,RECABLEL   TEST BALANCE ELEMENT PRESENT                 
         BZ    GETACCNL            NOT A LOW LEVEL ACCOUNT                      
                                                                                
         USING RSTELD,R1                                                        
GETACC08 ICM   R1,15,RECRSTEL      R1=A(RECORD STATUS ELEMENT)                  
         BNZ   *+6                                                              
         DC    H'0'                NO RECORD STATUS ELEMENT                     
         TM    GETIND,GETINLOK     IS LOCKED ACCOUNT TEST REQUIRED?             
         BZ    *+12                                                             
         TM    RSTSTAT1,RSTSACIL   TEST ACCOUNT IS LOCKED                       
         BO    GETACCLK                                                         
         TM    GETIND,GETINCLO     IS CLOSED ACCOUNT TEST REQUIRED?             
         BZ    *+12                                                             
         TM    RSTSTAT1,RSTSACIC   TEST ACCOUNT IS CLOSED                       
         BO    GETACCCL                                                         
         L     R4,AIOBUFF          R4=A(LOW LEVEL ACCOUNT RECORD)               
         MVC   RECOFFC,SPACES      RECORD OFFICE CODE                           
         CLC   PRODUL,ACTKUNT-ACTRECD(R4)  TEST PRODUCTION                      
         BNE   GETACC10            NO - ALWAYS CALL OFFAL                       
         TM    GETIND,GETIABLQ     YES - TEST LOW LEVEL ACCOUNT                 
         BNO   GETACC36            NO - DON'T CALL OFFAL YET                    
                                                                                
         PUSH  USING                                                            
         LA    R4,IO2Q             CALL GETOPT FOR BILLING TYPE                 
         SRL   R4,6                USE IO2 AREA FOR GOBLOCK                     
         BCTR  R4,0                                                             
         MH    R4,=Y(IOALQ)                                                     
         A     R4,AIOAS                                                         
         USING GOBLOCKD,R4                                                      
         XC    GOBLOCK(GOPTIONS-GOBLOCK),GOBLOCK                                
         LA    RE,IO3Q             USE IO3 FOR GETOPT BUFFER                    
         SRL   RE,6                                                             
         BCTR  RE,0                                                             
         MH    RE,=Y(IOALQ)                                                     
         A     RE,AIOAS                                                         
         ST    RE,GOABUFF                                                       
         MVC   GOLBUFF,=A(L'IOBUFF)                                             
         MVC   GOADM,VDATAMGR                                                   
         MVI   GOWHICH,GOWHALL                                                  
         MVI   GOSELLEV,GOSLVALL                                                
         MVC   GOCTRY,AGYCTRY                                                   
         MVC   GOAKEY,AIOBUFF      A(RE-READ KEY)                               
         L     RF,AIOBUFF          RF=A(CUL)                                    
         MVC   GOSELCUL,0(RF)                                                   
         LA    RF,L'GOSELCUL(RF)   RF=A(CLIENT CODE)                            
         XR    R1,R1                                                            
         IC    R1,LEDGTLVA         R1=L'CLIENT CODE                             
         BCTR  R1,0                                                             
         MVC   GOSELCLI,SPACES                                                  
         EX    R1,*+4                                                           
         MVC   GOSELCLI(0),0(RF)                                                
         LA    R1,1(R1)                                                         
         LA    RF,0(R1,RF)         RF=A(PRODUCT CODE)                           
         XR    RE,RE                                                            
         IC    RE,LEDGTLVB         RE=L'CLI/PRO                                 
         SR    RE,R1               RE=L'PRODUCT CODE                            
         BCTR  RE,0                                                             
         MVC   GOSELPRO,SPACES                                                  
         EX    RE,*+4                                                           
         MVC   GOSELPRO(0),0(RF)                                                
         LA    RE,1(RE)                                                         
         LA    RF,0(RE,RF)         RF=A(JOB CODE)                               
         IC    R1,LEDGTLVC         R1=L'CLI/PRO/JOB                             
         IC    RE,LEDGTLVB         RE=L'CLI/PRO                                 
         SR    R1,RE               R1=L'JOB CODE                                
         BCTR  R1,0                                                             
         MVC   GOSELJOB,SPACES                                                  
         EX    R1,*+4                                                           
         MVC   GOSELJOB(0),0(RF)                                                
         MVI   GOANYWC,C'N'                                                     
         GOTO1 VGETOPT,DMCB,GOBLOCK                                             
*        MVC   SBILTYP,GOBILTYP    SAVE BILLING TYPE CODE                       
         MVC   WORK(1),GOBILTYP    SAVE BILLING TYPE CODE                       
         L     R4,AIOBUFF                                                       
         POP   USING                                                            
                                                                                
         LA    R3,LEDGTLVC         R3=A(L'JOB LEVEL)                            
         LA    R0,3                R0=3 LEVELS - CLIENT/PRODUCT/JOB             
         B     GETACC12                                                         
                                                                                
GETACC10 CLI   LEDGTOFF,LDGOFLT1   TEST OFFICE IN ACCOUNT FILTER 1-5            
         BL    GETACC34            NO - CALL OFFAL NOW                          
         CLI   LEDGTLVA,L'ACTKACT  TEST ONE LEVEL LEDGER                        
         BE    GETACC34            YES - CALL OFFAL NOW                         
         LA    R3,LEDGTLVD         R3=A(L'LOWEST POSSIBLE LEVEL)                
         LA    R0,LEDGTLVD+1-LEDGTLVA  R0=MAX NUMBER OF POSSIBLE LEVELS         
         CLI   0(R3),0             TEST ANYTHING AT THIS LEVEL                  
         BNE   GETACC12            YES - LOWEST LEVEL ESTABLISHED               
         BCTR  R3,0                UP ONE LEVEL                                 
         BCT   R0,*-10             TRY AGAIN                                    
         DC    H'0'                A LEDGER WITHOUT LEVELS                      
                                                                                
GETACC12 MVC   KEY,SPACES          BUILD KEY FOR ACCOUNT READ TO IO2            
         IC    R1,0(R3)            TAKE L'CURRENT LEVEL                         
         LA    R1,2(R1)            ADD CUL (-1 FOR EXECUTE)                     
         EX    R1,*+4                                                           
         MVC   KEY(0),0(R4)        TAKE PART OF KEY FROM LOW LEVEL A/C          
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO2Q                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO2Q                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOBUFF                                                       
         LA    R1,ACCRFST-ACCRECD(R1)                                           
         XR    RF,RF                                                            
         CLC   PRODUL,ACTKUNT-ACTRECD(R4)  TEST PRODUCTION                      
         BE    GETACC20                                                         
                                                                                
         USING RSTELD,R1                                                        
GETACC14 CLI   RSTEL,0             TEST E-O-R                                   
         BNE   *+6                                                              
         DC    H'0'                DIE IF NO STATUS ELEMENT                     
         CLI   RSTEL,RSTELQ        TEST STATUS ELEMENT                          
         BE    GETACC16                                                         
         IC    RF,RSTLN                                                         
         AR    R1,RF                                                            
         B     GETACC14                                                         
GETACC16 MVC   WORK(1),LEDGTOFF    EXTRACT FILTER POSITION FOR OFFICE           
         NI    WORK,X'07'          X'F1'...X'F5' -> X'01'...X'05'               
         XR    RE,RE                                                            
         IC    RE,WORK                                                          
         IC    RE,FILDISP-1(RE)    TAKE DISPLACMENT TO FILTER                   
         LA    RE,RSTELD(RE)       DISPLACE INTO ELEMENT                        
         CLI   0(RE),C' '          TEST OFFICE PRESENT AT THIS LEVEL            
         BNH   GETACC18                                                         
         MVC   RECOFFC(1),0(RE)    SET OFFICE                                   
         B     GETACC30            AND CALL OFFAL                               
GETACC18 BCTR  R3,0                NO - UP ONE LEVEL                            
         BCT   R0,GETACC12         AND TRY AGAIN                                
         B     GETACC32            NO OFFICE FOUND AT ANY LEVEL                 
                                                                                
         USING PPRELD,R1                                                        
GETACC20 CLI   PPREL,0             TEST E-O-R                                   
         BE    GETACC26                                                         
         CLI   PPREL,PPRELQ        TEST PRODUCTION PROFILE ELEMENT              
         BE    GETACC22            YES - EXTRACT OFFICE                         
         IC    RF,PPRLN                                                         
         AR    R1,RF                                                            
         B     GETACC20                                                         
GETACC22 CLC   RECOFFC,SPACES      TEST OFFICE SET AT LOWER LEVEL               
         BH    GETACC26            SET PRODUCTION OFFICE IN OFFBLK              
         CLC   PPRUFORA,SPACES     TEST OLD UNIT FOR ANALYSIS                   
         BNH   *+10                                                             
         MVC   RECOFFC(L'PPRUFORA),PPRUFORA  EXTRACT IT                         
         CLC   PPRGAOFF,SPACES     TEST NEW OFFICE CODE                         
         BNH   *+10                                                             
         MVC   RECOFFC,PPRGAOFF    EXTRACT IT                                   
GETACC26 BCTR  R3,0                READ NEXT LEVEL UP                           
         BCT   R0,GETACC12         AND TRY AGAIN                                
                                                                                
         TM    GETIND,GETICCBQ     TEST CHECK FOR CLIENT BILLING TYPE           
         BZ    GETACC28                                                         
         CLI   WORK,PPRBCLIT       TEST CLIENT BILLING TYPE                     
         BE    *+12                                                             
         CLI   WORK,C' '           TEST ANY OTHER KNOWN TYPE                    
         BH    GETACC28                                                         
         ST    R4,AIOBUFF          RESTORE AIOBUFF=A(IO1) - LOW LVL A/C         
         MVC   KEY,0(R4)           RE-READ LOW LEVEL A/C FOR DMRSEQ             
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BE    GETACCCB            ERROR - CLIENT BILLING TYPE                  
         DC    H'0'                                                             
                                                                                
GETACC28 CLC   RECOFFC,SPACES      TEST OFFICE FOUND AT ANY LEVEL               
         BNH   GETACC32                                                         
                                                                                
         USING OFFALD,R1                                                        
GETACC30 L     R1,AOFFBLK          R1=A(OFFAL CONTROL BLOCK)                    
         MVC   OFFAOFFC,RECOFFC    SET OFFICE IN OFFBLK                         
         MVC   RECOFFC,SPACES      AND CLEAR THE WORK AREA                      
                                                                                
GETACC32 ST    R4,AIOBUFF          RESTORE AIOBUFF=A(IO1) - LOW LVL A/C         
         MVC   KEY,0(R4)           RE-READ LOW LEVEL A/C FOR DMRSEQ             
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BE    GETACC34                                                         
         DC    H'0'                                                             
                                                                                
GETACC34 DS    0H                                                               
*&&US*&& TM    BATCHSEC,CPYBSOFF   TEST OFFICE SECURITY OVERRIDE                
*&&US*&& BZ    GETACC36                                                         
         L     R1,AOFFBLK          R1=A(OFFAL CONTROL BLOCK)                    
         ST    R4,OFFAREC          SAVE A(ACCOUNT RECORD)                       
         MVC   OFFAOPOS,LEDGTOFF                                                
         MVC   OFFALDGL,LEDGTLVA                                                
         MVC   OFFALDGT,LEDGTCLO                                                
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL              CALL OFFAL FOR OFFICE/SECURITY CHECK         
         BNE   GETACCSL                                                         
         MVC   RECOFFC,OFFAOFFC    SET OFFICE CODE                              
                                                                                
GETACC36 DS    0H                                                               
                                                                                
GETACCX  MVC   FVXTRA,SPACES                                                    
         B     ROUTE                                                            
         DROP  R1,R2                                                            
                                                                                
GETACCIL MVC   FVMSGNO,=AL2(EALDGINV)                                           
         B     ROUTH                                                            
GETACCIA MVC   FVMSGNO,=AL2(EAACCINV)                                           
         B     ROUTH                                                            
GETACCSL MVC   FVMSGNO,=AL2(EASECLOC)                                           
         B     ROUTH                                                            
GETACCNL MVC   FVMSGNO,=AL2(EANOTLOW)                                           
         B     ROUTH                                                            
GETACCCB MVC   FVMSGNO,=AL2(EACLIBIL)                                           
         B     ROUTH                                                            
GETACCLK MVC   FVMSGNO,=AL2(EALOCKED)                                           
         B     ROUTH                                                            
GETACCCL MVC   FVMSGNO,=AL2(EACLOSED)                                           
         B     ROUTH                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET A LEDGER RECORD AND BUILD LEDGTAB (SEE LEDGTABD)     *         
* NTRY - R1=LOB - GLNOSECY BIT ON - DON'T TEST SECURITY               *         
*        KEY=KEY OF A LEDGER OR LOWER LEVEL RECORD                    *         
* EXIT - RECALDGT=A(LEDGER TABLE ENTRY)                               *         
***********************************************************************         
                                                                                
GETLDG   STC   R1,BYTE             SAVE CALLING PARAMETER LOB R1                
         LA    R0,LEDGMAXN         R0=MAXIMUM N'LEDGER TABLE ENTRIES            
         LA    R2,SAVED                                                         
         AH    R2,=Y(LEDGTAB-SAVED)                                             
         USING LEDGTABD,R2         R2=A(LEDGER TABLE)                           
GETLDG02 OC    LEDGTUL,LEDGTUL     TEST FREE SLOT                               
         BZ    GETLDG04                                                         
         CLC   LEDGTUL,KEY+(LDGKUNT-LDGKEY)                                     
         BE    GETLDGX                                                          
         LA    R2,LEDGTABL(R2)     BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,GETLDG02         DO FOR NUMBER OF ENTRIES                     
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
         B     GETLDGN                                                          
                                                                                
GETLDG04 MVC   KEYSAVE,KEY         SAVE CALLER'S KEY                            
         MVC   KEY,SPACES          TAKE LEDGER PORTION & READ                   
         MVC   KEY(LDGKEND),KEYSAVE                                             
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BNE   GETLDG06                                                         
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BE    *+6                                                              
         DC    H'0'                                                             
GETLDG06 MVC   KEY,KEYSAVE         RESTORE CALLER'S KEY                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
         B     GETLDGN                                                          
                                                                                
         L     R1,AIOBUFF          PROCESS LEDGER RECORD & BUILD ENTRY          
         MVC   LEDGTUL,LDGKUNT-LDGKEY(R1)                                       
         LA    R1,ACCRFST-ACCRECD(R1)                                           
         XR    R0,R0                                                            
GETLDG08 CLI   0(R1),0             TEST E-O-R                                   
         BE    GETLDGX                                                          
                                                                                
         USING LDGELD,R1                                                        
         CLI   LDGEL,LDGELQ        TEST LEDGER ELEMENT                          
         BNE   GETLDG10                                                         
*        MVC   LEDGTTYP,LDGTYPE                                                 
         MVC   LEDGTST2,LDGSTAT2                                                
         MVC   LEDGTLIK,LDGLIKE                                                 
         MVC   LEDGTOFF,LDGOPOS                                                 
*&&US                                                                           
         CLI   LEDGTOFF,LDGONONE   TEST OFFICE POSITION SET                     
         BNE   *+8                                                              
         MVI   LEDGTOFF,LDGOTRAN   DEFAULT TO TRANSACTION LEVEL                 
*&&                                                                             
         MVC   LEDGTCLO,LDGCLOS                                                 
         CLI   LDGLN,LDGLNQ        TEST SHORT ELEMENT                           
         BL    GETLDG22                                                         
         CLC   LDGCDSC,SPACES                                                   
         BNH   GETLDG22                                                         
         MVC   LEDGCDSC,LDGCDSC+(ACTKULA-ACTKCULA)                              
         B     GETLDG22                                                         
                                                                                
         USING OCNELD,R1                                                        
GETLDG10 CLI   OCNEL,OCNELQ        TEST OFFICE CHEQUE NO. ELEMENT               
         BNE   GETLDG12                                                         
         CLI   OCNLN,OCNLN2Q       TEST ELEMENT CARRIES DISCOUNT A/C            
         BL    GETLDG22                                                         
         CLC   OCNOFFID,TWAUSRID   TEST USER-ID MATCHES                         
         BNE   GETLDG22                                                         
         MVC   LEDGCDSC,OCNDISC                                                 
         B     GETLDG22                                                         
                                                                                
         USING APTELD,R1                                                        
GETLDG12 CLI   APTEL,APTELQ        TEST ACCOUNT POINTER ELEMENT                 
         BNE   GETLDG14                                                         
         TM    APTSTAT,APTSTAXL    TEST TAX ACCOUNT ON LEDGER                   
         BZ    GETLDG22                                                         
         MVC   LEDGDTAX,APTACC+(ACTKULA-ACTKCULA)  EXTRACT IT                   
         B     GETLDG22                                                         
                                                                                
         USING ACLELD,R1                                                        
GETLDG14 CLI   ACLEL,ACLELQ        TEST HIERARCHY ELEMENT                       
         BNE   GETLDG16                                                         
         MVC   LEDGTLVA,ACLVALS                                                 
         MVC   LEDGTLVB,ACLVALS+(L'ACLVALS*1)                                   
         MVC   LEDGTLVC,ACLVALS+(L'ACLVALS*2)                                   
         MVC   LEDGTLVD,ACLVALS+(L'ACLVALS*3)                                   
         B     GETLDG22                                                         
                                                                                
         USING NAMELD,R1                                                        
GETLDG16 CLI   NAMEL,NAMELQ        TEST NAME ELEMENT                            
         BNE   GETLDG18                                                         
         MVC   RECNAME,SPACES                                                   
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+8                                                           
         B     GETLDG22                                                         
         MVC   RECNAME(0),NAMEREC                                               
                                                                                
         USING RSTELD,R1                                                        
GETLDG18 CLI   RSTEL,RSTELQ        TEST STATUS ELEMENT                          
         BNE   GETLDG20                                                         
         MVC   LEDGTSEC,RSTSECY+1                                               
         TM    BYTE,GLNOSECY       TEST SECURITY REQUIRED                       
         BO    GETLDG22                                                         
*&&US*&& TM    BATCHSEC,CPYBSSEC   TEST LEDGER SECURITY OVERRIDE                
*&&US*&& BZ    GETLDG22                                                         
         CLC   TWAAUTH+1(1),LEDGTSEC  TEST SECURITY                             
         BNL   GETLDG22               OK                                        
         XC    LEDGTABD(LEDGTABL),LEDGTABD  CLEAR THE ENTRY                     
         MVC   FVMSGNO,=AL2(EASECLOC)                                           
         B     GETLDGN             INSUFFICIENT PRIVILEGE                       
                                                                                
         USING SPAELD,R1                                                        
GETLDG20 CLI   SPAEL,SPAELQ        TEST SPECIAL POSTING ACCOUNT ELEMENT         
         BNE   GETLDG22                                                         
         CLI   SPATYPE,SPATEXDF                                                 
         BNE   GETLDG22                                                         
         MVC   LEDGEXDF,SPAAULA                                                 
                                                                                
GETLDG22 IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     GETLDG08                                                         
                                                                                
GETLDGX  STCM  R2,15,RECALDGT      SET A(LEDGER TABLE ENTRY)                    
         MVC   RECCDSC,LEDGCDSC                                                 
         MVC   RECDTAX,LEDGDTAX                                                 
         MVC   RECEXDF,LEDGEXDF                                                 
         MVC   FVXTRA,SPACES                                                    
         B     ROUTE                                                            
                                                                                
GETLDGN  MVC   FVXTRA(L'LEDGTUL),KEY+(ACTKUNT-ACTKEY)                           
         B     ROUTH                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO READ ACCOUNT FILES                                      *         
***********************************************************************         
                                                                                
IOEXEC   ST    R1,IOCTRL                                                        
         TM    TWAMODE,TWAMHDRS    TEST WE'RE AT MAIN FILE READ                 
         BNZ   IOEXEC02                                                         
         GOTO1 VGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         XR    RE,RE                                                            
         ICM   RE,3,MRKMAXIO       MARKER MAX IOS PER TRANSACTION               
         XR    RF,RF                                                            
         ICM   RF,3,FATIOCNT       ACTUAL IOS THIS TRANSACTION                  
         CR    RF,RE               TEST WE'RE BELOW MAXIMUM                     
         BL    IOEXEC02            OK TO CONTINUE                               
         DROP  R1                                                               
*                                  SET ERROR AND CURSOR                         
         MVC   FVMSGNO,=AL2(EATRNMAX)                                           
         LA    R0,1                ASSUME FIRST UNPROTECTED FIELD               
         TM    XTYPINDS,TYPIC2ND                                                
         BZ    *+8                                                              
         LA    R0,2                SECOND UNPROTECTED FIELD                     
         TM    XTYPINDS,TYPIC3RD                                                
         BZ    *+8                                                              
         LA    R0,3                THIRD UNPROTECTED FIELD                      
         LA    R1,MRKOLAYH                                                      
         XR    RF,RF                                                            
         IC    RF,FVTLEN-FVIHDR(R1)                                             
         AR    R1,RF                                                            
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BO    *-10                                                             
         BCT   R0,*-14                                                          
         ST    R1,FVADDR                                                        
         NI    TWAMODE,TWAMRSRV    SET TO RE-INITIALISE                         
         L     RD,SAVERD           RESTORE ROOT RD BEFORE OLAY CALL             
         L     RD,8(RD)            GO TO REGS AT ENTRY TO OLAY                  
         XIT1  ,                                                                
                                                                                
IOEXEC02 MVI   IOERROR,0           CLEAR ERROR BYTE                             
         MVI   IOQUAL,0            ESTABLISH IO QUALIFIERS                      
         TM    IOCTCOMM,IOLOCK     TEST READ FOR UPDATE                         
         BZ    *+8                                                              
         OI    IOQUAL,IOQLOCK                                                   
         TM    IOCTCOMM,IORDEL     TEST READ DELETES                            
         BZ    *+8                                                              
         OI    IOQUAL,IOQDELT                                                   
                                                                                
         LA    R1,IO1Q+IO2Q        TEST IOAREA NUMBER PASSED                    
         N     R1,IOCTRL           SET IOAREA IN R1                             
         BNZ   *+6                                                              
         DC    H'0'                IOAREA NUMBER MISSING                        
         SRL   R1,6                ESTABLISH IO AREA ADDRESSES                  
         CLM   R1,1,=AL1(IOAMAX)                                                
         BNH   *+6                                                              
         DC    H'0'                INVALID IO AREA                              
         BCTR  R1,0                                                             
         MH    R1,=Y(IOALQ)                                                     
         L     R0,AIOAS            R0=A(IOAS)                                   
         AR    R1,R0                                                            
         ST    R1,AIOSAVE          SAVE A(SAVED D/A IODA/IOWORK)                
         LA    R1,L'IOSAVE(R1)                                                  
         ST    R1,AIOBUFF          SAVE A(DATA RECORD AREA)                     
         LA    R1,L'IOBUFF(R1)                                                  
         ST    R1,AIOBUFFA         SAVE A(BUFFER#2)                             
                                                                                
         NI    IOCTCOMM,IOCMNDS    LEAVE COMMAND ONLY INTACT                    
                                                                                
         LA    R1,IOFILES          ESTABLISH FILE                               
         N     R1,IOCTRL                                                        
         BNZ   *+6                                                              
         DC    H'0'                FILE NUMBER MISSING                          
         SRL   R1,8                                                             
         BCTR  R1,0                                                             
         MH    R1,=Y(L'ACCDIR)                                                  
         LA    R1,ACCDIR(R1)                                                    
         MVC   IOFILE,0(R1)        SET FILE NAME                                
                                                                                
         LA    R1,IOCMNDS          ESTABLISH DATAMGR COMMAND                    
         N     R1,IOCTRL                                                        
         BNZ   *+6                                                              
         DC    H'0'                COMMAND NUMBER MISSING                       
         BCTR  R1,0                                                             
         SLL   R1,4                                                             
         LA    R0,IOCDA                                                         
         TM    IOCTFILE,IOACCDA    TEST D/A IO (TO ACCMST/ACCARC)               
         BO    *+8                                                              
         LA    R0,IOCIS                                                         
         AR    R1,R0                                                            
*        CLI   FILEFORM,VLISQ      TEST OLD FILE                                
*        BNE   *+12                                                             
*        LA    R1,L'ISCMNDS(R1)    OFFSET INTO COMMANDS FOR OLD FILE            
*        B     IOEXEC08                                                         
                                                                                
         MVC   IOCOMM,0(R1)        SET COMMAND NAME                             
         TM    IOCTFILE,IOACCDA    TEST D/A IO (TO ACCMST/ACCARC)               
         BNO   IOEXEC06                                                         
                                                                                
         ICM   R0,15,AIOBUFF       D/A DATAMGR IO                               
         BNZ   *+6                                                              
         DC    H'0'                A(IO BUFFER) MISSING                         
                                                                                
         OC    IODA,IODA           TEST DISK ADDRESS SET BY CALLER              
         BNZ   IOEXEC04                                                         
         L     R1,AIOSAVE                                                       
         MVC   IODA,0(R1)          SET DISK ADDRESS                             
         MVC   IOWORK,L'IODA(R1)   SET A(DMWORK)                                
                                                                                
IOEXEC04 GOTO1 VDATAMGR,DMCB,(IOQUAL,IOCOMM),IOFILE,IODA,(R0),IOWORK            
                                                                                
         MVC   IOERROR,8(R1)                                                    
         TM    IOERROR,IOEALL-IOEDEL                                            
         BNZ   IOEXECX                                                          
         L     R1,AIOSAVE                                                       
         MVC   0(L'IODA,R1),IODA                                                
         MVC   L'IODA(L'IOWORK,R1),IOWORK                                       
         B     IOEXECX                                                          
                                                                                
IOEXEC06 TM    IOCTCOMM,IOHIGH     TEST READ HIGH                               
         BO    *+12                                                             
         TM    IOCTCOMM,IOSEQ      TEST READ SEQUENTIAL                         
         BNO   *+10                                                             
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         GOTO1 VDATAMGR,DMCB,(IOQUAL,IOCOMM),IOFILE,KEY,KEY                     
                                                                                
         MVC   IOERROR,8(R1)                                                    
         TM    IOERROR,IOEALL-IOEDEL                                            
         BNZ   IOEXECX                                                          
         MVC   IODA,KEY+(ACCKDA-ACCRECD)                                        
         B     IOEXECX                                                          
*                                                                               
*                                  I/O CALLS - TRANSLATE TO OLD FILE            
IOEXEC08 MVC   IOFILE,ACCFIL       SET FILE NAME                                
*        MVC   IOCOMM,0(R1)        SET COMMAND NAME                             
*        MVC   IOCTINDS,L'IOCOMM(R1)                                            
*        OC    IOCOMM,IOCOMM       TEST COMMAND VALID FOR FILE                  
*        BZ    IOEXECX             NO COMMAND                                   
*        TM    IOCTINDS,IOCTIGET   TEST GETREC                                  
*        BZ    IOEXEC10                                                         
*        TM    IOQUAL,IOQLOCK      TEST GETREC FOR UPDATE                       
*        BZ    IOEXECX                                                          
*        LA    R1,KEY              RE-BUILD KEY FOR READ WITH LOCK              
*        USING TRNRECD,R1                                                       
*        MVC   TRNKCULA,ACCOUNT                                                 
*        MVC   TRNKOFF,SPACES                                                   
*        CLC   PRODUL,TRNKUNT      TEST PRODUCTION                              
*        BNE   *+10                                                             
*        MVC   TRNKOFF,TSAROFF     SET PRODUCTION WORKCODE                      
*        MVC   TRNKCULC,TSARCON                                                 
*        MVC   TRNKDATE,TSARDAT                                                 
*        MVC   TRNKREF,TSARREF                                                  
*        MVC   TRNKSBR,TSARSBR                                                  
*        DROP  R1                                                               
*                                                                               
IOEXEC10 TM    IOCTCOMM,IOHIGH     TEST READ HIGH                               
*        BO    *+12                                                             
*        TM    IOCTCOMM,IOSEQ      TEST READ SEQUENTIAL                         
*        BNO   *+10                                                             
*        MVC   KEYSAVE,KEY         SAVE KEY ON HIGH OR SEQ                      
*        TM    IOCTINDS,IOCTIGET   TEST GETREC                                  
*        BNZ   IOEXEC12                                                         
*        TM    IOCTFILE,IOACCDA    IF A WRITE COMMAND, TRANSLATE TO OLD         
*        BZ    IOEXEC12            FORMAT RECORD BEFORE DATAMGR CALL            
*        GOTO1 VACCEMU,DMCB,=C'NEWO',,,AIOBUFF                                  
*        ORG   *-2                                                              
*        LR    R2,R1               EMU REQUIRES R2=A(DMCB)                      
*        BASR  RE,RF                                                            
*                                                                               
IOEXEC12 GOTO1 VDATAMGR,DMCB,(IOQUAL,IOCOMM),IOFILE,KEY,AIOBUFF                 
*        MVC   IOERROR,8(R1)                                                    
*        TM    IOCTFILE,IOACCDA    IF A READ COMMAND, TRANSLATE TO NEW          
*        BZ    *+12                FORMAT RECORD AFTER DATAMGR CALL             
*        TM    IOCTINDS,IOCTIGET                                                
*        BZ    IOEXECX                                                          
*        GOTO1 VACCEMU,DMCB,=C'OLDN',,,AIOBUFF                                  
*        ORG   *-2                                                              
*        LR    R2,R1                                                            
*        BASR  RE,RF                                                            
*        USING TRNRECD,R2                                                       
*        L     R2,AIOBUFF          SET MOS IN KEY AREA FROM TRANS               
*        LA    RF,TRNRFST                                                       
*        CLI   0(RF),TRNELQ                                                     
*        BNE   IOEXEC14                                                         
*        GOTO1 VCONVMOS,DMCB,(X'FE',(RF)),TRNRSMOS                              
*                                                                               
IOEXEC14 TM    IOCTFILE,IOACCDA                                                 
*        BNZ   IOEXECX                                                          
*        LA    R1,KEY              BUILD NEW DIR RECORD KEY IN KEY AREA         
*        USING ACCRECD,R1                                                       
*        MVC   ACCKEY,TRNKEY                                                    
*        MVC   ACCKSTA,TRNRSTA                                                  
*        XC    ACCKDA,ACCKDA                                                    
*        DROP  R1,R2                                                            
*                                                                               
IOEXECX  TM    IOERROR,IOEALL                                                   
         BZ    ROUTE               NO ERROR - CC EQUAL                          
         TM    IOERROR,IOEEOF+IOERNF+IOEDEL                                     
         BNZ   ROUTH               LOGICAL ERROR - CC HIGH                      
         B     ROUTL               HARD ERROR - CC LOW                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OVERLAY HEADER OR DETAIL SCREEN                          *         
***********************************************************************         
                                                                                
OVRSCR   STC   R1,TWASCROV                                                      
         MVC   DMCB+4(3),=X'D90616'                                             
         MVC   DMCB+7(1),TWASCROV                                               
         GOTO1 VCALLOV,DMCB,(0,MRKOLAYH)                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                KILL IF CAN'T LOAD SCREEN                    
         LA    R1,MRKMSGH          RE-TRANSMIT ALL HEADER FIELDS                
         XR    RE,RE                                                            
         LA    RF,MRKOLAYH-1                                                    
         ICM   RE,1,0(R1)                                                       
         BZ    *+12                                                             
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BXLE  R1,RE,*-12                                                       
         CLC   TWASCROV,XACTSCR1   TEST HEADER SCREEN JUST LOADED               
         BNE   OVRSCRX                                                          
OVRSCRX  B     ROUTX                                                            
         EJECT                                                                  
***********************************************************************         
* READ AND VALIDATE TYPE/ACTION PROFILE PAGES 1 AND 2                 *         
***********************************************************************         
                                                                                
         USING PROFKEYD,R3         R3=A(PROFILE KEY)                            
PROFILE  XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         MVI   PROFKSYS,C'A'-X'40' LOWER CASE SYSTEM                            
         MVC   PROFKXAC,XACTALFA                                                
         MVI   PROFKPAG,C'1'       PAGE 1                                       
         MVC   PROFKULA,ACCOUNT+(ACTKUNT-ACTRECD)                               
         MVC   PROFKAGY,TWAAGY                                                  
         GOTO1 VGETPROF,DMCB,PROFKEYD,PROFILE1,VDATAMGR                         
         MVI   PROFKPAG,C'2'       PAGE 2                                       
         GOTO1 VGETPROF,DMCB,PROFKEYD,PROFILE2,VDATAMGR                         
                                                                                
         USING PROFDFTD,R3         R3=A(DEFAULT PROFILES TABLE)                 
         L     R3,APROFDFT                                                      
PROFIL02 CLI   PROFALFA,EOT                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   PROFALFA,XACTALFA   FIND TABLE ENTRY FOR ALPHA                   
         BE    PROFIL04                                                         
         LA    R3,PROFDFTL(R3)                                                  
         B     PROFIL02                                                         
                                                                                
PROFIL04 OC    PROFSEQ,PROFSEQ     TEST KEY SEQUENCE SET IN PROFILE             
         BNZ   PROFIL08                                                         
PROFIL06 MVC   PROFSEQ,PROFDFKS    TAKE DEFAULT SORT KEY FIELDS                 
                                                                                
PROFIL08 XC    KEYDISP(KEYDISPL),KEYDISP                                        
         MVI   WORK,0                                                           
         LA    R1,PROFSEQ          R1=A(KEY PROFILE)                            
         LA    R0,L'PROFSEQ                                                     
         LA    RE,1                RE=DISPLACEMENT TO KEY VALUE                 
PROFIL10 XR    R2,R2                                                            
         ICM   R2,1,0(R1)          R2=KEY ELEMENT NUMBER                        
         BZ    PROFIL12                                                         
         LR    RF,R2                                                            
         LA    RF,KEYDISP-1(RF)    RF=A(KEY ELEMENT DISPLACEMENT)               
         BCTR  R2,0                                                             
         SLL   R2,1                KEYNUM-1*2                                   
         A     R2,AKEYTAB                                                       
         USING KEYTABD,R2          R2=A(KEY ELEMENT TABLE ENTRY)                
         MVC   WORK+L'KEYMASK(L'KEYMASK),KEYMASK                                
         NC    WORK+L'KEYMASK(L'KEYMASK),WORK                                   
         BNZ   PROFIL06            KEY ELEM DUPLICATED - USE DEFAULTS           
         OC    WORK(L'KEYMASK),KEYMASK                                          
         STC   RE,0(RF)                                                         
         XR    RF,RF                                                            
         IC    RF,KEYLEN                                                        
         AR    RE,RF               RE=DISPLACEMENT OF NEXT ELEMENT              
         CLM   RE,1,=AL1(L'TSARKEY-L'TSARCNT+1)                                 
         BH    PROFIL06            KEY TOO LONG - USE DEFAULTS                  
PROFIL12 LA    R1,1(R1)                                                         
         BCT   R0,PROFIL10                                                      
                                                                                
PROFILEX B     ROUTX                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CLOSE REPORT AND GENERATE SPOOL-ID MESSAGE               *         
***********************************************************************         
                                                                                
         USING REPD,R2                                                          
         USING TOTTABD,R4                                                       
PRTCLO   L     R2,AREPWRK                                                       
         LA    R3,TOTALS                                                        
         L     R4,AACTTOT          R4=A(ACTION TOTALS)                          
PRTCLO2  CLI   TOTTABD,X'FF'       TEST EOT                                     
         BE    PRTCLO6                                                          
         XR    RF,RF                                                            
         ICM   RF,1,TOTPDIS        TEST PRINT LINE TOTAL                        
         BZ    PRTCLO4             NO - GET NEXT TOTAL                          
         LA    RF,REPP2-1(RF)                                                   
         MVC   LAREADDR,TOTSTXT    S(TOTAL TEXT IN WORKD)                       
         EX    0,LARE                                                           
         MVC   0(TOTTXTL,RF),0(RE)                                              
         LA    RF,L'REPPS(RF)                                                   
         CURED (P8,0(R3)),(TOTAMTL,(RF)),2,ALIGN=LEFT,FLOAT=-                   
PRTCLO4  LA    R3,L'TOTALS(R3)                                                  
         LA    R4,TOTTABL(R4)                                                   
         B     PRTCLO2                                                          
                                                                                
PRTCLO6  GOTO1 VREPORT,REPD        PRINT TOTAL LINE                             
         MVI   REPACTN,REPACLO     CLOSE THE REPORT                             
         GOTO1 VREPORT,REPD                                                     
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                CLOSE ERROR                                  
         LA    R1,MRKACTH                                                       
         ST    R1,FVADDR                                                        
         LA    RF,FVXTRA           SET UP XXX,999                               
         MVC   0(3,RF),REPSUBID                                                 
         MVI   3(RF),C','                                                       
         XR    R0,R0                                                            
         ICM   R0,3,REPREPNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  WORK(5),DUB                                                      
         LA    R1,4                                                             
         LA    RE,WORK                                                          
         CLI   0(RE),C'0'                                                       
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
         EX    R1,*+4                                                           
         MVC   4(0,RF),0(RE)                                                    
         CLI   XACTION,ACTDRFT     TEST REPORT WAS DRAFT                        
         BNE   ROUTX                                                            
         LA    R1,MRKSCRH          SET CURSOR TO SCROLL FIELD                   
         MVC   FVMSGNO,=AL2(IGREPSPL)  REPORT XXX,999 HAS BEEN SPOOLED          
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         B     ROUTX                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALISE REPORT AND PRINT FRONT PAGE                   *         
***********************************************************************         
                                                                                
PRTINI   OC    PRTSUB,PRTSUB       TEST REPORT REQUESTED                        
         BZ    PRTINIX                                                          
         L     R0,ATIA             BUILD HEADER SCREEN IN TIA                   
         LH    R1,=Y(MRKOLAYH-MRKMSGH)                                          
         LA    RE,MRKMSGH                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY UP TO MRKOLAYH FROM CURRENT TWA         
         L     R1,ATIA             CLEAR MESSAGE FIELD & SET ACTION             
         XC    L'FVIHDR(L'MRKMSG,R1),L'FVIHDR(R1)                               
         XC    MRKACT-MRKMSGH(L'MRKACT,R1),MRKACT-MRKMSGH(R1)                   
         MVC   MRKACT-MRKMSGH(L'SACTNAME,R1),SACTNAME                           
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,(1,0),APQBUFF                       
         L     R0,ATIA                                                          
         AH    R0,=Y(MRKOLAYH-MRKMSGH)                                          
         LA    R1,SAVHEADL                                                      
         L     RE,APQBUFF                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY HEADER SCREEN FROM TWA PAGE 1           
         L     R3,AREPWRK          INITIALISE REPD FOR PRINTING                 
         USING REPD,R3             R3=A(REPORT W/S)                             
         MVC   REPACOM,ACOM                                                     
         LA    R0,REPHS                                                         
         ST    R0,REPABUF                                                       
         MVI   REPACTN,REPAINI                                                  
         MVI   REPHEADN,REPHN                                                   
         MVI   REPMIDSN,REPMN                                                   
         MVI   REPPRNTN,REPPN                                                   
         MVI   REPFOOTN,REPFN                                                   
         MVI   REPWIDTH,REPWREGQ                                                
         MVC   REPDATE,TODAYB      SET ONLINE VALUES                            
         MVC   REPAPQB,APQBUFF                                                  
         MVC   REPSYSID,=C'AC'                                                  
         MVC   REPPRGID,=C'MK'                                                  
         MVC   REPSUBID,PRTSUB                                                  
         MVC   REPDESC(L'SACTNAME),SACTNAME                                     
         MVC   REPRLH,=AL2(48)     SET LIVE RETAIN=48 HOURS                     
         MVC   REPRDH,=AL2(12)     SET DEAD RETAIN=12 HOURS                     
         MVI   REPACTN,REPAINI                                                  
         GOTO1 VREPORT,REPD        CALL REPORT TO INITIALISE REPD               
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         BASR  RE,RF                                                            
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   REPACTN,REPAPUT                                                  
         L     R0,AIOAS            CLEAR SPACE FOR HEADER SCREEN FORMAT         
         LH    R1,=Y(TWACOLS*TWAROWS)                                           
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE                                                            
         L     R1,ATIA             FORMAT HEADER SCREEN INTO IOA1/IOA2          
         XR    RE,RE                                                            
         LA    RF,4095(R1)                                                      
PRTINI4  ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    PRTINI6                                                          
         XR    R0,R0                                                            
         ICM   R0,3,FVABSA-FVIHDR(R1)                                           
         L     R2,AIOAS                                                         
         AR    R2,R0                                                            
         SH    RE,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RE,=Y(L'FVIHDR)                                                  
         BCTR  RE,0                                                             
         TM    FVATRB-FVIHDR(R1),FVAZERO                                        
         BO    *+14                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),L'FVIHDR(R1)                                             
         ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BXLE  R1,RE,PRTINI4                                                    
                                                                                
PRTINI6  L     RF,VREPORT          INITIALISE HEADER PRINT LOOP                 
         LA    R1,REPD                                                          
         L     R2,AIOAS                                                         
         LA    R0,TWAROWS                                                       
         MVI   REPP1,C'*'          PRINT HEADER SCREEN ON FIRST PAGE            
         MVC   REPP1+1(TWACOLS+1),REPP1                                         
         BASR  RE,RF                                                            
                                                                                
PRTINI8  MVI   REPP1,C'*'                                                       
         MVC   REPP1+1(TWACOLS),0(R2)                                           
         MVI   REPP1+1+TWACOLS,C'*'                                             
         BASR  RE,RF                                                            
         LA    R2,TWACOLS(R2)                                                   
         BCT   R0,PRTINI8                                                       
         MVI   REPP1,C'*'                                                       
         MVC   REPP1+1(TWACOLS+1),REPP1                                         
         BASR  RE,RF                                                            
         L     R1,ATSARBLK         RESTORE DESTROYED TSAR BUFFER                
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARES                                                    
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
         L     R1,AREPSPEC                                                      
         STCM  R1,15,REPAPHS       SET A(SPEC POOL)                             
         OI    REPHEADI,REPHFRCE   FORCE HEADLINE PRINTING                      
         MVC   REPPAGE,=H'1'                                                    
         GOTO1 ABLDBAL,REPH4+1     ACCNAME & BAL AT REPH4+1(+L'FVIHDR)          
         MVC   REPH4(L'LC8ACC),LC8ACC  IN FRONT OF ACCOUNT NAME                 
         MVC   REPH5(L'DISHEAD),DISHEAD                                         
         MVC   REPH6(L'DISHEAD),DISHEAD+L'DISHEAD                               
         MVI   REPSUBPG,0          SUB-PROGRAM FOR LIVE UPDATE                  
         CLI   XACTION,ACTUPDT     TEST LIVE UPDATE                             
         BE    PRTINIX                                                          
         MVI   REPSUBPG,1          SUB-PROGRAM FOR DRAFT REPORT                 
PRTINIX  B     ROUTX                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* REPORT TRANSACTION POSTING                                          *         
* NTR1 -> R1=A(TRANSACTION TO BE POSTED)                              *         
***********************************************************************         
                                                                                
REPTRN   OC    PRTSUB,PRTSUB       TEST REPORT REQUESTED                        
         BZ    REPTRNX                                                          
         L     R2,0(R1)                                                         
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         L     R4,AREPWRK          INITIALISE REPD FOR PRINTING                 
         USING REPD,R4                                                          
         USING RPTPOST,REPP1                                                    
         MVI   RPTLINE,C' '                                                     
         MVC   RPTLINE+1(L'RPTLINE-1),RPTLINE                                   
         MVC   RPTULA,TRNKULA                                                   
         MVC   RPTULC,TRNKULC                                                   
         GOTO1 VDATCON,PARM,(1,TRNKDATE),(17,RPTDATE)                           
         MVC   RPTREF,TRNKREF                                                   
         CURED TRNAMNT,(L'RPTAMNT,RPTAMNT),2,FLOAT=-                            
         MVC   RPTCRDR,AC@CR                                                    
         TM    TRNSTAT,TRNSDR                                                   
         BZ    *+10                                                             
         MVC   RPTCRDR,AC@DR                                                    
         EDIT  TRNKSBR,(3,RPTSBR)                                               
         CLI   XACTOLAY,CBOLAY       IS IT CREDITOR BALANCE?                    
         BNE   *+10                                                             
         MVC   RPTOFFC,TRNOFFC                                                  
         XR    R2,R2                                                            
*&&UK                                                                           
         USING AFCELD,R3                                                        
REPTRN02 IC    R2,AFCLN                                                         
         AR    R3,R2                                                            
         CLI   AFCEL,0                                                          
         BE    REPTRN06                                                         
         CLI   AFCEL,AFCELQ                                                     
         BE    REPTRN04                                                         
         CLI   AFCEL,FFTELQ                                                     
         BNE   REPTRN02                                                         
         PUSH  USING                                                            
         USING FFTELD,R3                                                        
         CLI   FFTTYPE,FFTTACUR                                                 
         BNE   REPTRN02                                                         
         MVC   RPTAFCA(L'CURTCUR),FFTDATA                                       
         POP   USING                                                            
         B     REPTRN02                                                         
         USING CURTABD,RF                                                       
REPTRN04 L     RF,ACURRTAB                                                      
         LA    R0,CURRTABN                                                      
         CLC   CURTCUR,AFCCURR                                                  
         BE    *+12                                                             
         LA    RF,L'CURRTAB(RF)                                                 
         BCT   R0,*-14                                                          
         MVC   RPTAFCA,SPACES                                                   
         CURED AFCAMNT,(L'RPTAFCA,RPTAFCA),CURTABD,CURSYMB=Y,FLOAT=-            
         DROP  RF                                                               
*&&                                                                             
REPTRN06 GOTO1 VREPORT,REPD        PRINT IT                                     
REPTRNX  B     ROUTX                                                            
         DROP  R2,R3,R4                                                         
***********************************************************************         
* INITIALISE TRANSACTION REPORTING                                    *         
***********************************************************************         
                                                                                
REPINI   OC    PRTSUB,PRTSUB       TEST REPORT REQUESTED                        
         BZ    REPINIX                                                          
         L     R4,AREPWRK          INITIALISE REPD FOR PRINTING                 
         USING REPD,R4                                                          
         USING RPTPOST,REPP1                                                    
         MVC   RPTLINE,SPACES                                                   
         GOTO1 VREPORT,REPD        PRINT IT                                     
         MVC   RPTLINE,SPACES                                                   
         MVC   RPTLINE(L'LC@PSTGS),LC@PSTGS                                     
         GOTO1 VREPORT,REPD        PRINT IT                                     
         MVC   RPTLINE,SPACES                                                   
         MVC   RPTULA(L'LC8ACC),LC8ACC                                          
         MVC   RPTULC(L'LC@CTRA),LC@CTRA                                        
         MVC   RPTDATE(L'LC@DATE),LC@DATE                                       
         MVC   RPTREF(L'LC@REF),LC@REF                                          
         MVC   RPTAMNT,LC@AMTR+(L'LC@AMTR-L'RPTAMNT)                            
         MVC   RPTCRDR,SPACES                                                   
         MVC   RPTSBR(L'LC3SUBR),LC3SUBR                                        
         CLI   XACTOLAY,CBOLAY       IS IT CREDITOR BALANCE?                    
         BNE   *+10                                                             
         MVC   RPTOFFC(L'LC@OFF),LC@OFF                                         
         GOTO1 VREPORT,REPD        PRINT IT                                     
REPINIX  B     ROUTX                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL OFFAL TO TEST IF A TRANSACTION IS LOCKED-OUT/CLOSED *         
*                                                                     *         
* A LEDGER TABLE ENTRY MUST EXIST FOR THE TRANSACTION'S UNIT/LEDGER   *         
***********************************************************************         
                                                                                
TCLOSE   L     R1,AOFFBLK                                                       
         USING OFFALD,R1           R1=A(OFFAL CONTROL BLOCK)                    
         L     RE,AIOBUFF          A(TRANSACTION DATA RECORD)                   
         ST    RE,OFFAREC                                                       
         LA    R0,LEDGMAXN                                                      
         L     RF,ALEDGTAB         FIND LEDGER TABLE ENTRY                      
         USING LEDGTABD,RF                                                      
         CLC   LEDGTUL,TRNKUNT-TRNRECD(RE)                                      
         BE    *+14                                                             
         LA    RF,LEDGTABL(RF)                                                  
         BCT   R0,*-14                                                          
         DC    H'0'                LEDGER TABLE ENTRY MUST EXIST                
         MVC   OFFAOPOS,LEDGTOFF   SET OFFAL VALUES                             
         MVC   OFFALDGL,LEDGTLVA                                                
         MVC   OFFALDGT,LEDGTCLO                                                
         DROP  RF                                                               
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         BNE   ROUTH               OFFICE LOCKOUT                               
         TM    OFFASTAT,OFFASCLS                                                
         BNZ   ROUTH               TRANSACTION IS CLOSED                        
TCLOSEX  B     ROUTE                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT A RECORD TO TSAR                                     *         
***********************************************************************         
                                                                                
TSARADD  L     R2,ATSARBLK                                                      
         USING TSARD,R2                                                         
         MVI   TSACTN,TSAADD                                                    
         MVI   TSERRS,0                                                         
         XC    TSARKEY(L'TSARKEY-L'TSARCNT),TSARKEY                             
         XR    RF,RF                                                            
*                                  BUILD KEY USING SAVED DISPLACEMENTS          
         ICM   RF,1,KEYOFFD        TEST/SET OFFICE OR WORK CODE                 
         BZ    *+14                                                             
         LA    RE,TSARKEY-1(RF)                                                 
         MVC   0(L'TSAROFF,RE),TSAROFF                                          
                                                                                
         ICM   RF,1,KEYCOND        TEST/SET CONTRA                              
         BZ    *+14                                                             
         LA    RE,TSARKEY-1(RF)                                                 
         MVC   0(L'TSARCON,RE),TSARCON                                          
                                                                                
         ICM   RF,1,KEYDATD        TEST/SET DATE                                
         BZ    TSARAD4                                                          
         LA    RE,TSARKEY-1(RF)                                                 
         MVC   0(L'TSARDAT,RE),TSARDAT                                          
         CLI   XACTOLAY,CAOLAY     PRE-PAYMENT CREDITOR ACTIONS                 
         BE    TSARAD2                                                          
         CLI   XACTOLAY,CSOLAY                                                  
         BE    TSARAD2                                                          
         CLI   XACTOLAY,CDOLAY                                                  
         BE    TSARAD2                                                          
         CLI   XACTOLAY,CHOLAY                                                  
         BNE   TSARAD4                                                          
TSARAD2  CLI   PROFDUED,C'Y'       MAY SUBSTITUTE TSARFDUE FOR TSARDAT          
         BNE   TSARAD4                                                          
         XC    0(L'TSARDAT,RE),0(RE)  CLEAR TRANSACTION DATE                    
         MVC   0(L'TSARFDUE,RE),TSARFDUE  SUBSTITUTE DUE DATE                   
                                                                                
TSARAD4  ICM   RF,1,KEYREFD        TEST/SET REFERENCE                           
         BZ    *+14                                                             
         LA    RE,TSARKEY-1(RF)                                                 
         MVC   0(L'TSARREF,RE),TSARREF                                          
                                                                                
         ICM   RF,1,KEYSBRD        TEST/SET SUB-REFERENCE                       
         BZ    *+14                                                             
         LA    RE,TSARKEY-1(RF)                                                 
         MVC   0(L'TSARSBR,RE),TSARSBR                                          
                                                                                
         ICM   RF,1,KEYMOSD        TEST/SET MOS                                 
         BZ    *+14                                                             
         LA    RE,TSARKEY-1(RF)                                                 
         MVC   0(L'TSARMOS,RE),TSARMOS                                          
                                                                                
         ICM   RF,1,KEYBATD        TEST/SET BATCH REF                           
         BZ    TSARAD6                                                          
         LA    RE,TSARKEY-1(RF)                                                 
         MVC   0(L'TSARBAT,RE),TSARBAT                                          
         CLI   XACTION,ACTREVS     TEST GENERAL/REVERSE                         
         BNE   TSARAD6                                                          
         MVC   0(L'TSARBAT,RE),TSARAMNT  SUBSTITUTE AMOUNT                      
                                                                                
TSARAD6  ICM   RF,1,KEYVARD        TEST/SET VARIABLE BYTE (EG. STATUS)          
         BZ    *+14                                                             
         LA    RE,TSARKEY-1(RF)                                                 
         MVC   0(L'TSARVAR,RE),TSARVAR                                          
                                                                                
         ICM   RF,3,STSARCNT       SAVED TSAR RECORD NUMBER                     
         LA    RF,1(RF)            ADD 1                                        
         STCM  RF,3,TSARCNT        INTO RECORD KEY                              
         STCM  RF,3,STSARCNT       SAVE NEW RECORD NUMBER                       
                                                                                
         CLI   STSARFLG,STSAVEQ    TEST CALLER WANTS TSARKEY SAVED (CC)         
         BNE   *+14                                                             
         MVI   STSARFLG,0          CLEAR CALLER'S FLAG                          
         MVC   STSARKEY,TSARKEY                                                 
                                                                                
         GOTO1 VTSAR,TSARD         ADD RECORD TO TSAR                           
         BNE   TSARAD8                                                          
         MVC   DISMAX,TSPRECN      TAKE NUMBER OF RECORDS                       
*                                  CLEAR TSAR RECORD BEYOND TSARLEN             
         XC    TSARREC+L'TSARLEN(TSARRECL-L'TSARLEN),TSARREC+L'TSARLEN          
         B     TSARADDX                                                         
                                                                                
TSARAD8  TM    TSERRS,TSEDUP       ERROR PROCEDURE                              
         BNO   TSARAD10                                                         
         MVC   FVMSGNO,=AL2(EGRECAOF)                                           
         NI    TWAMODE,TWAMRSRV                                                 
         NI    DISIND,255-DISIOFLO  SET ERROR WAS NOT OVERFLOW                  
         B     TSARADDX                                                         
                                                                                
TSARAD10 TM    TSERRS,TSEEOF       TEST END OF FILE                             
         BO    *+6                                                              
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         TM    DISIND,DISIOFLO     EOF - TEST OVERFLOW ALLOWED                  
         BNZ   TSARADDX            YES  - LET OVERLAY CONTINUE                  
         MVC   FVMSGNO,=AL2(EATRNMAX)                                           
         NI    TWAMODE,TWAMRSRV    SET TO RE-INITIALISE                         
         B     TSARADDX                                                         
                                                                                
TSARADDX CLI   TSERRS,0            SET CONDITION CODE NEQ ON ERROR              
         B     ROUTX                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET A RECORD FROM TSAR                                   *         
* NTRY - R1=A(RECORD NUMBER)                                          *         
***********************************************************************         
                                                                                
TSARGET  L     R2,ATSARBLK                                                      
         USING TSARD,R2            R2=A(TSAR BLOCK)                             
         MVI   TSACTN,TSAGET                                                    
         MVC   TSRNUM,0(R1)                                                     
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0            SET CC FOR CALLER                            
         B     ROUTX                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* TRANSMIT ALL FIELDS ON SCREEN & SET BEFORE & AFTER INDICATORS       *         
***********************************************************************         
                                                                                
XMTSCR   LA    R1,MRKMSGH                                                       
         XR    RE,RE                                                            
         LA    RF,4095(R1)                                                      
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         ICM   RE,1,0(R1)                                                       
         BZ    *+8                                                              
         BXLE  R1,RE,*-12                                                       
         MVI   1(R1),1             SET INDICS                                   
         MVI   2(R1),1                                                          
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* ROOT FACILTIES                                                      *         
***********************************************************************         
                                                                                
                                                                                
FILDISP  DS    0AL1                DISPLACEMENTS TO ACCOUNT FILTERS             
         DC    AL1(RSTFILT1-RSTELD)                                             
         DC    AL1(RSTFILT2-RSTELD)                                             
         DC    AL1(RSTFILT3-RSTELD)                                             
         DC    AL1(RSTFILT4-RSTELD)                                             
         DC    AL1(RSTFILT5-RSTELD)                                             
                                                                                
MRKMAXIO DC    AL2(28000)          MARKER MAX I/O COUNT                         
                                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACMRKWRK                                                       
       ++INCLUDE ACMRKCCD          CREDITOR/CHEQUE DSECT REQUIRED               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* ACGOBLOCK                                                                     
         PRINT OFF                                                              
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'095ACMRK41   03/18/15'                                      
         END                                                                    
