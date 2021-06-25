*          DATA SET ACREPAU02  AT LEVEL 054 AS OF 04/13/20                      
*PHASE ACAU02B                                                                  
*INCLUDE GETLOGO                                                                
*INCLUDE TMSUPD                                                                 
*INCLUDE DMDDNAME                                                               
*INCLUDE DDDYNALLOC                                                             
ACAU02   TITLE '- ACCOUNT UPDATE'                                               
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* RKEJ 054 16JAN20 <SPEC-40066> ADDED EMAIL OVERIDE EID JCL CARD      *         
*                               LOGIC                                 *         
* VGUP 054 18OCT19 <SPEC-40074> Added job name and environment in     *         
*                               Missing posting worker file errors    *         
* NMAL 053 06MAR18 <SPEC-09021> Pay2Job                               *         
* JSAY 053 06MAR18 <SPEC-20111> Post only balanced worker file via SN *         
***********************************************************************         
ACAU02   CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY SSB                                                              
         NMOD1 0,**ACAU**,RA,R9                                                 
         L     R7,0(,R1)                                                        
         USING ACWORKD,R7          R7=A(GLOBAL W/S)                             
         L     R8,ADMASTC                                                       
         USING MASTD,R8            R8=A(MASTC)                                  
         L     R6,LOGOC                                                         
         USING LOGOD,R6            R6=A(LOGOC)                                  
         L     R5,ADBXAREA                                                      
         USING BOXD,R5             R5=A(BOX AREA)                               
         LA    RC,SPACEND                                                       
         USING WORKD,RC            RC=A(LOCAL W/S)                              
         CLI   MODE,RUNFRST                                                     
         JNE   XIT                                                              
                                                                                
         LA    R0,WORKD            CLEAR WORK AREA TO BINARY ZEROES             
         LHI   R1,WORKL                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R3,PHASES           R3=A(LOADABLE PHASE LIST)                    
         LHI   R0,PHASEN                                                        
         LA    R2,APHASES          R2=A(PHASE ADDRESSES)                        
         MVC   MCDUB,=CL8'T00AXX'                                               
INIT02   GOTOR HEXOUT,DMCB,0(R3),MCDUB+4,1                                      
         GOTOR MCVLOADM,DMCB,0                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(L'APHASES,R2),4(R1)                                            
         AHI   R3,L'PHASES                                                      
         AHI   R2,L'APHASES                                                     
         BCT   R0,INIT02                                                        
                                                                                
         GOTOR ADDICTAT,DMCB,C'LL  ',DICI,DICO                                  
                                                                                
         MVI   BOXYORN,YES         INITIALISE BOXES                             
         MVI   BOXOFF,NO                                                        
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+05,BOXTOP                                                
         MVI   BOXROWS+08,BOXMID                                                
         MVI   BOXROWS+99,BOXBOT                                                
         MVC   BOXCOLS,SPACES                                                   
         OI    BOXDDCTL,BOXDDUL+BOXDDLC                                         
         MVI   RCFLAG1,RCFREQLC+RCFREPLC                                        
                                                                                
         ZAP   LEDGDR,PZERO        INITIALISE ACCUMULATORS                      
         ZAP   LEDGCR,PZERO                                                     
         ZAP   UNITDR,PZERO                                                     
         ZAP   UNITCR,PZERO                                                     
         ZAP   COMPDR,PZERO                                                     
         ZAP   COMPCR,PZERO                                                     
         ZAP   FILEDR,PZERO                                                     
         ZAP   FILECR,PZERO                                                     
                                                                                
         XC    TMSUBLK,TMSUBLK     CLEAR TMS UPDATE BLOCK                       
         MVI   WKFIND,0            INITIALISE                                   
                                                                                
         TM    MCPRTIND,MCPRTINL                                                
         BZ    *+8                                                              
         OI    RUNINDS,RUNINOLO    SET NOLOGOS                                  
         CLI   MCTSTRUN,FF                                                      
         BNE   *+8                                                              
         OI    RUNINDS,RUNITEST    SET TEST=YES                                 
         CLI   RCWRITE,YES                                                      
         BE    *+8                                                              
         OI    RUNINDS,RUNIDRFT    SET WRITE=NO                                 
                                                                                
         L     RF,MCSSB                                                         
         USING SSOOFFD,RF                                                       
* NMALIK START P2JOUT DSN NAME 1ST HLQ                                          
*                                                                               
         MVC   DSNP2J,SPACES                                                    
         MVC   DSNP2J(8),=C'TSTDISK.'  DEFAULT ALLOCATED TEST FILE              
                                                                                
         CLI   SSODSPAC,C'A'           NMALIK DSPACE VALUE                      
         BNE   *+10                                                             
         MVC   DSNP2J(8),=C'ACCDISK.'                                           
                                                                                
         CLI   SSODSPAC,C'C'           IS IT CSC                                
         BNE   *+10                                                             
         MVC   DSNP2J(8),=C'CSCDISK.'                                           
                                                                                
         CLI   SSODSPAC,C'Q'           IS IT FQA                                
         BNE   *+10                                                             
         MVC   DSNP2J(8),=C'FQADISK.'                                           
*                                                                               
* NMALIK END                                                                    
         LA    R0,RECVCPY                                                       
         ST    R0,SSOSQCNT         SET A(RECOVERY COUNTS)                       
         DROP  RF                                                               
                                                                                
         MVI   RCSUBPRG,0                                                       
         GOTOR DATCON,DMCB,(4,RCDATE),(1,TODAY1)                                
         GOTOR (RF),(R1),,(2,TODAY2)                                            
                                                                                
         MVC   ERRPDAY,TODAY1DD    SET TODAY IN ERROR FILE KEY                  
         MVC   TRNCTRY,RCCTRY      SET ADDTRN BLOCK VALUES                      
         TM    RUNINDS,RUNIDRFT    TEST WRITE=YES                               
         BZ    *+8                                                              
         OI    TRNINDS,TRNIWRNO    NO - SET ADDTRN INDICATOR                    
*                                                                               
         CLI   MCRECOVR,C'W'       TEST SOON (RECOVER=W)                        
         BNE   *+14                                                             
         OI    TRNINDS,TRNINDIR    YES - SET NO DIRECTORY RE-READS              
         MVC   AERR,AERR0          USE SORTWK01 FOR SOON JOBS                   
*                                                                               
         MVI   TRNMODE,TRNMOFLN                                                 
         MVC   TRNCOMF,ADCOMFAC                                                 
         MVI   TRN#LDGS,MAXLDGQ+1                                               
         LA    R1,LEDGDETS                                                      
         ST    R1,TRNLDG                                                        
         SR    R1,R1                                                            
         CLI   RCTRACE,YES         TEST TRACE=YES                               
         BNE   *+12                                                             
         MVI   BOXOFF,YES          NO BOXES IF TRACE IS ACTIVE                  
         LA    R1,IOHOOK                                                        
         ST    R1,TRNHOOK          SET A(I/O HOOK) IF TRACE IS ACTIVE           
         MVC   TRNACC,AIOACC                                                    
         MVC   TRNOFA,AIOAOFA                                                   
         MVC   TRNBUK,AIOBUK                                                    
         MVC   TRNCAC,AIOCAC                                                    
         MVC   TRNREC,AIOTRN                                                    
         LA    R0,PALAREA                                                       
         STCM  R0,15,TRNPAL                                                     
         MVC   TRNPDAT1,TODAY1                                                  
         MVC   TRNPDAT2,TODAY2                                                  
                                                                                
         MVC   FILE,=CL2'??'                                                    
         L     R2,AIOGEN                                                        
         USING CTWREC,R2           READ SYSTEM LIST RECORD (FOR FILE)           
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVI   CTWKREC,CTWKRSYS                                                 
         MVI   CTWKSYSN,CTWKACC                                                 
         GOTOR DATAMGR,DMCB,DMREAD,CTFILE,CTWREC,CTWREC                         
         BNE   INIT08                                                           
                                                                                
         LA    R1,CTWDATA                                                       
         SR    R0,R0                                                            
         USING SYSELD,R1           LOCATE SYSTEM ELEMENT FOR SENO               
INIT04   CLI   SYSEL,0             TEST EOR                                     
         BE    INIT08                                                           
         CLI   SYSEL,SYSELQ        TEST SYSTEM ELEMENT                          
         BNE   INIT06                                                           
         CLC   SYSSEN,MCIDSENO     TEST FOR CORRECT SE NUMBER                   
         BNE   INIT06                                                           
         MVC   FILENME,SYSNAME     SAVE THE NAME FOR THE EMAIL                  
         MVC   FILE,SYSNAME+3      SYSNAME IS ACC1, ACC2, ACCI1 ETC             
*        LA    RE,SYSNAME+L'SYSNAME-1                                           
*        CLI   0(RE),C' '          LOCATE LAST CHARACTER OF NAME                
*        BH    *+8                                                              
*        BCT   RE,*-8                                                           
*        MVC   FILE,0(RE)          SET LOGICAL SYSTEM NUMBER                    
         B     INIT08                                                           
INIT06   IC    R0,SYSLEN           BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     INIT04                                                           
         DROP  R1,R2                                                            
                                                                                
INIT08   L     R2,AIN              OPEN FILES                                   
         OPEN  ((2),(INPUT))                                                    
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
                                                                                
*NMALIK CODE CHANGES                                                            
                                                                                
         L     R3,MCUTL                                                         
         USING UTLD,R3                                                          
                                                                                
         MVC   TRFSE(3),=C'SE='                                                 
         MVI   TRFSE+3,X'00'                                                    
         MVC   TRFSE+4(1),TSYS                                                  
*                                                                               
         GOTOR DATAMGR,DMCB,(0,DDNAME),TRFSE,0                                  
         TM    8(R1),X'10'         SYSTEM NOT FOUND                             
         JO    *+2                                                              
         LT    R3,8(R1)            GET A(DDNADATA)                              
         JZ    *+2                                                              
         USING DDNAMED,R3                                                       
*                                                                               
INIT08A  DS    0H                                                               
         CLI   DDNASENA+4,C' '     IS LAST BYTE SPACE                           
         BNE   INIT08B                                                          
         MVC   DSNP2J+8(4),DDNASENA                                             
         MVC   DSNP2J+12(8),=C'.PAY2JOB'                                        
         B     INIT08C                                                          
INIT08B  DS    0H                                                               
         MVC   DSNP2J+8(5),DDNASENA                                             
         MVC   DSNP2J+13(8),=C'.PAY2JOB'                                        
*                                                                               
INIT08C  DS    0H                                                               
         MVC   DUB,=X'000400000100'                                             
         GOTO1 DYNALLOC,DMCB,(X'80',=CL8'P2JOUT'),                     +        
               (X'8D',DUB),DSNP2J,0,(X'80',0)                                   
*NMALIK CODE CHANGES                                                            
         L     R3,AP2JOUT                                                       
         OPEN  ((3),(OUTPUT))                                                   
         MVI   PJCPY,0                                                          
                                                                                
         GOTOR SETHDR,0            SET UPDATE RUN DATE                          
         TM    RUNINDS,RUNINOLO    IF NOT PRINTING LOGOS                        
         BZ    READ                                                             
         GOTOR PRINT,DMCB,SPACES,BC01                                           
         B     READ                                                             
         EJECT                                                                  
***********************************************************************         
* READ INPUT FILE AND CHECK FOR CONTROL BREAKS                        *         
***********************************************************************         
                                                                                
READ     GOTOR GETIN               GET NEXT INPUT RECORD                        
         L     R3,AIOIN                                                         
         AHI   R3,4                                                             
         USING PSHEADD,R3                                                       
         CLI   PSHDEL,PSLTELQ      TEST LEDGER TOTAL ELEMENT                    
         BE    READ02                                                           
         CLI   PSHDEL,PSHDELQ      TEST POSTING HEADER ELEMENT                  
         BE    READ02                                                           
         CLI   PSHDEL,PSTKELQ      TEST TRANSACTION KEY ELEMENT                 
         BE    READ02                                                           
         TM    RUNINDS,RUNISPCL    TEST FOR SPECIAL RECORD                      
         BO    READ                                                             
         GOTOR PUTERR,TRNETRNI                                                  
         B     READ                                                             
                                                                                
READ02   CLC   PSHDACC(LDGKEND),LKEY                                            
         BE    GETCPY                                                           
         CLI   LLDG,0              TEST FIRST TIME FOR LEDGER                   
         BE    READ04                                                           
         MVI   LLDG,0                                                           
         TM    LEDGSTAT,LEDGNOTF   TEST BAD LEDGER RECORD                       
         BNZ   READ04                                                           
         AP    UNITDR,LEDGDR                                                    
         AP    UNITCR,LEDGCR                                                    
         GOTOR PRTTOT,LEDGDR                                                    
                                                                                
READ04   CLC   PSHDACC(UNTKEND),LKEY                                            
         BE    GETCPY                                                           
         CLI   LUNT,0              TEST FIRST TIME FOR UNIT                     
         BE    READ06                                                           
         MVI   LUNT,0                                                           
         TM    UNITSTAT,UNITNOTF   TEST BAD UNIT RECORD                         
         BNZ   READ06                                                           
         AP    COMPDR,UNITDR                                                    
         AP    COMPCR,UNITCR                                                    
         MVC   LINED(L'CHGBOXLN),CHGBOXLN                                       
         MVI   SPACING,2                                                        
         MVC   BOXCOLS(L'TOTLBOXC),TOTLBOXC                                     
         GOTOR ACREPORT                                                         
         MVC   LINTDESC+1(L'AC@UNTTO),AC@UNTTO                                  
         MVI   SPACING,2                                                        
         GOTOR PRTTOT,UNITDR                                                    
         MVI   BOXREQ,BOXCLOSE                                                  
         MVI   SPACING,3                                                        
         GOTOR ACREPORT                                                         
                                                                                
READ06   CLC   PSHDACC(CPYKEND),LKEY                                            
         BE    GETCPY                                                           
         CLI   LCPY,0              TEST FIRST TIME FOR COMPANY                  
         BE    READ28                                                           
         MVI   LCPY,0                                                           
         TM    COMPSTAT,COMPNOTF   TEST BAD COMPANY RECORD                      
         BNZ   READ28                                                           
         AP    FILEDR,COMPDR                                                    
         AP    FILECR,COMPCR                                                    
         CP    COMPCR,COMPDR                                                    
         BE    READ06A                                                          
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         ZAP   UPDATERR,PONE       YES - PASS BACK ERROR COUNT                  
         DROP  RF                                                               
READ06A  MVC   BOXCOLS(L'TOTLBOXC),TOTLBOXC                                     
         MVI   BOXREQ,BOXOPEN                                                   
         MVI   SPACING,2                                                        
         GOTOR ACREPORT                                                         
         MVC   LINTDESC+1(L'AC@CPYTS),AC@CPYTS                                  
         MVI   SPACING,2                                                        
         GOTOR PRTTOT,COMPDR                                                    
         MVI   BOXREQ,BOXCLOSE                                                  
         GOTOR ACREPORT                                                         
                                                                                
         TM    COMPSTAT,COMPERRS   TEST ERROR FILE GENERATED                    
         BZ    READ22                                                           
         L     RE,AERRIO           BUILD & ADD FILE TRAILER RECORD              
         LA    R1,4(RE)                                                         
         USING PSSUBFD,R1          BUILD SUB-FILE ELEMENT                       
         MVI   PSSBEL,PSSBELQ                                                   
         MVI   PSSBLEN,PSSUBFL                                                  
         MVC   PSSBDESC,AC@PGAPE                                                
         ZAP   PSSBRECS,ERRCT                                                   
         ZAP   PSSBCASH,PZERO                                                   
         AHI   R1,PSSUBFL                                                       
         MVI   0(R1),0                                                          
         AHI   R1,1                                                             
         SR    R1,RE                                                            
         SLL   R1,16                                                            
         STCM  R1,15,0(RE)         SET RECORD LENGTH                            
         DROP  R1                                                               
                                                                                
         GOTOR DATAMGR,DMCB,WKADDR,WKFILE,ERRID,AERRIO,AERRBUFF                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR DATAMGR,DMCB,WKCLOS,WKFILE,ERRID,AERRIO,AERRBUFF                 
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   FORCECLR,YES                                                     
         GOTOR ACREPORT            CLEAR HEADINGS TO SPACES                     
         MVI   RCSUBPRG,2                                                       
         MVC   PAGE,HONE                                                        
         MVC   LINE,MAXLINES                                                    
         MVI   BOXYORN,YES                                                      
         MVI   BOXINIT,0                                                        
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS+(WKLBXLC-LINED),BOXLEFT                                  
         MVI   BOXCOLS+(WKLBXC1-LINED),BOXCOLM                                  
         MVI   BOXCOLS+(WKLBXC2-LINED),BOXCOLM                                  
         MVI   BOXCOLS+(WKLBXC3-LINED),BOXCOLM                                  
         MVI   BOXCOLS+(WKLBXC4-LINED),BOXCOLM                                  
         MVI   BOXCOLS+(WKLBXC5-LINED),BOXCOLM                                  
         MVI   BOXCOLS+(WKLBXC6-LINED),BOXCOLM                                  
         MVI   BOXCOLS+(WKLBXC7-LINED),BOXCOLM                                  
         MVI   BOXCOLS+(WKLBXRC-LINED),BOXRIGHT                                 
         ZAP   ERRDR,PZERO                                                      
         ZAP   ERRCR,PZERO                                                      
                                                                                
READ08   GOTOR DATAMGR,DMCB,WKREAD,WKFILE,ERRID,AERRIO,AERRBUFF                 
         TM    8(R1),X'81'                                                      
         BNZ   READ20                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    RE,RE               LOCATE POSTING ELEMENTS                      
         SR    RF,RF                                                            
         L     R1,AERRIO                                                        
         ICM   RF,3,0(R1)                                                       
         AR    RF,R1                                                            
         MVI   0(RF),0             SET EOR                                      
         SR    RF,RF                                                            
         AHI   R1,4                                                             
         SR    R0,R0                                                            
READ10   CLI   PSHDEL-PSHEADD(R1),0                                             
         BE    READ14                                                           
         CLI   TRNEL-TRNELD(R1),TRNELQ                                          
         BNE   *+6                                                              
         LR    RF,R1                                                            
         CLI   PSHDEL-PSHEADD(R1),PSHDELQ                                       
         BNE   READ12                                                           
         CLI   PSHDLEN-PSHEADD(R1),PSHEADL                                      
         BNE   *+6                                                              
         LR    RE,R1                                                            
READ12   IC    R0,PSHDLEN-PSHEADD(R1)                                           
         AR    R1,R0                                                            
         B     READ10                                                           
                                                                                
         PUSH  USING                                                            
         DROP  R3                                                               
READ14   LTR   R2,RE                                                            
         BZ    READ08                                                           
         USING PSHEADD,R2                                                       
         MVC   WKLACT,PSHDAUNT                                                  
         MVC   WKLCAC,PSHDSUNT                                                  
         MVC   WKLOFF1,PSHDANAL                                                 
                                                                                
         LTR   R4,RF                                                            
         BZ    READ08                                                           
         USING TRNELD,R4                                                        
         CLC   TRNOFFC,SPACES                                                   
         BE    READ16                                                           
         MVC   WKLOFF2,TRNOFFC                                                  
         CLC   WKLOFF1,SPACES                                                   
         BE    READ16                                                           
         MVI   WKLSLASH,C'/'       IF TWO CODES, SEPARATE THEM                  
READ16   GOTOR DATCON,DMCB,(1,TRNDATE),(8,WKLDAT)                               
         LA    RE,ERRDR                                                         
         LA    RF,WKLDR                                                         
         TM    TRNSTAT,TRNSDR      DEBIT OR CREDIT?                             
         BNZ   *+12                                                             
         LA    RE,ERRCR                                                         
         LA    RF,WKLCR                                                         
         AP    0(L'ERRCR,RE),TRNAMNT                                            
         CURED TRNAMNT,(L'WKLCR,(RF)),2,MINUS=YES                               
         MVC   WKLREF,TRNREF                                                    
         SR    R0,R0               PRINT ONE/TWO LINES OF NARRATIVE             
         IC    R0,TRNLN                                                         
         SHI   R0,TRNLN1Q                                                       
         BZ    READ18                                                           
         CLC   PRODUL,PSHDAUNT                                                  
         BNE   *+10                                                             
         CLC   BILLANAL,TRNOFFC                                                 
         BNE   *+8                                                              
         LHI   R0,15               PROD. BILLS - PRINT 15 BYTES ONLY            
         GOTOR CHOPPER,DMCB,((R0),TRNNARR),(L'WKLNARR,WKLNARR),        *        
               (C'P',2)                                                         
READ18   GOTOR ACREPORT                                                         
         B     READ08                                                           
         DROP  R2                                                               
                                                                                
READ20   GOTOR ACREPORT                                                         
         MVC   WKLCAC(L'AC@TOTLS),AC@TOTLS                                      
         CURED ERRDR,(L'WKLDR,WKLDR),2,MINUS=YES                                
         CURED ERRCR,(L'WKLCR,WKLCR),2,MINUS=YES                                
         CURED ERRCT,(8,WKLNARR),0,ALIGN=LEFT                                   
         LA    R1,WKLNARR+1                                                     
         AR    R1,R0               R0=SIGNIFICANT LENGTH                        
         CP    ERRCT,PONE          ONE ITEM?                                    
         BE    *+14                                                             
         MVC   0(L'AC@ITEMS,R1),AC@ITEMS                                        
         B     *+10                                                             
         MVC   0(L'AC@ITEM,R1),AC@ITEM                                          
         GOTOR ACREPORT                                                         
         MVI   BOXREQ,BOXCLOSE                                                  
         GOTOR ACREPORT                                                         
         MVI   RCSUBPRG,0                                                       
         TM    RUNINDS,RUNITEST+RUNIDRFT                                        
         BZ    READ22                                                           
         GOTOR DATAMGR,DMCB,WKPRGE,WKFILE,ERRID,AERRIO,AERRBUFF                 
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*&&US                                                                           
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         OC    WKFILEID,WKFILEID   TEST AC25 DRAFT UPDATE                       
         BZ    READ22                                                           
         ZAP   UPDATERR,ERRCT      YES - PASS BACK ERROR COUNT                  
         B     READ28                                                           
         DROP  RF                                                               
*&&                                                                             
READ22   TM    RUNINDS,RUNIREMO    TEST COMPANY REMOTE                          
         BZ    READ24                                                           
         GOTOR PRINT,DMCB,SPACES,BC01                                           
         GOTOR (RF),(R1),(L'PRTCLOSE,PRTCLOSE)                                  
         B     READ28                                                           
                                                                                
READ24   TM    RUNINDS,RUNINOLO    TEST NOLOGOS                                 
         BZ    READ26                                                           
         GOTOR PRINT,DMCB,SPACES,BC01                                           
         MVI   NEWPAGE,NO          SET DON'T SKIP TO CHANNEL 1 NEXT             
         B     READ28                                                           
                                                                                
READ26   MVI   LOGOTYPE,LOGOLAST   PRINT END LOGOS FOR PREVIOUS COMPANY         
         GOTOR LOGO,DMCB,LOGOD                                                  
                                                                                
READ28   CLI   PSHDACC,FF          TEST LAST RECORD                             
         BNE   GETCPY                                                           
         OI    TRNINDS,TRNILAST    CALL ADDTRN FOR LAST TIME                    
         OI    TRNINDS2,TRNIUPDG                                                
         GOTOR VADDTRN,TRNBLK                                                   
         BE    TOTALS                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* READ COMPANY RECORD. ON CHANGE OF COMPANY, EXTRACT COMPANY RECORD   *         
* VALUES AND PRINT START LOGOS.                                       *         
***********************************************************************         
                                                                                
GETCPY   CLC   LCPYSV,PSHDACPY                                                  
         BE    GETCPY20                                                         
         CLI   LCPYSV,0            TEST FIRST TIME                              
         BE    GETCPY00                                                         
         OI    TRNINDS,TRNILAST    NO - CALL ADDTRN FOR LAST TIME               
         OI    TRNINDS2,TRNIUPDG                                                
         GOTOR VADDTRN,TRNBLK                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    TRNINDS,FF-(TRNILAST)                                            
                                                                                
GETCPY00 MVI   TRNPALI,0           RESET P&L INDICATOR BYTE                     
         MVC   LCPY,PSHDACPY                                                    
         MVC   LCPYSV,PSHDACPY                                                  
         XC    LEDGXTRA(MAXLDGQ*LEDGDETL),LEDGXTRA                              
         MVI   COMPSTAT,0                                                       
         MVI   LLDG,0                                                           
         MVI   LUNT,0                                                           
                                                                                
         L     R2,AIOGEN                                                        
         USING CPYRECD,R2          R2=A(COMPANY RECORD)                         
         MVC   CPYKEY,SPACES       BUILD KEY OF COMPANY RECORD                  
         MVC   CPYKCPY,PSHDACC                                                  
         GOTOR DATAMGR,DMCB,DMREAD,ACCFIL,CPYRECD,CPYRECD                       
         BE    *+12                                                             
         MVI   COMPSTAT,COMPNOTF                                                
         B     GETCPY20                                                         
                                                                                
         MVC   COMPNAME,SPACES                                                  
         MVC   COMPADD1,SPACES                                                  
         MVC   COMPADD2,SPACES                                                  
         MVC   COMPADD3,SPACES                                                  
         MVC   COMPLOGO,SPACES                                                  
                                                                                
         LA    R1,CPYRECD+ACCORFST                                              
         USING CPYELD,R1                                                        
         SR    R0,R0                                                            
GETCPY02 CLI   CPYEL,0             TEST EOR                                     
         BE    GETCPY12                                                         
         CLI   CPYEL,CPYELQ        TEST COMPANY ELEMENT                         
         BE    GETCPY06                                                         
         CLI   CPYEL,NAMELQ        TEST NAME ELEMENT                            
         BE    GETCPY08                                                         
         CLI   CPYEL,ADRELQ        TEST ADDRESS ELEMENT                         
         BE    GETCPY10                                                         
GETCPY04 IC    R0,CPYLN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     GETCPY02                                                         
                                                                                
GETCPY06 MVC   COMPALPH,CPYALPHA                                                
         MVC   COMPLOGO,CPYLOGO                                                 
         MVC   COMPUSER,CPYUID                                                  
                                                                                
         MVI   COMPSTAD,0                                                       
         CLI   CPYLN,CPYLN4Q                                                    
         JL    GETCPY07                                                         
         MVC   COMPSTAD,CPYSTATD                                                
                                                                                
GETCPY07 MVC   TRNCPYS1,CPYSTAT1                                                
         MVC   TRNCPYS2,CPYSTAT2                                                
         MVC   TRNCPYS3,CPYSTAT3                                                
         MVC   TRNCPYS4,CPYSTAT4                                                
         MVC   TRNCPYS5,CPYSTAT5                                                
         MVC   TRNCPYS6,CPYSTAT6                                                
         MVC   TRNCPYS7,CPYSTAT7                                                
         MVC   TRNCPYS8,CPYSTAT8                                                
         MVI   TRNCPYS9,0                                                       
         MVI   TRNCPYSA,0                                                       
         XC    TRNGLMOA,TRNGLMOA   Reset GL info                                
         CLI   CPYLN,CPYLN3Q                                                    
         BL    GETCPY04                                                         
         MVC   TRNCPYS9,CPYSTAT9                                                
         MVC   TRNCPYSA,CPYSTATA                                                
         CLI   CPYLN,CPYLN4Q                                                    
         BL    GETCPY04                                                         
         MVC   TRNGLMOA,CPYGLMOA                                                
         B     GETCPY04                                                         
                                                                                
         USING NAMELD,R1                                                        
GETCPY08 SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         EX    RE,*+8                                                           
         B     GETCPY04                                                         
         MVC   COMPNAME(0),NAMEREC                                              
                                                                                
         USING ADRELD,R1                                                        
GETCPY10 MVC   COMPADD1,ADRADD1                                                 
         CLI   ADRNUM,1                                                         
         BE    GETCPY04                                                         
         MVC   COMPADD2,ADRADD2                                                 
         CLI   ADRNUM,2                                                         
         BE    GETCPY04                                                         
         MVC   COMPADD3,ADRADD3                                                 
         B     GETCPY04                                                         
                                                                                
GETCPY12 NI    RUNINDS,FF-RUNIREMO SET NOT REMOTE                               
         TM    RUNINDS,RUNITEST    TEST TEST=YES                                
         BNZ   GETCPY16                                                         
                                                                                
         L     RF,REMOTEC                                                       
         USING REMOTED,RF                                                       
         XC    REMOTKEY,REMOTKEY   CLEAR REMOTE KEY                             
         DROP  RF                                                               
                                                                                
         L     R2,AIOGEN                                                        
         USING CTPREC,R2           R2=A(PROFILE RECORD)                         
         XC    CTPKEY,CTPKEY                                                    
         MVI   CTPKTYP,CTPKTYPQ                                                 
         MVI   CTPKSYS,ACCSYSQ                                                  
         MVC   CTPKPROG,RCPROG                                                  
         MVC   CTPKORIG,COMPUSER                                                
         GOTOR DATAMGR,DMCB,DMREAD,CTFILE,CTPREC,CTPREC                         
         BNE   GETCPY16                                                         
                                                                                
         SR    R0,R0                                                            
         LA    R1,CTPDATA          R1=A(FIRST ELEMENT)                          
         USING CTOCOD,R1                                                        
GETCPY14 IC    R0,CTOCOLEN         LOCATE OUTPUT TYPE CODE ELEMENT              
         AR    R1,R0                                                            
         CLI   CTOCOEL,0           TEST EOR                                     
         BE    GETCPY16                                                         
         CLI   CTOCOEL,CTOCOELQ    TEST OUTPUT CODE ELEMENT                     
         BNE   GETCPY14                                                         
         CLC   CTOCODE(L'REMLIT),REMLIT                                         
         BNE   GETCPY16                                                         
         DROP  R1                                                               
                                                                                
         OI    RUNINDS,RUNIREMO    SET COMPANY IS REMOTE                        
                                                                                
         L     RF,REMOTEC                                                       
         USING REMOTED,RF                                                       
         MVI   REMOTJID,ACCSYSQ                                                 
         MVC   REMOTJID+1(L'RCPROG),RCPROG                                      
         MVC   REMOTKEY(L'REPLIT),REPLIT                                        
         MVC   REMOTDST,COMPUSER                                                
         MVI   REMOTCLS,C'Q'                                                    
         DROP  RF                                                               
                                                                                
GETCPY16 TM    RUNINDS,RUNINOLO+RUNIREMO                                        
         BNZ   GETCPY18                                                         
         MVC   LOGO1,COMPLOGO      PRINT START LOGOS FOR COMPANY                
         MVC   LOGONAME,COMPNAME                                                
         MVC   LOGOADD,COMPADD1                                                 
         GOTOR VGETLOGO,DMCB,COMPUSER,LOGOD,DATAMGR                             
         MVC   LOGOJOB,SPACES                                                   
         MVC   LOGOJOB(4),DMCB                                                  
         CLI   LOGOJOB+3,C' '                                                   
         BH    *+8                                                              
         MVI   LOGOJOB+3,C'X'                                                   
         MVI   LOGOJOB+4,ACCSYSQ                                                
         MVC   LOGOJOB+5(L'RCPROG),RCPROG                                       
         MVI   LOGOTYPE,LOGOFRST                                                
         GOTOR LOGO,DMCB,LOGOD                                                  
                                                                                
GETCPY18 MVI   FORCEHED,YES                                                     
         MVC   PAGE,HONE                                                        
                                                                                
GETCPY20 TM    COMPSTAT,COMPNOTF   TEST COMPANY STATUS                          
         BZ    GETCPY22                                                         
         GOTOR PUTERR,COMPINVQ                                                  
         B     READ                                                             
                                                                                
GETCPY22 OC    RCFFPARM(L'COMPALPH),RCFFPARM                                    
         BZ    GETUNT                                                           
         CLC   COMPALPH,RCFFPARM   APPLY COMPANY FILTER                         
         BNE   READ                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* READ UNIT RECORD ON CHANGE OF UNIT, EXTRACT UNIT VALUES.            *         
***********************************************************************         
                                                                                
GETUNT   CLC   LUNT,PSHDAUNT                                                    
         BE    GETUNT08                                                         
         MVC   LUNT,PSHDAUNT                                                    
         MVI   UNITSTAT,0                                                       
         MVI   LLDG,0                                                           
                                                                                
         L     R2,AIOGEN                                                        
         USING UNTRECD,R2          R2=A(UNIT RECORD)                            
         MVC   UNTKEY,SPACES       BUILD KEY OF UNIT RECORD                     
         MVC   UNTKEY(UNTKEND),PSHDACC                                          
         GOTOR DATAMGR,DMCB,DMREAD,ACCFIL,UNTRECD,UNTRECD                       
         BE    *+12                                                             
         MVI   UNITSTAT,UNITNOTF                                                
         B     GETUNT08                                                         
                                                                                
         MVC   UNITNAME,SPACES                                                  
         LA    R1,UNTRECD+ACCORFST                                              
         USING NAMELD,R1                                                        
         SR    R0,R0                                                            
GETUNT02 CLI   NAMEL,0             TEST EOR                                     
         BE    GETUNT08                                                         
         CLI   NAMEL,NAMELQ        TEST NAME ELEMENT                            
         BE    *+14                                                             
         IC    R0,NAMLN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     GETUNT02                                                         
                                                                                
         SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   UNITNAME(0),NAMEREC                                              
                                                                                
         MVC   BOXCOLS(L'UNITBOXC),UNITBOXC                                     
         CLI   FORCEHED,YES                                                     
         BNE   GETUNT04                                                         
         MVC   HEAD4+01(L'AC@CPY),AC@CPY  COPY HEADERS FOR NEW PAGE             
         MVC   HEAD4+09(L'COMPALPH),COMPALPH                                    
         MVC   HEAD4+12(L'COMPNAME),COMPNAME                                    
         B     GETUNT06                                                         
                                                                                
GETUNT04 MVI   BOXREQ,BOXOPEN      OPEN BOX FOR UNIT                            
         GOTOR ACREPORT                                                         
                                                                                
GETUNT06 MVC   LINUNIT,UNITNAME    MOVE UNIT NAME TO PRINT LINE                 
         CLI   UNITNAME+(L'LINUNIT),C' '                                        
         BE    GETUNT08                                                         
         LA    R1,LINUNIT+L'LINUNIT-1                                           
         CLI   0(R1),C' '                                                       
         BE    GETUNT08                                                         
         MVI   0(R1),C' '                                                       
         BCT   R1,*-12                                                          
                                                                                
GETUNT08 TM    UNITSTAT,UNITNOTF   TEST UNIT STATUS                             
         BZ    GETLDG                                                           
         GOTOR PUTERR,UNITINVQ                                                  
         B     READ                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* READ LEDGER RECORD ON CHANGE OF LEDGER, EXTRACT LEDGER VALUES.      *         
***********************************************************************         
                                                                                
GETLDG   CLC   LLDG,PSHDALDG                                                    
         BE    GETLDG14                                                         
         MVC   LLDG,PSHDALDG                                                    
         MVI   LEDGSTAT,0                                                       
                                                                                
         L     R2,AIOGEN                                                        
         USING LDGRECD,R2          R2=A(LEDGER RECORD)                          
         MVC   LDGKEY,SPACES       BUILD KEY OF LEDGER RECORD)                  
         MVC   LDGKEY(LDGKEND),PSHDACC                                          
         GOTOR DATAMGR,DMCB,DMREAD,ACCFIL,LDGRECD,LDGRECD                       
         BE    *+12                                                             
         MVI   LEDGSTAT,LEDGNOTF                                                
         B     GETLDG14                                                         
                                                                                
         XC    LEDGDETS(LEDGDETL+1),LEDGDETS                                    
         MVC   LEDGUL,LDGKUNT                                                   
         MVC   LEDGNAME,SPACES                                                  
         LA    R1,LDGRECD+ACCORFST                                              
         USING LDGELD,R1                                                        
         SR    R0,R0                                                            
GETLDG02 CLI   LDGEL,0             TEST EOR                                     
         BE    GETLDG14                                                         
         CLI   LDGEL,LDGELQ        TEST LEDGER ELEMENT                          
         BE    GETLDG06                                                         
         CLI   LDGEL,ACLELQ        TEST ACCOUNT LENGTHS ELEMENT                 
         BE    GETLDG08                                                         
         CLI   LDGEL,NAMELQ        TEST NAME ELEMENT                            
         BE    GETLDG10                                                         
         CLI   LDGEL,RSTELQ        TEST STATUS ELEMENT                          
         BE    GETLDG12                                                         
GETLDG04 IC    R0,LDGLN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     GETLDG02                                                         
                                                                                
GETLDG06 MVC   LEDGTYPE,LDGTYPE                                                 
         MVC   LEDGLIKE,LDGLIKE                                                 
         MVC   LEDGOFFP,LDGOPOS                                                 
         MVC   LEDGCLOS,LDGCLOS                                                 
         CLC   LEDGUL,CCSTUL                                                    
         BNE   *+10                                                             
         MVC   LEDGCPOS,LDGCPOS                                                 
         B     GETLDG04                                                         
                                                                                
         USING ACLELD,R1                                                        
GETLDG08 MVC   LEDGLVA,ACLVLEN                                                  
         MVC   LEDGLVB,ACLVLEN+(L'ACLVALS*1)                                    
         MVC   LEDGLVC,ACLVLEN+(L'ACLVALS*2)                                    
         MVC   LEDGLVD,ACLVLEN+(L'ACLVALS*3)                                    
         B     GETLDG04                                                         
                                                                                
         USING NAMELD,R1                                                        
GETLDG10 SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         EX    RE,*+8                                                           
         B     GETLDG04                                                         
         MVC   LEDGNAME(0),NAMEREC                                              
                                                                                
         USING RSTELD,R1                                                        
GETLDG12 MVC   LEDGSECY,RSTSECY+1                                               
         B     GETLDG04                                                         
                                                                                
GETLDG14 TM    LEDGSTAT,LEDGNOTF   TEST LEDGER STATUS                           
         BZ    GETLDG16                                                         
         GOTOR PUTERR,LEDGINVQ                                                  
         B     READ                                                             
                                                                                
GETLDG16 MVC   LINLEDG,LEDGNAME                                                 
         B     BLDTRN                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD A TRANSACTION RECORD AND CALL ADDTRN TO ADD IT                *         
***********************************************************************         
                                                                                
         USING PSLTOTD,R3                                                       
BLDTRN   CLI   PSLTEL,PSLTELQ      TEST LEDGER TOTAL ELEMENT                    
         BNE   BLDTRN02                                                         
         CLI   PSLTLEN,PSLTOTL     (INSURANCE)                                  
         BNE   BLDTRN02                                                         
         AP    LEDGDR,PSLTDR       UPDATE LEDGER TOTALS                         
         AP    LEDGCR,PSLTCR                                                    
         B     READ                GET NEXT RECORD                              
                                                                                
         USING PSHEADD,R3                                                       
BLDTRN02 L     R2,AIOTRN                                                        
         USING TRNRECD,R2          R2=A(TRANSACTION RECORD)                     
         XC    TRNKEY(256),TRNKEY  INITIALISE TRANSACTION RECORD                
         NI    TRNINDS,FF-TRNILIVE                                              
         CLI   PSHDEL,PSHDELQ      TEST POSTING HEADER ELEMENT                  
         BE    BLDTRN04                                                         
         CLI   PSHDEL,PSTKELQ                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TRNKEY,PSHEADD+(PSTKEY-PSTKEYD)                                  
         OI    TRNINDS,TRNILIVE    SET TRANSACTION DRAFT TO LIVE                
         B     BLDTRN08                                                         
                                                                                
BLDTRN04 MVC   TRNCACNM,PSHDSBNM   SAVE CONTRA-ACCOUNT NAME                     
         MVC   TRNKCULA,PSHDACC    ACCOUNT CODE                                 
         MVC   TRNKOFF,PSHDANAL    OFFICE/ANALYSIS CODE                         
         MVC   TRNKCULC,PSHDSBAC   CONTRA-ACCOUNT CODE                          
         SR    R0,R0                                                            
         IC    R0,PSHDLEN                                                       
         AR    R3,R0                                                            
         USING TRNELD,R3           R3=A(TRANSACTION ELEMENT)                    
         OC    TRNBTCH,SPACES                                                   
         OC    TRNREF,SPACES                                                    
         MVC   TRNKDATE,TRNDATE    TRANSACTION DATE                             
         MVC   TRNKREF,TRNREF      TRANSACTION REFERENCE                        
         MVC   TRNKSBR,TRNSUB      TRANSACTION SUB-REFERENCE                    
                                                                                
         LA    R1,TRNELD           LOCATE END OF INPUT RECORD                   
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   *-10                                                             
                                                                                
         LA    R0,TRNRECD+ACCORFST                                              
         AHI   R1,1                                                             
         SR    R1,R3                                                            
         LA    RE,TRNELD                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE ELEMENTS TO TRANSACTION RECORD          
                                                                                
         LA    R1,TRNRECD+ACCORFST                                              
         SR    R0,R0                                                            
         SR    RE,RE                                                            
BLDTRN06 IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   TRSEL-TRSELD(R1),TRSELQ                                          
         BNE   *+8                                                              
         CLI   TRSLN-TRSELD(R1),TRSLNQ                                          
         BNE   *+6                                                              
         LR    RE,R1               SAVE A(TRANSACTION STATUS ELEMENT)           
         CLI   0(R1),0             TEST EOR                                     
         BNE   BLDTRN06                                                         
                                                                                
         AHI   R1,1                SET RECORD LENGTH                            
         LA    R0,TRNRECD                                                       
         SR    R1,R0                                                            
         STCM  R1,3,TRNRLEN                                                     
                                                                                
         XC    TRNBMOS,TRNBMOS     SET ADDTRN EXTRA VALUES                      
         XC    TRNBSEQN,TRNBSEQN                                                
         XC    TRNPUSER,TRNPUSER                                                
         LTR   RE,RE               TEST STATUS ELEMENT FOUND                    
         BZ    BLDTRN08                                                         
         MVC   TRNBMOS,TRSPMOS-TRSELD(RE)                                       
         MVC   TRNBSEQN,TRSBSEQ-TRSELD(RE)                                      
         MVC   TRNPUSER,TRSUSER-TRSELD(RE)                                      
                                                                                
BLDTRN08 DS    0H                                                               
*&&UK                                                                           
         NI    TRNINDS2,FF-(TRNIDNAM)                                           
         CLC   =C'SZ',TRNKUNT      FOR EUROPEAN MEDIA PAYABLES                  
         BE    *+10                                                             
         CLC   =C'SF',TRNKUNT                                                   
         BNE   *+8                                                              
         OI    TRNINDS2,TRNIDNAM   ADD DUMMY NAME IF NONE FOUND                 
*&&                                                                             
         OI    TRNINDS2,TRNIADDG                                                
         GOTOR VADDTRN,TRNBLK      ADD TRANSACTION TO FILE                      
         BE    BLDTRN10                                                         
*&&US                                                                           
         CLC   TRNKULA,=CL14'SJCCCPPPJJJJJJ'                                    
         BE    *+10                                                             
         CLC   TRNKULA,=CL14'SV$$DON"T POST'                                    
         BE    *+10                                                             
         CLC   TRNKULA,=CL14'SW$$DON"T POST'                                    
         BNE   *+8                                                              
         OI    WKFIND,WKFNOPST     DON'T POST TI WORKER FILE                    
*&&                                                                             
         LLC   R1,TRNERRS          R1=ADDTRN ERROR VALUE                        
         CLI   TRNGLER#,ERRNOFLT   Report GL errors equal or above              
         BL    *+10                                                             
         LLC   R1,TRNGLER#                                                      
         GOTOR PUTERR,(R1)                                                      
         B     READ                                                             
                                                                                
BLDTRN10 CLC   PERSUL,TRNKUNT      CALL TMSUPD IF PERSON LEDGER                 
         BNE   BLDTRN12                                                         
         GOTOR VTMSUPD,DMCB,(X'C0',TRNRECD),ADCOMFAC,TMSUBLK,0                  
                                                                                
BLDTRN12 AP    LEDGDR,TRNTRNDR     UPDATE LEDGER TOTALS                         
         AP    LEDGCR,TRNTRNCR                                                  
                                                                                
*&&US*&& CLC   PRODUL,TRNKUNT                                                   
*&&US*&& BNE   *+8                                                              
*&&US*&& GOTOR UPDORD              UPDATE THE ORDER RECORD FOR TALENT           
                                                                                
BLDTRN15 TM    COMPSTAD,CPYSAP2J   Pay2Job checks                               
         JZ    BLDTRN40                                                         
         CLI   TRNKUNT,C'S'                                                     
         JNE   BLDTRN40                                                         
         CLI   TRNKLDG,C'V'                                                     
         JE    BLDTRN16                                                         
*&&US                                                                           
         CLI   TRNKLDG,C'W'                                                     
         JE    BLDTRN16                                                         
         CLI   TRNKLDG,C'X'                                                     
         JE    BLDTRN16                                                         
         CLI   TRNKLDG,C'Y'                                                     
         JNE   BLDTRN40                                                         
*&&                                                                             
*&&UK                                                                           
         CLI   TRNKLDG,C'X'                                                     
         JNE   BLDTRN40                                                         
*&&                                                                             
MY       USING TRNELD,R4                                                        
BLDTRN16 LA    R4,TRNRECD+ACCORFST                                              
         CLI   MY.TRNTYPE,TRNTCHQS                                              
         JE    BLDTRN17                                                         
*&&UK                                                                           
         CLI   MY.TRNTYPE,TRNTTRFS                                              
         JE    BLDTRN17                                                         
*&&                                                                             
         CLI   MY.TRNTYPE,TRNTPAYS                                              
         JNE   BLDTRN40                                                         
                                                                                
BLDTRN17 TM    MY.TRNSTAT,TRNSDR    Skip credits (should not be here)           
         JZ    BLDTRN40                                                         
         DROP  MY                                                               
                                                                                
         USING MPYELD,R4                                                        
BLDTRN18 LLC   R0,MPYLN                                                         
         AR    R4,R0                                                            
         CLI   MPYEL,0                                                          
         JE    BLDTRN40                                                         
         CLI   MPYEL,MPYELQ                                                     
         JNE   BLDTRN18                                                         
         DROP  R4                                                               
                                                                                
         CLI   RCWRITE,YES         TRNTRXDA not set under ACAU as emu-          
         JNE   BLDTRN30            LATED RECORDS USED, SO READ FOR D/A          
         LR    R0,R4                                                            
R4DA     USING TRNRECD,R4          and put to TRNTRXDA                          
         L     R4,AIOGEN                                                        
         MVC   R4DA.TRNKEY,TRNKEY                                               
         GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,R4DA.TRNKEY,R4DA.TRNKEY               
         JNE   *+2                                                              
         L     R4,AIOGEN                                                        
         MVC   TRNTRXDA,R4DA.TRNKDA                                             
         LR    R4,R0                                                            
         DROP  R4DA                                                             
                                                                                
BLDTRN30 OC    TRNTRXDA,TRNTRXDA       CHECK IF DA AVAILABLE    NMAL            
         JZ    BLDTRN40                IF NOT, NO NEED TO WRITE NMAL            
         GOTOR PUTJOB              (pass R4=MPYEL)                              
                                                                                
BLDTRN40 DS    0H                                                               
                                                                                
         B     READ                GET NEXT RECORD                              
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* UPDATE ORDER RECORD                                                 *         
***********************************************************************         
                                                                                
UPDORD   NTR1  ,                                                                
         L     R2,AIOTRN           FIND FFTEL FOR ORDER CLOSE                   
         USING TRNRECD,R2                                                       
                                                                                
         LA    R3,TRNRFST                                                       
         TM    TRNINDS,TRNILIVE    TEST DRAFT TO LIVE                           
         BO    *+8                                                              
         LA    R3,TRNRECD+ACCORFST                                              
         SR    R0,R0                                                            
         USING FFTELD,R3                                                        
UPDORD02 IC    R0,FFTLN                                                         
         AR    R3,R0                                                            
         CLI   FFTEL,0                                                          
         JE    XIT                                                              
         CLI   FFTEL,FFTELQ                                                     
         BNE   UPDORD02                                                         
                                                                                
         CLI   FFTTYPE,FFTTORDC    TEST ORDER CLOSE                             
         BNE   UPDORD02                                                         
                                                                                
         L     R4,AIOGEN                                                        
         USING ORDRECD,R4                                                       
         XC    ORDKEY,ORDKEY       BUILD KEY FOR ORDER                          
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,TRNKCPY                                                  
         MVC   ORDKORD,FFTDATA                                                  
                                                                                
         GOTOR DATAMGR,DMCB,DMREAD,ACCFIL,ORDRECD,ORDRECD                       
         JNE   XIT                                                              
         TM    ORDRECD+ACCOSTAT,ORDSLDEL+ORDSDEL                                
         JNZ   XIT                                                              
         OI    ORDRECD+ACCOSTAT,ORDSFMCH                                        
         L     RF,DATAMGR                                                       
         GOTOR ,DMCB,DMWRT,ACCFIL,ORDRECD,ORDRECD                               
         TM    RUNINDS,RUNIDRFT    TEST WRITE ALLOWED                           
         BNZ   *+6                                                              
         BASR  RE,RF                                                            
                                                                                
         ICM   RF,15,PORDCLO       COUNT CLOSED ORDERS                          
         AHI   RF,1                                                             
         STCM  RF,15,PORDCLO                                                    
                                                                                
         LA    R4,ORDRECD+ACCORFST FIND ORDER ELEMENT                           
         USING ORDELD,R4                                                        
         SR    R0,R0                                                            
UPDORD04 CLI   ORDEL,0                                                          
         JE    XIT                                                              
         CLI   ORDEL,ORDELQ                                                     
         BE    *+14                                                             
         IC    R0,ORDLN                                                         
         AR    R4,R0                                                            
         B     UPDORD04                                                         
                                                                                
         L     R2,AIOGEN           GET SJ SIDE OF ORDER                         
         USING TRNRECD,R2                                                       
         XC    TRNKEY,TRNKEY                                                    
         MVC   TRNKCULA,ORDJOB                                                  
         MVC   TRNKWORK,=C'**'                                                  
         MVC   TRNKCULC,ORDSUP                                                  
         MVC   TRNKDATE,ORDDATE                                                 
         MVC   TRNKREF,FFTDATA                                                  
         GOTOR DATAMGR,DMCB,DMREAD,ACCFIL,TRNRECD,TRNRECD                       
         JNE   XIT                                                              
         OI    TRNRECD+ACCOSTAT,TRNSDELT                                        
         L     RF,DATAMGR                                                       
         GOTOR ,DMCB,DMWRT,ACCFIL,TRNRECD,TRNRECD                               
         TM    RUNINDS,RUNIDRFT    TEST WRITE ALLOWED                           
         BNZ   *+6                                                              
         BASR  RE,RF                                                            
                                                                                
         ICM   RF,15,TRNTDEL       COUNT DELETED RECORDS                        
         AHI   RF,1                                                             
         STCM  RF,15,TRNTDEL                                                    
         J     XIT                                                              
         DROP  R2,R3,R4                                                         
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT FILE TOTALS PAGE                                              *         
***********************************************************************         
                                                                                
TOTALS   GOTOR SETHDR,1            SET GOOD UPDATE RUN DATE                     
                                                                                
         L     R3,AP2JOUT          CLOSE OUTPUT FILE                            
         CLOSE ((3))                                                            
                                                                                
         MVI   LOGOTYPE,LOGOFRST   START LOGO FOR CONTROL                       
         MVC   LOGONAME,CONTNAME                                                
         MVC   LOGOADD,CONTADDR                                                 
         MVC   LOGO1,CONTLOGO                                                   
         MVC   LOGO2,SPACES                                                     
         MVC   LOGOADD2,SPACES                                                  
         MVC   LOGOADD3,SPACES                                                  
         MVC   LOGOJOB(5),SPACES                                                
*&&UK*&& MVC   LOGOJOB+0(3),=C'ACC'                                             
*&&UK*&& MVC   LOGOJOB+3(2),FILE   SHOW SYSTEM IN BIG LETTERS                   
         GOTOR LOGO,DMCB,LOGOD                                                  
         MVC   MID1,SPACES                                                      
         MVI   FORCEHED,YES                                                     
         MVC   PAGE,HONE                                                        
         MVC   LINTDESC(L'FILETOTS),FILETOTS                                    
         MVC   BOXCOLS(L'TOTLBOXC),TOTLBOXC                                     
         GOTOR PRTTOT,FILEDR                                                    
         MVI   BOXREQ,BOXCLOSE                                                  
         MVI   SPACING,3                                                        
         GOTOR ACREPORT                                                         
         MVI   BOXCOLS+(LINDR-LINED-1),C'-'                                     
         MVI   BOXCOLS+(LINBAL-LINED-1),BOXRIGHT                                
         MVI   BOXCOLS+(LINEND-LINED-1),C' '                                    
         MVI   BOXREQ,BOXOPEN                                                   
         GOTOR ACREPORT                                                         
                                                                                
         L     R2,ATOTTAB          PRINT FILE RECORD TOTALS                     
         USING TOTTABD,R2          R2=A(TOTALS TABLE)                           
TOTALS02 CLI   TOTTDISP,TOTTEOTQ   TEST EOT                                     
         BE    TOTALS06                                                         
         SR    RF,RF                                                            
         ICM   RF,3,TOTTDISP                                                    
         LA    RF,WORKD(RF)                                                     
         ICM   RF,15,0(RF)         RF=ACCUMULATOR VALUE                         
         BZ    TOTALS04                                                         
         ICM   RE,15,FILEADD                                                    
         AR    RE,RF                                                            
         TM    TOTTINDS,TOTTIADD   TEST ADD TO TOTAL RECORDS ADDED              
         BZ    *+8                                                              
         STCM  RE,15,FILEADD                                                    
         ICM   RE,15,FILECHA                                                    
         AR    RE,RF                                                            
         TM    TOTTINDS,TOTTICHA   TEST ADD TO TOTAL RECORDS CHANGED            
         BZ    *+8                                                              
         STCM  RE,15,FILECHA                                                    
         ICM   RE,15,FILEDEL                                                    
         AR    RE,RF                                                            
         TM    TOTTINDS,TOTTIDEL   TEST ADD TO TOTAL RECORDS DELETED            
         BZ    *+8                                                              
         STCM  RE,15,FILEDEL                                                    
         MVC   LINTDESC(L'TOTTDESC),TOTTDESC                                    
         CURED (RF),(LINNUML-1,LINCR),0                                         
         GOTOR ACREPORT                                                         
TOTALS04 AHI   R2,TOTTABL          BUMP TO NEXT TABLE ENTRY                     
         B     TOTALS02                                                         
         DROP  R2                                                               
                                                                                
TOTALS06 GOTOR ACREPORT            PRINT A SPACE LINE                           
         MVI   BOXREQ,BOXRULE                                                   
         GOTOR ACREPORT                                                         
                                                                                
         USING ISDTF,R2                                                         
         GOTOR DATAMGR,DMCB,DMDTF,ACCDIR,FULL,FULL                              
         L     R2,12(,R1)                                                       
                                                                                
         MVC   FILNAME(L'ISFFID),ISFFID                                         
         MVI   FILNAME+L'ISFFID,C' '                                            
         CLI   ISFFIDX,C' '                                                     
         BNH   *+10                                                             
         MVC   FILNAME+L'ISFFID(L'ISFFIDX),ISFFIDX                              
         MVC   HALF,ISOVLAST                                                    
                                                                                
         MVC   LINTDESC,FILNAME                                                 
         MVC   LINTDESC+L'FILNAME+1(L'OVFLTRKS),OVFLTRKS                        
         LH    RF,ISXTNTIX+12                                                   
         CURED (RF),(LINNUML-1,LINCR),0                                         
         GOTOR ACREPORT                                                         
                                                                                
         MVC   LINTDESC,FILNAME                                                 
         MVC   LINTDESC+L'FILNAME+1(L'TRKSUSED),TRKSUSED                        
         LH    RF,HALF                                                          
         CURED (RF),(LINNUML-1,LINCR),0                                         
         GOTOR ACREPORT                                                         
                                                                                
         MVC   LINTDESC,FILNAME                                                 
         MVC   LINTDESC+L'FILNAME+1(L'TRKSLEFT),TRKSLEFT                        
         LH    RF,ISXTNTIX+12                                                   
         SH    RF,HALF                                                          
         CURED (RF),(LINNUML-1,LINCR),0                                         
         GOTOR ACREPORT                                                         
         DROP  R2                                                               
                                                                                
         USING DTFPHD,R2                                                        
         GOTOR DATAMGR,DMCB,DMDTF,ACCMST,FULL,FULL                              
         L     R2,12(,R1)                                                       
         NILH  GR2,X'00FF'          A(DTF) 24 BIT ADDRESS                       
         MVC   FILNAME(L'DTFFID),DTFFID                                         
         MVI   FILNAME+L'DTFFID,C' '                                            
         CLI   DTFFIDX,C' '                                                     
         BNH   *+10                                                             
         MVC   FILNAME+L'DTFFID(L'DTFFIDX),DTFFIDX                              
         MVC   DADNEXT,DNEXT                                                    
         OC    DNEXT,DNEXT                                                      
         BNZ   *+10                                                             
         MVC   DADNEXT,=X'00010000'  Dummy up address                           
                                                                                
         GOTOR DATAMGR,DMCB,DMKEY,ACCMST,(3,DADATA),0,0                         
         L     RE,DAUTRKS          Total # of used tracks                       
         L     RF,DATTRKS          Total # of tracks                            
         SR    RF,RE               Total tracks minus tracks used               
         MVC   LINTDESC,FILNAME                                                 
         MVC   LINTDESC+L'FILNAME+1(L'TRKSLEFT),TRKSLEFT                        
         CURED (RF),(LINNUML-1,LINCR),0                                         
         GOTOR ACREPORT                                                         
         DROP  R2                                                               
                                                                                
         USING TOTTABD,R2          R2=A(TOTALS TABLE)                           
TOTALS08 L     R2,ARCVTAB          PRINT RECOVERY RECORD TOTALS                 
         SR    R0,R0               R0=TOTAL RECOVERY RECORDS                    
TOTALS10 CLI   TOTTDISP,TOTTEOTQ   TEST EOT                                     
         BE    TOTALS16                                                         
         TM    TOTTINDS,TOTTITOT   TEST TOTALS LINE                             
         BZ    *+14                                                             
         LTR   RF,R0                                                            
         BNZ   TOTALS12                                                         
         B     TOTALS16                                                         
         SR    RF,RF                                                            
         ICM   RF,3,TOTTDISP                                                    
         LA    RF,WORKD(RF)                                                     
         ICM   RF,15,0(RF)         RF=ACCUMULATOR VALUE                         
         BZ    TOTALS14                                                         
         AR    R0,RF               ADD TO RECOVERY FILE TOTALS                  
TOTALS12 MVC   LINTDESC(L'TOTTDESC),TOTTDESC                                    
         CURED (RF),(LINNUML-1,LINCR),0                                         
         GOTOR ACREPORT                                                         
TOTALS14 AHI   R2,TOTTABL          BUMP TO NEXT TABLE ENTRY                     
         B     TOTALS10                                                         
         DROP  R2                                                               
                                                                                
TOTALS16 MVI   BOXREQ,BOXCLOSE                                                  
         MVI   SPACING,3                                                        
         GOTOR ACREPORT                                                         
         MVI   BOXOFF,YES                                                       
         CLI   ERRSW,0             TEST ANY ERRORS FOUND                        
         BE    TOTALS38                                                         
         MVI   RCSUBPRG,1                                                       
         MVC   PAGE,HONE                                                        
         MVI   FORCEHED,YES                                                     
         MVI   LCPY,0                                                           
         ZAP   FILEDR,PZERO                                                     
         ZAP   FILECR,PZERO                                                     
         L     R2,AERR             CLOSE FILE & OPEN TO READ                    
         CLOSE ((2))                                                            
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         FREEPOOL (R2)                                                          
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         OPEN  ((2),INPUT)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
                                                                                
         MVC   SUBFNME,FILENME                                                  
         MVC   SUBJNME,MCJOB                                                    
         L     RF,MCSSB                                                         
         USING SSOOFFD,RF                                                       
         MVC   SUBENVT,=C'PROD'      DEFAULT ALLOCATED PROD                     
         CLI   SSODSPAC,C'A'                                                    
         BE    TOTALS17                                                         
         MVC   SUBENVT,=C'CSC '                                                 
         CLI   SSODSPAC,C'C'           IS IT CSC                                
         BE    TOTALS17                                                         
         MVC   SUBENVT,=C'FQA '                                                 
         CLI   SSODSPAC,C'Q'           IS IT FQA                                
         BE    TOTALS17                                                         
         MVC   SUBENVT,=C'TST '                                                 
         DROP  RF                                                               
*                                                                               
TOTALS17 L     RE,MCAEXTRA                                                      
         USING MCEXTRA,RE                                                       
         CLC   MC@EMAIL,SPACES          DID WE PASS EID = EMAIL ADDRESS         
         JNH   *+16                     NO -SEND EMAIL TO EXISTING USER         
         MVC   TOWHO,SPACES             YES -OVERRIDE EMAIL ADDRESSES           
         MVC   TOWHO,MC@EMAIL           WITH PASSED EID                         
         DROP  RE                                                               
*                                                                               
         GOTOR VSMTP,DMCB,('SMTPAINI',JESMAIL)                                  
         GOTOR VSMTP,DMCB,('SMTPAPRS',TOWHO),(L'SUBDESC,SUBDESC)                
         GOTOR VSMTP,DMCB,('SMTPAPTL',EHEAD)                                    
         GOTOR VSMTP,DMCB,('SMTPAPTL',EHEADU)                                   
         GOTOR VSMTP,DMCB,('SMTPAPTL',EHEAD2)                                   
         GOTOR VSMTP,DMCB,('SMTPAPTL',EHEADU2)                                  
         XC    EMLNUM,EMLNUM       START NUMBERING EMAILS                       
                                                                                
TOTALS18 L     R3,AIOIN            GET NEXT ERROR RECORD                        
         AHI   R3,4                                                             
         USING PSHEADD,R3                                                       
         GET   (2),(3)                                                          
         B     TOTALS22                                                         
                                                                                
TOTALS20 CLOSE ((2))               CLOSE ERROR FILE & PRINT CASH TOTALS         
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         FREEPOOL (R2)                                                          
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         GOTOR ACREPORT                                                         
         MVC   ERRLREFN(L'TOTS),TOTS                                            
         CURED (P8,FILEDR),(L'ERRLDR,ERRLDR),2,MINUS=YES                        
         CURED (P8,FILECR),(L'ERRLCR,ERRLCR),2,MINUS=YES                        
         GOTOR ACREPORT                                                         
         B     TOTALS36                                                         
                                                                                
TOTALS22 CLC   LCPY,PSHDACC        TEST CHANGE OF COMPANY                       
         BE    TOTALS24                                                         
         MVC   LCPY,PSHDACC        SET NEW COMPANY CODE                         
         MVC   PAGE,HONE           NEW PAGE ON COMPANY BREAK                    
         MVI   FORCEHED,YES                                                     
                                                                                
TOTALS24 XOUT  PSHDACC,ERRLCPY,1   COMPANY CODE                                 
         MVC   EDETAIL1,SPACES                                                  
         MVC   EDETAIL2,SPACES                                                  
         MVC   ECPYC,ERRLCPY                                                    
         MVC   ERRLULA,PSHDAUNT    UNIT/LEDGER/ACCOUNT                          
         MVC   EACCT,ERRLULA                                                    
         MVC   ERRLULC,PSHDSUNT    UNIT/LEDGER/CONTRA                           
         MVC   ECONT,ERRLULC                                                    
         L     R1,AERRTAB                                                       
*                                                                               
TOTALS26 CLI   0(R1),0             TEST EOT                                     
         BE    TOTALS28                                                         
         CLC   0(1,R1),PSHDEL      MATCH ON ERROR NUMBER                        
         BE    TOTALS28                                                         
         AHI   R1,L'ERRTAB         BUMP TO NEXT TABLE ENTRY                     
         B     TOTALS26                                                         
*                                                                               
TOTALS28 MVC   ERRLMESS,1(R1)      SET ERROR MESSAGE                            
         MVC   EMESS,ERRLMESS                                                   
         LA    R3,PSHEADL(R3)                                                   
         USING TRNELD,R3                                                        
         CLI   TRNEL,TRNELQ        COULD BE A BAD RECORD                        
         BNE   TOTALS29                                                         
         MVC   ERRLBTCH,TRNBTCH                                                 
         MVC   EBTCH,ERRLBTCH                                                   
         MVC   ERRLOFF,TRNOFFC                                                  
         MVC   EOFF,ERRLOFF                                                     
         MVC   ERRLREFN,TRNREF                                                  
         MVC   EREF,ERRLREFN                                                    
         GOTOR DATCON,PARM,(1,TRNDATE),(8,ERRLDATE)                             
         MVC   EDATE,ERRLDATE                                                   
         LA    RE,FILEDR                                                        
         LA    RF,ERRLDR                                                        
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+12                                                             
         LA    RE,FILECR                                                        
         LA    RF,ERRLCR                                                        
         AP    0(L'FILEDR,RE),TRNAMNT                                           
         CURED TRNAMNT,(L'ERRLDR,0(RF)),2,MINUS=YES                             
         LA    RF,EDRA                                                          
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+8                                                              
         LA    RF,ECRA                                                          
         CURED TRNAMNT,(L'ERRLDR,0(RF)),2,MINUS=YES                             
*                                                                               
TOTALS29 GOTOR ACREPORT            PRINT ERROR LINE                             
         CLI   EMLNUM,MAXNUM       HIGHER THAN MAX - NO MORE EMAILS             
         BH    TOTALS18                                                         
         BL    TOTALS32            LOWER, KEEP SENDING                          
         GOTOR VSMTP,DMCB,('SMTPAPTL',MAXDESC1)                                 
         GOTOR VSMTP,DMCB,('SMTPAPTL',MAXDESC2)                                 
         B     TOTALS34                                                         
*                                                                               
TOTALS32 GOTOR VSMTP,DMCB,('SMTPAPTL',EDETAIL1)                                 
         GOTOR VSMTP,DMCB,('SMTPAPTL',EDETAIL2)                                 
*                                                                               
TOTALS34 LH    R1,EMLNUM                                                        
         AHI   R1,2                                                             
         STH   R1,EMLNUM                                                        
         B     TOTALS18                                                         
                                                                                
TOTALS36 GOTOR VSMTP,DMCB,('SMTPASND',0)                                        
         GOTOR VSMTP,DMCB,('SMTPAEND',0) DETACH SMTP                            
                                                                                
TOTALS38 MVI   LOGOTYPE,LOGOLAST   PRINT END OF CONTROL LOGOS                   
         GOTOR LOGO,DMCB,LOGOD                                                  
                                                                                
TOTALSX  DS    0H                                                               
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE FILE HEADER RECORD WITH ACTIVITY DATES            *         
*                                                                     *         
* NTRY - R1=ZERO TO INITIALISE, NON ZERO TO SET SUCCESSFUL UPDATE     *         
***********************************************************************         
                                                                                
SETHDR   NTR1  ,                                                                
         STC   R1,WORK                                                          
         L     R2,AIOGEN                                                        
         USING ACCRECD,R2          READ FILE HEADER RECORD                      
         XC    ACCKEY,ACCKEY                                                    
         MVI   ACCKEY+L'ACCKEY-1,1                                              
         GOTOR DATAMGR,DMCB,DMREAD,ACCFIL,ACCRECD,ACCRECD                       
         BE    *+6                                                              
         DC    H'0'                CAN'T READ FILE HEADER RECORD                
                                                                                
         LA    R1,ACCRECD+ACCORFST                                              
         USING HDRELD,R1                                                        
         SR    R0,R0                                                            
SETHDR02 CLI   HDREL,0             TEST EOR                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   HDREL,HDRELQ        TEST ACCFIL FILE HEADER ELEMENT              
         BE    SETHDR04                                                         
         CLI   HDREL,FHDELQ        TEST ACCMST FILE HEADER ELEMENT              
         BE    SETHDR06                                                         
         IC    R0,HDRLN                                                         
         AR    R1,R0                                                            
         B     SETHDR02                                                         
                                                                                
         USING HDRELD,R1                                                        
SETHDR04 CLI   WORK,0              TEST INITIALISE/END                          
         BE    *+14                                                             
         MVC   HDRGOOD,TODAY2      AT END SET GOOD UPDATE DATE                  
         B     SETHDR08                                                         
         MVC   HDRUPDT,TODAY2      AT START SET UPDATE DATE                     
         XC    HDRGOOD,HDRGOOD     AND CLEAR GOOD UPDATE DATE                   
         B     SETHDR08                                                         
                                                                                
         USING FHDELD,R1                                                        
SETHDR06 CLI   WORK,0              TEST INITIALISE/END                          
         BE    *+14                                                             
         MVC   FHDGOOD,TODAY2      AT END SET GOOD UPDATE DATE                  
         B     SETHDR08                                                         
         MVC   FHDUPDT,TODAY2      AT START SET UPDATE DATE                     
         XC    FHDGOOD,FHDGOOD     AND CLEAR GOOD UPDATE DATE                   
                                                                                
SETHDR08 L     RF,DATAMGR                                                       
         GOTOR ,DMCB,DMWRT,ACCFIL,ACCRECD,ACCRECD                               
         TM    RUNINDS,RUNIDRFT    TEST WRITE ALLOWED                           
         BNZ   SETHDRX                                                          
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    SETHDRX                                                          
         DC    H'0'                                                             
                                                                                
SETHDRX  J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET NEXT INPUT RECORD AND MERGE TRANSACTIONS             *         
***********************************************************************         
                                                                                
GETIN    NTR1  ,                                                                
*&&US                                                                           
         CLI   FIRSTSW,0                                                        
         BNE   GETIN08                                                          
         MVI   FIRSTSW,1                                                        
*&&                                                                             
GETIN02  L     R2,AIN                                                           
         L     R3,AIOIN                                                         
         GET   (2),(3)                                                          
         OI    RUNINDS,RUNIREGL    SET INPUT RECORD(S) FOUND                    
         SR    RE,RE                                                            
         ICM   RE,3,0(R3)                                                       
         LA    RE,0(RE,R3)                                                      
         XC    0(2,RE),0(RE)                                                    
*&&UK*&& B     GETINX                                                           
*&&US                                                                           
         AHI   R3,4                                                             
         USING MPDRECD,R3                                                       
         CLI   MPDKTYP,MPDKTYPQ    TEST MEDIA POSTING DETAIL RECORD             
         BNE   GETIN10                                                          
         OI    RUNINDS,RUNISPCL    Set special input record(s) found            
         GOTOR DATAMGR,DMCB,(X'88',DMREAD),ACCFIL,MPDRECD,AIOGEN                
         BE    GETIN04                                                          
         TM    8(R1),X'02'         TEST RECORD FOUND & DELETED                  
         BNZ   GETIN04                                                          
         TM    8(R1),X'10'         TEST NOT FOUND                               
         BNZ   GETIN06                                                          
         ICM   R1,15,MPDRERR       INCREMENT NUMBER OF ERRORS                   
         AHI   R1,1                                                             
         STCM  R1,15,MPDRERR                                                    
         B     GETIN02                                                          
                                                                                
GETIN04  L     RF,DATAMGR                                                       
         GOTOR ,DMCB,DMWRT,ACCFIL,MPDRECD,MPDRECD                               
         TM    RUNINDS,RUNIDRFT+RUNITEST                                        
         BNZ   GETIN05                                                          
         BASR  RE,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
GETIN05  ICM   R1,15,MPDRCHA       INCREMENT NUMBER OF CHANGES                  
         AHI   R1,1                                                             
         STCM  R1,15,MPDRCHA                                                    
         B     GETIN02                                                          
                                                                                
GETIN06  L     RF,DATAMGR                                                       
         GOTOR ,DMCB,DMADD,ACCFIL,MPDRECD,MPDRECD                               
         TM    RUNINDS,RUNIDRFT+RUNITEST                                        
         BNZ   GETIN07                                                          
         BASR  RE,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
GETIN07  ICM   R1,15,MPDRADD       INCREMENT NUMBER OF ADDS                     
         AHI   R1,1                                                             
         STCM  R1,15,MPDRADD                                                    
         B     GETIN02                                                          
         DROP  R3                                                               
                                                                                
GETIN08  L     R0,AIOIN            MOVE SAVED RECORD FROM IOSV TO IOIN          
         L     RE,AIOSV                                                         
         LA    R1,IOLEN                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R3,AIOIN            TEST SAVED RECORD WAS EOF RECORD             
         AHI   R3,4                                                             
         USING PSHEADD,R3                                                       
         CLI   PSHDACC,FF                                                       
         BE    GETINX                                                           
                                                                                
GETIN10  L     R2,AIN                                                           
         L     R3,AIOSV            GET RECORD INTO IOSV                         
         OI    RUNINDS,RUNIREGL    Set regular input record(s) found            
         GET   (2),(3)                                                          
         SR    RE,RE                                                            
         ICM   RE,3,0(R3)                                                       
         LA    RE,0(RE,R3)                                                      
         XC    0(2,RE),0(RE)                                                    
         XC    AVALS(AVALSL),AVALS                                              
         L     R2,AIOIN                                                         
         L     R3,AIOSV                                                         
         CLC   0(6,R2),0(R3)       LENGTH OF RECORD AND FIRST ELEMENT           
         BNE   GETINX              NOT THE SAME - NO MERGE                      
         AHI   R2,4                                                             
         AHI   R3,4                                                             
                                                                                
GETIN12  LLC   R1,1(R2)            LENGTH OF ELEMENT                            
         BCTR  R1,0                                                             
         EX    R1,*+8              MATCH CURRENT ELEMENT TO NEXT                
         B     *+10                                                             
         CLC   0(0,R2),0(R3)                                                    
         BNE   GETINX                                                           
                                                                                
GETIN14  LLC   R1,1(R2)            BUMP TO NEXT ELEMENTS                        
         AR    R2,R1               R2 TO NEXT IN CURRENT RECORD                 
         AR    R3,R1               R3 TO NEXT IN SAVE RECORD                    
         CLC   0(1,R2),0(R3)       TEST SAME ELEMENT ON BOTH RECORDS            
         BNE   GETINX                                                           
         CLI   0(R2),0             TEST END OF RECORDS                          
         BE    GETIN20                                                          
         USING TRNELD,R2                                                        
         CLI   TRNEL,TRNELQ        TEST TRANSACTION ELEMENT                     
         BNE   GETIN16                                                          
         CLI   TRNTYPE,6           ONLY FOR PROD BILL TRANSACTIONS              
         BNE   GETINX                                                           
         CLC   TRNANAL,BILLANAL    BUT NOT FOR THE BILLS                        
         BE    GETINX                                                           
         ST    R2,ACTRN            A(CURRENT 44 ELEMENT)                        
         ST    R3,ANTRN            A(NEXT 44)                                   
         CLC   TRNELD(TRNAMNT-TRNELD),0(R3)                                     
         BNE   GETINX                                                           
         LLC   R1,TRNLN                                                         
         SHI   R1,TRNANAL+1-TRNELD                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TRNANAL(0),TRNANAL-TRNELD(R3)                                    
         BE    GETIN14                                                          
         B     GETINX                                                           
                                                                                
         USING SCIELD,R2                                                        
GETIN16  CLI   SCIEL,SCIELQ        TEST SUBSIDIARY CASH ELEMENT                 
         BNE   GETIN18                                                          
         ST    R2,ACSUB            A(CURRENT 50 ELEMENT)                        
         ST    R3,ANSUB            A(NEXT 50)                                   
         CLC   SCIELD(SCIAMNT-SCIELD),0(R3)                                     
         BE    GETIN14                                                          
         B     GETINX                                                           
                                                                                
         USING MDTELD,R2                                                        
GETIN18  CLI   MDTEL,MDTELQ        TEST MEDIA TRANSFER ELEMENT                  
         BNE   GETIN12                                                          
         ST    R2,ACMED            A(CURRENT 1A ELEMENT)                        
         ST    R3,ANMED            A(NEXT 1A ELEMENT)                           
         CLC   MDTELD(MDTLNQ),0(R3)                                             
         BE    GETIN14                                                          
         B     GETINX                                                           
                                                                                
GETIN20  ICM   R2,15,ACTRN         ADD TRANSACTION AMOUNTS                      
         BZ    GETINX                                                           
         USING TRNELD,R2                                                        
         L     R3,ANTRN                                                         
         AP    TRNAMNT,TRNAMNT-TRNELD(L'TRNAMNT,R3)                             
                                                                                
         ICM   R2,15,ACSUB         ADD SUBSIDIARY CASH                          
         BZ    GETIN22                                                          
         L     R3,ANSUB                                                         
         USING SCIELD,R2                                                        
         AP    SCIAMNT,SCIAMNT-SCIELD(L'SCIAMNT,R3)                             
         CLI   SCILN,SCILN1Q                                                    
         BE    GETIN22                                                          
         AP    SCIADMN,SCIADMN-SCIELD(L'SCIADMN,R3)                             
                                                                                
GETIN22  ICM   R2,15,ACMED         ADD MEDAU TRANSFER                           
         BZ    GETIN24                                                          
         L     R3,ANMED                                                         
         USING MDTELD,R2                                                        
                                                                                
         ICM   R1,15,MDTGRS                                                     
         A     R1,MDTGRS-MDTELD(R3)                                             
         STCM  R1,15,MDTGRS                                                     
                                                                                
         ICM   R1,15,MDTNET                                                     
         A     R1,MDTNET-MDTELD(R3)                                             
         STCM  R1,15,MDTNET                                                     
                                                                                
         ICM   R1,15,MDTCOM                                                     
         A     R1,MDTCOM-MDTELD(R3)                                             
         STCM  R1,15,MDTCOM                                                     
                                                                                
         ICM   R1,15,MDTCD                                                      
         A     R1,MDTCD-MDTELD(R3)                                              
         STCM  R1,15,MDTCD                                                      
                                                                                
         ICM   R1,15,MDTINTL                                                    
         A     R1,MDTINTL-MDTELD(R3)                                            
         STCM  R1,15,MDTINTL                                                    
                                                                                
         ICM   R1,15,MDTRECV                                                    
         A     R1,MDTRECV-MDTELD(R3)                                            
         STCM  R1,15,MDTRECV                                                    
                                                                                
         ICM   R1,15,MDTVAT                                                     
         A     R1,MDTVAT-MDTELD(R3)                                             
         STCM  R1,15,MDTVAT                                                     
                                                                                
GETIN24  ICM   R1,15,FILEMRG       INCREMENT NUMBER OF MERGED RECORDS           
         AHI   R1,1                                                             
         STCM  R1,15,FILEMRG                                                    
         B     GETIN10             GET NEXT INPUT RECORD                        
*&&                                                                             
GETIN26  L     R2,AIN              CLOSE INPUT FILE                             
         CLOSE ((2))                                                            
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         FREEPOOL (R2)                                                          
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*&&UK*&& L     R3,AIOIN            DUMMY-UP END OF FILE RECORD                  
*&&US                                                                           
         L     R3,AIOSV            DUMMY-UP END OF FILE RECORD                  
         TM    RUNINDS,RUNIREGL    IF NO RECORDS WERE READ                      
         BNZ   *+8                                                              
         L     R3,AIOIN            USE THE MERGED RECORD AREA                   
*&&                                                                             
         AHI   R3,4                                                             
         USING PSHEADD,R3                                                       
         MVI   PSHDEL,PSHDELQ                                                   
         MVI   PSHDLEN,PSHEADL                                                  
         MVI   PSHDACC,FF                                                       
         MVC   PSHDACC+1(L'PSHDACC-1),PSHDACC                                   
                                                                                
GETINX   J     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT TOTALS AT ANY LEVEL                                *         
*                                                                     *         
* NTRY - R1=A(TWO 8 BYTE PACKED ACCUMULATORS)                         *         
***********************************************************************         
                                                                                
PRTTOT   NTR1  ,                                                                
         ZAP   EDIT1,0(8,R1)                                                    
         ZAP   EDIT2,8(8,R1)                                                    
         ZAP   EDIT3,0(8,R1)                                                    
         SP    EDIT3,8(8,R1)                                                    
         ZAP   0(8,R1),PZERO                                                    
         ZAP   8(8,R1),PZERO                                                    
                                                                                
         LA    R2,EDIT1                                                         
         LA    R3,LINDR                                                         
         LHI   R0,3                                                             
PRTTOT02 CP    0(L'EDIT1,R2),PZERO                                              
         BE    PRTTOT06                                                         
         CP    0(L'EDIT1,R2),=P'-9999999999'                                    
         BL    *+14                                                             
         CP    0(L'EDIT1,R2),=P'9999999999'                                     
         BNH   PRTTOT04                                                         
         CURED (P8,0(R2)),(LINNUML,0(R3)),2,MINUS=YES                           
         B     PRTTOT06                                                         
PRTTOT04 CURED (P8,0(R2)),(LINNUML,0(R3)),2,COMMAS=YES,MINUS=YES                
PRTTOT06 AHI   R2,L'EDIT1                                                       
         AHI   R3,LINNUML+1                                                     
         BCT   R0,PRTTOT02                                                      
                                                                                
         CLI   RCSUBPRG,0                                                       
         BH    PRTTOTX                                                          
         GOTOR ACREPORT                                                         
                                                                                
PRTTOTX  J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT ADDTRN TRACE                                       *         
***********************************************************************         
                                                                                
IOHOOK   ST    RE,SAVERE                                                        
         LR    R2,R0               R2=A(ADDTRN DMCB)                            
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVC   P+17(4),=C'RET='    SET DMCB+8 VALUE                             
         XOUT  8(R2),P+21,1                                                     
         LM    RE,RF,0(R2)         ACCOUNT COMMANDS                             
         MVC   P+01(7),0(RE)       SET COMMAND                                  
         MVC   P+09(7),0(RF)       SET FILE                                     
                                                                                
         CLC   0(5,RF),ACCFIL      TEST ACCOUNT FILE                            
         BE    IOHFIL                                                           
         CLC   0(5,RF),ACCMST      TEST ACCMST D/A FILE                         
         BE    IOHMST                                                           
         CLC   0(5,RF),ACCDIR      TEST ACCDIR I/S FILE                         
         BE    IOHDIR                                                           
         B     IOHOOKX                                                          
                                                                                
IOHFIL   L     R3,12(R2)           ACCOUNT COMMANDS                             
         MVC   P+24(ACCORFST),0(R3)                                             
         TR    P+24(ACCORFST),CHRTAB                                            
         LHI   R0,ACCORFST                                                      
         GOTOR HEXOUT,PARM,(R3),WORK2,(R0),HEXSEP                               
         MVC   PSECOND+24(ACCORFST),WORK2                                       
         MVC   PTHIRD+24(ACCORFST),WORK2+(ACCORFST)                             
         SR    R0,R0                                                            
         ICM   R0,3,ACCRLEN-ACCRECD(R3)                                         
         SHI   R0,ACCORFST                                                      
         AHI   R3,ACCORFST                                                      
         B     IOHPRT                                                           
                                                                                
IOHMST   L     R3,8(R2)            ACCMST COMMANDS                              
         MVC   PSECOND+09(4),=C'D/A='                                           
         GOTOR HEXOUT,PARM,(R3),PSECOND+13,4,HEXTOG                             
         L     R3,12(R2)                                                        
         MVC   P+24(ACCRFST-ACCRECD),0(R3)                                      
         TR    P+24(ACCRFST-ACCRECD),CHRTAB                                     
         LHI   R0,ACCRFST-ACCRECD                                               
         GOTOR HEXOUT,PARM,(R3),WORK2,(R0),HEXSEP                               
         MVC   PSECOND+24(ACCRFST-ACCRECD),WORK2                                
         MVC   PTHIRD+24(ACCRFST-ACCRECD),WORK2+(ACCRFST-ACCRECD)               
         SR    R0,R0                                                            
         ICM   R0,3,ACCRLEN-ACCRECD(R3)                                         
         SHI   R0,ACCRFST-ACCRECD                                               
         AHI   R3,ACCRFST-ACCRECD                                               
         B     IOHPRT                                                           
                                                                                
IOHDIR   L     R3,8(R2)            ACCDIR COMMANDS                              
         MVC   P+24(ACCKLEN),0(R3)                                              
         TR    P+24(ACCKLEN),CHRTAB                                             
         LHI   R0,ACCKLEN                                                       
         GOTOR HEXOUT,PARM,(R3),WORK2,(R0),HEXSEP                               
         MVC   PSECOND+24(ACCKLEN),WORK2                                        
         MVC   PTHIRD+24(ACCKLEN),WORK2+(ACCKLEN)                               
         SR    R0,R0                                                            
                                                                                
IOHPRT   MVI   SKIPSPEC,YES        PRINT KEY                                    
         MVI   SPACING,2                                                        
         GOTOR ACREPORT                                                         
         LTR   R0,R0                                                            
         BZ    IOHOOKX                                                          
                                                                                
IOHPRT2  LHI   R2,100              PRINT RECORD DATA                            
         CR    R0,R2                                                            
         BH    *+6                                                              
         LR    R2,R0                                                            
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P+24(0),0(R3)                                                    
         EX    R2,*+8                                                           
         B     *+10                                                             
         TR    P+24(0),CHRTAB                                                   
         GOTOR HEXOUT,PARM,(R3),WORK2,1(R2),HEXSEP                              
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   PSECOND+24(0),WORK2                                              
         LA    R1,WORK2+1(R2)                                                   
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   PTHIRD+24(0),0(R1)                                               
         MVI   SKIPSPEC,YES                                                     
         MVI   SPACING,2                                                        
         GOTOR ACREPORT                                                         
         AHI   R2,1                                                             
         AR    R3,R2                                                            
         SR    R0,R2                                                            
         BP    IOHPRT2                                                          
                                                                                
IOHOOKX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD AN ERROR RECORD AND ADD TO ERROR FILE              *         
*                                                                     *         
* NTRY - R1=ERROR NUMBER                                              *         
***********************************************************************         
                                                                                
PUTERR   NTR1  ,                                                                
         STC   R1,WORK             SAVE ERROR NUMBER                            
         L     R2,AERR                                                          
         CLI   ERRSW,0             TEST OUTPUT FILE OPEN                        
         BNE   PUTERR02                                                         
         OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         MVI   ERRSW,1             SET OUTPUT FILE IS OPEN                      
                                                                                
PUTERR02 L     R3,AIOIN            PUT RECORD TO ERROR FILE                     
         AHI   R3,4                                                             
         MVC   WORK+1(1),0(R3)                                                  
         MVC   0(1,R3),WORK        SET ERROR NUMBER IN RECORD                   
         PUT   (2),(3)                                                          
         MVC   0(1,R3),WORK+1                                                   
                                                                                
         CLI   WORK,COMPINVQ       TEST INVALID COMPANY                         
         BE    PUTERRX                                                          
         TM    WKFIND,WKFNOPST     DON'T PUT TO WORKER FILE                     
         BO    PUTERRX                                                          
         TM    COMPSTAT,COMPERRS   TEST FIRST TIME                              
         BNZ   PUTERR04                                                         
         MVC   ERRUSER,COMPUSER                                                 
         XC    ERRREST,ERRREST                                                  
         ZAP   ERRCT,PZERO                                                      
         TM    RUNINDS,RUNITEST+RUNIDRFT                                        
         BZ    *+8                                                              
         MVI   ERRFLAG,ERRFDUPS                                                 
         L     R1,AERRIO                                                        
         XC    0(256,R1),0(R1)                                                  
         GOTOR DATAMGR,DMCB,WKOPEN,WKFILE,ERRID,AERRIO,AERRBUFF                 
         BNE   PUTERRX                                                          
         OI    COMPSTAT,COMPERRS   SET ERROR FILE ADDED FOR COMPANY             
                                                                                
PUTERR04 GOTOR DATAMGR,DMCB,WKADDR,WKFILE,ERRID,AIOIN,AERRBUFF                  
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    ERRCT,PONE                                                       
                                                                                
PUTERRX  NI    WKFIND,FF-WKFNOPST  RESET FLAG                                   
         J     XIT                                                              
         EJECT                                                                  
XIT      XIT1  ,                                                                
                                                                                
***********************************************************************         
* PUT JOB PAYMENTS DATA INTO OUTPUT FILE                              *         
***********************************************************************         
                                                                                
         USING JARAYD,R2                                                        
PUTJOB   NTR1                                                                   
                                                                                
         USING MPYELD,R4                                                        
         LA    R2,WORK                                                          
         XC    WORK(JARAYLQ),WORK      CLEAR WORK AREA BEFORE BUILDING          
                                                                                
         CLI   PJCPY,0             Company - first one?                         
         JNE   PUTJ02                                                           
         MVC   PJCPY,LCPY                                                       
         J     PUTJ04                                                           
                                                                                
PUTJ02   CLC   PJCPY,LCPY          Company - change?                            
         JE    PUTJ06                                                           
         MVC   PJCPY,LCPY                                                       
                                                                                
PUTJ04   MVC   JARDFIL,PJCPY       ADD COMPANY HEADER ENTRY                     
         LHI   RE,-1                                                            
         STCM  RE,B'1111',JARDPDA                                               
                                                                                
         L     R3,AP2JOUT          PUT OUT                                      
         PUT   (3),(2)                                                          
                                                                                
PUTJ06   MVI   JARDFIL,JARDFMQ     ADD ENTRY                                    
         MVC   JARDPDA,TRNTRXDA                                                 
         XC    JARDERR,JARDERR                                                  
         MVC   JARDATE,MPYDTE                                                   
         MVC   JARPAYR,MPYNO                                                    
                                                                                
         L     R3,AP2JOUT          PUT OUT ENTRY                                
         PUT   (3),(2)                                                          
                                                                                
         J     XIT                                                              
         DROP  R2,R4                                                            
                                                                                
         LTORG                                                                  
                                                                                
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
ACCSYSQ  EQU   C'A'                                                             
LOGOFRST EQU   C'S'                                                             
LOGOLAST EQU   C'E'                                                             
BOXOPEN  EQU   C'O'                                                             
BOXCLOSE EQU   C'C'                                                             
BOXRULE  EQU   C'B'                                                             
BOXTOP   EQU   C'T'                                                             
BOXMID   EQU   C'M'                                                             
BOXBOT   EQU   C'B'                                                             
BOXLEFT  EQU   C'L'                                                             
BOXCOLM  EQU   C'C'                                                             
BOXRIGHT EQU   C'R'                                                             
                                                                                
*SB      DC    H'0'                FOR GESRCHEXEC TO INDICATE OFFLINE           
         EJECT                                                                  
AIOIN    DC    A(IOIN)             A(I/O AREA FOR IN DCB)                       
AIOSV    DC    A(IOSV)             A(I/O AREA FOR IN DCB)                       
AIOGEN   DC    A(IOGEN)            A(I/O AREA FOR GENERAL READING)              
AIOACC   DC    A(IOACC)            A(I/O AREA FOR ACCOUNTS)                     
AIOAOFA  DC    A(IOOFA)            A(I/O AREA FOR OFFICE/ACCOUNTS)              
AIOBUK   DC    A(IOBUK)            A(I/O AREA FOR BUCKETS)                      
AIOCAC   DC    A(IOCAC)            A(I/O AREA FOR CONTRA-ACCOUNTS)              
AIOTRN   DC    A(IOTRN)            A(I/O AREA FOR TRANSACTIONS)                 
AIN      DC    A(IN)               A(DCB FOR INPUT FILE)                        
AP2JOUT  DC    A(P2JOUT)           A(DCB FOR P2J OUTPUT FILE)                   
AERR     DC    A(SORTWK1)          A(ERROR FILE)                                
AERRIO   DC    A(ERRIO)            A(ERROR RECORD)                              
AERRTAB  DC    A(ERRTAB)           A(ERROR TABLE)                               
AERRBUFF DC    A(ERRBUFF)          A(ERROR BUFFER FOR WORKER)                   
AERR0    DC    A(SORTWK01)         A(ERROR FILE)                                
                                                                                
VGETLOGO DC    V(GETLOGO)                                                       
ATOTTAB  DC    A(TOTTAB)                                                        
ARCVTAB  DC    A(RCVTAB)                                                        
VTMSUPD  DC    V(TMSUPD)                                                        
                                                                                
PHASES   DS    0AL1                ** LOADED PHASES **                          
         DC    AL1(QADDTRN)                                                     
PHASEN   EQU   (*-PHASES)/L'PHASES                                              
                                                                                
PRODUL   DC    C'SJ'                                                            
CCSTUL   DC    C'1C'                                                            
PERSUL   DC    C'1R'                                                            
                                                                                
BILLANAL DC    C'99'                                                            
HONE     DC    H'1'                                                             
PONE     DC    P'1'                                                             
PZERO    DC    P'0'                                                             
                                                                                
ACCFIL   DC    C'ACCOUNT'                                                       
ACCDIR   DC    C'ACCDIR '                                                       
ACCMST   DC    C'ACCMST '                                                       
CTFILE   DC    C'CTFILE '                                                       
WKFILE   DC    C'WKFILE '                                                       
DMDTF    DC    C'DTFAD  '                                                       
                                                                                
WKOPEN   DC    C'OPEN   '                                                       
WKREAD   DC    C'READ   '                                                       
WKADDR   DC    C'ADD    '                                                       
WKCLOS   DC    C'CLOSE  '                                                       
WKPRGE   DC    C'PURGE  '                                                       
                                                                                
HEXSEP   DC    C'SEP'                                                           
HEXTOG   DC    C'TOG'                                                           
                                                                                
DDNAME   DC    CL8'DDNAME'                              NMALIK                  
                                                                                
BC01     DC    C'BC01'                                                          
PRTCLOSE DC    C'CLOSE'                                                         
                                                                                
REMLIT   DC    C'REMOTE'                                                        
REPLIT   DC    C'**AUTO**'                                                      
                                                                                
FILETOTS DC    C'File totals'                                                   
OVFLTRKS DC    C'Overflow tracks'                                               
TRKSUSED DC    C'Tracks used'                                                   
TRKSLEFT DC    C'Tracks left'                                                   
TOTS     DC    C'Totals'                                                        
                                                                                
CONTNAME DC    CL(L'LOGONAME)'******* INTERNAL CONTROL *******'                 
CONTADDR DC    CL(L'LOGOADD)'******* DO NOT  SEND OUT *******'                  
CONTLOGO DC    CL(L'LOGO1)'CONTROL'                                             
                                                                                
ERRID    DS    0XL16               ** OUTPUT WORKER FILE KEY **                 
ERRUSER  DS    XL2                 USER-ID NUMBER                               
ERRSYST  DC    AL1(ACCSYSQ)        SYSTEM ID                                    
ERRPROG  DC    C'PE'               PROGRAM CODE (POSTING ERROR)                 
         DC    X'00'               N/D                                          
ERRPDAY  DS    PL1                 DAY (PWOS)                                   
ERRTYPE  DC    C'P'                POSTING FILE                                 
ERRREST  DS    0XL8                                                             
         DC    XL5'00'             N/D                                          
ERRFLAG  DS    X                   FLAG BYTE                                    
ERRFDUPS EQU   X'01'               ALLOW DUPLICATE FILE NAMES                   
         DC    XL2'00'             N/D                                          
                                                                                
JESMAIL  DC    CL8'JESMAIL '                                                    
TOWHO    DC    CL48'NA-OOB_TEAM:'                                               
*                                                                               
SUBDESC  DS    0CL80                                                            
         DC    CL29'135 MISSING POSTING REPORT - '                              
SUBFNME  DS    CL5               ACC FILE NAME ACC1, ACCI1, ACC2 ETC..          
         DC    CL3' - '                                                         
SUBJNME  DS    CL8               Job name                                       
         DC    CL3' - '                                                         
SUBENVT  DS    CL4               Environment CSC - PROD - TST - FQA             
         DC    CL28' '                                                          
*                                                                               
MAXDESC1 DC    CL80'MAXIMUM NUMBER OF EMAILS EXCEEDED'                          
MAXDESC2 DC    CL80'CHECK REPORT OF BALANCE OF ERRORS'                          
MAXNUM   EQU   190                                                              
EMLNUM   DS    H                                                                
*                                                                               
EHEAD    DS    0CL80                                                            
         DC    CL2'CC'                                                          
         DC    C' '                                                             
         DC    CL14'ACCOUNT'                                                    
         DC    C' '                                                             
         DC    CL8'  DATE  '                                                    
         DC    C' '                                                             
         DC    CL6'REF#  '                                                      
         DC    C' '                                                             
         DC    CL14'  DEBIT AMOUNT'                                             
         DC    C' '                                                             
         DC    CL14' CREDIT AMOUNT'                                             
         DC    C' '                                                             
         DC    CL14'CONTRA ACCOUNT'                                             
         DS    CL(L'EHEAD-(*-EHEAD))' '                                         
*                                                                               
EHEADU   DS    0CL80                                                            
         DC    CL2'--'                                                          
         DC    C' '                                                             
         DC    CL14'--------------'                                             
         DC    C' '                                                             
         DC    CL8'--------'                                                    
         DC    C' '                                                             
         DC    CL6'------'                                                      
         DC    C' '                                                             
         DC    CL14'--------------'                                             
         DC    C' '                                                             
         DC    CL14'--------------'                                             
         DC    C' '                                                             
         DC    CL14'--------------'                                             
         DC    CL(L'EHEADU-(*-EHEADU))' '                                       
*                                                                               
EHEAD2   DS    0CL80                                                            
         DC    CL50' '                                                          
         DC    CL2'OF'                                                          
         DC    C' '                                                             
         DC    CL6'BATCH '                                                      
         DC    C' '                                                             
         DC    CL20'MESSAGE'                                                    
*                                                                               
EHEADU2  DS    0CL80                                                            
         DC    CL50' '                                                          
         DC    CL2'--'                                                          
         DC    C' '                                                             
         DC    CL6'------'                                                      
         DC    C' '                                                             
         DC    CL20'--------------------'                                       
*                                                                               
EDETAIL1 DS    0CL80                                                            
ECPYC    DS    CL(L'ERRLCPY)                                                    
         DS    C                                                                
EACCT    DS    CL(L'ERRLULA)                                                    
         DS    C                                                                
EDATE    DS    CL(L'ERRLDATE)                                                   
         DS    C                                                                
EREF     DS    CL(L'ERRLREFN)                                                   
         DS    C                                                                
EDRA     DS    CL(L'ERRLDR)                                                     
         DS    C                                                                
ECRA     DS    CL(L'ERRLCR)                                                     
         DS    C                                                                
ECONT    DS    CL(L'ERRLULC)                                                    
         DS    CL(L'EDETAIL1-(*-EDETAIL1))                                      
*                                                                               
EDETAIL2 DS    0CL80                                                            
         DS    CL50                                                             
EOFF     DS    CL(L'ERRLOFF)                                                    
         DS    C                                                                
EBTCH    DS    CL(L'ERRLBTCH)                                                   
         DS    C                                                                
EMESS    DS    CL(L'ERRLMESS)                                                   
*                                                                               
CHRTAB   DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 00-0F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 10-1F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 20-2F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 30-3F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 40-4F                     
         DC    XL16'504B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 50-5F                     
         DC    XL16'60614B4B4B4B4B4B4B4B4B4B4B4B4B4B' 60-6F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 70-7F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 80-8F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 90-9F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' A0-AF                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' B0-BF                     
         DC    XL16'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B' C0-CF                     
         DC    XL16'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B' D0-DF                     
         DC    XL16'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B' E0-EF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B' F0-FF                     
                                                                                
UNITBOXC DS    0CL(L'P)            BOXCOLS FOR UNIT BOX                         
         DC    AL1(BOXLEFT),(LINLEDG-LINUNIT-1)C'-'                             
         DC    AL1(BOXCOLM),(LINDR-LINLEDG-1)C'-'                               
         DC    AL1(BOXCOLM),(LINCR-LINDR-1)C'-'                                 
         DC    AL1(BOXCOLM),(LINBAL-LINCR-1)C'-'                                
         DC    AL1(BOXCOLM),(LINEND-LINBAL-1)C'-',AL1(BOXRIGHT)                 
         DC    (L'P-(*-UNITBOXC))C' '                                           
                                                                                
TOTLBOXC DS    0CL(L'P)            BOXCOLS FOR TOTALS BOX                       
         DC    (LINTDESC-LINUNIT)C' '                                           
         DC    AL1(BOXLEFT),(LINDR-LINTDESC-1)C'-'                              
         DC    AL1(BOXCOLM),(LINCR-LINDR-1)C'-'                                 
         DC    AL1(BOXCOLM),(LINBAL-LINCR-1)C'-'                                
         DC    AL1(BOXCOLM),(LINEND-LINBAL-1)C'-',AL1(BOXRIGHT)                 
         DC    (L'P-(*-TOTLBOXC))C' '                                           
                                                                                
CHGBOXLN DS    0CL(L'P)            PRINT LINE FOR CHANGE OF BOX                 
         DC    AL1(BOTL),(LINLEDG-LINUNIT-1)AL1(BOTF)                           
         DC    AL1(BOTT),(LINTDESC-LINLEDG-1)AL1(BOTF)                          
         DC    AL1(TOPT),(LINDR-LINTDESC-1)AL1(BOTF)                            
         DC    AL1(CROSS),(LINCR-LINDR-1)AL1(BOTF)                              
         DC    AL1(CROSS),(LINBAL-LINCR-1)AL1(BOTF)                             
         DC    AL1(CROSS),(LINEND-LINBAL-1)AL1(BOTF),AL1(RIGHTT)                
         DC    (L'P-(*-CHGBOXLN))C' '                                           
                                                                                
DICI     DCDDL AC#UNTTO,L'LINTDESC-2,L                                          
         DCDDL AC#CPYTS,L'LINTDESC-2,L                                          
         DCDDL AC#CPY,7,L                                                       
         DCDDL AC#ITEM,8,L                                                      
         DCDDL AC#ITEMS,8,L                                                     
         DCDDL AC#TOTLS,L'WKLCAC,L                                              
         DCDDL AC#PGAPE,L'PSSBDESC,L                                            
DICIX    DC    X'00'                                                            
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'**SSB **'                                                    
SSB      CSECT                                                                  
         DC    X'0000FF',X'00',XL252'00'                                        
*                                                                               
WORKD    DSECT                                                                  
                                                                                
PARM     DS    6F                                                               
WORK2    DS    XL256                                                            
FILNAME  DS    CL(L'ISFFID+L'ISFFIDX)                                           
*                                                                               
DADATA   DS    0F                                                               
DADNEXT  DS    F                   DNEXT                                        
DAUTRKS  DS    F                   Used tracks                                  
DATTRKS  DS    F                   Total tracks                                 
                                                                                
AVALS    DS    0A                                                               
ACTRN    DS    A                   A(CURRENT TRANSACTION ELEMENT)               
ANTRN    DS    A                   A(NEXT TRANSACTION ELEMENT)                  
ACSUB    DS    A                   A(CURRENT SUBSIDIARY CASH ELEMENT)           
ANSUB    DS    A                   A(NEXT SUBSIDIARY CASH ELEMENT)              
ACMED    DS    A                   A(CURRENT MEDIA TRANSFER ELEMENT)            
ANMED    DS    A                   A(NEXT MEDIA TRANSFER ELEMENT)               
AVALSL   EQU   *-AVALS                                                          
                                                                                
APHASES  DS    0A                  ** ADDRESSES OF LOADED PHASES **             
VADDTRN  DS    A                                                                
                                                                                
SAVERE   DS    A                   SAVED RE VALUE                               
FIRSTSW  DS    X                   FIRST TIME SWITCH                            
                                                                                
WKFIND   DS    X                   WORKER FILE (APE) INDICATOR                  
WKFNOPST EQU   X'80'               DO NOT POST THIS TO WORKER FILE              
                                                                                
ERRSW    DS    X                   ERROR SWITCH                                 
ERRDR    DS    PL8                 TOTAL DEBIT ERRORS                           
ERRCR    DS    PL8                 TOTAL CREDIT ERRORS                          
ERRCT    DS    PL8                 TOTAL NUMBER OF ERRORS                       
                                                                                
TODAY2   DS    XL2                 TODAY'S DATE COMPRESSED                      
TODAY1   DS    0PL3                TODAY'S DATE PACKED                          
TODAY1YY DS    PL1                 YEAR                                         
TODAY1MM DS    PL1                 MONTH                                        
TODAY1DD DS    PL1                 DAY                                          
                                                                                
RUNINDS  DS    X                   RUN CONTROL INDICATORS                       
RUNINOLO EQU   X'80'               NOLOGOS                                      
RUNIREMO EQU   X'40'               REMOTE COMPANY                               
RUNITEST EQU   X'20'               TEST=YES                                     
RUNIDRFT EQU   X'10'               WRITE=NO                                     
RUNISPCL EQU   X'01'               Specail postings exist                       
RUNIREGL EQU   X'02'               Regular postings exist                       
                                                                                
EDIT1    DS    PL8                 USED BY PRTTOT S/R                           
EDIT2    DS    PL8                 USED BY PRTTOT S/R                           
EDIT3    DS    PL8                 USED BY PRTTOT S/R                           
                                                                                
RECVCPY  DS    F                   RECOVERY COPIES                              
RECVCHA  DS    F                   RECOVERY CHANGES                             
RECVADD  DS    F                   RECOVERY ADDS                                
RECVOTH  DS    F                   RECOVERY OTHERS                              
                                                                                
FILE     DS    CL2                 FILE CODE                                    
FILEDR   DS    PL8                 FILE DEBITS                                  
FILECR   DS    PL8                 FILE CREDITS                                 
FILEADD  DS    XL4                 FILE RECORDS ADDED                           
FILECHA  DS    XL4                 FILE RECORDS CHANGED                         
FILEMRG  DS    XL4                 FILE RECORDS MERGED                          
FILEDEL  DS    XL4                 FILE RECORDS DELETED                         
MPDRADD  DS    XL4                 BILLING TRANSFERS ADDED                      
MPDRCHA  DS    XL4                 BILLING TRANSFERS CHANGED                    
MPDRERR  DS    XL4                 BILLING TRANSFERS IN ERROR                   
PORDCLO  DS    XL4                 PURCHASE ORDERS CLOSED                       
TRNTDEL  DS    XL4                 DELETED TRANSACTIONS                         
PJCPY    DS    XL1                 PUTJOB COMPANY CODE                          
DSNP2J   DS    CL44                PUTJOB DSN NAME    NMALIK                    
TRFSE    DS    CL5                 PARAMETER FOR DDNAME  NMALIK                 
                                                                                
LKEY     DS    0X                  ** LAST TIME KEY **                          
LCPY     DS    XL(L'CPYKCPY)       LAST COMPANY CODE                            
LUNT     DS    CL(L'UNTKUNT)       LAST UNIT CODE                               
LLDG     DS    CL(L'LDGKLDG)       LAST LEDGER CODE                             
LCPYSV   DS    XL(L'CPYKCPY)       SAVED LAST COMPANY CODE                      
                                                                                
COMPINVQ EQU   255                 COMPANY NOT FOUND OR INVALID                 
COMPSTAT DS    X                   COMPANY RECORD STATUS                        
COMPNOTF EQU   X'80'               COMPANY NOT ON FILE                          
COMPERRS EQU   X'40'               POSTING ERROR WORKER FILE ADDED              
COMPSTAD DS    XL(L'CPYSTATD)      COMPANY STATUS BYTE 13                       
COMPALPH DS    CL(L'CPYALPHA)      COMPANY ALPHA-ID                             
COMPUSER DS    XL(L'CPYUID)        COMPANY PRINCIPAL USER-ID NUMBER             
COMPNAME DS    CL(L'NAMEREC)       COMPANY NAME                                 
COMPADD1 DS    CL(L'ADRADD1)       COMPANY ADDRESS LINE 1                       
COMPADD2 DS    CL(L'ADRADD2)       COMPANY ADDRESS LINE 2                       
COMPADD3 DS    CL(L'ADRADD3)       COMPANY ADDRESS LINE 3                       
COMPLOGO DS    CL(L'CPYLOGO)       COMPANY LOGO                                 
COMPDR   DS    PL8                 COMPANY DEBITS                               
COMPCR   DS    PL8                 COMPANY CREDITS                              
                                                                                
UNITINVQ EQU   254                 UNIT NOT FOUND OR INVALID                    
UNITSTAT DS    X                   UNIT STATUS BYTE                             
UNITNOTF EQU   X'80'               UNIT NOT ON FILE                             
UNITNAME DS    CL(L'NAMEREC)       UNIT NAME                                    
UNITDR   DS    PL8                 UNIT DEBITS                                  
UNITCR   DS    PL8                 UNIT CREDITS                                 
                                                                                
LEDGINVQ EQU   253                 LEDGER NOT FOUND OR INVALID                  
LEDGSTAT DS    X                   LEDGER STATUS BYTE                           
LEDGNOTF EQU   X'80'               LEDGER NOT ON FILE                           
LEDGNAME DS    CL(L'NAMEREC)       LEDGER NAME                                  
LEDGDR   DS    PL8                 LEDGER DEBITS                                
LEDGCR   DS    PL8                 LEDGER CREDITS                               
                                                                                
LEDGDETS DS    0X                  MAPS TO ACLDGTABD                            
LEDGUL   DS    CL2                 UNIT/LEDGER CODE                             
LEDGTYPE DS    CL(L'LDGTYPE)       LEDGER TYPE                                  
LEDGLIKE DS    CL(L'LDGLIKE)       LEDGER IS LIKE                               
LEDGOFFP DS    XL(L'LDGOPOS)       OFFICE POSITION                              
LEDGCLOS DS    XL(L'LDGCLOS)       LEDGER CLOSE-OUT TYPE                        
LEDGLVA  DS    XL(L'ACLVLEN)       LEVEL A LENGTH                               
LEDGLVB  DS    XL(L'ACLVLEN)       LEVEL B LENGTH                               
LEDGLVC  DS    XL(L'ACLVLEN)       LEVEL C LENGTH                               
LEDGLVD  DS    XL(L'ACLVLEN)       LEVEL D LENGTH                               
LEDGSECY DS    X                   SECURITY NUMBER (RSTSECY+1)                  
LEDGCPOS DS    X                   DISPLACEMENT OF CLIENT                       
LEDGDETL EQU   *-LEDGDETS                                                       
                                                                                
MAXLDGQ  EQU   10                                                               
LEDGXTRA DS    (MAXLDGQ)XL(LEDGDETL)   EXTRA ENTRIES FOR ADDTRN USE             
                                                                                
PALAREA  DS    XL32                    P&L BUCKET AREA                          
                                                                                
FILENME  DS    CL5                 ACCFILE NAME                                 
                                                                                
DICO     DS    0C                                                               
         DSDDL PRINT=YES                                                        
         DS    X                   N/D                                          
                                                                                
       ++INCLUDE ACADDTRND                                                      
                                                                                
       ++INCLUDE DMGREQUS                                                       
                                                                                
TMSUBLK  DS    XL256               TMSUPD BLOCK                                 
                                                                                
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
                                                                                
         ORG   P                                                                
LINED    DS    0C                  ** LAYOUT FOR PRINT LINE **                  
         DS    C                                                                
LINUNIT  DS    CL27                UNIT                                         
         DS    C                                                                
LINLEDG  DS    CL36                LEDGER                                       
         ORG   LINLEDG                                                          
         DS    CL18                                                             
LINTDESC DS    CL18                TOTAL DESCRIPTION                            
         DS    C                                                                
LINNUML  EQU   14                  LENGTH OF NUMERICAL FIELD                    
LINDR    DS    CL(LINNUML)         DEBITS                                       
         DS    C                                                                
LINCR    DS    CL(LINNUML)         CREDITS                                      
         DS    C                                                                
LINBAL   DS    CL(LINNUML)         BALANCE                                      
         DS    C                                                                
LINEND   DS    0C                  END OF LINE                                  
                                                                                
         ORG   P                                                                
ERRLINE  DS    0C                  ** LAYOUT FOR ERROR LINE **                  
         DS    C                                                                
ERRLCPY  DS    CL2                 COMPANY CODE (HEXOUT)                        
         DS    C                                                                
ERRLULA  DS    CL14                UNIT/LEDGER/ACCOUNT                          
         DS    C                                                                
ERRLOFF  DS    CL2                 OFFICE/ANALYSIS CODE                         
         DS    C                                                                
ERRLULC  DS    CL14                UNIT/LEDGER/CONTRA-ACCOUNT                   
         DS    C                                                                
ERRLBTCH DS    CL6                 BATCH REFERENCE                              
         DS    C                                                                
ERRLDATE DS    CL8                 TRANSACTION DATE                             
         DS    C                                                                
ERRLREFN DS    CL6                 REFERENCE NUMBER                             
         DS    C                                                                
ERRLDR   DS    CL14                DEBIT AMOUNT                                 
         DS    C                                                                
ERRLCR   DS    CL14                CREDIT AMOUNT                                
         DS    C                                                                
ERRLMESS DS    CL20                ERROR MESSAGE                                
                                                                                
         ORG   P                                                                
WKLINE   DS    0C                  ** LAYOUT FOR WORKER LIST LINE **            
WKLBXLC  DS    C                                                                
WKLACT   DS    CL(L'PSHDACC-1)     ACCOUNT                                      
WKLBXC1  DS    C                                                                
WKLCAC   DS    CL(L'PSHDSBAC-1)    CONTRA ACCOUNT                               
WKLBXC2  DS    C                                                                
WKLOFF1  DS    CL(L'PSHDANAL)      KEY ANALYSIS CODE                            
WKLSLASH DS    C                                                                
WKLOFF2  DS    CL(L'TRNOFFC)       TRANSACTION ANALYSIS CODE                    
WKLBXC3  DS    C                                                                
WKLDAT   DS    CL8                 TRANSACTION DATE                             
WKLBXC4  DS    C                                                                
WKLDR    DS    CL14                DEBIT AMOUNT                                 
WKLBXC5  DS    C                                                                
WKLCR    DS    CL14                CREDIT AMOUNT                                
WKLBXC6  DS    C                                                                
         DS    C                                                                
WKLREF   DS    CL(L'TRNREF)        REFERENCE                                    
         DS    C                                                                
WKLBXC7  DS    C                                                                
WKLNARR  DS    CL28                NARRATIVE                                    
WKLBXRC  DS    C                                                                
*                                                                               
         EJECT                                                                  
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
                                                                                
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
                                                                                
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
                                                                                
* DMXTNTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMXTNTD                                                        
         PRINT ON                                                               
                                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
                                                                                
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
                                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
                                                                                
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
                                                                                
* DDSYSELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSYSELD                                                       
         PRINT ON                                                               
                                                                                
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
                                                                                
* DMDTFPH                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
                                                                                
* FASSBOFF                                                                      
         PRINT OFF                                                              
SSOOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
                                                                                
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
                                                                                
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
                                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
                                                                                
* ACGLPOSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGLPOSTD                                                      
         PRINT ON                                                               
                                                                                
* ACPAY2JD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACPAY2JD                                                       
         PRINT ON                                                               
                                                                                
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
                                                                                
*&&US                                                                           
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
*&&                                                                             
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
                                                                                
*NMALIK CODE START FOR DYNAMIC FILE ALLOCATION                                  
*UTLD                                                                           
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
                                                                                
*DDNAMED                                                                        
         PRINT OFF                                                              
DDNAMED  DSECT                                                                  
       ++INCLUDE DMDDNAMED                                                      
         PRINT ON                                                               
                                                                                
*NMALIK CODE END FOR DYNAMIC FILE ALLOCATION                                    
*                                                                               
TOTTABD  DSECT                     ** TOTALS RECORD TABLE **                    
TOTTDISP DS    AL2                 DISPLACEMENT TO FULLWORD ACCUM               
TOTTEOTQ EQU   255                 END OF TABLE INDICATOR                       
TOTTINDS DS    X                   ** TOTALS INDICATORS **                      
TOTTIADD EQU   X'80'               ADD TO TOTAL RECORDS ADDED                   
TOTTICHA EQU   X'40'               ADD TO TOTAL RECORDS CHANGED                 
TOTTIDEL EQU   X'20'               ADD TO TOTAL RECORDS DELETED                 
TOTTITOT EQU   X'01'               THIS IS A TOTAL                              
TOTTDESC DS    CL30                TOTALS DESCRIPTION                           
TOTTABL  EQU   *-TOTTABD                                                        
         EJECT                                                                  
ACAU02   CSECT                                                                  
TOTTAB   DS    0X                  ** RECORD TOTALS TABLE **                    
         DC    AL2(FILEMRG-WORKD),AL1(0)                                        
         DC    CL30'Transaction records merged'                                 
         DC    AL2(TRNACCA-WORKD),AL1(TOTTIADD)                                 
         DC    CL30'Accounts added'                                             
         DC    AL2(TRNACCU-WORKD),AL1(TOTTICHA)                                 
         DC    CL30'Accounts changed'                                           
         DC    AL2(TRNANCA-WORKD),AL1(0)                                        
         DC    CL30'Name change pointers added'                                 
         DC    AL2(TRNOAPA-WORKD),AL1(0)                                        
         DC    CL30'Office/Acct pointers added'                                 
         DC    AL2(TRNOAPU-WORKD),AL1(0)                                        
         DC    CL30'Office/Acct pointers changed'                               
         DC    AL2(TRNOFAA-WORKD),AL1(TOTTIADD)                                 
         DC    CL30'Office headers added'                                       
         DC    AL2(TRNOFAU-WORKD),AL1(TOTTICHA)                                 
         DC    CL30'Office headers changed'                                     
         DC    AL2(TRNBUKA-WORKD),AL1(TOTTIADD)                                 
         DC    CL30'Contra buckets added'                                       
         DC    AL2(TRNBUKU-WORKD),AL1(TOTTICHA)                                 
         DC    CL30'Contra buckets changed'                                     
         DC    AL2(TRNCACA-WORKD),AL1(TOTTIADD)                                 
         DC    CL30'Contra headers added'                                       
         DC    AL2(TRNCACU-WORKD),AL1(TOTTICHA)                                 
         DC    CL30'Contra headers changed'                                     
         DC    AL2(TRNTRNA-WORKD),AL1(TOTTIADD)                                 
         DC    CL30'Transactions added'                                         
         DC    AL2(TRNINVA-WORKD),AL1(0)                                        
         DC    CL30'Inv# passive pointers added'                                
         DC    AL2(TRNGINA-WORKD),AL1(0)                                        
         DC    CL30'GIN passive pointers added'                                 
         DC    AL2(TRNBDPA-WORKD),AL1(0)                                        
         DC    CL30'BDP passive pointers added'                                 
         DC    AL2(TRNSRCA-WORKD),AL1(0)                                        
         DC    CL30'Name search pointers added'                                 
         DC    AL2(TRNPALA-WORKD),AL1(0)                                        
         DC    CL30'P&&L buckets added'                                         
         DC    AL2(TRNPALU-WORKD),AL1(0)                                        
         DC    CL30'P&&L buckets changed'                                       
         DC    AL2(TRNSRCU-WORKD),AL1(0)                                        
         DC    CL30'Name search pointers changed'                               
         DC    AL2(TRNTRNU-WORKD),AL1(TOTTICHA)                                 
         DC    CL30'Transactions reversed'                                      
         DC    AL2(TRNTRNC-WORKD),AL1(TOTTICHA)                                 
         DC    CL30'Transactions matched'                                       
         DC    AL2(TRNTDEL-WORKD),AL1(TOTTIDEL)                                 
         DC    CL30'Transactions deleted'                                       
         DC    AL2(MPDRADD-WORKD),AL1(TOTTIADD)                                 
         DC    CL30'Billing transfers added'                                    
         DC    AL2(MPDRCHA-WORKD),AL1(TOTTICHA)                                 
         DC    CL30'Billing transfers changed'                                  
         DC    AL2(MPDRERR-WORKD),AL1(TOTTICHA)                                 
         DC    CL30'Billing transfer errors'                                    
         DC    AL2(PORDCLO-WORKD),AL1(TOTTICHA)                                 
         DC    CL30'Purchase orders closed'                                     
         DC    AL2(FILECHA-WORKD),AL1(TOTTITOT)                                 
         DC    CL30'* Total records changed *'                                  
         DC    AL2(FILEADD-WORKD),AL1(TOTTITOT)                                 
         DC    CL30'* Total records added *'                                    
         DC    AL2(FILEDEL-WORKD),AL1(TOTTITOT)                                 
         DC    CL30'* Total records deleted*'                                   
TOTTABX  DC    AL1(TOTTEOTQ)                                                    
                                                                                
RCVTAB   DS    0X                  ** RECOVERY TABLE **                         
         DC    AL2(RECVCPY-WORKD),AL1(TOTTIADD)                                 
         DC    CL30'Recovery copies'                                            
         DC    AL2(RECVCHA-WORKD),AL1(TOTTIADD)                                 
         DC    CL30'Recovery changes'                                           
         DC    AL2(RECVADD-WORKD),AL1(TOTTIADD)                                 
         DC    CL30'Recovery adds'                                              
         DC    AL2(RECVOTH-WORKD),AL1(TOTTIADD)                                 
         DC    CL30'Recovery others'                                            
         DC    AL2(0),AL1(TOTTITOT)                                             
         DC    CL30'* Total recovery records *'                                 
RCVTABX  DC    AL1(TOTTEOTQ)                                                    
         EJECT                                                                  
ERRTAB   DS    0CL21               ** ERROR MESSAGES **                         
         DC    AL1(COMPINVQ),CL20'Invalid company'                              
         DC    AL1(UNITINVQ),CL20'Invalid unit'                                 
         DC    AL1(LEDGINVQ),CL20'Invalid ledger'                               
         DC    AL1(TRNETRNI),CL20'Invalid transaction'                          
         DC    AL1(TRNEACCI),CL20'Invalid account'                              
         DC    AL1(TRNEOFAI),CL20'Invalid office'                               
         DC    AL1(TRNETRNS),CL20'Invalid status'                               
         DC    AL1(TRNERRGL),CL20'Invalid GL setup'                             
         DC    AL1(ERRNORUL),CL20'No default GL rule'                           
         DC    AL1(ERRNOFLT),CL20'No GL filt for off'                           
         DC    AL1(ERROFPOS),CL20'Offpos problem'                               
         DC    AL1(ERRNOCLO),CL20'No client office'                             
         DC    AL1(ERRNOGLA),CL20'Invalid GL account'                           
         DC    AL1(ERRLKOCL),CL20'GL acct lock/closed'                          
         DC    AL1(ERRNOOFF),CL20'No GL office set'                             
ERRTABX  DC    AL1(0),CL20'????? Unknown ?????'                                 
                                                                                
IN       DCB   DDNAME=IN,DSORG=PS,RECFM=VB,MACRF=GM,EODAD=GETIN26               
                                                                                
                                                                                
P2JOUT   DCB   DDNAME=P2JOUT,DSORG=PS,MACRF=PM,RECFM=FB,               X        
               BLKSIZE=0,LRECL=JARAYLQ                                          
                                                                                
SORTWK1  DCB   DDNAME=SORTWK1,DSORG=PS,RECFM=FB,MACRF=(GM,PM),         X        
               LRECL=256,BLKSIZE=5120,EODAD=TOTALS20                            
                                                                                
SORTWK01 DCB   DDNAME=SORTWK01,DSORG=PS,RECFM=FB,MACRF=(GM,PM),        X        
               LRECL=256,BLKSIZE=5120,EODAD=TOTALS20                            
                                                                                
IOLEN    EQU   2048                                                             
IOIN     DC    (IOLEN)X'00'        INPUT RECORD AREA                            
IOSV     DC    (IOLEN)X'00'        INPUT RECORD SAVED AREA                      
IOGEN    DC    (IOLEN)X'00'        GENERAL I/O AREA                             
IOACC    DC    (IOLEN)X'00'        ACCOUNT RECORD I/O AREA                      
IOOFA    DC    (IOLEN)X'00'        OFFICE/ACCOUNT I/O AREA                      
IOBUK    DC    (IOLEN)X'00'        BUCKET ELEMENT BUFFER                        
IOCAC    DC    (IOLEN)X'00'        CONTRA-ACCOUNT I/O AREA                      
IOTRN    DC    (IOLEN)X'00'        TRANSACTION RECORD I/O AREA                  
                                                                                
ERRIO    DC    (IOLEN)X'00'        ERROR RECORD                                 
ERRBUFF  DC    6144X'00'           ERROR FILE BUFFER                            
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054ACREPAU02 04/13/20'                                      
         END                                                                    
