*          DATA SET ACREPBG02  AT LEVEL 067 AS OF 07/29/20                      
*PHASE ACBG02A                                                                  
*INCLUDE BMONVAL                                                                
*INCLUDE CONVMOS                                                                
*INCLUDE PERVERT                                                                
*&&US                                                                           
*INCLUDE ACRAPPER                                                               
*INCLUDE ACSMFBAL                                                               
*&&                                                                             
ACBG02   TITLE '- DAILY JOURNAL: BATCH RECORDS GENERATOR/MARKER'                
***********************************************************************         
* VGUP 067 30JUL20 <SPEC-48588> Fix for Order GAP status expiration             
*                                                                               
***********************************************************************         
* Format of parameter card:                                          *          
*                                                                    *          
* Byte 1 = Y - generate batch records for postings from recovery,    *          
*              worker, (old) bill, and interagency (US only).        *          
*          R - generate batch records for recovery postings ONLY     *          
*              NB. a DATE= card with this option is recommended.     *          
*          X - process everything EXCEPT recovery file postings.     *          
*                                                                    *          
* Byte 2 = Y - generate worker files for use in SM/AU programs -     *          
*              ADJ(draft posting keys), ADK(online-updated postings) *          
*                                                                    *          
* Byte 3-4 =   two-character agency alpha filter - works only on     *          
*              existing batches on file, not 'non-postman' records.  *          
*                                                                    *          
* Byte 5-8 =   four-digit user-id filter - works on recovery records *          
*              and postman batches.                                  *          
**********************************************************************          
ACBG02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACBG**,R9                                                    
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING WORKD,RC            RC=A(LOCAL W/S)                              
         LARL  R8,GLOBALS                                                       
         USING GLOBALS,R8          R8=A(Global literals)                        
*                                                                               
         CLI   MODE,PROCRCVR       PROCESS A RECOVERY FILE RECORD               
         BE    RCVR                                                             
         CLI   MODE,RUNFRST        INITIALIZATION                               
         BE    FRST                                                             
         CLI   MODE,RUNLAST        ADD RECOVERY/WORKER BATCHES, AND             
         BE    LAST                PROCESS BATCHES ALREADY ON FILE              
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
FRST     L     R3,ADMASTC                                                       
         USING MASTD,R3                                                         
         LA    R2,VADDTRN          LOADABLE PHASES                              
         MVC   MCDUB,=CL8'T00AXX'                                               
FRST01   GOTO1 HEXOUT,DMCB,0(R2),MCDUB+4,1,0,0                                  
         GOTO1 MCVLOADM,DMCB,0                                                  
         BE    *+6                 MUSTTEST PHASE NOT FOUND                     
         DC    H'0'                                                             
         MVC   0(4,R2),4(R1)                                                    
         LA    R2,4(R2)                                                         
         CLI   0(R2),0                                                          
         BNE   FRST01                                                           
*                                                                               
         MVC   MCDUB,=CL8'T00A9F'                                               
         GOTO1 MCVLOADM,DMCB,0                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   VPADDLE,4(R1)                                                    
         DROP  R3                                                               
*                                                                               
         L     RF,ADCOMFAC         SET V-TYPS FOR ROUTINES NOT INCLUDED         
         USING COMFACSD,RF                                                      
         L     RE,CGETDAY                                                       
         ST    RE,VGETDAY                                                       
         L     RE,CHELLO                                                        
         ST    RE,VHELLO                                                        
         L     RE,CCALLOV                                                       
         ST    RE,VCALLOV                                                       
         DROP  RF                                                               
         LH    RF,=Y(DSLC2-WORKD)                                               
         AR    RF,RC                                                            
         ST    RF,ADSLC2                                                        
*                                                                               
         GOTO1 ADDICTAT,DMCB,C'LU  ',ADCUC,DSUC                                 
         GOTO1 ADDICTAT,DMCB,C'LL  ',ADCLC1,DSLC1                               
         GOTO1 ADDICTAT,DMCB,C'LL  ',ADCLC2,ADSLC2                              
         MVI   RCFLAG1,RCFREPLC                                                 
*                                                                               
         GOTO1 DATCON,(R1),(5,0),(1,TODAYRP)                                    
         GOTO1 DATCON,(R1),(4,RCDATE),(1,TODAYP)                                
         GOTO1 (RF),(R1),(1,TODAYP),(2,TODAYC)                                  
         GOTO1 (RF),(R1),(1,TODAYP),(8,TODAYD)                                  
         GOTO1 (RF),(R1),(1,TODAYP),(0,TODAYE)                                  
         XR    RF,RF                                                            
         ICM   RF,7,TODAYP                                                      
         LNR   RF,RF                                                            
         STCM  RF,7,TODAYPCO                                                    
         MVC   TODAYCC,EFFALL                                                   
         XC    TODAYCC,TODAYC      COMPLEMENT COMPRESSED DATE                   
*                                  SET DATE RANGE FOR BATCH FILTERING           
         LA    R0,31               HIGH: TODAY+31                               
         GOTO1 ADDAY,DMCB,TODAYE,TEMP,(R0)                                      
         GOTO1 DATCON,DMCB,(0,TEMP),(2,BHIDAT)                                  
         LNR   R0,R0               LOW:  TODAY-31                               
         GOTO1 ADDAY,DMCB,TODAYE,TEMP,(R0)                                      
         GOTO1 DATCON,DMCB,(0,TEMP),(2,BLODAT)                                  
         GOTO1 DATAMGR,DMCB,(0,DMOPEN),ACCMST,NACCDAY                           
*                                  SET USER BATCH FILTERING VALUES              
         MVC   CRE8BAT,RCFFPARM+0  Y/R/X - CREATE BATCHES                       
         MVC   CRE8WKR,RCFFPARM+1  Y - CREATE WORKER FILES                      
         CLC   RCFFPARM+2(2),SPACES                                             
         BE    *+10                                                             
         MVC   FLTALFA,RCFFPARM+2  2-BYTE AGY ALPHA                             
         CLC   RCFFPARM+4(4),CZERO                                              
         BNH   FRST02                                                           
         PACK  DUB,RCFFPARM+4(4)   4-BYTE USER-ID                               
         CVB   RF,DUB                                                           
         STCM  RF,3,FLTUSR                                                      
FRST02   L     RF,ADMASTC                                                       
         MVC   SENO,MCIDSENO-MASTD(RF)                                          
         ZAP   BATCASH,PZERO                                                    
         ZAP   ITMCASH,PZERO                                                    
         XC    SLUID,SLUID                                                      
         XC    REPPSTG,REPPSTG                                                  
         MVC   REPCPY,SPACES                                                    
         MVC   REPUSR,SPACES                                                    
         MVC   REPCMNT,SPACES                                                   
         MVI   SORTIND,0                                                        
         ZAP   TOFUIDDR,PZERO                                                   
         ZAP   TOFUIDCR,PZERO                                                   
         MVI   PROIND,PROIOTHR     SET PROCESSING NON-POSTMAN TYPES             
         L     RF,AXTPQF           INITIALISE EXTERNAL PQ FILES LIST            
         LA    RE,4(RF)            FIRST AVAILABLE ENTRY                        
         ST    RE,0(RF)                                                         
*                                                                               
         LA    R1,ROUTS            ESTABLISH ROUT A-TYPES                       
         LA    R0,ROUTSN                                                        
         SR    RE,RE                                                            
         L     RF,=A(ROUT)                                                      
FRST04   STCM  RE,1,0(R1)                                                       
         STCM  RF,7,1(R1)                                                       
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,FRST04                                                        
*                                                                               
         L     R0,AACMTAB          INITIALISE ACCUMS TABLE                      
         LH    R1,=Y(ACMTABLN)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LR    RF,R0                                                            
         MVI   0(RF),EFFS          SET EOT MARKER                               
*                                                                               
         L     R0,AREPTOT          INITIALISE REPORT TOTALS TABLE               
         LH    R1,=Y(REPTOTLN)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LR    RF,R0                                                            
         MVI   0(RF),EFFS          SET EOT MARKER                               
*                                                                               
         LA    RF,MYADDS                                                        
         LA    RE,BGATYPES                                                      
         LA    R0,BGATYNUM                                                      
FRST05   MVC   0(L'MYADDS,RF),0(RE) SAVE IO ADDRESSES IN LOCAL STORAGE          
         LA    RF,L'MYADDS(RF)                                                  
         LA    RE,L'MYADDS(RE)                                                  
         BCT   R0,FRST05                                                        
*                                                                               
         USING TRNBLKD,R1                                                       
         L     R1,ATRNBLK          INITIALIZE ADDTRN BLOCK                      
         MVC   TRNCTRY,RCCTRY                                                   
         MVC   TRNCOMF,ADCOMFAC                                                 
         MVC   TRNACC,AADDTIO1                                                  
         MVC   TRNOFA,AADDTIO2                                                  
         MVC   TRNBUK,AADDTIO3                                                  
         MVC   TRNCAC,AADDTIO4                                                  
         MVC   TRNREC,AIO3                                                      
*                                                                               
         LHI   RF,PALAREA-WORKD                                                 
         AR    RF,RC                                                            
         STCM  RF,15,TRNPAL        SET A(P&L BUCKET AREA)                       
*                                                                               
         MVC   TRNUDATE,TODAYC                                                  
         MVI   TRNMODE,TRNMEMUY    SET NEW FILES                                
         OI    TRNINDS2,TRNIDNAM   SET CREATE DUMMY NAMELS IF NEEDED            
         CLI   RCWRITE,NO          TEST WRITE=NO REQUESTED                      
         BNE   *+8                                                              
         OI    TRNINDS,TRNIWRNO                                                 
         CLI   RCTRACE,YES         TEST TRACE REQUESTED                         
         BNE   FRST06                                                           
         MVC   TRNHOOK,AIOTRACE                                                 
         DROP  R1                                                               
*                                                                               
FRST06   L     RF,ADBXAREA                                                      
         USING BOXD,RF                                                          
         MVI   BOXCOLS+(PBXL-PBLIN),C'L'                                        
         MVI   BOXCOLS+(PBX1-PBLIN),C'C'                                        
         MVI   BOXCOLS+(PBX2-PBLIN),C'C'                                        
         MVI   BOXCOLS+(PBX3-PBLIN),C'C'                                        
         MVI   BOXCOLS+(PBX4-PBLIN),C'C'                                        
         MVI   BOXCOLS+(PBX5-PBLIN),C'C'                                        
         MVI   BOXCOLS+(PBX6-PBLIN),C'C'                                        
         MVI   BOXCOLS+(PBX7-PBLIN),C'C'                                        
         MVI   BOXCOLS+(PBX8-PBLIN),C'C'                                        
         MVI   BOXCOLS+(PBX9-PBLIN),C'C'                                        
         MVI   BOXCOLS+(PBXR-PBLIN),C'R'                                        
         MVI   BOXROWS+05,C'T'                                                  
         MVI   BOXROWS+07,C'M'                                                  
         MVI   BOXROWS+99,C'B'                                                  
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         DROP  RF                                                               
*                                                                               
*                                  READ ALL COMPANY RECORDS ON FILE             
*                                  AND SAVE THE DETAILS IN CPYTAB               
*                                                                               
         LA    R3,CPYFRSTQ         SET INITIAL COMPANY CODE                     
FRST08   LA    R2,IOKEY                                                         
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,SPACES                                                    
         STC   R3,CPYKCPY                                                       
         CLI   CPYKCPY,X'FF'       TEST EOF                                     
         BE    FRSTX                                                            
         LA    R1,IOHI+IOACCDIR+IO1                                             
         GOTO1 AIO                                                              
         BNE   FRST18                                                           
         CLC   CPYKEY+L'CPYKCPY(L'CPYKEY-L'CPYKCPY),SPACES                      
         BNE   FRST18              NOT A COMPANY RECORD                         
         LA    R1,IOGET+IOACCMST+IO1                                            
         GOTO1 AIO                                                              
         BNE   FRST18                                                           
         SR    RE,RE               DISPLACE TO TABLE ENTRY FOR THIS CPY         
         IC    RE,CPYKCPY                                                       
         SH    RE,=Y(CPYFRSTQ)                                                  
         MH    RE,=Y(CPYTABL)                                                   
         L     RF,ACPYTAB                                                       
         AR    RE,RF                                                            
         USING CPYTABD,RE                                                       
         L     R2,AIOBUFF                                                       
         LA    RF,CPYRFST                                                       
         USING NAMELD,RF                                                        
FRST10   CLI   NAMEL,0             FIND NAME AND COMPANY ELEMENT                
         BE    FRST18                                                           
         CLI   NAMEL,CPYELQ                                                     
         BE    FRST16                                                           
         CLI   NAMEL,NAMELQ                                                     
         BE    FRST14                                                           
FRST12   SR    R0,R0                                                            
         IC    R0,NAMLN                                                         
         AR    RF,R0                                                            
         B     FRST10                                                           
*                                  SAVE RELEVANT DETAILS                        
FRST14   MVC   CPYTNAME,SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y((NAMEREC-NAMEL)+1)                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CPYTNAME(0),NAMEREC                                              
         B     FRST12                                                           
*                                                                               
         USING CPYELD,RF                                                        
FRST16   MVC   CPYTCPY,CPYKCPY                                                  
         MVC   CPYTUID,CPYUID                                                   
         MVC   CPYTALPH,CPYALPHA                                                
         MVC   CPYTPROD,CPYPROD                                                 
         MVC   CPYTSTA1,CPYSTAT1                                                
         MVC   CPYTSTA2,CPYSTAT2                                                
         MVC   CPYTSTA3,CPYSTAT3                                                
         MVC   CPYTSTA4,CPYSTAT4                                                
         CLI   CPYLN,CPYLN1Q                                                    
         BE    FRST12                                                           
         MVC   CPYTSTA5,CPYSTAT5                                                
         MVC   CPYTSTA6,CPYSTAT6                                                
         MVC   CPYTSTA7,CPYSTAT7                                                
         MVC   CPYTSTA8,CPYSTAT8                                                
         CLI   CPYLN,CPYLN3Q                                                    
         BL    FRST12                                                           
         MVC   CPYTSTA9,CPYSTAT9                                                
         MVC   CPYTSTAA,CPYSTATA                                                
         CLI   CPYLN,CPYLN4Q                                                    
         BL    FRST12                                                           
         MVC   CPYTGMOA,CPYGLMOA                                                
         B     FRST12                                                           
*                                                                               
FRST18   IC    R3,CPYKCPY          INCREMENT COMPANY CODE                       
         LA    R3,1(R3)                                                         
         B     FRST08                                                           
*                                                                               
FRSTX    B     EXIT                                                             
         DROP  R2,RE,RF                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS A RECOVERY FILE RECORD                                      *         
***********************************************************************         
*                                                                               
RCVR     CLI   CRE8BAT,YES         TEST CREATE ALL NON-INPUT BATCHES            
         BE    *+12                                                             
         CLI   CRE8BAT,RCVRY       TEST CREATE RECOVERY BATCHES ONLY            
         BNE   EXIT                                                             
         MVI   SRCIND,RECOVR       SET SOURCE FOR REPORT HEADS                  
         L     R2,ADTRANS                                                       
         USING RCVRECD,R2          R2=A(RECOVERY HEADER)                        
         CLI   RCVFILTY,RCVFAMST   TEST ACCMST RECORD                           
         BNE   EXIT                                                             
         CLI   RCVRECTY,RCVRADDQ   TEST RECOVERY ADD                            
         BNE   EXIT                                                             
         CLI   RCVTSKID,X'FF'      TEST DELETED                                 
         BE    EXIT                                                             
         L     RF,ARPRGTAB         TABLE OF RECOVERY BATCH TYPES                
         USING RPRGTABD,RF                                                      
RCVR04   CLI   RPRGNO,0                                                         
         BE    EXIT                NOT IN TABLE                                 
         CLC   RCVPRGNO,RPRGNO                                                  
         BE    RCVR08                                                           
RCVR06   LA    RF,RPRGTABL(RF)                                                  
         B     RCVR04                                                           
*                                                                               
RCVR08   ST    RF,SAVERF                                                        
         OC    FLTUSR,FLTUSR       TEST USER-ID FILTER                          
         BZ    *+14                                                             
         CLC   RCVUSRID,FLTUSR                                                  
         BNE   EXIT                                                             
*                                                                               
         LA    R3,RCVRECRD                                                      
         USING TRNRECD,R3          R3=A(TRANSACTION KEY)                        
         TM    TRNRSTAT,TRNSDRFT                                                
         BNZ   EXIT                IGNORE DRAFT POSTINGS                        
         LA    R4,TRNRFST                                                       
         USING TRNELD,R4           R4=A(TRANSACTION ELEMENT)                    
         CLI   TRNEL,TRNELQ        TEST TRANSACTION ELEMENT                     
         BNE   EXIT                                                             
         SR    R0,R0                                                            
         LR    R1,R4               LOCATE STATUS ELEMENT                        
         USING TRSELD,R1                                                        
RCVR10   IC    R0,TRSLN                                                         
         AR    R1,R0                                                            
         CLI   TRSEL,0                                                          
         BE    EXIT                NO STATUS ELEMENT                            
         CLI   TRSEL,TRSELQ                                                     
         BNE   RCVR10                                                           
         CLI   RPRGSUBP,0          TEST MATCH ON CREATING PROGRAM               
         BE    RCVR12                                                           
         MVC   BYTE,TRSSTAT3                                                    
         NI    BYTE,TRSSPROG                                                    
         CLC   RPRGSUBP,BYTE                                                    
         BNE   RCVR06                                                           
*                                                                               
RCVR12   XC    S1REC(S1RECL),S1REC BUILD SORT RECORD                            
         STM   RF,R1,BGREGS                                                     
         GOTO1 VCONVMOS,DMCB,(X'FE',TRNELD),S1KMOS                              
         CLI   RCTRACE,YES         TEST TRACE REQUESTED                         
         BNE   RCVR14                                                           
         LA    R0,RCVREAD          DUMMY UP PARMS FOR TRACE ROUTINE             
         ST    R0,TEMP                                                          
         LA    R0,ACCRCV                                                        
         ST    R0,TEMP+4                                                        
         MVI   TEMP+8,0                                                         
         LA    R0,TEMP                                                          
         GOTO1 AIOTRACE                                                         
RCVR14   LM    RF,R1,BGREGS        RECOVER REGS                                 
         MVC   S1KCPY,TRNKCPY      SET COMPANY CODE                             
         MVC   S1KUID,RCVUSRID     SET USER-ID                                  
*&&UK                                                                           
         CLI   RPRGNO,RCVPWFMQ     IF POSTING WAS MADE BY WFM                   
         BNE   RCVR15                                                           
         CLC   RCVUSRID,DATA1ID    AND USER WAS DATA CONTROL                    
         BE    *+10                                                             
         CLC   RCVUSRID,DATA2ID                                                 
         BE    *+10                                                             
         CLC   RCVUSRID,DATA3ID                                                 
         BNE   RCVR15                                                           
         SR    RE,RE               OVERRIDE USER-ID                             
         IC    RE,TRNKCPY          WITH COMPANY'S PRINCIPAL ID                  
         SH    RE,=Y(CPYFRSTQ)                                                  
         MH    RE,=Y(CPYTABL)                                                   
         L     R0,ACPYTAB                                                       
         AR    RE,R0                                                            
         USING CPYTABD,RE                                                       
         CLI   CPYTCPY,0                                                        
         BE    EXIT                                                             
         MVC   S1KUID,CPYTUID                                                   
         DROP  RE                                                               
*&&                                                                             
RCVR15   TM    RPRGIND,RPRGIMBQ    TEST MULTIPLE BATCH TYPES ALLOWED            
         BZ    *+14                                                             
         MVC   S1KTYP,RPRGBTYP     SET BATCH TYPE FROM TABLE ENTRY              
         B     RCVR16                                                           
         OC    S1KTYP,TRNTYPE      SET BATCH TYPE FROM TRNTYPE                  
         BZ    EXIT                DROP RECORD IF NO BATCH TYPE                 
         CLI   RPRGBTYP,0          TEST BATCH RESTRICTION                       
         BE    RCVR16                                                           
         LA    RE,X'70'            SET NEQ CC                                   
         TM    RPRGIND,RPRGXBTY    TEST EXCLUDING SPECIFIC BATCH TYPE           
         BZ    *+8                                                              
         LA    RE,X'80'            SET EQ CC                                    
         CLC   TRNTYPE,RPRGBTYP    YES - MATCH ON TABLE ENTRY                   
         EX    RE,*+8                                                           
         B     *+8                                                              
         NOP   RCVR26                                                           
*                                                                               
RCVR16   CLI   RCTRACE,YES                                                      
         BNE   RCVR17                                                           
         LA    R0,=C'SELECT'                                                    
         ST    R0,TEMP                                                          
         LA    R0,TEMP                                                          
         GOTO1 AIOTRACE                                                         
         LM    RF,R1,BGREGS                                                     
*                                                                               
RCVR17   MVC   S1KSIN,RCVSEQNO     SET SYSTEM INPUT NO                          
         TM    RPRGIND,RPRGNOSQ    TEST SIN IS SIG. SORT KEY COMPONENT          
         BZ    *+16                                                             
         MVC   S1KINC,RCVSEQNO     NO - USE TO PREVENT DUP. SORT KEYS           
         MVC   S1KBRF,TRNBREF           AND MAKE BATCH REF. SIGNIFICANT         
         MVC   S1KDAT,RCVDATE      SET BATCH DATE                               
         MVC   S1KREF,TRNBREF      SET BATCH REF                                
         TM    RPRGIND,RPRGBRFQ    TEST BATCH REFERENCE OVERRIDE                
         BZ    RCVR18                                                           
         ICM   RE,15,RCVSEQNO      USE SYSTEM INPUT NO.                         
         CVD   RE,DUB                                                           
         UNPK  S1KREF,DUB                                                       
         OI    S1KREF+3,X'F0'                                                   
*                                                                               
RCVR18   CLI   RPRGNO,RCVPINTQ     IF POSTING MADE BY INTERAGENCY               
         BNE   *+8                                                              
         CLI   S1KTYP,BT30         AND BATCH TYPE 30                            
         BNE   *+8                                                              
         OI    S1KIND,S1KINREV     SET 'BATCH CANNOT BE REVERSED' IND           
         MVC   S1KSEQ,TRSBSEQ      SET BATCH SEQUENCE NO.                       
         TM    RPRGIND,RPRGISQQ    TEST OVERRIDE SEQUENCE NO.                   
         BZ    RCVR20                                                           
         LH    RE,SEQNUM           GENERATE SEQUENCE NO.                        
         LA    RE,1(RE)                                                         
         STH   RE,SEQNUM                                                        
         MVC   S1KSEQ,SEQNUM                                                    
*                                                                               
RCVR20   MVC   S1DTKEY,TRNKEY      MOVE IN TRANSACTION KEY                      
         ZAP   S1DCASH,PZERO                                                    
         TM    RPRGIND,RPRGHRSQ    TEST BATCH TOTALS IN HOURS                   
         BNZ   *+10                                                             
         ZAP   S1DCASH,TRNAMNT     SET CASH AMOUNT                              
         MVC   S1DSTAT,TRNSTAT     SET TRANSACTION STATUS                       
         SR    R0,R0                                                            
         USING SCIELD,R4                                                        
RCVR22   IC    R0,SCILN            LOOK FOR F/CURRENCY, PERSON ID ELS           
         AR    R4,R0                                                            
         CLI   SCIEL,0                                                          
         BE    RCVR30                                                           
         CLI   SCIEL,PIDELQ                                                     
         BNE   *+14                                                             
         MVC   S1KPID,PIDNO-PIDELD(R4)  SET PID NUMBER IN SORTKEY               
         B     RCVR22                                                           
         CLI   SCIEL,TIDELQ                                                     
         BNE   *+14                                                             
         MVC   S1DLUID,TID-TIDELD(R4)   SET LUID IN SORTDATA                    
         B     RCVR22                                                           
         TM    RPRGIND,RPRGHRSQ    TEST BATCH TOTALS IN HOURS                   
         BZ    RCVR24                                                           
         CLI   SCIELD,SCIELQ                                                    
         BNE   RCVR24                                                           
         CLI   SCITYPE,SCITSJHR                                                 
         BNE   RCVR24                                                           
         ZAP   S1DCASH,SCIAMNT     TAKE HOURS AS CASH FIGURE                    
         B     RCVR22                                                           
RCVR24   CLI   SCIEL,AFCELQ                                                     
         BNE   RCVR22                                                           
         CLI   S1KTYP,BT30         TEST DEBTORS BATCH                           
         BE    *+12                                                             
         CLI   S1KTYP,BT36         TEST MARKER BATCH                            
         BNE   RCVR22                                                           
         MVC   S1DAFCD,AFCEL-AFCELD(R4)  PASS WHOLE ELEMENT                     
         MVI   S1DIND,S1DIAFC      SET AFCEL PRESENT IN SORTREC                 
         B     RCVR22                                                           
         DROP  RF                                                               
*                                                                               
RCVR26   L     RF,SAVERF                                                        
         B     RCVR06                                                           
*                                                                               
RCVR30   OC    S1KPID,S1KPID                                                    
         BNZ   *+10                                                             
         MVC   S1KPID,RCVTERM                                                   
                                                                                
         GOTO1 ASORTPT,DMCB,(1,S1REC)  PUT RECORD TO SORT                       
         B     EXIT                                                             
         DROP  R1,R2,R3,R4                                                      
         EJECT                                                                  
***********************************************************************         
* CREATE BATCHES FROM SORTED RECOVERY RECORDS, WORKER FILES,          *         
* OLD BT7/BT48/BT57 BATCHES, AND INTERAGENCY POSTINGS                 *         
***********************************************************************         
*                                                                               
LAST     CLI   CRE8BAT,YES         TEST CREATE ALL NON-INPUT BATCHES            
         BE    LAST02                                                           
         CLI   CRE8BAT,RCVRY       TEST CREATE RECOVERY BATCHES ONLY            
         BE    LAST02                                                           
         CLI   CRE8BAT,XRCVR       TEST CREATE ALL EXCEPT RECOVERY BATS         
         BE    LAST06                                                           
         B     PROBAT                                                           
*                                                                               
***********************************************************************         
* RECOVERY FILE BATCHES                                               *         
***********************************************************************         
*                                                                               
LAST02   CLI   SORTIND,0           TEST FIRST SORT INITIALIZED                  
         BE    LAST06                                                           
         GOTO1 ABATBLD             CREATE BATCHES FOR RECOVERY POSTINGS         
         CLI   CRE8BAT,RCVRY       IF PROCESSING RECOVERY FILE ONLY             
         BNE   LAST06                                                           
         MVI   FORCEHED,C'Y'                                                    
         B     PROBAT              SKIP OTHER NON-INPUT TYPES                   
*                                                                               
***********************************************************************         
* WORKER FILE BATCHES                                                 *         
***********************************************************************         
*                                                                               
LAST06   XC    WKID,WKID                                                        
*&&UK*&& B     LAST30              NOT USED IN UK                               
         MVI   SRCIND,WRKFIL       SET SOURCE FOR REPORT HEADS                  
*                                                                               
LAST08   LA    R1,IOWKNDX+IOWORKER+IO4                                          
         GOTO1 AIO                                                              
         BNE   LAST30              EOF                                          
         CLI   WKITYPE,WKITPOST    TEST POSTING FILE                            
         BNE   LAST08                                                           
         CLI   WKISTAT,WKISACTV    TEST ACTIVE STATUS ONLY                      
         BNE   LAST08                                                           
         L     R3,AWKRTAB          TABLE OF WORKER BATCH TYPES                  
         USING WKRTABD,R3                                                       
LAST10   CLI   WKRTPRG,0           EOT                                          
         BE    LAST08                                                           
         CLC   WKISPRG,WKRTPRG                                                  
         BE    LAST12                                                           
         LA    R3,WKRTABL(R3)                                                   
         B     LAST10                                                           
LAST12   TM    WKRTIND,WKRTISP     TEST ONLY IF SPECIFIED IN PROFILES           
         BZ    LAST14                                                           
*                                  *TEST WKFILE LIST IN PROFILE HERE*           
*                                                                               
LAST14   GOTO1 AGETUSR,WKIUSER     READ USER-ID DETAILS                         
         BNE   LAST08              SENO DIFFERS - NOT FOR THIS ACC FILE         
         MVC   SWKIKEY,WKIKEY      SAVE INDEX KEY                               
         XC    ITEMSEQ,ITEMSEQ     INITIALIZE ITEM COUNT                        
         XC    ASKLSEQ,ASKLSEQ     INITIALIZE ASKEL COUNT                       
         ZAP   BATCASH,PZERO       INITIALIZE BATCH HDR CASH ACCUM              
         L     R4,AIO4                                                          
         LA    R4,4(R4)            R4=A(WORKER REC)                             
LAST16   LA    R1,IOWKRD+IOWORKER+IO4                                           
         GOTO1 AIO                                                              
         BNE   LAST22              END OF WORKER FILE                           
         USING PSHEADD,R4                                                       
         CLI   PSHDEL,PSHDELQ      TEST POSTING HEADER                          
         BNE   LAST16                                                           
         OC    ITEMSEQ,ITEMSEQ     IF FIRST FOR BATCH                           
         BNZ   LAST18                                                           
         GOTO1 AWBKBLD             BUILD BATCH RECORD KEY                       
         MVC   IOKEY,SBLDKEY                                                    
         LA    R1,IORDD+IOACCDIR+IO5 TEST READ FOR BATCH KEY                    
         GOTO1 AIO                                                              
         BE    LAST08              BATCH ALREADY EXISTS - CAN'T PROCESS         
         TM    IOERR,IOERNF        TEST NOT FOUND                               
         BNZ   LAST18              OK                                           
         TM    IOERR,IOEDEL        TEST EXISTS BUT DELETED                      
         BZ    LAST08              NO                                           
LAST18   GOTO1 ATRNBLD             BUILD AND ADD TRANSACTION                    
         BE    *+6                                                              
         DC    H'0'                ADDTRN RETURNED ERROR                        
*                                                                               
LAST20   GOTO1 AITMBLD,0           ADD ASKEL TO BATCH ITEM REC                  
         BE    LAST16              GET NEXT TRANSACTION                         
         GOTO1 AITMADD             ITEM FULL - WRITE IT                         
         B     LAST20              NOW GO BACK AND START NEW ITEM               
*                                                                               
LAST22   OC    ASKLSEQ,ASKLSEQ     TEST ITEM PENDING                            
         BZ    LAST24                                                           
         GOTO1 AITMADD             ADD LAST ITEM                                
LAST24   LA    R1,DRAFTQ           ADD BATCH HEADER WITH DRAFT STATUS           
         TM    WKRTIND,WKRTACR     TEST ADD AS AN ACCRUAL                       
         BZ    *+8                                                              
         LA    R1,DRAFTQ+ACCRUQ                                                 
         GOTO1 AHDRADD                                                          
         XC    WKID,WKID                                                        
         MVC   WKIKEY,SWKIKEY      RE-READ WORKER INDEX                         
         LA    R1,IOWKNDX+IOWORKER+IO4                                          
         GOTO1 AIO                                                              
         BE    *+12                                                             
         BAS   RE,IOTERR           PRINT ERROR TRACE                            
         B     LAST08                                                           
         LA    R1,IOWKEEP+IOWORKER+IO4                                          
         GOTO1 AIO                 AND SET STATUS TO KEEP                       
         BE    *+8                                                              
         BAS   RE,IOTERR           PRINT ERROR TRACE                            
         B     LAST08                                                           
         DROP  R3,R4                                                            
*                                                                               
***********************************************************************         
* BT7/BT48/BT57 OLD-STYLE BATCHES                                     *         
***********************************************************************         
*                                                                               
LAST30   XC    IOKEY,IOKEY                                                      
         MVI   SRCIND,OLDBAT       SET SOURCE FOR REPORT HEADS                  
         LA    R2,IOKEY                                                         
         USING BATRECD,R2                                                       
         MVI   BATKTYP,BATKTYPQ                                                 
LAST32   LA    R2,IOKEY                                                         
         MVI   BATKEY+L'BATKEY-1,EFFS                                           
         LA    R1,IOHI+IOACCDIR+IO2                                             
         GOTO1 AIO                                                              
         BNE   LAST60                                                           
LAST34   CLI   BATKTYP,BATKTYPQ                                                 
         BNE   LAST60              NO/END OF BATCH RECORDS                      
         CLI   BATKTYPE,BT07                                                    
         BE    LAST44                                                           
         BH    LAST36                                                           
         MVI   BATKTYPE,BT07       SET INPUT TYPE                               
         B     LAST40                                                           
*                                                                               
LAST36   CLI   BATKTYPE,BT48                                                    
         BE    LAST44                                                           
         BH    LAST38                                                           
         MVI   BATKTYPE,BT48       SET INPUT TYPE                               
         B     LAST40                                                           
*                                                                               
LAST38   CLI   BATKTYPE,BT57                                                    
         BE    LAST44                                                           
         BH    LAST42                                                           
         MVI   BATKTYPE,BT57       SET INPUT TYPE                               
LAST40   XC    BATKDATE(L'BATKDATE+L'BATKREF),BATKDATE                          
         B     LAST32                                                           
*                                                                               
LAST42   SR    RF,RF                                                            
         IC    RF,BATKGRUP         FORCE NEXT GROUP OR OFFICE OR CPY            
         LA    RF,1(RF)                                                         
         STC   RF,BATKGRUP                                                      
         XC    BATKTYPE(L'BATKTYPE+L'BATKDATE+L'BATKREF),BATKTYPE               
         B     LAST32                                                           
*                                                                               
LAST44   TM    BATKSTAT,BATSDELT+BATSNOK+BATSUPD+BATSRECV                       
         BNZ   LAST32                                                           
         CLC   BATKDATE,TODAYP                                                  
         BNE   LAST32                                                           
         OC    BATSPRG(L'BATSPRG+L'BATSDAT),BATSPRG                             
         BNZ   LAST32              ALREADY UPDATED                              
         LA    R1,IOGET+IOACCMST+IO2                                            
         GOTO1 AIO                                                              
         BE    *+12                                                             
         BAS   RE,IOTERR           PRINT ERROR TRACE                            
         B     LAST32                                                           
         XC    ITEMSEQ,ITEMSEQ     INIT BATCH ITEM SEQUENCE                     
         XC    ASKLSEQ,ASKLSEQ     INIT ASKEL SEQUENCE                          
         ZAP   BATCASH,PZERO       CLEAR BATCH CASH ACCUMULATOR                 
         ZAP   ITMCASH,PZERO       CLEAR ITEM  CASH ACCUMULATOR                 
         XC    SBLDKEY,SBLDKEY                                                  
         CLC   BATKCPY,SCPYCPY                                                  
         BE    LAST46                                                           
         GOTO1 AGETCPY,BATKCPY                                                  
         BNE   LAST32                                                           
LAST46   GOTO1 ANBKBLD             BUILD NEW STYLE BATCH KEY                    
LAST48   L     R2,AIO2                                                          
         LA    R2,BATRFST                                                       
         SR    R0,R0                                                            
         USING BTIELD,R2                                                        
LAST50   CLI   BTIEL,0                                                          
         BE    LAST56                                                           
         CLI   BTIEL,BTIELQ        TEST BATCH ITEM ELEMENT                      
         BE    LAST54                                                           
LAST52   IC    R0,BTILN                                                         
         AR    R2,R0                                                            
         B     LAST50                                                           
*                                                                               
LAST54   OC    BTIDA,BTIDA                                                      
         BZ    LAST52                                                           
         MVC   IODAOVER,BTIDA      READ ACCDAY RECORD                           
         LA    R1,IODMDIR+IOACCDAY+IO4                                          
         GOTO1 AIO                                                              
         BNE   LAST52                                                           
         L     RF,AIOBUFF                                                       
         LH    RE,0(RF)                                                         
         AR    RF,RE                                                            
         MVI   0(RF),0                                                          
         GOTO1 AADDITE             BUILD/ADD XACTIONS AND BATCH ITEM/S          
         B     LAST52                                                           
*                                                                               
LAST56   L     R2,AIO2                                                          
         USING BATRECD,R2                                                       
         MVC   BATRPRG,RCPROG      MARK OLD BATCH AS PROCESSED BY BG            
         MVC   BATRDAT,TODAYC                                                   
         LA    R1,IOPUT+IOACCMST+IO2                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,BATKEY                                                        
         LA    R2,IOKEY            UPDATE BATCH DIRECTORY REC                   
         MVC   BATKEY,0(R1)                                                     
         MVC   BATKSTA,BATRSTA-BATRECD(R1)                                      
         MVC   BATKDA,IODA                                                      
         LA    R1,IOWRITE+IOACCDIR+IO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,OBHDCHA       MAINTAIN OLD BAT HDRS CHANGED COUNT          
         LA    RF,1(RF)                                                         
         STCM  RF,15,OBHDCHA                                                    
         LA    R1,IORD+IOACCDIR+IO2   RE-READ OLD BATCH KEY                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOSEQ+IOACCDIR+IO2  READ NEXT                                 
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOSAVKEY,IOKEY      SAVE THE OLD BATCH RECORD KEY                
         CLC   BATKEY(L'BATKEY-1),IOKEYSAV  TEST SAME BATCH                     
         BNE   LAST58                                                           
         LA    R1,IOGET+IOACCMST+IO2                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     LAST48              CONTINUE PROCESSING SAME BATCH               
*                                                                               
LAST58   OC    ITEMSEQ,ITEMSEQ     TEST ANYTHING ADDED                          
         BZ    LAST34                                                           
         GOTO1 AHDRADD,DRAFTQ      ADD BATCH HEADER WITH DRAFT STATUS           
         MVC   IOKEY,IOSAVKEY                                                   
         LA    R1,IORD+IOACCDIR+IO2                                             
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     LAST34                                                           
*                                                                               
***********************************************************************         
* INTERAGENCY POSTINGS BATCHES                                        *         
***********************************************************************         
*                                                                               
LAST60   XC    IOKEY,IOKEY                                                      
         MVI   SRCIND,INTAGY       SET SOURCE FOR REPORT HEADS                  
         LA    R2,IOKEY                                                         
         USING IDJRECD,R2                                                       
         MVI   IDJKTYP,IDJKTYPQ                                                 
         MVI   IDJKSUB,IDJKSUBQ    READ PASSIVE PTR                             
         XC    IDJKDATE,IDJKDATE                                                
LAST62   MVI   IDJKEY+L'IDJKEY-1,EFFS                                           
         LA    R1,IOHI+IOACCDIR+IO1                                             
         GOTO1 AIO                                                              
         BNE   LAST70                                                           
         CLI   IDJKTYP,IDJKTYPQ                                                 
         BNE   LAST70                                                           
         CLI   IDJKSUB,IDJKSUBQ                                                 
         BNE   LAST70                                                           
         CLC   IDJKDATE,TODAYP     WANT TODAY OR EARLIER ONLY                   
         BH    LAST70                                                           
         TM    IDJKSTAT,X'80'      TEST DELETED                                 
         BNZ   LAST62                                                           
         OC    IDJKPDAT,IDJKPDAT   TEST ALREADY POSTED                          
         BNZ   LAST62                                                           
         LA    R1,IOGET+IOACCMST+IO1                                            
         GOTO1 AIO                                                              
         BE    *+12                                                             
         BAS   RE,IOTERR           PRINT ERROR TRACE                            
         B     LAST62                                                           
         L     R3,AIO1                                                          
         USING INTRECD,R3                                                       
         XC    S2REC(S2RECL),S2REC                                              
         MVC   S2KCPY,IDJKCPY                                                   
         MVC   S2KIAK,INTKCULA                                                  
         MVC   S2DDA,IODA                                                       
         MVC   S2DSTA,INTRSTA                                                   
         MVC   S2KDATP,IDJKDATE                                                 
         GOTO1 DATCON,DMCB,(1,IDJKDATE),(2,S2DDATC)                             
         SR    R0,R0                                                            
         LA    R3,INTRFST                                                       
         USING IPRELD,R3                                                        
LAST64   CLI   IPREL,0             LOCATE INTAGY PROFILE EL                     
         BE    LAST62                                                           
         CLI   IPREL,IPRELQ                                                     
         BE    *+14                                                             
         IC    R0,IPRLN                                                         
         AR    R3,R0                                                            
         B     LAST64                                                           
         MVC   S2KUID,IPRORIG                                                   
*                                  LOCATE INTAGY ESTIMATE ELEMENT(S)            
         USING IESEL,R3                                                         
LAST66   IC    R0,IESLN                                                         
         AR    R3,R0                                                            
         CLI   IESEL,0                                                          
         BE    LAST62                                                           
         CLI   IESEL,IESELQ                                                     
         BNE   LAST66                                                           
         CLC   IESDTO,S2DDATC      TEST CORRECT ELEMENT                         
         BNE   LAST66                                                           
         MVC   S2KMOS,IESMTH                                                    
*                                  PUT A SORT RECORD FOR EACH IESMTH            
         GOTO1 ASORTPT,DMCB,(2,S2REC)                                           
         B     LAST66                                                           
*                                                                               
LAST70   CLI   SORTIND,2           TEST ANY INTAGY POSTINGS FOUND               
         BNE   LAST90                                                           
         GOTO1 AIAGBAT             GENERATE INTAGY BATCHES                      
*                                                                               
LAST90   CLI   GPFLAG,0            TEST ANYTHING PROCESSED                      
         BE    PROBAT              NO                                           
         MVI   FORCEHED,C'Y'       PRESET NEW REPORT PAGE                       
         L     R1,ATRNBLK                                                       
         USING TRNBLKD,R1                                                       
         OI    TRNINDS,TRNILAST    SET LAST CALL TO ADDTRN                      
         OI    TRNINDS2,TRNIUPDG                                                
         GOTO1 VADDTRN                                                          
         NI    TRNINDS,255-TRNILAST  SWITCH OFF LAST CALL BIT                   
*                                  FINISHED WITH THE SPECIALS,                  
         B     PROBAT              NOW PROCESS BATCHES ON FILE                  
         DROP  R1,R2,R3                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESSING CONTROL FOR BATCHES ON FILE                              *         
***********************************************************************         
PROBAT   MVI   PROIND,0            CLEAR PROCESSING INDICATOR                   
         MVI   SRCIND,NUBATS       SET SOURCE FOR REPORT HEADS                  
         XC    SBATCPY,SBATCPY                                                  
         XC    SBATUSR,SBATUSR                                                  
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING TBARECD,R2                                                       
         MVI   TBAPTYP,TBAPTYPQ    READ PASSIVE                                 
PROB01   LA    R1,IOHI+IOACCDIR+IO2                                             
         B     *+8                                                              
PROB02   LA    R1,IOSEQ+IOACCDIR+IO2  READ NEXT PASSIVE POINTER                 
         GOTO1 AIO                                                              
         CLI   TBAPTYP,TBAPTYPQ                                                 
         BE    PROB06                                                           
         OI    PROIND,PROIEBAT     SET END OF BATCHES                           
         L     R1,ATRNBLK                                                       
         OI    TRNINDS-TRNBLKD(R1),TRNICONV+TRNILAST                            
         OI    TRNINDS2-TRNBLKD(R1),TRNIUPDG                                    
         GOTO1 VADDTRN             CALL ADDTRN TO WRITE ACCOUNT ETC.            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 ABLDWDJ             BUILD FINAL ADJ WORKER FILE                  
         GOTO1 ABLDWDK             BUILD FINAL ADK WORKER FILE                  
         GOTO1 AREPSUM             PRINT SUMMARY ADJ/ADK TOTALS PAGE            
         GOTO1 AGAPEXP                                                          
         B     EXIT                END OF BATCH RECORDS, END OF RUN             
*                                                                               
PROB06   CLC   TBAPEFDT,BLODAT     TEST WITHIN DATE RANGE                       
         BNL   PROB08                                                           
         MVC   TBAPEFDT,BLODAT     SET LOW DATE                                 
         XC    TBAPGRUP(TBAPLAST-TBAPGRUP),TBAPGRUP                             
         B     PROB01                                                           
*                                                                               
PROB08   CLC   TBAPEFDT,BHIDAT                                                  
         BNH   *+14                                                             
         MVC   TBAPEFDT,EFFALL     FORCE NEXT UID READ                          
         B     PROB02                                                           
*                                                                               
         CLC   TBAHKADT,TODAYC     TEST BATCH WAS ADDED AFTER 'TODAY'           
         BH    PROB02              MUST BE RE-RUNNING WITH A DATE CARD          
         CLI   CRE8BAT,RCVRY       TEST PROCESSING RECOVERY ONLY                
         BNE   PROB09                                                           
         CLC   TBAHKUDT,TODAYC     TEST BATCH WAS UPDATED TODAY                 
         BNE   PROB02                                                           
         TM    TBAHKIND,TBAHIGDJ   AND WAS GENERATED BY BG                      
         BZ    PROB02                                                           
PROB09   LA    R1,IOGET+IOACCMST+IO2                                            
         GOTO1 AIO                                                              
         BE    PROB10                                                           
         BAS   RE,IOTERR           PRINT ERROR TRACE                            
         B     PROB02                                                           
*                                                                               
PROB10   MVC   SBLDKEY,TBAPAS                                                   
         GOTO1 AGETTYP                                                          
         BNE   PROB02                                                           
         MVC   SBATPP,IOKEY        SAVE BATCH HEADER PASSIVE                    
         LA    R2,SBATKEY          BUILD SAVED BATCH KEY FROM DATA REC          
         L     RF,AIOBUFF                                                       
         MVC   TBAKEY,0(RF)                                                     
         MVC   TBAKSTA,TBARSTA-TBARECD(RF)                                      
         MVC   TBAKDA,IODA                                                      
*                                                                               
         LA    R2,IOKEY                                                         
         CLC   SBATUSR,TBAPUSER    TEST USER-ID CHANGE                          
         BNE   PROB12                                                           
         TM    PROIND,PROIXUSR     TEST USER FILTERED OUT                       
         BNZ   PROB02                                                           
         B     PROB30                                                           
*                                  * PROCESS LAST FOR PRECEDING USER            
PROB12   OC    SBATUSR,SBATUSR                                                  
         BZ    PROB18              FIRST USER, FIRST COMPANY                    
         GOTO1 ABLDWDJ             BUILD ADJ WORKER FILE, RE-INIT SORT          
         GOTO1 ABLDWDK             BUILD ADK WORKER FILE, CLEAR ACMTAB          
         MVI   FORCEHED,C'Y'       NEW PAGE FOR REPORT                          
*                                                                               
PROB14   CLC   SBATCPY,TBAPCPY     TEST COMPANY CHANGE                          
         BNE   PROB16                                                           
         TM    PROIND,PROIXCPY     TEST CPY FILTERED OUT                        
         BNZ   PROB02                                                           
         B     PROB26                                                           
*                                                                               
PROB16   DS    0H                                                               
PROB18   MVC   SBATCPY,TBAPCPY                                                  
         MVI   SCPYST5,0                                                        
         MVI   PROIND,0                                                         
         MVC   REPCPY,SPACES                                                    
         GOTO1 AGETCPY,SBATCPY                                                  
         XOUT  SBATCPY,REPCPY,1                                                 
         MVC   REPCPY+10(L'SCPYNAM),SCPYNAM                                     
*                                                                               
PROB20   OC    FLTALFA,FLTALFA     TEST ALPHA ID SPECIFIED                      
         BZ    PROB26                                                           
         CLC   SCPYALF,FLTALFA                                                  
         BE    PROB26                                                           
         OI    PROIND,PROIXCPY     SET SKIP ALL UNTIL NEXT COMPANY              
         B     PROB02                                                           
*                                  * PROCESS FIRST FOR THIS USER                
PROB26   MVC   SBATUSR,TBAPUSER                                                 
         NI    PROIND,255-PROIXUSR                                              
         OC    FLTUSR,FLTUSR       TEST USER-ID SPECIFIED                       
         BZ    PROB28                                                           
         CLC   TBAPUSER,FLTUSR                                                  
         BE    PROB28                                                           
         OI    PROIND,PROIXUSR     SET SKIP ALL UNTIL NEXT USER                 
         B     PROB02                                                           
*                                                                               
PROB28   GOTO1 AGETUSR,SBATUSR     GET USER-ID AND NAME INTO REPUSR             
         GOTO1 ARDPROF             READ PROFILES FOR USER/CPY                   
*                                                                               
PROB30   GOTO1 AFLTBAT             FILTER/CLASSIFY BATCH                        
         BNE   PROB38              NOT WANTED                                   
*                                                                               
         L     R3,AIO2             LOCATE BATCH HEADER ELEMENT                  
         LA    R3,TBARFST-TBARECD(R3)                                           
         USING BHDELD,R3                                                        
PROB32   CLI   BHDEL,0                                                          
         BE    PROB02              BAD BATCH                                    
         CLI   BHDEL,BHDELQ                                                     
         BE    PROB34                                                           
         IC    R0,BHDLN                                                         
         AR    R3,R0                                                            
         B     PROB32                                                           
PROB34   MVC   SBATITM,BHDITEMA    SET ACTUAL ITEM COUNT                        
         MVC   SBATST1,BHDSTAT1    SET HEADER STATUS                            
         DROP  R3                                                               
*                                                                               
         GOTO1 AMRKBAT             PROCESS ALL ACCOUNT FILE BATCH TYPES         
*                                                                               
         TM    SBATST1,BHDSACRU    TEST ACCRUAL BATCH                           
         BZ    PROB36              NO                                           
         OC    STYPARN,STYPARN     TEST PRE-DEFINED                             
         BNZ   PROB36              YES                                          
         MVC   REPCMNT(L'AC@ACRL),AC@ACRL  SET ACCRUAL DEFINED BY USER          
PROB36   L     R1,AIO2                                                          
         GOTO1 AREPBAT                                                          
         TM    SBATST1,BHDSACRU    TEST ACCRUAL BATCH                           
         BZ    PROB38              NO                                           
         CLI   FBATIND,0           TEST ANY ERRORS                              
         BNE   PROB38                                                           
         CLI   FBATTYP,FBATOVUP    TEST OFFLINE                                 
         BE    *+12                                                             
         CLI   FBATTYP,FBATDRLI    TEST DRAFT GOING LIVE                        
         BNE   PROB38                                                           
         GOTO1 AACRREV             GENERATE ACCRUAL REVERSAL BATCH              
         MVC   REPCMNT(L'AC@ACRR),AC@ACRR                                       
         MVI   FBATTYP,0           CLEAR CLASSIFICATION BEFORE PRINTING         
         MVI   FBATIND,0                                                        
         L     R1,AIO2                                                          
         GOTO1 AREPBAT                                                          
PROB38   MVC   IOKEY,SBATPP        RESTORE BATCH HEADER PASSIVE                 
         LA    R2,IOKEY                                                         
         GOTO1 AIO,IOHID+IOACCDIR+IO2                                           
         BE    PROB02              MAKE SURE I GET THE NEXT RECORD              
         TM    IOERR,IOEDEL        DELETE IS OK                                 
         BNZ   PROB02                                                           
         DC    H'0'                ANY OTHER ERROR IS NOT                       
         EJECT                                                                  
***********************************************************************         
* SUBROUTINES - BRANCH INDEX IN H.O.B. RF                             *         
***********************************************************************         
         DS    0D                                                               
ROUT     NMOD1 0,**ROUT**,R9,R7,R6,R5                                           
         L     RC,4(RD)                                                         
         L     RA,60(RC)                                                        
         L     RC,68(RC)                                                        
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     ACRREV              GENERATE ACCRUAL REVERSAL BATCH              
         B     ADDITE              ADD BATCH ITEM/TRANS FROM ACCDAY REC         
         B     ADDMON              INCREMENT PWOS YMD MONTH                     
         B     BATBLD              BUILD BATCH RECORD SET                       
         B     BLDACR              BUILD ACCRUAL REVERSAL BATCH HDR             
         B     BLDWDJ              BUILD ADJ WORKER FILE FROM SORTRECS          
         B     BLDWDK              BUILD ADK WORKER FILE FROM ACMTAB            
         B     DELBAT              SET ENTIRE BATCH AS DELETED                  
         B     FLTBAT              FILTER BATCH RECORDS                         
         B     GETCPY              READ COMPANY RECORD, SAVE VALUES             
         B     GETTYP              FIND AND SAVE TYPTAB BATCH TYPE NTRY         
         B     GETUSR              READ CTFILE USERID REC, SAVE ID+NAME         
         B     HDRADD              ADD BATCH HEADER TO ACCFILE                  
         B     IAGBAT              GENERATE INTERAGENCY POSTING BATCH           
         B     IBKBLD              BUILD BATCH KEY FROM INTAGY RECORD           
         B     IGETDU              GET INTERAGENCY POSTING DUE DAYS             
         B     ITRNAD              POST INTAGY TRANSACTIONS, BATCH ITEM         
         B     IOEXEC              ACCOUNT & WORKER FILE I/O EXECUTIVE          
         B     IOTRACE             PRINT TRACE FOR ADDTRN/IOEXEC                
         B     ITMADD              ADD A BATCH ITEM RECORD TO ACCFILE           
         B     ITMBLD              BUILD A BATCH ITEM RECORD                    
         B     IVALAC              VALIDATE A/CS FOR INTAGY POSTINGS            
         B     MRKBAT              MARK BATCH RECORDS FOR DJ REPORTING          
         B     NBKBLD              BUILD BATCH KEY FROM X'0B' BATCH REC         
         B     PACTST              VALIDATE PACKED FIELD                        
         B     RBKBLD              BUILD BATCH KEY FROM RECOVERY DATA           
         B     RDPROF              READ/STORE USER PROFILES                     
         B     REPBAT              PRINT BATCHES PROCESSED REPORT LINE          
         B     REPSUM              PRINT ADJ/ADK SUMMARY TOTALS PAGE            
         B     SORTPT              INITIALISE/PUT RECORDS TO SORTER             
         B     TABMNT              MAINTAIN U/L ACCUMS TABLE                    
         B     TRNBLD              BUILD A TRANSACTION                          
         B     TSTBMO              TEST BATCH MOA ACCESSIBILITY                 
         B     UPDBAT              SET DRAFT BATCH AS LIVE                      
         B     UPDHDR              UPDATE AND RE-WRITE BATCH HEADER             
         B     WBKBLD              BUILD BATCH KEY FROM WORKER POSTING          
         B     ORDAUD              UPDATE ORDER AUDIT HISTORY                   
         B     GAPEXP              CHECK FOR EXPIRED GAP ORDERS                 
         B     SUPGAP              READ SUPPL RECORD TO CHECL GAP STGS          
*                                                                               
ROUTL    CLI   *,255               SET CC LOW                                   
         B     ROUTX                                                            
*                                                                               
ROUTH    CLI   *,0                 SET CC HIGH                                  
         B     ROUTX                                                            
*                                                                               
ROUTE    CLI   *+1,0               SET CC EQUAL                                 
*                                                                               
ROUTX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* PRINT MESSAGE AND TRACE AFTER I/O ERROR  ** NB - NOT A ROUT **      *         
***********************************************************************         
IOTER2   MVC   PSECOND+1(37),=C'*** ERROR - DRAFT TRANS COUNT -VE ***'          
IOTERR   MVC   P+1(34),=C'*** I/O ERROR AT      + 000000 ***'                   
         STM   RE,R1,BGREGS        SAVE CALLERS REGS                            
         SR    RE,RB               RE=DISPLACEMENT INTO CSECT                   
         ST    RE,FULL                                                          
         L     RE,4(RD)                                                         
         MVC   P+18(4),0(RE)                                                    
         XOUT  FULL+1,P+25,3                                                    
         GOTO1 ACREPORT                                                         
         CLI   RCTRACE,YES         DON'T TRACE AGAIN IF ALREADY DONE            
         BE    IOTEX                                                            
         LA    R0,DMCB                                                          
         GOTO1 AIOTRACE                                                         
IOTEX    LM    RE,R1,BGREGS                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ACCRUAL BATCHES: GENERATE REVERSED ACCRUAL BATCH FROM ACCRUAL BATCH *         
***********************************************************************         
         USING TBARECD,R2                                                       
ACRREV   L     R2,AIO2                                                          
         MVC   IOKEY,TBAKEY        SET KEY FROM HEADER RECORD                   
         LA    R2,SBATKEY                                                       
         XC    TBAKTSEQ,TBAKTSEQ   RESET SEQUENCE NUMBER                        
         LA    R2,IOKEY                                                         
         L     R1,ATRNBLK                                                       
         USING TRNBLKD,R1                                                       
         MVC   TRNEFDT,TODAYC      SET ADDTRN EFFECTIVE DATE                    
         DROP  R1                                                               
         MVI   ACRINDS,0                                                        
         GOTO1 ABLDACR             BUILD ACCRUAL RVRSL BAT HDR IN IO2           
         BNE   ACRVX               REVERSAL MOS IS LOCKED                       
         XC    ACRITM,ACRITM       REVERSAL BATCH ITEM COUNT                    
*                                                                               
         L     R3,AIO2             R3=A(ACCRUAL REVERSAL)                       
ACRV2    MVC   TBAPAS,TBAKEY-TBARECD(R3)                                        
         MVI   TBAPTYP,TBAPTYPQ                                                 
         MVC   TBAPEFDT,TODAYC                                                  
         MVC   TBAPTSEQ,TBAHRADT-TBARECD(R3)                                    
         LA    R1,IORDD+IOACCDIR+IO1  TEST REVERSL PASSIVE CAN BE ADDED         
ACRV4    GOTO1 AIO                                                              
         BNE   ACRV12              NOT FOUND - OK                               
         TM    TBAKHSTA,TBAHSDEL   ELSE TEST DELETED                            
         BZ    ACRV20              NO                                           
         OI    ACRINDS,ACRIPPD     SET PASSIVE FOUND BUT DELETED                
*                                                                               
ACRV12   XC    ACRIODA,ACRIODA                                                  
         MVC   TBAKEY,TBAKEY-TBARECD(R3)                                        
         MVC   TBAKADDT,TODAYCC                                                 
         LA    R1,IORDD+IOACCDIR+IO1  TEST REVERSL KEY CAN BE ADDED             
ACRV14   GOTO1 AIO                                                              
         BNE   ACRV24              NOT FOUND - OK                               
         TM    TBAKHSTA,TBAHSDEL   ELSE TEST DELETED                            
         BZ    ACRV20              NO                                           
         OI    ACRINDS,ACRIBHD     SET KEY FOUND BUT DELETED                    
         MVC   ACRIODA,IODA        SAVE D/A FOR UPDATE                          
         B     ACRV24                                                           
*                                                                               
ACRV20   IC    R1,TBAKSEQN-TBARECD(R3)  BUMP SEQUENCE IN HEADER RECORD          
         LA    R1,1(R1)                                                         
         STC   R1,TBAKSEQN-TBARECD(R3)                                          
         B     ACRV2                    AND START AGAIN                         
*                                                                               
ACRV24   L     R1,ATRNBLK                                                       
         USING TRNBLKD,R1                                                       
         MVC   TRNPUSER,TBAKUSER   USER-ID                                      
         MVC   TRNBMOS,ACRMOSP     PWOS P'YYMM'                                 
         DROP  R1                                                               
*                                                                               
ACRV26   CLC   SBATITM,SBATKEY+(TBAKTSEQ-TBARECD)                               
         BE    ACRV68              ALL ITEMS ACCOUNTED FOR                      
         SR    R1,R1                                                            
         ICM   R1,3,SBATKEY+(TBAKTSEQ-TBARECD)                                  
         LA    R1,1(R1)                                                         
         STCM  R1,3,SBATKEY+(TBAKTSEQ-TBARECD)                                  
         LA    R2,IOKEY                                                         
         MVC   TBAKEY,SBATKEY      SET FIRST/NEXT SOURCE ITEM KEY               
         LA    R1,IOREAD+IOACCDIR+IO1                                           
         GOTO1 AIO                                                              
         BE    *+12                                                             
         BAS   RE,IOTERR           PRINT ERROR TRACE OF RECORD                  
         B     ACRV26                                                           
         LA    R1,IOGET+IOACCMST+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOBUFF                                                       
         TM    TBARESTA,TBAESLDE   TEST LOGICALLY DELETED                       
         BNZ   ACRV26              YES - DROP THIS ITEM                         
         TM    TBARESTA,TBAESORD   TEST ITEM CARRIES ORDER(S)                   
         BZ    *+12                                                             
         BAS   RE,IOTERR           PRINT ERROR TRACE OF RECORD                  
         B     ACRV26              CANNOT ACCRUE BATCHES WITH ORDERS            
*                                                                               
         L     R1,AIO2             R1=A(NEW BATCH HEADER)                       
         MVC   TBAKEY,0(R1)        SET NEW KEY                                  
         SR    RF,RF                                                            
         ICM   RF,3,ACRITM         BUMP AND SET REVERSAL ITEM COUNT             
         LA    RF,1(RF)                                                         
         STCM  RF,3,ACRITM                                                      
         STCM  RF,3,TBAKTSEQ                                                    
*                                  PROCESS BATCH ITEM                           
         LA    R4,TBARFST                                                       
         USING BIAELD,R4                                                        
         SR    R0,R0                                                            
ACRV30   CLI   BIAEL,0             TEST EOR                                     
         BE    ACRV66                                                           
         CLI   BIAEL,BIAELQ                                                     
         BE    ACRV34                                                           
         CLI   BIAEL,BTAELQ                                                     
         BE    ACRV34                                                           
         CLI   BIAEL,BIOELQ                                                     
         BE    ACRV36                                                           
         CLI   BIAEL,BICELQ                                                     
         BE    ACRV38                                                           
         CLI   BIAEL,SFSELQ                                                     
         BE    ACRV40                                                           
         CLI   BIAEL,ASKELQ                                                     
         BE    ACRV42                                                           
*                                                                               
ACRV32   SR    R0,R0                                                            
         IC    R0,BIALN                                                         
         AR    R4,R0                                                            
         B     ACRV30                                                           
*                                                                               
ACRV34   ZAP   DUB,BIAAMT                                                       
         MP    DUB,=P'-1'                                                       
         ZAP   BIAAMT,DUB                                                       
         MVI   BIAREF+L'BIAREF-1,C'R'                                           
         B     ACRV32                                                           
*                                                                               
         USING BIOELD,R4                                                        
ACRV36   BAS   RE,IOTERR           PRINT ERROR TRACE                            
         B     ACRV26              CANNOT SUPPORT ORDERS - DROP ITEM            
*                                                                               
         USING BICELD,R4                                                        
ACRV38   ZAP   DUB,BICAMT                                                       
         MP    DUB,=P'-1'                                                       
         ZAP   BICAMT,DUB                                                       
         B     ACRV32                                                           
*                                                                               
         USING SFSELD,R4                                                        
ACRV40   B     ACRV32              NO ACTION FOR SFSELS                         
*                                  PROCESS POSTING ON ITEM RECORD               
         USING ASKELD,R4                                                        
ACRV42   LA    R2,IOKEY                                                         
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,ASKKEY                                                    
         LA    R1,IOREAD+IOACCDIR+IO3                                           
         GOTO1 AIO                                                              
         BE    *+12                                                             
         BAS   RE,IOTERR           PRINT ERROR TRACE                            
         B     ACRV32                                                           
         LA    R1,IOGET+IOACCMST+IO3                                            
         GOTO1 AIO                                                              
         BE    *+12                                                             
         BAS   RE,IOTERR           PRINT ERROR TRACE                            
         B     ACRV32                                                           
         L     R2,AIOBUFF          R2=A(TRANSACTION RECORD)                     
         NI    TRNRSTAT,255-(TRNSARCH)                                          
         NI    TRNRSTA2,255-(TRNSGLUP+TRNSUSED+TRNSPEEL)                        
         XC    TRNRSUSE,TRNRSUSE                                                
         SR    R0,R0                                                            
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         CLI   TRNEL,TRNELQ        TEST TRANSACTION ELEMENT FIRST               
         BE    *+12                                                             
         BAS   RE,IOTERR           BAD TRANSACTION - PRINT TRACE                
         B     ACRV32                                                           
         MVC   TRNMOS,ACRMOSC                                                   
         MVC   TRNBREF,SBATKEY+(TBAKBREF-TBARECD)                               
         OC    STYPARN,STYPARN     TEST PRE-DEFINED ACCRUAL BATCH               
         BZ    *+10                                                             
         MVC   TRNTYPE,STYPARN     SET SECOND BATCH TYPE                        
*&&US                                                                           
         LA    R1,TRNDATE                                                       
         GOTO1 AADDMON             BUMP TRANSACTION DATE BY 1 MONTH             
         MVI   TRNREF+L'TRNREF-1,C'R'                                           
         MVC   TRNKDATE,TRNDATE    SET NEW DATE IN RECORD KEY ALSO              
         MVC   TRNKREF,TRNREF                                                   
*&&                                                                             
         ZAP   DUB,TRNAMNT                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   TRNAMNT,DUB         REVERSE TRANSACTION AMOUNT                   
         LA    RF,TOFUIDDR                                                      
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+8                                                              
         LA    RF,TOFUIDCR                                                      
         AP    0(8,RF),DUB                                                      
         CLC   =C'SJ',TRNKUNT      TEST PRODUCTION                              
         BNE   ACRV46                                                           
         CLC   TRNKWORK,=C'99'     TEST BILL POSTING                            
         BNE   ACRV46                                                           
*&&UK                                                                           
         TM    SCPYST5,CPYSNVAT    TEST AGENCY ON NEW VAT                       
         BNZ   ACRV46              YES - MEMO AMOUNTS HELD SEPARATELY           
         LA    R0,13                                                            
*&&                                                                             
*&&US*&& LA    R0,3                                                             
         LA    R1,TRNNARR+15       ELSE MEMO AMOUNTS ARE IN NARRATIVE           
ACRV45   ICM   R1,8,=X'06'         L'PACKED FIELD                               
         GOTO1 APACTST             ENSURE ALL FIELDS ARE VALID PL6              
         BNE   ACRV46              NO - GIVE UP                                 
         ZAP   DUB,0(6,R1)                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   0(6,R1),DUB                                                      
         LA    R1,6(R1)                                                         
         BCT   R0,ACRV45                                                        
*                                                                               
ACRV46   SR    R0,R0                                                            
         IC    R0,TRNLN                                                         
         AR    R3,R0                                                            
ACRV47   CLI   TRNEL,0                                                          
         BE    ACRV64                                                           
         CLI   TRNEL,SCIELQ                                                     
         BE    ACRV48                                                           
         CLI   TRNEL,MPYELQ                                                     
         BE    ACRV52                                                           
         CLI   TRNEL,TRSELQ                                                     
         BE    ACRV54                                                           
         CLI   TRNEL,XPYELQ                                                     
         BE    ACRV58                                                           
         CLI   TRNEL,MDTELQ                                                     
         BE    ACRV60                                                           
         CLI   TRNEL,AFCELQ                                                     
         BE    ACRV63                                                           
         B     ACRV46                                                           
*                                                                               
         USING SCIELD,R3                                                        
ACRV48   ZAP   DUB,SCIAMNT                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   SCIAMNT,DUB         REVERSE SUBSIDIARY CASH AMOUNT               
         CLI   SCILN,SCILN2Q                                                    
         BL    ACRV46                                                           
         ZAP   DUB,SCIADMN                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   SCIADMN,DUB         REVERSE ADMINISTRATION AMOUNT                
         B     ACRV46                                                           
*                                                                               
ACRV52   GOTO1 VHELLO,DMCB,(C'D',ACCMST),('MPYELQ',TRNRECD),0                   
         B     ACRV47                                                           
*                                                                               
         USING TRSELD,R3                                                        
ACRV54   ICM   RE,3,TRSPMOS        SAVE ORIGINAL MOS                            
         IC    RF,TRSLN                                                         
         SH    RF,=Y(TRSDATE+1-TRSELD)                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    TRSDATE(0),TRSDATE  CLEAR TRSEL DATA                             
         MVC   TRSDATE,TODAYC      SET TODAY'S DATE                             
         STCM  RE,3,TRSRMOS        SET REVERSING TRANSACTION MOS                
         OI    TRSSTAT,TRSSREVS    SET 'REVERSE ME'                             
         OI    TRSSTAT2,TRSSACRV   SET ACCRUAL REVERSAL                         
         B     ACRV46                                                           
*                                                                               
         USING XPYELD,R3                                                        
ACRV58   ZAP   DUB,XPYCD           REVERSE CASH DISCOUNT                        
         MP    DUB,=P'-1'                                                       
         ZAP   XPYCD,DUB                                                        
         B     ACRV46                                                           
*                                                                               
         USING MDTELD,R3                                                        
ACRV60   LA    RE,MDTGRS           REVERSE ALL MEDIA TRANSFER AMOUNTS           
         LA    RF,(MDTFDTE-MDTGRS)/L'MDTGRS                                     
ACRV62   ICM   R0,15,0(RE)                                                      
         BZ    *+12                                                             
         MH    R0,=H'-1'                                                        
         STCM  R0,15,0(RE)                                                      
         LA    RE,4(RE)                                                         
         BCT   RF,ACRV62                                                        
         ICM   R0,15,MDTVAT                                                     
         MH    R0,=H'-1'                                                        
         STCM  R0,15,MDTVAT                                                     
         B     ACRV46                                                           
*                                                                               
         USING AFCELD,R3                                                        
ACRV63   ZAP   DUB,AFCAMNT         REVERSE FOREIGN CURRENCY AMOUNT              
         MP    DUB,=P'-1'                                                       
         ZAP   AFCAMNT,DUB                                                      
         B     ACRV46                                                           
*                                                                               
ACRV64   LA    R3,1(R3)            R3=EOR+1                                     
         SR    R3,R2                                                            
         STCM  R3,3,TRNRLEN        SET RECORD LENGTH                            
         OC    TRNKULA,SPACES                                                   
         OC    TRNKULC,SPACES                                                   
         L     R1,ATRNBLK                                                       
         USING TRNBLKD,R1                                                       
         MVC   TRNBSEQN,ACRITM     SET ADDTRN SEQUENCE NUMBER                   
         OI    TRNINDS,TRNICONV+TRNIDRFT+TRNIDUCN                               
         OI    TRNTIND1,TRNTIARV                                                
         OI    TRNINDS2,TRNIADDG                                                
         DROP  R1                                                               
         GOTO1 VADDTRN                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ASKKEY,TRNKEY       SET NEW KEY IN ASKEL                         
         GOTO1 ASORTPT,DMCB,(3,ASKKEY)                                          
         B     ACRV32                                                           
*                                  ADD ACCRUAL REVERSAL BATCH ITEM              
ACRV66   LA    R1,IOADDREC+IOACCMST+IO1                                         
         GOTO1 AIO                                                              
         BE    ACRV26                                                           
         TM    IOERR,IOEDUP        TEST ITEM ALREADY EXISTS                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1             YES, RE-USE IT (MUST BE DELETED)             
         LA    R2,IOKEY                                                         
         USING TBARECD,R2                                                       
         MVC   TBAKEY,0(R3)        READ IT                                      
         LA    R1,IORDD+IOACCDIR+IO5                                            
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOEDEL        CONFIRM ITEM HAS DELETE STATUS               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGETREC+IOACCMST+IO5                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIOARE5          COPY D/A AND WORK FROM GETREC                
         L     RE,AIOARE1          TO PUTREC                                    
         MVC   0(L'IODA+L'IOWORK,RE),0(RF)                                      
         LA    R1,IOPUTREC+IOACCMST+IO1                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TBAKSTA,TBARSTA-TBARECD(R3)                                      
         MVC   TBAKDA,IODA                                                      
         LA    R1,IOWRITE+IOACCDIR+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     ACRV26                                                           
*                                  ADD ACCRUAL REVERSAL BATCH REC               
ACRV68   LA    R2,IOKEY            R2=A(BATCH KEY)                              
         USING TBARECD,R2                                                       
         L     R3,AIO2             R3=A(BATCH DATA RECORD)                      
         SR    R0,R0                                                            
         LA    RF,TBARFST-TBARECD(R3)                                           
         USING BHDELD,RF                                                        
         CLI   BHDEL,BHDELQ                                                     
         BE    *+14                                                             
         IC    R0,BHDLN                                                         
         AR    RF,R0                                                            
         B     *-14                                                             
         MVC   BHDITEMA,ACRITM     SET TRUE ITEM COUNT                          
         XC    BHDDELIT,BHDDELIT   NO DELETED ITEMS ON REVERSAL BATCH           
         OC    ACRIODA,ACRIODA     TEST HEADER EXISTS                           
         BNZ   ACRV70              UPDATE IT                                    
         LA    R1,IOADDREC+IOACCMST+IO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TBAPAS,0(R3)        BUILD PASSIVE POINTER                        
         MVI   TBAPTYP,TBAPTYPQ                                                 
         MVC   TBAPEFDT,TBAHREDT-TBARECD(R3)                                    
         MVC   TBAKSTA,TBARSTA-TBARECD(R3)                                      
         MVC   TBAKDA,IODA                                                      
         LA    R1,IOADD+IOACCDIR+IO2                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     ACRVX                                                            
*                                                                               
ACRV70   MVC   IODAOVER,ACRIODA    SET DISK ADDRESS, GET DELETED REC            
         LA    R1,IOGETREC+IOACCMST+IO5                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIOARE5                                                       
         L     RE,AIOARE2                                                       
         MVC   0(L'IODA+L'IOWORK,RE),0(RF)  D/A AND WORK FROM GETREC            
         LA    R1,IOPUTREC+IOACCMST+IO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TBAKEY,0(R3)        UPDATE DIRECTORY RECORD                      
         LA    R1,IORDD+IOACCDIR+IO2                                            
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   TBAKSTA,TBARSTA-TBARECD(R3)                                      
         MVC   TBAKDA,ACRIODA                                                   
         LA    R1,IOWRITE+IOACCDIR+IO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TBAPTYP,TBAPTYPQ    UPDATE PASSIVE POINTER                       
         MVC   TBAPEFDT,TBAHREDT-TBARECD(R3)                                    
         MVC   TBAPTSEQ,TBAHRADT-TBARECD(R3)                                    
         TM    ACRINDS,ACRIPPD     TEST PP EXISTS                               
         BZ    ACRV72                                                           
         LA    R1,IORDD+IOACCDIR+IO2                                            
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   TBAKSTA,TBARSTA-TBARECD(R3)                                      
         MVC   TBAKDA,ACRIODA                                                   
         LA    R1,IOWRITE+IOACCDIR+IO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     ACRVX                                                            
*                                                                               
ACRV72   MVC   TBAKSTA,TBARSTA-TBARECD(R3)                                      
         MVC   TBAKDA,ACRIODA                                                   
         LA    R1,IOADD+IOACCDIR+IO2                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ACRVX    B     ROUTE                                                            
         DROP  R2,R3,R4,RF                                                      
         EJECT                                                                  
***********************************************************************         
* ACCRUAL BATCHES: INCREMENT MONTH IN PWOS YMD OR YM0                 *         
* NTRY: R1=A(PWOS YMD/YM0)                                            *         
***********************************************************************         
*                                                                               
         USING YMDD,R2                                                          
ADDMON   LR    R2,R1                                                            
         MVC   DUB(2),YMDYEAR      EXTRACT YEAR/MONTH                           
         MVI   DUB+2,X'01'         SET DAY (YMDDAY MAY NOT BE SET)              
         GOTO1 DATCON,DMCB,(X'31',DUB),(1,DUB+3),(2,0)                          
         MVC   YMDYEAR(2),DUB+3    SET NEXT MONTH, LEAVE DAY INTACT             
*                                                                               
         SR    RE,RE               ADJUST DAYS                                  
         IC    RE,YMDMONTH         TAKE MONTH                                   
         LA    RE,MTHDAYS-1(RE)    RE=A(DAYS IN MONTH)                          
         CLC   YMDDAY,0(RE)        TEST NOT EXCEEDED                            
         BNH   ADDMX                                                            
         MVC   DUB(2),YMDYEAR      EXTRACT YEAR/MONTH                           
         MVI   DUB+2,X'01'         SET DUMMY VALID DAY                          
         GOTO1 DATCON,DMCB,(X'31',DUB),(1,DUB+3),(1,0)                          
         MVC   YMDDAY,DUB+5        SET LAST DAY OF THE MONTH                    
*                                                                               
ADDMX    B     ROUTE                                                            
         DROP  R2                                                               
*                                                                               
MTHDAYS  DC    X'312831303130313130000000000000313031'                          
*                 1 2 3 4 5 6 7 8 9            101112                           
         EJECT                                                                  
***********************************************************************         
* RECOVERY FILE BATCHES: BUILD AND ADD BATCH RECORDS FROM SORT RECORDS*         
***********************************************************************         
BATBLD   XC    LKEY,LKEY           SET FIRST TIME                               
         B     BATBLD8                                                          
*                                                                               
BATBLD2  OC    LKEY,LKEY           TEST FIRST TIME                              
         BZ    BATBLD3                                                          
         GOTO1 AITMADD             ADD THE ITEM RECORD BUILT IN IOA1            
         CLC   LKEY(S1KSL),S1KEY   TEST CHANGE OF BATCH                         
         BE    BATBLD4                                                          
         LA    R1,LIVEQ            SET 'ADD LIVE HEADER'                        
         TM    LKEY+(S1KIND-S1KEY),S1KINREV                                     
         BZ    *+8                                                              
         LA    R1,LIVEQ+NOREVQ     + 'CANNOT BE REVERSED' STATUS BIT            
         GOTO1 AHDRADD             ADD BATCH HEADER                             
         CLI   S1KEY,EFFS          TEST SORT EOF                                
         BE    ROUTX                                                            
*                                                                               
BATBLD3  MVC   LKEY(S1KEYL),S1KEY  SET CURRENT SORT KEY                         
         GOTO1 ARBKBLD             BUILD NEW BATCH KEY, GET TYPTAB NTRY         
*                                                                               
BATBLD4  MVC   STRNKEY,S1DTKEY     SET TRANS KEY                                
         SR    R1,R1                                                            
         TM    S1DIND,S1DIAFC      TEST AFCEL PRESENT                           
         BZ    BATBLD6                                                          
         LA    R1,S1DAFCD          R1=A(AFCEL) TO APPEND TO ITEM RECORD         
BATBLD6  GOTO1 AITMBLD             ADD TO/BUILD ITEM RECORD                     
         BNE   BATBLD2             ITEM FULL                                    
         AP    ITMCASH,S1DCASH                                                  
         TM    S1DSTAT,TRNSDR      TEST DEBIT POSTING                           
         BZ    *+10                                                             
         AP    BATCASH,S1DCASH                                                  
*                                                                               
BATBLD8  MVI   S1KEY,EFFS                                                       
         GOTO1 ADSORTER,DMCB,SORTGET                                            
         ICM   R1,15,4(R1)                                                      
         BZ    BATBLD2             END OF SORT FILE                             
         MVC   S1REC(S1RECL),0(R1)                                              
         CLC   LKEY(S1KSL),S1KEY   TEST CHANGE OF BATCH                         
         BNE   BATBLD2             YES - ADD LAST ITEM + HDR FOR PREV           
         B     BATBLD4                                                          
         EJECT                                                                  
***********************************************************************         
* ACCRUAL BATCHES: BUILD ACCRUAL REVERSAL HEADER RECORD               *         
***********************************************************************         
         USING TBARECD,R2                                                       
BLDACR   L     R2,AIO2             R2=A(ACCRUAL BATCH HEADER RECORD)            
         OC    STYPARN,STYPARN     TEST PRE-DEFINED ACCRUAL BATCH TYPE          
         BZ    BLDA4                                                            
         MVC   TBAKBTYP,STYPARN    SET REVERSING TYPE                           
*                                                                               
BLDA4    MVC   TBAKADDT,TODAYCC    ADDED TODAY                                  
         MVI   TBARHSTA,TBAHSUPD   SET UPDATED                                  
         MVC   TBAHRADT,TODAYC     ADDED TODAY                                  
         MVC   TBAHREDT,TODAYC     EFFECTIVE TODAY                              
         MVC   TBAHRUDT,TODAYC     UPDATED TODAY                                
         NI    TBAHRIND,TBAHIGDJ   PRESERVE 'BATCH GENERATED' BIT ONLY          
         OI    TBAHRIND,TBAHIUOF   SET UPDATED OFFLINE                          
         XC    FULL,FULL                                                        
         MVC   FULL(2),TBAKBMOS    PASS ACCRUAL PWOS YM                         
         LA    R1,FULL                                                          
         GOTO1 AADDMON             INCREMENT MONTH                              
         MVC   ACRMOSP,FULL                                                     
*                                                                               
*        LA    R1,ACRMOSP          * ALWAYS ALLOW REVERSAL MOA *                
*        GOTO1 ATSTBMO             TEST REVERSAL MOA CAN BE POSTED              
*        BNE   ROUTH                                                            
*        MVC   ACRMOSP,FULL                                                     
*        MVC   ACRMOSC,FULL+2                                                   
         MVC   TBAKBMOS,ACRMOSP    SET REVERSAL MOA                             
         MVC   ACRMOSC,ACRMOSP     GENERATE CHARACTER MOA                       
         OI    ACRMOSC,X'F0'                                                    
         SR    RE,RE                                                            
         IC    RE,ACRMOSC+1                                                     
         LA    RE,MOSTAB-1(RE)                                                  
         MVC   ACRMOSC+1(1),0(RE)                                               
*                                  LOCATE BATCH HEADER ELEMENT                  
         LA    R3,TBARFST                                                       
         USING BHDELD,R3                                                        
BLDA12   CLI   BHDEL,0                                                          
         BE    ROUTH               BAD BATCH                                    
         CLI   BHDEL,BHDELQ                                                     
         BE    BLDA14                                                           
         IC    R0,BHDLN                                                         
         AR    R3,R0                                                            
         B     BLDA12                                                           
*                                                                               
BLDA14   MVC   SBATITM,BHDITEMA    SAVE ACTUAL ITEM COUNT                       
*                                                                               
         ZAP   DUB,BHDCASHC        REVERSE ALL TOTALS                           
         MP    DUB,=P'-1'                                                       
         ZAP   BHDCASHC,DUB                                                     
         ZAP   DUB,BHDCASHA                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   BHDCASHA,DUB                                                     
         XC    BHDDELIT,BHDDELIT   CLEAR DELETED ITEM COUNT                     
         NI    BHDSTAT1,255-BHDSACRU  CLEAR ACCRUAL STATUS                      
         OI    BHDSTAT1,BHDSACRV   SET ACCRUAL REVERSAL STATUS                  
         CLI   BHDLN,BHDLN2Q       TEST LONG ELEMENT                            
         BNE   BLDAX                                                            
         ZAP   DUB,BHDTOTDR                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   BHDTOTDR,DUB                                                     
         ZAP   DUB,BHDTOTCR                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   BHDTOTCR,DUB                                                     
*                                                                               
BLDAX    B     ROUTE                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GENERAL: BUILD ADJ WORKER FILE FROM SORTED ASKKEYS, AND ADD AN      *         
* OFFLINE ENTRY TO THE REPORT TOTALS TABLE                            *         
***********************************************************************         
BLDWDJ   CLI   SORTIND,3           TEST ANYTHING PUT                            
         BNE   BLDJX               NO                                           
         GOTO1 ADSORTER,DMCB,SORTGET                                            
         ICM   R1,15,4(R1)                                                      
         BZ    BLDJX                                                            
         XC    WKID,WKID           SET WORKER INDEX VALUES                      
         MVC   WKIUSER,SBATUSR                                                  
         MVI   WKISYS,WKISACC                                                   
         MVC   WKIPRG,=C'DJ'                                                    
         MVC   WKIDAY,TODAYP+2                                                  
         MVI   WKITYPE,WKITPOST                                                 
*                                                                               
         L     R2,AIO4             BUILD WORKER POSTING IN IO4                  
         XC    0(256,R2),0(R2)                                                  
         LA    R0,PSTKEYL+5                                                     
         SLL   R0,16                                                            
         STCM  R0,15,0(R2)         SET RECORD LENGTH                            
         LA    R2,4(R2)                                                         
         USING PSTKEYD,R2                                                       
BLDJ02   MVI   PSTKEL,PSTKELQ      SET TRANSACTION KEY POSTING VALUES           
         MVI   PSTKLEN,PSTKEYL                                                  
         MVC   PSTKEY,0(R1)                                                     
         LA    R1,IOWKADD+IOWORKER+IO4                                          
         GOTO1 AIO                                                              
         GOTO1 ADSORTER,DMCB,SORTGET                                            
         ICM   R1,15,4(R1)                                                      
         BNZ   BLDJ02                                                           
*                                                                               
         CLI   PSTKEL,0                 TEST ANYTHING ADDED                     
         BE    BLDJX                    NO                                      
         LA    R1,IOWKCLS+IOWORKER+IO4  ELSE CLOSE THE WORKER FILE              
         GOTO1 AIO                                                              
         USING REPTOTD,R4                                                       
         L     R4,AREPTOT          ADD DJ REPORT TOTALS TO TABLE                
BLDJ04   OC    REPTUID,REPTUID     INIT NEXT ENTRY                              
         BZ    BLDJ06                                                           
         CLC   REPTUID,=C'99'                                                   
         BE    BLDJ08              OVERFLOW ENTRY                               
         CLC   REPTUID,SBATUSR                                                  
         BE    BLDJ06              EXISTING ENTRY                               
         CLI   0(R4),EFFS                                                       
         BE    *+12                EOT - START OVERFLOW ENTRY                   
         LA    R4,REPTOTL(R4)                                                   
         B     BLDJ04                                                           
         SH    R4,=Y(REPTOTL)      BACK UP TO THE LAST ENTRY                    
         MVC   REPTUID,=C'99'                                                   
         MVC   REPTUSR,=C'THE REST'  AND USE IT FOR THE REST                    
         B     BLDJ08                                                           
BLDJ06   MVC   REPTUID,SBATUSR                                                  
         MVC   REPTUSR,REPUSR                                                   
         ZAP   REPTDJDR,PZERO                                                   
         ZAP   REPTDJCR,PZERO                                                   
         ZAP   REPTDKDR,PZERO                                                   
         ZAP   REPTDKCR,PZERO                                                   
BLDJ08   AP    REPTDJDR,TOFUIDDR                                                
         AP    REPTDJCR,TOFUIDCR                                                
BLDJX    MVI   SORTIND,0           SET SORT3 TO BE RE-INITIALIZED               
         ZAP   TOFUIDDR,PZERO      CLEAR DJ TOTALS FOR USER                     
         ZAP   TOFUIDCR,PZERO                                                   
         B     ROUTE                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* GENERAL: BUILD ADK WORKER FILE FROM ACMTAB ENTRIES, AND ADD         *         
* AN ONLINE ENTRY TO THE REPORT TOTALS TABLE                          *         
***********************************************************************         
BLDWDK   CLI   CRE8WKR,YES         TEST WORKER FILES REQUIRED                   
         BNE   BLDK16                                                           
         L     R3,AACMTAB                                                       
         USING ACMTABD,R3                                                       
         CLI   ACMTCUL,0           TEST ANYTHING IN THE TABLE                   
         BE    BLDKX                                                            
*                                                                               
         SR    RF,RF               SORT THE TABLE                               
BLDK02   CLI   ACMTABL(R3),0       END OF ENTRIES                               
         BE    BLDK04                                                           
         CLI   ACMTCUL,EFFS        END OF TABLE                                 
         BE    BLDK04                                                           
         CLC   ACMTCUL,ACMTABL(R3)                                              
         BNH   *+24                                                             
         XC    0(ACMTABL,R3),ACMTABL(R3)                                        
         XC    ACMTABL(ACMTABL,R3),0(R3)                                        
         XC    0(ACMTABL,R3),ACMTABL(R3)                                        
         LR    RF,R3               SET SWAPPED                                  
         LA    R3,ACMTABL(R3)                                                   
         B     BLDK02                                                           
BLDK04   LTR   RF,RF               TEST ANYTHING SWAPPED                        
         BZ    *+14                FINISHED                                     
         L     R3,AACMTAB          REFRESH TABLE POINTER                        
         SR    RF,RF                                                            
         B     BLDK02                                                           
*                                                                               
         USING REPTOTD,R4                                                       
         L     R4,AREPTOT          REPORT TOTALS TABLE                          
BLDK06   OC    REPTUID,REPTUID     INIT NEXT ENTRY                              
         BZ    BLDK08                                                           
         CLC   REPTUID,=C'99'                                                   
         BE    BLDK10              OVERFLOW ENTRY                               
         CLC   REPTUID,SBATUSR                                                  
         BE    BLDK10              EXISTING ENTRY                               
         CLI   0(R4),EFFS                                                       
         BE    BLDK07              EOT - START OVERFLOW ENTRY                   
         LA    R4,REPTOTL(R4)                                                   
         B     BLDK06                                                           
                                                                                
BLDK07   SHI   R4,REPTOTL          BACK UP TO THE LAST ENTRY                    
         MVC   REPTUID,=C'99'                                                   
         MVC   REPTUSR,=C'THE REST'  AND USE IT FOR THE REST                    
         B     BLDK10                                                           
                                                                                
BLDK08   MVC   REPTUID,SBATUSR                                                  
         MVC   REPTUSR,REPUSR                                                   
         ZAP   REPTDJDR,PZERO                                                   
         ZAP   REPTDJCR,PZERO                                                   
         ZAP   REPTDKDR,PZERO                                                   
         ZAP   REPTDKCR,PZERO                                                   
*                                                                               
BLDK10   XC    WKID,WKID           SET WORKER INDEX VALUES                      
         MVC   WKIUSER,SBATUSR                                                  
         MVI   WKISYS,WKISACC                                                   
         MVC   WKIPRG,=C'DK'                                                    
         MVC   WKIDAY,TODAYP+2                                                  
         MVI   WKITYPE,WKITPOST                                                 
*                                                                               
         L     R2,AIO4             BUILD WORKER POSTING IN IO4                  
         XC    0(256,R2),0(R2)                                                  
         LA    R0,PSLTOTL+5                                                     
         SLL   R0,16                                                            
         STCM  R0,15,0(R2)         SET RECORD LENGTH                            
         LA    R2,4(R2)                                                         
         USING PSLTOTD,R2                                                       
         L     R3,AACMTAB                                                       
BLDK12   CLI   ACMTCUL,0           END OF ENTRIES                               
         BE    BLDK14                                                           
         CLI   ACMTCUL,EFFS        END OF TABLE                                 
         BE    BLDK14                                                           
         MVI   PSLTEL,PSLTELQ      SET LEDGER TOTAL POSTING VALUES              
         MVI   PSLTLEN,PSLTOTL                                                  
         MVC   PSLTCUL,ACMTCUL                                                  
         ZAP   PSLTDR,ACMTDR                                                    
         ZAP   PSLTCR,ACMTCR                                                    
         AP    REPTDKDR,ACMTDR     ACCUMULATE TOTALS FOR REPORT SUMMARY         
         AP    REPTDKCR,ACMTCR                                                  
         LA    R1,IOWKADD+IOWORKER+IO4                                          
         GOTO1 AIO                                                              
         LA    R3,ACMTABL(R3)                                                   
         B     BLDK12                                                           
*                                                                               
BLDK14   CLI   PSLTEL,0            TEST ANYTHING ADDED                          
         BE    BLDKX               NO                                           
         LA    R1,IOWKCLS+IOWORKER+IO4  ELSE CLOSE THE WORKER FILE              
         GOTO1 AIO                                                              
BLDK16   TM    PROIND,PROIEBAT     TEST END OF BATCHES ON FILE                  
         BNZ   BLDKX                                                            
         L     R0,AACMTAB          RE-INITIALIZE ACCUMS TABLE                   
         LHI   R1,ACMTABLN                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
BLDKX    B     ROUTE                                                            
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* GENERAL: FILTER AND CLASSIFY BATCH RECORDS READ FROM ACC FILE       *         
* NTRY: IO2=BATCH HEADER                                              *         
***********************************************************************         
         USING TBARECD,R2                                                       
FLTBAT   L     R2,AIO2                                                          
         CLI   TBAKTYP,TBAKTYPQ    *DEFENSIVE CODE                              
         BE    *+12                *                                            
         BAS   RE,IOTERR           *PRINT ERROR                                 
         B     ROUTH               *AND GET OUT                                 
         MVI   FBATTYP,0                                                        
         MVI   FBATIND,0                                                        
         MVI   FBATDAYS,0                                                       
         SR    R3,R3                                                            
         TM    TBARHSTA,TBAHSDEL   TEST DELETED                                 
         BNZ   ROUTH               NOT WANTED                                   
         GOTO1 DATCON,DMCB,(2,TBAHRADT),(0,TEMP)                                
         GOTO1 VPERVERT,DMCB,TEMP,TODAYE  CALC TODAY-BATCH ADDED DATE           
         MVC   HALF,8(R1)          SAVE RESULT                                  
         TM    TBARHSTA,TBAHSIAD   TEST INSTANT UPDATE STATUS                   
         BZ    FLTB02                                                           
         CLC   TBAHRADT,TODAYC     BATCH ADDED TO FILE TODAY                    
         BNE   ROUTH                                                            
         OC    TBAHRUDT,TBAHRUDT   TEST MARKED BY BG ALREADY                    
         BNZ   ROUTH               YES - MUST BE RERUNNING                      
         MVI   FBATTYP,FBATINUP    SET *INSTANT UPDATE*                         
         B     FLTBX                                                            
*                                                                               
FLTB02   TM    TBARHSTA,TBAHSEND+TBAHSUPD+TBAHSSAV                              
         BNZ   FLTB04                                                           
         OI    FBATIND,FBATOPEN    SET OPEN INDICATOR                           
         MVI   FBATTYP,FBATDELT    *BATCH FOR DELETION*                         
         ICM   R3,1,PRFBOPND       TEST OPEN DAYS PROFILE SET                   
         BZ    FLTBX               NO                                           
         LA    R3,1(R3)                                                         
         SH    R3,HALF             TEST PROFILE DAYS EXCEEDED                   
         BNP   FLTBX                                                            
         MVI   FBATTYP,FBATWARN    *BATCH FOR WARNING*                          
         STC   R3,FBATDAYS                                                      
         B     FLTBX                                                            
*                                                                               
FLTB04   TM    TBARHSTA,TBAHSSAV                                                
         BZ    FLTB06                                                           
         OI    FBATIND,FBATSAVD    SET SAVED INDICATOR                          
         MVI   FBATTYP,FBATDELT    *BATCH FOR DELETION*                         
         ICM   R3,1,PRFBSAVD       TEST SAVED DAYS PROFILE SET                  
         BZ    FLTBX                                                            
         LA    R3,1(R3)                                                         
         SH    R3,HALF             TEST SAVED DAYS EXCEEDED                     
         BNP   FLTBX                                                            
         MVI   FBATTYP,FBATWARN    *BATCH FOR WARNING*                          
         STC   R3,FBATDAYS         SET DAYS REMAINING                           
         B     FLTBX                                                            
*                                                                               
FLTB06   CLC   TBAHRUDT,TODAYC     BATCH UPDATED TODAY                          
         BNE   FLTB08                                                           
         TM    TBAHRIND,TBAHIUOF   TEST MARKED BY BG ALREADY                    
         BNZ   ROUTH               YES - MUST BE RERUNNING                      
         MVI   FBATTYP,FBATONUP    SET *ONLINE UPDATE*                          
         B     FLTBX                                                            
*                                                                               
FLTB08   TM    TBARHSTA,TBAHSUPD   BATCH UPDATED TO FILE BEFORE TODAY           
         BNZ   ROUTH                                                            
         OC    TBAHREDT,TBAHREDT   TEST EFFECTIVE DATE SET                      
         BZ    FLTB10                                                           
         CLC   TBAHRADT,TBAHREDT   TEST EFFECTIVE DATE=ADDED DATE               
         BE    FLTB10                                                           
         LA    R1,TBAKBMOS                                                      
         GOTO1 ATSTBMO             TEST MOA LOCKED                              
         BE    *+8                                                              
         OI    FBATIND,FBATMOAL                                                 
         TM    SCPYST5,CPYSBAPE    TEST EFFDT BATCH APPROVAL REQ'D              
         BZ    FLTB14                                                           
         TM    TBARHSTA,TBAHSAPR   TEST BATCH IS APPROVED                       
         BNZ   *+8                                                              
         OI    FBATIND,FBATUNAP                                                 
         B     FLTB14                                                           
*                                                                               
FLTB10   TM    SCPYST5,CPYSBAPR    TEST REGULAR BATCH APPROVAL REQ'D            
         BZ    FLTB11                                                           
         TM    TBARHSTA,TBAHSAPR   TEST BATCH IS APPROVED                       
         BNZ   *+8                                                              
         OI    FBATIND,FBATUNAP                                                 
FLTB11   CLI   FBATIND,0           TEST ANY ERRORS                              
         BNE   FLTB12                                                           
         MVI   FBATTYP,FBATOVUP    *OVERNIGHT UPDATE*                           
         B     FLTBX                                                            
*                                  REGULAR BATCH ERROR - TEST PROFILE           
FLTB12   MVI   FBATTYP,FBATDELT                                                 
         ICM   R3,1,PRFBCLSD       TEST PROFILE VALUE SET                       
         BZ    FLTBX                                                            
         GOTO1 VPERVERT,DMCB,TEMP,TODAYE   CALC TODAY-ADDED DATE                
         MVC   HALF,8(R1)                                                       
         LA    R3,1(R3)                                                         
         SH    R3,HALF             TEST PROFILE DAYS EXCEEDED                   
         BNP   FLTBX               BATCH TO BE DELETED                          
         MVI   FBATTYP,FBATWARN                                                 
         STC   R3,FBATDAYS         SET DAYS REMAINING                           
         B     FLTBX                                                            
*                                                                               
FLTB14   GOTO1 DATCON,DMCB,(2,TBAHREDT),(0,TEMP)                                
         CLC   TBAHREDT,TODAYC     EFFECTIVE DATE TODAY OR EARLIER              
         BH    FLTB16                                                           
         MVI   FBATTYP,FBATDRLI    *DRAFT BATCH GOING LIVE*                     
         CLI   FBATIND,0           TEST ANY ERRORS                              
         BE    FLTBX                                                            
         MVI   FBATTYP,FBATDELT    *DRAFT BATCH TO BE DELETED*                  
         ICM   R3,1,PRFBXCPD       TEST PROFILE SET                             
         BZ    FLTBX                                                            
         GOTO1 VPERVERT,DMCB,TEMP,TODAYE   CALC TODAY-EFFECTIVE DATE            
         MVC   HALF,8(R1)                                                       
         LA    R3,1(R3)            TEST PROFILE EXCEPTION DAYS                  
         SH    R3,HALF                                                          
         BNP   FLTBX                                                            
         MVI   FBATTYP,FBATWARN    *BATCH FOR WARNING*                          
         STC   R3,FBATDAYS         SET DAYS REMAINING                           
         B     FLTBX                                                            
*                                                                               
FLTB16   GOTO1 VPERVERT,DMCB,TODAYE,TEMP   CALC EFFECTIVE DATE-TODAY            
         LH    R1,8(R1)                                                         
         BCTR  R1,0                                                             
         IC    R3,PRFBDUED         TEST PROFILE 'BATCH DUE IN N DAYS'           
         CR    R3,R1                                                            
         BL    ROUTH               DATE OUT OF RANGE - NOT INTERESTED           
         MVI   FBATTYP,FBATDULI    *DRAFT DUE TO GO LIVE*                       
         STC   R1,FBATDAYS         SET DAYS UNTIL LIVE                          
         B     FLTBX                                                            
*                                                                               
FLTBX    B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GENERAL: READ COMPANY RECORD, SAVE USEFUL VALUES                    *         
* NTRY: R1=A(HEXCOMP)                                                 *         
***********************************************************************         
GETCPY   MVC   SCPYNAM,SPACES                                                   
         XC    SCPYSTA(SCPYSTAL),SCPYSTA                                        
         SR    RE,RE                                                            
         IC    RE,0(R1)            DISPLACE INTO CPYTAB FOR THIS CPY            
         SHI   RE,CPYFRSTQ                                                      
         MHI   RE,CPYTABL                                                       
         L     R0,ACPYTAB                                                       
         AR    RE,R0                                                            
         USING CPYTABD,RE                                                       
         CLI   CPYTCPY,0                                                        
         BE    GETCNF              NOT FOUND?                                   
         MVC   SCPYNAM,CPYTNAME    EXTRACT DETAILS FROM TABLE ENTRY             
         MVC   SCPYCPY,CPYTCPY                                                  
         MVC   SCPYST1,CPYTSTA1                                                 
         MVC   SCPYST2,CPYTSTA2                                                 
         MVC   SCPYST3,CPYTSTA3                                                 
         MVC   SCPYST4,CPYTSTA4                                                 
         MVC   SCPYST7,CPYTSTA7                                                 
         L     R1,ATRNBLK                                                       
         USING TRNBLKD,R1                                                       
         MVC   TRNCPYS1,CPYTSTA1   SET STATUS VALUES FOR ADDTRN                 
         MVC   TRNCPYS2,CPYTSTA2                                                
         MVC   TRNCPYS3,CPYTSTA3                                                
         MVC   TRNCPYS4,CPYTSTA4                                                
         MVC   TRNCPYS5,CPYTSTA5                                                
         MVC   TRNCPYS6,CPYTSTA6                                                
         MVC   TRNCPYS7,CPYTSTA7                                                
         MVC   TRNCPYS8,CPYTSTA8                                                
         MVC   TRNCPYS9,CPYTSTA9                                                
         MVC   TRNCPYSA,CPYTSTAA                                                
         MVC   TRNGLMOA,CPYTGMOA                                                
         DROP  R1                                                               
         MVC   SCPYALF,CPYTALPH    ALPHA ID                                     
         MVC   SCPYPROD,CPYTPROD   PRODUCTION LEDGER                            
         MVC   SCPYST5,CPYTSTA5                                                 
         B     GETCX                                                            
*                                                                               
GETCNF   MVC   SCPYNAM,=C' **** COMPANY RECORD NOT FOUND **** '                 
*                                                                               
GETCX    B     ROUTE                                                            
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* LOCATE AND SAVE BATCH TYPE ENTRY FROM TYPTAB                        *         
***********************************************************************         
         USING TYPTABD,RF                                                       
GETTYP   L     RF,ATYPTAB                                                       
GETT02   CLC   TYPNUM,SBLDKEY+(TBAKBTYP-TBAKEY)                                 
         BE    GETT06                                                           
         CLI   TYPNUM,0            EOT                                          
         BE    ROUTH                                                            
GETT04   LA    RF,TYPTABL(RF)                                                   
         B     GETT02                                                           
*                                                                               
GETT06   CLI   TYPCTRY,CTRYALL     TEST COUNTRY VALIDITY                        
         BE    GETT08              ALL COUNTRIES                                
         MVC   BYTE,TYPCTRY                                                     
         LA    R1,X'70'            SET BRANCH NE CC                             
         TM    BYTE,CTRYNOT        TEST 'NOT THIS COUNTRY' BIT SET              
         BZ    *+12                                                             
         NI    BYTE,255-CTRYNOT    CLEAR IT FOR COMPARE                         
         LA    R1,X'80'            SET BRANCH EQ CC                             
         CLC   BYTE,RCCTRY                                                      
         EX    R1,*+8                                                           
         B     *+8                                                              
         NOP   GETT04                                                           
GETT08   MVC   STYPNTRY,TYPNUM     SAVE ENTRY                                   
         B     ROUTE                                                            
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* READ CONTROL FILE FOR USER-ID AND NAME                              *         
* NTRY R1=A(USER-ID)                                                  *         
***********************************************************************         
GETUSR   MVC   WORK(L'IOKEY),IOKEY  SAVE KEY                                    
         MVC   REPUSR,SPACES                                                    
         MVI   GUFLAG,C'N'                                                      
         LA    R3,IOKEY                                                         
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,0(R1)                                                    
         LA    R1,IOREAD+IOCONFIL+IO4                                           
         GOTO1 AIO                                                              
         BNE   GETUX                                                            
         SR    R0,R0                                                            
         L     R3,AIOBUFF                                                       
         LA    R3,CTIDATA                                                       
         USING CTDSCD,R3                                                        
GETU02   CLI   CTDSCEL,0                                                        
         BE    GETUX                                                            
         CLI   CTDSCEL,CTDSCELQ    DESCRIPTION ELEMENT                          
         BE    GETU04                                                           
         CLI   CTDSCEL,CTDSTELQ    DESTINATION ELEMENT                          
         BE    GETU06                                                           
         CLI   CTDSCEL,CTSYSELQ    SYSTEM ELEMENT                               
         BE    GETU08                                                           
         B     GETU10                                                           
*                                                                               
GETU04   MVC   REPUSR(10),CTDSC    USER-ID                                      
         B     GETU10                                                           
*                                                                               
         USING CTDSTD,R3                                                        
GETU06   MVC   REPUSR+10(L'CTDSTNAM),CTDSTNAM  NAME                             
         B     GETU10                                                           
*                                                                               
         USING CTSYSD,R3                                                        
GETU08   CLI   CTSYSNUM,6          TEST ACCOUNTING SYSTEM ELEMENT               
         BNE   GETU10                                                           
         CLC   CTSYSSE,SENO        TEST USER-ID ON CURRENT ACCFILE              
         BNE   GETUX               POSSIBLY NEQ IF UID CAME FROM WKRFIL         
         MVI   GUFLAG,C'Y'                                                      
*                                                                               
GETU10   IC    R0,CTSYSLEN                                                      
         AR    R3,R0                                                            
         B     GETU02                                                           
*                                                                               
GETUX    MVC   IOKEY,WORK          RESTORE CALLER'S KEY                         
         CLI   GUFLAG,C'Y'         SET CC FOR CALLER                            
         B     ROUTX                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* RECOVERY/WORKER FILE/INTERAGENCY/OLD BT7,48,57 BATCHES:             *         
* ADD NEW BATCH HEADER RECORD DRAFT OR LIVE                           *         
* NTRY: R1=STATUS BITS - SEE HDINDS                                   *         
***********************************************************************         
HDRADD   STC   R1,HDINDS           SAVE PASSED PARAMETER                        
         L     R2,AIO1             R2=A(LAST ITEM RECORD FOR BATCH)             
         USING TBARECD,R2                                                       
         CLC   SBATCPY,TBAKCPY     TEST FIRST/CHANGED CPY                       
         BE    HDRA01                                                           
         GOTO1 AGETCPY,TBAKCPY     READ CPY REC FOR STATUS + NAME               
HDRA01   CLC   SBATUSR,TBAKUSER    TEST FIRST/CHANGED USER ID                   
         BE    HDRA02                                                           
         GOTO1 AGETUSR,TBAKUSER    GET USER NAME + OFFICE (IF ANY)              
HDRA02   MVC   SBATNAM,SPACES                                                   
         SR    R4,R4                                                            
         TM    HDINDS,LIVEQ        TEST RECOVERY BATCH (INC. NEW BT7)           
         BNZ   HDRA03                                                           
         ICM   R4,3,STYPBNDD       DSLIST DISP TO PREDEFINED NAME               
         BNZ   HDRA14              YES - OLD BATCH HEADER DOESN'T EXIST         
HDRA03   TM    STYPIND1,TYPINOLD   DON'T BOTHER TO CHECK FOR OLD BH             
         BZ    *+12                                                             
         ICM   R4,3,STYPBNDD       DSLIST DISP TO PREDEFINED NAME               
         B     HDRA14              GO ADD A NEW BATCH HEADER                    
         CLI   STYPNUM,BT99        TEST WFM 'BATCH'                             
         BNE   HDRA08                                                           
*                                                                               
         LA    RF,TBARFST          EXTRACT BATCH NAME FROM WFMEL                
         SR    R0,R0                                                            
         USING ASKELD,RF                                                        
HDRA04   CLI   ASKEL,ASKELQ        READ A POSTING                               
         BE    *+14                                                             
         IC    R0,ASKLN                                                         
         AR    RF,R0                                                            
         B     HDRA04                                                           
         LA    R3,IOKEY                                                         
         USING TRNRECD,R3                                                       
         MVC   TRNKEY,ASKKEY                                                    
         LA    R1,IORD+IOACCDIR+IO2                                             
         GOTO1 AIO                                                              
         BE    *+12                                                             
         BAS   RE,IOTERR                                                        
         B     HDRA14                                                           
         LA    R1,IOGET+IOACCMST+IO2                                            
         GOTO1 AIO                                                              
         BE    *+12                                                             
         BAS   RE,IOTERR                                                        
         B     HDRA14                                                           
         L     R3,AIO2                                                          
         LA    RF,TRNRFST                                                       
         USING WFMELD,RF                                                        
HDRA06   CLI   WFMEL,0             LOCATE WFM ELEMENT                           
         BE    HDRA14                                                           
         CLI   WFMEL,WFMELQ                                                     
         BE    *+14                                                             
         IC    R0,WFMLN                                                         
         AR    RF,R0                                                            
         B     HDRA06                                                           
         MVI   SBATNAM,DD#ESCL     BUILD DD ESCAPE SEQUENCE                     
         MVC   SBATNAM+1(L'WFMPROG),WFMPROG                                     
         GOTO1 ADDICTAT,DMCB,C'SU  ',SBATNAM,0  RESOLVE IT                      
         B     HDRA14                                                           
         DROP  RF                                                               
*                                  READ OLD BATCH HEADER                        
HDRA08   LA    R3,IOKEY                                                         
         USING BATRECD,R3                                                       
         MVC   BATKEY,SPACES                                                    
         XC    BATKEY(BATKEND),BATKEY                                           
         MVI   BATKTYP,BATKTYPQ                                                 
         MVC   BATKCPY,TBAKCPY                                                  
         MVC   BATKOFF,TBAKUSER                                                 
         MVC   BATKGRUP,TBAKGRUP                                                
         MVC   BATKTYPE,TBAKBTYP                                                
         MVC   BATKDATE,TODAYP                                                  
         MVC   BATKREF,TBAKBMOS    MOS+REF                                      
         OI    BATKREF,X'F0'       TRANSLATE MOS TO DISPLAY FORMAT              
         SR    RF,RF                                                            
         IC    RF,BATKREF+1                                                     
         LA    RF,MOSTAB-1(RF)                                                  
         MVC   BATKREF+1(1),0(RF)                                               
*                                                                               
         LA    R1,IORD+IOACCDIR+IO2                                             
         GOTO1 AIO                                                              
         BE    *+12                                                             
         BAS   RE,IOTERR           PRINT ERROR TRACE                            
         B     HDRA14                                                           
*                                                                               
         LA    R1,IOGET+IOACCMST+IO2                                            
         GOTO1 AIO                                                              
         BE    *+12                                                             
         BAS   RE,IOTERR           PRINT ERROR TRACE                            
         B     HDRA14                                                           
*                                                                               
HDRA10   L     R3,AIO2                                                          
         SR    R0,R0                                                            
         LA    R1,BATRFST          LOCATE HEADER ELEMENT                        
         USING BTHELD,R1                                                        
HDRA12   CLI   BTHEL,BTHELQ                                                     
         BNE   *+14                                                             
         MVC   SBATNAM,BTHNAME     SAVE THE NAME                                
         B     HDRA14                                                           
         CLI   BTHEL,0                                                          
         BE    HDRA14              NO HDR ELEMENT ON OLD BATCH HDR ?            
         IC    R0,BTHLN                                                         
         AR    R1,R0                                                            
         B     HDRA12                                                           
         DROP  R1                                                               
*                                                                               
HDRA14   XC    TBAKTSEQ,TBAKTSEQ   SET NEW RECORD IS BATCH HEADER               
         MVC   TBAHRADT,TODAYC                                                  
         MVC   TBAHREDT,TODAYC                                                  
         XC    TBAHRUDT,TBAHRUDT                                                
         MVI   TBARHSTA,TBAHSEND   SET BATCH ENDED STATUS                       
         TM    SCPYST5,CPYSBAPR    TEST BATCH APPROVAL REQUIRED                 
         BZ    *+8                                                              
         OI    TBARHSTA,TBAHSAPR   GIVE IT                                      
         TM    HDINDS,DRAFTQ       TEST ADDING DRAFT STATUS BATCH               
         BNZ   HDRA16                                                           
         MVI   TBARHSTA,TBAHSUPD   ELSE SET BATCH UPDATED STATUS                
         MVC   TBAHRUDT,TODAYC     AND BATCH UPDATED DATE                       
HDRA16   OI    TBAHRIND,TBAHIGDJ   GENERATED BY DAILY JOURNAL                   
         LA    R3,TBARFST                                                       
         XC    0(256,R3),0(R3)                                                  
*                                                                               
         USING BHDELD,R3           BUILD BATCH HEADER ELEMENT                   
         MVI   BHDEL,BHDELQ                                                     
         MVI   BHDLN,BHDLNQ                                                     
         LTR   R4,R4               TEST BATCH NAME FROM TYPTAB                  
         BNZ   *+14                YES                                          
         MVC   BHDNAME,SBATNAM     ELSE USE SUPPLIED NAME                       
         B     HDRA18                                                           
         LA    R4,DSUC(R4)                                                      
         MVC   BHDNAME,0(R4)                                                    
HDRA18   MVC   BHDITEMC,ITEMSEQ    SET ITEM CONTROL TOTAL                       
         MVC   BHDITEMA,ITEMSEQ    SET ITEMS ADDED                              
         ZAP   BHDCASHC,BATCASH    SET CASH CONTROL TOTAL                       
         ZAP   BHDCASHA,BATCASH    SET CASH TOTAL ADDED                         
         MVC   BHDLUID,SLUID       SET LUID (OR ZEROS)                          
         TM    HDINDS,ACCRUQ       TEST ACCRUAL REQUIRED                        
         BZ    *+8                                                              
         OI    BHDSTAT1,BHDSACRU                                                
         TM    HDINDS,NOREVQ       TEST NO REVERSAL ALLOWED                     
         BZ    *+8                                                              
         OI    BHDSTAT1,BHDSNREV                                                
         XC    SLUID,SLUID                                                      
         XC    ITEMSEQ,ITEMSEQ                                                  
         ZAP   BATCASH,PZERO                                                    
         LA    RF,BHDLNQ(R3)                                                    
         LA    R0,TBARECD                                                       
         BCTR  R0,0                                                             
         SR    RF,R0                                                            
         STCM  RF,3,TBARLEN        SET RECLEN                                   
*                                                                               
         LA    R1,IOADDREC+IOACCMST+IO1                                         
         GOTO1 AIO                 ADD DATA RECORD                              
         BNE   HDRA20                                                           
         ICM   RF,15,NBHDADD       MAINTAIN HDR RECORD COUNT                    
         LA    RF,1(RF)                                                         
         STCM  RF,15,NBHDADD                                                    
         B     HDRA24                                                           
*                                                                               
HDRA20   TM    IOERR,IOEDUP        TEST HEADER ALREADY EXISTS                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1             YES, RE-USE IT (MUST BE DELETED)             
         LA    R2,IOKEY                                                         
         USING TBARECD,R2                                                       
         MVC   TBAKEY,0(R3)        READ IT                                      
         LA    R1,IORDD+IOACCDIR+IO5                                            
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOEDEL        CONFIRM DELETE STATUS                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGETREC+IOACCMST+IO5                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIOARE5          COPY D/A AND WORK FROM GETREC                
         L     RE,AIOARE1          TO PUTREC                                    
         MVC   0(L'IODA+L'IOWORK,RE),0(RF)                                      
         LA    R1,IOPUTREC+IOACCMST+IO1                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TBAKSTA,TBARSTA-TBARECD(R3)   SET NEW STATUS                     
         MVC   TBAKDA,IODA                   AND DISK ADDRESS                   
         LA    R1,IOWRITE+IOACCDIR+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
HDRA24   LA    R2,IOKEY            BUILD PASSIVE POINTER                        
         L     R3,AIOBUFF                                                       
         MVC   TBAPAS,0(R3)                                                     
         MVC   TBAKSTA,TBARSTA-TBARECD(R3)                                      
         MVC   TBAKDA,IODA                                                      
         MVI   TBAPTYP,TBAPTYPQ                                                 
         MVC   TBAPEFDT,TODAYC                                                  
         MVC   TBAPTSEQ,TBAHRADT-TBARECD(R3)                                    
         LA    R1,IOADD+IOACCDIR+IO1                                            
         GOTO1 AIO                                                              
         BE    HDRA26                                                           
         TM    IOERR,IOEDUP        TEST ALREADY EXISTS                          
         BNZ   *+6                                                              
         DC    H'0'                FILE ERROR                                   
         MVC   FULL,IODA           TEST PASSIVE EXISTS WITH DELETED STS         
         LA    R1,IORDD+IOACCDIR+IO1                                            
         GOTO1 AIO                                                              
         BH    *+6                                                              
         DC    H'0'                FOUND UNDELETED OR HARD ERROR                
         TM    IOERR,IOEDEL        TEST DELETED                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   TBAKSTA,TBARSTA-TBARECD(R3)  UPDATE STATUS AREA                  
         MVC   TBAKDA,FULL                  AND DISK ADDRESS                    
         LA    R1,IOWRITE+IOACCDIR+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
HDRA26   L     R2,AIO1                                                          
         CLI   SBATCPY,0           TEST FIRST COMPANY                           
         BE    HDRA28                                                           
         CLC   SBATUSR,TBAKUSER    TEST CHANGE OF USER                          
         BE    HDRA30                                                           
         MVI   FORCEHED,C'Y'                                                    
HDRA28   XOUT  TBAKCPY,REPCPY,1                                                 
         MVC   SBATCPY,TBAKCPY                                                  
         MVC   SBATUSR,TBAKUSER                                                 
         MVC   REPCPY+10(L'SCPYNAM),SCPYNAM                                     
HDRA30   MVC   REPCMNT(L'AC@BATC),AC@BATC     'BATCH CREATED'                   
         L     R1,AIO1                                                          
         GOTO1 AREPBAT                                                          
         MVI   GPFLAG,1            A BATCH HAS BEEN PROCESSED                   
         B     ROUTE                                                            
         DROP  R2,R3                                                            
*                                                                               
HDINDS   DS    CL1                 HDRADD INDICATOR PASSED BY CALLER            
DRAFTQ   EQU   X'80'               ADD DRAFT STATUS BATCH                       
LIVEQ    EQU   X'40'               ADD UPDATED STATUS BATCH                     
ACCRUQ   EQU   X'20'               SET BATCH IS AN ACCRUAL                      
NOREVQ   EQU   X'10'               SET BATCH CANNOT BE REVERSED                 
         EJECT                                                                  
***********************************************************************         
* GENERATE INTERAGENCY BATCHES                                        *         
* NTRY: SORT BUFFER CONTAINS INTAGY KEY RECORDS                       *         
***********************************************************************         
*                                                                               
IAGBAT   CLI   SORTIND,2                                                        
         BNE   IAGBX               SORT NOT INITIALISED                         
         XC    LKEY,LKEY                                                        
         XC    SBATUSR,SBATUSR                                                  
         XC    SBATCPY,SBATCPY                                                  
IAGB02   GOTO1 ADSORTER,DMCB,SORTGET                                            
         ICM   R1,15,4(R1)                                                      
         BNZ   *+12                                                             
         MVI   S2REC,EFFS          SET END OF SORT                              
         B     IAGB04                                                           
         MVC   S2REC(S2RECL),0(R1)                                              
         CLC   LKEY(S2KSL),S2KEY   TEST SIGNIFICANT CHANGE IN KEY               
         BE    IAGB10                                                           
         OC    LKEY(S2KSL),LKEY    TEST FIRST TIME                              
         BZ    IAGB08                                                           
IAGB04   OC    ITEMSEQ,ITEMSEQ     TEST ANY BATCH ITEMS ADDED                   
         BZ    IAGB08              NO                                           
         OC    ASKLSEQ,ASKLSEQ     TEST BATCH ITEM OUTSTANDING                  
         BZ    IAGB06                                                           
         GOTO1 AITMADD             ADD LAST BATCH ITEM                          
IAGB06   GOTO1 AHDRADD,DRAFTQ      ADD BATCH HEADER WITH DRAFT STATUS           
         AP    IBKSEQ,=P'1'                                                     
IAGB08   CLI   S2REC,EFFS                                                       
         BE    IAGBX               END OF SORT                                  
         CLC   SBATUSR,S2KUID      TEST CHANGE OF UID                           
         BE    *+10                                                             
         ZAP   IBKSEQ,=P'1'        INIT SEQUENCE NO. USED FOR BAT REF           
         CLC   SBATCPY,S2KCPY      TEST CHANGE OF COMPANY                       
         BE    IAGB09                                                           
         GOTO1 AGETCPY,S2KCPY      READ COMPANY REC                             
         MVC   SBATCPY,S2KCPY                                                   
IAGB09   GOTO1 AIBKBLD             BUILD INTERAGENCY BATCH KEY                  
         GOTO1 AIVALAC             VALIDATE POSTING ACCOUNTS                    
         BNE   IAGB02              ESSENTIAL A/C NOT FOUND, CAN'T POST          
         GOTO1 AIGETDU             ** TEST CHANGE AGY/CLT B4 CALLING **         
         MVC   LKEY(S2KSL),S2KEY                                                
IAGB10   MVC   IODAOVER,S2DDA      READ INTAGY DATA RECORD                      
         LA    R1,IOGET+IOACCMST+IO2                                            
         GOTO1 AIO                                                              
         BNE   IAGB02              NOT FOUND ?                                  
         L     R2,AIOBUFF                                                       
         USING INTRECD,R2                                                       
         LA    R1,INTRFST                                                       
         USING IPRELD,R1                                                        
         SR    R0,R0                                                            
IAGB12   CLI   IPREL,0                                                          
         BE    IAGB20                                                           
         CLI   IPREL,IPRELQ        LOCATE PROFILE ELEMENT                       
         BE    IAGB16                                                           
         CLI   IPREL,IESELQ        LOCATE ESTIMATE ELEMENT(S)                   
         BE    IAGB18                                                           
IAGB14   IC    R0,IPRLN                                                         
         AR    R1,R0                                                            
         B     IAGB12                                                           
*                                                                               
IAGB16   MVC   IAESTDE,IPRDES      SAVE ESTIMATE DESCRIPTION                    
         B     IAGB14                                                           
*                                                                               
         USING IESELD,R1                                                        
IAGB18   CLC   IESDTO,S2DDATC      TEST IESEL DATE = 2D04 RECORD DATE           
         BNE   IAGB14                                                           
         OC    IESUSED,IESUSED     TEST USED DATE NOT SET                       
         BNZ   IAGB14                                                           
         TM    IESSTAT,IESSPOST    TEST POSTED BIT NOT SET                      
         BNZ   IAGB14                                                           
         CLC   IESMTH,S2KMOS       TEST RIGHT IESEL FOR THIS SORT REC           
         BNE   IAGB14                                                           
         GOTO1 AITRNAD             MAKE POSTINGS, BUILD BATCH ITEM              
*                                  MARK ELEMENT                                 
         MVC   IESUSED,TODAYC      SET UPDATED DATE                             
         OI    IESSTAT,IESSPOST    AND POSTED BIT                               
         B     IAGB14                                                           
*                                  UPDATE INTAGY DATA RECORD                    
IAGB20   LA    R1,IOPUTREC+IOACCMST+IO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         LA    R3,IOKEY            RE-READ AND UPDATE PASSIVE                   
         MVC   IOKEY,SPACES                                                     
         USING IDJRECD,R3                                                       
         MVI   IDJKTYP,IDJKTYPQ                                                 
         MVI   IDJKSUB,IDJKSUBQ                                                 
         MVC   IDJKDATE,S2KDATP                                                 
         MVC   IDJKCULA(INTKSUBL),INTKCULA                                      
         LA    R1,IOREAD+IOACCDIR+IO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IDJKPDAT,TODAYC     SET POSTED DATE                              
         LA    R1,IOWRITE+IOACCDIR+IO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,IAGRCHA       MAINTAIN INTAGY RECS CHANGED COUNT           
         LA    RF,1(RF)                                                         
         STCM  RF,15,IAGRCHA                                                    
         B     IAGB02                                                           
*                                                                               
IAGBX    B     ROUTE                                                            
         DROP  R1,R2,R3                                                         
         EJECT                                                                  
***********************************************************************         
* INTERAGENCY BATCHES: BUILD BATCH KEY IN SBLDKEY                     *         
***********************************************************************         
IBKBLD   LA    R2,SBLDKEY          BUILD X'03' BATCH KEY                        
         USING TBARECD,R2                                                       
         XC    TBAKEY,TBAKEY                                                    
         MVI   TBAKTYP,TBAKTYPQ                                                 
         MVC   TBAKCPY,S2KCPY                                                   
         MVC   TBAKUSER,S2KUID                                                  
         MVC   TBAKADDT,TODAYCC                                                 
         MVI   TBAKGRUP,TBAGGENQ                                                
         MVI   TBAKBTYP,BT58                                                    
         MVC   TBAKBMOS,S2KDATP    MOS FROM POSTING YYMM                        
         MVI   TBAKBREF+0,C'U'     BUILD REFERENCE                              
         OI    IBKSEQ+L'IBKSEQ-1,X'0F'                                          
         UNPK  TBAKBREF+1(3),IBKSEQ                                             
         XC    TBAKBCHR(TBAKLAST-TBAKBCHR),TBAKBCHR                             
*                                                                               
         GOTO1 AGETTYP             LOCATE AND SAVE TYPTAB ENTRY                 
*&&US                                                                           
         TM    STYPIND1,TYPIPRDQ   TEST BATCH GROUP=PROD                        
         BZ    *+8                                                              
         MVI   TBAKGRUP,TBAGPRDQ   SET IT                                       
*&&                                                                             
         MVC   IOKEYSAV,SBLDKEY                                                 
         B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INTERAGENCY BATCHES: READ INTERAGENCY CONTROL PROFILE FOR DUE DAYS  *         
***********************************************************************         
IGETDU   XC    WORK,WORK                                                        
         XC    IDUEDYS,IDUEDYS                                                  
         LA    R0,2                TWO CALLS                                    
IGETD02  MVI   WORK+0,C'A'-X'40'                                                
         MVC   WORK+1(3),=C'INT'                                                
         MVC   WORK+12(2),SCPYALF                                               
         GOTO1 GETPROF,DMCB,WORK,WORK+20,DATAMGR                                
         BCT   R0,IGETD04                                                       
         MVC   IDUEDYS,WORK+20                                                  
         B     IGETDUX                                                          
*                                                                               
IGETD04  XC    WORK(20),WORK       TRY FOR CLIENT(+OFFICE) PROFILE              
         MVC   WORK+5(L'SCPYPROD),SCPYPROD                                      
         MVC   WORK+5+L'SCPYPROD(L'S2KCLT),S2KCLT                               
         CLC   ACOFFC,SPACES                                                    
         BE    IGETD02                                                          
         CLC   ACOFFC+1(L'ACOFFC-1),SPACES                                      
         BNE   IGETD06                                                          
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),ACOFFC                                                
         B     IGETD02                                                          
*                                                                               
IGETD06  MVI   WORK+10,C'+'                                                     
         MVC   WORK+14(L'ACOFFC),ACOFFC                                         
         B     IGETD02                                                          
*                                                                               
IGETDUX  B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* INTERAGENCY BATCHES: BUILD AND ADD TRANSACTIONS, ADD BATCH ITEMS    *         
* NTRY: AIO2=INTERAGENCY RECORD, R1=A(IESEL ELEMENT FOR POSTING)      *         
***********************************************************************         
*                                                                               
ITRNAD   LR    R4,R1               R4=A(IESEL ELEMENT)                          
         USING IESELD,R4                                                        
         L     R2,AIO2                                                          
         USING INTRECD,R2                                                       
         L     R3,AIO3                                                          
         USING TRNRECD,R3                                                       
         LA    RE,AIACTS                                                        
         LA    RF,AIACTSL*AIACTSM                                               
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVI   AIACTSN,0                                                        
*                                  BUILD C0 DATA                                
         MVI   GPFLAG,X'80'                                                     
         LA    R1,INTKCULA+1       DEBIT SR                                     
         BAS   RE,APEADD                                                        
*                                                                               
         MVI   GPFLAG,X'00'                                                     
         LA    R1,SIACC+1          CREDIT SI                                    
         BAS   RE,APEADD                                                        
*                                                                               
         TM    SCPYST1,CPYSCOST    COSTING POSTINGS REQUIRED?                   
         BZ    ITRN00              NO, SKIP THE REST                            
         CLI   IACPFLG,NO          DON'T ADD IF ACCCCOUNTS ARE BAD              
         BE    ITRN00                                                           
*                                                                               
         MVI   GPFLAG,X'80'                                                     
         LA    R1,ACC1C+1          DEBIT 1C                                     
         BAS   RE,APEADD                                                        
*                                                                               
         MVI   GPFLAG,X'00'                                                     
         LA    R1,ACC11+1          CREDIT 11                                    
         BAS   RE,APEADD                                                        
*                                                                               
         MVI   GPFLAG,X'00'                                                     
         LA    R1,ACC12+1          CREDIT 12                                    
         BAS   RE,APEADD                                                        
*                                  * BUILD DEBIT POSTING *                      
ITRN00   MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,INTKCULA                                                
         TM    SCPYST4,CPYSOFF2    TEST 2-CHARACTER OFFICES IN USE              
         BZ    *+10                                                             
         MVC   TRNKOFF,ACOFFC      SET OFFICE IN KEY                            
         MVC   TRNKCULC,SPACES                                                  
         L     R1,ATRNBLK                                                       
         USING TRNBLKD,R1                                                       
         MVC   TRNCACNM,SPACES                                                  
         TM    S2DSTA,INTSMI       TEST MEDIA INTERFACE RECORD IN USE           
         BZ    ITRN02                                                           
         MVC   TRNKCULC+3(L'TRNKCULC-3),MIDESC   C/A IS MI DESCRIPTION          
         MVC   TRNCACNM+3(L'TRNKCULC-3),MIDESC   DITTO NAME                     
         B     ITRN04                                                           
*                                                                               
ITRN02   MVC   TEMP,SIACCNM        C/A IS INCOME ACCOUNT NAME                   
         GOTO1 ADSQUASH,DMCB,TEMP,36                                            
         GOTO1 CHOPPER,DMCB,(36,TEMP),(12,TRNKCULC+3),(0,1)                     
         L     R1,ATRNBLK                                                       
         MVC   TRNCACNM+3(L'TRNCACNM-3),TEMP                                    
*                                                                               
ITRN04   MVC   TRNKDATE,S2KDATP                                                 
         MVC   TRNKREF,INTKEST                                                  
         MVI   TRNKSBR,0                                                        
*                                  TRANSACTION ELEMENT                          
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         MVI   TRNEL,TRNELQ                                                     
         L     RF,AIO3                                                          
         MVC   TRNDATE,TRNKDATE-TRNRECD(RF)                                     
         MVC   TRNREF,INTKEST                                                   
         MVI   TRNSUB,0                                                         
         MVC   TRNTYPE,SBLDKEY+(TBAKBTYP-TBAKEY)                                
         OI    TRNSTAT,TRNSDR                                                   
         MVC   TRNMOS,SBLDKEY+(TBAKBMOS-TBAKEY)                                 
         OI    TRNMOS,X'F0'        CONVERT MOS TO DISPLAY                       
         SR    RF,RF                                                            
         IC    RF,TRNMOS+1                                                      
         LA    RF,MOSTAB-1(RF)                                                  
         MVC   TRNMOS+1(1),0(RF)                                                
         MVC   TRNBREF,SBLDKEY+(TBAKBREF-TBAKEY)                                
         ZAP   TRNAMNT,IESRAO                                                   
         MVC   TRNOFFC,ACOFFC                                                   
         MVC   TEMP,IESMTH         CONVERT PWOS ADVT MONTH TO MMM/YY            
         MVI   TEMP+L'IESMTH,0                                                  
         GOTO1 DATCON,DMCB,(1,TEMP),(6,INTALDT)                                 
         MVC   TRNNARR(INTALITL),INTALIT                                        
         LA    RE,TRNNARR+INTALITL                                              
         L     RF,AIO3                                                          
         LA    RF,TRNRFST-TRNRECD(RF)                                           
         SR    RE,RF                                                            
         STC   RE,TRNLN                                                         
         LA    R3,0(RE,R3)                                                      
*                                  MEDIA TRANSFER ELEMENT                       
         USING MDTELD,R3                                                        
         XC    MDTEL(MDTLNQ),MDTEL                                              
         MVI   MDTEL,MDTELQ                                                     
         MVI   MDTLN,MDTLNQ                                                     
         MVI   MDTSYS,MDTSINTA     INTERAGENCY                                  
         MVC   MDTMED,SPACES                                                    
         MVC   MDTCLI,INTKCLT                                                   
         MVC   MDTPRD,INTKPRD                                                   
         MVC   MDTEST,INTKEST                                                   
         TM    S2DSTA,INTSMI                                                    
         BZ    *+8                                                              
         OI    MDTSTAT,MDTSMIQ     SET MDTMED2 IS MI RECORD CODE                
         MVC   MDTMED2,INTKMED                                                  
         MVC   MDTMOS,IESMTH       ADV MONTH                                    
         MVC   MDTDSCP,IAESTDE     ESTIMATE DESCRIPTION                         
         ZAP   DUB,IESGRO                                                       
**                                                                              
         CP    DUB,=P'2100000000'  IF OVER $21,000,000.00                       
         BNH   *+10                                                             
         ZAP   DUB,=P'0'           THE CVB INSTRUCTION IS BROKEN                
**                                                                              
         CVB   R0,DUB                                                           
         STCM  R0,15,MDTGRS                                                     
         XC    MDTNET,MDTNET                                                    
         XC    MDTCOM,MDTCOM                                                    
         XC    MDTCD,MDTCD                                                      
         ZAP   DUB,IESRAO                                                       
**                                                                              
         CP    DUB,=P'2100000000'  IF OVER $21,000,000.00                       
         BNH   *+10                                                             
         ZAP   DUB,=P'0'           THE CVB INSTRUCTION IS BROKEN                
**                                                                              
         CVB   R0,DUB                                                           
         STCM  R0,15,MDTRECV       ORIGINAL RECEIVABLE AMOUNT                   
         STCM  R0,15,MDTCOM        INCOME (COMMISSION)                          
         LA    R3,MDTLNQ(R3)                                                    
*                                  SUBSIDIARY CASH ELEMENT                      
         USING SCIELD,R3                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGRSS                                                 
         ZAP   SCIAMNT,IESGRO                                                   
         LA    R3,SCILN1Q(R3)                                                   
*                                  DUE DATE ELEMENT                             
         USING DUEELD,R3                                                        
         MVI   DUEEL,DUEELQ                                                     
         MVI   DUELN,DUELNQ                                                     
         GOTO1 DATCON,DMCB,(2,S2DDATC),(0,DUB)                                  
         SR    RF,RF                                                            
         IC    RF,IDUEDYS                                                       
         GOTO1 ADDAY,DMCB,DUB,TEMP,(RF)  CALCULATE DUE DATE                     
         GOTO1 DATCON,DMCB,(0,TEMP),(2,DUEDATE)                                 
         LA    RE,DUELNQ(R3)                                                    
         MVI   0(RE),0             SET EOR                                      
*                                                                               
         L     R3,AIO3                                                          
         USING TRNRECD,R3                                                       
         LA    RE,1(RE)            RE=EOR+1                                     
         SR    RE,R3                                                            
         STCM  RE,3,TRNRLEN        SET RECORD LENGTH                            
*                                                                               
         LA    RF,TRNRFST                                                       
         USING TRNELD,RF                                                        
*                                                                               
         BAS   RE,ADDTR            POST DEBIT                                   
*                                  * CREDIT POSTING *                           
         MVC   TRNKCULA,SIACC                                                   
         MVC   TRNKCULC,SPACES                                                  
         MVC   TRNKCCPY,INTKCPY                                                 
         MVC   TRNKCUNT(L'TRNKCUNT+L'TRNKCLDG),SCPYPROD                         
         MVC   TRNKCACT(L'INTKCLT+L'INTKPRD),INTKCLT                            
         L     R1,ATRNBLK                                                       
         MVC   TRNCACNM,PRODNAM                                                 
*                                                                               
         NI    TRNSTAT-TRNELD(RF),255-TRNSDR                                    
*                                  DELETE DUEEL FROM CREDIT POSTING             
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),('DUEELQ',TRNRECD),0                   
*                                                                               
         BAS   RE,ADDTR            POST CREDIT                                  
*                                                                               
         TM    SCPYST1,CPYSCOST    TEST COSTING POSTINGS REQUIRED               
         BZ    ITRNX                                                            
         CLI   IACPFLG,NO          CAN'T IF COSTING A/C(S) ARE BAD              
         BE    ITRNX                                                            
         LA    RF,TRNRFST                                                       
         USING TRNELD,RF                                                        
*                                  * DR 1C, CR 11 - GROSS AMOUNT *              
         MVC   TRNKCULA,ACC1C                                                   
         MVC   TRNKCULC,ACC11                                                   
         L     R1,ATRNBLK                                                       
         MVC   TRNCACNM,ACC11NM                                                 
         ZAP   TRNAMNT,IESGRO                                                   
         OI    TRNSTAT,TRNSDR                                                   
         SR    RE,RE                                                            
         IC    RE,TRNLN                                                         
         AR    RE,RF                                                            
         MVI   0(RE),0             SET NEW EOR                                  
         LA    RE,1(RE)                                                         
         SR    RE,R3                                                            
         STCM  RE,3,TRNRLEN        SET RECORD LENGTH                            
*                                                                               
         BAS   RE,ADDTR                                                         
*                                                                               
         MVC   TRNKCULA,ACC11                                                   
         MVC   TRNKCULC,ACC1C                                                   
         L     R1,ATRNBLK                                                       
         MVC   TRNCACNM,ACC1CNM                                                 
         NI    TRNSTAT,255-TRNSDR                                               
*                                                                               
         BAS   RE,ADDTR                                                         
*                                  * DR 1C, CR 12 - RECEIVABLE AMOUNT *         
         MVC   TRNKCULA,ACC1C                                                   
         MVC   TRNKCULC,ACC12                                                   
         L     R1,ATRNBLK                                                       
         MVC   TRNCACNM,ACC12NM                                                 
         ZAP   TRNAMNT,IESRAO                                                   
         OI    TRNSTAT,TRNSDR                                                   
*                                                                               
         BAS   RE,ADDTR                                                         
*                                                                               
         MVC   TRNKCULA,ACC12                                                   
         MVC   TRNKCULC,ACC1C                                                   
         L     R1,ATRNBLK                                                       
         MVC   TRNCACNM,ACC1CNM                                                 
         NI    TRNSTAT,255-TRNSDR                                               
*                                                                               
         BAS   RE,ADDTR                                                         
*                                                                               
ITRNX    GOTO1 AITMADD                                                          
         B     ROUTE                                                            
*                                                                               
*                                  CALL ADDTRN AND ITMBLD                       
*                                                                               
ADDTR    STM   RE,RF,BGREGS                                                     
         OC    TRNKULA,SPACES                                                   
         OC    TRNKULC,SPACES                                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),('APEELQ',TRNRECD),0                   
         BAS   RE,ADDC0                                                         
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),TRNRECD,ATRNWK,0                       
*                                                                               
         L     R1,ATRNBLK                                                       
         OI    TRNINDS,TRNICONV+TRNIDRFT                                        
         OI    TRNINDS2,TRNIADDG                                                
         GOTO1 VADDTRN                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   STRNKEY,TRNKEY      SET TRANS KEY FOR BATCH ITEM BUILD           
ADDTR02  GOTO1 AITMBLD,0                                                        
         BE    ADDTR04             ASKEL ADDED TO ITEM                          
         GOTO1 AITMADD             ELSE ITEM IS FULL, SO WRITE IT               
         B     ADDTR02             AND START A NEW ONE                          
ADDTR04  LA    RF,TRNRFST                                                       
         TM    TRNSTAT,TRNSDR                                                   
         BZ    *+10                                                             
         AP    BATCASH,TRNAMNT                                                  
         ZAP   ITMCASH,TRNAMNT                                                  
         LM    RE,RF,BGREGS                                                     
         BR    RE                                                               
                                                                                
ADDC0    STM   RE,RC,ADDCREGS                                                   
         L     RF,ATRNWK                                                        
         USING APEELD,RF                                                        
         XC    0(256,RF),0(RF)     CLEAR TRANSACTION ELEMENT AREA               
                                                                                
         MVI   APEEL,APEELQ                                                     
         MVI   APELN,APELN1Q                                                    
         MVI   APENUM,0                                                         
         LA    R1,AIACTS           R1=A(TABLE OF ANALYSIS ACCOUNTS)             
         SR    R0,R0                                                            
         ICM   R0,1,AIACTSN        R0=NUMBER OF TABLE ENTRIES                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
ADDC02   CLC   L'APENSTAT(L'APENACT,R1),TRNKUNT                                 
         BE    ADDC04                                                           
         CLC   L'APENSTAT(L'APENACT,R1),TRNKCUNT                                
         BE    ADDC04                                                           
         SR    RE,RE                                                            
         IC    RE,APELN                                                         
         LA    RE,APEELD(RE)                                                    
         USING APENTRY,RE          RE=A(APEEL ENTRY)                            
         MVC   APENSTAT,0(R1)                                                   
         MVC   APENACT,L'APENSTAT(R1)                                           
         LA    R2,APENACT+L'APENACT-1                                           
         CLI   0(R2),C' '          LOCATE END OF ACCOUNT CODE                   
         BH    *+12                                                             
         MVI   0(R2),0             AND DROP TRAILING SPACES                     
         BCT   R2,*-12                                                          
         LA    R2,1(R2)                                                         
         SR    R2,RE               R2=L'SUB-ELEMENT                             
         STC   R2,APENLEN                                                       
         DROP  RE                                                               
*                                                                               
         SR    RE,RE               INCREMENT ELEMENT LENGTH                     
         IC    RE,APELN                                                         
         AR    RE,R2                                                            
         STC   RE,APELN                                                         
         IC    RE,APENUM           INCREMENT NUMBER OF SUB-ELEMENTS             
         LA    RE,1(RE)                                                         
         STC   RE,APENUM                                                        
*                                                                               
ADDC04   LA    R1,AIACTSL(R1)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,ADDC02                                                        
*                                                                               
ADDC0X   LM    RE,RC,ADDCREGS                                                   
         BR    RE                                                               
         DROP  R1,R2,R3,R4,RF                                                   
*                                                                               
INTALIT  DC    C'INTERAGENCY ESTIMATE AUTO BATCH FOR ADV '                      
INTALDT  DC    C'MMM/YY '                                                       
INTALITL EQU   *-INTALIT                                                        
         EJECT                                                                  
***********************************************************************         
* GENERAL: I/O ROUTINES FOR ACCMST/ACCDIR/ACCDAY/WORKER/CONTROL FILES *         
***********************************************************************         
*                                                                               
IOEXEC   ST    R1,IOCTRL                                                        
         MVI   IOERR,0             CLEAR ERROR BYTE                             
         MVI   IOQUAL,0            ESTABLISH IO QUALIFIERS                      
         TM    IOCTFILE,IORDL      TEST READ DELETES                            
         BZ    *+8                                                              
         OI    IOQUAL,IOQDELT                                                   
*                                  CLEAR READ FOR DELETE BIT                    
         NI    IOCTFILE,255-IORDL                                               
*                                                                               
         LA    R1,IOAREALL         TEST IOAREA NUMBER PASSED                    
         N     R1,IOCTRL           SET IOAREA IN R1                             
         BNZ   *+6                                                              
         DC    H'0'                IOAREA NUMBER MISSING                        
         SRL   R1,4                ESTABLISH IO AREA ADDRESSES                  
         CLM   R1,1,=AL1(IOAREAS)                                               
         BNH   *+6                                                              
         DC    H'0'                INVALID IO AREA                              
         BCTR  R1,0                                                             
         MH    R1,=Y(IOAREALN)                                                  
         L     R0,AIOARE1                                                       
         AR    R1,R0                                                            
         ST    R1,AIOSAVE          SAVE A(SAVED D/A IODA/IOWORK)                
         LA    R1,L'IODA+L'IOWORK(R1)                                           
         ST    R1,AIOBUFF          SAVE A(DATA RECORD AREA)                     
*                                                                               
         LA    R1,IOFILES          ESTABLISH FILE                               
         N     R1,IOCTRL                                                        
         BNZ   *+6                                                              
         DC    H'0'                FILE NUMBER MISSING                          
         SRL   R1,8                                                             
         BCTR  R1,0                                                             
         MH    R1,=Y(L'ACCDIR)                                                  
         LA    R1,ACCDIR(R1)                                                    
         MVC   IOFILE,0(R1)        SET FILE NAME                                
*                                                                               
         LA    R1,IOCMNDS          ESTABLISH DATAMGR COMMAND                    
         N     R1,IOCTRL                                                        
         BNZ   *+6                                                              
         DC    H'0'                COMMAND NUMBER MISSING                       
         BCTR  R1,0                                                             
         SLL   R1,3                                                             
         CLI   IOCTFILE,IOWKWRK    TEST WKR IO (TO WKFILE)                      
         BNE   *+12                                                             
         LA    R1,WKCMNDS(R1)      INDEX INTO WKR COMMANDS                      
         B     IOEX02                                                           
         CLI   IOCTFILE,IOACCIS    TEST I/S IO (TO ACCDIR)                      
         BNE   *+12                                                             
         LA    R1,ISCMNDS(R1)      INDEX INTO I/S COMMANDS                      
         B     IOEX02                                                           
         CLI   IOCTFILE,IOACCDA    TEST D/A IO (TO ACCMST)                      
         BNE   *+12                                                             
         LA    R1,DACMNDS(R1)      INDEX INTO D/A COMMANDS                      
         B     IOEX02                                                           
         CLI   IOCTFILE,IOCTFIL    TEST I/S IO (TO CTFILE)                      
         BNE   *+12                                                             
         LA    R1,ISCMNDS(R1)      I/S COMMANDS FOR CTFILE                      
         B     IOEX02                                                           
         LA    R1,DACMNDS(R1)      D/A COMMANDS FOR ACCDAY                      
*                                                                               
IOEX02   MVC   IOCOMM,0(R1)        SET COMMAND NAME                             
         MVC   IOCTIND,L'IOCOMM(R1)  SET COMMAND TYPE                           
         TM    IOCTFILE,IOWKWRK                                                 
         BNO   IOEX06                                                           
*                                                                               
         ICM   R0,15,AIOBUFF       WKR DATAMGR IO                               
         BNZ   *+6                                                              
         DC    H'0'                A(IO BUFFER) MISSING                         
*                                                                               
         GOTO1 DATAMGR,DMCB,IOCOMM,IOFILE,WKID,(R0),AWKAREA                     
         ORG   *-2                                                              
         TM    IOCTIND,IOCTIUPD    TEST UPDATIVE COMMAND                        
         BZ    *+12                                                             
         CLI   RCWRITE,NO          TEST WRITE=NO REQUESTED                      
         BE    IOEXX                                                            
         BASR  RE,RF               CALL DATAMGR                                 
         MVC   IOERR,8(R1)                                                      
         B     IOEXX                                                            
*                                                                               
IOEX06   CLI   IOCTFILE,IOACCIS    TEST I/S DATAMGR IO                          
         BNE   IOEX10                                                           
         TM    IOCTCOMM,IOHIGH     TEST READ HIGH                               
         BNZ   *+12                                                             
         TM    IOCTCOMM,IOSEQ      TEST READ SEQUENTIAL                         
         BZ    *+10                                                             
         MVC   IOKEYSAV,IOKEY      SAVE KEY                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(IOQUAL,IOCOMM),IOFILE,IOKEY,IOKEY                  
         ORG   *-2                                                              
         TM    IOCTIND,IOCTIUPD    TEST UPDATIVE COMMAND                        
         BZ    *+12                                                             
         CLI   RCWRITE,NO          TEST WRITE=NO REQUESTED                      
         BE    IOEXX                                                            
         BASR  RE,RF               CALL DATAMGR                                 
         MVC   IOERR,8(R1)                                                      
         TM    IOERR,IOEALL-IOEDEL                                              
         BNZ   *+10                                                             
         MVC   IODA,IOKEY+(ACCKDA-ACCRECD)                                      
         B     IOEXX                                                            
*                                                                               
IOEX10   CLI   IOCTFILE,IOACCDA                                                 
         BNE   IOEX14                                                           
         ICM   R0,15,AIOBUFF       D/A DATAMGR IO                               
         BNZ   *+6                                                              
         DC    H'0'                A(IO BUFFER) MISSING                         
*                                                                               
         TM    IOCTIND,IOCTISAD    TEST COMMAND NEEDS SAVED D/A+WORK            
         BZ    *+20                                                             
         L     RF,AIOSAVE                                                       
         MVC   IODA,0(RF)                                                       
         MVC   IOWORK,L'IODA(RF)                                                
         OC    IODAOVER,IODAOVER   TEST OVERRIDE D/A SET                        
         BZ    *+10                                                             
         MVC   IODA,IODAOVER       USE IT                                       
*                                                                               
         GOTO1 DATAMGR,DMCB,(IOQUAL,IOCOMM),IOFILE,IODA,(R0),IOWORK             
         ORG   *-2                                                              
         TM    IOCTIND,IOCTIUPD    TEST UPDATIVE COMMAND                        
         BZ    *+12                                                             
         CLI   RCWRITE,NO          TEST WRITE=NO REQUESTED                      
         BE    IOEXX                                                            
         BASR  RE,RF                                                            
         MVC   IOERR,8(R1)                                                      
         TM    IOERR,IOEALL-IOEDEL                                              
         BNZ   IOEXX                                                            
         L     RF,AIOSAVE                                                       
         MVC   0(L'IODA,RF),IODA                                                
         MVC   L'IODA(L'IOWORK,RF),IOWORK                                       
         B     IOEXX                                                            
*                                                                               
IOEX14   CLI   IOCTFILE,IOCTFIL                                                 
         BNE   IOEX18                                                           
         ICM   R0,15,AIOBUFF       CONTROL FILE I/S I/O                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(IOQUAL,IOCOMM),IOFILE,IOKEY,(R0)                   
         ORG   *-2                                                              
         TM    IOCTIND,IOCTIUPD    TEST UPDATIVE COMMAND                        
         BZ    *+12                                                             
         CLI   RCWRITE,NO          TEST WRITE=NO REQUESTED                      
         BE    IOEXX                                                            
         BASR  RE,RF               CALL DATAMGR                                 
         MVC   IOERR,8(R1)                                                      
         B     IOEXX                                                            
*                                                                               
IOEX18   ICM   R0,15,AIOBUFF       ACCDAY FILE I/O                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   IODA,IODAOVER       OVERRIDE D/A ALWAYS SET                      
         GOTO1 DATAMGR,DMCB,(IOQUAL,IOCOMM),IOFILE,IODA,(R0)                    
         ORG   *-2                                                              
         TM    IOCTIND,IOCTIUPD    TEST UPDATIVE COMMAND                        
         BZ    *+12                                                             
         CLI   RCWRITE,NO          TEST WRITE=NO REQUESTED                      
         BE    IOEXX                                                            
         BASR  RE,RF               CALL DATAMGR                                 
         MVC   IOERR,8(R1)                                                      
         B     IOEXX                                                            
*                                                                               
IOEXX    CLI   RCTRACE,YES         TEST TRACE REQUESTED                         
         BNE   IOEXX2                                                           
         LR    R0,R1               SET R0=A(DMCB)                               
         GOTO1 AIOTRACE                                                         
IOEXX2   XC    IODAOVER,IODAOVER   CLEAR OVERRIDE D/A                           
         TM    IOERR,IOEALL                                                     
         BZ    ROUTE               NO ERROR - CC EQUAL                          
         TM    IOERR,IOEEOF+IOERNF+IOEDEL                                       
         BNZ   ROUTH               LOGICAL ERROR - CC HIGH                      
         B     ROUTL               HARD ERROR - CC LOW                          
         EJECT                                                                  
***********************************************************************         
* GENERAL: ROUTINE TO PRINT TRACE                                     *         
* NTRY: R0=A(DMCB) - SET BY ADDTRN/IOEXEC                             *         
***********************************************************************         
*                                                                               
IOTRACE  LR    R2,R0               R2=A(DMCB)                                   
         L     R4,AGENWK           R4=A(WORK AREA)                              
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVC   P+17(4),=C'RET='    SET DMCB+8 VALUE                             
         XOUT  8(R2),P+21,1                                                     
         LM    RE,RF,0(R2)         ACCOUNT COMMANDS                             
         MVC   P+01(7),0(RE)       SET COMMAND                                  
         MVC   P+09(7),0(RF)       SET FILE                                     
*                                                                               
         CLC   0(5,RF),WKFILE      TEST WORKER FILE                             
         BE    IOTWRK                                                           
         CLC   0(5,RF),ACCMST      TEST ACCMST D/A FILE                         
         BE    IOTMST                                                           
         CLC   0(5,RF),ACCDIR      TEST ACCDIR I/S FILE                         
         BE    IOTDIR                                                           
         CLC   0(5,RF),ACCRCV      TEST ACCRCV FILE                             
         BE    IOTRCV                                                           
         CLC   0(5,RF),ACCDAY      TEST ACCDAY FILE                             
         BE    IOTACD                                                           
         B     IOTRACX                                                          
*                                                                               
IOTACD   L     R3,8(R2)            ACCDAY COMMANDS                              
         MVC   PSECOND+09(4),=C'D/A='                                           
         GOTO1 HEXOUT,PARM,(R3),PSECOND+13,4,HEXTOG                             
         SR    R0,R0                                                            
         CLI   IOERR,0             IF NOT FOUND                                 
         BNE   IOTPRT              DON'T PRINT I/O AREA                         
         L     R3,12(R2)           IOAREA                                       
         ICM   R0,3,0(R3)          RECLEN                                       
         MVI   SKIPSPEC,YES                                                     
         MVI   SPACING,2                                                        
         B     IOTPRT2             NO KEY TO PRINT                              
*                                                                               
IOTWRK   L     R3,8(R2)            WORKER COMMANDS                              
         MVC   P+24(L'WKID),0(R3)                                               
         TR    P+24(L'WKID),CHRTAB                                              
         LA    R0,L'WKID                                                        
         GOTO1 HEXOUT,PARM,(R3),(R4),(R0),HEXSEP                                
         L     RF,AGENWK                                                        
         MVC   PSECOND+24(L'WKID),0(R4)                                         
         MVC   PTHIRD+24(L'WKID),L'WKID(R4)                                     
         SR    R0,R0                                                            
         CLC   P+01(7),WKINDEX     IF INDEX COMMAND                             
         BE    IOTPRT              DON'T PRINT I/O AREA                         
         L     R3,12(R2)                                                        
         ICM   R0,3,0(R3)                                                       
         B     IOTPRT                                                           
*                                                                               
IOTMST   L     R3,8(R2)            ACCMST COMMANDS                              
         MVC   PSECOND+09(4),=C'D/A='                                           
         GOTO1 HEXOUT,PARM,(R3),PSECOND+13,4,HEXTOG                             
         L     R3,12(R2)                                                        
         MVC   P+24(ACCRFST-ACCRECD),0(R3)                                      
         TR    P+24(ACCRFST-ACCRECD),CHRTAB                                     
         LA    R0,ACCRFST-ACCRECD                                               
         GOTO1 HEXOUT,PARM,(R3),(R4),(R0),HEXSEP                                
         MVC   PSECOND+24(ACCRFST-ACCRECD),0(R4)                                
         MVC   PTHIRD+24(ACCRFST-ACCRECD),(ACCRFST-ACCRECD)(R4)                 
         SR    R0,R0                                                            
         ICM   R0,3,ACCRLEN-ACCRECD(R3)                                         
         SH    R0,=Y(ACCRFST-ACCRECD)                                           
         LA    R3,ACCRFST-ACCRECD(R3)                                           
         B     IOTPRT                                                           
*                                                                               
IOTDIR   L     R3,8(R2)            ACCDIR COMMANDS                              
         CLI   IOERR,0             TEST RECORD WAS FOUND                        
         BE    *+8                                                              
         LA    R3,IOKEYSAV         NO - PRINT CALLER'S KEY                      
         MVC   P+24(ACCKLEN),0(R3)                                              
         TR    P+24(ACCKLEN),CHRTAB                                             
         LA    R0,ACCKLEN                                                       
         GOTO1 HEXOUT,PARM,(R3),(R4),(R0),HEXSEP                                
         MVC   PSECOND+24(ACCKLEN),0(R4)                                        
         MVC   PTHIRD+24(ACCKLEN),ACCKLEN(R4)                                   
         SR    R0,R0                                                            
         B     IOTPRT                                                           
*                                                                               
IOTRCV   L     R3,ADTRANS          RECOVERY RECORD (VIA MONACC)                 
         MVC   P+24(L'RCVHDR),4(R3)                                             
         TR    P+24(L'RCVHDR),CHRTAB                                            
         LA    R0,L'RCVHDR                                                      
         LA    RF,4(R3)                                                         
         GOTO1 HEXOUT,PARM,(RF),(R4),(R0),HEXSEP                                
         MVC   PSECOND+24(L'RCVHDR),0(R4)                                       
         MVC   PTHIRD+24(L'RCVHDR),L'RCVHDR(R4)                                 
         ICM   R0,3,0(R3)          L'RECORD                                     
         LA    RF,L'RCVHDR+4                                                    
         SR    R0,RF               LESS L'HEADER                                
         LA    R3,RCVRECRD-RCVRECD(R3)                                          
         B     IOTPRT                                                           
*                                                                               
IOTPRT   MVI   SKIPSPEC,YES        PRINT KEY                                    
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         LTR   R0,R0                                                            
         BZ    IOTRACX                                                          
*                                                                               
IOTPRT2  LA    R2,100              PRINT RECORD DATA                            
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
         GOTO1 HEXOUT,PARM,(R3),(R4),1(R2),HEXSEP                               
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   PSECOND+24(0),0(R4)                                              
         LA    R1,1(R2,R4)                                                      
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   PTHIRD+24(0),0(R1)                                               
         MVI   SKIPSPEC,YES                                                     
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         LA    R2,1(R2)                                                         
         AR    R3,R2                                                            
         SR    R0,R2                                                            
         BP    IOTPRT2                                                          
*                                                                               
IOTRACX  B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* GENERAL: WRITE BATCH ITEM RECORD TO ACC FILE                        *         
***********************************************************************         
*                                                                               
ITMADD   L     R2,AIO1                                                          
         USING TBARECD,R2                                                       
         LA    R3,TBARFST                                                       
         USING BIAELD,R3                                                        
         ZAP   BIAAMT,ITMCASH      SET ITEM AMOUNT                              
         MVC   BIAREF,ITMREF       USE SUPPLIED ITEM REF                        
         TM    PROIND,PROIOTHR     TEST NON-POSTMAN CREATED BATCH               
         BNZ   ITMADD2                                                          
         LH    RF,ITEMSEQ           ELSE USE SEQUENCE NO.                       
         CVD   RF,DUB                                                           
         UNPK  BIAREF,DUB                                                       
         OI    BIAREF+L'BIAREF-1,X'F0'                                          
         MVC   BIAREF,ITMREF                                                    
ITMADD2  L     RF,ITEMEND                                                       
         MVI   0(RF),0             SET EOR                                      
         LA    R0,TBARECD                                                       
         BCTR  R0,0                                                             
         SR    RF,R0                                                            
         STCM  RF,3,TBARLEN        SET RECORD LENGTH                            
         LA    R1,IOADDREC+IOACCMST+IO1                                         
         GOTO1 AIO                 WRITE DATA RECORD                            
         BNE   ITMADD4                                                          
         ICM   RF,15,NBITADD       MAINTAIN ITEM RECORD COUNT                   
         LA    RF,1(RF)                                                         
         STCM  RF,15,NBITADD                                                    
         B     ITMADDX                                                          
*                                                                               
ITMADD4  TM    IOERR,IOEDUP        TEST ITEM ALREADY EXISTS                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1             YES, RE-USE IT (MUST BE DELETED)             
         LA    R2,IOKEY                                                         
         USING TBARECD,R2                                                       
         MVC   TBAKEY,0(R3)        READ IT                                      
         LA    R1,IORDD+IOACCDIR+IO5                                            
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOEDEL        CONFIRM ITEM HAS DELETE STATUS               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGETREC+IOACCMST+IO5                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIOARE5          COPY D/A AND WORK FROM GETREC                
         L     RE,AIOARE1          TO PUTREC                                    
         MVC   0(L'IODA+L'IOWORK,RE),0(RF)                                      
         LA    R1,IOPUTREC+IOACCMST+IO1                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TBAKSTA,TBARSTA-TBARECD(R3)                                      
         MVC   TBAKDA,IODA                                                      
         LA    R1,IOWRITE+IOACCDIR+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ITMADDX  ICM   RF,3,REPPSTG        MAINTAIN POSTING COUNT FOR REPORT            
         AH    RF,ASKLSEQ                                                       
         STCM  RF,3,REPPSTG                                                     
         XC    ASKLSEQ,ASKLSEQ     CLEAR ASKEL SEQUENCE NUMBER                  
         ZAP   ITMCASH,PZERO                                                    
         B     ROUTE                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GENERAL: BUILD BATCH ITEM RECORD IN IOAREA1                         *         
* NTRY - R1=A(AFCEL) OR 0                                             *         
***********************************************************************         
*                                                                               
ITMBLD   L     R2,AIO1                                                          
         USING TBARECD,R2                                                       
         OC    ASKLSEQ,ASKLSEQ     TEST FIRST FOR BATCH ITEM                    
         BNZ   ITMBLD2                                                          
         MVC   TBAKEY,SBLDKEY      BUILD BATCH ITEM                             
         XC    TBARSTA(TBARFST-TBARSTA),TBARSTA                                 
         LH    RF,ITEMSEQ          BUMP AND SAVE ITEM COUNT                     
         LA    RF,1(RF)                                                         
         STH   RF,ITEMSEQ                                                       
         MVC   TBAKTSEQ,ITEMSEQ    SET ITEM SEQUENCE                            
         MVI   TBARESTA,0          ENTRY STATUS                                 
         MVI   TBARESCR,0          CLEAR SCREEN NUMBER                          
         LA    R3,TBARFST                                                       
         USING BIAELD,R3           BUILD ITEM AMOUNT ELEMENT                    
         XC    BIAELD(BIALNQ),BIAELD                                            
         MVI   BIAEL,BIAELQ                                                     
         MVI   BIALN,BIALNQ                                                     
         MVC   BIAREF,SPACES       ITEM REFERENCE                               
         ZAP   BIAAMT,PZERO                                                     
         LA    R3,BIALNQ(R3)                                                    
         LTR   R1,R1               R1=A(AFCEL) OR 0                             
         BZ    ITMBLD4                                                          
         MVC   0(AFCLNQ,R3),0(R1)  COPY AFCEL TO ITEM RECORD                    
         LA    R3,AFCLNQ(R3)                                                    
         B     ITMBLD4                                                          
*                                  ADD AN ASKEL TO BATCH ITEM                   
ITMBLD2  TM    PROIND,PROIOTHR     TEST ONE-POSTING-PER-ITEM BATCH              
         BNZ   ROUTX               YES - WRITE ITEM                             
         L     RF,ITEMEND          RF=A(END-OF-ITEM)                            
         LA    R0,TBARECD          TEST ROOM ON RECORD FOR ELEMENT              
         SR    RF,R0                                                            
         CH    RF,=Y(RECLMAX-ASKLNQ)                                            
         BH    ROUTX               NO - WRITE CURRENT ITEM AWAY                 
         L     R3,ITEMEND                                                       
         USING ASKELD,R3                                                        
ITMBLD4  MVI   ASKEL,ASKELQ        SET ELEMENT CODE                             
         MVI   ASKLN,ASKLNQ        SET ELEMENT LENGTH                           
         LH    RF,ASKLSEQ          BUMP AND SAVE SEQUENCE NO.                   
         LA    RF,1(RF)                                                         
         STH   RF,ASKLSEQ                                                       
         STC   RF,ASKSEQN          SET ELEMENT SEQUENCE NO.                     
         MVC   ASKKEY,STRNKEY      MOVE IN TRANSACTION KEY                      
         TM    PROIND,PROIOTHR     IF ONE POSTING PER ITEM                      
         BZ    *+10                                                             
         MVC   ITMREF,STRNKEY+(TRNKREF-TRNKEY)  SAVE FOR ITEM REF               
         LA    R3,ASKLNQ(R3)                                                    
         ST    R3,ITEMEND          UPDATE A(END-OF-ITEM)                        
         B     ROUTE                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* INTERAGENCY BATCHES: VALIDATE, BUILD AND SAVE ACCOUNT CODES AND     *         
* NAMES FOR INTERAGENCY POSTINGS                                      *         
* NTRY: S2REC=INTERAGENCY RECORD KEY                                  *         
***********************************************************************         
*                                                                               
IVALAC   MVC   WORK(L'IOKEY),IOKEY   SAVE CALLER'S KEY                          
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,S2KIAK       * RECEIVABLE ACCOUNT *                     
         BAS   RE,GETNAM                                                        
         BNE   IVALERR                                                          
         MVC   SRACCNM,TEMP                                                     
*                                                                               
         MVC   SIACC,SPACES                                                     
         MVC   ACC12,SPACES                                                     
         LA    R2,SIACC              * SI ACCOUNT *                             
         MVC   ACTKCPY,S2KCPY        BUILD DEFAULT                              
         MVI   ACTKUNT,C'S'                                                     
         MVI   ACTKLDG,C'I'                                                     
         MVC   ACTKACT(L'INTKMED),S2KIAK+(INTKMED-INTKCULA)                     
         TM    S2DSTA,INTSMI         TEST BUILD FROM MEDIA I/FACE               
         BZ    IVAL06                                                           
         LA    RF,IOKEY                                                         
         USING MINRECD,RF                                                       
         MVC   MINKEY,SPACES                                                    
         MVI   MINKTYP,MINKTYPQ                                                 
         MVC   MINKCPY,S2KCPY                                                   
         MVC   MINKMED,S2KIAK+(INTKMED-INTKCULA)                                
         LA    R1,IOREAD+IOACCDIR+IO4                                           
         GOTO1 AIO                                                              
         BNE   IVAL06              NOT FOUND, USE DEFAULT                       
         LA    R1,IOGET+IOACCMST+IO4                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO4                                                          
         LA    RF,MINRFST                                                       
         USING MDIELD,RF                                                        
IVAL02   CLI   MDIEL,0                                                          
         BE    IVAL06              NOT FOUND, USE DEFAULT                       
         CLI   MDIEL,MDIELQ                                                     
         BE    IVAL04                                                           
         IC    R0,MDILN                                                         
         AR    RF,R0                                                            
         B     IVAL02                                                           
IVAL04   MVC   MIDESC,MDIDESC                                                   
         MVC   ACTKUNT(L'ACTKCULA-L'ACTKCPY),MDICOMM                            
         CLC   MDICOST,SPACES      TEST COSTING A/C SPECIFIED                   
         BNH   IVAL06                                                           
         LA    R2,ACC12            * 12 ACCOUNT *                               
         MVC   ACTKCPY,S2KCPY                                                   
         MVI   ACTKUNT,C'1'                                                     
         MVI   ACTKLDG,C'2'                                                     
         MVC   ACTKACT,MDICOST                                                  
*                                                                               
IVAL06   LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,SIACC                                                   
         BAS   RE,GETNAM                                                        
         BNE   IVALERR                                                          
         MVC   SIACCNM,TEMP                                                     
         TM    SCPYST1,CPYSCOST    TEST COSTING POSTINGS REQUIRED               
         BZ    IVAL16                                                           
         CLC   ACC12,SPACES        TEST 12 ACCOUNT FOUND                        
         BNE   IVAL14                                                           
         MVC   TEMP,SPACES                                                      
         L     R2,AIO4             R2=A(SI ACCOUNT REC)                         
         LA    RF,ACTRFST                                                       
         USING SPAELD,RF                                                        
IVAL08   CLI   SPAEL,0             LOOK FOR 12 ACCOUNT IN HERE                  
         BE    IVAL10                                                           
         CLI   SPAEL,SPAELQ                                                     
         BNE   *+12                                                             
         CLI   SPATYPE,SPATANAL                                                 
         BE    *+14                                                             
         IC    R0,SPALN                                                         
         AR    RF,R0                                                            
         B     IVAL08                                                           
         MVC   TEMP(L'S2KCPY),S2KCPY                                            
         MVC   TEMP+L'S2KCPY(2),=C'12'                                          
         MVC   TEMP+L'S2KCPY+2(L'SPAAANAL),SPAAANAL                             
         B     IVAL14                                                           
*                                                                               
IVAL10   LA    RF,ACTRFST                                                       
         USING RSTELD,RF                                                        
IVAL12   CLI   RSTEL,0             LOOK FOR 12 ACCOUNT IN HERE                  
         BE    IVAL14                                                           
         CLI   RSTEL,RSTELQ                                                     
         BE    *+14                                                             
         IC    R0,RSTLN                                                         
         AR    RF,R0                                                            
         B     IVAL12                                                           
         MVC   TEMP(L'S2KCPY),S2KCPY                                            
         MVC   TEMP+L'S2KCPY(2),=C'12'                                          
         MVC   TEMP+L'S2KCPY+2(L'RSTCOSTG),RSTCOSTG                             
*                                                                               
IVAL14   CLC   ACC12,SPACES                                                     
         BNE   *+10                                                             
         MVC   ACC12,TEMP                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,ACC12                                                   
         CLC   ACTKACT,SPACES      TEST ACCOUNT CODE HAS BEEN BUILT             
         BH    *+12                                                             
         MVI   IACPFLG,NO          SET DON'T DO COSTING POSTINGS                
         B     IVAL16                                                           
         BAS   RE,GETNAM                                                        
         BE    *+8                                                              
         MVI   IACPFLG,NO                                                       
         MVC   ACC12NM,TEMP                                                     
         MVC   ACC11,ACC12                                                      
         MVI   ACC11+2,C'1'        * 11 ACCOUNT *                               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,ACC11                                                   
         BAS   RE,GETNAM                                                        
         BE    *+8                                                              
         MVI   IACPFLG,NO                                                       
         MVC   ACC11NM,TEMP                                                     
*                                                                               
IVAL16   MVC   ACTKEY,SPACES       PRODUCT NAME                                 
         MVC   ACTKCPY,S2KCPY                                                   
         MVC   ACTKUNT(L'SCPYPROD),SCPYPROD                                     
         MVC   ACTKACT(L'INTKCLT+L'INTKPRD),S2KIAK+(INTKCLT-INTKCULA)           
         BAS   RE,GETNAM                                                        
         MVC   PRODNAM,TEMP                                                     
*                                                                               
         MVC   ACC1C,SPACES                                                     
         MVC   ACOFFC,SPACES                                                    
         BAS   RE,GETCOST                                                       
         CLC   ACOFFC,SPACES       TEST OFFICE FOUND BY GETCOST                 
         BE    IVAL18              NO, TRY AGAIN WITH CLIENT REC                
         CLC   ACC1C,SPACES                                                     
         BH    IVAL20                                                           
*                                                                               
IVAL18   MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,S2KCPY                                                   
         MVC   ACTKUNT(L'SCPYPROD),SCPYPROD                                     
         MVC   ACTKACT(L'INTKCLT),S2KIAK+(INTKCLT-INTKCULA)                     
         LA    R1,IORD+IOACCDIR+IO4                                             
         GOTO1 AIO                                                              
         BNE   IVALERR                                                          
         LA    R1,IOGET+IOACCMST+IO4                                            
         GOTO1 AIO                                                              
         BNE   IVALERR                                                          
         BAS   RE,GETCOST                                                       
*                                                                               
IVAL20   MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,ACC1C                                                   
         BAS   RE,GETNAM                                                        
         MVC   ACC1CNM,TEMP                                                     
         B     IVALX                                                            
*                                                                               
IVALERR  BAS   RE,IOTERR           PRINT ERROR TRACE                            
         MVC   IOKEY,WORK          RESTORE CALLER'S KEY                         
         LA    R1,IORD+IOACCDIR+IO4                                             
         GOTO1 AIO                 AND I/O SEQUENCE                             
         B     ROUTH                                                            
*                                                                               
IVALX    MVC   IOKEY,WORK          RESTORE CALLER'S KEY                         
         LA    R1,IORD+IOACCDIR+IO4                                             
         GOTO1 AIO                 AND I/O SEQUENCE                             
         B     ROUTE                                                            
         DROP  R2,RF                                                            
*                                                                               
*                                                                               
* GET NAME FOR AN ACCOUNT, RETURN IN TEMP, CC NEQ = NOT FOUND                   
*                                                                               
GETNAM   ST    RE,BGREGS                                                        
         MVC   TEMP(L'NAMEREC),SPACES                                           
         LA    R1,IORD+IOACCDIR+IO4                                             
         GOTO1 AIO                                                              
         BNE   GETNEQX             ACCOUNT NOT FOUND                            
         LA    R1,IOGET+IOACCMST+IO4                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         L     RF,AIO4                                                          
         LA    RF,ACTRFST-ACTRECD(RF)                                           
         USING NAMELD,RF                                                        
GETN02   CLI   NAMEL,0             NO NAME ON ACCOUNT                           
         BE    GETNEQX                                                          
         IC    RE,NAMLN                                                         
         CLI   NAMEL,NAMELQ                                                     
         BE    *+10                                                             
         AR    RF,RE                                                            
         B     GETN02                                                           
GETN04   SH    RE,=Y((NAMEREC-NAMEL)+1)                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TEMP(0),NAMEREC                                                  
GETEQLX  CLI   *+1,0                                                            
         B     *+8                                                              
GETNEQX  CLI   *+0,0                                                            
GETNX    L     RE,BGREGS           RESTORE RETURN ADDRESS                       
         BR    RE                                                               
         DROP  RF                                                               
*                                                                               
* GET COST ACCOUNT AND/OR PRODUCTION OFFICE FROM CLIENT OR PROD REC             
*                                                                               
GETCOST  ST    RE,BGREGS                                                        
         MVC   TEMP,SPACES                                                      
         L     RF,AIO4             R4=A(CLIENT OR PRODUCT RECORD)               
         LA    R1,ACTRFST-ACTRECD(RF)                                           
         USING PPRELD,R1                                                        
         SR    R0,R0                                                            
GETCO02  CLI   PPREL,0                                                          
         BE    GETCOX                                                           
         CLI   PPREL,PPRELQ                                                     
         BE    *+14                                                             
         IC    R0,PPRLN                                                         
         AR    R1,R0                                                            
         B     GETCO02                                                          
         CLC   ACC1C,SPACES                                                     
         BH    *+10                                                             
         MVC   ACC1C,PPRCOST                                                    
         CLC   ACOFFC,SPACES                                                    
         BH    GETCOX                                                           
         CLC   PPRGAOFF,SPACES                                                  
         BNH   GETCOX                                                           
         MVC   ACOFFC,PPRGAOFF                                                  
GETCOX   L     RE,BGREGS                                                        
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ORDINARY BATCHES: MARK BATCH RECORDS FOR REPORTING BY DAILY JOURNAL *         
* NTRY: FBATTYP VALUE (SET BY FLTBAT) USED AS BRANCH INDEX            *         
*       IO2=BATCH HEADER RECORD                                       *         
***********************************************************************         
*                                                                               
MRKBAT   IC    RF,FBATTYP                                                       
         SLL   RF,24                                                            
         SRL   RF,24-2             FBATTYP * 4 FOR BRANCH TABLE DISP            
         B     *(RF)                                                            
*                                                                               
         B     MKINUP              INSTANT UPDATE BATCH                         
         B     MKACMT              ONLINE UPDATE BATCH                          
         B     MKOVUP              UPDATE OVERNIGHT BATCH                       
         B     MKOVUP              UPDATE DRAFT BATCH                           
         B     MKDELT              DELETE BATCH                                 
         B     MKWARN              SET WARNING ON BATCH                         
         B     MKDULI              DRAFT BATCH DUE LIVE SOON                    
         B     MKOVUP              SET WARNING ON BATCH                         
*                                                                               
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* SET UPDATE DATE FOR INSTANT UPDATE BATCHES, ACCUMULATE POSTINGS     *         
***********************************************************************         
*                                                                               
         USING TBARECD,R2                                                       
MKINUP   L     R2,AIO2                                                          
         MVC   TBAHRUDT,TODAYC                                                  
         GOTO1 AUPDHDR                                                          
         B     MKACMT                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ADD POSTING FIGURES FOR ONLINE/INSTANT BATCHES TO U/L ACCUMS TABLE  *         
***********************************************************************         
*                                                                               
         USING TBARECD,R2                                                       
MKACMT   LA    R2,SBATKEY          USE SAVED BATCH KEY                          
         CLC   SBATITM,TBAKTSEQ    TEST ALL ITEMS ACCOUNTED FOR                 
         BE    MKACX               END OF BATCH                                 
         ICM   R1,3,TBAKTSEQ                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,TBAKTSEQ                                                    
         MVC   IOKEY(L'TBAKEY),SBATKEY                                          
         LA    R1,IORDD+IOACCDIR+IO1                                            
         GOTO1 AIO                                                              
         BE    MKAC02                                                           
         TM    IOERR,IOEDEL        GET NEXT IF ITEM DELETED                     
         BNZ   MKACMT                                                           
         BAS   RE,IOTERR           BATCH RECORD ERROR - PRINT TRACE             
         B     MKACMT                                                           
MKAC02   TM    IOKEY+(TBAKESTA-TBARECD),TBAESLDE                                
         BNZ   MKACMT                                                           
         LA    R1,IOGET+IOACCMST+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         L     R2,AIOBUFF          R2=A(ITEM RECORD)                            
         LA    R2,TBARFST          R2=A(FIRST ELEMENT)                          
         USING ASKELD,R2                                                        
MKAC04   CLI   ASKEL,0             TEST EOR                                     
         BE    MKACMT                                                           
         CLI   ASKEL,ASKELQ        TEST ACCOUNT SYSTEM KEY ELEMENT              
         BNE   MKAC06                                                           
         MVC   IOKEY,ASKKEY        READ TRANSACTION                             
         LA    R1,IORD+IOACCDIR+IO3                                             
         GOTO1 AIO                                                              
         BE    MKAC05                                                           
*                                                                               
         TM    IOERR,IOERNF        IF ERROR IS NOT FOUND                        
         BZ    MKAC04A                                                          
         CLC   ASKKEY+1(2),=C'1R'   AND IT'S A 1R ACCOUNT                       
         BNE   MKAC04A                                                          
         LA    RE,SBATKEY           AND IT'S A TYPE 34                          
         CLI   TBAKBTYP-TBAKEY(RE),TRNTJBTX                                     
         BNE   MKAC04A                                                          
         TM    SCPYST7,CPYSTMSY     AND THEY ARE TMS USERS                      
         BO    MKAC14               DON'T PRINT AN ERROR MESSAGE                
*                                                                               
MKAC04A  BAS   RE,IOTERR                                                        
         B     MKAC14                                                           
*                                                                               
MKAC05   LA    R1,IOGET+IOACCMST+IO3                                            
         GOTO1 AIO                                                              
         BE    MKAC10                                                           
         DC    H'0'                                                             
*                                                                               
MKAC06   CLI   ASKEL,GINELQ                                                     
         BNE   MKAC14                                                           
         USING GINELD,R2                                                        
         LA    R3,IOKEY            READ GIN PASSIVE FOR TRANSACTIONS            
         USING GINPASD,R3                                                       
         XC    GINPKEY,GINPKEY                                                  
         MVI   GINPTYP,GINPTYPQ                                                 
         MVC   GINPCPY,SBATCPY                                                  
         MVC   GINPINV,GININV                                                   
         LA    R1,IOHI+IOACCDIR+IO3                                             
         B     *+8                                                              
MKAC08   LA    R1,IOSEQ+IOACCDIR+IO3                                            
         GOTO1 AIO                                                              
         BE    *+12                                                             
         BAS   RE,IOTERR           NOT FOUND - PRINT ERROR TRACE                
         B     MKAC14                                                           
         CLC   GINPINV,GININV      TEST CHANGE OF INVOICE GROUP                 
         BNE   MKAC14                                                           
         LA    R1,IOGET+IOACCMST+IO3                                            
         GOTO1 AIO                                                              
         BE    MKAC10                                                           
         DC    H'0'                                                             
*                                                                               
MKAC10   L     R1,AIOBUFF                                                       
         GOTO1 ATABMNT             MAINTAIN ACCUMS TABLE                        
         ICM   RF,3,REPPSTG                                                     
         LA    RF,1(RF)                                                         
         STCM  RF,3,REPPSTG                                                     
*                                                                               
MKAC12   CLI   GINEL,GINELQ        IF PROCESSING A GINEL                        
         BE    MKAC08              LOOK FOR ANOTHER TRANSACTION                 
MKAC14   IC    R0,GINLN                                                         
         AR    R2,R0                                                            
         B     MKAC04                                                           
*                                                                               
MKACX    B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* MARK OVERNIGHT/DRAFT BATCH FOR UPDATE OR WARNING - MRKBAT ROUTINE   *         
***********************************************************************         
*                                                                               
         USING TBARECD,R2                                                       
MKOVUP   L     R2,AIO2                                                          
         MVC   TBAHRUDT,TODAYC     UPDATE DATE                                  
         NI    TBAHRIND,TBAHIMLT+TBAHIGDJ                                       
         CLI   FBATIND,0           TEST EXCEPTIONS                              
         BE    MKOV02                                                           
         TM    FBATIND,FBATMOAL    MOA LOCKED                                   
         BZ    *+8                                                              
         OI    TBAHRIND,TBAHIMAL                                                
         TM    FBATIND,FBATUNAP    BATCH UNAPPROVED                             
         BZ    *+8                                                              
         OI    TBAHRIND,TBAHIAAP                                                
         OI    TBAHRIND,TBAHIWRN   SET WARNING                                  
         B     MKOVHDR             UPDATE THE BATCH HEADER ONLY                 
*                                                                               
MKOV02   CLC   TBAHREDT,TODAYC     TEST EFFECTIVE DATE=TODAY                    
         BE    MKOV04                                                           
         LA    R2,IOKEY            ENSURE PASSIVE PTR WITH THIS BATCH           
         L     R1,AIO2             KEY AND EFF/DATE=TODAY DOESN'T               
         MVC   TBAKEY,0(R1)        ALREADY EXIST BECAUSE UPDBAT                 
         MVI   TBAPTYP,TBAPTYPQ    WILL CREATE ONE                              
         MVC   TBAPEFDT,TODAYC                                                  
         MVC   TBAPTSEQ,TBAHRADT                                                
         BAS   RE,MKOVRD           TEST READ FOR PASSIVE PTR                    
         BNE   MKOVHDR             FOUND - UPDATE BATCH HEADER ONLY             
*                                                                               
MKOV04   TM    SBATST1,BHDSACRU    TEST ACCRUAL BATCH                           
*        BZ    MKOV08                                                           
         B     MKOV08              DUP TESTS HANDLED IN ACRREV ROUTINE          
         LA    R2,IOKEY            ENSURE ACCRUAL REVERSAL HEADER CAN           
         L     R1,AIO2             BE ADDED, BEFORE PROCESSING ACCRUAL          
         MVC   TBAKEY,0(R1)                                                     
         OC    STYPARN,STYPARN     TEST PREDEFINED ACCRUAL                      
         BZ    *+10                                                             
         MVC   TBAKBTYP,STYPARN    YES - SET REVERSING BATCH TYPE               
         MVC   TBAKADDT,TODAYCC                                                 
         XC    FULL,FULL                                                        
         MVC   FULL(L'TBAKBMOS),TBAKBMOS                                        
         GOTO1 AADDMON,FULL        INCREMENT BATCH MOA                          
         MVC   TBAKBMOS,FULL                                                    
         MVC   IOKEYSAV,IOKEY      SAVE KEY PRIOR TO READ                       
         BAS   RE,MKOVRD           TEST READ FOR BATCH KEY                      
         BNE   MKOVHDR             FOUND - UPDATE HEADER ONLY                   
         CLC   TBAHREDT,TODAYC     TEST EFFECTIVE DATE=TODAY                    
         BE    MKOV10                                                           
         MVC   IOKEY,IOKEYSAV      RESTORE KEY                                  
         MVI   TBAPTYP,TBAPTYPQ    TEST READ FOR ACCRUAL REVERSAL               
         MVC   TBAPEFDT,TODAYC     PASSIVE PTR WITH TODAY'S DATE TOO            
         MVC   TBAPTSEQ,TBAHRADT                                                
         BAS   RE,MKOVRD           TEST READ FOR PASSIVE PTR                    
         BNE   MKOVHDR             FOUND - UPDATE HEADER ONLY                   
         B     MKOV08                                                           
*                                                                               
MKOV08   L     R2,AIO2                                                          
         CLC   TBAHREDT,TODAYC                                                  
         BE    MKOV10                                                           
         LA    R2,IOKEY                                                         
         L     R1,AIO2                                                          
         MVC   TBAKEY,0(R1)        READ AND DELETE CURRENT PASSIVE PTR          
         MVI   TBAPTYP,TBAPTYPQ                                                 
         MVC   TBAPEFDT,TBAHREDT-TBARECD(R1)                                    
         MVC   TBAPTSEQ,TBAHRADT-TBARECD(R1)                                    
         LA    R1,IORD+IOACCDIR+IO1  READ IT                                    
         GOTO1 AIO                                                              
         BNE   MKOV10                                                           
         OI    TBAKHSTA,TBAHIDEL                                                
         XC    TBAKDA,TBAKDA                                                    
         LA    R1,IOWRITE+IOACCDIR+IO1                                          
         GOTO1 AIO                                                              
*                                                                               
MKOV10   GOTO1 AUPDBAT             UPDATE THE BATCH                             
         B     ROUTE                                                            
*                                  READ FOR VARIOUS BATCH HEADER KEYS           
MKOVRD   ST    RE,BGREGS                                                        
         LA    R1,IORD+IOACCDIR+IO1                                             
         GOTO1 AIO                                                              
         BNE   MKOVRDX             NOT FOUND - OK                               
         L     R1,AIO2                                                          
         SR    R0,R0               ELSE SET STATUS IN BATCH HEADER EL           
         LA    RF,TBARFST-TBARECD(R1)                                           
         USING BHDELD,RF                                                        
         CLI   BHDEL,BHDELQ                                                     
         BE    *+14                                                             
         IC    R0,BHDLN                                                         
         AR    RF,R0                                                            
         B     *-14                                                             
         OI    BHDSTAT1,BHDSPEND   SET UPDATE PENDING                           
         OI    FBATIND,FBATIDUP    SET DUPLICATE INDICATOR                      
         MVI   FBATTYP,0           THIS BATCH CAN'T BE UPDATED TODAY            
         MVC   REPCMNT,=CL30'CANNOT UPDATE'   SET LITERAL FOR REPORT            
         CLI   *+0,1               SET CC NOT EQUAL                             
         B     *+8                                                              
MKOVRDX  CLI   *+1,0               SET CC EQUAL                                 
         L     RE,BGREGS                                                        
         BR    RE                                                               
         DROP  RF                                                               
*                                                                               
MKOVHDR  GOTO1 AUPDHDR             UPDATE THE BATCH HEADER ONLY                 
         B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE DRAFT/OPEN/SAVED BATCH - MRKBAT SUBROUTINE                   *         
***********************************************************************         
*                                                                               
MKDELT   GOTO1 ADELBAT             DELETE THE BATCH                             
         B     ROUTE                                                            
*                                                                               
***********************************************************************         
* WARN OR DELETE OPEN/SAVED BATCH - MRKBAT SUBROUTINE                 *         
***********************************************************************         
*                                                                               
MKWARN   TM    FBATIND,FBATOPEN+FBATSAVD  TEST BATCH IS OPEN/SAVED              
         BZ    MKOVUP              NO - ALLOW WARNING                           
         TM    STYPIND1,TYPIORDS   TEST BATCH CAN PROCESS ORDERS                
         BZ    MKOVUP              NO - ALLOW WARNING                           
         LA    R2,IOKEY            IF ANY ITEM HAS AN ORDER, THE BATCH          
         USING TBARECD,R2          WILL BE DELETED REGARDLESS OF PROFIL         
         L     R1,AIO2                                                          
         MVC   TBAKEY,0(R1)                                                     
         SR    R0,R0                                                            
         ICM   R0,3,SBATITM                                                     
         LA    R1,IORD+IOACCDIR+IO1                                             
         GOTO1 AIO                                                              
MKWA02   LA    R1,IOSEQD+IOACCDIR+IO1                                           
         GOTO1 AIO                                                              
         BNE   *+12                IGNORE DELETED ITEMS                         
         TM    TBAKESTA,TBAESORD   TEST ITEM CARRIES ORDER(S)                   
         BNZ   MKWA04                                                           
         BCT   R0,MKWA02                                                        
         B     MKOVUP              NONE FOUND - ALLOW WARNING                   
*                                                                               
MKWA04   OI    FBATIND,FBATIORD    NOTE BATCH ITEM HAS ORDER(S)                 
         MVI   FBATTYP,FBATDELT    RE-CLASSIFY IT                               
         GOTO1 ADELBAT             AND DELETE IT                                
         B     ROUTE                                                            
*                                                                               
***********************************************************************         
* MARK DRAFT BATCH DUE TO GO LIVE SOON - MRKBAT SUBROUTINE            *         
***********************************************************************         
*                                                                               
         USING TBARECD,R2                                                       
MKDULI   L     R2,AIO2                                                          
         MVC   TBAHRUDT,TODAYC     UPDATE DATE                                  
         NI    TBAHRIND,TBAHIMLT+TBAHIGDJ                                       
         CLI   FBATIND,0           TEST EXCEPTIONS                              
         BE    MKDU02                                                           
         TM    FBATIND,FBATMOAL    MOA LOCKED                                   
         BZ    *+8                                                              
         OI    TBAHRIND,TBAHIMAL                                                
         TM    FBATIND,FBATUNAP    BATCH UNAPPROVED                             
         BZ    *+8                                                              
         OI    TBAHRIND,TBAHIAAP                                                
         OI    TBAHRIND,TBAHIWRN   SET WARNING                                  
         B     *+8                                                              
MKDU02   OI    TBAHRIND,TBAHIUSN   SET UPDATE SOON INDICATOR                    
         GOTO1 AUPDHDR             UPDATE BATCH HEADER                          
         B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* OLD BT7/48/57 BATCHES: BUILD NEW BATCH KEY IN SBLDKEY               *         
***********************************************************************         
*                                                                               
NBKBLD   LA    R2,SBLDKEY          BUILD X'03' BATCH KEY                        
         USING TBARECD,R2                                                       
         L     R3,AIO2             FROM X'0B' BATCH RECORD                      
         USING BATRECD,R3                                                       
         XC    TBAKEY,TBAKEY                                                    
         MVI   TBAKTYP,TBAKTYPQ                                                 
         MVC   TBAKCPY,BATKCPY                                                  
         MVC   TBAKUSER,BATKOFF                                                 
         MVC   TBAKADDT,TODAYCC                                                 
         MVC   TBAKGRUP,BATKGRUP                                                
         MVC   TBAKBTYP,BATKTYPE                                                
         XC    TBAKBMOS,TBAKBMOS   SET FROM FIRST ACCDAY POSTING                
         MVC   TBAKBREF,BATKREF+L'TBAKBMOS                                      
         XC    TBAKBCHR(TBAKLAST-TBAKBCHR),TBAKBCHR                             
*                                                                               
         GOTO1 AGETTYP             LOCATE AND SAVE TYPTAB ENTRY                 
*                                                                               
         MVC   IOKEYSAV,SBLDKEY                                                 
         B     ROUTE                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* TEST FIELD IS VALID PACKED FORMAT                                   *         
***********************************************************************         
*                                                                               
PACTST   LR    RF,R1                                                            
         SRL   RF,32-8             RF=L'INPUT                                   
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BM    PACTNEX             INPUT LENGTH < 1                             
         STC   RF,PKWK                                                          
         SRA   RF,3                                                             
         BNZ   PACTNEX             INPUT LENGTH > 8                             
         IC    RF,PKWK                                                          
         LA    RF,X'F0'(RF)        SET L'PKWK FOR EXECUTE                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  PKWK(0),0(0,R1)     UNPACK INPUT                                 
*                                                                               
         LA    RF,PKWK             TEST ALL NIBBLES ARE 0-9 (BAR LAST)          
         LA    R1,L'PKWK-1                                                      
PACT02   CLI   0(RF),C'0'                                                       
         BL    PACTNEX                                                          
         CLI   0(RF),C'9'                                                       
         BH    PACTNEX                                                          
         LA    RF,1(RF)                                                         
         BCT   R1,PACT02                                                        
*                                  HANDLE LAST BYTE                             
         MVC   PKWK(1),PKWK+L'PKWK-1  COPY FOR DESTRUCTIVE TESTING              
         NI    PKWK,X'F0'          CLEAR NUMERIC NIBBLE                         
         LA    RF,X'A0'            TEST SIGN NIBBLE IS A-F                      
         EX    RF,LASTBYTE                                                      
         BL    PACTNEX                                                          
         MVC   PKWK(1),PKWK+L'PKWK-1                                            
         NI    PKWK,X'0F'          CLEAR SIGN NIBBLE                            
         LA    RF,X'09'            TEST NUMERIC NIBBLE IS 0-9                   
         EX    RF,LASTBYTE                                                      
         BH    PACTNEX                                                          
         B     PACTEQX                                                          
*                                                                               
LASTBYTE CLI   PKWK,0              EXECUTED TEST ON LAST BYTE                   
*                                                                               
PACTEQX  B     ROUTE                                                            
PACTNEX  B     ROUTH                                                            
         EJECT                                                                  
***********************************************************************         
* RECOVERY FILE BATCHES: BUILD BATCH KEY IN SBLDKEY                   *         
***********************************************************************         
*                                                                               
RBKBLD   LA    R2,SBLDKEY          BUILD BATCH KEY                              
         USING TBARECD,R2                                                       
         XC    TBAKEY,TBAKEY                                                    
         MVI   TBAKTYP,TBAKTYPQ                                                 
         MVC   TBAKCPY,S1KCPY                                                   
         MVC   TBAKUSER,S1KUID                                                  
         MVC   TBAKADDT,TODAYCC                                                 
         MVI   TBAKGRUP,TBAGGENQ                                                
         MVC   TBAKBTYP,S1KTYP                                                  
         MVC   TBAKBMOS,S1KMOS                                                  
         MVC   TBAKBREF,S1KREF                                                  
         XC    TBAKBCHR(TBAKLAST-TBAKBCHR),TBAKBCHR                             
         MVC   TBAKBCHR,S1KPID     SET PERSON NUMBER                            
         MVC   SLUID,S1DLUID       SAVE LUID                                    
*                                                                               
         GOTO1 AGETTYP             LOCATE AND SAVE TYPTAB ENTRY                 
         BE    RBKBLD2                                                          
         L     RF,ADEFRTYP                                                      
         MVC   STYPNTRY,0(RF)      NO ENTRY FOR THIS BATCH                      
         MVC   STYPNUM,S1KTYP      USE DEFAULT VALUES - 'UNKNOWN'               
RBKBLD2  DS    0H                                                               
*&&US                                                                           
         TM    STYPIND1,TYPIPRDQ   TEST BATCH GROUP=PROD                        
         BZ    *+8                                                              
         MVI   TBAKGRUP,TBAGPRDQ   SET IT                                       
*&&                                                                             
         MVC   IOKEY,TBARECD                                                    
         MVC   IOKEYSAV,TBARECD                                                 
         GOTO1 AIO,IORDD+IOACCDIR+IO1                                           
         BE    RBKBLD4                                                          
         MVI   TBAPTYP,TBAPTYPQ    NOW READ FOR BATCH PASSIVE POINTER           
         XC    TBAPEFDT,EFFALL                                                  
         MVC   IOKEY,TBARECD                                                    
         MVC   IOKEYSAV,TBARECD                                                 
         GOTO1 AIO,IORDD+IOACCDIR+IO1                                           
         MVI   TBAKTYP,TBAKTYPQ                                                 
         MVC   TBAKADDT,TODAYCC                                                 
         BNE   ROUTE                                                            
*                                                                               
RBKBLD4  IC    RE,TBAKSEQN         BUMP DUPLICATE BATCH SEQUENCE#               
         LA    RE,1(RE)                                                         
         STC   RE,TBAKSEQN                                                      
         B     RBKBLD2                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* READ AND SAVE PROFILES                                              *         
***********************************************************************         
*                                                                               
RDPROF   XC    WORK,WORK                                                        
         MVC   WORK(4),=C'A0JX'                                                 
         CLC   PRFALF,SCPYALF      TEST CHANGE OF AGENCY                        
         BE    RDPR02                                                           
         MVC   PRFALF,SCPYALF                                                   
         MVC   WORK+12(2),PRFALF                                                
         GOTO1 GETPROF,DMCB,(X'C0',WORK),AGYPROFS,DATAMGR                       
*                                                                               
RDPR02   CLC   PRFUSR,SBATUSR      TEST CHANGE OF USER                          
         BE    RDPROFX                                                          
         XC    USRPROFS,USRPROFS                                                
         MVC   PRFUSR,SBATUSR                                                   
         MVC   WORK+12(2),SBATUSR                                               
         GOTO1 GETPROF,DMCB,WORK,USRPROFS,DATAMGR                               
         OC    USRPROFS,USRPROFS                                                
         BNZ   RDPROFX                                                          
         MVC   USRPROFS,AGYPROFS   USE AGENCY PROFILES IF NONE FOR USER         
RDPROFX  B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD REPORT PRINT-LINE FROM BATCH HEADER RECORD                    *         
* NTRY: R1=A(BATCH HEADER RECORD)                                     *         
***********************************************************************         
*                                                                               
REPBAT   LR    R2,R1                                                            
         USING TBARECD,R2                                                       
         MVC   PBMOS,TBAKBMOS      BATCH MOS+REF                                
         OI    PBMOS,X'F0'                                                      
         SR    RE,RE                                                            
         IC    RE,TBAKBMOS+1                                                    
         LA    RE,MOSTAB-1(RE)                                                  
         MVC   PBMOS+1(1),0(RE)                                                 
         MVC   PBREF,TBAKBREF                                                   
         CURED (B1,TBAKSEQN),(3,PBBSQ),0                                        
         LA    R3,TBARFST                                                       
         USING BHDELD,R3                                                        
         SR    R0,R0                                                            
REPB02   CLI   BHDEL,0                                                          
         BE    REPB04              BAD BATCH                                    
         CLI   BHDEL,BHDELQ                                                     
         BE    *+14                                                             
         IC    R0,BHDLN                                                         
         AR    R3,R0                                                            
         B     REPB02                                                           
         MVC   PBNAM,BHDNAME       BATCH NAME                                   
REPB04   CURED (B1,TBAKBTYP),(2,PBTYP),0                                        
         SR    RF,RF                                                            
         ICM   RF,3,STYPBTDD       DISPLACEMENT TO DD EXP                       
         L     RE,ADSLC2                                                        
         AR    RE,RF                                                            
         MVC   PBTYP+3(L'AC@TT001),0(RE)                                        
*                                                                               
REPB06   MVC   SPACES,PPERS                                                     
         OC    TBAKBCHR,TBAKBCHR   TEST BATCHER PRESENT                         
         BZ    REPB12                                                           
         LA    RF,IOKEY                                                         
         USING SA0REC,RF                                                        
         XC    SA0KEY,SA0KEY       READ SECURITY SYSTEM PERSON-ID REC           
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,SCPYALF                                                  
         MVC   SA0KNUM,TBAKBCHR                                                 
         LA    R1,IOREAD+IOCONFIL+IO4                                           
         GOTO1 AIO                                                              
         BNE   REPB10              RECORD NOT FOUND                             
         L     RF,AIOBUFF                                                       
         LA    RF,SA0DATA                                                       
         USING SAPALD,RF                                                        
         SR    R0,R0                                                            
REPB08   CLI   SAPALEL,0           TEST EOR                                     
         BE    REPB10              NOT FOUND - USE NUMBER                       
         CLI   SAPALEL,SAPALELQ    TEST PERSON PERSONAL-ID ELEMENT              
         BE    *+14                                                             
         IC    R0,SAPALLN                                                       
         AR    RF,R0                                                            
         B     REPB08                                                           
         MVC   PPERS,SAPALPID      BATCHER                                      
         B     REPB12                                                           
         DROP  RF                                                               
REPB10   CURED (B2,TBAKBCHR),(L'PPERS,PPERS),0                                  
REPB12   CURED (B2,BHDITEMA),(L'PITMS,PITMS),0                                  
         OC    REPPSTG,REPPSTG     TEST POSTING COUNT EXISTS                    
         BNZ   *+12                                                             
         MVI   PPSTG+(L'PPSTG-1),C'-'  NO - PRINT DASH INSTEAD                  
         B     REPB14                                                           
         CURED (B2,REPPSTG),(L'PPSTG,PPSTG),0                                   
REPB14   CURED (P8,BHDCASHA),(L'PCASH,PCASH),2,MINUS=YES                        
         GOTO1 DATCON,DMCB,(2,TBAHREDT),(8,PEFDT)                               
         GOTO1 DATCON,DMCB,(2,TBAHRADT),(8,PADDT)                               
         XC    REPPSTG,REPPSTG                                                  
         MVC   PCMNT,REPCMNT       COMMENTS                                     
         CLI   FBATTYP,0                                                        
         BNE   *+12                                                             
         CLI   FBATIND,0                                                        
         BE    REPB26                                                           
         LA    RF,PCMNT+L'PCMNT-1  FIND LAST CHAR USED                          
         LA    R1,L'PCMNT-1                                                     
         CLI   0(RF),C' '                                                       
         BH    *+14                                                             
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
         B     *+12                                                             
         MVI   1(RF),C'/'                                                       
         LA    RF,2(RF)            NEXT AVAILABLE POSITION                      
         LA    RE,L'PCMNT-2                                                     
         SR    RE,R1                                                            
         LR    R0,RE               R0=SPACE REMAINING                           
         SR    R1,R1                                                            
         ICM   R1,1,FBATTYP        TEST/LOAD BATCH CLASSIFICATION               
         BNZ   *+12                                                             
         SR    RE,RE                                                            
         BCTR  RF,0                                                             
         B     REPB16              UNCLASSIFIED                                 
         CH    RE,=Y(L'AC@INSUP)   FULL L'LITERAL                               
         BNH   *+8                                                              
         LA    RE,L'AC@INSUP                                                    
         SR    R0,RE               R0 > 0 = ROOM FOR FURTHER COMMENTS           
         BCTR  RE,0                -1 FOR EXECUTE                               
         MH    R1,=Y(6)            DISPLACE TO LITERAL                          
         EX    RE,*+8-6(R1)                                                     
         B     REPB16                                                           
*                                                                               
         MVC   0(0,RF),AC@INSUP                                                 
         MVC   0(0,RF),AC@ONLUP                                                 
         MVC   0(0,RF),AC@OVNUP                                                 
         MVC   0(0,RF),AC@DRMLI                                                 
         MVC   0(0,RF),AC@DRDEL                                                 
         MVC   0(0,RF),AC@DRWAR                                                 
         MVC   0(0,RF),AC@DULIS                                                 
         MVC   0(0,RF),AC@DULIW                                                 
*                                                                               
REPB16   MVI   BYTE,0                                                           
         OC    BYTE,FBATIND        TEST WARNING/ERROR REASON SET                
         BZ    REPB22                                                           
REPB18   LTR   R1,R0                                                            
         BNP   REPB26              NO MORE ROOM                                 
         CLI   BYTE,0                                                           
         BE    REPB22              END OF REASONS                               
         LA    RF,1(RE,RF)                                                      
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         LA    R1,1(R1)            BUMP SPACE REMAINING                         
         BCT   RF,*-12                                                          
         MVI   1(RF),C'/'                                                       
         LA    RF,2(RF)                                                         
         SH    R1,=Y(2)                                                         
         LR    R0,R1                                                            
         LR    RE,R0                                                            
         CH    RE,=Y(L'AC@MOSLK)   TEST REMAINING SPACE > L'LITERAL             
         BNH   *+8                                                              
         LA    RE,L'AC@MOSLK       SET L'LITERAL                                
         SR    R0,RE                                                            
         BCTR  RE,0                -1 FOR EXECUTE                               
         LA    R1,1                                                             
         TM    BYTE,FBATMOAL                                                    
         BZ    *+12                                                             
         NI    BYTE,255-FBATMOAL                                                
         B     REPB20                                                           
         LA    R1,2                                                             
         TM    BYTE,FBATUNAP                                                    
         BZ    *+12                                                             
         NI    BYTE,255-FBATUNAP                                                
         B     REPB20                                                           
         LA    R1,3                                                             
         TM    BYTE,FBATOPEN                                                    
         BZ    *+12                                                             
         NI    BYTE,255-FBATOPEN                                                
         B     REPB20                                                           
         LA    R1,4                                                             
         TM    BYTE,FBATSAVD                                                    
         BZ    *+12                                                             
         NI    BYTE,255-FBATSAVD                                                
         B     REPB20                                                           
         LA    R1,5                                                             
         TM    BYTE,FBATIBER                                                    
         BZ    *+12                                                             
         NI    BYTE,255-FBATIBER                                                
         B     REPB20                                                           
         LA    R1,6                                                             
         TM    BYTE,FBATINOD                                                    
         BZ    *+12                                                             
         NI    BYTE,255-FBATINOD                                                
         B     REPB20                                                           
         LA    R1,7                                                             
         TM    BYTE,FBATIDUP                                                    
         BZ    *+12                                                             
         NI    BYTE,255-FBATIDUP                                                
         B     REPB20                                                           
         LA    R1,8                                                             
         TM    BYTE,FBATIORD                                                    
         BZ    REPB18                                                           
         NI    BYTE,255-FBATIORD                                                
         B     REPB20                                                           
REPB20   MH    R1,=Y(6)                                                         
         EX    RE,*+8-6(R1)                                                     
         B     REPB18                                                           
*                                                                               
         MVC   0(0,RF),AC@MOSLK                                                 
         MVC   0(0,RF),AC@UAPR                                                  
         MVC   0(0,RF),AC@OPEN2                                                 
         MVC   0(0,RF),AC@SAVED                                                 
         MVC   0(0,RF),AC@CRP                                                   
         MVC   0(0,RF),=C'DIR PTR!'                                             
         MVC   0(0,RF),=C'Pending '                                             
         MVC   0(0,RF),=C'Order(s)'                                             
*                                                                               
REPB22   TM    FBATIND,FBATIORD    EXCEPT FOR BATCHES WITH ORDERS               
         BNZ   REPB26                                                           
         CLI   FBATTYP,FBATWARN    IF BATCH IN ERROR                            
         BE    *+12                SHOW DAYS EVEN IF ZERO                       
         CLI   FBATDAYS,0          ELSE TEST WARNING/DUE LIVE DAYS SET          
         BNH   REPB26                                                           
         LTR   R1,R0                                                            
         BNP   REPB26              NO MORE ROOM                                 
         LA    RF,1(RE,RF)                                                      
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         LA    R1,1(R1)            BUMP SPACE REMAINING                         
         BCT   RF,*-12                                                          
         MVI   1(RF),C'/'                                                       
         LA    RF,2(RF)                                                         
         SH    R1,=Y(2)                                                         
         LR    R0,R1                                                            
         LR    RE,R0                                                            
         SH    RE,=Y(3)                                                         
         BM    REPB24              IF < 3 CHARS LEFT, DO NUMBER ONLY            
         CH    RE,=Y(L'AC@DAYS-1)                                               
         BNH   *+8                                                              
         LA    RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,RF),AC@DAYS                                                  
*                                                                               
REPB24   ICM   RE,1,FBATDAYS                                                    
         CVD   RE,DUB                                                           
         UNPK  0(2,RF),DUB                                                      
         OI    1(RF),X'F0'                                                      
*                                                                               
REPB26   MVC   REPCMNT,SPACES                                                   
         LA    RF,HEAD3            BUILD STATUS INFO IN HEAD3/4                 
         USING HSMAPD,RF                                                        
         MVC   HSCPY,REPCPY                                                     
         MVC   HSSLT,=C'Source='                                                
         SR    R1,R1                                                            
         SR    RE,RE                                                            
         IC    R1,SRCIND                                                        
         MH    R1,=Y(6)                                                         
         EX    RE,*+8-6(R1)                                                     
         B     REPB28                                                           
*                                                                               
         MVC   HSSRC,=C'Recovery File  '                                        
         MVC   HSSRC,=C'Worker File    '                                        
         MVC   HSSRC,=C'Old Batch Conv.'                                        
         MVC   HSSRC,=C'Interagency    '                                        
         MVC   HSSRC,=C'All New Batches'                                        
*                                                                               
REPB28   LA    RF,HEAD4                                                         
         MVC   HSCPY(L'REPUSR),REPUSR                                           
         DROP  RF                                                               
REPB30   GOTO1 ACREPORT                                                         
         B     ROUTE                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT ADJ/ADK POSTINGS TOTALS SUMMARY AND FILE RECORD TOTALS        *         
* A LA UPDATE PROGRAM                                                 *         
***********************************************************************         
         USING BOXD,RF                                                          
REPSUM   L     RF,ADBXAREA                                                      
         MVI   BOXREQ,C'C'                                                      
         MVI   FORCEHED,C'N'                                                    
         GOTO1 ACREPORT                                                         
         L     RF,ADBXAREA                                                      
         MVI   BOXCOLS,C' '                                                     
         MVC   BOXCOLS+1(L'BOXCOLS-1),BOXCOLS                                   
         MVI   BOXROWS,C' '                                                     
         MVC   BOXROWS+1(L'BOXROWS-1),BOXROWS                                   
         MVI   BOXCOLS+(LINBXL-LINED),C'L'                                      
         MVI   BOXCOLS+(LINBX1-LINED),C'C'                                      
         MVI   BOXCOLS+(LINBX2-LINED),C'C'                                      
         MVI   BOXCOLS+(LINBXR-LINED),C'R'                                      
         MVI   BOXROWS+05,C'T'                                                  
         MVI   BOXROWS+08,C'M'                                                  
         MVI   BOXROWS+99,C'B'                                                  
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   RCSUBPRG,2                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
         ZAP   TOFUIDDR,PZERO                                                   
         ZAP   TOFUIDCR,PZERO                                                   
*                                  PRINT TOTALS BY USER-ID                      
         USING REPTOTD,R4                                                       
         L     R4,AREPTOT                                                       
REPS2    OC    REPTUID,REPTUID                                                  
         BZ    REPS4                                                            
         LA    R2,LINFILE                                                       
         MVC   0(L'AC@PGA08,R2),AC@PGA08                                        
         BAS   RE,REPSP                                                         
         MVC   0(L'REPTUSR,R2),REPTUSR                                          
         BAS   RE,REPSP                                                         
         MVC   0(L'TODAYD,R2),TODAYD                                            
         CURED REPTDJDR,(L'LINDR,LINDR),2,MINUS=YES                             
         CURED REPTDJCR,(L'LINCR,LINCR),2,MINUS=YES                             
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         LA    R2,LINFILE                                                       
         MVC   0(L'AC@PGA09,R2),AC@PGA09                                        
         BAS   RE,REPSP                                                         
         MVC   0(L'REPTUSR,R2),REPTUSR                                          
         BAS   RE,REPSP                                                         
         MVC   0(L'TODAYD,R2),TODAYD                                            
         CURED REPTDKDR,(L'LINDR,LINDR),2,MINUS=YES                             
         CURED REPTDKCR,(L'LINCR,LINCR),2,MINUS=YES                             
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         AP    TOFUIDDR,REPTDJDR   ACCUMULATE GRAND TOTALS                      
         AP    TOFUIDCR,REPTDJCR                                                
         AP    TOFUIDDR,REPTDKDR                                                
         AP    TOFUIDCR,REPTDKCR                                                
         LA    R4,REPTOTL(R4)                                                   
         B     REPS2                                                            
*                                  PRINT GRAND TOTALS                           
REPS4    L     RF,ADBXAREA                                                      
         MVI   BOXREQ,C'B'                                                      
         GOTO1 ACREPORT                                                         
         MVC   LINFILE(L'AC@TPSTS),AC@TPSTS                                     
         CURED TOFUIDDR,(L'LINDR,LINDR),2,MINUS=YES                             
         CURED TOFUIDCR,(L'LINCR,LINCR),2,MINUS=YES                             
         GOTO1 ACREPORT                                                         
         GOTOR =V(ACSMFBAL),DMCB,(1,(RA)),TOFUIDDR,TOFUIDCR,PZERO,0,0           
*                                  PRINT RECORD COUNTS                          
         L     RF,ADBXAREA                                                      
         MVI   BOXREQ,C'C'                                                      
         GOTO1 ACREPORT                                                         
         L     RF,ADBXAREA                                                      
         MVI   BOXCOLS,C' '                                                     
         MVC   BOXCOLS+1(L'BOXCOLS-1),BOXCOLS                                   
         MVI   BOXROWS,C' '                                                     
         MVC   BOXROWS+1(L'BOXROWS-1),BOXROWS                                   
         MVI   BOXCOLS+(RCTBXL-LINED),C'L'                                      
         MVI   BOXCOLS+(RCTBX1-LINED),C'C'                                      
         MVI   BOXCOLS+(RCTBXR-LINED),C'R'                                      
         MVI   BOXROWS+05,C'T'                                                  
         MVI   BOXROWS+99,C'B'                                                  
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   RCSUBPRG,3                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
         L     R4,ATOTTAB                                                       
         USING TOTTABD,R4          R4=A(TOTALS TABLE)                           
REPS6    CLI   TOTTDISP,TOTTEOTQ   TEST EOT                                     
         BE    REPS10                                                           
         SR    RF,RF                                                            
         ICM   RF,3,TOTTDISP       TAKE DISPLACEMENT TO ACCUMULATOR             
         LA    R1,WORKD                                                         
         TM    TOTTINDS,TOTTITRN   TEST DISPLACEMENT FROM ADDTRN BLOCK          
         BZ    *+8                                                              
         L     R1,ATRNBLK                                                       
         AR    RF,R1                                                            
         ICM   RF,15,0(RF)         RF=ACCUMULATOR VALUE                         
         BZ    REPS8                                                            
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
         MVC   RCTDES,TOTTDESC                                                  
         CURED (RF),(L'RCTTOT,RCTTOT),0                                         
         GOTO1 ACREPORT                                                         
REPS8    LA    R4,TOTTABL(R4)      BUMP TO NEXT TABLE ENTRY                     
         B     REPS6                                                            
         DROP  R4                                                               
*                                                                               
REPS10   L     RF,ADBXAREA                                                      
         MVI   BOXREQ,C'C'                                                      
         GOTO1 ACREPORT                                                         
*                                                                               
REPSUMX  B     ROUTE                                                            
         DROP  RF                                                               
*                                                                               
REPSP    LA    R2,LINFILE+L'LINFILE-1                                           
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE/PUT RECORDS TO SORTER                                    *         
* NTRY: R1=A(A(SORT RECORD)) + HOB=SORT 1/2/3                         *         
***********************************************************************         
*                                                                               
SORTPT   L     R0,0(R1)            R0=A(SORT RECORD)                            
         SR    RF,RF                                                            
         IC    RF,0(R1)            RF=SORT TYPE                                 
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     SORTP1                                                           
         B     SORTP2                                                           
         B     SORTP3                                                           
*                                                                               
SORTP1   CLI   SORTIND,1           TEST INITIALISED                             
         BE    SORTPALL                                                         
         GOTO1 ADSORTER,DMCB,SORTCAD1,SORTREC1,0                                
         MVI   SORTIND,1                                                        
         B     SORTPALL                                                         
*                                                                               
SORTP2   CLI   SORTIND,2           TEST INITIALISED                             
         BE    SORTPALL                                                         
         GOTO1 ADSORTER,DMCB,SORTCAD2,SORTREC2,0                                
         MVI   SORTIND,2                                                        
         B     SORTPALL                                                         
*                                                                               
SORTP3   CLI   SORTIND,3           TEST INITIALISED                             
         BE    SORTPALL                                                         
         GOTO1 ADSORTER,DMCB,SORTCAD3,SORTREC3,0                                
         MVI   SORTIND,3                                                        
         B     SORTPALL                                                         
*                                                                               
SORTPALL GOTO1 ADSORTER,DMCB,SORTPUT,(R0)                                       
*                                                                               
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* ADD ON-LINE/RECOVERY POSTING AMOUNTS TO TABLE OF U/L ACCUMULATORS   *         
* NTRY: R1=A(TRANSACTION RECORD)                                      *         
***********************************************************************         
*                                                                               
         USING TRNRECD,R1                                                       
TABMNT   TM    TRNRSTAT,TRNSDRFT   IGNORE DRAFT TRANSACTIONS                    
         BNZ   TABMX                                                            
         LA    R2,TRNRFST                                                       
         USING TRNELD,R2                                                        
TABM02   CLI   TRNEL,0                                                          
         BE    TABMX               EOR                                          
         CLI   TRNEL,TRNELQ                                                     
         BE    TABM06                                                           
TABM04   IC    R0,TRNLN                                                         
         AR    R2,R0                                                            
         B     TABM02                                                           
*                                                                               
TABM06   ZAP   DUB,TRNAMNT                                                      
         LA    RE,ACMTDR-ACMTABD   SET DISP TO DR/CR IN TABLE ENTRY             
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+8                                                              
         LA    RE,ACMTCR-ACMTABD                                                
         L     RF,AACMTAB          LOCATE TABLE ENTRY FOR U/L                   
         USING ACMTABD,RF                                                       
TABM08   CLI   ACMTCUL,0                                                        
         BE    TABM10              CREATE A NEW ENTRY                           
         CLI   ACMTCUL,EFFS                                                     
         BE    TABM04              EOT REACHED                                  
         CLC   ACMTCUL,TRNKEY                                                   
         BE    TABM12                                                           
         LA    RF,ACMTABL(RF)                                                   
         B     TABM08                                                           
*                                                                               
TABM10   MVC   ACMTCUL,TRNKEY                                                   
         ZAP   ACMTDR,PZERO                                                     
         ZAP   ACMTCR,PZERO                                                     
TABM12   AR    RE,RF               DISPLACE TO DR/CR ACCUMULATOR                
         AP    0(8,RE),DUB                                                      
         B     TABM04                                                           
*                                                                               
TABMX    B     ROUTE                                                            
         DROP  R1,R2,RF                                                         
         EJECT                                                                  
***********************************************************************         
* OLD BT7/48/57 BATCHES: PROCESS AN ACCDAY RECORD, ADD ITEM RECORD    *         
* AND TRANSACTIONS. NTRY: AIO4=ACCDAY RECORD                          *         
***********************************************************************         
*                                                                               
ADDITE   L     R2,AIO4                                                          
         LA    R2,2(R2)            R2=A(FIRST ELEMENT ON RECORD)                
*                                                                               
         USING DLPOSTD,R2          R2=A(FIRST ACCDAY RECORD ELEMENT)            
ADDITE02 CLI   DLPSEL,0            TEST END OF RECORD                           
         BE    ADDITE09                                                         
*                                                                               
         CLI   DLPSEL,DLPSEDRQ     TEST SINGLE DEBIT                            
         BNE   ADDITE04                                                         
         BAS   RE,APEADR                                                        
*&&US                                                                           
         CLC   =C'1C',DLPSDBU                                                   
         BNE   ADDITE08                                                         
         CLC   =C'13',DLPSCRU                                                   
         BNE   ADDITE08                                                         
         BAS   RE,APEACR                                                        
*&&                                                                             
         B     ADDITE08                                                         
*                                                                               
ADDITE04 CLI   DLPSEL,DLPSECRQ     TEST SINGLE DEBIT                            
         BNE   ADDITE06                                                         
         BAS   RE,APEACR                                                        
*&&US                                                                           
         CLC   =C'1C',DLPSCRU                                                   
         BNE   ADDITE08                                                         
         CLC   =C'13',DLPSDBU                                                   
         BNE   ADDITE08                                                         
         BAS   RE,APEADR                                                        
*&&                                                                             
         B     ADDITE08                                                         
*                                                                               
ADDITE06 CLI   DLPSEL,DLPSEDCQ     TEST DEBIT/CREDIT                            
         BNE   ADDITE08                                                         
         BAS   RE,APEADR                                                        
         BAS   RE,APEACR                                                        
*                                                                               
ADDITE08 IC    R0,DLPSLEN          BUMP TO NEXT ACCDAY ELEMENT                  
         AR    R2,R0                                                            
         B     ADDITE02                                                         
*                                                                               
ADDITE09 L     R1,ATRNBLK                                                       
         USING TRNBLKD,R1                                                       
         MVC   TRNBSEQN,=H'1'      SET ITEM SEQUENCE NUMBER                     
         XC    TRNPUSER,TRNPUSER                                                
         XC    TRNBMOS,TRNBMOS     PWOS P'YYMM'                                 
         MVC   TRNEFDT,TODAYC                                                   
         L     R2,AIO4                                                          
         LA    R2,2(R2)                                                         
         USING DLDESCD,R2                                                       
*                                                                               
ADDITE10 L     R3,ATRNWK           R3=A(TRNEL TO BE BUILT)                      
         XC    0(256,R3),0(R3)     CLEAR TRANSACTION ELEMENT AREA               
         USING TRNELD,R3           CREATE A TRANSACTION ELEMENT                 
         L     RF,AIO2             RF=A(OLD BATCH RECORD)                       
         USING BATRECD,RF                                                       
         MVI   TRNEL,TRNELQ        ELEMENT CODE                                 
         MVC   TRNTYPE,BATKTYPE    BATCH TYPE                                   
         MVC   TRNMOS,BATKREF      BATCH MONTH                                  
         MVC   TRNBREF,BATKREF+L'TRNMOS   BATCH REFERENCE                       
         MVC   TRNDATE,DLDSDATE    TRANSACTION DATE                             
         MVC   TRNREF,DLDSREF      TRANSACTION REFERENCE                        
         MVC   TRNSTAT,DLDSSTAT    TRANSACTION STATUS                           
*                                                                               
         LA    RF,SBLDKEY          RF=A(NEW BATCH KEY)                          
         USING TBARECD,RF                                                       
         OC    TBAKBMOS,TBAKBMOS   TEST BMOS NOT YET SET                        
         BNZ   ADDITE11                                                         
         GOTO1 VCONVMOS,DMCB,(X'FE',ATRNWK),TBAKBMOS  SET FROM 1ST TRNS         
         DROP  RF                                                               
*                                                                               
ADDITE11 SR    RF,RF                                                            
         IC    RF,DLDSLEN                                                       
         SH    RF,=Y(DLDSLN1Q+1)                                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TRNNARR(0),DLDSNARR TRANSACTION NARRATIVE                        
*                                                                               
         LA    RF,TRNLN1Q+1(RF)                                                 
         STC   RF,TRNLN                                                         
         LA    RF,TRNELD(RF)                                                    
         MVI   0(RF),0                                                          
         XC    AIADDRS(AIADDRSL),AIADDRS                                        
*                                                                               
ADDITE12 SR    R0,R0               POINT TO NEXT POSTING ELEMENT                
         IC    R0,DLDSLEN                                                       
         AR    R2,R0                                                            
         CLI   DLDSEL,0            TEST END OF RECORD                           
         BE    ADDITE32                                                         
*                                                                               
         CLI   DLDSEL,DLPSEDCQ     REGULAR POSTING ELEMENTS                     
         BL    *+12                                                             
         CLI   DLDSEL,DLPSECRQ     ARE 68,69 AND 6A                             
         BNH   ADDITE14                                                         
         CLI   DLDSEL,DLDSELQ      ANOTHER DESCRIPTION ELEMT MAY FOLLOW         
         BNE   ADDITE13                                                         
         CLI   TRNTYPE,BT57        FOR THIS BATCH TYPE                          
         BNE   ADDITE13                                                         
         GOTO1 AITMADD             ADD PENDING ITEM                             
         B     ADDITE10            AND LOOP BACK                                
*                                                                               
ADDITE13 ST    R2,AIALAST          SET A(LAST EXTRA ELEMENT)                    
         OC    AIAFRST,AIAFRST                                                  
         BNZ   *+8                                                              
         ST    R2,AIAFRST          SET A(FIRST EXTRA ELEMENT)                   
         B     ADDITE12                                                         
*                                                                               
         USING DLPOSTD,R2                                                       
ADDITE14 L     R0,AIO3             CLEAR IOAREA FOR EACH TRANSACTION            
         LH    R1,=Y(IOALN)                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     RE,AIO3             =TRNREC IN ADDTRN BLOCK                      
         USING TRNRECD,RE                                                       
         MVC   TRNKCULA,DLPSDBAC   ASSUME DEBIT(69) OR 68 (DOUBLE)              
         MVC   TRNKCULC,DLPSCRAC                                                
         MVC   TRNKDATE,TRNDATE                                                 
         MVC   TRNKREF,TRNREF                                                   
         MVI   TRNKSBR,0                                                        
         XC    TRNKSTA,TRNKSTA                                                  
         NI    TRNSTAT,255-TRNSHOLD-TRNSBREC-TRNSNOCM                           
         TM    DLPSTYPE,DLPSTNCM                                                
         BZ    *+8                                                              
         OI    TRNSTAT,TRNSNOCM    NON-COMMISSIONABLE                           
         TM    DLPSTYPE,DLPSTBRC                                                
         BZ    *+8                                                              
         OI    TRNSTAT,TRNSBREC    RECONCILED ADVANCE PAYMENT                   
         TM    DLPSTYPE,DLPSTHCQ                                                
         BZ    *+8                                                              
         OI    TRNSTAT,TRNSHOLD    HOLD FOR GERMAN CHEQUES                      
         ZAP   TRNAMNT,DLPSAMNT                                                 
*                                                                               
ADDITE16 MVC   TRNOFFC,DLPSANAL                                                 
         OI    TRNSTAT,TRNSDR      SET DEBIT                                    
         CLI   DLPSEL,DLPSECRQ                                                  
         BNE   ADDITE18                                                         
         NI    TRNSTAT,255-TRNSDR                                               
         MVC   TRNKCULA,DLPSCRAC                                                
         MVC   TRNKCULC,DLPSDBAC                                                
*                                                                               
ADDITE18 MVC   TRNKWORK,SPACES     SET ANALYSIS CODE FOR PRODUCTION             
         CLC   =C'SJ',TRNKUNT                                                   
         BNE   *+10                                                             
         MVC   TRNKWORK,DLPSANAL                                                
*                                                                               
         L     R1,ATRNBLK                                                       
         MVC   TRNCACNM,DLPSDBNM   IF CREDIT, CONTRA NAME IS DEBIT NAME         
         TM    TRNSTAT,TRNSDR                                                   
         BZ    ADDITE20                                                         
         MVC   TRNCACNM,DLPSCRNM   IF DEBIT, CONTRA NAME IS CREDIT NAME         
         CLI   TRNKUNT,C'S'        TEST SUBSIDIARY UNIT                         
         BNE   ADDITE20                                                         
         AP    BATCASH,TRNAMNT                                                  
         DROP  RE                                                               
ADDITE20 GOTO1 UPDTRN              UPDATE FILES                                 
ADDITE22 GOTO1 AITMBLD,0                                                        
         BE    ADDITE24                                                         
         GOTO1 AITMADD                                                          
         B     ADDITE22                                                         
*                                                                               
ADDITE24 AP    ITMCASH,TRNAMNT                                                  
         CLI   DLPSEL,DLPSEDCQ     DO CREDIT SIDE OF A DOUBLE                   
         BNE   ADDITE30                                                         
         L     RE,AIO3             =TRNREC IN ADDTRN BLOCK                      
         USING TRNRECD,RE                                                       
         NI    TRNSTAT,255-TRNSDR                                               
         MVC   TRNKCULA,DLPSCRAC                                                
         MVC   TRNKCULC,DLPSDBAC                                                
         MVC   STRNKEY,TRNKEY      SET KEY FOR ITMBLD                           
         DROP  RE                                                               
         L     R1,ATRNBLK                                                       
         MVC   TRNCACNM,DLPSDBNM   CONTRA NAME IS DEBIT NAME                    
         GOTO1 UPDTRN                                                           
ADDITE26 GOTO1 AITMBLD,0                                                        
         BE    ADDITE28                                                         
         GOTO1 AITMADD                                                          
         B     ADDITE26                                                         
ADDITE28 TM    PROIND,PROIOTHR     IF ONE POSTING PER ITEM                      
         BZ    ADDITE30                                                         
         AP    ITMCASH,TRNAMNT     TAKE CREDIT AS ITEM AMOUNT                   
*                                                                               
ADDITE30 XC    AIAFRST,AIAFRST                                                  
         XC    AIALAST,AIALAST                                                  
         B     ADDITE12                                                         
*                                                                               
ADDITE32 GOTO1 AITMADD             ADD THE (LAST) ITEM                          
*                                                                               
         L     R1,ATRNBLK                                                       
         OI    TRNINDS,TRNICONV+TRNILAST                                        
         OI    TRNINDS2,TRNIUPDG                                                
         GOTO1 VADDTRN             CALL ADDTRN TO WRITE ACCOUNT ETC.            
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERRORS                            
*                                                                               
         NI    TRNINDS,255-TRNILAST  SWITCH OFF LAST CALL BIT                   
         B     ROUTE                                                            
         DROP  R1,R2,R3                                                         
*                                                                               
* ROUTINE TO ADD AN ENTRY TO TABLE OF PRIMARY ACCOUNTS FOR POSTING              
*                                                                               
         USING DLPOSTD,R2          R2=A(FIRST ACCDAY ELEMENT)                   
APEADR   LA    R1,DLPSDBU          R1=A(DEBIT ACCOUNT CODE)                     
         MVI   GPFLAG,APENSDR      SET STATUS TO DEBIT                          
         B     APEADD                                                           
*                                                                               
APEACR   LA    R1,DLPSCRU          R1=A(CREDIT ACCOUNT CODE)                    
         MVI   GPFLAG,0            SET STATUS TO CREDIT                         
*                                                                               
APEADD   LA    RF,AIACTS           RF=A(TABLE OF PRIMARY ACCOUNTS)              
         LA    R0,AIACTSM          R0=MAX NUMBER OF ENTRIES IN TABLE            
APEADD02 OC    0(AIACTSL,RF),0(RF) TEST END OF TABLE                            
         BZ    APEADD04                                                         
         CLC   0(L'APENSTAT,RF),GPFLAG                                          
         BNE   *+12                                                             
         CLC   L'APENSTAT(L'APENACT,RF),0(R1)                                   
         BER   RE                                                               
         LA    RF,AIACTSL(RF)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,APEADD02                                                      
         BR    RE                                                               
*                                                                               
APEADD04 MVC   0(L'APENSTAT,RF),GPFLAG                                          
         MVC   L'APENSTAT(L'APENACT,RF),0(R1)                                   
         CLC   =C'SE',0(R1)        TEST PRIMARY SE POSTING                      
         BNE   *+8                                                              
         OI    AIACTI,AIACTISE     SET SE INDICATOR                             
*                                                                               
APEADD06 IC    RF,AIACTSN          INCREMENT NUMBER OF TABLE ENTRIES            
         LA    RF,1(RF)                                                         
         STC   RF,AIACTSN                                                       
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CALL ADDTRN TO ADD A DRAFT OR LIVE TRANSACTION                      *         
***********************************************************************         
*                                                                               
UPDTRN   NTR1  ,                                                                
         L     R2,AIO3             =TRNREC IN ADDTRN BLOCK                      
         USING TRNRECD,R2          R2=A(TRANSACTION RECORD KEY)                 
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3           R3=A(TRANSACTION ELEMENT)                    
         L     RF,ATRNWK                                                        
         MVC   TRNELD(256),0(RF)   RF=A(TRNEL BUILT BY ADDITE)                  
         TM    SCPYST4,CPYSOFF2    IF 2-CHARACTER OFFICES                       
         BZ    *+10                                                             
         MVC   TRNKOFF,TRNOFFC     SET OFFICE IN KEY FROM TRNEL                 
         SR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         LA    RF,TRNELD(RF)                                                    
         SH    RF,=Y(TRNLNBDQ)                                                  
*&&UK                                                                           
         CLI   TRNLN,136           TEST FOR BILLING NARRATIVE                   
         BNE   UPDTRN14                                                         
         CLI   TRNNARR+14,X'FF'                                                 
         BNE   UPDTRN14                                                         
*&&                                                                             
*&&US                                                                           
         CLC   0(L'TRNDESC,RF),UC@MANBL                                         
         BE    UPDTRN02                                                         
         CLC   0(L'TRNDESC,RF),UC@CLIBG                                         
         BE    UPDTRN02                                                         
         CLC   0(L'TRNDESC,RF),UC@CLIWB                                         
         BE    UPDTRN02                                                         
         B     UPDTRN14                                                         
*&&                                                                             
UPDTRN02 CLC   =C'SJ',TRNKUNT                                                   
         BNE   UPDTRN04                                                         
         CLC   =C'99',TRNKWORK     TEST BILL POSTING                            
         BNE   UPDTRN04                                                         
*                                                                               
         ZIC   R1,TRNLN            SAVE ORIGINAL LENGTH                         
         MVC   TRNNARR(TRNLNBDQ),0(RF)                                          
         MVI   TRNLN,TRNLNBQ                                                    
         SH    R1,=Y(TRNLNBQ)      FIND DIFFERENCE IN LENGTHS                   
         BNP   UPDTRN14            ELEMENT WAS NOT SHORTENED                    
         ZIC   RE,TRNLN            GET NEW LENGTH                               
         LA    RE,TRNELD(RE)       POINT TO END OF ELEMENT                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,RE),0(RE)       CLEAR END OF OLD ELEMENT                     
         B     UPDTRN14                                                         
*                                                                               
UPDTRN04 SR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         SH    RF,=Y(TRNLNBDQ)                                                  
         BCTR  RF,0                DROP TRAILING CHARACTER                      
         STC   RF,TRNLN                                                         
         LA    RF,TRNELD(RF)                                                    
         LH    RE,=Y(TRNLNBDQ)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,RF),0(RF)       CLEAR END OF ELEMENT                         
*                                                                               
UPDTRN14 SR    RE,RE                                                            
         IC    RE,TRNLN                                                         
         LA    RE,TRNELD(RE)                                                    
         MVI   0(RE),0                                                          
         DROP  R3                                                               
*                                                                               
         ST    RE,FULL             SAVE A(END OF RECORD)                        
         ICM   R0,15,AIAFRST       TEST/SET A(FIRST EXTRA ELEMENT)              
         BZ    UPDTRN16                                                         
         ICM   R1,15,AIALAST       SET A(LAST EXTRA ELEMENT)                    
         SR    RF,RF                                                            
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         SR    R1,R0               R1=L'EXTRA ELEMENTS                          
         LA    RF,0(R1,RE)                                                      
         ST    RF,FULL             SAVE A(NEW END OF RECORD)                    
         LR    RF,R1                                                            
         MVCL  RE,R0               MOVE EXTRA ELEMENT(S) TO RECORD              
*                                                                               
UPDTRN16 TM    AIACTI,AIACTISE     TEST SE ACCOUNT INVOLVED                     
         BZ    UPDTRN24                                                         
*                                                                               
         CLC   =C'SE',TRNKUNT                                                   
         BE    UPDTRN18                                                         
         CLC   =C'SE',TRNKCUNT                                                  
         BE    UPDTRN18                                                         
*                                                                               
         CLC   =C'1C',TRNKUNT                                                   
         BNE   UPDTRN24                                                         
         CLC   =C'13',TRNKCUNT                                                  
         BNE   UPDTRN24                                                         
*                                                                               
UPDTRN18 L     RF,FULL                                                          
         USING APEELD,RF           RF=A(ANALYSIS POINTER ELEMENT)               
         MVI   APEEL,APEELQ                                                     
         MVI   APELN,APELN1Q                                                    
         MVI   APENUM,0                                                         
         LA    R1,AIACTS           R1=A(TABLE OF ANALYSIS ACCOUNTS)             
         SR    R0,R0                                                            
         ICM   R0,1,AIACTSN        R0=NUMBER OF TABLE ENTRIES                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDTRN20 CLC   L'APENSTAT(L'APENACT,R1),TRNKUNT                                 
         BE    UPDTRN22                                                         
         CLC   L'APENSTAT(L'APENACT,R1),TRNKCUNT                                
         BE    UPDTRN22                                                         
         SR    RE,RE                                                            
         IC    RE,APELN                                                         
         LA    RE,APEELD(RE)                                                    
         USING APENTRY,RE          RE=A(APEEL ENTRY)                            
         MVC   APENSTAT,0(R1)                                                   
         MVC   APENACT,L'APENSTAT(R1)                                           
         LA    R3,APENACT+L'APENACT-1                                           
         CLI   0(R3),C' '          LOCATE END OF ACCOUNT CODE                   
         BH    *+12                                                             
         MVI   0(R3),0             AND DROP TRAILING SPACES                     
         BCT   R3,*-12                                                          
         LA    R3,1(R3)                                                         
         SR    R3,RE               R3=L'SUB-ELEMENT                             
         STC   R3,APENLEN                                                       
         DROP  RE                                                               
*                                                                               
         SR    RE,RE               INCREMENT ELEMENT LENGTH                     
         IC    RE,APELN                                                         
         AR    RE,R3                                                            
         STC   RE,APELN                                                         
         IC    RE,APENUM           INCREMENT NUMBER OF SUB-ELEMENTS             
         LA    RE,1(RE)                                                         
         STC   RE,APENUM                                                        
*                                                                               
UPDTRN22 LA    R1,AIACTSL(R1)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,UPDTRN20                                                      
         CLI   APELN,APELN1Q       TEST ANY POINTERS IN ELEMENT                 
         BNE   UPDTRN24                                                         
         XC    APEELD(APELN1Q),APEELD                                           
         DROP  RF                                                               
*                                                                               
         USING TRNBLKD,R1                                                       
UPDTRN24 L     R1,ATRNBLK                                                       
         OI    TRNINDS,TRNICONV+TRNIDRFT                                        
         OI    TRNINDS2,TRNIADDG                                                
         GOTO1 VADDTRN             CALL ADDTRN TO ADD TRANSACTION               
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERRORS                            
         MVC   STRNKEY,TRNKEY      SET KEY FOR ITMBLD                           
*                                                                               
UPDTRNX  B     ROUTE                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* WORKER FILE BATCHES: BUILD AND ADD TRANSACTION RECORD               *         
***********************************************************************         
*                                                                               
         USING PSHEADD,R4          R4=A(WORKER FILE POSTING)                    
TRNBLD   L     R2,AIO3                                                          
         USING TRNRECD,R2          R2=A(TRANSACTION RECORD)                     
         XC    TRNKEY(256),TRNKEY  INITIALIZE TRANSACTION RECORD                
*                                                                               
         L     R1,ATRNBLK                                                       
         USING TRNBLKD,R1                                                       
         MVC   TRNCACNM,PSHDSBNM   SAVE CONTRA-ACCOUNT NAME                     
         DROP  R1                                                               
         MVC   TRNKCULA,PSHDACC    ACCOUNT CODE                                 
         MVC   TRNKOFF,PSHDANAL    OFFICE/ANALYSIS CODE                         
         MVC   TRNKCULC,PSHDSBAC   CONTRA-ACCOUNT CODE                          
         SR    R3,R3                                                            
         IC    R3,PSHDLEN                                                       
         AR    R3,R4                                                            
         USING TRNELD,R3           R3=A(TRANSACTION ELEMENT)                    
         OC    TRNBTCH,SPACES                                                   
         OC    TRNREF,SPACES                                                    
         MVC   TRNKDATE,TRNDATE    TRANSACTION DATE                             
         MVC   TRNKREF,TRNREF      TRANSACTION REFERENCE                        
         MVC   TRNKSBR,TRNSUB      TRANSACTION SUB-REFERENCE                    
         TM    TRNSTAT,TRNSDR                                                   
         BZ    *+16                                                             
         AP    BATCASH,TRNAMNT     ACCUMULATE BATCH HDR CASH TOTAL              
         AP    ITMCASH,TRNAMNT     ACCUMULATE BATCH ITEM CASH TOTAL             
*                                                                               
         LA    R1,TRNELD           LOCATE END OF INPUT RECORD                   
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   *-10                                                             
*                                                                               
         LA    R0,TRNRFST                                                       
         LA    R1,1(R1)                                                         
         SR    R1,R3                                                            
         LA    RE,TRNELD                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE ELEMENTS TO TRANSACTION RECORD          
*                                                                               
         LA    R1,TRNRFST                                                       
         SR    R0,R0                                                            
         SR    RE,RE                                                            
TRNBLD04 IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   TRSEL-TRSELD(R1),TRSELQ                                          
         BNE   TRNBLD05                                                         
         CLI   TRSLN-TRSELD(R1),TRSLNQ                                          
         BL    TRNBLD05                                                         
         LR    RE,R1               SAVE A(TRANSACTION STATUS ELEMENT)           
TRNBLD05 CLI   0(R1),0             TEST EOR                                     
         BNE   TRNBLD04                                                         
*                                                                               
         LA    R1,1(R1)            SET RECORD LENGTH                            
         SR    R1,R2                                                            
         STCM  R1,3,TRNRLEN                                                     
*                                                                               
         L     R1,ATRNBLK                                                       
         USING TRNBLKD,R1                                                       
         XC    TRNBMOS,TRNBMOS     SET ADDTRN EXTRA VALUES                      
         XC    TRNBSEQN,TRNBSEQN                                                
         XC    TRNPUSER,TRNPUSER                                                
         OI    TRNINDS,TRNICONV+TRNIDRFT  ADD TRANSACTION AS DRAFT              
         LTR   RE,RE               TEST STATUS ELEMENT FOUND                    
         BZ    TRNBLD06                                                         
         MVC   TRNBMOS,TRSPMOS-TRSELD(RE)                                       
         MVC   TRNBSEQN,TRSBSEQ-TRSELD(RE)                                      
         MVC   TRNPUSER,TRSUSER-TRSELD(RE)                                      
*                                                                               
TRNBLD06 OC    TRNKULA,SPACES                                                   
         OC    TRNKULC,SPACES                                                   
         OI    TRNINDS2,TRNIADDG                                                
         GOTO1 VADDTRN             ADD TRANSACTION TO FILE                      
         BNE   ROUTX               EXIT WITH CC NEQ                             
*                                                                               
         MVC   STRNKEY,TRNKEY      SET TRANS KEY FOR ITMBLD                     
         B     ROUTE               GET NEXT RECORD                              
         DROP  R1,R2,R3,R4                                                      
         EJECT                                                                  
***********************************************************************         
* GENERAL: TEST BATCH MOA IS OPEN FOR POSTING                         *         
* NTRY: R1=A(PWOS MOA)                                                *         
* EXIT: FULL(2)= PWOS YYMM, FULL+2(2)=CHARACTER YM                    *         
***********************************************************************         
*                                                                               
         USING TBARECD,R2                                                       
TSTBMO   L     R2,AIO2                                                          
         XC    FULL,FULL                                                        
         MVC   FULL(L'TBAKBMOS),0(R1)                                           
         MVI   FULL+L'TBAKBMOS,1                                                
         MVC   DUB,SPACES                                                       
         GOTO1 DATCON,PARM,(1,FULL),(9,DUB)                                     
         L     R3,AGENWK                                                        
         USING BMONVALD,R3                                                      
         GOTO1 VBMONVAL,PARM,(8,DUB),(TBAKBTYP,ADCOMFAC),              X        
               (RCLANG,BMONVALD),(TBAKCPY,0)                                    
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BE    TSTB2                                                            
         CLI   BMOERR,BMOELOKQ     TEST LOCKED MONTH IS THE ONLY ERROR          
         BNE   ROUTH                                                            
         TM    STYPIND1,TYPIXLOK   TEST BATCH TYPE IGNORES MOS LOCK             
         BZ    ROUTH               NO                                           
TSTB2    MVC   FULL(L'BMOMOSP),BMOMOSP                                          
         MVC   FULL+L'BMOMOSP(L'BMOMOSC),BMOMOSC                                
         B     TSTBMOX                                                          
*                                                                               
TSTBMOX  B     ROUTE                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ORDINARY BATCHES: UPDATE OR DELETE A BATCH                          *         
* NTRY: AIO2=BATCH HEADER RECORD                                      *         
***********************************************************************         
*                                                                               
DELBAT   MVI   UBINDS,UBIDEL                                                    
         B     *+8                                                              
UPDBAT   MVI   UBINDS,UBIUPD                                                    
*                                                                               
         L     R0,ACHASTAB         CLEAR CHEQUE/ASKKEY TABLE                    
         L     R1,=A(CHASTABL)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,AIO2                                                          
         USING TBARECD,R2                                                       
         LA    R1,TBARFST          UPDATE CONTROL TOTALS                        
         USING BHDELD,R1                                                        
         SR    R0,R0                                                            
UPDBAT02 CLI   BHDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ                                                     
         BE    *+14                                                             
         IC    R0,BHDLN                                                         
         AR    R1,R0                                                            
         B     UPDBAT02                                                         
         TM    UBINDS,UBIDEL       TEST DELETE                                  
         BZ    UPDBAT04                                                         
         MVC   BHDDELDT,TODAYC     SET DATE DELETED                             
         TM    FBATIND,FBATIORD    TEST SAVED/OPEN BATCH WITH ORDER(S)          
         BZ    UPDBAT04                                                         
         OI    BHDSTAT1,BHDSDORD   SET STATUS FOR DJ                            
UPDBAT04 NI    BHDSTAT1,255-BHDSPEND  ENSURE PENDING STATUS NOT SET             
         TM    BHDSTAT1,BHDSACRU                                                
         BZ    *+12                                                             
         L     R1,ATRNBLK                                                       
         OI    TRNTIND1-TRNBLKD(R1),TRNTIACR   SET ACCRUAL BATCH                
*                                                                               
UPDBAT06 LA    R2,SBATKEY          USE SAVED BATCH KEY                          
         CLC   SBATITM,TBAKTSEQ    TEST ALL ITEMS ACCOUNTED FOR                 
         BE    UPDBAT48            END OF BATCH                                 
         ICM   R1,3,TBAKTSEQ                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,TBAKTSEQ                                                    
         MVC   IOKEY(L'TBAKEY),SBATKEY                                          
         LA    R1,IORDD+IOACCDIR+IO1                                            
         GOTO1 AIO                                                              
         BE    UPDBAT08                                                         
         TM    IOERR,IOEDEL        GET NEXT IF ITEM DELETED                     
         BNZ   UPDBAT06                                                         
         BAS   RE,IOTERR           NOT FOUND? PRINT ERROR TRACE                 
         B     UPDBAT06                                                         
UPDBAT08 TM    UBINDS,UBIDEL                                                    
         BNZ   *+12                                                             
         TM    IOKEY+(TBAKESTA-TBARECD),TBAESLDE                                
         BNZ   UPDBAT06                                                         
         LA    R1,IOGET+IOACCMST+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOBUFF                                                       
         TM    UBINDS,UBIDEL       TEST DELETING BATCH                          
         BZ    *+12                                                             
         TM    TBARESTA,TBAESLDE   TEST LOGICALLY DELETED ITEM                  
         BNZ   UPDBAT46            PHYSICALLY DELETE IT                         
*                                                                               
         XC    KEYTEMP,KEYTEMP                                                  
         MVI   UOCBIOSN,0          RESET BIOEL SEQUENCE NUMBER                  
         XC    UOCBIONM,UOCBIONM                                                
         LA    R2,TBARFST          R2=A(FIRST ELEMENT)                          
         USING ASKELD,R2                                                        
UPDBAT10 CLI   ASKEL,0             TEST EOR                                     
         BE    UPDBAT46                                                         
         NI    UBINDS,EFFS-(UBIGIN)                                             
         CLI   ASKEL,ASKELQ        TEST ACCOUNT SYSTEM KEY ELEMENT              
         BNE   UPDBAT12                                                         
*                                                                               
         OC    KEYTEMP,KEYTEMP     SAVE FIRST TX FOR ORDER DETAILS              
         BNZ   *+10                                                             
         MVC   KEYTEMP(L'ASKKEY),ASKKEY                                         
*                                                                               
         ICM   RF,3,REPPSTG        KEEP COUNT OF POSTINGS FOR REPORT            
         LA    RF,1(RF)                                                         
         STCM  RF,3,REPPSTG                                                     
         MVC   IOKEY,ASKKEY        SET TRANSACTION KEY                          
         TM    UBINDS,UBIDEL       TEST DELETE                                  
         BZ    UPDBAT22                                                         
         LA    R1,IORD+IOACCDIR+IO3                                             
         GOTO1 AIO                                                              
         BE    *+12                                                             
         BAS   RE,IOTERR           NOT FOUND - PRINT ERROR TRACE                
         B     UPDBAT40                                                         
         TM    IOKEY+(TRNKSTAT-TRNRECD),TRNSDRFT                                
         BZ    UPDBAT44            DON'T DELETE LIVE TRANSACTIONS               
         LA    R1,IOGET+IOACCMST+IO3                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     UPDBAT16                                                         
*                                                                               
UPDBAT12 CLI   ASKEL,GINELQ        TEST GROUP INVOICE ELEMENT                   
         BNE   UPDBAT28                                                         
         OI    UBINDS,UBIGIN                                                    
         USING GINELD,R2                                                        
         LA    R3,IOKEY            READ GIN PASSIVE FOR TRANSACTION             
         USING GINPASD,R3                                                       
         XC    GINPKEY,GINPKEY                                                  
         MVI   GINPTYP,GINPTYPQ                                                 
         MVC   GINPCPY,SBATCPY                                                  
         MVC   GINPINV,GININV                                                   
         TM    UBINDS,UBIDEL       TEST DELETE                                  
         BZ    UPDBAT22                                                         
         LA    R1,IOHI+IOACCDIR+IO3                                             
         B     *+8                                                              
UPDBAT14 LA    R1,IOSEQ+IOACCDIR+IO3                                            
         GOTO1 AIO                                                              
         BE    *+12                                                             
         BAS   RE,IOTERR           NOT FOUND - PRINT ERROR TRACE                
         B     UPDBAT40                                                         
         CLC   GINPINV,GININV      TEST CHANGE OF INVOICE GROUP                 
         BNE   UPDBAT40                                                         
         MVC   TEMP(L'GINPKEY),GINPKEY  SAVE KEY FOR READ SEQUENTIAL            
         LA    R1,IOGET+IOACCMST+IO3                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,AIOBUFF          LOOK FOR JOB POSTING                         
         USING TRNRECD,RF                                                       
         CLC   GINPISN,=Y(1)                                                    
         BNE   UPDBAT16                                                         
         CLI   GINPPTYP,0                                                       
         BNE   UPDBAT16                                                         
         OC    KEYTEMP,KEYTEMP                                                  
         BNZ   UPDBAT16                                                         
         MVC   KEYTEMP(L'TRNKEY),TRNKEY  SAVE KEY FOR ORDER PROCESSING          
*                                                                               
UPDBAT16 L     RF,AIOBUFF          RF=A(TRANSACTION RECORD)                     
         MVC   UBREFDS,TRNKDATE-TRNRECD(RF)                                     
         MVC   UBREFNO,SPACES                                                   
         MVC   UBREFNO(L'TRNKREF),TRNKREF-TRNRECD(RF)                           
         OI    TRNRSTA-TRNRECD(RF),TRNSDELT                                     
         LA    R1,IOPUTREC+IOACCMST+IO3                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIOBUFF                                                       
         MVC   IOKEY,0(RF)         READ DIRECTORY RECORD                        
         LA    R1,IORD+IOACCDIR+IO3                                             
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIOBUFF          SET STATUS AND WRITE BACK                    
         MVC   IOKEY+(TRNKSTA-TRNRECD)(L'TRNKSTA),TRNRSTA-TRNRECD(RF)           
         MVC   IOKEY+(TRNKDA-TRNRECD)(L'TRNKDA),IODA                            
         LA    R1,IOWRITE+IOACCDIR+IO3                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,ACHASTAB         UPDATE DRAFT TRANSACTION COUNT               
         USING ASKTABD,R1                                                       
         LH    R0,=Y(ASKTMAX)      MAXIMUM ASKTAB ENTRIES                       
UPDBAT18 CLI   ASKTULA,0           TEST EMPTY SLOT                              
         BNE   *+10                                                             
         MVC   ASKTULA,IOKEY+(ACTKUNT-ACTRECD)  SET ACCOUNT ENTRY               
         CLC   ASKTULA,IOKEY+(ACTKUNT-ACTRECD)  TEST MATCHING ENTRY             
         BE    UPDBAT20                                                         
         LA    R1,ASKTABL(R1)      BUMP TO NEXT ENTRY/EMPTY SLOT                
         BCT   R0,UPDBAT18                                                      
         DC    H'0'                TOO MANY ACCOUNTS TO PROCESS                 
*                                                                               
UPDBAT20 SR    RF,RF               INCREMENT DELETED TRANSACTION COUNT          
         ICM   RF,3,ASKTDEL                                                     
         LA    RF,1(RF)                                                         
         STCM  RF,3,ASKTDEL                                                     
         TM    UBINDS,UBIGIN       TEST PROCESSING GROUP INVOICE EL             
         BZ    UPDBAT40                                                         
         MVC   IOKEY,TEMP          RESTORE GIN PASSIVE KEY                      
         LA    R1,IORD+IOACCDIR+IO3    AND READ SEQUENCE                        
         GOTO1 AIO                                                              
         B     UPDBAT14                                                         
         DROP  R1                                                               
*                                                                               
UPDBAT22 TM    UBINDS,UBIUPD       TEST UPDATE                                  
         BZ    UPDBAT40                                                         
         TM    UBINDS,UBIGIN       TEST PROCESSING GROUP INVOICE EL             
         BNZ   UPDBAT23                                                         
         LA    R1,IORD+IOACCDIR+IO3                                             
         GOTO1 AIO                                                              
         BNE   UPDBAT24                                                         
         LA    R1,IOGET+IOACCMST+IO3                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     UPDBAT26                                                         
*                                                                               
UPDBAT23 LA    R1,IOHI+IOACCDIR+IO3                                             
         B     *+8                                                              
UPDBAT24 LA    R1,IOSEQ+IOACCDIR+IO3                                            
         GOTO1 AIO                                                              
         BE    *+12                                                             
         BAS   RE,IOTERR           NOT FOUND - PRINT ERROR TRACE                
         B     UPDBAT40                                                         
         CLC   GINPINV,GININV      TEST END OF GROUP                            
         BNE   UPDBAT40                                                         
         LA    R1,IOGET+IOACCMST+IO3                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDBAT26 L     RF,AIOBUFF          RF=A(TRANSACTION RECORD)                     
         USING TRNRECD,RF                                                       
         LA    R1,TRNRFST                                                       
         USING TRNELD,R1                                                        
         CLI   TRNEL,TRNELQ                                                     
         BNE   UPDBAT27                                                         
         ZAP   DUB,TRNAMNT                                                      
         LA    RE,TOFUIDDR         ACCUMULATE ADJ TOTALS FOR REPORT             
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+8                                                              
         LA    RE,TOFUIDCR                                                      
         AP    0(8,RE),DUB                                                      
         DROP  R1                                                               
UPDBAT27 GOTO1 ASORTPT,DMCB,(3,TRNKEY)  PUT FOR ADJ WORKER FILE                 
*                                                                               
         TM    UBINDS,UBIGIN       TEST PROCESSING GROUP INVOICE EL             
         BNZ   UPDBAT24                                                         
         B     UPDBAT40                                                         
*                                                                               
         USING BICELD,R2           R2=A(BATCH ITEM CHEQUE ELEMENT)              
UPDBAT28 CLI   BICEL,BICELQ        TEST BATCH ITEM CHEQUE ELEMENT               
         BNE   UPDBAT40                                                         
         TM    UBINDS,UBIUPD       TEST UPDATE                                  
         BZ    UPDBAT40                                                         
         LA    R0,CHQTMAX                                                       
         L     R1,ACHASTAB         ADD TO/UPDATE CHEQUE TABLE                   
         USING CHQTABD,R1          R1=A(CHEQUE TABLE)                           
UPDBAT30 OC    CHQITEM,CHQITEM     TEST EMPTY SLOT                              
         BZ    UPDBAT34                                                         
         CLC   CHQDATA(CHQKEYL),BICACT TEST KEY MATCH                           
         BNE   UPDBAT32                                                         
         CLC   SCPYBANK,BICACTU                   TEST BANK                     
         BNE   UPDBAT32                                                         
         AP    BICAMT-BICELD(L'BICAMT,R1),BICAMT  ADD AMOUNT                    
         B     UPDBAT40                                                         
UPDBAT32 LA    R1,CHQTABL(R1)                                                   
         BCT   R0,UPDBAT30                                                      
         DC    H'0'                                                             
UPDBAT34 L     RE,AIO1                                                          
         MVC   CHQITEM,TBAKTSEQ-TBARECD(RE)                                     
         MVC   CHQDATA,SPACES      SPACE-FILL DATA                              
         IC    RF,BICLN            BICEL IS VARIABLE LENGTH                     
         SH    RF,=Y((BICACT-BICELD)+1)                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CHQDATA(0),BICACT   EXTRACT ELEMENT BEYOND BICACT                
*                                                                               
         USING BIOELD,R2                                                        
UPDBAT40 CLI   BIOEL,BIOELQ        TEST BATCH ITEM ORDER ELEMENT                
         BNE   UPDBAT44                                                         
         OC    UOCBIONM,UOCBIONM   HAVE WE SAVED ITEM SEQUENCE NUMBER           
         BZ    UPDBAT41            NO                                           
         CLC   BIOITSEQ,UOCBIONM   YES - IS IT THE SAME                         
         BE    *+8                 YES                                          
         MVI   UOCBIOSN,0          NO - RESET SEQUENCE COUNT                    
UPDBAT41 MVC   UOCBIONM,BIOITSEQ   SET ITEM SEQUENCE NUMBER                     
         SR    RF,RF                                                            
         XR    RF,RF                                                            
         IC    RF,UOCBIOSN         CURRENT BIOEL SEQUENCE NUMBER                
         AHI   RF,1                                                             
         STC   RF,UOCBIOSN                                                      
         LA    R1,BIOELD                                                        
         GOTO1 AUPDORD                                                          
         JNL   UPDBAT44                                                         
         LLC   R1,BYTE                                                          
         BAS   RE,IOTERR                                                        
         J     ROUTE                                                            
*                                                                               
UPDBAT44 SR    R0,R0                                                            
         IC    R0,BIOLN            BUMP TO NEXT ITEM RECORD ELEMENT             
         AR    R2,R0                                                            
         B     UPDBAT10                                                         
*                                                                               
UPDBAT46 TM    UBINDS,UBIDEL       TEST DELETE                                  
         BZ    UPDBAT06                                                         
         GOTO1 AORDAUD                                                          
         L     RF,AIO1                                                          
         OI    TBARESTA-TBARECD(RF),TBAESDEL                                    
         LA    R1,IOPUTREC+IOACCMST+IO1                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIOBUFF                                                       
         MVC   IOKEY,0(RF)         READ DIRECTORY RECORD                        
         LA    R1,IORD+IOACCDIR+IO1                                             
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIOBUFF          SET STATUS AND WRITE BACK                    
         MVC   IOKEY+(TBAKSTA-TBARECD)(L'TBAKSTA),TBARSTA-TBARECD(RF)           
         MVC   IOKEY+(TBAKDA-TBARECD)(L'TBAKDA),IODA                            
         LA    R1,IOWRITE+IOACCDIR+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,NBITCHA       MAINTAIN ITEMS CHANGED COUNT                 
         LA    RF,1(RF)                                                         
         STCM  RF,15,NBITCHA                                                    
         B     UPDBAT06                                                         
*                                                                               
UPDBAT48 TM    UBINDS,UBIUPD       TEST UPDATE                                  
         BZ    *+8                                                              
         BAS   RE,UPDCHQ           UPDATE CHEQUE TABLE                          
*                                                                               
UPDBAT50 L     R2,AIO2             UPDATE BATCH RECORD                          
         USING TBARECD,R2                                                       
         MVC   TBAHRUDT,TODAYC     SET DATE UPDATED                             
         TM    UBINDS,UBIDEL                                                    
         BZ    UPDBAT52                                                         
         OI    TBARHSTA,TBAHSDEL   SET DELETED                                  
         NI    TBAHRIND,255-TBAHIWRN  ENSURE WARNING BIT NOT SET TOO            
         OI    TBAHRIND,TBAHIDEL   BY DAILY JOURNAL                             
         B     UPDBAT56                                                         
*                                                                               
UPDBAT52 TM    UBINDS,UBIUPD                                                    
         BZ    UPDBAT56                                                         
         CLC   TBAHREDT,TODAYC     TEST EFFECTIVE DATE IS TODAY                 
         BE    *+14                                                             
         OI    UBINDS,UBIPPD       NO - SET PASSIVE WAS DELETED                 
         MVC   TBAHREDT,TODAYC     MAKE IT EFFECTIVE TODAY                      
         MVI   TBARHSTA,TBAHSUPD   SET UPDATED                                  
         NI    TBAHRIND,TBAHIMLT+TBAHIGDJ                                       
         OI    TBAHRIND,TBAHIUOF   BY DAILY JOURNAL                             
*                                                                               
         SR    R0,R0               UPDATE ITEM COUNT IN BATCH HEADER            
         LA    RF,TBARFST                                                       
         USING BHDELD,RF                                                        
UPDBAT54 CLI   BHDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ                                                     
         BE    *+14                                                             
         IC    R0,BHDLN                                                         
         AR    RF,R0                                                            
         B     UPDBAT54                                                         
         MVC   BHDITEMA,HALF2      USE ITEM COUNT FROM UPDCHQ                   
         TM    STYPIND1,TYPIADVP   IF ADVANCE PAYMENT TYPE                      
         BZ    *+10                                                             
         MVC   BHDITEMC,HALF2      SET CONTROL ITEM COUNT                       
*                                                                               
UPDBAT56 LA    R2,IOKEY            R2=A(BATCH KEY)                              
         L     R1,AIO2                                                          
         MVC   TBAKEY,0(R1)        TEST READ DIRECTORY RECORD                   
         LA    R1,IORD+IOACCDIR+IO1                                             
         GOTO1 AIO                                                              
         BE    *+8                                                              
         OI    FBATIND,FBATINOD                                                 
         LA    R1,IOPUTREC+IOACCMST+IO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY            R2=A(BATCH KEY)                              
         L     R1,AIO2                                                          
         MVC   TBAKEY,0(R1)             SET DIRECTORY RECORD                    
         TM    FBATIND,FBATINOD         TEST FOUND                              
         BNZ   UPDBAT58                 NO                                      
         LA    R1,IORD+IOACCDIR+IO1     YES - READ IT AGAIN                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
UPDBAT58 L     R1,AIO2                                                          
         MVC   TBAKSTA,TBARSTA-TBARECD(R1)                                      
         MVC   TBAKDA,IODA                                                      
         LA    R1,IOADD+IOACCDIR+IO1    SET ADD DIRECTORY RECORD                
         TM    FBATIND,FBATINOD         TEST FOUND                              
         BNZ   *+8                      NO                                      
         LA    R1,IOWRITE+IOACCDIR+IO1  YES - WRITE IT BACK                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  HANDLE PASSIVE POINTER                       
         MVI   TBAPTYP,TBAPTYPQ                                                 
         MVC   TBAPEFDT,TBAHKEDT                                                
         MVC   TBAPTSEQ,TBAHKADT                                                
*        MVC   IOKEYSAV,TBAKEY                                                  
         MVC   KEYSAFE,TBAKEY      MAKE SURE THIS KEY IS SAFE                   
         TM    UBINDS,UBIPPD       TEST EFFECTIVE DATE CHANGED                  
         BNZ   UPDBAT60                                                         
         LA    R1,IORD+IOACCDIR+IO1     ELSE READ IT                            
         GOTO1 AIO                                                              
         BE    UPDBAT62                                                         
         MVC   TBAKEY,KEYSAFE      RESET KEY                                    
         XC    TBAPTSEQ,TBAPTSEQ   CLEAR ADDED DATE                             
         LA    R1,IORD+IOACCDIR+IO1     AND TRY TO READ IT                      
         GOTO1 AIO                                                              
         BE    UPDBAT62                                                         
         DC    H'0'                                                             
*                                                                               
UPDBAT60 LA    R1,IORDD+IOACCDIR+IO1   TEST PASSIVE EXISTS, BUT DELETED         
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'0'                    HARDWARE ERROR                           
         TM    IOERR,IOERNF                                                     
         BNZ   UPDBAT64                NO - ADD A NEW ONE                       
UPDBAT62 L     R1,AIO2                                                          
         MVC   TBAKSTA,TBARSTA-TBARECD(R1)       SET STATUS                     
         MVC   TBAKDA,KEYSAFE+(TBAKDA-TBARECD)   DISK ADDRESS                   
         LA    R1,IOWRITE+IOACCDIR+IO1           WRITE IT BACK                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     UPDBAT66                                                         
*                                  ADD A NEW PASSIVE                            
UPDBAT64 MVC   TBAPAS,KEYSAFE                    SET KEY                        
         L     R1,AIO2                                                          
         MVC   TBAKSTA,TBARSTA-TBARECD(R1)       STATUS                         
         MVC   TBAKDA,KEYSAFE+(TBAKDA-TBARECD)   DISK ADDRESS                   
         LA    R1,IOADD+IOACCDIR+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
UPDBAT66 ICM   RF,15,NBHDCHA       MAINTAIN BATCH HDRS CHANGED COUNT            
         LA    RF,1(RF)                                                         
         STCM  RF,15,NBHDCHA                                                    
*                                                                               
         TM    UBINDS,UBIDEL       TEST DELETE                                  
         BZ    UPDBATX                                                          
         LH    R0,=Y(ASKTMAX)      MAXIMUM ASKTAB ENTRIES                       
         L     R2,ACHASTAB                                                      
         USING ASKTABD,R2                                                       
         LA    R3,IOKEY                                                         
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES       UPDATE DRAFT TRANS COUNT ON ACCOUNTS         
         MVC   ACTKCPY,SBATCPY     FROM WHICH WE DELETED TRANSACTIONS           
*                                                                               
UPDBAT68 CLI   ASKTULA,0           TEST EOT                                     
         BE    UPDBATX                                                          
         MVC   ACTKUNT(L'ASKTULA),ASKTULA                                       
         LA    R1,IOREAD+IOACCDIR+IO3                                           
         GOTO1 AIO                                                              
         BNE   UPDBAT72            IGNORE ERRORS                                
         LA    R1,IOGET+IOACCMST+IO3                                            
         GOTO1 AIO                                                              
         BNE   UPDBAT72                                                         
         L     R1,AIOBUFF                                                       
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING ASTELD,R1                                                        
         SR    RF,RF                                                            
UPDBAT70 CLI   ASTEL,0                                                          
         BE    UPDBAT72                                                         
         CLI   ASTEL,ASTELQ                                                     
         BE    *+14                                                             
         IC    RF,ASTLN                                                         
         AR    R1,RF                                                            
         B     UPDBAT70                                                         
         ICM   RF,7,ASTDRAFT       RF=ACCOUNT DRAFT TRANSACTION COUNT           
         SR    RE,RE                                                            
         ICM   RE,3,ASKTDEL        RE=DELETED DRAFT TRANSACTION COUNT           
         SR    RF,RE                                                            
         BNM   *+10                                                             
         BAS   RE,IOTER2                                                        
         SR    RF,RF                                                            
         STCM  RF,7,ASTDRAFT       UPDATE DRAFT TRANSACTION COUNT               
         LA    R1,IOPUTREC+IOACCMST+IO3                                         
         GOTO1 AIO                                                              
         BNE   UPDBAT72                                                         
         LA    R2,ASKTABL(R2)      BUMP TO NEXT ASKTAB ENTRY                    
UPDBAT72 BCT   R0,UPDBAT68                                                      
         B     UPDBATX                                                          
*                                                                               
UPDBATX  B     ROUTE                                                            
         DROP  R1,R2,R3,RF                                                      
         EJECT                                                                  
***********************************************************************         
* BUILD A TRANSACTION FOR EACH CHQTAB ENTRY AND CALL ADDTRN TO UPDATE *         
***********************************************************************         
*                                                                               
UPDCHQ   NTR1  ,                                                                
         LA    R0,CHQTMAX          R0=MAXIMUM CHEQUE TABLE ENTRIES              
         L     R2,ACHASTAB                                                      
         USING CHQTABD,R2          R2=A(CHEQUE TABLE)                           
         MVC   HALF2,SBATITM       TAKE PRESENT HIGH ITEM NUMBER                
*                                                                               
UPDCHQ02 OC    CHQITEM,CHQITEM     TEST NO MORE ENTRIES                         
         BZ    UPDCHQX                                                          
         MVC   IOKEY,SBATKEY                                                    
         LA    R1,IOKEY                                                         
         MVC   TBAKTSEQ-TBARECD(L'TBAKTSEQ,R1),CHQITEM                          
         LA    R1,IOREAD+IOACCDIR+IO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGET+IOACCMST+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOBUFF                                                       
         LA    R1,TBARFST-TBARECD(R1)                                           
         SR    RE,RE                                                            
         USING ASKELD,R1                                                        
UPDCHQ04 IC    RE,ASKLN                                                         
         AR    R1,RE                                                            
         CLI   ASKEL,0                                                          
         BE    UPDCHQ06                                                         
         CLI   ASKEL,ASKELQ                                                     
         BNE   UPDCHQ04                                                         
         MVC   IOKEY,ASKKEY        READ POSTING FOR NARRATIVE                   
         LA    R1,IOREAD+IOACCDIR+IO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGET+IOACCMST+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOBUFF                                                       
         LA    R1,TRNRFST-TRNRECD(R1)                                           
         USING TRNELD,R1                                                        
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         IC    RE,TRNLN                                                         
         SH    RE,=Y(TRNLN1Q+1)                                                 
         XC    SCHQNAR,SCHQNAR                                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SCHQNAR(0),TRNNARR                                               
         MVC   SCHQDAT,SPACES                                                   
         MVC   SCHQDEP,SPACES                                                   
*                                                                               
         USING BICELD,R2           CHEQUE ENTRIES MAP TO BICACT                 
UPDCHQ06 GOTO1 DATCON,PARM,(1,BICDAT),(8,SCHQDAT)                               
         GOTO1 (RF),(R1),(1,BICDEP),(8,SCHQDEP)                                 
         L     R1,ATRNBLK                                                       
         USING TRNBLKD,R1                                                       
         MVC   TRNCACNM,BICCACN    C/A NAME (SPACE-FILLED)                      
         DROP  R1                                                               
         L     RF,AIO3             =TRNREC IN ADDTRN BLOCK                      
         USING TRNRECD,RF          RF=A(TRANSACTION RECORD)                     
         MVC   TRNKCULA,BICACT     KEY ACCOUNT                                  
         MVC   TRNKOFF,SPACES      SPACES IN OFFICE/WORKCODE                    
         MVC   TRNKCULC,BICCAC     KEY CONTRA-ACCOUNT                           
         MVC   TRNKDATE,BICDAT     KEY DATE (CHEQUE DATE)                       
         MVC   TRNKREF,BICCNO      KEY REFERENCE                                
         MVI   TRNKSBR,0           KEY SUB-REFERENCE                            
         XC    TRNRLEN,TRNRLEN                                                  
         XC    TRNRSTA,TRNRSTA     CLEAR STATUS AREA                            
*                                                                               
         LA    R1,TRNRFST          R2=A(FIRST ELEMENT)                          
         USING TRNELD,R1                                                        
         XC    TRNELD(256),TRNELD  CLEAR SOME SPACE                             
         MVI   TRNEL,TRNELQ        BUILD TRANSACTION                            
         MVC   TRNDATE,TRNKDATE    DATE                                         
         MVC   TRNREF,TRNKREF      REFERENCE                                    
         MVC   TRNSUB,TRNKSBR      SUB-REFERENCE                                
         MVC   TRNTYPE,SBATKEY+(TBAKBTYP-TBARECD) BATCH TYPE                    
         MVC   TRNSTAT,BICSTA      USE STATUS FROM BICEL                        
         MVC   TRNMOS,SBATKEY+(TBAKBMOS-TBARECD)                                
         OI    TRNMOS,X'F0'        CONVERT MOS TO DISPLAY                       
         SR    RE,RE                                                            
         IC    RE,TRNMOS+1                                                      
         LA    RE,MOSTAB-1(RE)                                                  
         MVC   TRNMOS+1(1),0(RE)                                                
         MVC   TRNBREF,SBATKEY+(TBAKBREF-TBARECD)                               
         ZAP   TRNAMNT,BICAMT      AMOUNT                                       
         MVC   TRNOFFC,BICOFF      OFFICE/WORKCODE                              
         MVC   TRNNARR,SCHQNAR                                                  
         LA    RE,TRNEL+TRNLN1Q+L'TRNNARR                                       
         CLI   0(RE),0                                                          
         BH    *+8                 RE=A(LAST CHARACTER OF TRNEL)                
         BCT   RE,*-8                                                           
         LA    RE,1(RE)                                                         
         SR    RE,R1               RE=L'TRNEL                                   
         STC   RE,TRNLN            TRANSACTION LENGTH                           
*                                                                               
         L     R1,ATRNBLK                                                       
         USING TRNBLKD,R1                                                       
         OI    TRNINDS,TRNICONV+TRNIDRFT    ADD TRANSACTION AS DRAFT            
         TM    SBATST1,BHDSACRU                                                 
         BZ    *+8                                                              
         OI    TRNTIND1,TRNTIACR   SET ACCRUAL                                  
         OI    TRNINDS2,TRNIADDG                                                
         DROP  R1                                                               
         OC    TRNKULA,SPACES                                                   
         OC    TRNKULC,SPACES                                                   
         DROP  RF                                                               
         GOTO1 VADDTRN                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,SBATKEY                                                    
         LA    RF,IOKEY                                                         
         USING TBAKEY,RF                                                        
         LH    R1,HALF2            TAKE CURRENT HIGH ITEM NUMBER                
         LA    R1,1(R1)                                                         
         STH   R1,HALF2                                                         
         MVC   TBAKTSEQ,HALF2      SET ITEM NUMBER IN KEY                       
         L     RF,AIO1             RF=A(BATCH ITEM RECORD)                      
         MVC   TBAKEY,IOKEY        SET BATCH ITEM KEY                           
         XC    TBARSTA(TBARFST-TBARSTA),TBARSTA                                 
         MVI   TBARESTA,TBAESAUT   SET AUTO-GENERATED BATCH ITEM                
*                                                                               
         L     R1,AIO3             R1=A(TRANSACTION RECORD)                     
         LA    R1,TRNRFST-TRNRECD(R1)                                           
         USING TRNELD,R1           R1=A(TRANSACTION ELEMENT WE ADDED)           
         LA    R3,TBARFST                                                       
         USING BIAELD,R3           BUILD TOTAL AMOUNT ELEMENT                   
         XC    BIAELD(BIALNQ),BIAELD                                            
         MVI   BIAEL,BTAELQ        NOTE - SPECIAL ELEMENT CODE                  
         MVI   BIALN,BIALNQ                                                     
         ZAP   BIAAMT,TRNAMNT                                                   
         MVC   BIAREF,TRNREF                                                    
         SR    R0,R0                                                            
         IC    R0,BIALN                                                         
         AR    R3,R0               BUMP TO NEXT ELEMENT                         
*                                                                               
         USING ASKELD,R3                                                        
         MVI   ASKEL,ASKELQ        BUILD SYSTEM KEY ELEMENT                     
         MVI   ASKLN,ASKLNQ                                                     
         MVI   ASKSEQN,1                                                        
         L     R1,AIO3                                                          
         MVC   ASKKEY,0(R1)        SET KEY FROM TRANSACTION RECORD              
         MVI   ASKLNQ(R3),0        SET EOR                                      
         LA    R0,ASKLNQ+1(R3)                                                  
         SR    R0,RF                                                            
         STCM  R0,3,TBARLEN        SET ITEM RECORD LENGTH                       
         GOTO1 ASORTPT,DMCB,(3,ASKKEY)  PUT FOR ADJ WORKER FILE                 
*                                                                               
         LA    R1,IORD+IOACCDIR+IO1                                             
         GOTO1 AIO                                                              
         BNE   *+6                                                              
         DC    H'0'                RECORD MUST NOT EXIST UNDELETED              
         TM    IOERR,IOERNF                                                     
         BZ    UPDCHQ10                                                         
         LA    R1,IOADDREC+IOACCMST+IO1                                         
         GOTO1 AIO                                                              
         B     UPDCHQ12                                                         
*                                                                               
UPDCHQ10 TM    IOERR,IOEDEL        RECORD EXISTS - CHECK DELETED                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOPUTREC+IOACCMST+IO1                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOBUFF          BUILD DIRECTORY KEY                          
         LA    RF,IOKEY                                                         
         MVC   TBAKSTA,TBARSTA-TBARECD(R1)                                      
         MVC   TBAKDA,IODA                                                      
         DROP  RF                                                               
         LA    R1,IOWRITE+IOACCDIR+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDCHQ12 LA    R2,CHQTABL(R2)      BUMP TO NEXT CHEQUE TABLE ENTRY              
         BCT   R0,UPDCHQ02                                                      
*                                                                               
UPDCHQX  B     ROUTE                                                            
         DROP  R1,R2,R3                                                         
         EJECT                                                                  
***********************************************************************         
* RE-WRITE BATCH HEADER RECORD, PASSIVE POINTER AND KEY TO ACC FILE   *         
* NTRY:IO2 CONTAINS RECORD                                            *         
***********************************************************************         
*                                                                               
         USING TBARECD,R2                                                       
UPDHDR   L     R2,AIO2                                                          
         SR    R0,R0                                                            
         LA    R1,TBARFST                                                       
         USING BHDELD,R1                                                        
UPDH2    CLI   BHDEL,0             LOCATE HEADER ELEMENT                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ                                                     
         BE    UPDH4                                                            
         IC    R0,BHDLN                                                         
         AR    R1,R0                                                            
         B     UPDH2                                                            
UPDH4    MVC   BHDWDAYS,FBATDAYS   SET/CLEAR DAYS UNTIL LIVE OR DELETED         
         DROP  R1                                                               
*                                                                               
         LA    R1,IOPUTREC+IOACCMST+IO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY            R2=A(BATCH KEY)                              
         L     R1,AIO2                                                          
         MVC   TBAKEY,TBAKEY-TBARECD(R1)                                        
         MVC   TBAKSTA,TBARSTA-TBARECD(R1)                                      
         MVC   TBAKDA,IODA         WRITE DIRECTORY RECORD                       
         GOTO1 AIO,IOWRITE+IOACCDIR+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TBAPTYP,TBAPTYPQ    READ DIRECTORY PASSIVE POINTER               
         MVC   TBAPEFDT,TBAHKEDT                                                
         MVC   TBAPTSEQ,TBAHKADT                                                
         MVC   IOKEYSAV,TBAKEY                                                  
         GOTO1 AIO,IORD+IOACCDIR+IO2                                            
         BE    UPDH6                                                            
         MVC   TBAKEY,IOKEYSAV                                                  
         XC    TBAPTSEQ,TBAPTSEQ   CLEAR SEQUENCE NUMBER                        
         GOTO1 AIO,IORD+IOACCDIR+IO2                                            
         BE    UPDH6                                                            
         DC    H'0'                                                             
UPDH6    L     R1,AIO2             SET STATUS AND WRITE BACK                    
         MVC   TBAKSTA,TBARSTA-TBARECD(R1)                                      
         MVC   TBAKDA,IODA                                                      
         GOTO1 AIO,IOWRITE+IOACCDIR+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,NBHDCHA       MAINTAIN BATCH HDRS CHANGED COUNT            
         LA    RF,1(RF)                                                         
         STCM  RF,15,NBHDCHA                                                    
         B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* WORKER FILE BATCHES: BUILD BATCH KEY IN SBLDKEY                     *         
***********************************************************************         
*                                                                               
         USING WKRTABD,R3          R3=A(WKR TABLE ENTRY)                        
         USING PSHEADD,R4          R4=A(1ST WORKER POSTING)                     
WBKBLD   LA    R2,SBLDKEY                                                       
         USING TBARECD,R2                                                       
         SR    RF,RF                                                            
         IC    RF,PSHDLEN                                                       
         AR    RF,R4                                                            
         USING TRNELD,RF                                                        
         XC    TBAKEY,TBAKEY                                                    
         MVI   TBAKTYP,TBAKTYPQ                                                 
         MVC   TBAKCPY,PSHDACC                                                  
         MVC   TBAKUSER,WKIUSER                                                 
         MVC   TBAKADDT,TODAYCC                                                 
         MVI   TBAKGRUP,TBAGGENQ                                                
         MVC   TBAKBTYP,WKRTBTYP                                                
         MVC   TBAKBREF,TRNBREF                                                 
         XC    TBAKBCHR(TBAKLAST-TBAKBCHR),TBAKBCHR                             
         GOTO1 VCONVMOS,DMCB,(0,TRNELD),TBAKBMOS                                
*                                                                               
         GOTO1 AGETTYP             LOCATE AND SAVE TYPTAB ENTRY                 
*&&US                                                                           
         TM    STYPIND1,TYPIPRDQ   TEST BATCH GROUP=PROD                        
         BZ    *+8                                                              
         MVI   TBAKGRUP,TBAGPRDQ   SET IT                                       
*&&                                                                             
         MVC   IOKEYSAV,SBLDKEY                                                 
         B     ROUTE                                                            
         DROP  R2,R3,R4,RF                                                      
         EJECT                                                                  
***********************************************************************         
* SET ORDER TO GAP EXPIRED AND BUILD AUDIT IF GAP ORDER EXPIRED       *         
***********************************************************************         
*                                                                               
GAPEXP   L     RF,=A(GAPEXD)       MOVED TO END FOR ADDRESSABILITY              
         BASR  RE,RF                                                            
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* READ SUPPLIER TO GET GAP ACKNOWLEDGE/QUERY ACTIONS                  *         
***********************************************************************         
*                                                                               
SUPGAP   L     RF,=A(SUPGAPQ)      MOVED TO END FOR ADDRESSABILITY              
         BASR  RE,RF                                                            
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* AUDIT ORDER                                                         *         
***********************************************************************         
*                                                                               
ORDAUD   L     RF,=A(ORDAUDQ)      MOVED TO END FOR ADDRESSABILITY              
         BASR  RE,RF                                                            
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK FOR EXPIRED GAP ORDERS                                        *         
***********************************************************************         
*                                                                               
         USING OGAPASD,R2                                                       
ORD      USING ORDRECD,R2                                                       
         USING AUDRECD,R3                                                       
GAPEXD   NTR1  BASE=*                                                           
         LA    R2,IOKEY                                                         
         XC    OGAPAS,OGAPAS                                                    
         MVI   OGAPTYP,OGAPTYPQ                                                 
         MVI   OGAPSUB,OGAPSUBQ                                                 
         MVI   OGAPCPY,X'41'                                                    
         MVC   SVFRCPY,OGAPCPY                                                  
         MVI   OGAPGAP,OGAPACC                                                  
*                                                                               
GAPEXD02 LA    R1,IOHI+IOACCDIR+IO3                                             
         GOTO1 AIO                                                              
         BE    GAPEXD06                                                         
         DC    H'0'                                                             
*                                                                               
GAPEXD04 LA    R1,IOSEQ+IOACCDIR+IO3                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GAPEXD06 LA    R2,IOKEY                                                         
         CLI   OGAPTYP,OGAPTYPQ    STILL READING OGAPASD?                       
         BNE   GAPEXD42                                                         
         CLI   OGAPSUB,OGAPSUBQ                                                 
         BNE   GAPEXD42                                                         
         CLC   OGAPCPY,SVFRCPY     SAME COMPANY?                                
         BNE   GAPEXD42                                                         
         CLI   OGAPGAP,OGAPACC     ACCESSED?                                    
         BE    *+16                                                             
         CLI   OGAPGAP,OGAPSNT     SENT?                                        
         BL    GAPEXD04                                                         
         BH    GAPEXD42                                                         
*                                                                               
         OC    OGAPEXD,OGAPEXD     Any expiry date?                             
         BZ    GAPEXD04                                                         
         CLC   TODAYPCO(L'OGAPEXD),OGAPEXD CHECK WHETHER IT HAS EXPIRED         
         BH    GAPEXD04                                                         
         TM    OGAPSTA,ORDSFMCH+ORDSLDEL+ORDCLOSE+ORDGDRCV                      
         BNZ   GAPEXD04            IGNORE THESE STATUSES                        
         TM    OGAPSTA2,ORDSAPPR   MUST BE APPROVED                             
         BZ    GAPEXD04                                                         
         MVC   KEYSAFE,IOKEY                                                    
         MVC   SVSUPC,OGAPSUP                                                   
         GOTO1 ASUPGAP                                                          
         CLI   SVACKQ,YES          ACKNOWLEDGE/QUERY IN USE?                    
         BE    GAPEXD08                                                         
         CLI   OGAPGAP,ORDGACCS    IGNORE ACCESSED?                             
         BE    GAPEXD40                                                         
*                                                                               
GAPEXD08 MVC   IOKEY,KEYSAFE       RE-READ AFTER SUPGAP USED IO3                
         LA    R1,IORD+IOACCDIR+IO3                                             
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGETREC+IOACCMST+IO3                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   RCWRITE,NO                                                       
         BE    GAPEXD10                                                         
         USING CPTRBLK,FOOT1       TEMPORARY USE                                
         XC    CPTRBLK,CPTRBLK     AREA TO KEEP PADDLE HAPPY                    
         GOTO1 VPADDLE,DMCB,(C'D',AIO3),CPTRBLK,0,0,ADCOMFAC                    
*                                                                               
GAPEXD10 L     R2,AIO3                                                          
         GOTO1 VHELLO,DMCB,(C'G',=C'ACCMST'),('ORDELQ',ORD.ORDRECD),0           
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING ORDELD,R1                                                        
         L     R1,12(R1)                                                        
         MVC   SVPGAPST,ORDGSTAT   SAVE PREVIOUS GAP STATUS                     
         MVI   ORDGSTAT,ORDGREXP   IF SO MARK AS EXPIRED                        
         CLI   RCWRITE,NO                                                       
         BE    GAPEXD12                                                         
         LA    R1,IOPUTREC+IOACCMST+IO3                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    CPTRBLK,CPTRBLK     AREA TO KEEP PADDLE HAPPY                    
         GOTO1 VPADDLE,DMCB,(C'A',AIO3),(C'A',CPTRBLK),IODA,0,ADCOMFAC          
*                                                                               
GAPEXD12 DS    0H                                                               
*&&UK                                                                           
         USING RNSPASD,R4          READ PASSIVES TO FIND TRANSACTIONS           
         LA    R4,IOKEY                                                         
         XC    IOKEY,IOKEY         WHICH NEED UPDATING                          
         MVI   RNSPTYP,RNSPTYPQ                                                 
         MVI   RNSPSUB,RNSPSUBQ                                                 
         MVC   RNSPCPY,SVFRCPY                                                  
         MVI   RNSPIND,RNSPORQ                                                  
         MVC   RNSPREF,ORD.ORDKORD                                              
         MVC   IOSAVKEY,IOKEY                                                   
         LA    R1,IOHI+IOACCDIR+IO2                                             
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GAPEXD14 LA    R1,IOSEQ+IOACCDIR+IO2                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOSAVKEY(RNSPBTY-RNSPASD),IOKEY                                  
         BNE   GAPEXD22            REACHED END OF PASSIVES?                     
         MVC   IOSAVKEY,IOKEY                                                   
         LA    R1,IOGETREC+IOACCMST+IO2                                         
         TM    RNSPSTA,TRNSARCH                                                 
         BZ    *+8                                                              
         LA    R1,IOGET+IOACCARC+IO2                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RNSPSTA,TRNSARCH    IF ARCHIVED PROMOTE TO ACCMST                
         BZ    GAPEXD16                                                         
         GOTOR VPROMOTE,DMCB,AIO2,ADCOMFAC                                      
         MVC   IOKEY,IOSAVKEY                                                   
         LA    R1,IORDD+IOACCDIR+IO2                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                IF WE DIE HERE TRANSACTION DOESN'T           
         LA    R1,IOGETREC+IOACCMST+IO2                                         
         GOTO1 AIO                                                              
         BE    *+6                 EXIST!                                       
         DC    H'0'                                                             
*                                                                               
         USING TRNRECD,R4                                                       
         USING TRSELD,R1                                                        
GAPEXD16 L     R4,AIO2                                                          
         XC    TEMP,TEMP           LOOK FOR TRSELD                              
         GOTO1 VHELLO,DMCB,(C'G',=C'ACCMST'),('TRSELQ',TRNRECD),0               
         CLI   12(R1),0                                                         
         BNE   GAPEXD18                                                         
*                                                                               
         L     R1,12(R1)           EXTRACT TO TEMP                              
         LLC   RF,TRSLN                                                         
         SHI   RF,1                                                             
         MVC   TEMP(0),TRSELD                                                   
         EX    RF,*-6                                                           
         MVI   TRSEL,X'FF'                                                      
         GOTO1 VHELLO,DMCB,(C'D',=C'ACCMST'),(X'FF',TRNRECD),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,TEMP             INCREASE LENGTH                              
         CLI   TRSLN,TRSLNQ                                                     
         BH    *+8                                                              
         MVI   TRSLN,TRSLN2Q                                                    
         MVI   TRSGSTAT,ORDGREXP                                                
*        LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'ACCMST'),TRNRECD,TEMP,0                     
         CLI   12(R1),0                                                         
         BE    GAPEXD20                                                         
         DC    H'0'                                                             
*                                                                               
GAPEXD18 LA    R1,TEMP             ADD A TRSELD                                 
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLN2Q                                                    
         MVI   TRSGSTAT,ORDGREXP                                                
*        LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'ACCMST'),TRNRECD,TEMP,0                     
         CLI   12(R1),0                                                         
         BE    GAPEXD20                                                         
         DC    H'0'                                                             
*                                                                               
GAPEXD20 CLI   RCWRITE,NO                                                       
         BE    GAPEXD14                                                         
         LA    R1,IOPUTREC+IOACCMST+IO2                                         
         GOTO1 AIO                                                              
         BE    GAPEXD14            UDPATE IT                                    
         DC    H'0'                                                             
*&&                                                                             
GAPEXD22 L     R2,AIO3                                                          
         LA    R3,IOKEY            NOW BUILD AUDIT ENTRY                        
         XC    IOKEY,IOKEY                                                      
         XC    AUDKEY,AUDKEY                                                    
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVI   AUDKAUDT,AUDKORD                                                 
         MVC   AUDKCPY,SVFRCPY                                                  
         MVC   AUDKORDN,ORD.ORDKORD                                             
         GOTO1 AIO,IOREAD+IOACCDIR+IO3                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOSAVKEY,IOKEY                                                   
         MVI   BYTE,NO                                                          
*                                                                               
GAPEXD24 GOTO1 AIO,IOSEQ+IOACCDIR+IO3 READ SEQUENTIALLY TO FIND LAST            
         BE    *+6                    AUDIT RECORD                              
         DC    H'0'                                                             
         CLC   IOSAVKEY(AUDKSEQ-AUDRECD),IOKEY                                  
         BNE   GAPEXD26                                                         
         MVC   IOSAVKEY,IOKEY                                                   
         B     GAPEXD24                                                         
*                                                                               
GAPEXD26 MVC   IOKEY,IOSAVKEY                                                   
         GOTO1 AIO,IOREAD+IOACCDIR+IO3                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGETREC+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO3                                                          
         LA    R4,AUDRFST                                                       
*                                                                               
         USING STCELD,R4                                                        
GAPEXD28 CLI   STCEL,0               FIND LAST ENTRY                            
         BE    GAPEXD34                                                         
         CLI   STCEL,STCELQ                                                     
         BE    GAPEXD32                                                         
GAPEXD30 LLC   R0,STCLN                                                         
         AR    R4,R0                                                            
         B     GAPEXD28                                                         
*                                                                               
GAPEXD32 MVC   SVUID,STCUSER         SAVE OFF USER AND PERS, OK COS             
         MVC   SVPERS,STCPERS        WE'RE USING GAP SERVICES ID                
         B     GAPEXD30                                                         
*                                                                               
GAPEXD34 LA    R4,AUDRFST                                                       
         XR    RF,RF                                                            
         ICM   RF,3,AUDRLEN                                                     
         AHI   RF,STCGLNQ                                                       
         CHI   RF,MAXLEN              CHECK WE CAN FIT STCELD ON RECORD         
         BNH   GAPEXD36                                                         
*                                                                               
         MVI   BYTE,YES               IF NOT ADD A NEW ONE                      
         MVC   TEMP,0(R3)                                                       
*                                                                               
         L     R0,AIO3                CLEAR OUT AIO3                            
         LHI   R1,IOAREALN                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   0(L'TEMP,R3),TEMP      MOVE KEY BACK IN                          
         LHI   R0,AUDRFST-AUDRECD                                               
         STCM  R0,3,AUDRLEN                                                     
         MVC   AUDKSTDT,TODAYC                                                  
         MVC   AUDKENDT,TODAYC                                                  
         LHI   R0,X'01'                                                         
         STCM  R0,3,AUDKINDX                                                    
         LLC   RF,AUDKSEQ                                                       
         AHI   RF,1                                                             
         STC   RF,AUDKSEQ                                                       
         CHI   RF,X'FF'                                                         
         BL    *+6                                                              
         DC    H'0'                   RUN OUT OF AUDITS                         
*                                                                               
GAPEXD36 LA    R4,TEMP                                                          
         XC    TEMP,TEMP                                                        
         MVI   STCEL,STCELQ                                                     
         MVI   STCIND,STCIORD2                                                  
         MVI   STCOTYP,STCOGAPQ                                                 
         OI    STCOTYP,STCOAURQ                                                 
         MVC   STCOUSR,SVUID                                                    
         MVC   STCOPID,SVPERS                                                   
         MVI   STCLN,STCGAPLQ                                                   
         GOTO1 DATCON,DMCB,(5,0),(1,STCODTE)                                    
         XC    DUB,DUB                                                          
         TIME  DEC                                                              
         SRL   R0,8                                                             
         SLL   R0,4                                                             
         AHI   R0,X'0C'                                                         
         STCM  R0,15,DUB+4                                                      
         ZAP   STCOTIM,DUB                                                      
         MVC   STCGAPFR,SVPGAPST                                                
         MVI   STCGAPTO,STCGREXP                                                
         LA    RF,KEYSAFE                                                       
         XR    RE,RE                                                            
         ICM   RE,7,OGAPEXD-OGAPASD(RF)                                         
         LNR   RE,RE                                                            
         STCM  RE,7,STCGEXDT                                                    
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'ACCMST'),AUDRECD,STCELD,(RF)                
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   BYTE,YES            ADDING NEW RECORD?                           
         BNE   GAPEXD38                                                         
         CLI   RCWRITE,NO                                                       
         BE    GAPEXD40                                                         
         GOTO1 AIO,IOADDREC+IOACCMST+IO3                                        
         BE    GAPEXD40                                                         
         DC    H'0'                                                             
*                                                                               
GAPEXD38 CLI   RCWRITE,NO                                                       
         BE    GAPEXD40                                                         
         GOTO1 AIO,IOPUTREC+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GAPEXD40 MVC   IOKEY,KEYSAFE       RESTORE KEY                                  
         LA    R1,IOHI+IOACCDIR+IO3                                             
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEYSAFE(OGAPSTA-OGAPASD),IOKEY                                   
         BE    GAPEXD04            IF NOT WRITING BACK READ SEQUENTIAL          
         B     GAPEXD06                                                         
*                                                                               
GAPEXD42 LLC   RF,SVFRCPY                                                       
         AHI   RF,1                                                             
         STC   RF,SVFRCPY                                                       
         CLI   SVFRCPY,X'FE'       HIT END OF COMPANIES?                        
         BNL   GAPEXDX                                                          
*                                                                               
         LA    R2,IOKEY                                                         
         XC    OGAPAS,OGAPAS                                                    
         MVI   OGAPTYP,OGAPTYPQ                                                 
         MVI   OGAPSUB,OGAPSUBQ                                                 
         MVC   OGAPCPY,SVFRCPY                                                  
         MVI   OGAPGAP,OGAPACC                                                  
         B     GAPEXD02                                                         
*                                                                               
GAPEXDX  XIT1  ,                                                                
         EJECT                                                                  
         DROP  R2,R3,R4,ORD                                                     
*                                                                               
***********************************************************************         
* Read supplier records and set GAP settings                          *         
* on SVSUPC= supplier code                                            *         
***********************************************************************         
         USING ACTRECD,R2                                                       
         USING RSTELD,R3                                                        
SUPGAPQ  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*SUPGAP*'                                                      
         LA    R2,IOKEY                                                         
         MVC   IOKEY,SPACES                                                     
         MVC   ACTKCPY,SVFRCPY                                                  
         MVC   ACTKULA,SVSUPC                                                   
         LA    R1,IORD+IOACCDIR+IO3                                             
         GOTO1 AIO                                                              
         JNE   SUPGAPQX                                                         
         MVC   IOSAVKEY,IOKEY                                                   
         LA    R1,IOGETREC+IOACCMST+IO3                                         
         GOTO1 AIO                                                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         GOTO1 VHELLO,DMCB,(C'G',=C'ACCMST'),('RSTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R3,12(R1)                                                        
*&&US                                                                           
         TM    RSTSTAT7,RSTGAPAQ   ACKNOWLEDGE/QUERY                            
         JZ    *+8                                                              
         MVI   SVACKQ,YES                                                       
*&&                                                                             
*&&UK                                                                           
         TM    RSTSTAT7,RSTGAPAY   ACKNOWLEDGE/QUERY                            
         JZ    *+8                                                              
         MVI   SVACKQ,YES                                                       
         TM    RSTSTAT7,RSTGAPAN                                                
         JZ    *+8                                                              
         MVI   SVACKQ,NO                                                        
*&&                                                                             
*&&US                                                                           
         CLI   SVACKQ,YES                                                       
         JE    SUPGAPQX                                                         
*&&                                                                             
*&&UK                                                                           
         CLI   SVACKQ,C' '                                                      
         JH    SUPGAPQX                                                         
*&&                                                                             
         USING LDGRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   LDGKEY,SPACES       READ LEDGER LEVELS                           
         MVC   LDGKCPY,SVFRCPY                                                  
         MVC   LDGKUNT(L'LDGKUNT+L'LDGKLDG),SVSUPC                              
         LA    R1,IORD+IOACCDIR+IO3                                             
         GOTO1 AIO                                                              
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGETREC+IOACCMST+IO3                                         
         GOTO1 AIO                                                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         GOTO1 VHELLO,DMCB,(C'G',=C'ACCMST'),('ACLELQ',LDGRECD),0               
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R3,12(R1)                                                        
         MVC   LDGAL1,ACLELLVA-ACLELD(R3)                                       
         MVC   LDGAL2,ACLELLVB-ACLELD(R3)                                       
         MVC   LDGAL3,ACLELLVC-ACLELD(R3)                                       
         MVC   LDGAL4,ACLELLVD-ACLELD(R3)                                       
*                                                                               
         USING ACTRECD,R2                                                       
SUPGAP02 CLI   LDGAL1,L'ACTKACT   Only 1 level?                                 
         JE    SUPGAPQX                                                         
         LA    R2,IOSAVKEY                                                      
*                                                                               
         LA    R0,4                                                             
         LA    R6,LDGAL4                                                        
SUPGAP04 CLI   0(R6),0                                                          
         JE    SUPGAP06                                                         
         CLI   0(R6),L'ACTKACT                                                  
         JE    SUPGAP06                                                         
         LLC   RF,0(R6)                                                         
         SHI   RF,1                                                             
         LA    RF,ACTKACT(RF)      Find length of account                       
         CLI   0(RF),C' '                                                       
         JH    SUPGAP08                                                         
SUPGAP06 SHI   R6,1                                                             
         JCT   R0,SUPGAP04                                                      
         J     SUPGAPQX                                                         
*                                                                               
K        USING ACTRECD,IOKEY                                                    
SUPGAP08 MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCPY,SVFRCPY                                                
         MVC   K.ACTKUNT(L'ACTKUNT+L'ACTKLDG),ACTKUNT                           
         LLC   RF,0(R6)                                                         
         SHI   RF,1                                                             
         LTR   RF,RF                                                            
         JNM   *+6                                                              
         DC    H'0'                                                             
         BASR  RE,0                                                             
         MVC   K.ACTKACT(0),ACTKACT                                             
SUPGAP10 LA    R1,IORD+IOACCDIR+IO3                                             
         GOTO1 AIO                                                              
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGETREC+IOACCMST+IO3                                         
         GOTO1 AIO                                                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         GOTO1 VHELLO,DMCB,(C'G',=C'ACCMST'),('RSTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R3,12(R1)                                                        
*&&US                                                                           
         MVI   SVACKQ,YES                                                       
         TM    RSTSTAT7,RSTGAPAQ   ACKNOWLEDGE/QUERY                            
         JZ    *+8                                                              
         MVI   SVACKQ,YES                                                       
*&&                                                                             
*&&UK                                                                           
SUPGAP12 CLI   SVACKQ,C' '         ACKNOWLEDGE/QUERY                            
         JH    SUPGAP14                                                         
         TM    RSTSTAT7,RSTGAPAY                                                
         JZ    *+8                                                              
         MVI   SVACKQ,YES                                                       
         TM    RSTSTAT7,RSTGAPAN                                                
         JZ    *+8                                                              
         MVI   SVACKQ,NO                                                        
*&&                                                                             
*                                                                               
SUPGAP14 DS    0H                                                               
*&&US                                                                           
         CLI   SVACKQ,YES                                                       
         JE    SUPGAPQX                                                         
*&&                                                                             
*&&UK                                                                           
         CLI   SVACKQ,C' '                                                      
         JH    SUPGAPQX                                                         
*&&                                                                             
         J     SUPGAP06                                                         
*                                                                               
SUPGAPQX XIT1  ,                                                                
         EJECT                                                                  
         DROP  K,R1,R2,R3                                                       
***********************************************************************         
* BUILD AUDIT ENTRY FOR DELETED/ADDED BATCH ITEM                      *         
***********************************************************************         
         SPACE 1                                                                
ORDAUDQ  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*ORDAUD*'                                                      
         LA    R5,ELEMENT                                                       
         L     R2,AIO1             R2=A(BATCH ITEM RECORD)                      
         XC    UOWCCNT,UOWCCNT                                                  
         XC    UOORDNUM,UOORDNUM                                                
                                                                                
         USING TBARECD,R2                                                       
         TM    TBARESTA,TBAESLDE   TEST LOGICALLY DELETED                       
         JNZ   ORDAUDX             YES - DROP THIS ITEM                         
                                                                                
         USING STCELD,R5                                                        
         XC    STCEL(L'ELEMENT),STCEL                                           
         MVI   STCEL,STCELQ                                                     
         MVI   STCIND,STCIORD2                                                  
         MVI   STCOTYP,STCOMATQ                                                 
         MVI   STCLN,STCOLN5Q                                                   
         MVC   STCOUSR,TBAKUSER                                                 
         MVC   STCOPID,TBAKBCHR                                                 
         MVC   STCODTE,TODAYRP                                                  
         MVI   STCOAPPL,STCOPSTM                                                
         MVC   STCOBREF,TBAKBREF                                                
         MVC   STCOBMY,TBAKBMOS   CONVERT YYMM -> YM EBCDIC                     
         OI    STCOBMY,X'F0'                                                    
         LLC   R1,STCOBMY+1                                                     
         LHI   RF,X'F0'                                                         
         TM    STCOBMY+1,X'10'                                                  
         JNO   *+8                                                              
         LHI   RF,X'B1'                                                         
         AR    R1,RF                                                            
         STC   R1,STCOBMY+1                                                     
                                                                                
         MVC   STCOIREF,UBREFNO                                                 
         MVC   STCOIDTE,UBREFDS                                                 
         XC    DUB,DUB                                                          
         TIME  DEC                                                              
         SRL   R0,8                                                             
         SLL   R0,4                                                             
         AHI   R0,X'0C'                                                         
         STCM  R0,15,DUB+4                                                      
         ZAP   STCOTIM,DUB                                                      
                                                                                
         LA    R3,TBARFST                                                       
         USING BIOELD,R3                                                        
         LAY   R4,UOWCTAB                                                       
WC       USING UOWCTAB,R4                                                       
ORDAUD02 CLI   BIOEL,0                                                          
         JE    ORDAUD18                                                         
         CLI   BIOEL,BIOELQ                                                     
         JE    ORDAUD14                                                         
         CLI   BIOEL,RUIELQ                                                     
         JE    ORDAUD06                                                         
ORDAUD04 LLC   RF,BIOLN                                                         
         AR    R3,RF                                                            
         J     ORDAUD02                                                         
                                                                                
         USING RUIELD,R3                                                        
ORDAUD06 MVC   STCOILOG,RUIREB                                                  
         J     ORDAUD04                                                         
                                                                                
         USING BIOELD,R3                                                        
ORDAUD14 OC    UOORDNUM,UOORDNUM   HAVE WE READ ANY BIOELS                      
         JNZ   ORDAUD16            YES                                          
         MVC   UOORDNUM,BIOONUM    NO - FIRST TIME EXTRACT ORDER NUMBER         
         MVI   STCOMIND,STCOIUMA                                                
                                                                                
ORDAUD16 MVC   WC.UOWITSEQ,BIOITSEQ                                             
         MVC   WC.UOWCMTCH,BIOWORK                                              
         ZAP   WC.UOWCAMT,=P'0'                                                 
*&&UK*&& ZAP   WC.UOWCFCAM,=P'0'                                                
         SP    WC.UOWCAMT,BIOAMNT                                               
*&&UK*&& SP    WC.UOWCFCAM,BIOFCAMT                                             
         LA    R4,UOWCTABL(R4)                                                  
         LLC   RF,UOWCCNT                                                       
         AHI   RF,1                                                             
         CHI   RF,UOWCMAXT*UOWCMAX DON'T EXCEED MAX ALLOWED                     
         JH    ORDAUD18            PUT OUT WHAT WE HAVE                         
         STC   RF,UOWCCNT                                                       
         J     ORDAUD04                                                         
                                                                                
ORDAUD18 OC    UOWCCNT,UOWCCNT     ANY ORDERS ON BATCH ITEM                     
         JZ    ORDAUDX             NO - EXIT                                    
         LA    R2,IOKEY            BUILD ORDER RECORD KEY                       
         USING ORDRECD,R2                                                       
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,SBATCPY                                                  
         MVC   ORDKORD,UOORDNUM                                                 
         GOTO1 AIO,IOREAD+IOACCDIR+IO3                                          
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         MVC   UOSTAT,ORDRSTAT     STORE OLD ORDER STATUS                       
         MVC   UOSTA2,ORDRSTA2                                                  
         LAY   R4,UOWCTAB                                                       
ORDAUD20 LA    R3,ORDRFST                                                       
         USING OAMELD,R3                                                        
ORDAUD22 CLI   OAMEL,0                                                          
         JE    ORDAUD30                                                         
         CLI   OAMEL,OAMELQ                                                     
         JE    ORDAUD28                                                         
ORDAUD24 LLC   RF,OAMLN                                                         
         AR    R3,RF                                                            
         J     ORDAUD22                                                         
*                                                                               
ORDAUD28 MVC   WC.UOWCORIG,OAMWORK                                              
         MVC   HALF,WC.UOWITSEQ    IS IT THE SAME ITEM SEQUENCE                 
         LA    R4,UOWCTABL(R4)                                                  
         CLC   HALF,WC.UOWITSEQ                                                 
         JNE   ORDAUD20            NO - RESET TO READ FROM BEGINNING OF         
         J     ORDAUD24                                   ORDER                 
*                                                                               
ORDAUD30 LAY   R4,UOWCTAB                                                       
         LLC   RF,UOWCCNT                                                       
ORDAUD32 LA    R3,STCOMTNT                                                      
AUD      USING STCOMTNT,R3                                                      
ORDAUD34 CP    WC.UOWCAMT,=P'0'                                                 
         JNE   ORDAUD36                                                         
*&&UK                                                                           
         CP    WC.UOWCFCAM,=P'0'                                                
*&&                                                                             
         JE    ORDAUD38                                                         
ORDAUD36 MVC   AUD.STCOOWC,WC.UOWCORIG                                          
         MVC   AUD.STCOMWC,SPACES                                               
         CLC   AUD.STCOOWC,SPACES                                               
         JE    *+10                                                             
         MVC   AUD.STCOMWC,WC.UOWCMTCH                                          
         ZAP   AUD.STCOMWCA,WC.UOWCAMT                                          
*&&UK                                                                           
         ZAP   AUD.STCOMWCF,WC.UOWCFCAM                                         
*&&                                                                             
         LA    R3,L'STCOMTNT(R3)                                                
         LLC   RE,STCLN                                                         
         AHI   RE,L'STCOMTNT                                                    
         STC   RE,STCLN                                                         
         LLC   RE,STCOMWC#                                                      
         AHI   RE,1                                                             
         STC   RE,STCOMWC#                                                      
ORDAUD38 MVC   HALF,WC.UOWITSEQ IS IT THE SAME ITEM SEQUENCE                    
         LA    R4,UOWCTABL(R4)                                                  
         SHI   RF,1                SUBTRACT FROM COUNT                          
         STC   RF,UOWCCNT          UPDATE COUNT                                 
         LTR   RF,RF               HAVE WE FINISHED                             
         JZ    ORDAUD40            YES                                          
         CLC   HALF,WC.UOWITSEQ NO - IS THIS PART OF THE SAME ITEM              
         JE    ORDAUD34            YES - GET NEXT ENTRY                         
         DROP  WC,AUD                                                           
                                                                                
         USING AUDRECD,R2                                                       
ORDAUD40 LA    R2,IOKEY            BUILD ORDER AUDIT RECORD                     
         XC    AUDKEY,AUDKEY                                                    
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,SBATCPY                                                  
         MVI   AUDKAUDT,AUDKORD                                                 
         MVC   AUDKORDN,UOORDNUM                                                
         GOTO1 AIO,IORD+IOACCDIR+IO3                                            
         JNE   ORDAUDX                                                          
         MVC   KEYTEMP,IOKEY       SAVE OFF KEY AND SEQUENCE                    
                                                                                
ORDAUD46 GOTO1 AIO,IOSEQ+IOACCDIR+IO3 FIND LAST AUDIT RECORD                    
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEYTEMP(AUDKSEQ-AUDRECD),IOKEY                                   
         JNE   ORDAUD48                                                         
         MVC   KEYTEMP,IOKEY                                                    
         J     ORDAUD46                                                         
*                                                                               
ORDAUD48 MVC   IOKEY,KEYTEMP                                                    
         GOTO1 AIO,IOREAD+IOACCDIR+IO3                                          
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 AIO,IOGETREC+IOACCMST+IO3                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,AUDRLEN        GET CURRENT RECORD LENGTH                    
         JNZ   *+8                                                              
         LHI   R0,AUDRFST+1-AUDRECD                                             
         LLC   R1,STCLN            R1=L'ELEMENT TO BE ADDED                     
         AR    R0,R1               UPDATE RECORD LENGTH                         
                                                                                
         CHI   R0,2000                                                          
         JH    ORDAUD50            NO ROOM ADD A NEW AUDIT RECORD               
*                                                                               
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'ACCMST'),AUDRECD,ELEMENT,(RF)               
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,IOPUTREC+IOACCMST+IO3                                        
         JE    ORDAUD60                                                         
         DC    H'0'                                                             
*                                                                               
ORDAUD50 L     R2,AIO3             CLEAR AIO AREA AND ADD NEW AUDIT             
         LR    R0,R2                                                            
         LHI   R1,2048                                                          
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         XC    AUDKEY,AUDKEY                                                    
         MVC   AUDKEY,KEYTEMP                                                   
         MVC   AUDRSTAT,UOSTAT                                                  
         MVC   AUDRSTA2,UOSTA2                                                  
         LLC   RF,AUDKSEQ                                                       
         AHI   RF,1                                                             
         STC   RF,AUDKSEQ                                                       
         LHI   R0,AUDRFST-AUDRECD                                               
         STCM  R0,3,AUDRLEN                                                     
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'ACCMST'),AUDRECD,ELEMENT,(RF)               
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,IOADDREC+IOACCMST+IO3                                        
         JE    *+6                                                              
         DC    H'0'                                                             
ORDAUD60 XR    RF,RF                                                            
         ICM   RF,1,UOWCCNT                                                     
         JNZ   ORDAUD32                                                         
*                                                                               
ORDAUDX  XIT1  ,                                                                
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
ACBGROUT CSECT                                                                  
*              ADDRESSES HIVED OFF FOR ADDRESSABILITY                           
***********************************************************************         
* PROCESS A BATCH ITEM ORDER ELEMENT                                  *         
* NTRY - R1=A(BATCH ITEM ORDER ELEMENT)                               *         
***********************************************************************         
*                                                                               
UPDORD   NMOD1 0,**UORD**                                                       
         LR    R3,R1               R3=A(BATCH ITEM ORDER ELEMENT)               
         USING BIOELD,R3                                                        
         L     RF,4(RD)            RECOVER RC                                   
         L     RC,68(RF)                                                        
         XC    UOSTAT,UOSTAT                                                    
         LA    R2,IOKEY            BUILD ORDER RECORD KEY                       
         USING ORDRECD,R2                                                       
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,SBATCPY                                                  
         MVC   ORDKORD,BIOONUM                                                  
         GOTO1 AIO,IOREAD+IOACCDIR+IO3                                          
         BE    *+12                                                             
         MVI   BYTE,1              SET ERROR CODE FOR IOTERR                    
         J     UOROUTL             AND CONTINUE                                 
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         MVC   UOSTAT,ORDRSTAT      SAVE ORDER STATUS FOR LATER                 
         USING CPTRBLK,FOOT1       TEMPORARY USE                                
         XC    CPTRBLK,CPTRBLK                                                  
         GOTO1 VPADDLE,DMCB,(C'D',AIO3),(C'K',CPTRBLK),0,0,ADCOMFAC             
*                                                                               
UPDORD02 LA    R4,ORDRFST                                                       
         USING ORDELD,R4                                                        
         SR    R0,R0                                                            
UPDORD04 CLI   ORDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                ORDER ELEMENT MISSING                        
         CLI   ORDEL,ORDELQ                                                     
         BE    *+14                                                             
         IC    R0,ORDLN                                                         
         AR    R4,R0                                                            
         B     UPDORD04                                                         
                                                                                
         MVI   GPFLAG,NO                                                        
         CLC   SCPYPROD,ORDACCU                                                 
         BNE   *+8                                                              
         MVI   GPFLAG,YES          SET PRODUCTION ORDER                         
         MVI   PRESFLAG,NO                                                      
                                                                                
         CLI   ORDLN,ORDLN2Q       TEST ELEMENT LONG ENOUGH                     
         BL    UPDORD05            NO                                           
         TM    ORDSTAT,ORDSPRES    TEST FOR PRESTO ORDER                        
         BZ    *+8                                                              
         MVI   PRESFLAG,YES        YES                                          
         CLI   ORDLN,ORDLN3Q                                                    
         BL    UPDORD05                                                         
         OI    ORDSTAT2,ORDSSTAT                                                
*                                                                               
UPDORD05 DS    0H                                                               
*&&UK                                                                           
         CLI   BIOSTAT,BIOSPTQ     DON'T CHANGE JOB IF PART MATCHING            
         BE    UPDORD10                                                         
         CLI   ORDLN,ORDLN3Q       CHECK LONG ENOUGH FOR ORIGINAL JOB           
         BL    UPDORD10                                                         
         CLC   ORDOJOB,SPACES      YES - RESTORE ORIGINAL JOB IF                
         BNH   UPDORD10                  NECESSARY                              
         MVC   ORDJOB,ORDOJOB                                                   
         XC    ORDOJOB,ORDOJOB                                                  
         B     UPDORD10                                                         
UPDORD06 CLC   ORDOJOB,SPACES      HAS JOB CHANGED BEFORE?                      
         BH    UPDORD08                                                         
         CLC   ORDJOB,KEYTEMP      NO - TEST IF DIFFERENT NOW                   
         BE    UPDORD10                                                         
         MVC   ORDOJOB,ORDJOB      SAVE ORIGINAL JOB                            
         MVC   ORDJOB,KEYTEMP      MOVE IN NEW JOB                              
UPDORD08 MVC   KEYTEMP(L'ORDJOB),ORDOJOB   USE ORIGINAL JOB FOR TX              
         B     *+10                                                             
UPDORD10 MVC   KEYTEMP(L'ORDJOB),ORDJOB                                         
         MVC   KEYTEMP+L'ORDJOB(L'ORDSUP),ORDSUP                                
*&&                                                                             
         GOTO1 UPDOAM,0            UPDATE ORDER AMOUNT ELEMENT                  
         OI    ORDRSTA2,ORDSSTAT   SET ORDER STATUS CHANGE                      
         CLI   PRESFLAG,YES                                                     
         BNE   UPDORD15                                                         
*                                                                               
         L     R1,ARAPBLK                                                       
         USING ACRAPD,R1                                                        
         XC    RAPBLK(RAPBLKL),RAPBLK                                           
         MVI   RAPACTN,RAPAELEM    RA ELEMENT MAINTENANCE CALL                  
         MVC   RAPCPY,SBATCPY                                                   
         MVI   RAPEMU,C'N'                                                      
         MVI   RAPRTYP,RAPKRORD                                                 
         MVC   RAPACOM,ADCOMFAC                                                 
         MVC   RAPAREC,AIO3                                                     
         GOTO1 VRAPPER,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDORD15 GOTO1 AIO,IOPUTREC+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   UONSTAT,ORDRSTAT                                                 
         GOTO1 VPADDLE,DMCB,(C'A',AIO3),(C'A',CPTRBLK),IODA,0,ADCOMFAC          
*                                                                               
         CLI   PRESFLAG,YES        TEST FOR PRESTO ORDER                        
         BNE   UPDORD16                                                         
         CLI   RCWRITE,NO          TEST WRITE=NO                                
         BE    UPDORD16            YES                                          
*                                                                               
         L     R1,ARAPBLK                                                       
         MVI   RAPACTN,RAPAPTR     YES-NOW DO POINTER MAINTENANCE               
         GOTO1 VRAPPER,(R1)                                                     
         BE    UPDORD16                                                         
         DC    H'0'                                                             
*                                                                               
UPDORD16 DS    0H                                                               
*&&US*&& TM    ORDRSTAT,ORDSCON    CONTRACT ORDER?                              
*&&US*&& BO    UPDORD70                                                         
         LA    R2,IOKEY            BUILD KEY FOR ORDER TRANSACTION              
         USING TRNRECD,R2                                                       
*&&UK*&& MVC   TRNKCULA,KEYTEMP                                                 
*&&UK*&& MVC   TRNKCULC,KEYTEMP+L'TRNKCULA                                      
*&&US*&& MVC   TRNKCULA,ORDJOB                                                  
*&&US*&& MVC   TRNKCULC,ORDSUP                                                  
         MVC   TRNKWORK,=C'**'                                                  
         CLC   TRNKUNT(L'CPYPROD),SCPYPROD                                      
         BE    *+10                                                             
         MVC   TRNKWORK(L'TRNKWORK+L'TRNKCCPY),SPACES                           
         MVC   TRNKDATE,ORDDATE                                                 
         MVC   TRNKREF,BIOONUM                                                  
         MVI   TRNKSBR,0                                                        
*                                                                               
         GOTO1 AIO,IORDD+IOACCDIR+IO3                                           
         BE    UPDORD18                                                         
         CLI   IOERR,IOEDEL        ALLOW DELETES, BUT NO OTHER ERROR            
         BE    UPDORD18                                                         
         MVI   BYTE,1              SET ERROR CODE FOR IOTERR                    
         J     UOROUTL             AND CONTINUE                                 
*                                                                               
UPDORD18 GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO3                                                          
         GOTO1 UPDOAM,1            UPDATE ORDER AMOUNT ELEMENT                  
*                                                                               
         GOTO1 AIO,IOPUTREC+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY+(TRNKSTA-TRNRECD)(L'TRNKSTA),TRNRSTA                       
         GOTO1 AIO,IOWRITE+IOACCDIR+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* STCEL UPDATE (BRANDOCEAN AUDIT TRAIL)                                         
*                                                                               
UPDORD70 CLI   UOCBIOSN,1          ONLY DO THIS ONCE                            
         JNE   UPDORDX                                                          
         CLC   UOSTAT,UONSTAT      CHECK THE ORDER MATCHING STATUS              
         JE    UPDORDX                                                          
*                                                                               
UPDORD72 GOTO1 UPDAUD                                                           
*                                                                               
UPDORDX  J     UOROUTX                                                          
         DROP  R1,R2,R3,R4                                                      
         EJECT                                                                  
***********************************************************************         
* COMMON EXITS FOR THIS CSECT                                                   
***********************************************************************         
*                                                                               
UOROUTL  CLI   *,255               SET CC LOW                                   
         J     UOROUTX                                                          
*                                                                               
UOROUTH  CLI   *,0                 SET CC HIGH                                  
         J     UOROUTX                                                          
*                                                                               
UOROUTE  CLI   *+1,0               SET CC EQUAL                                 
*                                                                               
UOROUTX  XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* UPDATE ORDER AUDIT/HISTORY FOR MATCHING ACTIVITY                    *         
***********************************************************************         
         SPACE 1                                                                
         USING BIOELD,R3                                                        
         USING STCELD,R4                                                        
         USING TBARECD,R1                                                       
UPDAUD   NTR1  ,                                                                
                                                                                
UPDAUD58 L     R1,AIO1                                                          
         LA    R4,ELEMENT                                                       
         XC    STCEL(L'ELEMENT),STCEL                                           
         MVI   STCEL,STCELQ                                                     
         MVI   STCIND,STCIORD2                                                  
         MVI   STCOTYP,STCOCHGQ                                                 
         MVI   STCLN,STCOLN2Q                                                   
         MVC   STCOUSR,TBAKUSER                                                 
         MVC   STCOPID,TBAKBCHR                                                 
         DROP  R1                                                               
         MVC   STCODTE,TODAYRP                                                  
         OI    STCOIND3,STCOWSTQ SET WORKFLOW STATUS HAS CHANGED                
         MVI   STCOFRST,STCOAPPD                                                
         TM    UOSTAT,ORDSFMCH   IF ORDER WAS PREVIOUSLY FULLY MATCHED          
         JZ    *+8                                                              
         MVI   STCOFRST,STCOCOMP IT SHOULD BE MARKED AS COMPLETE                
         MVI   STCOTOST,STCOAPPD APPROVED TO STATUS                             
         ZAP   STCOAMT,PZERO                                                    
         L     R1,ATRNBLK                                                       
         USING TRNBLKD,R1                                                       
         MVC   STCOCURC,TRNCCCUR AGENCY CURRENCY CODE                           
         DROP  R1                                                               
         XC    DUB,DUB                                                          
         TIME  DEC                                                              
         SRL   R0,8                                                             
         SLL   R0,4                                                             
         AHI   R0,X'0C'                                                         
         STCM  R0,15,DUB+4                                                      
         ZAP   STCOTIM,DUB                                                      
                                                                                
         LA    R2,IOKEY            BUILD ORDER RECORD KEY                       
         USING ORDRECD,R2                                                       
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,SBATCPY                                                  
         MVC   ORDKORD,BIOONUM                                                  
         MVC   UOORDNUM,BIOONUM                                                 
         GOTO1 AIO,IOREAD+IOACCDIR+IO3                                          
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         MVC   UOSTAT,ORDRSTAT     STORE ORDER STATUS                           
         MVC   UOSTA2,ORDRSTA2                                                  
         LA    R3,ORDRFST                                                       
         USING ORDELD,R3                                                        
UPDAUD60 CLI   ORDEL,0                                                          
         JE    UPDAUD92                                                         
         CLI   ORDEL,ORDELQ                                                     
         JE    UPDAUD64                                                         
         CLI   ORDEL,AFCELQ                                                     
         JE    UPDAUD68                                                         
         CLI   ORDEL,FFTELQ                                                     
         JE    UPDAUD74                                                         
         CLI   ORDEL,SPAELQ                                                     
         JE    UPDAUD70                                                         
         CLI   ORDEL,SORELQ                                                     
         JE    UPDAUD78                                                         
         CLI   ORDEL,OAMELQ                                                     
         JE    UPDAUD80                                                         
UPDAUD62 LLC   RF,ORDLN                                                         
         AR    R3,RF                                                            
         J     UPDAUD60                                                         
                                                                                
UPDAUD64 MVI   STCOSTAT,STCOIUMA  SET THE DEFAULT OF UNMATCHED                  
         TM    ORDSTAT,ORDSPART   IF ORDER IS NOW PART MATCHED                  
         JZ    *+8                                                              
         MVI   STCOSTAT,STCOIPMA  IT SHOULD BE MARKED AS PART MATCHED           
         TM    ORDRSTAT,ORDSFMCH  IF ORDER IS NOW FULLY MATCHED                 
         JZ    *+12                                                             
         MVI   STCOTOST,STCOCOMP  IT SHOULD BE MARKED AS COMPLETE               
         MVI   STCOSTAT,STCOIFMA  IT SHOULD BE MARKED AS FULLY MATCHED          
         TM    ORDRSTAT,ORDGDRCV  GOODS RECIEVED                                
         JZ    *+8                                                              
         OI    STCOSTAT,STCOGDRD  YES                                           
         MVC   STCODATE,ORDDATE                                                 
         MVC   STCORQDT,ORDRQBD                                                 
         MVC   STCOSULA,ORDSUPU   COPY SUPPLIER                                 
         CLI   GPFLAG,YES         IS IT PRODUCTION ORDER                        
         JNE   UPDAUD66           NO                                            
         MVC   STCOSJAC,ORDACCA   COPY JOB                                      
         J     UPDAUD62                                                         
                                                                                
UPDAUD66 MVC   STCOXULA,ORDACCU   COPY EXPENSE ACCOUNT                          
         J     UPDAUD62                                                         
                                                                                
         USING AFCELD,R3                                                        
UPDAUD68 ZAP   STCOFCAM,AFCAMNT   COPY CURRENCY INFORMATION                     
         MVC   STCOCURC,AFCCURR                                                 
         J     UPDAUD62                                                         
                                                                                
         USING SPAELD,R3                                                        
UPDAUD70 CLI   SPATYPE,SPATDEPT   COPY DEPARTMENT INFORMATION                   
         JNE   UPDAUD72                                                         
         MVC   STCO2DAC,SPAAACT                                                 
         J     UPDAUD62                                                         
                                                                                
UPDAUD72 CLI   SPATYPE,SPATPERS   COPY PERSON INFORMATION                       
         JNE   UPDAUD62                                                         
         MVC   STCO2PAC,SPAAACT                                                 
         J     UPDAUD62                                                         
                                                                                
         USING FFTELD,R3                                                        
UPDAUD74 CLI   FFTTYPE,FFTTEXTY   COPY ETYPE INFORMATION                        
         JNE   UPDAUD76                                                         
         LLC   RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         MVC   STCOETYP(0),FFTDATA                                              
         EX    RF,*-6                                                           
         OC    STCOETYP,SPACES                                                  
         J     UPDAUD62                                                         
                                                                                
UPDAUD76 CLI   FFTTYPE,FFTTOFFC                                                 
         JNE   UPDAUD62                                                         
         OC    STCO2DAC,STCO2DAC                                                
         JNZ   UPDAUD62                                                         
         LLC   RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         MVC   STCO2DAC(0),FFTDATA                                              
         EX    RF,*-6                                                           
         OC    STCO2DAC,SPACES                                                  
         J     UPDAUD62                                                         
                                                                                
         USING SORELD,R3                                                        
UPDAUD78 MVC   STCOSJAC,SORAACT   COPY JOB INFORMATION                          
         J     UPDAUD62                                                         
                                                                                
         USING OAMELD,R3                                                        
UPDAUD80 AP    STCOAMT,OAMAMNT    ADD UP TOTAL FOR ORDER                        
         J     UPDAUD62                                                         
                                                                                
         USING AUDRECD,R2                                                       
UPDAUD92 LA    R2,IOKEY           BUILD ORDER AUDIT RECORD                      
         XC    AUDKEY,AUDKEY                                                    
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,SBATCPY                                                  
         MVI   AUDKAUDT,AUDKORD                                                 
         MVC   AUDKORDN,UOORDNUM                                                
         GOTO1 AIO,IORD+IOACCDIR+IO3                                            
         JNE   UPDAUDX                                                          
         MVC   KEYTEMP,IOKEY       SAVE OFF KEY AND SEQUENCE                    
*                                                                               
UPDAUD94 GOTO1 AIO,IOSEQ+IOACCDIR+IO3 FIND LAST AUDIT RECORD                    
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEYTEMP(AUDKSEQ-AUDRECD),IOKEY                                   
         JNE   UPDAUD96                                                         
         MVC   KEYTEMP,IOKEY                                                    
         J     UPDAUD94                                                         
*                                                                               
UPDAUD96 MVC   IOKEY,KEYTEMP                                                    
         GOTO1 AIO,IORD+IOACCDIR+IO3                                            
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGETREC+IOACCMST+IO3                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,AUDRLEN        GET CURRENT RECORD LENGTH                    
         JNZ   *+8                                                              
         LHI   R0,AUDRFST+1-AUDRECD                                             
         LLC   R1,STCLN            R1=L'ELEMENT TO BE ADDED                     
         AR    R0,R1               UPDATE RECORD LENGTH                         
                                                                                
         CHI   R0,2000                                                          
         JH    UPDAUD98            NO ROOM ADD A NEW AUDIT RECORD               
*                                                                               
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AUDRECD,ELEMENT,(RF)                   
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOPUTREC+IOACCMST+IO3                                        
         JE    UPDAUDX                                                          
         DC    H'0'                                                             
*                                                                               
UPDAUD98 L     R2,AIO3             CLEAR AIO AREA AND ADD NEW AUDIT             
         LR    R0,R2                                                            
         LHI   R1,2048                                                          
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         XC    AUDKEY,AUDKEY                                                    
         MVC   AUDKEY,KEYTEMP                                                   
         MVC   AUDRSTAT,UOSTAT                                                  
         MVC   AUDRSTA2,UOSTA2                                                  
         LLC   RF,AUDKSEQ                                                       
         AHI   RF,1                                                             
         STC   RF,AUDKSEQ                                                       
         LHI   R0,AUDRFST-AUDRECD                                               
         STCM  R0,3,AUDRLEN                                                     
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AUDRECD,ELEMENT,(RF)                   
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 AIO,IOADDREC+IOACCMST+IO3                                        
         JE    *+6                                                              
         DC    H'0'                                                             
UPDAUDX  J     UOROUTX                                                          
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* UPDATE ORDER AMOUNT ELEMENT IN ORDER RECORD/TRANSACTION             *         
* NTRY - R1=0 IF ORDER RECORD PASSED                                  *         
*        R1=1 IF ORDER TRANSACTION PASSED                             *         
*        R3=A(BATCH ITEM ORDER ELEMENT)                               *         
***********************************************************************         
*                                                                               
         USING BIOELD,R3                                                        
         USING ORDRECD,R5                                                       
UPDOAM   NTR1  ,                                                                
         LR    R2,R1                                                            
         L     R5,AIO3                                                          
         LA    R1,ORDRFST                                                       
         SR    R0,R0                                                            
         USING OAMELD,R1                                                        
UPDOAM02 CLI   OAMEL,0                                                          
         BE    UPDOAMX                                                          
         CLI   OAMEL,OAMELQ                                                     
         BE    UPDOAM05                                                         
UPDOAM04 IC    R0,OAMLN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     UPDOAM02            LOOP BACK UP FOR NEXT ELEMENT                
*                                                                               
UPDOAM05 CLI   GPFLAG,YES          TEST PRODUCTION ORDER                        
         BNE   *+14                                                             
         CLC   OAMWORK,BIOWORK     TEST WORKCODE MATCHES                        
         BNE   UPDOAM04                                                         
                                                                                
         TM    UBINDS,UBIDEL       ARE WE DELETING                              
         JZ    UPDOAM07            NO                                           
         L     RF,AIO2             TEST IF BATCH IS CLOSED                      
         TM    TBARHSTA-TBARECD(RF),TBAHSEND                                    
         BNZ   UPDOAM06            IF CLOSED DON'T REPEAT WHEN UPDATING         
         LTR   R2,R2               TEST THIS IS THE ORDER RECORD                
         JNZ   UPDOAM07            NO - THIS IS THE TRANSACTION                 
         SP    OAMTVAL,BIOAMNT                                                  
         J     UPDOAM07                                                         
*                                                                               
UPDOAM06 SP    OAMIVAL,BIOAMNT     ADD BACK AMOUNT INVOICED TO DATE             
         SP    OAMINUM,=P'1'                                                    
*                                                                               
UPDOAM07 TM    UBINDS,UBIUPD+UBIDEL ARE WE UPDATING OR DELETING                 
         JZ    UPDOAM08            NO                                           
         LLC   RF,OAMIPND          YES - REDUCE PENDING COUNT                   
         SHI   RF,1                                                             
         STC   RF,OAMIPND                                                       
*                                                                               
UPDOAM08 LTR   R2,R2               TEST THIS IS THE ORDER RECORD                
         JNZ   UPDOAM20            NO - THIS IS THE TRANSACTION                 
         LA    R4,ORDRFST                                                       
         SR    RE,RE                                                            
         USING ORDELD,R4                                                        
UPDOAM10 CLI   ORDEL,ORDELQ        FIND THE ORDEL                               
         JE    UPDOAM12                                                         
         IC    RE,ORDLN                                                         
         AR    R4,RE                                                            
         J     UPDOAM10                                                         
*                                                                               
UPDOAM12 TM    UBINDS,UBIUPD       ARE WE UPDATING                              
         JZ    UPDOAM16            NO                                           
         CLI   BIOSTAT,BIOSPTQ     IS THIS A PART MATCHED ORDER                 
         JE    UPDOAM18            NO                                           
*                                                                               
UPDOAM14 NI    ORDSTAT,X'FF'-ORDSPART                                           
         OI    ORDSTAT,ORDSMNUP                                                 
         OI    ORDRSTAT,ORDSFMCH                                                
         J     UPDOAMX                                                          
*                                                                               
UPDOAM16 TM    UBINDS,UBIDEL       ARE WE DELETING                              
         JZ    UPDOAMX             NO                                           
         CLI   BIOSTAT,BIOSPTQ     IS THIS A PART MATCHED ORDER                 
         JNE   UPDOAM19            NO -  REMOVE FULLY MATCHED STAT              
                                                                                
UPDOAM18 TM    ORDRSTAT,ORDSFMCH   IF SAVE/CLOSED WAS IT FULLY MATCHED          
         JNZ   UPDOAMX             PREVIOUSLY - IF SO DO NOTHING                
UPDOAM19 OI    ORDSTAT,ORDSPART                                                 
         NI    ORDSTAT,X'FF'-ORDSMNUP                                           
         NI    ORDRSTAT,X'FF'-ORDSFMCH  ASSUME PART MATCHED NOW                 
         CP    OAMINUM,PZERO       ANY INVOICES LEFT ON ORDER?                  
         JNE   UPDOAMX                                                          
         OC    OAMIPND,OAMIPND                                                  
         JNZ   UPDOAMX                                                          
         NI    ORDSTAT,X'FF'-ORDSPART    UNMATCH IF NOTHING LEFT                
         J     UPDOAMX                                                          
*                                                                               
* TRANSACTION                                                                   
*                                                                               
UPDOAM20 TM    UBINDS,UBIUPD       ARE WE UPDATING                              
         JZ    UPDOAMX             NO                                           
         CLI   BIOSTAT,BIOSPTQ                                                  
         JE    UPDOAM22            PARTMATCHED                                  
         OI    ORDRSTAT,TRNSDELT   DELETE IF ORDER IS FULLY MATCHED             
         J     UPDOAMX                                                          
*                                                                               
UPDOAM22 TM    UOSTAT,ORDSFMCH     FULLY MATCHED ORDER                          
         JNZ   UPDOAMX             YES - DON'T UNDELETE TRANS                   
UPDOAM24 NI    ORDRSTAT,X'FF'-TRNSDELT UNDELETE THE TRANSACTION                 
                                                                                
UPDOAMX  J     UOROUTX                                                          
         DROP  R1,R3,R4,R5                                                      
         EJECT                                                                  
ACBG02   CSECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
GLOBALS  DS    0D                                                               
MAXLEN   EQU   2000                                                             
         LTORG                                                                  
         SPACE 1                                                                
AUPDORD  DC    A(UPDORD)                                                        
*********************************************************************           
* COMMON ADCONS                                                     *           
*********************************************************************           
         DS    0D                                                               
VADDTRN  DC    AL1(QADDTRN),AL3(0)  LOADABLE PHASES                             
         DC    AL1(0)                                                           
*                                                                               
VBMONVAL DC    V(BMONVAL)                                                       
VCONVMOS DC    V(CONVMOS)                                                       
VPERVERT DC    V(PERVERT)                                                       
VRAPPER  DC    V(ACRAPPER)                                                      
ARPRGTAB DC    A(RPRGTAB)                                                       
AWKRTAB  DC    A(WKRTAB)                                                        
ADEFRTYP DC    A(DEFRTYP)                                                       
ATYPTAB  DC    A(TYPTAB)                                                        
BGATYPES EQU   *                          SHARED ATYPES (UPDORD CSECT)          
         DC    A(IOAREA1)                                                       
         DC    A(IOAREA1+L'IODA+L'IOWORK)                                       
         DC    A(IOAREA2)                                                       
         DC    A(IOAREA2+L'IODA+L'IOWORK)                                       
         DC    A(IOAREA3)                                                       
         DC    A(IOAREA3+L'IODA+L'IOWORK)                                       
         DC    A(IOAREA4)                                                       
         DC    A(IOAREA4+L'IODA+L'IOWORK)                                       
         DC    A(IOAREA5)                                                       
         DC    A(IOAREA5+L'IODA+L'IOWORK)                                       
BGATYPEL EQU   *-BGATYPES                                                       
BGATYNUM EQU   BGATYPEL/4                                                       
AWKAREA  DC    A(WKAREA)                                                        
AADDTIO1 DC    A(ADDTIO1)                                                       
AADDTIO2 DC    A(ADDTIO2)                                                       
AADDTIO3 DC    A(ADDTIO3)                                                       
AADDTIO4 DC    A(ADDTIO4)                                                       
ACHASTAB DC    A(CHASTAB)                                                       
AACMTAB  DC    A(ACMTAB)                                                        
AREPTOT  DC    A(REPTOT)                                                        
ATOTTAB  DC    A(TOTTAB)                                                        
ATRNWK   DC    A(TRNWK)                                                         
AGENWK   DC    A(GENWK)                                                         
ARAPBLK  DC    A(RAPBLOCK)                                                      
ACPYTAB  DC    A(CPYTAB)                                                        
ATRNBLK  DC    A(ADDTBLK)                                                       
AXTPQF   DC    A(XTPQFS)                                                        
ADCUC    DC    A(DCUC)                                                          
ADCLC1   DC    A(DCLC1)                                                         
ADCLC2   DC    A(DCLC2)                                                         
ADSLC2   DC    A(0)                                                             
*                                                                               
SORTCAD1 DC    C'SORT FIELDS=(1,26,A),FORMAT=BI,WORK=1 '                        
SORTREC1 DC    C'RECORD TYPE=F,LENGTH=108 '                                     
SORTCAD2 DC    C'SORT FIELDS=(1,37,A),FORMAT=BI,WORK=1 '                        
SORTREC2 DC    C'RECORD TYPE=F,LENGTH=44 '                                      
SORTCAD3 DC    C'SORT FIELDS=(1,42,A),FORMAT=BI,WORK=1 '                        
SORTREC3 DC    C'RECORD TYPE=F,LENGTH=42 '                                      
SORTPUT  DC    C'PUT'                                                           
SORTGET  DC    C'GET'                                                           
*                                                                               
PZERO    DC    PL8'0'              Leave as PL8, ACSMFBAL needs it              
CZERO    DC    4C'0'                                                            
EFFALL   DC    4X'FF'                                                           
EFFS     EQU   X'FF'                                                            
RECLMAX  EQU   1000                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
ACCRCV   DC    C'RECOVRY'                                                       
RCVREAD  DC    C'READ   '                                                       
HEXTOG   DC    C'TOG'                                                           
HEXSEP   DC    C'SEP'                                                           
CHRTAB   DC    (12*16)C'.'             00-BF    TRANSLATE TABLE FOR             
         ORG   CHRTAB+X'40'                     CHARACTER DISPLAY               
         DC    C' '                                                             
         ORG                                                                    
         DC    CL16'.ABCDEFGHI......'  C0-CF                                    
         DC    CL16'.JKLMNOPQR......'  D0-DF                                    
         DC    CL16'..STUVWXYZ......'  E0-EF                                    
         DC    CL16'0123456789......'  F0-FF                                    
*                                                                               
MOSTAB   DC    C'123456789......ABC'                                            
DATA1ID  DC    AL2(2285)           DATA CONTROL UIDS FOR WFM                    
DATA2ID  DC    AL2(2286)                                                        
DATA3ID  DC    AL2(2836)                                                        
         EJECT                                                                  
***********************************************************************         
* IOEXEC CONSTANTS                                                    *         
***********************************************************************         
* I/O FILE NAMES                                                                
***********************************************************************         
ACCDIR   DC    C'ACCDIR '                                                       
ACCMST   DC    C'ACCMST '                                                       
WKFILE   DC    C'WKFILE '                                                       
CTFILE   DC    C'CTFILE '                                                       
ACCDAY   DC    C'ACCDAY '                                                       
*                                                                               
*              ACCDAY OPEN CALL                                                 
*                                                                               
DMOPEN   DC    C'DMOPEN '                                                       
NACCDAY  DC    C'NACCDAY X'                                                     
*                                                                               
* I/O COMMANDS                                                                  
*                ACCDIR + CTFILE                                                
*                                                                               
ISCMNDS  DS    0CL8                                                             
         DC    C'DMRDHI ',AL1(0)                                                
         DC    C'DMREAD ',AL1(0)                                                
         DC    C'DMRSEQ ',AL1(0)                                                
         DC    C'DMADD  ',AL1(IOCTIUPD)                                         
         DC    C'DMWRT  ',AL1(IOCTIUPD)                                         
*                                                                               
*                ACCMST+ACCDAY                                                  
*                                                                               
DACMNDS  DS    0CL8                                                             
         DC    C'GETREC ',AL1(0)                                                
         DC    C'PUTREC ',AL1(IOCTIUPD+IOCTISAD)                                
         DC    C'ADDREC ',AL1(IOCTIUPD)                                         
         DC    C'ADFREC ',AL1(0)                                                
         DC    C'DMRDIR ',AL1(0)                                                
*                                                                               
*                WKFILE                                                         
*                                                                               
WKCMNDS  DS    0CL8                                                             
WKINDEX  DC    C'INDEX  ',AL1(0)                                                
         DC    C'RANDOM ',AL1(0)                                                
         DC    C'READ   ',AL1(0)                                                
         DC    C'SEQ    ',AL1(0)                                                
         DC    C'ADD    ',AL1(IOCTIUPD)                                         
         DC    C'WKWRIT ',AL1(IOCTIUPD)                                         
         DC    C'KEEP   ',AL1(IOCTIUPD)                                         
         DC    C'CLOSE  ',AL1(IOCTIUPD)                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DATA DICTIONARY, SORT AND OTHER CONSTANTS                           *         
***********************************************************************         
*                                                                               
DCUC     DS    0X                                                               
         DCDDL AC#ANY,L'AC@ANY                                                  
         DCDDL AC#AUPOS,L'AC@AUPOS                                              
         DCDDL AC#CLIBG,L'UC@CLIBG                                              
         DCDDL AC#CLIWB,L'UC@CLIWB                                              
         DCDDL AC#MANBL,L'UC@MANBL                                              
         DCDDL AC#MEDBT,L'AC@MEDBT                                              
         DCDDL AC#WFMPS,L'AC@WFMPS                                              
         DCDDL AC#TMSHR,L'AC@TMSHR                                              
         DCDDL AC#R8ACR,L'AC@R8ACR                                              
         DCDDL AC#R8ARV,L'AC@R8ARV                                              
         DC    AL1(0)                                                           
*                                                                               
DCLC1    DS    0X                                                               
         DCDDL AC#ACRL,L'AC@ACRL                                                
         DCDDL AC#ACRRV,L'AC@ACRR                                               
         DCDDL AC#BAT,L'AC@BAT                                                  
         DCDDL AC#BATC,L'AC@BATC                                                
         DCDDL AC#CRP,L'AC@CRP                                                  
         DCDDL AC#DAYS,L'AC@DAYS                                                
         DCDDL AC#DRDEL,L'AC@DRDEL                                              
         DCDDL AC#DRMLI,L'AC@DRMLI                                              
         DCDDL AC#DRWAR,L'AC@DRWAR                                              
         DCDDL AC#DULIS,L'AC@DULIS                                              
         DCDDL AC#DULIW,L'AC@DULIW                                              
         DCDDL AC#INSUP,L'AC@INSUP                                              
         DCDDL AC#MOSLK,L'AC@MOSLK                                              
         DCDDL AC#ONLUP,L'AC@ONLUP                                              
         DCDDL AC#OPEN2,L'AC@OPEN2                                              
         DCDDL AC#OVNUP,L'AC@OVNUP                                              
         DCDDL AC#PGA08,L'AC@PGA08                                              
         DCDDL AC#PGA09,L'AC@PGA09                                              
         DCDDL AC#PQFID,L'AC@PQFID                                              
         DCDDL AC#RCVBT,L'AC@RCVB                                               
         DCDDL AC#SAVED,L'AC@SAVED                                              
         DCDDL AC#TPSTS,L'AC@TPSTS                                              
         DCDDL AC#UAPR,L'AC@UAPR                                                
         DCDDL AC#USRID,L'AC@USRID                                              
         DCDDL AC#WFIBT,L'AC@WKRB                                               
         DC    AL1(0)                                                           
*                                                                               
DCLC2    DS    0X                                                               
         DCDDL AC#TT001,L'AC@TT001                                              
         DCDDL AC#TT002,L'AC@TT001                                              
         DCDDL AC#TT003,L'AC@TT001                                              
         DCDDL AC#TT004,L'AC@TT001                                              
         DCDDL AC#TT005,L'AC@TT001                                              
         DCDDL AC#TT006,L'AC@TT001                                              
         DCDDL AC#TT007,L'AC@TT001                                              
         DCDDL AC#TT008,L'AC@TT001                                              
         DCDDL AC#TT009,L'AC@TT001                                              
         DCDDL AC#TT010,L'AC@TT001                                              
         DCDDL AC#TT011,L'AC@TT001                                              
         DCDDL AC#TT012,L'AC@TT001                                              
         DCDDL AC#TT013,L'AC@TT001                                              
         DCDDL AC#TT014,L'AC@TT001                                              
         DCDDL AC#TT015,L'AC@TT001                                              
         DCDDL AC#TT016,L'AC@TT001                                              
         DCDDL AC#TT017,L'AC@TT001                                              
         DCDDL AC#TT018,L'AC@TT001                                              
         DCDDL AC#TT019,L'AC@TT001                                              
         DCDDL AC#TT020,L'AC@TT001                                              
         DCDDL AC#TT021,L'AC@TT001                                              
         DCDDL AC#TT022,L'AC@TT001                                              
         DCDDL AC#TT023,L'AC@TT001                                              
         DCDDL AC#TT024,L'AC@TT001                                              
         DCDDL AC#TT025,L'AC@TT001                                              
         DCDDL AC#TT026,L'AC@TT001                                              
         DCDDL AC#TT027,L'AC@TT001                                              
         DCDDL AC#TT028,L'AC@TT001                                              
         DCDDL AC#TT029,L'AC@TT001                                              
         DCDDL AC#TT030,L'AC@TT001                                              
         DCDDL AC#TT031,L'AC@TT001                                              
         DCDDL AC#TT032,L'AC@TT001                                              
         DCDDL AC#TT033,L'AC@TT001                                              
         DCDDL AC#TT034,L'AC@TT001                                              
         DCDDL AC#TT035,L'AC@TT001                                              
         DCDDL AC#TT036,L'AC@TT001                                              
         DCDDL AC#TT037,L'AC@TT001                                              
         DCDDL AC#TT038,L'AC@TT001                                              
         DCDDL AC#TT039,L'AC@TT001                                              
         DCDDL AC#TT040,L'AC@TT001                                              
         DCDDL AC#TT041,L'AC@TT001                                              
         DCDDL AC#TT042,L'AC@TT001                                              
         DCDDL AC#TT043,L'AC@TT001                                              
         DCDDL AC#TT044,L'AC@TT001                                              
         DCDDL AC#TT045,L'AC@TT001                                              
         DCDDL AC#TT046,L'AC@TT001                                              
         DCDDL AC#TT047,L'AC@TT001                                              
         DCDDL AC#TT048,L'AC@TT001                                              
         DCDDL AC#TT049,L'AC@TT001                                              
         DCDDL AC#TT050,L'AC@TT001                                              
         DCDDL AC#TT051,L'AC@TT001                                              
         DCDDL AC#TT052,L'AC@TT001                                              
         DCDDL AC#TT053,L'AC@TT001                                              
         DCDDL AC#TT054,L'AC@TT001                                              
         DCDDL AC#TT055,L'AC@TT001                                              
         DCDDL AC#TT056,L'AC@TT001                                              
         DCDDL AC#TT057,L'AC@TT001                                              
         DCDDL AC#TT058,L'AC@TT001                                              
         DCDDL AC#TT059,L'AC@TT001                                              
         DCDDL AC#TT060,L'AC@TT001                                              
         DCDDL AC#TT061,L'AC@TT001                                              
         DCDDL AC#TT062,L'AC@TT001                                              
         DCDDL AC#TT064,L'AC@TT001                                              
         DCDDL AC#TT070,L'AC@TT001                                              
         DCDDL AC#TT071,L'AC@TT001                                              
         DCDDL AC#TT072,L'AC@TT001                                              
         DCDDL AC#TT075,L'AC@TT001                                              
         DCDDL AC#TT079,L'AC@TT001                                              
         DCDDL AC#TT081,L'AC@TT001                                              
         DCDDL AC#TT096,L'AC@TT001                                              
         DCDDL AC#TT098,L'AC@TT001                                              
         DCDDL AC#TT099,L'AC@TT001                                              
         DCDDL AC#UNKWN,L'AC@TT001                                              
         DC    AL1(0)                                                           
         EJECT                                                                  
***********************************************************************         
* TABLE OF BATCH TYPES (INFORMATION EXTRACTED FROM TYPTAB IN ACBAT00) *         
***********************************************************************         
*                                                                               
DEFRTYP  DC    AL1(0,0)            DEFAULT FOR UNDEFINED RCVRY BATCHES          
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@UNKWN-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
TYPTAB   DS    0H                                                               
         DC    AL1(BT01,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIORDS+TYPIPRDQ)                                           
         DC    AL2(AC@TT001-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT02,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT002-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT03,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIORDS+TYPIPRDQ)                                           
         DC    AL2(AC@TT003-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT04,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT004-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT05,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT005-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT06,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIPRDQ)                                                    
         DC    AL2(AC@TT006-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT07,0)         BILL                                         
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPICONV+TYPIPRDQ)                                           
         DC    AL2(AC@TT007-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT08,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPINOLD+TYPIPRDQ)                                           
         DC    AL2(AC@TT008-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT09,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT009-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT10,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIORDS)                                                    
         DC    AL2(AC@TT010-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT11,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT011-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT12,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT012-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT13,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT013-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT14,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIPRDQ)                                                    
         DC    AL2(AC@TT014-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT15,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIPRDQ)                                                    
         DC    AL2(AC@TT015-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT16,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT016-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT17,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIXLOK)                                                    
         DC    AL2(AC@TT017-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT18,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT018-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT19,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT019-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT20,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT020-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT21,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIORDS)                                                    
         DC    AL2(AC@TT021-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT22,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIORDS)                                                    
         DC    AL2(AC@TT022-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT23,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT023-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT24,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT024-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT25,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT025-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT26,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT026-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT27,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIXLOK)                                                    
         DC    AL2(AC@TT027-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT28,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIXLOK)                                                    
         DC    AL2(AC@TT028-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT29,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIXLOK)                                                    
         DC    AL2(AC@TT029-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT30,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT030-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT31,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT031-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT32,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT032-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT33,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT033-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT34,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIPRDQ)                                                    
         DC    AL2(AC@TT034-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT35,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT035-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT36,0)                                                      
         DC    AL1(CTRYGER)                                                     
         DC    AL1(TYPIADVP)                                                    
         DC    AL2(AC@TT036-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT36,0)                                                      
         DC    AL1(CTRYNOT+CTRYGER)                                             
         DC    AL1(0)                                                           
         DC    AL2(AC@TT036-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT37,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT037-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT38,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT038-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT39,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIACRL)                                                    
         DC    AL2(AC@TT039-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT40,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIACRV)                                                    
         DC    AL2(AC@TT040-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT41,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT041-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT42,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT042-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT43,BT44)                                                   
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIACRL)                                                    
         DC    AL2(AC@TT043-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT44,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIACRV)                                                    
         DC    AL2(AC@TT044-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT45,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT045-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT46,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIPRDQ)                                                    
         DC    AL2(AC@TT046-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT47,0)                                                      
         DC    AL1(CTRYNOT+CTRYUSA)                                             
         DC    AL1(0)                                                           
         DC    AL2(AC@TT047-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT47,0)                                                      
         DC    AL1(CTRYUSA)                                                     
         DC    AL1(TYPIORDS+TYPIPRDQ)                                           
         DC    AL2(AC@TT047-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT48,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPICONV+TYPIPRDQ)                                           
         DC    AL2(AC@TT048-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
*        DC    AL1(BT49,0)                                                      
*        DC    AL1(CTRYNOT+CTRYUSA)                                             
*        DC    AL1(0)                                                           
*        DC    AL2(AC@TT049-DSLC2)                                              
*        DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT49,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPINOLD+TYPIPRDQ)                                           
         DC    AL2(AC@TT049-DSLC2)                                              
         DC    AL2(AC@TMSHR-DSUC)                                               
*                                                                               
         DC    AL1(BT50,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT050-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT51,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT051-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT52,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT052-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT53,BT54)                                                   
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIACRL)                                                    
         DC    AL2(AC@TT053-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT54,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIACRV)                                                    
         DC    AL2(AC@TT054-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT55,BT56)                                                   
         DC    AL1(CTRYNOT+CTRYUSA)                                             
         DC    AL1(TYPIACRL)                                                    
         DC    AL2(AC@TT055-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT55,BT56)                                                   
         DC    AL1(CTRYUSA)                                                     
         DC    AL1(TYPIACRL+TYPINOLD)                                           
         DC    AL2(AC@TT055-DSLC2)                                              
         DC    AL2(AC@R8ACR-DSUC)                                               
*                                                                               
         DC    AL1(BT56,0)                                                      
         DC    AL1(CTRYNOT+CTRYUSA)                                             
         DC    AL1(TYPIACRV)                                                    
         DC    AL2(AC@TT056-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT56,0)                                                      
         DC    AL1(CTRYUSA)                                                     
         DC    AL1(TYPIACRV)                                                    
         DC    AL2(AC@TT056-DSLC2)                                              
         DC    AL2(AC@R8ARV-DSUC)                                               
*                                                                               
         DC    AL1(BT57,0)         BILL WRITE OFF                               
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPICONV+TYPIPRDQ)                                           
         DC    AL2(AC@TT057-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT58,0)         =CLOSE                                       
         DC    AL1(CTRYNOT+CTRYUSA)                                             
         DC    AL1(0)                                                           
         DC    AL2(AC@TT058-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT58,0)         INTERAGENCY POSTINGS                         
         DC    AL1(CTRYUSA)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT058-DSLC2)                                              
         DC    AL2(AC@AUPOS-DSUC)                                               
*                                                                               
         DC    AL1(BT59,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT059-DSLC2)                                              
         DC    AL2(0)                                                           
*&&US                                                                           
         DC    AL1(BT60,0)                                                      
         DC    AL1(CTRYUSA)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT060-DSLC2)                                              
         DC    AL2(0)                                                           
*&&                                                                             
         DC    AL1(BT61,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIPRDQ)                                                    
         DC    AL2(AC@TT061-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT62,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(TYPIPRDQ)                                                    
         DC    AL2(AC@TT062-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT64,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT064-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT70,0)                                                      
         DC    AL1(CTRYNOT+CTRYUSA)                                             
         DC    AL1(TYPIORDS)                                                    
         DC    AL2(AC@TT070-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT71,0)                                                      
         DC    AL1(CTRYNOT+CTRYUSA)                                             
         DC    AL1(TYPIORDS)                                                    
         DC    AL2(AC@TT071-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT72,0)                                                      
         DC    AL1(CTRYNOT+CTRYUSA)                                             
         DC    AL1(TYPIORDS)                                                    
         DC    AL2(AC@TT072-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT75,0)                                                      
         DC    AL1(CTRYNOT+CTRYUSA)                                             
         DC    AL1(0)                                                           
         DC    AL2(AC@TT075-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT79,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT079-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT81,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT081-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT96,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT096-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(BT98,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT098-DSLC2)                                              
         DC    AL2(AC@MEDBT-DSUC)                                               
*                                                                               
         DC    AL1(BT99,0)                                                      
         DC    AL1(CTRYALL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(AC@TT099-DSLC2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(0)              EOT                                          
         EJECT                                                                  
***********************************************************************         
* TABLE OF RECOVERY RECORD PRGNOS FOR WHICH BATCH RECS WILL BE BUILT  *         
***********************************************************************         
*                                                                               
RPRGTAB  DS    0X                                                               
         DC    AL1(RCVPDEBQ)                                                    
         DC    AL1(0)                                                           
         DC    AL1(BT30)                                                        
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(RCVPCAPQ)                                                    
         DC    AL1(RPRGNOSQ+RPRGHRSQ)                                           
         DC    AL1(BT49)                                                        
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(RCVPBRAQ)       BRANDOCEAN                                   
         DC    AL1(RPRGNOSQ)                                                    
         DC    AL1(BT08)                                                        
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(RCVPBRAQ)       BRANDOCEAN                                   
         DC    AL1(RPRGNOSQ+RPRGHRSQ)                                           
         DC    AL1(BT49)                                                        
         DC    AL1(0)                                                           
*&&US                                                                           
         DC    AL1(RCVPINTQ)                                                    
         DC    AL1(RPRGISQQ+RPRGNOSQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*&&                                                                             
         DC    AL1(RCVPCLOQ)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(RCVPMRKQ)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TRSSMRK1)                                                    
*                                                                               
         DC    AL1(RCVPMRKQ)                                                    
         DC    AL1(RPRGIMBQ)                                                    
         DC    AL1(BT37)                                                        
         DC    AL1(TRSSMRK2)                                                    
*                                                                               
         DC    AL1(RCVPMRKQ)                                                    
         DC    AL1(RPRGIMBQ)                                                    
         DC    AL1(BT64)                                                        
         DC    AL1(TRSSMRK3)                                                    
*                                                                               
         DC    AL1(RCVPWFMQ)                                                    
         DC    AL1(RPRGISQQ+RPRGIMBQ+RPRGBRFQ)                                  
         DC    AL1(BT99)                                                        
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(RCVPNBLQ)       NEW BILLING                                  
         DC    AL1(RPRGXBTY)                                                    
         DC    AL1(BT99)                                                        
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(0)              EOT                                          
         EJECT                                                                  
***********************************************************************         
* TABLE OF WORKER FILE TYPES FOR WHICH BATCH RECS WILL BE BUILT       *         
***********************************************************************         
*                                                                               
WKRTAB   DS    0X                                                               
*                                                                               
         DC    CL3'AR8'                                                         
         DC    AL1(WKRTACR)                                                     
         DC    AL1(BT55)                                                        
*                                                                               
         DC    AL1(0)              EOT                                          
         EJECT                                                                  
***********************************************************************         
* RECORD TOTALS TABLE                                                 *         
***********************************************************************         
*                                                                               
TOTTAB   DS    0X                  ** RECORD TOTALS TABLE **                    
         DC    AL2(TRNACCA-TRNBLKD),AL1(TOTTITRN+TOTTIADD)                      
         DC    CL30'Accounts added'                                             
         DC    AL2(TRNACCU-TRNBLKD),AL1(TOTTITRN+TOTTICHA)                      
         DC    CL30'Accounts changed'                                           
         DC    AL2(TRNANCA-TRNBLKD),AL1(TOTTITRN)                               
         DC    CL30'Name change pointers added'                                 
         DC    AL2(TRNOAPA-TRNBLKD),AL1(TOTTITRN)                               
         DC    CL30'Office/Acct pointers added'                                 
         DC    AL2(TRNOFAA-TRNBLKD),AL1(TOTTITRN)                               
         DC    CL30'Office/Acct pointers changed'                               
         DC    AL2(TRNOFAU-TRNBLKD),AL1(TOTTITRN+TOTTICHA)                      
         DC    CL30'Office headers added'                                       
         DC    AL2(TRNOFAU-TRNBLKD),AL1(TOTTITRN)                               
         DC    CL30'Office headers changed'                                     
         DC    AL2(TRNBUKA-TRNBLKD),AL1(TOTTITRN+TOTTIADD)                      
         DC    CL30'Contra buckets added'                                       
         DC    AL2(TRNBUKU-TRNBLKD),AL1(TOTTITRN+TOTTICHA)                      
         DC    CL30'Contra buckets changed'                                     
         DC    AL2(TRNCACA-TRNBLKD),AL1(TOTTITRN+TOTTIADD)                      
         DC    CL30'Contra headers added'                                       
         DC    AL2(TRNCACU-TRNBLKD),AL1(TOTTITRN+TOTTICHA)                      
         DC    CL30'Contra headers changed'                                     
         DC    AL2(TRNTRNA-TRNBLKD),AL1(TOTTITRN+TOTTIADD)                      
         DC    CL30'Transactions added'                                         
         DC    AL2(TRNTRNU-TRNBLKD),AL1(TOTTITRN+TOTTICHA)                      
         DC    CL30'Transactions reversed'                                      
         DC    AL2(TRNTRNC-TRNBLKD),AL1(TOTTITRN+TOTTICHA)                      
         DC    CL30'Transactions matched'                                       
         DC    AL2(NBHDADD-WORKD),AL1(TOTTIADD)                                 
         DC    CL30'New Batch headers added'                                    
         DC    AL2(NBHDCHA-WORKD),AL1(TOTTICHA)                                 
         DC    CL30'New Batch headers changed'                                  
         DC    AL2(NBITADD-WORKD),AL1(TOTTIADD)                                 
         DC    CL30'New Batch items added'                                      
         DC    AL2(NBITCHA-WORKD),AL1(TOTTICHA)                                 
         DC    CL30'New Batch items changed'                                    
         DC    AL2(OBHDCHA-WORKD),AL1(TOTTICHA)                                 
         DC    CL30'Old Batch headers changed'                                  
         DC    AL2(IAGRCHA-WORKD),AL1(TOTTICHA)                                 
         DC    CL30'Interagency records changed'                                
         DC    AL2(FILECHA-WORKD),AL1(TOTTITOT)                                 
         DC    CL30'* Total records changed *'                                  
         DC    AL2(FILEADD-WORKD),AL1(TOTTITOT)                                 
         DC    CL30'* Total records added *'                                    
TOTTABX  DC    AL1(TOTTEOTQ)                                                    
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
*                                                                               
WORKD    DSECT                                                                  
PARM     DS    6F                                                               
PKWK     DS    0XL16                                                            
BGREGS   DS    4F                                                               
VGETDAY  DS    V                                                                
VHELLO   DS    V                                                                
VCALLOV  DS    V                                                                
VPADDLE  DS    V                                                                
*                                                                               
MYADDS   DS    0A                                                               
AIOARE1  DS    A                   THESE ARE MAPPED TO BGATYPES                 
AIO1     DS    A                   *                                            
AIOARE2  DS    A                   *                                            
AIO2     DS    A                   *                                            
AIOARE3  DS    A                   *                                            
AIO3     DS    A                   *                                            
AIOARE4  DS    A                   *                                            
AIO4     DS    A                   *                                            
AIOARE5  DS    A                   *                                            
AIO5     DS    A                   *                                            
*                                                                               
TEMP     DS    XL64                                                             
KEYTEMP  DS    XL64                                                             
ITEMEND  DS    A                                                                
ITEMSEQ  DS    H                                                                
ASKLSEQ  DS    H                                                                
SEQNUM   DS    H                                                                
GPFLAG   DS    XL1                                                              
PRESFLAG DS    XL1                                                              
GUFLAG   DS    XL1                                                              
IBKSEQ   DS    PL2                 INTAGY BATREF SEQUENCE                       
CTMON    DS    CL2                 CHARACTER YEAR/MONTH                         
TODAYCC  DS    XL2                 COMPRESSED COMPLEMENT                        
TODAYC   DS    XL2                 COMPRESSED                                   
TODAYRP  DS    XL3                 PWOS - REAL TODAY                            
TODAYP   DS    XL3                 PWOS - PRODUCTION TODAY                      
TODAYPCO DS    PL3                 PWOS COMPLEMENTED                            
TODAYE   DS    CL6                 EBCDIC YYMMDD                                
TODAYD   DS    CL8                 DISPLAY DDMMMYY,MMMDD/YY                     
BLODAT   DS    XL2                 TODAYC-31 FOR BATCH FILTERING                
BHIDAT   DS    XL2                 TODAYC+31 FOR BATCH FILTERING                
LKEY     DS    XL(L'ASKKEY)        SAVED SORT KEY - L'LONGEST SORT KEY          
STRNKEY  DS    XL(L'ASKKEY)        SAVED TRANS KEY FOR BATCH ITEM BUILD         
SWKIKEY  DS    XL(L'WKIKEY)        SAVED WORKER INDEX KEY                       
SBLDKEY  DS    XL(L'IOKEY)         SET BY - IBKBLD/NBKBLD/RBKBLD/WBKBLD         
ADDCREGS DS    16F                 SAVE AREA FOR ADDC0 ROUTINE                  
*                                                                               
*                                                                               
LDGAL1   DS    XL1                 SUPPLIER LEDGER LENGTHS                      
LDGAL2   DS    XL1                                                              
LDGAL3   DS    XL1                                                              
LDGAL4   DS    XL1                                                              
*                                                                               
UBINDS   DS    XL1                 INDICATORS                                   
UBIUPD   EQU   X'80'               ACTION IS UPDATE                             
UBIPPD   EQU   X'40'               PASSIVE DELETED (SEE MKOVUP)                 
UBIGIN   EQU   X'20'               PROCESSING GROUP INVOICE ELEMENT             
UBIDEL   EQU   X'08'               ACTION IS DELETE                             
UBWORK   DS    CL7                 PREVIOUS ORDER NUMBER                        
*                                                                               
SAVERF   DS    A                   SAVEAREA FOR RF                              
*                                                                               
BATCASH  DS    PL8                                                              
ITMCASH  DS    PL8                                                              
ITMREF   DS    CL6                                                              
REPPSTG  DS    XL2                 POSTING COUNT FOR REPORT                     
REPCMNT  DS    CL(L'PCMNT)         COMMENTS FOR REPORT                          
REPCPY   DS    CL46                CPY CODE & NAME FOR REPORT                   
REPUSR   DS    CL46                USER CODE & NAME FOR REPORT                  
TOFUIDDR DS    PL8                 TOTAL OFFLINE POSTING DRS FOR USER           
TOFUIDCR DS    PL8                 TOTAL OFFLINE POSTING CRS FOR USER           
*                                                                               
NBHDADD  DS    XL4                 BATCH HEADERS ADDED                          
NBHDCHA  DS    XL4                 BATCH HEADERS CHANGED                        
NBITADD  DS    XL4                 BATCH ITEMS ADDED                            
NBITCHA  DS    XL4                 BATCH ITEMS CHANGED                          
OBHDCHA  DS    XL4                 OLD BATCH HEADERS CHANGED                    
IAGRCHA  DS    XL4                 INTERAGENCY RECORDS CHANGED                  
FILEADD  DS    XL4                 FILE RECORDS ADDED                           
FILECHA  DS    XL4                 FILE RECORDS CHANGED                         
*                                                                               
CRE8BAT  DS    CL1                 Y=CREATE NON-INP BATS (RCFFPARM+0)           
RCVRY    EQU   C'R'                R=CREATE RECOVERY NON-INP BATS ONLY          
XRCVR    EQU   C'X'                X=CREATE ALL EXCEPT RECOVERY BATS            
CRE8WKR  DS    CL1                 Y=CREATE ADJ/K WKRFLS (RCFFPARM+1)           
FLTALFA  DS    CL2                 SPECIFIED AGY ALPHA   (RCFFPARM+2)           
FLTUSR   DS    XL2                 SPECIFIED USER-ID     (RCFFPARM+4)           
PROIND   DS    CL1                 PROCESS INDICATOR                            
PROIEBAT EQU   X'80'               END OF BATCHES ON FILE                       
PROIXCPY EQU   X'40'               SKIP ALL BATCHES UNTIL CPY CHANGES           
PROIXUSR EQU   X'20'               SKIP ALL BATCHES UNTIL USER CHANGES          
PROIOTHR EQU   X'10'               PROCESSING NON-POSTMAN POSTINGS              
SORTIND  DS    XL1                 SORT INIT'D: 1/2/3=SORT1/2/3; 0=NOT          
SENO     DS    XL1                 ACCOUNT SYSTEM SE NUMBER                     
SRCIND   DS    XL1                 PROCESSING SOURCE FOR REPORT HEADS:          
RECOVR   EQU   1                   RECOVERY FILE                                
WRKFIL   EQU   2                   WORKER FILE                                  
OLDBAT   EQU   3                   OLD BATCHES ON FILE                          
INTAGY   EQU   4                   INTERAGENCY RECORDS                          
NUBATS   EQU   5                   POSTMAN-STYLE BACTHES                        
UOCBIOSN DS    XL1                 CURRENT BIOEL SEQUENCE NUMBER                
UOCBIONM DS    XL2                 CURRENT BIOEL ITEM NUMBER                    
*                                                                               
SVFRCPY  DS    XL1                                                              
SVUID    DS    XL2                 SAVED USER ID FROM LAST STCELD               
SVPERS   DS    XL2                 SAVED PERSON FROM LAST STCELD                
SVPGAPST DS    XL1                 SAVED PREVIOUS GAP STATUS                    
         DS    XL1                                                              
SVSUPC   DS    CL14                SUPPLIER CODE                                
SVACKQ   DS    CL1                 GAP ACKNOWLEDGE QUERY ACTION                 
*                                                                               
UBREFDT  DS    XL2                 COMPRESSED DATE                              
UBREFDS  DS    XL3                 SAVED DATE                                   
UBTRXDA  DS    XL4                 TRNRECD D/A                                  
UBREFNO  DS    CL20                                                             
UOSTAT   DS    XL1                 ORDER STATUS BYTE 1                          
UONSTAT  DS    XL1                 ORDER NEW STATUS BYTE 1                      
UOSTA2   DS    XL1                 ORDER STATUS BYTE 2                          
UOWCCNT  DS    XL1                 WORKCODE COUNT                               
UOORDNUM DS    CL6                 ORDER NUMBER                                 
ELEMENT  DS    XL255               ELEMENT                                      
*                                                                               
STYPNTRY DS    0XL(TYPTABL)        SAVED TYPTAB ENTRY                           
STYPNUM  DS    XL1                                                              
STYPARN  DS    XL1                                                              
STYPCTRY DS    XL1                                                              
STYPIND1 DS    XL1                                                              
STYPBTDD DS    AL2                                                              
STYPBNDD DS    AL2                                                              
*                                                                               
SBATCPY  DS    XL1                 SAVED BATCH COMPANY BYTE                     
SBATUSR  DS    XL2                 SAVED BATCH USER-ID                          
SBATKEY  DS    XL(L'IOKEY)         SAVED BATCH HDR KEY                          
SBATPP   DS    XL(L'IOKEY)         SAVED BATCH HDR PASSIVE POINTER              
SBATNAM  DS    CL(L'BHDNAME)       SAVED BATCH NAME                             
SBATITM  DS    XL2                 SAVED BATCH ITEM COUNT                       
SBATST1  DS    XL1                 SAVED BATCH HEADER STATUS                    
SCHQNAR  DS    XL(L'TRNNARR)       SAVED CHEQUE NARRATIVE                       
SCHQDAT  DS    CL8                 SAVED CHEQUE DATE                            
SCHQDEP  DS    CL8                 SAVED CHQ DEPOSIT DATE                       
SLUID    DS    CL8                 SAVED LUID FROM RECOVERY BATCHES             
*                                                                               
SCPYCPY  DS    XL1                 SAVED COMPANY CODE                           
SCPYBANK DS    XL(L'CPYBANK)       SAVED CPY UNIT/LEDGER FOR BANK               
SCPYPROD DS    XL(L'CPYPROD)       SAVED CPY UNIT/LEDGER FOR PRODUCTION         
SCPYSTA  DS    0X                                                               
SCPYST1  DS    XL1                 CPY STATUS BYTE 1                            
SCPYST2  DS    XL1                 CPY STATUS BYTE 2                            
SCPYST3  DS    XL1                 CPY STATUS BYTE 3                            
SCPYST4  DS    XL1                 CPY STATUS BYTE 4                            
SCPYST5  DS    XL1                 CPY STATUS BYTE 5                            
SCPYST7  DS    XL1                 CPY STATUS BYTE 7                            
SCPYALF  DS    CL2                 SAVED CPY ALPHA ID                           
SCPYSTAL EQU   *-SCPYSTA                                                        
SCPYNAM  DS    CL36                SAVED CPY NAME                               
*                                  A/CS AND NAMES FOR INTAGY POSTINGS           
SIACC    DS    CL(L'ACTKCULA)                                                   
SIACCNM  DS    CL(L'NAMEREC)                                                    
ACC1C    DS    CL(L'ACTKCULA)                                                   
ACC1CNM  DS    CL(L'NAMEREC)                                                    
ACC11    DS    CL(L'ACTKCULA)                                                   
ACC11NM  DS    CL(L'NAMEREC)                                                    
ACC12    DS    CL(L'ACTKCULA)                                                   
ACC12NM  DS    CL(L'NAMEREC)                                                    
SRACCNM  DS    CL(L'NAMEREC)                                                    
ACOFFC   DS    CL2                                                              
MIDESC   DS    CL(L'MDIDESC)       MEDIA INTERFACE DESCRIPTION                  
PRODNAM  DS    CL(L'NAMEREC)                                                    
IAESTDE  DS    CL(L'IPRDES)        ESTIMATE DESCRIPTION                         
IDUEDYS  DS    XL1                                                              
IACPFLG  DS    XL1                 NO=DON'T MAKE COSTING POSTINGS               
*                                                                               
ACRMOSP  DS    PL2                 PACKED MOS WORK AREA                         
ACRMOSC  DS    CL2                 CHARACTER MOS WORK AREA                      
ACRINDS  DS    XL1                                                              
ACRIBHD  EQU   X'80'               BATCH HDR EXISTS WITH DELETE STATUS          
ACRIPPD  EQU   X'40'               PASSIVE PTR EXISTS WITH DELETE STS           
ACRIODA  DS    XL4                 D/A OF EXISTING BATCH HDR                    
ACRITM   DS    XL2                 ACCRUAL REVERSAL ITEM COUNT                  
*                                                                               
FBATTYP  DS    XL1                 FLTBAT BATCH CLASSIFICATION                  
FBATINUP EQU   1                   INSTANT UPDATE BATCH                         
FBATONUP EQU   2                   ONLINE UPDATE BATCH                          
FBATOVUP EQU   3                   OVERNIGHT UPDATE BATCH                       
FBATDRLI EQU   4                   DRAFT BATCH TO BE MADE LIVE                  
FBATDELT EQU   5                   DELETE BATCH                                 
FBATWARN EQU   6                   SET WARNING ON BATCH                         
FBATDULI EQU   7                   DRAFT BATCH DUE TO GO LIVE SOON              
FBATDUWA EQU   8                   DRAFT BATCH DUE, TO BE WARNED                
FBATIND  DS    XL1                 BATCH INDICATOR                              
FBATMOAL EQU   X'80'               BATCHING MOA LOCKED                          
FBATUNAP EQU   X'40'               BATCH AWAITING APPROVAL                      
FBATOPEN EQU   X'20'               BATCH IS OPEN                                
FBATSAVD EQU   X'10'               BATCH IS SAVED                               
FBATINOD EQU   X'08'               BATCH DIRECTORY PTR MISSING                  
FBATIDUP EQU   X'04'               PRE-EXISTING PASSIVE PTR ON FILE             
FBATIBER EQU   X'02'               BATCH IN ERROR - INCOMPLETE/CORRUPT          
FBATIORD EQU   X'01'               OPEN/SAVD BATCH HAS ORDER, DELETE IT         
FBATDAYS DS    XL1                 DAYS REMAINING UNTIL LIVE/DELETION           
*                                                                               
AIADDRS  DS    0A                  ** ELEMENT ADDRESSES **                      
AIAFRST  DS    A                   A(FIRST TRANSACTION EXTRA ELEMENT)           
AIALAST  DS    A                   A(LAST TRANSACTION EXTRA ELEMENT)            
AIADDRSL EQU   *-AIADDRS                                                        
AIFLAG   DS    XL1                                                              
AIFITDEL EQU   X'80'               DELETED ITEM EXISTS                          
AIACTI   DS    XL1                 INDICATOR FOR AIACTS                         
AIACTISE EQU   X'80'               SE ACCOUNT PRESENT IN AIACTS                 
AIACTISB EQU   X'40'               SB ACCOUNT PRESENT IN AIACTS                 
AIACTSN  DS    XL1                 NUMBER OF ENTRIES IN AIACTS                  
AIACTSL  EQU   L'APENSTAT+L'APENACT                                             
AIACTSM  EQU   32                  MAXIMUM NUMBER OF AIACTS ENTRIES             
AITRNSM  EQU   20                  MAXIMUM NUMBER OF AITRNS ENTRIES             
AITRNSN  DS    XL1                 NUMBER OF ENTRIES IN AITRNS                  
AITRNS   DS    (AITRNSM)XL(L'TRNKEY)                                            
*                                                                               
PRFALF   DS    CL2                 AGENCY ALPHA                                 
PRFUSR   DS    XL2                 USER-ID                                      
AGYPROFS DS    XL16                AGENCY PROFILES                              
USRPROFS DS    0XL16               USER PROFILES                                
PRFBDUED DS    XL1                 REPORT BATCHES DUE IN NEXT N DAYS            
PRFBXCPD DS    XL1                 REPT EXCPTNS UP TO N DAYS AFTER EFDT         
PRFBOPND DS    XL1                 DON'T DELETE OPEN BATCHES FOR N DAYS         
PRFBSAVD DS    XL1                   "     "   SAVED    "     "  "  "           
PRFBCLSD DS    XL1                   "     "   CLOSED   "     "  "  "           
         DS    XL11                                                             
*                                                                               
ROUTS    DS    0A                                                               
AACRREV  DS    A                                                                
AADDITE  DS    A                                                                
AADDMON  DS    A                                                                
ABATBLD  DS    A                                                                
ABLDACR  DS    A                                                                
ABLDWDJ  DS    A                                                                
ABLDWDK  DS    A                                                                
ADELBAT  DS    A                                                                
AFLTBAT  DS    A                                                                
AGETCPY  DS    A                                                                
AGETTYP  DS    A                                                                
AGETUSR  DS    A                                                                
AHDRADD  DS    A                                                                
AIAGBAT  DS    A                                                                
AIBKBLD  DS    A                                                                
AIGETDU  DS    A                                                                
AITRNAD  DS    A                                                                
AIO      DS    A                                                                
AIOTRACE DS    A                                                                
AITMADD  DS    A                                                                
AITMBLD  DS    A                                                                
AIVALAC  DS    A                                                                
AMRKBAT  DS    A                                                                
ANBKBLD  DS    A                                                                
APACTST  DS    A                                                                
ARBKBLD  DS    A                                                                
ARDPROF  DS    A                                                                
AREPBAT  DS    A                                                                
AREPSUM  DS    A                                                                
ASORTPT  DS    A                                                                
ATABMNT  DS    A                                                                
ATRNBLD  DS    A                                                                
ATSTBMO  DS    A                                                                
AUPDBAT  DS    A                                                                
AUPDHDR  DS    A                                                                
AWBKBLD  DS    A                                                                
AORDAUD  DS    A                                                                
AGAPEXP  DS    A                                                                
ASUPGAP  DS    A                                                                
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
*                                                                               
AIOBUFF  DS    A                                                                
AIOSAVE  DS    A                                                                
         EJECT                                                                  
***********************************************************************         
* SORT RECORD DEFINITIONS                                             *         
***********************************************************************         
*                                                                               
S1REC    DS    0X                  SORT REC FOR RECOVERY BATCHES                
S1KEY    DS    0X                                                               
S1KCPY   DS    XL1                                                              
S1KUID   DS    XL2                                                              
S1KTYP   DS    XL1                                                              
S1KSIN   DS    XL4                 SYSTEM INPUT NUMBER                          
         ORG S1KSIN                                                             
S1KBRF   DS    CL4                 BATREF FOR INTERAGENCY TY30                  
S1KMOS   DS    CL2                                                              
S1KPID   DS    XL2                 PERSON ID NUMBER                             
S1KSL    EQU   *-S1KEY             SIGNIFICANT L'KEY FOR BATCH CREATE           
S1KDAT   DS    CL3                                                              
S1KREF   DS    CL4                                                              
S1KSEQ   DS    XL2                                                              
S1KINC   DS    XL4                 LOCATION OF SIN IF RPRGNOSQ SET              
S1KIND   DS    XL1                                                              
S1KINREV EQU   X'80'               BATCH CANNOT BE REVERSED                     
S1KEYL   EQU   *-S1KEY                                                          
S1DTKEY  DS    XL(L'TRNKEY)                                                     
S1DCASH  DS    PL8                                                              
S1DSTAT  DS    XL1                                                              
S1DLUID  DS    CL8                                                              
S1DIND   DS    XL1                 SORT RECORD INDICATOR                        
S1DIAFC  EQU   X'80'               AFCEL PRESENT                                
S1DAFCD  DS    XL(AFCLNQ)          AFCEL FROM POSTING                           
         DS    XL1                 SPARE                                        
S1RECL   EQU   *-S1REC                                                          
         ORG   S1REC                                                            
S2REC    DS    0X                  SORT REC DEFINITION FOR INTAGY               
S2KEY    DS    0X                                                               
S2KCPY   DS    XL1                                                              
S2KUID   DS    XL2                                                              
S2KIAK   DS    0XL(INTKSUBL)       INTAGY X'2D03' RECORD KEY                    
S2KCULA  DS    XL(L'INTKCULA)                                                   
S2KCLT   DS    XL(L'INTKCLT)                                                    
S2KPRD   DS    XL(L'INTKPRD)                                                    
S2KMED   DS    XL(L'INTKMED)                                                    
S2KEST   DS    XL(L'INTKEST)                                                    
S2KDATP  DS    PL3                 PWOS POSTING DATE                            
S2KSL    EQU   *-S2KEY             SIGNIFICANT L'KEY FOR BATCH CREATE           
S2KMOS   DS    PL(L'IESMTH)        ADVERTISING MONTH                            
S2DATA   DS    0X                                                               
S2DDATC  DS    XL2                 COMPRESSED POSTING DATE                      
S2DDA    DS    XL4                 INTAGY RECORD D/A                            
S2DSTA   DS    XL1                 INTAGY RECORD STATUS                         
S2RECL   EQU   *-S2REC                                                          
         ORG   S1REC                                                            
S3REC    DS    0X                  SORT REC DEFINITION FOR A08 POSTINGS         
S3KEY    DS    XL(L'ASKKEY)                                                     
S3RECL   EQU   *-S3REC                                                          
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* WORKER INDEX RECORD DEFINITION                                      *         
***********************************************************************         
*                                                                               
WKID     DS    0XL16                                                            
WKIKEY   DS    0XL8                WORKER KEY                                   
WKIUSER  DS    XL2                 USER-ID NUMBER                               
WKISPRG  DS    0CL3                                                             
WKISYS   DS    CL1                 SYSTEM CODE                                  
WKISACC  EQU   C'A'                ACCOUNTING                                   
WKISFEE  EQU   C'F'                FEE                                          
WKISMED  EQU   C'M'                MEDIA                                        
WKISPRT  EQU   C'P'                SPOT                                         
WKISSPT  EQU   C'S'                PRINT                                        
WKISTLT  EQU   C'T'                TALENT                                       
WKIPRG   DS    CL2                 PROGRAM ID                                   
WKISUB   DS    XL1                 SUB-PROGRAM/LEDGER CODE                      
WKIDAY   DS    PL1                 DAY ADDED (PWOS DD)                          
WKITYPE  DS    CL1                 FILE TYPE                                    
WKITPOST EQU   C'P'                POSTING FILE                                 
WKITODDS EQU   C'O'                ACODDS FILE                                  
WKITCHQS EQU   C'C'                CHEQUE FILE                                  
         DS    XL2                 N/D                                          
WKISEQN  DS    XL2                 FILE SEQUENCE NUMBER                         
WKISTAT  DS    XL1                 WORKER STATUS                                
WKISACTV EQU   X'80'               STATUS ACTIVE                                
WKISHOLD EQU   X'40'               STATUS HOLD                                  
WKISDELT EQU   X'20'               STATUS DELETE                                
WKISKEEP EQU   X'08'               STATUS KEEP                                  
         DS    XL16                N/D                                          
         EJECT                                                                  
DSUC     DS    0X                                                               
AC@ANY   DS    CL3                 DUMMY ENTRY AT ZERO DISP. FROM DSUC          
AC@AUPOS DS    CL(L'BHDNAME)                                                    
UC@CLIBG DS    CL15                                                             
UC@CLIWB DS    CL15                                                             
UC@MANBL DS    CL15                                                             
AC@MEDBT DS    CL(L'BHDNAME)                                                    
AC@WFMPS DS    CL(L'BHDNAME)                                                    
AC@TMSHR DS    CL(L'BHDNAME)                                                    
AC@R8ACR DS    CL(L'BHDNAME)                                                    
AC@R8ARV DS    CL(L'BHDNAME)                                                    
*                                                                               
DSLC1    DS    0X                                                               
AC@ACRL  DS    CL(L'PCMNT)                                                      
AC@ACRR  DS    CL(L'PCMNT)                                                      
AC@BAT   DS    CL6                                                              
AC@BATC  DS    CL(L'PCMNT)                                                      
AC@CRP   DS    CL8                                                              
AC@DAYS  DS    CL4                                                              
AC@DRDEL DS    CL15                                                             
AC@DRMLI DS    CL15                                                             
AC@DRWAR DS    CL15                                                             
AC@DULIS DS    CL15                                                             
AC@DULIW DS    CL15                                                             
AC@INSUP DS    CL15                                                             
AC@MOSLK DS    CL8                                                              
AC@ONLUP DS    CL15                                                             
AC@OPEN2 DS    CL8                                                              
AC@OVNUP DS    CL15                                                             
AC@PGA08 DS    CL30                                                             
AC@PGA09 DS    CL30                                                             
AC@PQFID DS    CL8                                                              
AC@RCVB  DS    CL(L'PCMNT)                                                      
AC@SAVED DS    CL8                                                              
AC@TPSTS DS    CL20                                                             
AC@UAPR  DS    CL8                                                              
AC@USRID DS    CL30                                                             
AC@WKRB  DS    CL(L'PCMNT)                                                      
         EJECT                                                                  
***********************************************************************         
* I/O CONTROLLER WORK AREA                                            *         
***********************************************************************         
*                                                                               
         DS    0D                                                               
IOCTRL   DS    0F                  CONTROL BYTE FOR IO CALLS                    
         DS    H                   N/D                                          
IOCTFILE DS    XL1                 FILE NUMBER                                  
IOCTCOMM DS    XL1                 COMMAND NUMBER                               
IOCTIND  DS    XL1                 I/O INDICATORS                               
IOCTIUPD EQU   X'80'               UPDATIVE COMMAND                             
IOCTISAD EQU   X'40'               USE DISK ADDRESS IN IOSAVE                   
IOCTIAWC EQU   X'20'               O'RIDE UPD=N ON THIS CMND (TESTING)          
IODA     DS    XL4                 DISK ADDRESS                                 
IOWORK   DS    XL96                DATAMGR D/A WORK AREA                        
IODAOVER DS    XL4                 NON-ZERO=OVERRIDE D/A                        
IOFILE   DS    CL7                 IO FILE NAME                                 
IOCOMM   DS    CL7                 IO COMMAND                                   
IOQUAL   DS    XL1                 QUALIFIER FOR DATAMGR IO                     
IOQLOCK  EQU   X'80'               READ FOR UPDATE                              
IOQDELT  EQU   X'08'               READ DELETES                                 
IOERR    DS    XL1                 ERROR RETURN BYTE                            
IOEEOF   EQU   X'80'               END OF FILE                                  
IOEDSK   EQU   X'40'               NON-RECOVERABLE DISK ERROR                   
IOEDUP   EQU   X'20'               DUPLICATE KEY ON ADD                         
IOERNF   EQU   X'10'               RECORD NOT FOUND                             
IOEDEL   EQU   X'02'               RECORD DELETED                               
IOEALL   EQU   X'FF'               ALL ERROR BITS (CONTROLLER USE ONLY)         
IOSAVKEY DS    XL(L'ACTKEY)        SAVED VERSION OF KEY                         
IOKEY    DS    XL64                KEY FOR I/O                                  
IOKEYSAV DS    XL64                SAVED VERSION OF KEY                         
KEYSAFE  DS    XL64                A SAFE SAVED VERSION OF KEY!                 
AIACTS   DS    (AIACTSM)XL(AIACTSL)                                             
         EJECT                                                                  
***********************************************************************         
* I/O CONTROLLER EQUATES                                              *         
***********************************************************************         
*                                                                               
IOFILES  EQU   X'0700'             RESERVED FOR CONTROLLER USE                  
IOACCDIR EQU   X'0100'             I/O TO ACCOUNT DIRECTORY                     
IOACCMST EQU   X'0200'             I/O TO ACCOUNT MASTER FILE                   
IOWORKER EQU   X'0300'             I/O TO WORKER SYSTEM                         
IOCONFIL EQU   X'0400'             I/O TO CONTROL FILE                          
IOACCDAY EQU   X'0500'             I/O TO ACCDAY FILE                           
IOACCIS  EQU   X'01'               INDEX SEQUENTIAL IO FILE                     
IOACCDA  EQU   X'02'               DIRECT ACCESS IO FILE                        
IOWKWRK  EQU   X'03'               WORKER SYSTEM FILE                           
IOCTFIL  EQU   X'04'               CONTROL FILE                                 
IOACCDY  EQU   X'05'               ACCDAY FILE                                  
*                                                                               
*                                  ** I/O AREA EQUATES **                       
IOAREALL EQU   X'00F0'             RESERVED FOR CONTROLLER USE                  
IO1      EQU   X'0010'             I/O TO AREA 1                                
IO2      EQU   X'0020'             I/O TO AREA 2                                
IO3      EQU   X'0030'             I/O TO AREA 3                                
IO4      EQU   X'0040'             I/O TO AREA 4                                
IO5      EQU   X'0050'             I/O TO AREA 5                                
*                                                                               
*                                  ** I/O COMMAND EQUATES **                    
IOCMNDS  EQU   X'000F'             RESERVED FOR CONTROLLER USE                  
*                                                                               
IORDEL   EQU   X'0800'             READ DELETED RECORDS                         
IORDL    EQU   X'08'                                                            
*                                  I/S COMMANDS                                 
IOHIGH   EQU   X'0001'             DMRDHI                                       
IOHI     EQU   IOHIGH              DMRDHI                                       
IOHID    EQU   IOHI+IORDEL         DMRDHI (FOR DELETES)                         
IOREAD   EQU   X'0002'             DMREAD                                       
IORD     EQU   IOREAD              DMREAD                                       
IORDD    EQU   IORD+IORDEL         DMREAD (FOR DELETES)                         
IOSEQ    EQU   X'0003'             DMRSEQ                                       
IOSEQD   EQU   IOSEQ+IORDEL        DMRSEQ (FOR DELETES)                         
IOADD    EQU   X'0004'             DMADD                                        
IOWRITE  EQU   X'0005'             DMWRT                                        
*                                  D/A COMMANDS                                 
IOGETREC EQU   X'0001'             GETREC                                       
IOGET    EQU   IOGETREC            GETREC                                       
IOPUTREC EQU   X'0002'             PUTREC                                       
IOPUT    EQU   IOPUTREC            PUTREC                                       
IOADDREC EQU   X'0003'             ADDREC                                       
IOADFR   EQU   X'0004'             ADFREC (MOVE FROM ACCARC TO ACCMST)          
IODMDIR  EQU   X'0005'             DMRDIR                                       
*                                  WORKER COMMANDS                              
IOWKNDX  EQU   X'0001'             INDEX READ                                   
IOWKRAN  EQU   X'0002'             RANDOM READ                                  
IOWKRD   EQU   X'0003'             DIRECT READ                                  
IOWKSEQ  EQU   X'0004'             SEQUENTIAL READ                              
IOWKADD  EQU   X'0005'             ADDREC                                       
IOWKWRT  EQU   X'0006'             WRITE                                        
IOWKEEP  EQU   X'0007'             KEEP                                         
IOWKCLS  EQU   X'0008'             CLOSE                                        
         EJECT                                                                  
DSLC2    DS    0X                  BATCH TYPE NAMES                             
AC@TT001 DS    CL15                                                             
AC@TT002 DS    CL15                                                             
AC@TT003 DS    CL15                                                             
AC@TT004 DS    CL15                                                             
AC@TT005 DS    CL15                                                             
AC@TT006 DS    CL15                                                             
AC@TT007 DS    CL15                                                             
AC@TT008 DS    CL15                                                             
AC@TT009 DS    CL15                                                             
AC@TT010 DS    CL15                                                             
AC@TT011 DS    CL15                                                             
AC@TT012 DS    CL15                                                             
AC@TT013 DS    CL15                                                             
AC@TT014 DS    CL15                                                             
AC@TT015 DS    CL15                                                             
AC@TT016 DS    CL15                                                             
AC@TT017 DS    CL15                                                             
AC@TT018 DS    CL15                                                             
AC@TT019 DS    CL15                                                             
AC@TT020 DS    CL15                                                             
AC@TT021 DS    CL15                                                             
AC@TT022 DS    CL15                                                             
AC@TT023 DS    CL15                                                             
AC@TT024 DS    CL15                                                             
AC@TT025 DS    CL15                                                             
AC@TT026 DS    CL15                                                             
AC@TT027 DS    CL15                                                             
AC@TT028 DS    CL15                                                             
AC@TT029 DS    CL15                                                             
AC@TT030 DS    CL15                                                             
AC@TT031 DS    CL15                                                             
AC@TT032 DS    CL15                                                             
AC@TT033 DS    CL15                                                             
AC@TT034 DS    CL15                                                             
AC@TT035 DS    CL15                                                             
AC@TT036 DS    CL15                                                             
AC@TT037 DS    CL15                                                             
AC@TT038 DS    CL15                                                             
AC@TT039 DS    CL15                                                             
AC@TT040 DS    CL15                                                             
AC@TT041 DS    CL15                                                             
AC@TT042 DS    CL15                                                             
AC@TT043 DS    CL15                                                             
AC@TT044 DS    CL15                                                             
AC@TT045 DS    CL15                                                             
AC@TT046 DS    CL15                                                             
AC@TT047 DS    CL15                                                             
AC@TT048 DS    CL15                                                             
AC@TT049 DS    CL15                                                             
AC@TT050 DS    CL15                                                             
AC@TT051 DS    CL15                                                             
AC@TT052 DS    CL15                                                             
AC@TT053 DS    CL15                                                             
AC@TT054 DS    CL15                                                             
AC@TT055 DS    CL15                                                             
AC@TT056 DS    CL15                                                             
AC@TT057 DS    CL15                                                             
AC@TT058 DS    CL15                                                             
AC@TT059 DS    CL15                                                             
AC@TT060 DS    CL15                                                             
AC@TT061 DS    CL15                                                             
AC@TT062 DS    CL15                                                             
AC@TT064 DS    CL15                                                             
AC@TT070 DS    CL15                                                             
AC@TT071 DS    CL15                                                             
AC@TT072 DS    CL15                                                             
AC@TT075 DS    CL15                                                             
AC@TT079 DS    CL15                                                             
AC@TT081 DS    CL15                                                             
AC@TT096 DS    CL15                                                             
AC@TT098 DS    CL15                                                             
AC@TT099 DS    CL15                                                             
AC@UNKWN DS    CL15                                                             
UOWCTAB  DS    0X                  ORDER WC TABLE                               
UOWITSEQ DS    XL2                 ITEM SEQUENCE FOR INVOICE LOG                
UOWCORIG DS    CL2                 ORIGINAL WORKCODE                            
UOWCMTCH DS    CL2                 MATCHED WORKCODE                             
UOWCAMT  DS    PL6                 AMOUNT                                       
UOWCFCAM DS    PL6                 FOREIGN CURRENCY AMOUNT                      
UOWCTABL EQU   *-UOWCTAB                                                        
UOWCMAX  EQU   6                                                                
UOWCMAXT EQU   100                                                              
         DS   (UOWCMAXT*UOWCMAX-1)XL(UOWCTABL)                                  
PALAREA  DS    XL20                P&L BUCKET AREA                              
WORKX    DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS                                                        *         
***********************************************************************         
*                                                                               
RPRGTABD DSECT                     ** COVERS RECOVERY PROG TABLE **             
RPRGNO   DS    XL1                 PROGRAM NUMBER                               
RPRGIND  DS    XL1                 PROGRAM INDICATORS                           
RPRGISQQ EQU   X'80'               OVERRIDE TRSEL BATCH SEQ #                   
RPRGIMBQ EQU   X'40'               MULTIPLE BATCH TYPES (USE RPRGBTYP)          
RPRGBRFQ EQU   X'20'               OVERRIDE BATCH REFERENCE                     
RPRGNOSQ EQU   X'10'               DON'T USE SIN IN SIG. SORT KEY               
RPRGXBTY EQU   X'08'               EXCLUDE POSTINGS WHERE B/T=RPRGBTYP          
RPRGHRSQ EQU   X'04'               BATCH/ITEM TOTALS IN HOURS, NOT CASH         
RPRGBTYP DS    XL1                 TEST/OVERRIDE BATCH TYPE OR 0                
RPRGSUBP DS    XL1                 TEST TRSSTAT3 VALUE OR 0                     
RPRGTABL EQU   *-RPRGTABD          L'TABLE ENTRY                                
*                                                                               
WKRTABD  DSECT                     ** COVERS WORKER FILE TABLE **               
WKRTPRG  DS    CL3                 PROGRAM NUMBER                               
WKRTIND  DS    XL1                 PROGRAM INDICATORS                           
WKRTISP  EQU   X'80'               TEST WKRTPRG SPECIFIED IN PROFILES           
WKRTACR  EQU   X'40'               BATCH TO BE AN ACCRUAL                       
WKRTBTYP DS    XL1                 APPLIED BATCH TYPE                           
WKRTABL  EQU   *-WKRTABD           L'TABLE ENTRY                                
*                                                                               
YMDD     DSECT                     ** COVERS PWOS YMD (SEE MONADD) **           
YMDYEAR  DS    XL1                                                              
YMDMONTH DS    XL1                                                              
YMDDAY   DS    XL1                                                              
*                                                                               
TYPTABD  DSECT                     ** COVERS BATCH TYPE TABLE **                
TYPNUM   DS    XL1                 BATCH TYPE NUMBER (SEE BELOW)                
TYPARN   DS    XL1                 TYPE NUMBER FOR ACCRUAL REVERSAL             
TYPCTRY  DS    XL1                 COUNTRY VALIDITY (00=ALL,8N=NOT N)           
TYPIND1  DS    XL1                 BATCH INDICATOR 1                            
TYPIACRL EQU   X'80'               ACCRUAL BATCH TYPE                           
TYPICONV EQU   X'40'               CONVERT OLD BATCH TO NEW BATCH               
TYPIADVP EQU   X'20'               ADVANCE PAYMENT BATCH TYPE                   
TYPIXLOK EQU   X'10'               IGNORE MONTH OF SERVICE LOCK                 
TYPIORDS EQU   X'08'               BATCH TYPE CAN PROCESS ORDER RECS            
TYPIPRDQ EQU   X'04'               BATCH GROUP IS PROD (ELSE GENERAL)           
TYPINOLD EQU   X'02'               DON'T CHECK FOR OLD BATCH HEADER             
TYPIACRV EQU   X'01'               ACCRUAL REVERSAL BATCH TYPE                  
TYPBTDD  DS    AL2                 DSLIST DISP OF BATCH TYPE NAME               
TYPBNDD  DS    AL2                 DSLIST DISP OF BATCH NAME OR 0               
TYPTABL  EQU   *-TYPTABD           L'TABLE ENTRY                                
*                                                                               
ASKTABD  DSECT                     ** DSECT COVERS ASKTAB **                    
ASKTULA  DS    CL(L'ACTKULA)       UNIT/LEDGER/ACCOUNT                          
ASKTDEL  DS    XL2                 DELETED DRAFT TRANSACTION COUNT              
ASKTABL  EQU   *-ASKTABD                                                        
ASKTMAX  EQU   CHASTABL/ASKTABL    MAXIMUM ASKTAB ENTRIES                       
*                                                                               
CHQTABD  DSECT                     ** DSECT COVERS CHEQUE TABLE **              
CHQITEM  DS    XL(L'TBAKTSEQ)      FIRST MERGED ITEM NUMBER                     
CHQDATA  DS    XL(BICLN1Q+L'BICCACN)                                            
CHQKEYL  EQU   (BICSTA-BICACT)                                                  
CHQTABL  EQU   *-CHQTABD                                                        
CHQTMAX  EQU   400                 MAXIMUM ENTRIES                              
*                                                                               
ACMTABD  DSECT                     ** DSECT COVERS U/L ACCUMS TABLE **          
ACMTCUL  DS    XL3                                                              
         DS    XL1                 N/D                                          
ACMTDR   DS    PL8                                                              
ACMTCR   DS    PL8                                                              
ACMTABL  EQU   *-ACMTABD                                                        
ACMTMAX  EQU   100                 MAXIMUM ENTRIES                              
*                                                                               
REPTOTD  DSECT                     ** DSECT COVERS REP TOTALS TABLE **          
REPTUID  DS    XL2                                                              
REPTUSR  DS    CL8                                                              
REPTDJDR DS    PL8                 DJ TOTALS FOR USER                           
REPTDJCR DS    PL8                                                              
REPTDKDR DS    PL8                 DK TOTALS FOR USER                           
REPTDKCR DS    PL8                                                              
REPTOTL  EQU   *-REPTOTD                                                        
REPTMAX  EQU   100                 MAXIMUM ENTRIES                              
*                                                                               
TOTTABD  DSECT                     ** TOTALS RECORD TABLE **                    
TOTTDISP DS    AL2                 DISPLACEMENT TO FULLWORD ACCUM               
TOTTEOTQ EQU   255                 END OF TABLE INDICATOR                       
TOTTINDS DS    XL1                 TOTALS INDICATORS                            
TOTTIADD EQU   X'80'               ADD TO TOTAL RECORDS ADDED                   
TOTTICHA EQU   X'40'               ADD TO TOTAL RECORDS CHANGED                 
TOTTITRN EQU   X'02'               TOTTDISP IS FROM TRNBLKD, NOT WORKD          
TOTTITOT EQU   X'01'               THIS IS A TOTAL                              
TOTTDESC DS    CL30                TOTALS DESCRIPTION                           
TOTTABL  EQU   *-TOTTABD                                                        
*                                                                               
CPYTABD  DSECT                     ** COMPANY TABLE **                          
CPYTCPY  DS    XL(L'CPYKCPY)       COMPANY CODE                                 
CPYTUID  DS    XL(L'CPYUID)        PRINCIPAL USER-ID NUMBER                     
CPYTSTA1 DS    XL1                 STATUS BYTE 1                                
CPYTSTA2 DS    XL1                 STATUS BYTE 2                                
CPYTSTA3 DS    XL1                 STATUS BYTE 3                                
CPYTSTA4 DS    XL1                 STATUS BYTE 4                                
CPYTSTA5 DS    XL1                 STATUS BYTE 5                                
CPYTSTA6 DS    XL1                 STATUS BYTE 6                                
CPYTSTA7 DS    XL1                 STATUS BYTE 7                                
CPYTSTA8 DS    XL1                 STATUS BYTE 8                                
CPYTSTA9 DS    XL1                 STATUS BYTE 9                                
CPYTSTAA DS    XL1                 STATUS BYTE 10                               
CPYTALPH DS    XL(L'CPYALPHA)      ALPHA-ID                                     
CPYTPROD DS    CL2                 PRODUCTION LEDGER                            
CPYTGMOA DS    PL2                 GL MOA                                       
CPYTNAME DS    XL(L'NAMEREC)       COMPANY NAME                                 
CPYTABL  EQU   *-CPYTABD                                                        
CPYFRSTQ EQU   X'41'               FIRST COMPANY CODE                           
*                                                                               
XPQFNTD  DSECT                                                                  
XPQFUID  DS    XL2                                                              
XPQFNAM  DS    CL3                                                              
XPQFSEQ  DS    CL2                                                              
XPQFNTL  EQU   *-XPQFNTD                                                        
*                                                                               
HSMAPD   DSECT                     MAP HEAD3/4 REPORT LINE                      
         DS    CL12                                                             
HSCPY    DS    CL(L'REPCPY)        COMPANY CODE AND NAME                        
         DS    CL1                                                              
HSEXT    DS    0CL22               'EXTERNAL PHASE ACTIVE'                      
HSSLT    DS    CL7                 'SOURCE='                                    
HSSRC    DS    CL15                                                             
HSMAPL   EQU   *-HSMAPD                                                         
         DS    CL(L'P-HSMAPL)                                                   
*                                                                               
BT01     EQU   1                   BATCH TYPE EQUATES                           
BT02     EQU   2                                                                
BT03     EQU   3                                                                
BT04     EQU   4                                                                
BT05     EQU   5                                                                
BT06     EQU   6                                                                
BT07     EQU   7                                                                
BT08     EQU   8                                                                
BT09     EQU   8                                                                
BT10     EQU   10                                                               
BT11     EQU   11                                                               
BT12     EQU   11                                                               
BT13     EQU   13                                                               
BT14     EQU   14                                                               
BT15     EQU   15                                                               
BT16     EQU   16                                                               
BT17     EQU   17                                                               
BT18     EQU   18                                                               
BT19     EQU   19                                                               
BT20     EQU   20                                                               
BT21     EQU   21                                                               
BT22     EQU   22                                                               
BT23     EQU   23                                                               
BT24     EQU   24                                                               
BT25     EQU   25                                                               
BT26     EQU   26                                                               
BT27     EQU   27                                                               
BT28     EQU   27                                                               
BT29     EQU   27                                                               
BT30     EQU   30                                                               
BT31     EQU   31                                                               
BT32     EQU   32                                                               
BT33     EQU   33                                                               
BT34     EQU   34                                                               
BT35     EQU   35                                                               
BT36     EQU   36                                                               
BT37     EQU   37                                                               
BT38     EQU   38                                                               
BT39     EQU   39                                                               
BT40     EQU   40                  ACCRUAL REVERSAL FROM BT39                   
BT41     EQU   41                                                               
BT42     EQU   42                                                               
BT43     EQU   43                                                               
BT44     EQU   44                  ACCRUAL REVERSAL FROM BT43                   
BT45     EQU   45                                                               
BT46     EQU   46                                                               
BT47     EQU   47                                                               
BT48     EQU   48                                                               
BT49     EQU   49                                                               
BT50     EQU   50                                                               
BT51     EQU   51                                                               
BT52     EQU   52                                                               
BT53     EQU   53                                                               
BT54     EQU   54                  ACCRUAL REVERSAL FROM BT53                   
BT55     EQU   55                                                               
BT56     EQU   56                  ACCRUAL REVERSAL FROM BT55                   
BT57     EQU   57                                                               
BT58     EQU   58                  INTERAGENCY POSTINGS                         
BT59     EQU   59                                                               
BT60     EQU   60                                                               
BT61     EQU   61                                                               
BT62     EQU   62                                                               
BT64     EQU   64                                                               
BT70     EQU   70                                                               
BT71     EQU   71                                                               
BT72     EQU   72                                                               
BT75     EQU   75                                                               
BT79     EQU   79                  EXCHANGE DIFFS. (CLIENT BILLING)             
BT81     EQU   81                  Retained earnings                            
BT96     EQU   96                  MATCH ADJUSTMENTS                            
BT98     EQU   98                  MEDIA BILLING TRANSFERS                      
BT99     EQU   99                  WFM POSTINGS                                 
*                                                                               
         EJECT                                                                  
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACGENPOST                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
*ACGENDAY                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*&&US                                                                           
* TRNELD EXTENSION FOR BILLING                                                  
         PRINT OFF                                                              
TRNELD   DSECT                                                                  
         ORG   TRNNARR                                                          
TRNDESC  DS    CL15                DESCRIPTION (50% ESTIMATE ETC.)              
TRNLNDQ  EQU   *-TRNELD            LENGTH OF SHORT ELEMENT                      
TRNLACCS EQU   6                                                                
TRNCOMM  DS    PL(TRNLACCS)        COMMISSION BILLED                            
TRNGRSS  DS    0PL6                                                             
TRNGRSXR DS    PL(TRNLACCS)        GROSS BILLED AT VAT RATE EXEMPT              
TRNGRSZR DS    PL(TRNLACCS)        GROSS BILLED AT VAT RATE NIL                 
TRNGRSR1 DS    PL(TRNLACCS)        GROSS BILLED AT VAT RATE RATE 1              
TRNGRSR2 DS    PL(TRNLACCS)        GROSS BILLED AT VAT RATE RATE 2              
TRNGRSR3 DS    PL(TRNLACCS)        GROSS BILLED AT VAT RATE RATE 3              
TRNGRSR4 DS    PL(TRNLACCS)        GROSS BILLED AT VAT RATE RATE 4              
TRNGRSR5 DS    PL(TRNLACCS)        GROSS BILLED AT VAT RATE RATE 5              
TRNGRSN  EQU   (*-TRNGRSS)/L'TRNGRSS                                            
TRNVATS  DS    0PL6                                                             
TRNVATR1 DS    PL(TRNLACCS)        V.A.T BILLED AT VAT RATE RATE 1              
TRNVATR2 DS    PL(TRNLACCS)        V.A.T BILLED AT VAT RATE RATE 2              
TRNVATR3 DS    PL(TRNLACCS)        V.A.T BILLED AT VAT RATE RATE 3              
TRNVATR4 DS    PL(TRNLACCS)        V.A.T BILLED AT VAT RATE RATE 4              
TRNVATR5 DS    PL(TRNLACCS)        V.A.T BILLED AT VAT RATE RATE 5              
TRNVATN  EQU   (*-TRNVATS)/L'TRNVATS                                            
TRNLNBDQ EQU   *-TRNDESC           LENGTH OF BILLING DETAIL                     
TRNLNBQ  EQU   *-TRNELD            LENGTH OF BILLING ELEMENT                    
         PRINT ON                                                               
*                                                                               
*&&                                                                             
*ACRCVRECD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACRCVRECD                                                      
         PRINT ON                                                               
*ACBMONVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBMONVALD                                                     
         PRINT ON                                                               
*ACRAPPERD                                                                      
         PRINT OFF                                                              
ACRAPD   DSECT                                                                  
       ++INCLUDE ACRAPPERD                                                      
         PRINT ON                                                               
*ACDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
*DDDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDDEQUS                                                       
         PRINT ON                                                               
*ACLDCPTRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACLDCPTRD                                                      
         PRINT ON                                                               
*DDCTRYEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
CTRYALL  EQU   0                                                                
CTRYNOT  EQU   X'80'                                                            
         PRINT ON                                                               
*SEACSFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*DDCOMFACSD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACSD                                                     
         PRINT ON                                                               
*ACADDTRND                                                                      
         PRINT OFF                                                              
TRNBLKD  DSECT                                                                  
       ++INCLUDE ACADDTRND                                                      
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
ACWORKD  DSECT                                                                  
         ORG   P                                                                
PBLIN    DS    0CL(L'P)            * BATCH REPORT LINE DEF *                    
PBXL     DS    XL1                                                              
PBMOS    DS    CL2                                                              
PBREF    DS    CL4                                                              
PBBSQ    DS    CL3                                                              
PBX1     DS    XL1                                                              
PBNAM    DS    CL15                                                             
PBX2     DS    XL1                                                              
PBTYP    DS    CL18                                                             
PBX3     DS    XL1                                                              
PPERS    DS    CL8                                                              
PBX4     DS    XL1                                                              
PITMS    DS    CL4                                                              
PBX5     DS    XL1                                                              
PPSTG    DS    CL7                                                              
PBX6     DS    XL1                                                              
PCASH    DS    CL14                                                             
PBX7     DS    XL1                                                              
PADDT    DS    CL8                                                              
PBX8     DS    XL1                                                              
PEFDT    DS    CL8                                                              
PBX9     DS    XL1                                                              
PCMNT    DS    CL30                                                             
PBXR     DS    XL1                                                              
         ORG   P                                                                
LINED    DS    0CL(L'P)            ** POSTINGS SUMMARY LINE DEF **              
         DS    CL16                                                             
LINBXL   DS    CL1                                                              
LINFILE  DS    CL67                                                             
LINBX1   DS    CL1                                                              
LINDR    DS    CL14                                                             
LINBX2   DS    CL1                                                              
LINCR    DS    CL14                                                             
LINBXR   DS    CL1                                                              
         ORG   P                                                                
         DS    CL30                                                             
RCTBXL   DS    CL1                                                              
RCTDES   DS    CL30                                                             
RCTBX1   DS    CL1                                                              
RCTTOT   DS    CL14                                                             
RCTBXR   DS    CL1                                                              
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* BUFFERS AND WORK AREAS                                              *         
***********************************************************************         
*                                                                               
ACBG02   CSECT                                                                  
*                                                                               
IOALN    EQU   2000                                                             
IOAREALN EQU   IOALN+L'IODA+L'IOWORK                                            
IOAREA1  DS    (IOAREALN)X         I/O BUFFERS                                  
IOAREA2  DS    (IOAREALN)X                                                      
IOAREA3  DS    (IOAREALN)X                                                      
IOAREA4  DS    (IOAREALN)X                                                      
IOAREA5  DS    (IOAREALN)X                                                      
IOAREAS  EQU   (*-IOAREA1)/IOAREALN                                             
*                                                                               
ADDTIO1  DS    (IOAREALN)X         ADDTRN BUFFERS                               
ADDTIO2  DS    (IOAREALN)X                                                      
ADDTIO3  DS    (IOAREALN)X                                                      
ADDTIO4  DS    (IOAREALN)X                                                      
*                                                                               
WKAREALN EQU   4096                                                             
WKAREA   DS    (WKAREALN)X         WORKER BUFFER                                
*                                                                               
CHASTABL EQU   36*1024                                                          
CHASTAB  DS    (CHASTABL)X         CHEQUE/ASKKEY TABLE FOR UPD/DELBAT           
*                                                                               
ACMTABLN EQU   ACMTMAX*ACMTABL                                                  
ACMTAB   DS    (ACMTABLN)X         ADK TOTALS BY U/L                            
         DS    XL1                 EOT MARKER                                   
*                                                                               
REPTOTLN EQU   REPTMAX*REPTOTL                                                  
REPTOT   DS    (REPTOTLN)X         ADJ/ADK TOTALS BY USERID                     
         DS    XL1                 EOT MARKER                                   
*                                                                               
TRNWK    DS    XL1000              WORK AREA FOR ADDITE (BT7/48/57)             
*                                                                               
GENWK    DS    XL512               GENERAL WORK AREA (IOTRACE/TSTBMO)           
*                                                                               
         DS    0F                                                               
RAPBLOCK DS    XL(RAPBLKL)         ACRAPPER BLOCK                               
*                                                                               
CPYTAB   DS    ((EFFS-CPYFRSTQ)*CPYTABL)X  DETAILS FOR ALL CPYS ON FILE         
*                                                                               
XTPQFS   DS    XL1024              IDS OF PQ FILES CREATED BY EXTERN(S)         
*                                                                               
ADDTBLK  DS    XL(TRNBLKL)         ADDTRN BLOCK                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067ACREPBG02 07/29/20'                                      
         END                                                                    
