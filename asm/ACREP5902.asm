*          DATA SET ACREP5902  AT LEVEL 045 AS OF 10/21/15                      
*PHASE AC5902C                                                                  
*INCLUDE ACJBEXC                                                                
         SPACE 2                                                                
*              REQUEST OPTIONS                                                  
***********************************************************************         
* 1 N=NET,G=GROSS                                                     *         
* 2 Y=ALLJOBS, 1-9,A-E=EXCPTN REASON, BLK=ALL EXCPTNS                 *         
* 3 S=SUPPRESS CLOSED ACCTS, C=CLOSED ACCTS ONLY                      *         
* 4 S=SUPPRESS LOCKED ACCTS, L=LOCKED ACCTS ONLY                      *         
* 5 Y=WORKCODE SUMMARY                                                *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*              PROFILES                                                         
***********************************************************************         
* 1  BUILD SUMMARY BY MONTH OF OPENING                       Y,N      *         
* 2  APPLY START/END DATES TO OPEN OR CLOSE DATE OR BOTH     N,O,C,B  *         
* 3  LARGE BALANCE VALUE (X100)                              0-250    *         
* 4  NEW PAGE PER PRODUCT                                    Y,N      *         
* 5  SUPPRESS ZERO AMOUNT WORKCODES                          Y,N      *         
***********************************************************************         
         SPACE 2                                                                
*              HISTORY                                                          
***********************************************************************         
*              2/98 YEAR 2000 CHANGES                                           
*                                                                               
*              6/92 FIX BUG SO TO GROSS UP HELD AMOUNT                          
*                                                                               
*              8/93 ADD STUDIO RELATED EXCEPTION REASONS                        
*                   ADD MAXEXCEP, MAXIMIM NUMBER OF EXCEPTION REASONS           
*                   59 WILL REPORT AT A TIME (FORMERLY 6)                       
*                   CALL JOBBER VIA SPECS (FORMERLY LOOKUP)                     
***********************************************************************         
         TITLE 'AC5902 - PRODUCTION JOB EXCEPTION REPORT'                       
AC5902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC59**,R9,R8,RR=R5                                           
         L     RA,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RA                                                       
*                                                                               
         LA    RC,SPACEND                                                       
*                                                                               
         USING AC5902D,RC                                                       
*                                                                               
         ST    R5,PRELOC                                                        
         EJECT ,                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   REQF1                                                            
*&&UK                                                                           
         MVI   REMOTOP,C'Y'        IF PRINTING REMOTE                           
         L     R2,REMOTEC                                                       
*                                                                               
         USING REMOTED,R2                                                       
*                                                                               
         OC    REMOTKEY,REMOTKEY                                                
         BNZ   JEXIT               THEN NO BOXES                                
         MVI   REMOTOP,C'N'                                                     
*&&                                                                             
         L     R2,=A(SAVERC)       SAVE REG C                                   
         A     R2,PRELOC                                                        
         ST    RC,0(,R2)                                                        
         L     R2,VEXTRAS                                                       
*                                                                               
         USING RUNXTRAD,R2                                                      
*                                                                               
         L     R2,ADMASTD                                                       
*                                                                               
         USING MASTD,R2                                                         
*                                                                               
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX            STORE ADDR OF BOX ROUTINE                    
         L     R2,=A(HOOK)                                                      
         A     R2,PRELOC                                                        
         ST    R2,HEADHOOK                                                      
         B     JEXIT                                                            
         EJECT ,                                                                
REQF1    CLI   MODE,REQFRST                                                     
         BNE   JE2                                                              
         L     R7,=A(BUFFALOC)                                                  
         A     R7,PRELOC                                                        
         ST    R7,ABUFF                                                         
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         ZAP   REQJOBS,=P'0'                                                    
         ZAP   CLIJOBS,=P'0'                                                    
*                                                                               
REQF1C   MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,0                                                       
         BAS   RE,BUILDTE          BUILD HEADLINE AND DATE LIST                 
*                                                                               
         MVC   PZR(40),=5PL8'0'                                                 
*        MVC   RQLN,=CL45'TOTAL FOR REQUEST'                                    
         MVC   RQAC(24),PZR                                                     
         MVI   OFFICE,C'N'                                                      
         MVI   NEWOFF,C'N'                                                      
         L     R4,ADCMPEL                                                       
*                                                                               
         USING ACCOMPD,R4                                                       
*                                                                               
         TM    ACMPSTAT,X'20'                                                   
         BZ    *+8                                                              
         MVI   OFFICE,C'Y'                                                      
         TM    ACMPSTA4,X'01'      TEST NEW OFFICES                             
         BZ    *+8                                                              
         MVI   NEWOFF,C'Y'                                                      
*                                                                               
         L     R3,=A(EXCPTB)                                                    
*                                                                               
REQF1E   DS    0H                                                               
         ZAP   70(4,R3),=P'0'                                                   
         ZAP   74(8,R3),=P'0'                                                   
         LA    R3,94(,R3)                                                       
         CLI   0(R3),X'FF'                                                      
         BNE   REQF1E                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK)                                  
         GOTO1 DATCON,DMCB,(0,WORK),(1,JBDATE)                                  
*                                                                               
         LA    R3,DATELIST         BUILD 36 MONTH LIST                          
         LA    R4,36                                                            
         L     R2,=F'-35'                                                       
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,(R2)                               
*                                                                               
REQF4    MVC   WORK(6),WORK+6                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(1,0(R3))                                   
         MVC   WORK+4(2),=C'15'                                                 
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,F'1'                               
         ZAP   2(6,R3),=P'0'                                                    
         ZAP   8(3,R3),=P'0'                                                    
         LA    R3,11(,R3)                                                       
         BCT   R4,REQF4                                                         
*                                                                               
         XC    START,START                                                      
         MVC   END,=X'FFFFFF'                                                   
         CLC   QSTART,SPACES                                                    
         BE    REQF6                                                            
         MVC   QSTART+4(2),=C'01'                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(1,START)                                 
*                                                                               
REQF6    CLC   QEND,SPACES                                                      
         BE    JEXIT                                                            
         MVC   QEND+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(X'30',QEND),(X'20',QEND),(1,0)                      
         GOTO1 DATCON,DMCB,(0,QEND),(1,END)                                     
         B     JEXIT                                                            
         EJECT ,                                                                
JE2      CLI   MODE,LEVAFRST                                                    
         BNE   JE3                                                              
         MVI   LASTSW,0                                                         
         MVI   PRODAMT,0                                                        
         ZAP   BIGBAL,=P'0'                                                     
*                                                                               
JE22     SR    RE,RE                                                            
         ICM   RE,1,PROGPROF+2                                                  
         BZ    JE23                                                             
         MH    RE,=H'10000'        BIG BALANCE VALUE X100                       
         CVD   RE,DOUBLE                                                        
         L     RF,=A(EXCPTBB)                                                   
         LA    RF,33(,RF)                                                       
         EDIT  (P8,DOUBLE),(11,0(RF)),2,ALIGN=LEFT                              
         ZAP   BIGBAL,DOUBLE                                                    
*                                                                               
JE23     BAS   RE,CLIENT           SET ACCUM LINE                               
         MVI   FRSTCLI,C'Y'                                                     
         MVC   AGAC(40),PZR                                                     
*                                                                               
         L     R3,=A(EXCPTB)       EXECPTION BY CLIENT                          
         ZAP   82(4,R3),=P'0'                                                   
         ZAP   86(8,R3),=P'0'                                                   
         LA    R3,94(,R3)                                                       
         CLI   0(R3),X'FF'                                                      
         BNE   *-20                                                             
         B     JEXIT                                                            
         EJECT ,                                                                
JE3      CLI   MODE,LEVBFRST                                                    
         BNE   JE4                                                              
         CLI   LASTSW,1            WAS THERE A LAST PRODUCT                     
         BNE   JE3A                                                             
         CLI   PRODAMT,1           DID IT HAVE DATA                             
         BNE   JE3A                                                             
         BAS   RE,TOTBOX   UNDERLINE AND PRINT LAST PRODUCT TOTAL               
*                                                                               
JE3A     BAS   RE,PRODUCT          SET UP ACCUM LINE                            
         MVI   FRSTPRD,C'Y'                                                     
         CLI   PROGPROF+3,C'Y'     PROFILE FOR NEW PAGE PER PROD                
         BNE   JEXIT                                                            
         MVI   FORCEHED,C'Y'                                                    
         B     JEXIT                                                            
         EJECT ,                                                                
JE4      CLI   MODE,PROCACC                                                     
         BNE   JE6                                                              
*                                                                               
         USING JXCEPTD,R6                                                       
*                                                                               
         LA    R6,JXBLOCK                                                       
         XC    JXBLOCK(JXLEN),JXBLOCK                                           
         MVI   JXMODE,JXMPRACC     INITIALIZE JXBLOCK                           
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
*                                                                               
         DROP  R6                                                               
*                                                                               
         ZAP   SMALL,=P'5000'                                                   
         MVC   OVEREST,=F'10000'                                                
         BAS   RE,PROFXTRA                                                      
         BAS   RE,JOB                                                           
         ZAP   ORDERS,=P'0'                                                     
         MVI   HOLDSW,C'N'                                                      
         MVI   REASONC,C'N'                                                     
         MVI   CLIBILL,C'N'                                                     
         MVI   APPSW,C'Y'          SET TO APPROVED ESTIMATE                     
*                                                                               
         MVC   ESTIMATE(40),PZR                                                 
         XC    WKCNT,WKCNT                                                      
         MVC   MDAC(40),PZR                                                     
*                                                                               
         L     R4,ADACCBAL                                                      
*                                                                               
         USING ACBALD,R4                                                        
*                                                                               
         ZAP   BALNC,ACBLDR                                                     
         SP    BALNC,ACBLCR        SAVE BALANCE                                 
         MVI   BALIND,C'D'         DEBIT BALANCE                                
         CP    BALNC,=P'0'                                                      
         BNL   *+14                                                             
         MP    BALNC,=P'-1'        MAKE IT POSITIVE                             
         MVI   BALIND,C'C'         CREDIT JOB                                   
*                                                                               
         CLI   QOPT1,C'G'                                                       
         BNE   JB41                                                             
         CP    BALNC,=P'0'                                                      
         BNE   JB41                                                             
         MVI   BALIND,C'C'                                                      
*                                                                               
JB41     L     R4,ADACC                                                         
         MVI   ELCODE,X'26'                                                     
         BAS   RE,GETEL                                                         
         BNE   JB41B                                                            
*                                                                               
         USING ACJOBD,R4                                                        
*                                                                               
         TM    ACJBSTAT,X'80'      TEST FOR UNAPPROVED ESTIMATE                 
         BZ    *+8                                                              
         MVI   APPSW,C'N'          YES                                          
         L     R3,ADGOBLOC                                                      
*                                                                               
         USING GOBLOCKD,R3                                                      
*                                                                               
         MVC   JBEND,ACJBCLOS                                                   
         LA    R3,GOBILAM1                                                      
         MVC   PEREST,0(R3)                                                     
         CLI   ACJBLEN,ACJBLNQ3                                                 
         BL    JB41B                                                            
         LA    R0,2                                                             
         LA    RE,X'80'                                                         
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    ACJBBIST,X'00'                                                   
         BZ    JB41B                                                            
*                                                                               
JB41A    LA    R3,4(,R3)                                                        
         OC    0(4,R3),0(R3)       NEXT PERCENT IS ZERO                         
         BZ    JB41B               SO GET OUT                                   
         SRL   RE,1                SET FOR NEXT PERCENT                         
         L     RF,0(,R3)                                                        
         A     RF,PEREST                                                        
         ST    RF,PEREST                                                        
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    ACJBBIST,X'00'                                                   
         BZ    JB41B                                                            
         BCT   R0,JB41A                                                         
*                                                                               
         USING ACSTATD,R4                                                       
*                                                                               
JB41B    L     R4,ADACCSTA                                                      
         MVC   JBSTRT,ACSTBFDT                                                  
         MVC   JBSTAT,ACSTSTAT                                                  
         TM    JBSTAT,X'40'                                                     
         BO    *+10                                                             
         AP    CLIJOBS,=P'1'       COUNT NO. OF OPEN JOBS                       
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCRDEST,C'N'                                                     
         CLI   QOPT3,C' '                                                       
         BE    JB43                                                             
         CLI   QOPT3,C'C'          CLOSED JOBS                                  
         BE    JB42                                                             
         TM    JBSTAT,X'40'        MUST BE SUPPRESS CLOSED                      
         BO    JEXIT               AND ITS CLOSED                               
         B     JB43                                                             
*                                                                               
JB42     TM    JBSTAT,X'40'        CLOSED ONLY                                  
         BZ    JEXIT               THIS JOB NOT CLOSED                          
*                                                                               
JB43     CLI   PROGPROF+1,C'B'     OPENED OR CLOSED IN PERIOD                   
         BE    JB43B                                                            
         CLI   PROGPROF+1,C'C'     CLOSED IN PERIOD                             
         BE    JB43C                                                            
         CLI   PROGPROF+1,C'O'                                                  
         BE    JB43O               OPENED IN PERIOD                             
         B     JB48                                                             
*                                                                               
JB43B    CLC   JBEND,START         TEST DATE CLOSED FOR 'BOTH'                  
         BL    JB43B1                                                           
         CLC   JBEND,END                                                        
         BH    JB43B1                                                           
         B     JB48                CLOSED IN PERIOD                             
*                                                                               
JB43B1   CLC   JBSTRT,START        TEST DATE OPENED FOR 'BOTH'                  
         BL    JEXIT                                                            
         CLC   JBSTRT,END                                                       
         BH    JEXIT                                                            
         B     JB48                OPENED IN PERIOD                             
*                                                                               
JB43C    CLC   JBEND,START         TEST DATE CLOSED WHEN PROF=C                 
         BL    JEXIT                                                            
         CLC   JBEND,END                                                        
         BH    JEXIT                                                            
         B     JB48                CLOSED IN PERIOD                             
*                                                                               
JB43O    CLC   JBSTRT,START        TEST DATE OPENED WHEN PROF=O                 
         BL    JEXIT                                                            
         CLC   JBSTRT,END                                                       
         BH    JEXIT                                                            
         B     JB48                OPENED IN PERIOD                             
*                                                                               
JB48     CLI   QOPT4,C' '          LOCKED JOBS                                  
         BE    JB49                                                             
         CLI   QOPT4,C'S'                                                       
         BE    *+16                                                             
         TM    JBSTAT,X'20'                                                     
         BZ    JEXIT                                                            
         B     JB49                                                             
         TM    JBSTAT,X'20'                                                     
         BO    JEXIT                                                            
*                                                                               
JB49     MVI   FCRDTRNS,C'Y'                                                    
*                                                                               
         L     R3,ADGOBLOC                                                      
*                                                                               
         USING GOBLOCKD,R3                                                      
*                                                                               
         XC    RETFLAG,RETFLAG                                                  
         CLC   GODIST,SPACES       DISTRIBUTION SCHEME ON JOB                   
         BNH   *+8                 NO                                           
         OI    RETFLAG,RETSCHEM    FLAG JOB HAS A SCHEME                        
*                                                                               
         USING ACMD,RE                                                          
         L     RE,AMONACC          ADDRESS JOBBER TABLES                        
         L     R3,ACMACOL                                                       
         L     R5,ACMAJOBB                                                      
         DROP  RE                                                               
*                                                                               
         USING JBLOCKD,R5                                                       
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BNE   JB57                NO                                           
         USING MJETABD,R3                                                       
         ZAP   ESTIMATE,MJETVAL    SET ESTIMATE TO NET                          
         ZAP   GROSSEST,MJETVAL+6(6) SET GROSS ESTIMATTE                        
         ZAP   GROSS,=P'0'                                                      
         ZAP   HRESTMAT,MJETVAL+12(6) SAVE HIGHEST REVISION AMOUNT              
         ZAP   HIREVEST,MJETVAL+12(6) SAVE HIGHEST REVISION AMOUNT              
         CLI   QOPT1,C'G'          DO THEY WANT GROSS ?                         
         BNE   *+10                NO                                           
         ZAP   HIREVEST,MJETVAL+18(6) SAVE HIGHEST REVISION GROSS               
*                                                                               
JB55     CLI   MJETTYP,MJETTEQ                                                  
         BE    JEXIT                                                            
         CLI   MJETTYP,MJETTWQ                                                  
         BNE   JB56                                                             
         OC    MJET1RA,MJET1RA     BUT NOT 1R-LEVEL DETAILS                     
         BNZ   JB56                                                             
         MVC   WC,MJETWCD                                                       
         ZAP   DUB,MJETVAL         GET CURRENT NET                              
         CLI   QOPT1,C'G'          DO THEY WANT GROSS ?                         
         BNE   *+10                NO                                           
         ZAP   DUB,MJETVAL+6(6)    YES, CHANGE ESTIMATE ALSO                    
*                                                                               
         L     R2,WKCNT                                                         
         LA    R6,1(,R2)                                                        
         ST    R6,WKCNT                                                         
         MH    R2,=AL2(WKLNQ)                                                   
         L     RF,=A(DETAILS)                                                   
         LA    R2,0(R2,RF)                                                      
*                                                                               
         USING DLIND,R2                                                         
*                                                                               
         MVC   WKCD,WC                                                          
         ZAP   WKESTMTE,DUB                                                     
         ZAP   WKACT,=P'0'                                                      
         ZAP   WKHOLD,=P'0'                                                     
*                                                                               
JB56     XR    R1,R1                                                            
         IC    R1,MJETLEN                                                       
         AR    R3,R1                                                            
         B     JB55                                                             
*                                                                               
         USING JBCOLD,R3                                                        
JB57     LH    R1,JBNROWS                                                       
         ZAP   ESTIMATE,JBCOLVAL   SET ESTIMATE TO NET                          
         ZAP   GROSSEST,JBCOLVAL+6(6) SET GROSS ESTIMATTE                       
         ZAP   GROSS,=P'0'                                                      
         ZAP   HRESTMAT,JBCOLVAL+12(6) SAVE HIGHEST REVISION AMOUNT             
         ZAP   HIREVEST,JBCOLVAL+12(6) SAVE HIGHEST REVISION AMOUNT             
         CLI   QOPT1,C'G'          DO THEY WANT GROSS ?                         
         BNE   *+10                NO                                           
         ZAP   HIREVEST,JBCOLVAL+18(6) SAVE HIGHEST REVISION GROSS              
*                                                                               
JB58     CLI   JBCOLTYP,JBCOLTWC                                                
         BNE   JB59                                                             
         MVC   WC,JBCOLWC                                                       
         ZAP   DUB,JBCOLVAL        GET CURRENT NET                              
         CLI   QOPT1,C'G'          DO THEY WANT GROSS ?                         
         BNE   *+10                NO                                           
         ZAP   DUB,JBCOLVAL+6(6)   YES, CHANGE ESTIMATE ALSO                    
*                                                                               
         L     R2,WKCNT                                                         
         LA    R6,1(,R2)                                                        
         ST    R6,WKCNT                                                         
         MH    R2,=AL2(WKLNQ)                                                   
         L     RF,=A(DETAILS)                                                   
         LA    R2,0(R2,RF)                                                      
*                                                                               
         USING DLIND,R2                                                         
*                                                                               
         MVC   WKCD,WC                                                          
         ZAP   WKESTMTE,DUB                                                     
         ZAP   WKACT,=P'0'                                                      
         ZAP   WKHOLD,=P'0'                                                     
*                                                                               
JB59     AH    R3,JBLCOL                                                        
         BCT   R1,JB58                                                          
         B     JEXIT                                                            
*                                                                               
         DROP  R2,R3,R5                                                         
         EJECT ,                                                                
JE6      CLI   MODE,PROCTRNS                                                    
         BNE   JE7                                                              
         L     R4,ADTRANS                                                       
*                                                                               
         USING TRANSD,R4                                                        
*                                                                               
         CLI   TRNSEL,X'44'                                                     
         BNE   JEXIT                                                            
*                                                                               
         USING JXCEPTD,R6                                                       
*                                                                               
         LA    R6,JXBLOCK                                                       
         MVI   JXMODE,JXMPRTRN     INITIALIZE JXBLOCK                           
         L     R1,ADTRANS                                                       
         SH    R1,DATADISP                                                      
         ST    R1,JXAREC                                                        
*                                                                               
         USING ACMD,R1                                                          
*                                                                               
         L     R1,AMONACC                                                       
         MVC   JXAJBBLK,ACMAJOBB   A(JOBBER BLOCK)                              
         MVC   JXAGOBLK,ADGOBLOC   A(GETOPT BLOCK)                              
         ZAP   JXEST,ESTIMATE      NET ESTIMATE VALUE                           
         ZAP   JXGEST,GROSSEST     GROSS ESTIMATE VALUE                         
         ZAP   JXHREST,HRESTMAT    HIGHEST REVISION ESTIMATE VALUE              
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
*                                                                               
         DROP  R1,R6                                                            
*                                                                               
         L     R3,ADGOBLOC                                                      
*                                                                               
         USING GOBLOCKD,R3                                                      
*                                                                               
         TM    TRNSSTAT,X'04'      HELD ITEMS                                   
         BZ    JE60                                                             
         MVI   HOLDSW,C'Y'         DO NOT INCLUDE IN ACTUAL COUNT-BUT           
*        B     JE65                THIS IS NOT SO IN UK - DROP THROUGH          
*                                                                               
JE60     CLC   TRNSANAL,=C'**'     NOT SO IN US-10/89-AS PER IRENE W.           
         BNE   *+8                                                              
         BAS   RE,ORDIT                                                         
*                                                                               
         DROP  R3                                                               
*                                                                               
         TM    TRNSBTCH,X'F0'      TEST FOR VALID MONTH OF ACTIVITY             
         BNO   JE61                SOME OLD TRANSACTIONS DON'T                  
         CLI   TRNSBTCH+1,C'A'     HAVE IT                                      
         BL    JE61                                                             
         CLI   TRNSBTCH+1,C'C'                                                  
         BNH   JE62                                                             
         CLI   TRNSBTCH+1,C'1'                                                  
         BL    JE61                                                             
         CLI   TRNSBTCH+1,C'9'                                                  
         BNH   JE62                                                             
*                                                                               
JE61     GOTO1 ACDATE,DMCB,(1,TRNSDATE),TRNSBTCH,DATCON                         
*                                                                               
JE62     DS    0H                                                               
         TM    TRNSSTAT,X'80'                                                   
         BNO   JE62A                                                            
         CLC   TRNSANAL,=C'**'                                                  
         BE    *+8                                                              
         BAS   RE,SETGROSS         SET GROSS ACCUM (USES DUB AND CASHD)         
*                                                                               
JE62A    ZAP   DUB,TRNSAMNT                                                     
         ZAP   CASHD,=P'0'                                                      
         CLI   QOPT1,C'G'                                                       
         BNE   JE62B                                                            
         MVI   ELCODE,X'50'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   JE62B                                                            
*                                                                               
         USING TRCASHD,R4                                                       
*                                                                               
         CLI   TRCSTYPE,C'D'       GET CASH DISCOUNT FOR GROSS OPTION           
         BNE   JE62B                                                            
         ZAP   CASHD,TRCSAMNT                                                   
*                                                                               
         USING TRANSD,R4                                                        
*                                                                               
JE62B    L     R4,ADTRANS                                                       
         TM    TRNSSTAT,X'80'                                                   
         BO    *+10                                                             
         MP    DUB,=P'-1'                                                       
         CLC   TRNSANAL(2),=C'99'                                               
         BE    DOBILL                                                           
         AP    ACTUAL,DUB                                                       
         CLI   QOPT1,C'G'                                                       
         BNE   AGEIT                                                            
         AP    DUB,CASHD                                                        
         TM    TRNSSTAT,X'01'      NON-COMM                                     
         BO    JE62C                                                            
         BAS   RE,ADCOM            ADD COMMISSION TO DUB                        
*                                                                               
JE62C    SP    DUB,CASHD                                                        
         B     AGEIT                                                            
*                                                                               
DOBILL   AP    BILLING,TRNSAMNT                                                 
*                                                                               
         CLC   TRNSNARR(9),=C'CLIENT WP'   $BILL BILL                           
         BE    DOBILL10                                                         
         CLI   TRNSNARR,C'A'       ALLOCATED BILL?                              
         BNE   DOBILL12                                                         
*                                                                               
DOBILL10 MVI   CLIBILL,C'Y'        THERE IS CLIENT BILLING HERE                 
*                                                                               
DOBILL12 OI    RETFLAG,HASBILLS                                                 
*                                                                               
         L     R5,ADTRANS                                                       
         SH    R5,DATADISP         POINT TO KEY                                 
*                                                                               
         USING ACKEYD,R5                                                        
*                                                                               
         CLI   ACKEYCON+1,C'3'     UNIT 3 CONTRA ACCOUNT BILL                   
         BNE   *+8                                                              
         OI    RETFLAG,RETBILLS    FLAG AS HAVING RETAIL BILLS                  
*                                                                               
         CLI   TRNSLEN,121                                                      
         BNE   AGEIT                                                            
         CLI   QOPT1,C'G'                                                       
         BNE   AGEIT                                                            
         AP    BILLCOM,TRNSNARR+15(6)   COMMISSION                              
*        MP    TRNSNARR+15(6),=P'-1'                                            
         XI    TRNSNARR+15+5,X'01'                                              
         AP    DUB,TRNSNARR+15(6)                                               
*                                                                               
AGEIT    ZAP   PKEIGHT,DUB                                                      
         CLI   BALIND,C'D'         DEBIT JOB                                    
         BE    *+10                                                             
         MP    PKEIGHT,=P'-1'                                                   
         CP    PKEIGHT,=P'0'                                                    
         BL    JE65                                                             
*                                                                               
         LA    R5,FRSMNTH+6             GET RIGHT BUCKET FOR AGEING             
         LA    R7,MDAC+24                                                       
         LA    R3,3                                                             
*                                                                               
JE63     GOTO1 ACDATE,DMCB,(6,TRNSBTCH),(R5)                                    
         CLI   DMCB,8                                                           
         BNL   JE64                EQUAL OR HIGH                                
         SH    R5,=H'2'                                                         
         SH    R7,=H'8'                                                         
         BCT   R3,JE63                                                          
*                                                                               
JE64     AP    0(8,R7),PKEIGHT                                                  
*                                                                               
JE65     CLC   TRNSANAL(2),=C'99'                                               
         BE    JEXIT                                                            
*                                                                               
         L     R3,WKCNT            PUT IT IN WORK TABLE                         
         L     R5,=A(DETAILS)                                                   
         LTR   R3,R3                                                            
         BZ    JE67                                                             
*                                                                               
         USING DLIND,R5                                                         
*                                                                               
         CLC   TRNSANAL,WKCD                                                    
         BE    JE68                                                             
         LA    R5,WKLNQ(,R5)                                                    
         BCT   R3,*-14                                                          
*                                                                               
JE67     MVC   WKCD,TRNSANAL       NO EST FOR WORK-CODE                         
         ZAP   WKESTMTE,=P'0'                                                   
         L     R3,WKCNT                                                         
         LA    R3,1(,R3)                                                        
         ST    R3,WKCNT                                                         
         ZAP   WKACT,=P'0'                                                      
         ZAP   WKHOLD,=P'0'                                                     
*---------------------------------------------------------------------          
*        NOTE: HELD ITEMS INCLUDED IN W/C TOTALS AS PER IWEX, 4/90              
*---------------------------------------------------------------------          
JE68     AP    WKACT,DUB           ADD TO WORK-CODE ACTUAL                      
         TM    TRNSSTAT,X'04'                                                   
         BNO   JEXIT                                                            
*                                                                               
         CLI   QOPT1,C'G'          WANT GROSS                                   
         BNE   JE68A                                                            
         AP    WKHOLD,TRGROSS      KEEP GROSS HELD                              
         B     JEXIT                                                            
*                                                                               
JE68A    AP    WKHOLD,TRNSAMNT     ADD NET TO WORK CODE HELD                    
         B     JEXIT                                                            
*                                                                               
         DROP  R4                                                               
         EJECT ,                                                                
JE7      CLI   MODE,ACCLAST                                                     
         BNE   JE8                                                              
*                                                                               
         USING JXCEPTD,R6                                                       
*                                                                               
         LA    R6,JXBLOCK                                                       
         MVI   JXMODE,JXMLSACC                                                  
         MVC   JXAREC,ADACC                                                     
*                                                                               
         USING ACMD,R1                                                          
*                                                                               
         L     R1,AMONACC                                                       
         MVC   JXAJBBLK,ACMAJOBB   A(JOBBER BLOCK)                              
         MVC   JXAGOBLK,ADGOBLOC   A(GETOPT BLOCK)                              
         ZAP   JXEST,ESTIMATE      NET ESTIMATE VALUE                           
         ZAP   JXGEST,GROSSEST     GROSS ESTIMATE VALUE                         
         ZAP   JXHREST,HRESTMAT    HIGHEST REVISION ESTIMATE VALUE              
         ZAP   JXBIGBAL,BIGBAL     FOR EXCEPTION REASON B                       
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
*                                                                               
         MVI   REASONC,C'N'                                                     
         MVI   JXMODE,JXMCHK       IS REASON C ON THIS JOB                      
         MVI   JXCODE,C'C'                                                      
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
         BNE   *+8                                                              
         MVI   REASONC,C'Y'        FLAG TO PRINT WITH NO BILLABLE               
*                                                                               
         MVI   REASON7,C'N'                                                     
         MVI   JXMODE,JXMCHK       IS REASON 7 ON THIS JOB                      
         MVI   JXCODE,C'7'                                                      
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
         BNE   JE71F                                                            
         MVI   REASON7,C'Y'                                                     
*                                                                               
         DROP  R1                                                               
*                                                                               
JE71F    L     R4,ADACC                                                         
         XC    ALNKEL,ALNKEL                                                    
         MVI   ELCODE,LNKELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         ST    R4,ALNKEL                                                        
*                                                                               
         L     R3,ADGOBLOC                                                      
*                                                                               
         USING GOBLOCKD,R3                                                      
*                                                                               
         L     R2,AMONACC                                                       
*                                                                               
         USING ACMD,R2                                                          
*                                                                               
         L     R2,ACMAJOBB                                                      
*                                                                               
         USING JBLOCKD,R2                                                       
*                                                                               
         MVC   P2SAVE,SPACES                                                    
         ZAP   JOBHOLD,=P'0'                                                    
         MVI   HIREVSW,C'N'                                                     
*                                                                               
         CLI   REASON7,C'Y'        IS THIS  A REASON 7 JOB                      
         BNE   JE72                NO                                           
*                                                                               
         CLI   JXHELD,C'Y'         IF THERE ARE HELD ITEMS                      
         BE    JE72                THEN PRINT THIS JOB                          
*                                                                               
         CLI   REASONC,C'Y'        REASON C                                     
         BE    JE72                THEN PRINT THIS JOB                          
*                                                                               
         CLI   QOPT2,C'Y'          PRINT ALL, OR OPTION 7 JOBS ONLY             
         BE    JE77                                                             
         CLI   QOPT2,C'7'                                                       
         BNE   JEXIT                                                            
*                                                                               
         USING JXCEPTD,R6                                                       
*                                                                               
         LA    R6,JXBLOCK                                                       
         XC    JXCODES,JXCODES                                                  
         MVI   JXCODES,C'7'                                                     
         MVI   JXNCODES,1                                                       
         B     JE77                                                             
*                                                                               
JE72     CLI   REASON7,C'Y'        IS REASON 7 HERE                             
         BNE   JE72A                                                            
*                                                                               
         MVI   JXMODE,JXMREM       REMOVE REASON 7                              
         MVI   JXCODE,C'7'                                                      
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
*                                                                               
         DROP  R6                                                               
*                                                                               
JE72A    ZAP   OVER,ACTUAL                                                      
         SP    OVER,ESTIMATE                                                    
         ZAP   UNBILLED,ACTUAL                                                  
         SP    UNBILLED,BILLING                                                 
*                                                                               
         ZAP   PL13,ESTIMATE                                                    
         L     R1,OVEREST                                                       
         CVD   R1,DUB                                                           
         MP    PL13,DUB+4(4)                                                    
         DP    PL13,=P'10000'                                                   
         ZAP   BIGEST,PL13(10)                                                  
*                                                                               
         USING JXCEPTD,R6                                                       
*                                                                               
         LA    R6,JXBLOCK                                                       
*                                                                               
         CLI   QOPT2,C'8'          SPECIFIC REQ FOR JOB PAST CLOSING            
         BE    ER90                YES                                          
*                                                                               
         MVI   JXMODE,JXMREM       REMOVE REASON 8, IF THERE                    
         MVI   JXCODE,C'8'                                                      
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
*                                                                               
ER90     DS    0H                                                               
         MVI   JXMODE,JXMCHK                                                    
         MVI   JXCODE,C'1'         IS REASON 1 FLAGGED ON JOB                   
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
         BNE   ERE0                NO                                           
*                                                                               
         MVI   JXMODE,JXMREM       IF SO, REMOVE REASON C                       
         MVI   JXCODE,C'C'         IS REASON 1 FLAGGED ON JOB                   
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
*                                                                               
ERE0     BAS   RE,SOFTHEAD         WRITE MISSING USERFIELDS TO P2SAVE           
*                                                                               
         MVI   JXMODE,JXMCHK                                                    
         MVI   JXCODE,C'F'         ARE THERE HIGHER REV AWAITING APP            
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
         BNE   ERG0                NO                                           
*                                                                               
         MVC   CUREST,JBHIAPP      SAVE ESTIMATE NUMBERS FOR REPORT             
         MVC   HIREV,JBHIREV                                                    
         MVI   HIREVSW,C'Y'        FLAG TO PRINT HIGHER REV ON REPORT           
*                                                                               
ERG0     DS    0H                                                               
         B     JE75                                                             
*                                                                               
         DROP  R2,R5                                                            
*                                                                               
JE75     DS    0H                                                               
         CLI   QOPT2,C'Y'          ALL JOBS                                     
         BE    JE77                                                             
*                                                                               
         CLI   QOPT2,C' '          ALL BAD JOBS                                 
         BNE   JE76                NO, LOOK FOR A SPECIFIC CODE                 
*                                                                               
         CLC   QSELECT,SPACES      ANY LIST FOR MULTIPLE EXCEPTIONS             
         BNE   JE75SEL             YES                                          
*                                                                               
         USING JXCEPTD,R6                                                       
*                                                                               
         LA    R6,JXBLOCK                                                       
         CLI   JXNCODES,0          ANY EXCEPTION REASONS                        
         BE    JEXIT               NO                                           
*                                                                               
*        PRINT BILLABLE JOBS ONLY, EXCEPT WHEN ...                              
*                                                                               
         CLI   HOLDSW,C'Y'         IF ANY HELD ITEMS PRINT JOB                  
         BE    JE77                                                             
         CLI   REASONC,C'Y'        IF REASON C PRINT JOB                        
         BE    JE77                                                             
*                                                                               
         CLI   GOBILTYP,C'P'       IF NO UNBILLED CHARGES WITH PROG'IVE         
         BE    JE75AA              BILLTYPE, SKIP JOB                           
         CLI   GOBILTYP,C'U'                                                    
         BE    JE75AA                                                           
         CLI   GOBILTYP,C'T'                                                    
         BNE   JE77                                                             
*                                                                               
JE75AA   CP    UNBILLED,=P'0'                                                   
         BE    JEXIT                                                            
         B     JE77                                                             
*                                                                               
*                                                                               
JE75SEL  LA    RF,L'QSELECT        PROCESS EXCEPTIONS IN QSELECT                
         LA    RE,QSELECT                                                       
*                                                                               
JE75B    LA    R7,JXCODES                                                       
*                                                                               
JE75C    CLI   0(R7),0                                                          
         BE    JE75D                                                            
         CLC   0(1,RE),0(R7)                                                    
         BE    JE77                FOUND CODE IN                                
*                                                                               
         LA    R7,2(,R7)                                                        
         B     JE75C                                                            
*                                                                               
JE75D    LA    RE,1(,RE)                                                        
         BCT   RF,JE75B                                                         
*                                                                               
         B     JEXIT                                                            
*                                                                               
JE76     LA    R7,JXCODES          PROCESS AN EXCEPTION IN QOPT2                
*                                                                               
JE76A    CLI   0(R7),0                                                          
         BE    JEXIT               REASON IN OPT2 IS NOT IN LIST                
*                                                                               
         CLC   QOPT2,0(R7)         REQUESTED REASON IN LIST?                    
         BE    JE77                YES                                          
*                                                                               
         LA    R7,2(,R7)                                                        
         B     JE76A                                                            
*                                                                               
         DROP  R6                                                               
*                                                                               
JE77     LA    R6,JBAC        BUILD JOB LINE                                    
         L     R5,=A(DETAILS)   FROM WK-CODE TABLE                              
         L     R1,WKCNT                                                         
         LTR   R1,R1                                                            
         BZ    JE77A                                                            
*                                                                               
         USING DLIND,R5                                                         
*                                                                               
JE77A1   CLC   WKCD,=C'**'                                                      
         BE    JE77A3                                                           
         AP    0(8,R6),WKESTMTE                                                 
         AP    8(8,R6),WKACT                                                    
         AP    JOBHOLD,WKHOLD                                                   
*                                                                               
JE77A3   LA    R5,WKLNQ(,R5)                                                    
         BCT   R1,JE77A1                                                        
*                                                                               
JE77A    ZAP   16(8,R6),BILLING                                                 
         CLI   QOPT1,C'G'          GROSS                                        
         BNE   JE78                                                             
         AP    16(8,R6),BILLCOM    ADD COMMISSION                               
         ZAP   UNBILLED,8(8,R6)    ACTUAL GROSS                                 
         SP    UNBILLED,16(8,R6)   BILLING GROSS                                
         ZAP   BALNC,UNBILLED                                                   
         MVI   BALIND,C'D'         DEBIT BALANCE                                
         CP    BALNC,=P'0'                                                      
         BNL   *+14                                                             
         MP    BALNC,=P'-1'        MAKE IT POSITIVE                             
         MVI   BALIND,C'C'         CREDIT JOB                                   
*                                                                               
         USING JXCEPTD,R6                                                       
*                                                                               
JE78     LA    R6,JXBLOCK          ADD EM UP BY REASON CODE                     
         LA    R7,JXCODES                                                       
*                                                                               
JE78B    CLI   0(R7),0             END OF EXCEPTION REASON LIST                 
         BE    JE79                                                             
*                                                                               
         L     R2,=A(EXCPTB)                                                    
*                                                                               
JE78C    CLC   0(1,R7),0(R2)       FIND CODE IN TABLE                           
         BNE   JE78D                                                            
         AP    70(4,R2),=P'1'                                                   
         AP    74(8,R2),UNBILLED                                                
         AP    82(4,R2),=P'1'                                                   
         AP    86(8,R2),UNBILLED                                                
         B     JE78E               GET NEXT CODE FROM LIST                      
*                                                                               
JE78D    LA    R2,EXCPTBLN(,R2)                                                 
         CLI   0(R2),X'FF'                                                      
         BNE   JE78C                                                            
         DC    H'0'                WHAT REASON IS THIS????                      
*                                                                               
JE78E    LA    R7,2(,R7)           PUT NEXT CODE IN THE LIST                    
         B     JE78B                                                            
*                                                                               
         DROP  R6                                                               
*                                                                               
JE79     CLI   FRSTCLI,C'Y'        FIRST FOR  CLIENT                            
         BNE   *+8                                                              
         BAS   RE,DIRCTRY          PRINT DIRECTORY                              
         MVI   FRSTCLI,C'N'                                                     
*                                                                               
         CLI   FRSTPRD,C'Y'        FIRST FOR  PRODUCT                           
         BNE   *+8                                                              
         BAS   RE,PRDHD            PRINT PRODUCT NAME                           
         MVI   FRSTPRD,C'N'                                                     
*                                                                               
         BAS   RE,REPORT                                                        
         SR    R1,R1                                                            
         IC    R1,JBLNTH                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+1(0),JBLN         JOB NUMBER                                   
         LA    R5,P(R1)                                                         
         LA    R6,JBLN(R1)                                                      
         LA    R7,42               MAX L'NAME+MAX L'CODE                        
         SR    R7,R1               SPACE REMAINING                              
*                                                                               
         MVC   PJOB,SPACES         SPACE TO PRINT NAME, DATES, BT, ETC.         
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   PJOB(0),2(R6)                                                    
         LA    R6,PJOB                                                          
         LA    R6,2(R7,R6)                                                      
         GOTO1 DATCON,DMCB,(1,JBSTRT),(8,0(R6))                                 
*                                                                               
         MVI   8(R6),C'-'                                                       
         GOTO1 DATCON,DMCB,(1,JBEND),(8,9(R6))                                  
*                                                                               
         LA    R6,18(,R6)                                                       
*&&UK*&& LA    R6,1(,R6)                                                        
*                                                                               
         MVC   0(4,R6),=C'B/T='                                                 
         MVC   4(1,R6),GOBILTYP                                                 
         LA    R6,6(,R6)                                                        
         TM    JBSTAT,X'40'        FLAG CLOSED JOBS                             
         BZ    *+10                                                             
         MVC   0(3,R6),=C'(C)'                                                  
         GOTO1 ADSQUASH,DMCB,PJOB,100,RR=PRELOC                                 
         L     R7,DMCB+4                                                        
*&&US*&& GOTO1 CHOPPER,DMCB,((R7),PJOB),(30,3(R5)),(C'P',4)                     
*&&UK*&& GOTO1 CHOPPER,DMCB,((R7),PJOB),(23,3(R5)),(C'P',4)                     
*                                                                               
         GOTO1 FORMAT,DMCB,JBAC,5                                               
*                                                                               
         L     R2,AMONACC                                                       
*                                                                               
         USING ACMD,R2                                                          
*                                                                               
         L     R2,ACMAJOBB                                                      
*                                                                               
         USING JBLOCKD,R2                                                       
*                                                                               
         CLI   JBNEWEST,C'Y'       IS THIS A NEW ESTIMATE JOB?                  
         BNE   *+8                 NO                                           
         BAS   RE,PRTESTNU         PRINT ESTIMATE NUMBER                        
*                                                                               
         DROP  R2                                                               
*                                                                               
         CLI   HIREVSW,C'Y'        ARE THERE ESTIMATES AWAITING APPRVL          
         BNE   *+8                 NO                                           
         BAS   RE,PRTHIREV         YES, PRINT THEM                              
*                                                                               
         USING JXCEPTD,R6                                                       
*                                                                               
         LA    R6,JXBLOCK          PRINT EXCEPTION REASONS                      
*&&US*&& MVC   P+111(11),JXCODES                                                
*&&UK*&& MVC   P+101(11),JXCODES                                                
         CLC   P2SAVE,SPACES                                                    
         BE    JE80                                                             
         MVC   PSECOND+111(11),P2SAVE                                           
         CLC   P2SAVE+12(11),SPACES                                             
         BE    JE80                                                             
         MVC   PTHIRD+111(11),P2SAVE+12                                         
         CLC   P2SAVE+24(11),SPACES                                             
         BE    JE80                                                             
         MVC   PFOURTH+111(11),P2SAVE+24                                        
*                                                                               
JE80     BAS   RE,REPORT                                                        
*                                                                               
         CLC   QAPPL(2),SPACES     PRINT A USER FIELD UNDER JOB?                
         BNH   JA799                                                            
         L     R4,ADACC                                                         
         MVI   ELCODE,ACUFELQ                                                   
         AH    R4,DATADISP                                                      
*                                                                               
JA791    BAS   RE,NEXTEL                                                        
         BNE   JA799                                                            
*                                                                               
         USING ACUFD,R4                                                         
*                                                                               
         CLC   ACUFCODE,QAPPL      PRINT THIS USER FIELD?                       
         BNE   JA791               NO, GET NEXT.                                
         CLI   ACUFLEN,32          ANYTHING THERE                               
         BNH   JA791               NO                                           
         MVC   P+5(2),ACUFCODE     PRINT USER CODE                              
         MVI   P+7,C'='                                                         
         ZIC   R1,ACUFLEN                                                       
         SH    R1,=H'33'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+8(0),ACUFDATA     PRINT USER DATA                              
         BAS   RE,REPORT                                                        
*                                                                               
JA799    CLI   QOPT5,C'Y'          OPTION TO DO W-CODE TOTALS                   
         BE    JE79A               FOR EACH JOB                                 
         CLI   PROGPROF+5,C'Y'     OPTION TO SUPRESS WC TOTALS                  
         BE    JE79B               YES                                          
*                                                                               
         LA    R1,JXCODES          LIST OF EXCEPT REASONS                       
*                                                                               
JE7991   CLI   0(R1),0             END OF REASON LIST                           
         BE    JE79B               YES                                          
*                                                                               
         CLI   0(R1),C'1'          PRINT W/C'S IF REASON 1,C OR H               
         BE    JE79A                                                            
         CLI   0(R1),C'C'                                                       
         BE    JE79A                                                            
         CLI   0(R1),C'H'                                                       
         BE    JE79A                                                            
         LA    R1,2(,R1)                                                        
         B     JE7991                                                           
*                                                                               
JE79A    BAS   RE,WKTOT            PRINT WORK CODE TOTALS                       
*                                                                               
JE79B    AP    PRAC(8),JBAC(8)                                                  
         AP    PRAC+8(8),JBAC+8(8)                                              
         AP    PRAC+16(8),JBAC+16(8)                                            
         MVC   JBAC(24),PZR                                                     
         CP    UNBILLED,=P'0'                                                   
         BE    JE7B                                                             
*                                                                               
         LA    R7,MDAC+24                                                       
         LA    R4,4                                                             
         ZAP   PKEIGHT,BALNC                                                    
*                                                                               
JE79C    CP    0(8,R7),PKEIGHT     BUCKET TO JOB TOTAL                          
         BNH   *+10                                                             
         ZAP   0(8,R7),PKEIGHT     BUCKET HIGH IT GETS REMAINDER                
         SP    PKEIGHT,0(8,R7)     GET NEW REMAINDER                            
         CLI   BALIND,C'C'                                                      
         BNE   *+10                                                             
         MP    0(8,R7),=P'-1'      CREDIT JOB                                   
         SH    R7,=H'8'                                                         
         BCT   R4,JE79C                                                         
*                                                                               
         LA    R7,MDAC                                                          
         ZAP   MDTOT,=P'0'                                                      
         LA    R4,4                                                             
         AP    MDTOT,0(8,R7)                                                    
         LA    R7,8(,R7)                                                        
         BCT   R4,*-10                                                          
         CLI   BALIND,C'C'                                                      
         BNE   *+10                                                             
         MP    BALNC,=P'-1'                                                     
*                                                                               
         MVC   BUFKY,SPACES                                                     
         MVI   BUFKY,3             TYPE 3                                       
         MVC   BUFKY+1(1),FCRULMED      MEDIA                                   
         MVC   BUFAC,MDAC               TOTALS                                  
         BAS   RE,PUT                                                           
         LA    R4,5                                                             
         LA    R7,MDAC                                                          
         LA    R6,AGAC                                                          
         AP    0(8,R6),0(8,R7)                                                  
         LA    R7,8(,R7)                                                        
         LA    R6,8(,R6)                                                        
         BCT   R4,*-14                                                          
*                                                                               
         CLI   OFFICE,C'Y'         ANALYSIS SUMMARY                             
         BNE   JE7B                                                             
         MVI   BUFKY,2             TYPE                                         
         L     R5,PROFADD                                                       
*                                                                               
         USING ACPROFD,R5                                                       
*                                                                               
         MVC   BUFKY+1(2),ACPROFFC                                              
         MVC   BUFKY+3(1),FCRULMED                                              
         BAS   RE,PUT                                                           
*                                                                               
JE7B     DS    0H                                                               
         CLI   PROGPROF,C'Y'                                                    
         BNE   JEXIT                                                            
*                                                                               
         USING ACSTATD,R5                                                       
*                                                                               
         L     R5,ADACCSTA                                                      
         LA    R4,DATELIST                                                      
         LA    RF,36                                                            
*                                                                               
JE7D     CLC   0(2,R4),ACSTBFDT                                                 
         BE    JE7F                                                             
         LA    R4,11(,R4)                                                       
         BCT   RF,JE7D                                                          
         B     JEXIT                                                            
*                                                                               
JE7F     AP    2(6,R4),UNBILLED    ADD TO COUNTER FOR MONTH OF OPENING          
         AP    8(3,R4),=P'1'                                                    
         B     JEXIT                                                            
*                                                                               
         DROP  R3,R4,R5                                                         
         EJECT ,                                                                
JE8      CLI   MODE,LEVBLAST                                                    
         BNE   JE9                                                              
         CLC   PRAC(24),PZR                                                     
         BNE   JE8A                                                             
         MVI   PRODAMT,0           LAST PRODUCT HAS NO DATA                     
         CLI   LASTSW,1            ANY PREVIOUS PRODUCTS                        
         BE    *+8                 YES                                          
         MVI   LASTSW,0            NO PREVIOUS PRODUCT                          
         B     JEXIT                                                            
*                                                                               
JE8A     MVI   LASTSW,1            GOOD PRODUCT                                 
         MVI   PRODSW,0            NO PRODUCT TOTALS                            
         MVI   PRODAMT,1           LAST PRODUCT HAS DATA                        
         AP    CLAC(8),PRAC(8)                                                  
         AP    CLAC+8(8),PRAC+8(8)                                              
         AP    CLAC+16(8),PRAC+16(8)                                            
         CLC   PRAC(24),SVAMTS                                                  
         BE    JE8B                                                             
         MVI   PRODSW,1            PRODUCT TOTALS                               
         BAS   RE,REPORT                                                        
         MVI   SPACING,2                                                        
         GOTO1 FORMAT,DMCB,PRAC,5                                               
         MVC   P+14(14),=C'PRODUCT TOTALS'                                      
*                                                                               
JE8B     MVC   PRAC(24),PZR                                                     
         B     JEXIT                                                            
         EJECT ,                                                                
JE9      CLI   MODE,LEVALAST                                                    
         BNE   JEA                                                              
         CLC   CLAC(24),PZR                                                     
         BNE   JE9AA                                                            
         AP    REQJOBS,CLIJOBS                                                  
         ZAP   CLIJOBS,=P'0'                                                    
         B     JE9B                                                             
*                                                                               
JE9AA    MVI   SPACING,2                                                        
         CLC   CLAC(24),SVAMTS     IF NO CLIENT TOTALS NECESSARY                
         BE    *+12                SKIP UNDERLINE                               
         BAS   RE,TOTBOX   UNDERLINE AND PRINT LAST PRODUCT TOTALS              
         B     JE9AC                                                            
         BAS   RE,REPORT           PRINT LAST PRODUCT TOTALS                    
*                                                                               
JE9AC    MVC   PRLN,SPACES                                                      
         AP    RQAC(8),CLAC(8)                                                  
         AP    RQAC+8(8),CLAC+8(8)                                              
         AP    RQAC+16(8),CLAC+16(8)                                            
         CLC   CLAC(24),SVAMTS                                                  
         BE    JE9A                                                             
         BAS   RE,REPORT                                                        
         GOTO1 FORMAT,DMCB,CLAC,5                                               
         MVC   P+15(13),=C'CLIENT TOTALS'                                       
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
JE9A     MVC   BUFKY,SPACES        AGEING LIST BY CLIENT                        
         MVI   BUFKY,1             TO BUFFALO                                   
         MVC   BUFKY+1(45),CLLN    CLIENT CODE AND NAME                         
         MVC   BUFAC,AGAC                                                       
         BAS   RE,PUT                                                           
*                                                                               
JE9B     MVC   CLAC(24),PZR                                                     
         MVI   FORCEHED,C'Y'       EXCEPTION BY CLIENT                          
         MVI   RCSUBPRG,5                                                       
         L     R3,=A(EXCPTB)                                                    
*                                                                               
JE9C     CP    82(4,R3),=P'0'                                                   
         BE    JE9D                                                             
*                                                                               
         USING JXCEPTD,R6                                                       
*                                                                               
         LA    R6,JXBLOCK                                                       
         MVI   JXMODE,JXMLIT       GET EXCEPTION LITERAL                        
         MVC   JXCODE,0(R3)                                                     
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
         ICM   RF,15,JXALIT                                                     
*                                                                               
         USING JXMSGD,RF                                                        
*                                                                               
         MVC   P+1(70),JXMLONG                                                  
*                                                                               
         DROP  R6,RF                                                            
*                                                                               
         EDIT  (P4,82(R3)),(7,P+78)                                             
         EDIT  (P8,86(R3)),(12,P+88),2,MINUS=YES                                
         ZAP   82(4,R3),=P'0'                                                   
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
JE9D     LA    R3,94(,R3)                                                       
         CLI   0(R3),X'FF'                                                      
         BNE   JE9C                                                             
         CP    CLIJOBS,=P'0'                                                    
         BE    JEXIT                                                            
         BAS   RE,MYREPORT                                                      
         MVC   P+1(23),=C'NUMBER OF OPEN JOBS FOR'                              
         MVC   P+25(6),=C'CLIENT'                                               
         EDIT  CLIJOBS,(6,P+32)                                                 
         BAS   RE,MYREPORT                                                      
         AP    REQJOBS,CLIJOBS                                                  
         ZAP   CLIJOBS,=P'0'                                                    
         MVI   RCSUBPRG,0                                                       
         CLI   PROGPROF,C'Y'                                                    
         BNE   JEXIT                                                            
         MVI   RCSUBPRG,6                                                       
*&&US*&& MVI   FORCEHED,C'Y'                                                    
*&&UK                                                                           
         GOTO1 ACREPORT                                                         
         MVC   P+1(13),=C'MONTH SUMMARY'                                        
         MVI   PSECOND+1,BOTF                                                   
         CLI   REMOTOP,C'Y'                                                     
         BNE   *+8                                                              
         MVI   PSECOND+1,C'-'                                                   
         MVC   PSECOND+2(12),PSECOND+1                                          
         GOTO1 REPORT                                                           
*&&                                                                             
         LA    R3,DATELIST                                                      
         LA    R4,36                                                            
*                                                                               
JE9F     CP    2(6,R3),=P'0'                                                    
         BE    JE9G                                                             
         MVC   WORK(2),0(R3)                                                    
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(9,P+1)                                     
         EDIT  (P3,8(R3)),(7,P+78)                                              
         EDIT  (P6,2(R3)),(12,P+88),2,MINUS=YES                                 
         GOTO1 REPORT                                                           
*                                                                               
JE9G     DS    0H                                                               
         ZAP   2(6,R3),=P'0'                                                    
         ZAP   8(3,R3),=P'0'                                                    
         LA    R3,11(,R3)                                                       
         BCT   R4,JE9F                                                          
*                                                                               
         MVI   RCSUBPRG,0                                                       
         B     JEXIT                                                            
         EJECT ,                                                                
JEA      CLI   MODE,REQLAST                                                     
         BNE   JEXIT                                                            
*                                                                               
         CLC   RQAC(24),SVAMTS                                                  
         BE    JEA1                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         GOTO1 FORMAT,DMCB,RQAC,5                                               
         MVC   P+14(14),=C'REQUEST TOTALS'                                      
*                                                                               
JEA1     BAS   RE,REPORT                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         XC    BUFKY,BUFKY                                                      
         MVI   BUFKY,1                                                          
         MVC   JBAC(40),PZR                                                     
*                                                                               
JEA2     BAS   RE,GET                                                           
         BNE   JEA3                                                             
*                                                                               
JEA2P    MVI   SPACING,2                                                        
         MVC   P+1(45),BUFKY+1                                                  
         BAS   RE,FORMATA                                                       
         BAS   RE,GET                                                           
         BE    JEA2P                                                            
*                                                                               
         CLC   JBAC(40),SVAMTS                                                  
         BE    JEA3                                                             
         MVC   BUFAC(40),JBAC                                                   
         BAS   RE,REPORT                                                        
         MVC   P+1(6),=C'TOTALS'                                                
         BAS   RE,FORMATA                                                       
*                                                                               
JEA3     CLI   OFFICE,C'Y'                                                      
         BNE   JEA7                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
         XC    BUFKY,BUFKY                                                      
         MVI   BUFKY,2                                                          
         MVC   JBAC(40),PZR                                                     
         BAS   RE,GET                                                           
         BNE   JEA7                                                             
         MVC   OFFICE,BUFKY+1                                                   
*                                                                               
JEA3P    MVC   P+1(2),OFFICE                                                    
         LA    R5,BUFKY+1                                                       
         BAS   RE,OFN              GET OFFICE NAME                              
         MVC   P+4(15),WORK                                                     
*                                                                               
JEA3S    MVC   P+26(1),BUFKY+3                                                  
         LA    R5,BUFKY+3                                                       
         BAS   RE,MEDN             GET MEDIA NAME                               
         MVC   P+28(15),WORK                                                    
         BAS   RE,FORMATA                                                       
         BAS   RE,GET                                                           
         BNE   JEA3T                                                            
         CLC   OFFICE,BUFKY+1                                                   
         MVC   OFFICE,BUFKY+1                                                   
         BE    JEA3S                                                            
         BAS   RE,REPORT                                                        
         B     JEA3P                                                            
*                                                                               
JEA3T    CLC   JBAC(40),SVAMTS                                                  
         BE    JEA7                                                             
         MVC   BUFAC(40),JBAC                                                   
         BAS   RE,REPORT                                                        
         MVC   P+1(6),=C'TOTALS'                                                
         BAS   RE,FORMATA                                                       
*                                                                               
JEA7     MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,3                                                       
         XC    BUFKY,BUFKY                                                      
         MVI   BUFKY,3                                                          
         MVC   JBAC(40),PZR                                                     
         BAS   RE,GET                                                           
         BNE   JEA8                                                             
*                                                                               
JEA7P    MVC   P+26(1),BUFKY+1                                                  
         LA    R5,BUFKY+1                                                       
         BAS   RE,MEDN             GET MEDIA NAME                               
         MVC   P+28(15),WORK                                                    
         BAS   RE,FORMATA                                                       
         BAS   RE,GET                                                           
         BE    JEA7P                                                            
*                                                                               
         CLC   JBAC(40),SVAMTS                                                  
         BE    JEA8                                                             
         BAS   RE,REPORT                                                        
         MVC   BUFAC(40),JBAC                                                   
         MVC   P+1(6),=C'TOTALS'                                                
         BAS   RE,FORMATA                                                       
*                                                                               
JEA8     MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,5                                                       
         L     R3,=A(EXCPTB)                                                    
*                                                                               
JEA8A    CP    70(4,R3),=P'0'                                                   
         BE    JEA8B                                                            
*                                                                               
         USING JXCEPTD,R6                                                       
*                                                                               
         LA    R6,JXBLOCK                                                       
         MVI   JXMODE,JXMLIT       GET EXCEPTION LITERAL                        
         MVC   JXCODE,0(R3)                                                     
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
         ICM   RF,15,JXALIT                                                     
*                                                                               
         USING JXMSGD,RF                                                        
*                                                                               
         MVC   P+1(70),JXMLONG                                                  
*                                                                               
         DROP  R6,RF                                                            
*                                                                               
         EDIT  (P4,70(R3)),(7,P+78)                                             
         EDIT  (P8,74(R3)),(12,P+88),2,MINUS=YES                                
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
JEA8B    LA    R3,94(,R3)                                                       
         CLI   0(R3),X'FF'                                                      
         BNE   JEA8A                                                            
         CP    REQJOBS,=P'0'                                                    
         BE    JEXIT                                                            
         CLI   QACCOUNT,C' '       SPECIFIC CLIENT IN REQUEST                   
         BNE   JEXIT                                                            
         BAS   RE,MYREPORT                                                      
         MVC   P+1(23),=C'NUMBER OF OPEN JOBS FOR'                              
         MVC   P+25(7),=C'REQUEST'                                              
         EDIT  REQJOBS,(6,P+34)                                                 
         BAS   RE,MYREPORT                                                      
*                                                                               
JEXIT    XIT1                                                                   
         EJECT ,                                                                
BUILDTE  NTR1                                                                   
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK+6)                                
*        GOTO1 DATCON,DMCB,(5,0),(0,WORK+6)         TODAY'S DATE YYMMDD         
         MVI   DTBYTE,C'N'                                                      
         MVC   WORK+10(2),=C'01'                    FIRST DAY IN MONTH          
         CLC   QEND,SPACES                                                      
         BE    BLDT2                                                            
         CLI   PROGPROF+1,X'00'                                                 
         BE    BLDT1                                                            
         CLI   PROGPROF+1,C'N'     USE  REQUEST DATES FOR CLOSED ?              
         BE    BLDT1               NO,  SKIP                                    
         CLI   QOPT3,C' '                                                       
         BE    BLDT1                                                            
         MVI   DTBYTE,C'Y'                                                      
         B     BLDT2                                                            
*                                                                               
BLDT1    DS    0H                  USE  END DATE                                
         MVC   WORK+6(4),QEND                                                   
*                                                                               
BLDT2    DS    0H                                                               
         MVC   DATLIN,SPACES                                                    
         MVC   DATLIN2,SPACES                                                   
         GOTO1 DATCON,DMCB,(0,WORK+6),(6,DATLIN+45) MMM/YY                      
         MVC   DATLIN2+42(9),=C'AND AFTER'                                      
         GOTO1 ACDATE,DMCB,(0,WORK+6),FRSMNTH+6,DATCON                          
*                                                                               
*                                                   END  DATE  MINUS            
         GOTO1 ADDAY,DMCB,(C'M',WORK+6),WORK,F'-3'       THREE MONTHS           
         CLC   QSTART,SPACES                                                    
         BE    BLDT3                                                            
         CLI   DTBYTE,C'Y'                                                      
         BE    BLDT3                                                            
         CLC   QSTART(4),WORK                       <     THREE MONTHS?         
         BH    *+10                                 YES,  SKIP                  
         MVC   WORK(4),QSTART                       USE   START DATE            
         MVC   WORK+4(2),=C'01'                                                 
*                                                                               
BLDT3    DS    0H                                   MIN   THREE MONTHS          
         GOTO1 DATCON,DMCB,(0,WORK),(6,DATLIN+3)    MMM/YY                      
         GOTO1 ACDATE,DMCB,(0,WORK),FRSMNTH,DATCON                              
         MVC   DATLIN2(9),=C'AND PRIOR'                                         
*                                                                               
         MVC   WORK+18(6),WORK                      SAVE  START DATE            
         XR    R3,R3                                                            
*                                                                               
*                                  COUNT MONTHS     START DATE  TO              
BLDT4    DS    0H                                   END   DATE                  
*                                                   CUR   DATE  PLUS            
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+12,F'1'        ONE   MONTH           
         CLC   WORK+12(4),WORK+6                                                
         BE    BLDT5                                                            
         LA    R3,1(,R3)                                                        
         MVC   WORK(6),WORK+12                      NEXT  MONTH                 
         B     BLDT4                                                            
*                                                                               
BLDT5    DS    0H                  SET   UP  COLUMN 2     AND   3               
         MVC   WORK(6),WORK+18                      START DATE                  
         XR    R2,R2                                                            
         D     R2,=F'2'            R3 =  NUM OF MONTHS/2  IN    COL  2          
         LR    R4,R3               R4 =  NUM OF MONTHS/2  IN    COL  3          
         AR    R3,R2               EXTRA MONTH (IF ANY)   IN    COL  2          
*                                                   START DATE  PLUS            
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+18,F'1'        ONE   MONTH           
         MVC   WORK(6),WORK+18                                                  
*                                                                               
*                                  COLUMN 2                                     
         GOTO1 DATCON,DMCB,(0,WORK),(6,DATLIN+17)   MMM/YY                      
         GOTO1 ACDATE,DMCB,(0,WORK),FRSMNTH+2,DATCON                            
         CH    R3,=H'1'            ONLY   THREE     MONTHS ?                    
         BNH   BLDT8               YES,   SKIP                                  
         BCTR  R3,0                ONE    MONTH DONE ALREADY                    
*                                                                               
BLDT6    DS    0H                  FIND   THE NUM   OF    MONTHS                
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+18,F'1'  NEXT  MONTH                 
         MVC   WORK(6),WORK+18                                                  
         BCT   R3,BLDT6                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(6,DATLIN2+17)  MMM/YY                      
         MVC   DATLIN2+12(4),=C'THRU'                                           
*                                                                               
BLDT8    DS    0H                  COLUMN 3                                     
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+18,F'1'  NEXT  MONTH                 
         MVC   WORK(6),WORK+18                                                  
         GOTO1 DATCON,DMCB,(0,WORK),(6,DATLIN+31)   MMM/YY                      
         GOTO1 ACDATE,DMCB,(0,WORK),FRSMNTH+4,DATCON                            
         CH    R4,=H'1'            ONLY   THREE     MONTHS ?                    
         BE    BLDT10              YES,   DONE                                  
         BCTR  R4,0                                                             
*                                                                               
BLDT9    DS    0H                  LOOP   FOR NUM   OF    MONTHS                
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+18,F'1'  NEXT  MONTH                 
         MVC   WORK(6),WORK+18                                                  
         BCT   R4,BLDT9                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(6,DATLIN2+31)  MMM/YY                      
         MVC   DATLIN2+26(4),=C'THRU'                                           
*                                                                               
BLDT10   DS    0H                                                               
         MVC   DATLIN+59(5),=C'TOTAL'                                           
         B     JEXIT                                                            
         EJECT ,                                                                
*              ROUTINE TO EXTRACT EXTRA PROFILE INFORMATION                     
         SPACE 1                                                                
         USING ACHEIRD,R5                                                       
         SPACE 1                                                                
PROFXTRA NTR1                                                                   
         L     R5,ADLDGHIR                                                      
         ZIC   R4,ACHRLEVB                                                      
         L     R2,ADACC                                                         
         MVC   JOBCODE,SPACES                                                   
         LA    RF,3(R4,R2)                                                      
         ZIC   R1,ACHRLEVC                                                      
         SR    R1,R4                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   JOBCODE(0),0(RF)                                                 
         L     R3,ADGOBLOC                                                      
*                                                                               
         USING GOBLOCKD,R3                                                      
*                                                                               
         MVI   NEEDEST,C'Y'                                                     
         CLI   GONEEDES,C'Y'                                                    
         BE    *+8                                                              
         MVI   NEEDEST,C'N'                                                     
*                                                                               
PX4      CP    GOMINBIL,=P'5000'                                                
         BE    PX6                                                              
         ZAP   SMALL,GOMINBIL                                                   
*                                                                               
PX6      CP    GOOVRPER,=P'10000'                                               
         BE    PX8                                                              
         ZAP   DUB,GOOVRPER                                                     
         CVB   R4,DUB                                                           
         ST    R4,OVEREST                                                       
*                                                                               
PX8      ZAP   OVERAMT,=P'0'                                                    
         CP    GOOVRAMT,=P'0'      MAXIMUM OVER AMOUNT                          
         BE    PXIT                                                             
         ZAP   OVERAMT,GOOVRAMT                                                 
*                                                                               
PXIT     XIT1                                                                   
*                                                                               
         DROP  R3,R5                                                            
         EJECT ,                                                                
CLIENT   NTR1                                                                   
         LA    R5,CLLN                                                          
         L     R4,ADLDGHIR                                                      
*                                                                               
         USING ACHEIRD,R4                                                       
*                                                                               
         SR    R3,R3                                                            
         SR    R6,R6                                                            
         IC    R6,ACHRLEVA                                                      
         L     R2,ADHEIRA                                                       
         MVC   CLICODE,SPACES                                                   
         LA    RF,3(,R2)                                                        
         ZIC   R1,ACHRLEVA                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CLICODE(0),0(RF)                                                 
         L     R4,ADLVANAM                                                      
         B     BOTH                                                             
*                                                                               
PRODUCT  NTR1                                                                   
         LA    R5,PRLN                                                          
         L     R4,ADLDGHIR                                                      
         SR    R3,R3                                                            
         SR    R6,R6                                                            
         IC    R3,ACHRLEVA                                                      
         IC    R6,ACHRLEVB                                                      
         L     R2,ADHEIRB                                                       
         MVC   PRDCODE,SPACES                                                   
         LA    RF,3(R3,R2)                                                      
         ZIC   R1,ACHRLEVB                                                      
         SR    R1,R3                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRDCODE(0),0(RF)                                                 
         L     R4,ADLVBNAM                                                      
         B     BOTH                                                             
*                                                                               
JOB      NTR1                                                                   
         L     R4,ADPROFIL                                                      
         ST    R4,PROFADD                                                       
         LA    R5,JBLN                                                          
         L     R4,ADLDGHIR                                                      
         SR    R3,R3                                                            
         SR    R6,R6                                                            
         IC    R3,ACHRLEVB                                                      
         IC    R6,ACHRLEVC                                                      
         L     R2,ADACC                                                         
         L     R4,ADACCNAM                                                      
*                                                                               
BOTH     MVC   0(45,R5),SPACES                                                  
         MVC   45(24,R5),PZR                                                    
         SR    R6,R3                                                            
         LA    R3,3(R2,R3)                                                      
         BCTR  R6,0                                                             
         STC   R6,JBLNTH                                                        
         EX    R6,*+8                                                           
         B     *+10                                                             
*                                                                               
         MVC   0(0,R5),0(R3)                                                    
         LA    R6,2(R5,R6)                                                      
*                                                                               
         USING ACNAMED,R4                                                       
*                                                                               
         CLI   0(R4),ACNMELQ                                                    
         BNE   JEXIT                                                            
         SR    R2,R2                                                            
         IC    R2,ACNMLEN                                                       
         SH    R2,=H'3'                                                         
         EX    R2,*+8                                                           
         B     JEXIT                                                            
         MVC   0(0,R6),ACNMNAME                                                 
         EJECT ,                                                                
         SPACE 1                                                                
         USING GOBLOCKD,R3                                                      
         SPACE 1                                                                
ADCOM    NTR1                                                                   
         L     R3,ADGOBLOC                                                      
         ZAP   PL13,DUB            ADD COMMISSION TO DUB                        
         MP    PL13,GOAGYCOM                                                    
         SRP   PL13,64-6,5         RATE IS FOUR DECIMAL PLACES                  
         AP    DUB,PL13                                                         
         B     JEXIT                                                            
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        ADD THE GROSS AMOUNT OF THIS TRAN TO THE GROSS ACCUM                   
*        THIS USES DUB AND CASHD                                                
*---------------------------------------------------------------------          
*                                                                               
         USING TRANSD,R4                                                        
*                                                                               
SETGROSS NTR1                                                                   
         L     R4,ADTRANS                                                       
         ZAP   DUB,TRNSAMNT                                                     
         ZAP   CASHD,=P'0'                                                      
         MVI   ELCODE,X'50'                                                     
*                                                                               
SETG10   BAS   RE,NEXTEL                                                        
         BNE   SETG30                                                           
*                                                                               
         USING TRCASHD,R4                                                       
*                                                                               
         CLI   TRCSTYPE,C'D'       GET CASH DISCOUNT FOR GROSS OPTION           
         BNE   SETG10              TRY AGAIN                                    
         ZAP   CASHD,TRCSAMNT                                                   
*                                                                               
         USING TRANSD,R4                                                        
*                                                                               
SETG30   L     R4,ADTRANS                                                       
         TM    TRNSSTAT,X'01'      NON-COMM                                     
         BO    SETG40                                                           
         AP    DUB,CASHD                                                        
         BAS   RE,ADCOM            ADD COMMISSION TO DUB                        
         SP    DUB,CASHD                                                        
*                                                                               
SETG40   AP    GROSS,DUB                                                        
         ZAP   TRGROSS,DUB         GROSS TRANSACTION AMOUNT                     
         B     JEXIT                                                            
*                                                                               
         DROP  R4                                                               
         EJECT ,                                                                
DIRCTRY  NTR1                                                                   
         MVI   RCSUBPRG,4                                                       
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,REPORT                                                        
         MVC   P+1(28),=C'DIRECTORY OF EXCEPTION CODES'                         
*                                                                               
         USING JXCEPTD,R6                                                       
*                                                                               
         LA    R6,JXBLOCK                                                       
         MVI   JXMODE,JXMLIT       GET EXCEPTION LITERAL                        
         MVI   JXCODE,C'1'         FIRST                                        
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
         ICM   R3,15,JXALIT                                                     
*                                                                               
         USING JXMSGD,R3                                                        
*                                                                               
DIRCTNXT CLI   0(R3),X'FF'                                                      
         BE    DIRCTOUT                                                         
         CLI   0(R3),C'B'                                                       
         BNE   DIRCT050                                                         
         CP    BIGBAL,=P'0'                                                     
         BE    DIRCT150                                                         
*                                                                               
DIRCT050 MVC   P+35(70),JXMLONG                                                 
         CLI   P+35,C'2'                                                        
         BNE   DIRCT100                                                         
*&&UK                                                                           
         MVI   P+1,BOTF                                                         
         CLI   REMOTOP,C'Y'        IF PRINTING REMOTE                           
         BNE   *+8                                                              
         MVI   P+1,C'-'            USE CHICKEN TRACKS                           
*&&                                                                             
*&&US*&& MVI   P+1,C'-'                                                         
         MVC   P+2(27),P+1                                                      
*                                                                               
DIRCT100 BAS   RE,REPORT                                                        
*                                                                               
DIRCT150 LA    R3,JXMLEN(,R3)                                                   
         B     DIRCTNXT                                                         
*                                                                               
DIRCTOUT DS    0H                                                               
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     JEXIT                                                            
*                                                                               
         DROP  R6                                                               
         EJECT 1                                                                
PRDHD    NTR1                                                                   
         BAS   RE,REPORT                                                        
         MVC   P+1(45),PRLN                                                     
         LA    R3,P+46                                                          
         CLI   0(R3),X'40'                                                      
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
*&&US*&& MVI   PSECOND+1,C'-'                                                   
*&&UK                                                                           
         MVI   PSECOND+1,BOTF                                                   
         CLI   REMOTOP,C'Y'        IF PRINTING REMOTE                           
         BNE   *+8                                                              
         MVI   PSECOND+1,C'-'      USE CHICKEN TRACKS                           
*&&                                                                             
         LA    R2,P+2                                                           
         SR    R3,R2                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   PSECOND+2(0),PSECOND+1                                           
         BAS   RE,REPORT                                                        
         B     JEXIT                                                            
         EJECT ,                                                                
ORDIT    NTR1                      READ ORDER DATA                              
         ZAP   ORDERS2,=P'0'                                                    
*                                                                               
ORD2     CLI   0(R4),0                                                          
         BE    ORD10                                                            
         CLI   0(R4),X'68'                                                      
         BE    ORD6                                                             
*                                                                               
ORD4     ZIC   RE,1(,R4)                                                        
         AR    R4,RE                                                            
         B     ORD2                                                             
*                                                                               
         USING ACOAMTD,R4                                                       
*                                                                               
ORD6     CLC   ACOAWC,SPACES       SKIP ORDERS W/O WORKCODE (EXPENSE)           
         BNH   ORD4                                                             
         ZAP   DUB,ACOAMT                                                       
         SP    DUB,ACOAIVAL                                                     
         CLI   QOPT1,C'G'          GROSS OPTION                                 
         BNE   *+8                                                              
         BAS   RE,ADCOM                                                         
         AP    ORDERS,DUB                                                       
         AP    ORDERS2,DUB                                                      
         B     ORD4                                                             
*                                                                               
ORD10    L     R3,WKCNT            PUT IT IN WORK TABLE                         
         L     R5,=A(DETAILS)                                                   
         LTR   R3,R3                                                            
         BZ    ORD12                                                            
*                                                                               
         USING DLIND,R5                                                         
*                                                                               
         CLC   WKCD,=C'**'                                                      
         BE    ORD14                                                            
         LA    R5,WKLNQ(,R5)                                                    
         BCT   R3,*-14                                                          
*                                                                               
ORD12    MVC   WKCD,=C'**'                                                      
         ZAP   WKESTMTE,=P'0'                                                   
         L     R3,WKCNT                                                         
         LA    R3,1(,R3)                                                        
         ST    R3,WKCNT                                                         
         ZAP   WKACT,=P'0'                                                      
         ZAP   WKHOLD,=P'0'                                                     
*                                                                               
ORD14    AP    WKACT,ORDERS2       ADD TO WORK-CODE ACTUAL                      
         B     JEXIT                                                            
         EJECT ,                                                                
*              ROUTINE TO PRINT UNDERLINE AND PRODUCT TOTALS                    
         SPACE 2                                                                
TOTBOX   NTR1                                                                   
         CLI   PROGPROF+3,C'Y'     DON'T IF NEW PAGE/PRODUCT                    
         BNE   TOTBOX1                                                          
         BAS   RE,REPORT                                                        
         B     JEXIT                                                            
*                                                                               
TOTBOX1  L     R2,ADBOX                                                         
*                                                                               
         USING BOXD,R2                                                          
*                                                                               
         LTR   R2,R2                                                            
         BZ    JEXIT                                                            
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS(R1)                                                   
         CLI   PRODSW,1            PRODUCT TOTALS                               
         BE    TOTBOX2                                                          
         BCTR  R1,0                                                             
*                                                                               
TOTBOX2  MVI   0(R1),C'M'          MOVE IN UNDERLINE                            
         ST    R1,SAVELINE         STORE ADDRESS OF UNDERLINE                   
         BAS   RE,REPORT                                                        
         L     R1,SAVELINE                                                      
         MVI   0(R1),C' '          ERASE UNDERLINE                              
         B     JEXIT                                                            
         EJECT ,                                                                
REPORT   NTR1                                                                   
*                                                                               
         BAS   RE,MYREPORT                                                      
         B     JEXIT                                                            
         SPACE 2                                                                
MYREPORT ST    RE,SAVERE                                                        
         GOTO1 ACREPORT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT ,                                                                
*              SEE IF MISSING ANY SOFT HEADLINES                                
*                                                                               
         USING JXCEPTD,R6                                                       
*                                                                               
SOFTHEAD NTR1                                                                   
         LA    R6,JXBLOCK                                                       
         MVC   P2SAVE,SPACES                                                    
         OC    JXMISUFS,JXMISUFS   ANY MISSING USERFIELDS                       
         BZ    SFTX                                                             
*                                                                               
         LA    R2,P2SAVE                                                        
         MVC   0(2,R2),=CL2'NO'                                                 
         LA    R2,3(,R2)                                                        
         MVC   0(L'JXMISUFS,R2),JXMISUFS                                        
         OC    0(L'JXMISUFS,R2),SPACES                                          
*                                                                               
SFTX     B     JEXIT                                                            
         EJECT ,                                                                
*&&US                                                                           
FORMAT   NTR1                                                                   
         L     R2,0(,R1)                                                        
         CLI   7(R1),5                                                          
         BNE   *+10                                                             
         MVC   SVAMTS(24),0(R2)                                                 
         CP    0(8,R2),=P'0'       ESTIMATE                                     
         BE    FORMAT2                                                          
         EDIT  (P8,0(R2)),(12,P+42),2,MINUS=YES                                 
*                                                                               
FORMAT2  LA    RF,JOBHOLD                                                       
         CLI   7(R1),3                                                          
         BNE   *+8                                                              
         LA    RF,16(,R2)          WK HOLD ITEMS                                
         CP    8(8,R2),=P'0'       ACTUAL                                       
         BE    FORMAT2A                                                         
         CP    8(8,R2),=P'999999999'                                            
         BH    FORMAT2B                                                         
         EDIT  (P8,8(R2)),(11,P+55),2,MINUS=YES                                 
*                                                                               
FORMAT2A CLI   MODE,ACCLAST        FOR JOBS ONLY                                
         BNE   FORMAT3                                                          
         CP    0(8,RF),=P'0'       PRINT HELD ITEMS ON SECOND LINE              
         BE    FORMAT3                                                          
*                                                                               
         CLI   7(R1),3                                                          
         BE    FORM2AA                                                          
         LA    R3,PSECOND+100                                                   
         EDIT  (P8,0(RF)),(10,0(R3)),2,MINUS=YES                                
         MVI   0(R3),C'H'                                                       
*                                                                               
FORM2AA  LA    R3,PSECOND+55                                                    
         EDIT  (P8,0(RF)),(11,0(R3)),2,MINUS=YES                                
         MVI   0(R3),C'H'                                                       
         B     FORMAT3                                                          
*                                                                               
FORMAT2B LA    R3,PSECOND+54                                                    
         EDIT  (P8,8(R2)),(12,0(R3)),2,MINUS=YES                                
         CLI   MODE,ACCLAST        FOR JOBS ONLY                                
         BNE   FORMAT3                                                          
         CP    0(8,RF),=P'0'       PRINT HELD ITEMS ON THIRD LINE               
         BE    FORMAT3                                                          
         CLI   7(R1),3                                                          
         BE    FORM2BA                                                          
*                                                                               
         LA    R3,PTHIRD+100                                                    
         EDIT  (P8,0(RF)),(10,0(R3)),2,MINUS=YES                                
         MVI   0(R3),C'H'                                                       
*                                                                               
FORM2BA  LA    R3,PTHIRD+55                                                     
         EDIT  (P8,0(RF)),(11,0(R3)),2,MINUS=YES                                
         MVI   0(R3),C'H'                                                       
*                                                                               
FORMAT3  CLI   7(R1),5                                                          
         BNE   FORMAT5                                                          
         CP    16(8,R2),=P'0'      BILLING                                      
         BE    FORMAT4                                                          
         CP    16(8,R2),=P'999999999'                                           
         BH    FORMAT3B                                                         
         EDIT  (P8,16(R2)),(11,P+87),2,MINUS=YES                                
         B     FORMAT4                                                          
*                                                                               
FORMAT3B LA    R3,PSECOND+86                                                    
         EDIT  (P8,16(R2)),(12,0(R3)),2,MINUS=YES                               
*                                                                               
FORMAT4  ZAP   PWRK,8(8,R2)        ACTUAL                                       
         SP    PWRK,16(8,R2)       MINUS BILLING                                
         CP    PWRK,=P'0'          UNBILLED                                     
         BE    FORMAT5                                                          
         EDIT  (P8,PWRK),(11,P+99),2,MINUS=YES                                  
*                                                                               
FORMAT5  ZAP   PWRK,8(8,R2)        ACTUAL                                       
         SP    PWRK,0(8,R2)        MINUS ESTIMATE                               
         CP    PWRK,=P'0'          OVER                                         
         BE    JEXIT                                                            
         EDIT  (P8,PWRK),(12,P+67),2,MINUS=YES                                  
         CLI   P+78,C' '                                                        
         BNE   *+8                                                              
         MVI   P+78,C'+'                                                        
         CLI   P+78,C'-'                                                        
         BNE   *+8                                                              
         MVI   P+78,C' '                                                        
*                                                                               
FORMAT6  CP    0(8,R2),=P'0'                                                    
         BE    JEXIT                                                            
         CP    0(8,R2),=P'2100000000'                                           
         BH    JEXIT                                                            
*                                                                               
         ZAP   PWRK2,PWRK          AMOUNT OVER/UNDER                            
         MP    PWRK2,=P'1000'                                                   
         DP    PWRK2,2(6,R2)       DIVIDED BY ESTIMATE                          
         OI    PWRK2+6,X'0F'                                                    
         EDIT  (P7,PWRK2),(6,P+79),1                                            
         MVC   P+85(1),P+78                                                     
         CLI   P+79,C' '                                                        
         BE    JEXIT                                                            
         MVC   P+79(7),=C'  HIGH '                                              
         B     JEXIT                                                            
*&&                                                                             
         EJECT ,                                                                
*&&UK                                                                           
FORMAT   NTR1                                                                   
         L     R2,0(,R1)                                                        
         CLI   7(R1),5                                                          
         BNE   *+10                                                             
         MVC   SVAMTS(24),0(R2)                                                 
         CP    0(8,R2),=P'0'       ESTIMATE                                     
         BE    FORMAT2                                                          
         EDIT  (P8,0(R2)),(12,P+32),2,MINUS=YES                                 
*                                                                               
FORMAT2  LA    RF,JOBHOLD                                                       
         CLI   7(R1),3                                                          
         BNE   *+8                                                              
         LA    RF,16(,R2)          WK HOLD ITEMS                                
         CP    8(8,R2),=P'0'       ACTUAL                                       
         BE    FORMAT2A                                                         
         EDIT  (P8,8(R2)),(11,P+45),2,MINUS=YES                                 
*                                                                               
FORMAT2A CLI   MODE,ACCLAST        FOR JOBS ONLY                                
         BNE   FORMAT3                                                          
         CP    0(8,RF),=P'0'       PRINT HELD ITEMS ON SECOND LINE              
         BE    FORMAT3                                                          
         LA    R3,PSECOND+47                                                    
         EDIT  (P8,0(RF)),(11,0(R3)),2,MINUS=YES                                
         MVI   0(R3),C'H'                                                       
*                                                                               
FORMAT3  CLI   7(R1),5                                                          
         BNE   FORMAT5                                                          
         CP    16(8,R2),=P'0'      BILLING                                      
         BE    FORMAT4                                                          
         EDIT  (P8,16(R2)),(11,P+77),2,MINUS=YES                                
*                                                                               
FORMAT4  ZAP   PWRK,8(8,R2)        ACTUAL                                       
         SP    PWRK,16(8,R2)       MINUS BILLING                                
         CP    PWRK,=P'0'          UNBILLED                                     
         BE    FORMAT5                                                          
         EDIT  (P8,PWRK),(11,P+89),2,MINUS=YES                                  
*                                                                               
FORMAT5  ZAP   PWRK,8(8,R2)        ACTUAL                                       
         SP    PWRK,0(8,R2)        MINUS ESTIMATE                               
         CP    PWRK,=P'0'          OVER                                         
         BE    JEXIT                                                            
         EDIT  (P8,PWRK),(12,P+57),2,MINUS=YES                                  
         CLI   P+68,C' '                                                        
         BNE   *+8                                                              
         MVI   P+68,C'+'                                                        
         CLI   P+68,C'-'                                                        
         BNE   *+8                                                              
         MVI   P+68,C' '                                                        
*                                                                               
FORMAT6  CP    0(8,R2),=P'0'                                                    
         BE    JEXIT                                                            
         CP    0(8,R2),=P'2100000000'                                           
         BH    JEXIT                                                            
*                                                                               
         ZAP   PWRK2,PWRK          AMOUNT OVER/UNDER                            
         MP    PWRK2,=P'1000'                                                   
         DP    PWRK2,2(6,R2)       DIVIDED BY ESTIMATE                          
         OI    PWRK2+6,X'0F'                                                    
         EDIT  (P7,PWRK2),(6,P+70),1                                            
         MVC   P+76(1),P+69                                                     
         CLI   P+70,C' '                                                        
         BE    JEXIT                                                            
         MVC   P+70(7),=C'  HIGH '                                              
         B     JEXIT                                                            
*&&                                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        PRTESTNU - THIS JOB USES NEW EXTIMATES, PRINT THE R # OF THE           
*                   ESTIMATE BEFORE THE ESTIMATE VALUE                          
*---------------------------------------------------------------------          
*                                                                               
PRTESTNU NTR1                                                                   
         L     R2,AMONACC                                                       
*                                                                               
         USING ACMD,R2                                                          
*                                                                               
         L     R2,ACMAJOBB                                                      
*                                                                               
         USING JBLOCKD,R2                                                       
*                                                                               
         OC    JBCURVER,JBCURVER                                                
         BZ    PRTESTX                                                          
*                                                                               
         LA    R2,JBCURVER                                                      
         LA    R3,P+39                                                          
         BAS   RE,PHIEDNUM                                                      
*                                                                               
PRTESTX  B     JEXIT                                                            
*                                                                               
         DROP R2                                                                
*                                                                               
*---------------------------------------------------------------------          
*        PRTHIREV - THIS JOB HAS BEEN FLAGGED WITH EXCEPTION REASON             
*                   'F', SO PRINT THE VALUE OF THE HIGHER ESTIMATE              
*                   BELOW THE CURRENT ESTIMATE                                  
*---------------------------------------------------------------------          
*                                                                               
PRTHIREV NTR1                                                                   
         LA    R2,HIREVEST                                                      
         LA    R3,PSECOND+42                                                    
         EDIT  (P8,0(R2)),(12,0(R3)),2,MINUS=YES                                
         LA    R2,CUREST                                                        
         LA    R3,P+39                                                          
         BAS   RE,PHIEDNUM                                                      
*                                                                               
         LA    R2,HIREV                                                         
         LA    R3,PSECOND+39                                                    
         BAS   RE,PHIEDNUM                                                      
*                                                                               
         B     JEXIT                                                            
*                                                                               
PHIEDNUM MVI   0(R3),C'R'                                                       
         EDIT  (B1,0(R2)),(3,1(R3)),ALIGN=LEFT                                  
         BR    RE                                                               
         EJECT ,                                                                
WKTOT    NTR1                                                                   
         L     R5,=A(DETAILS)                                                   
         L     R3,WKCNT                                                         
         LTR   R3,R3                                                            
         BZ    JEXIT                                                            
*                                                                               
         LA    R2,WKLNQ                                                         
         GOTO1 XSORT,DMCB,(R5),(R3),(R2),2,0                                    
*                                                                               
         USING DLIND,R5                                                         
*                                                                               
WKT1     CLI   PROGPROF+4,C'Y'     SUPPRESS ZERO WORK-CODES                     
         BNE   *+14                                                             
         CLC   WKESTMTE(24),=3PL8'0'                                            
         BE    WKT8                                                             
*&&UK                                                                           
         MVC   P+15(2),WKCD                                                     
         MVC   P+18(15),=CL15'OTHERS'                                           
*&&                                                                             
*&&US                                                                           
         MVC   P+10(2),WKCD                                                     
         MVC   P+13(15),=CL15'OTHERS'                                           
*&&                                                                             
*                                                                               
         CLC   WKCD,=C'**'         PURCHASE ORDER?                              
         BNE   WKT1A                                                            
*&&US*&& MVC   P+13(15),=CL15'ORDERS'                                           
*&&UK*&& MVC   P+18(15),=CL15'ORDERS'                                           
         B     WKT1B                                                            
*                                                                               
WKT1A    CLC   WKCD,=C'99'         BILLING?                                     
         BNE   WKT1B                                                            
*&&US*&& MVC   P+13(15),=CL15'BILLING'                                          
*&&UK*&& MVC   P+18(15),=CL15'BILLING'                                          
*                                                                               
WKT1B    L     R2,ADLEDGER                                                      
         AH    R2,DATADISP                                                      
         SR    R4,R4                                                            
*                                                                               
WKT2     CLI   0(R2),0                                                          
         BE    WKT4                                                             
         CLI   0(R2),X'12'                                                      
         BNE   WKT10                                                            
*                                                                               
         USING ACANALD,R2                                                       
*                                                                               
         CLC   WKCD,ACANCODE                                                    
         BNE   WKT10                                                            
*&&US*&& MVC   P+13(15),ACANDESC                                                
*&&UK*&& MVC   P+18(15),ACANDESC                                                
*                                                                               
WKT4     CLC   WKCD,=C'**'         OUTPUT ORDER AMOUNT IN PARENS                
         BNE   WKT6                AND NOTHING ELSE FOR ORDERS                  
*&&UK*&& EDIT  WKACT,(11,P+45),2,MINUS=YES,BRACKET=YES                          
*&&US*&& EDIT  WKACT,(11,P+42),2,MINUS=YES,BRACKET=YES                          
         B     WKT7                                                             
*                                                                               
WKT6     GOTO1 FORMAT,DMCB,WKESTMTE,3                                           
*                                                                               
WKT7     BAS   RE,REPORT                                                        
*                                                                               
WKT8     LA    R5,WKLNQ(,R5)                                                    
         BCT   R3,WKT1                                                          
         BAS   RE,REPORT                                                        
         B     JEXIT                                                            
*                                                                               
WKT10    IC    R4,1(,R2)                                                        
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R4                                                            
         B     WKT2                                                             
         EJECT ,                                                                
PUT      MVC   COMMAND,=CL4'PUT'                                                
         B     RDBUFF                                                           
*                                                                               
GET      MVC   COMMAND,=CL4'HIGH'                                               
         CLI   BUFKY+1,0                                                        
         BE    RDBUFF                                                           
         MVC   COMMAND,=CL4'SEQ'                                                
*                                                                               
RDBUFF   NTR1                                                                   
         MVC   SVBUF,BUFKY                                                      
         GOTO1 BUFFALO,DMCB,COMMAND,ABUFF,BUFKY,1                               
         CLI   DMCB+8,0                                                         
         BNE   JEXIT                                                            
         CLC   COMMAND,=CL4'PUT'                                                
         BE    JEXIT                                                            
         CLC   SVBUF,BUFKY         CONDITION CHECKED AFTER                      
         B     JEXIT               RETURN                                       
         EJECT ,                                                                
FORMATA  NTR1                                                                   
         MVC   SVAMTS(40),BUFAC                                                 
         LA    R5,BUFAC                                                         
         LA    R3,5                                                             
         LA    R6,JBAC                                                          
         LA    R2,P+41                                                          
*                                                                               
FORMATB  AP    0(8,R6),0(8,R5)                                                  
         CP    0(8,R5),=P'0'                                                    
         BE    FORMATC                                                          
*                                                                               
         EDIT  (P8,0(R5)),(12,0(R2)),2,MINUS=YES                                
*                                                                               
FORMATC  LA    R5,8(,R5)                                                        
         LA    R2,14(,R2)                                                       
         LA    R6,8(,R6)                                                        
         BCT   R3,FORMATB                                                       
         BAS   RE,REPORT                                                        
         B     JEXIT                                                            
         EJECT ,                                                                
OFN      NTR1                                                                   
         MVC   WORK,SPACES         GET OFFICE NAME                              
         L     R2,ADACC            FROM 2D LEDGER                               
         MVC   SVKEY,0(R2)         SAVE KEY FOR MONACC                          
         CLI   NEWOFF,C'Y'         TEST NEW OFFICE COMPANY                      
         BE    OFN2                YES                                          
         MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(1),SVKEY      COMPANY                                      
         MVC   MYKEY+1(2),=C'2D'                                                
         MVC   MYKEY+3(1),0(R5)    OFFICE                                       
         MVC   SVKEY2,MYKEY                                                     
         L     R2,=A(MYIO)                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',MYKEY,(R2)                       
         CLC   MYKEY(5),0(R2)                                                   
         BE    OFN5                FOUND IT                                     
         B     OFN10                                                            
*                                                                               
         USING ACOGKEY,R2                                                       
*                                                                               
OFN2     LA    R2,MYKEY                                                         
         XC    ACOGKEY,ACOGKEY                                                  
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOFF    READ PRODUCTION OFFICE                       
         MVC   ACOGCUL(1),RCCOMPFL                                              
         MVC   ACOGCUL+1(2),=C'SJ'                                              
         MVC   ACOGOFC,0(R5)                                                    
         MVC   SVKEY2,MYKEY        SAVE OFFICE KEY                              
         L     R2,=A(MYIO)                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',MYKEY,(R2)                       
         CLC   ACOGKEY,0(R2)                                                    
         BNE   OFN10                                                            
*                                                                               
OFN5     AH    R2,DATADISP                                                      
         XR    R4,R4                                                            
*                                                                               
OFN6     CLI   0(R2),0                                                          
         BE    OFN10                                                            
         CLI   0(R2),ACNMELQ                                                    
         BE    OFN8                                                             
         IC    R4,1(,R2)                                                        
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R4                                                            
         B     OFN6                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         USING ACNAMED,R2                                                       
*                                                                               
OFN8     IC    R4,ACNMLEN                                                       
         SH    R4,=Y(ACNMNAME-ACNAMED+1)                                        
         EX    R4,*+8                                                           
         B     OFN10                                                            
         MVC   WORK(0),ACNMNAME                                                 
*                                                                               
OFN10    MVC   MYKEY,SVKEY         RE-READ MONACC KEY                           
         L     R2,=A(MYIO)                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',MYKEY,(R2)                       
         CLC   MYKEY,0(R2)                                                      
         BE    JEXIT                                                            
         DC    H'0'                                                             
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
MEDN     NTR1                                                                   
         MVC   WORK,SPACES         GET MEDIA NAMES                              
         L     R2,ADCOMP                                                        
         AH    R2,DATADISP                                                      
         XR    R4,R4                                                            
*                                                                               
MEDN2    CLI   0(R2),0                                                          
         BE    JEXIT                                                            
         CLI   0(R2),ACMDELQ                                                    
         BE    MEDN3                                                            
*                                                                               
MEDN2A   IC    R4,1(,R2)                                                        
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R4                                                            
         B     MEDN2                                                            
*                                                                               
         USING ACMEDIAD,R2                                                      
*                                                                               
MEDN3    CLC   0(1,R5),ACMDCODE                                                 
         BNE   MEDN2A                                                           
         MVC   WORK(12),ACMDDESC+3                                              
         B     JEXIT                                                            
         EJECT ,                                                                
* CONVERTS DIFFERENT FORMAT DATES TO A 2 BYTE MONTH OF SERVICE FORMAT.          
* COMPARES TWO MONTH OF SERVICE DATES AND RETURNS A CODE INDICATING             
* WHICH IS GREATER.                                                             
*                                                                               
* TO CONVERT A DATE.                                                            
*                                                                               
* PARAMETER 1  BYTE  0        INPUT TYPE (SAME AS DATCON)                       
*              BYTES 1-3      A(INPUT DATE)                                     
*                                                                               
* PARAMETER 2  BYTES 1-3      A(OUTPUT DATE)                                    
*                                                                               
* PARAMETER 3  BYTE  1-3      A(DATCON)                                         
*                                                                               
*                                                                               
* TO COMPARE TWO MONTH OF SERVICE DATES.                                        
*    (NOTE: NOT USED IN THIS PROGRAM)                                           
*                                                                               
* PARAMETER 1  BYTE  0        INPUT TYPE = 6                                    
*              BYTES 1-3      A(FIRST DATE)                                     
*                                                                               
* PARAMETER 2  BYTES 1-3      A(SECOND DATE)                                    
*                                                                               
* AFTER COMPARE PARAMETER 1  BYTE 0  X'00'   INVALID INPUT                      
*                                    X'04'   A IS LOWER THAN B                  
*                                    X'08'   A IS EQUAL B                       
*                                    X'0C'   A IS GREATER THAN B                
         SPACE 2                                                                
ACDATE   NTR1                                                                   
         CLI   0(R1),6                                                          
         BNE   DATCON2                                                          
         L     R2,0(,R1)           A(FIRST DATE)                                
         L     R3,4(,R1)           A(SECOND DATE)                               
         MVI   0(R1),0             SET INVALID                                  
*                                                                               
         XR    R4,R4                                                            
         XR    R5,R5                                                            
         IC    R4,0(,R2)           YEAR 1                                       
         IC    R5,0(,R3)           YEAR 2                                       
         SH    R4,=H'240'                                                       
         SH    R5,=H'240'                                                       
         MH    R4,=H'10'                                                        
         LA    R4,YRTAB(R4)                                                     
         AR    R4,R5                                                            
         IC    R5,0(,R4)           SAVE CODE FOR YEAR                           
*                                                                               
         XR    R6,R6                                                            
         XR    R4,R4               VALIDATE MONTH                               
         IC    R4,1(,R2)           MONTH 1                                      
         IC    R6,1(,R3)           MONTH 2                                      
         CH    R4,=H'240'                                                       
         BH    *+8                                                              
         AH    R4,=H'57'           MAKE A C1 AN FA                              
*                                                                               
         CH    R6,=H'240'                                                       
         BH    *+8                                                              
         AH    R6,=H'57'                                                        
*                                                                               
         STC   R5,0(,R1)      SET CODE FROM YEAR COMPARE                        
         CLI   0(R1),8        IF EQUAL                                          
         BE    *+8            MUST COMPARE MONTH                                
         B     ACDXIT         ELSE, OK TO RETURN                                
*                                                                               
         CR    R4,R6                                                            
         BE    ACDXIT                                                           
         MVI   0(R1),LT                                                         
         BL    ACDXIT                                                           
         MVI   0(R1),GT                                                         
         B     ACDXIT                                                           
*                                                                               
DATCON2  L     R2,0(,R1)                                                        
         MVC   LORK+8(6),0(R2)     INPUT DATE TO WORK                           
         MVC   LORK(4),0(R1)       INPUT PARAMETER                              
         LA    R2,LORK+8           RETURN DATE HERE                             
         ST    R2,LORK+4                                                        
         L     R3,4(,R1)           OUTPUT ADDRESS                               
         L     RF,8(,R1)           DATCON                                       
         LA    R1,LORK                                                          
         CLI   LORK,0                                                           
         BE    *+6                                                              
         BASR  RE,RF                                                            
*                                                                               
         MVC   0(1,R3),LORK+9      YEAR                                         
         MVC   1(1,R3),LORK+11     MONTH                                        
         CLC   LORK+10(2),=C'10'                                                
         BL    ACDXIT                                                           
         MVI   1(R3),C'A'                                                       
         CLC   LORK+10(2),=C'11'                                                
         BL    ACDXIT                                                           
         MVI   1(R3),C'B'                                                       
         CLC   LORK+10(2),=C'12'                                                
         BL    ACDXIT                                                           
         MVI   1(R3),C'C'                                                       
*                                                                               
ACDXIT   XMOD1 1                                                                
         SPACE 2                                                                
LORK     DS    4F                                                               
*                                                                               
EQ       EQU   8                                                                
LT       EQU   4                                                                
GT       EQU   12                                                               
         EJECT ,                                                                
*              SECOND    YEAR                                                   
*                   0  1  2  3  4  5  6  7  8  9                                
YRTAB    DC    AL1(EQ,LT,LT,LT,LT,LT,GT,GT,GT,GT)          0   F                
         DC    AL1(GT,EQ,LT,LT,LT,LT,LT,GT,GT,GT)          1   I                
         DC    AL1(GT,GT,EQ,LT,LT,LT,LT,LT,GT,GT)          2   R                
         DC    AL1(GT,GT,GT,EQ,LT,LT,LT,LT,LT,GT)          3   S                
         DC    AL1(GT,GT,GT,GT,EQ,LT,LT,LT,LT,LT)          4   T                
         DC    AL1(LT,GT,GT,GT,GT,EQ,LT,LT,LT,LT)          5                    
         DC    AL1(LT,LT,GT,GT,GT,GT,EQ,LT,LT,LT)          6   Y                
         DC    AL1(LT,LT,LT,GT,GT,GT,GT,EQ,LT,LT)          7   E                
         DC    AL1(LT,LT,LT,LT,GT,GT,GT,GT,EQ,LT)          8   A                
         DC    AL1(LT,LT,LT,LT,LT,GT,GT,GT,GT,EQ)          9   R                
         EJECT ,                                                                
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
EXCPTB   DC    CL70'1 = CAN''T BILL-OVER EST MAX PCT-EXCEPT WHEN %EST,SX        
               PEC AMT,EST NOT REQ'                                             
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
EXCPTBLN EQU   *-EXCPTB                                                         
*                                                                               
         DC    CL70'2 = CAN''T BILL - BALANCE LESS THAN ''LOW''.'               
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
         DC    CL70'3 = WARNING - JOB HAS BEEN INACTIVE FOR 6 MONTHS.'          
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
         DC    CL70'4 = WARNING - BILL TYPE IS %EST; ACTUALS EXCEED PCTX        
               . OF EST.'                                                       
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
         DC    CL70'5 = CAN''T BILL - JOB HAS UNBILLABLE BILLING TYPE.'         
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
         DC    CL70'6 = CAN''T BILL - JOB DOES NOT HAVE AN ESTIMATE (ORX        
                CURRENT=0).'                                                    
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
         DC    CL70'7 = WARNING - JOB HAS NO ESTIMATE, CHARGES OR BILLIX        
               NG'                                                              
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
         DC    CL70'8 = WARNING - JOB IS PAST CLOSING DATE.'                    
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
         DC    CL70'9 = CAN''T BILL - CLT WP,ALLOCATED BILLING OR RETAIX        
               L/NONRETAIL CONFLICT'                                            
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
         DC    CL70'A = CAN''T BILL - JOB HAS UNAPPROVED ESTIMATE.'             
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
EXCPTBB  DC    CL70'B = JOB HAS BALANCE GREATER THAN'                           
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
EXCPTBC  DC    CL70'C = WARNING - CHARGES PLUS ORDERS EXCEED ESTIMATE.'         
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
EXCPTBD  DC    CL70'D = WARNING - JOB HAS HELD INVOICES.'                       
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
EXCPTBE  DC    CL70'E = CAN''T BILL -  MISSING USER FIELD'                      
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
EXCPTBF  DC    CL70'F = WARNING - ADDITIONAL REVISIONS AWAITING APPROVAX        
               L'                                                               
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
EXCPTBG  DC    CL70'G = CAN''T BILL - BILLING SELECT SET TO NO'                 
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
EXCPTBH  DC    CL70'H = CAN''T BILL-OVER EST MAX AMT-EXCEPT WHEN %EST,SX        
               PEC AMT,EST NOT REQ''D'                                          
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
EXCPTBI  DC    CL70'I = WARNING-OVER EST MAX PCT IN GROSS DOLLARS'              
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
*XCPTBJ  DC    CL70'J = WARNING-LINKED AGENCY EXPENSE JOB; NO INTER COM         
*              PANY POSTINGS MADE.'                                             
*        DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
EXCPTBJ  DC    CL70'J = CAN''T BILL - INVALID BILL TYPE FOR STUDIO JOB X        
               WITH LINK.'                                                      
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
EXCPTBK  DC    CL70'K = CAN''T BILL - STUDIO JOB IS MISSING LINK.'              
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
EXCPTBL  DC    CL70'L = CAN''T BILL RETAIL FOR STUDIO JOB WITH LINK.'           
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
         DC    CL70'M = CAN''T BILL - NECESSARY ASP INFORMATION MISSINGX        
               .'                                                               
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
         DC    CL70'N = CAN''T BILL - FORMAT REQUIRES A WORKCODE.'              
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
         DC   CL70'O = CAN''T BILL - ACTUALS EXCEED MAX CE, ADDITIONAL X        
               R''S NEED APPROVAL.'                                             
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
         DC   CL70'P = CAN''T BILL - JOB IS NOT FUNDED.'                        
         DC    PL4'0',PL8'0',PL4'0',PL8'0'                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
EXCPTEND EQU   *                                                                
         EJECT ,                                                                
*              BOX ROUTINES (HOOK)                                              
         SPACE 2                                                                
         ENTRY HOOK                                                             
HOOK     NMOD1 0,*HOOK*                                                         
         L     RC,SAVERC           RESTORE REG C                                
         L     R4,ADBOX                                                         
*                                                                               
         USING BOXD,R4                                                          
*                                                                               
         MVC   MYCOL,SPACES                                                     
         MVC   MYROW,SPACES                                                     
         MVI   MYROW+6,C'T'        SET ROWS                                     
         MVI   MYROW+9,C'M'                                                     
         MVI   MYROW+56,C'B'                                                    
         MVI   MYCOL,C'L'          SET LH MARGIN                                
*                                                                               
*                                  FIND SPROG                                   
         CLI   RCSUBPRG,1                                                       
         BL    HOOK0                                                            
         BE    HOOK1                                                            
         CLI   RCSUBPRG,4                                                       
         BL    HOOK23                                                           
         BE    HOOK4                                                            
         B     HOOK56                                                           
*                                                                               
HOOK0    DS    0H                                                               
*&&US*&& MVI   MYCOL+38,C'C'       SPROG 0                                      
*&&UK*&& MVI   MYCOL+33,C'C'       SPROG 0                                      
*&&US*&& MVI   MYCOL+54,C'C'                                                    
*&&UK*&& MVI   MYCOL+44,C'C'                                                    
*&&US*&& MVI   MYCOL+66,C'C'                                                    
*&&UK*&& MVI   MYCOL+56,C'C'                                                    
*&&US*&& MVI   MYCOL+86,C'C'                                                    
*&&UK*&& MVI   MYCOL+76,C'C'                                                    
*&&US*&& MVI   MYCOL+99,C'C'                                                    
*&&UK*&& MVI   MYCOL+88,C'C'                                                    
*&&US*&& MVI   MYCOL+110,C'C'                                                   
*&&UK*&& MVI   MYCOL+100,C'C'                                                   
         MVI   MYCOL+122,C'R'                                                   
*                                                                               
         CLI   FRSTPRD,C'Y'                                                     
         BE    HOOKLAST                                                         
*                                                                               
         CLC   PRLN,SPACES         IS THERE A PRODUCT TO PRINT                  
         BNH   HOOKLAST            NO                                           
*                                                                               
         MVC   MID1+1(45),PRLN                                                  
         MVC   MID2+5(10),=C'-CONTINUED'                                        
         MVI   FORCEMID,C'Y'                                                    
         B     HOOKLAST                                                         
*                                                                               
HOOK23   MVI   MYCOL+24,C'C'       SPROG 2,3                                    
*                                                                               
HOOK1    LA    R5,MYCOL+40         SPROG 1,2,3                                  
         LA    R6,5                                                             
*                                                                               
HOOK1A   MVI   0(R5),C'C'                                                       
         LA    R5,14(,R5)                                                       
         BCT   R6,HOOK1A                                                        
         BCTR  R5,0                                                             
         MVI   0(R5),C'R'                                                       
         B     HOOKLAST                                                         
*                                                                               
HOOK4    MVI   MYROW+9,C' '        SPROG 4                                      
         MVI   MYCOL+105,C'R'                                                   
         B     HOOKLAST                                                         
*                                                                               
HOOK56   MVI   MYCOL+77,C'C'       SPROG 5,6                                    
         MVI   MYCOL+87,C'C'                                                    
         MVI   MYCOL+100,C'R'                                                   
*                                                                               
HOOKLAST MVC   BOXROWS,MYROW                                                    
         MVC   BOXCOLS,MYCOL                                                    
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
*                                                                               
*        BUILD HEADLINES                                                        
*                                                                               
         MVC   HEAD5+104(6),=C'OPTION'                                          
         MVC   HEAD5+112(5),=C'NET  '                                           
         CLI   QOPT1,C'G'                                                       
         BNE   *+10                                                             
         MVC   HEAD5+112(5),=C'GROSS'                                           
*                                                                               
         CLI   RCSUBPRG,4                                                       
         BE    *+12                                                             
         CLI   RCSUBPRG,0                                                       
         BNE   BHD1                                                             
         CLI   MODE,REQLAST                                                     
         BE    BHDX                                                             
         MVC   HEAD5+1(6),=C'CLIENT'                                            
         MVC   HEAD5+10(45),CLLN                                                
         B     BHDX                                                             
*                                                                               
*&&UK                                                                           
BHD1     MVC   HEAD8+44(70),DATLIN                                              
         MVC   HEAD9+44(70),DATLIN2                                             
*&&                                                                             
*&&US                                                                           
BHD1     MVC   HEAD8+43(70),DATLIN                                              
         MVC   HEAD9+43(70),DATLIN2                                             
*&&                                                                             
         MVC   HEAD5+10(27),=C'SUMMARY OF WORK IN PROGRESS'                     
         CLI   RCSUBPRG,1                                                       
         BNE   BHD2                                                             
         MVC   HEAD8+1(20),=C'CLIENT CODE AND NAME'                             
         B     BHDX                                                             
*                                                                               
BHD2     MVC   HEAD8+26(5),=C'MEDIA'                                            
         CLI   RCSUBPRG,2                                                       
         BNE   BHD3                                                             
         MVC   HEAD8+1(22),=C'ANALYSIS CODE AND NAME'                           
         B     BHDX                                                             
*                                                                               
BHD3     CLI   RCSUBPRG,3                                                       
         BNE   BHD5                                                             
         MVC   HEAD8+1(23),=C'AGENCY WORK IN PROGRESS'                          
*                                                                               
BHD5     CLI   RCSUBPRG,5                                                       
         BNE   BHD6                                                             
         MVC   HEAD8,SPACES                                                     
         MVC   HEAD9,SPACES                                                     
         MVC   HEAD8+1(17),=C'EXCEPTION SUMMARY'                                
         MVC   HEAD5+1(60),SPACES                                               
         CLI   MODE,LEVALAST                                                    
         BNE   *+16                                                             
         MVC   HEAD5+1(6),=C'CLIENT'                                            
         MVC   HEAD5+10(45),CLLN                                                
         MVC   HEAD8+80(6),=C'NUMBER'                                           
         MVC   HEAD9+80(7),=C'OF JOBS'                                          
         MVC   HEAD8+93(5),=C'TOTAL'                                            
         MVC   HEAD9+91(8),=C'UNBILLED'                                         
*                                                                               
BHD6     CLI   RCSUBPRG,6                                                       
         BNE   BHDX                                                             
         MVC   HEAD8,SPACES                                                     
         MVC   HEAD9,SPACES                                                     
         MVC   HEAD8+1(13),=C'MONTH SUMMARY'                                    
         MVC   HEAD5+1(60),SPACES                                               
         MVC   HEAD5+1(6),=C'CLIENT'                                            
         MVC   HEAD5+10(45),CLLN                                                
         MVC   HEAD8+80(6),=C'NUMBER'                                           
         MVC   HEAD9+80(7),=C'OF JOBS'                                          
         MVC   HEAD8+93(5),=C'TOTAL'                                            
         MVC   HEAD9+91(8),=C'UNBILLED'                                         
*                                                                               
BHDX     XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
SAVERC   DC    A(0)                                                             
         LTORG                                                                  
         EJECT ,                                                                
DETAILS  DS    200XL(WKLNQ)                                                     
*                                                                               
MYIO     DS    CL2000                                                           
*                                                                               
         BUFF  LINES=200,ROWS=1,COLUMNS=5,FLAVOR=PACKED,KEYLIST=(46,A)          
         EJECT ,                                                                
DLIND    DSECT                                                                  
WKCD     DS    CL2                 WORK CODE                                    
WKESTMTE DS    PL8                 ESTIMATE                                     
WKACT    DS    PL8                 ACTUAL                                       
WKHOLD   DS    PL8                 HOLD                                         
WKLNQ    EQU   *-DLIND                                                          
*                                                                               
AC5902D  DSECT                                                                  
ABUFF    DS    A                   A(BUFFALOC)                                  
PRELOC   DS    F                                                                
OVEREST  DS    F                                                                
OVERAMT  DS    PL4                                                              
PROFADD  DS    F                                                                
WKCNT    DS    F                                                                
ALNKEL   DS    A                   ADDRESS OF LNK ELEMENT OR X'00'S             
BILFLTSW DS    C                   BILLING SWITCH INDICATOR                     
PL13     DS    PL13                                                             
DATLIN   DS    CL70                DATE HEADLINE                                
DATLIN2  DS    CL70                                                             
FRSMNTH  DS    4CL2                YEAR/MONTH                                   
JBDATE   DS    CL3                                                              
JBSTRT   DS    CL3                                                              
JBEND    DS    CL3                                                              
JBSTAT   DS    CL1                                                              
ELCODE   DS    CL1                                                              
START    DS    CL3                                                              
END      DS    CL3                                                              
WC       DS    CL2                                                              
P2SAVE   DS    CL34                                                             
PEREST   DS    F                                                                
*                                                                               
JBLN     DS    CL45                JOB NAME                                     
JBAC     DS    3PL8                JOB ACCUM                                    
PRLN     DS    CL45                PRODUCT NAME                                 
PRAC     DS    3PL8                PRODUCT ACCUM  EST/ACTUAL/BILLING            
CLLN     DS    CL45                CLIENT NAME                                  
CLAC     DS    3PL8                CLIENT ACCUM                                 
*RQLN    DS    CL45                REQUEST NAME                                 
RQAC     DS    3PL8                REQUEST ACCUM                                
SVAMTS   DS    5PL8                                                             
*                                                                               
PZR      DS    5PL8                ZEROS                                        
NEWOFF   DS    CL1                 Y=NEW OFFICES COMPANY                        
OFFICE   DS    CL2                 IF OFFICE=Y, DO ANALYSIS CODE SUMMRY         
SMALL    DS    PL4                                                              
*                                                                               
FRSTCLI  DS    CL1                                                              
FRSTPRD  DS    CL1                                                              
*                                                                               
ESTIMATE DS    PL8                                                              
ACTUAL   DS    PL8                                                              
BILLING  DS    PL8                                                              
UNBILLED DS    PL8                                                              
BILLCOM  DS    PL8                                                              
CASHD    DS    PL6                                                              
ORDERS   DS    PL6                                                              
ORDERS2  DS    PL6                                                              
TRGROSS  DS    PL6                                                              
JOBHOLD  DS    PL8                                                              
HIREVEST DS    PL8                                                              
HRESTMAT DS    PL8                                                              
GROSS    DS    PL8                                                              
GROSSEST DS    PL8                                                              
*                                                                               
DTBYTE   DS    CL1                                                              
APPSW    DS    CL1                                                              
HOLDSW   DS    CL1                                                              
REASONC  DS    CL1                                                              
REASON7  DS    CL1                                                              
HIREVSW  DS    CL1                                                              
CUREST   DS    CL1                 CURRENT ESTIMATE NUMBER                      
HIREV    DS    CL1                 THE HIGHEST REVISION NUMBER                  
CLIBILL  DS    CL1                 Y, THERE ARE CLIENT OR ALLOC BILLS           
RETFLAG  DS    C                                                                
RETBILLS EQU   1                   JOB HAS RETAIL BILLS                         
RETSCHEM EQU   2                   JOB HAS A SCHEME                             
HASBILLS EQU   4                   JOB HAS BILLS                                
BIGBAL   DS    PL6                                                              
AGAC     DS    4PL8                                                             
AGTOT    DS    PL8                                                              
MDAC     DS    4PL8                                                             
MDTOT    DS    PL8                                                              
*                                                                               
OVER     DS    PL8                                                              
BIGEST   DS    PL8                                                              
REQJOBS  DS    PL6                                                              
CLIJOBS  DS    PL6                                                              
NEEDEST  DS    CL1                                                              
*                                                                               
         DS    0D                                                               
PWRK     DS    PL8                                                              
PWRK2    DS    PL13                                                             
JBLNTH   DS    CL1                                                              
BALNC    DS    PL8                                                              
BALIND   DS    CL1                                                              
PKEIGHT  DS    PL8                                                              
*                                                                               
BUFKY    DS    CL46                                                             
BUFAC    DS    CL40                                                             
*                                                                               
ADBOX    DS    F                                                                
MYROW    DS    CL100                                                            
MYCOL    DS    CL132                                                            
SAVERE   DS    F                                                                
PRODSW   DS    CL1                                                              
LASTSW   DS    CL1                 IF ON, OK TO UNDERLINE                       
PRODAMT  DS    CL1                                                              
SAVELINE DS    F                                                                
*                                                                               
COMMAND  DS    CL4                                                              
SVBUF    DS    CL1                                                              
SVKEY    DS    CL42                                                             
SVKEY2   DS    CL42                                                             
MYKEY    DS    CL42                                                             
REMOTOP  DS    C                   'Y' MEANS PRINTING REMOTE                    
CLICODE  DS    CL6                                                              
PRDCODE  DS    CL6                                                              
JOBCODE  DS    CL6                                                              
DATELIST DS    36CL11                                                           
PJOB     DS    CL100                                                            
JXBLOCK  DS    CL(JXLEN)           INTERFACE BLOCK FOR JOB EXCEPTIONS           
         EJECT ,                                                                
GOBLOCKD DSECT                                                                  
*   ACGOBLOCK                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
GOXBLKD  DSECT                                                                  
*   ACGOXBLOCK                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACGOXBLOCK                                                     
         PRINT ON                                                               
* ACJOBEXCPD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACJOBEXCPD                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDREPMASTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* ACJOBBERD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
         EJECT ,                                                                
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045ACREP5902 10/21/15'                                      
         END                                                                    
