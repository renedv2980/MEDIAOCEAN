*          DATA SET ACREP2502  AT LEVEL 049 AS OF 02/25/20                      
*PHASE AC2502B                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE ACADDBUK                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE BMONVAL                                                                
         SPACE 3                                                                
***********************************************************************         
*              PROFILES                                                         
***********************************************************************         
*        1.  ALL CONTRA POSTINGS TO LEDGER                                      
*        2.  SKIP NIL PRESENT BAL. ACCTS IN TRIBAL                              
*        3.  DETAILS OF THIS MOS POSTINGS ONLY IN TRIBAL                        
*        4.  SUPPRESS LEDGER LEVEL SUMMARY                                      
*        5.  INCLUDE SUMMARY AT REQLAST/RUNLAST                                 
*        6.  POST TO G/L BY OFFICE                                              
*        7.  LEDGER USES G/L OFFICE RULES                                       
*        8.  HONOR LOCKED MOA ON CORRECTION AND SUBSEQUENT RUNS                 
***********************************************************************         
*        RERUN = YES                                                            
*              If not updated then skip                                         
*              If updated then only update ones matching the DATE card          
*                 If marked as updated via $INP, then prior amount              
*        RERUN = ALL                                                            
*              If not updated yet then update                                   
*              If updated then only update ones matching the DATE card          
*                 If marked as updated via $INP, then prior amount              
*   Reverse                                                                     
*              If reverse option then only deal with ones updated               
*                 don't reverse if done via $INP                                
*        RERUN=YES                                                              
*              If not marked as updated but TRSUPDT matches date card           
*                 then reprocess. Don't update GLBRECs. This is used            
*                 to recreate posting file for failed runs or problems          
*              If date doesn't match or marked as udpated then exit             
*        RERUN=ALL                                                              
*              If not marked as updated but TRSUPDT matches date card           
*                 then re-process, without updating GLBRECs for these.          
*              If date doesn't match then reverse as usual, except for          
*                 one done via $INP.                                            
***********************************************************************         
         TITLE 'GENERAL LEDGER UPDATE'                                          
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* JSAY 049 18DEC19 <SPEC-36730> A25 Honor Locked Month of Activity    *         
***********************************************************************         
AC2502   CSECT                                                                  
         PRINT NOGEN                                                            
         USING ACWORKD,RA                                                       
         USING WORKD,RC                                                         
         NMOD1 0,**AC25**,R9                                                    
         L     RA,0(,R1)                                                        
         LA    RC,SPACEND                                                       
         EJECT                                                                  
***********************************************************************         
* FIRST FOR RUN                                                       *         
***********************************************************************         
         CLI   MODE,RUNFRST                                                     
         BNE   RQF00                                                            
         GOTOR RNF00                                                            
                                                                                
***********************************************************************         
* FIRST FOR REQUEST                                                   *         
***********************************************************************         
RQF00    TM    RNSW,RNTB           IS IT TRIAL BALANCE TIME                     
         BNO   RQF02                                                            
         GOTO1 ADTRL,DMCB,(RC)                                                  
         B     XIT                                                              
*                                                                               
RQF02    CLI   MODE,REQFRST                                                     
         BNE   LGF00                                                            
         CLI   SVQSEQ,0                                                         
         BNE   RQF04                                                            
         MVC   SVQSEQ,QSEQ         SAVE REQUESTED SEQUENCE                      
         MVC   SVFCSEQ,FCSEQ                                                    
*                                                                               
RQF04    MVI   QSEQ,C' '           ALWAYS USE ACCOUNT SEQUENCE HERE             
         MVI   FCSEQ,FCSEQACC                                                   
         MVI   FCRDACC,YES                                                      
         LA    R5,REQTOT                                                        
         BAS   RE,CLR              CLEAR REQUEST TOTALS                         
         BRAS  RE,GETSJ                                                         
                                                                                
         USING CPYELD,R4                                                        
         L     R4,ADCMPEL          GET COMPANY ELEMENT                          
         MVC   COOFFC,CPYOFFC      SAVE DEFAULT COMPANY OFFICE                  
         XC    GLMOA,GLMOA         Initialize                                   
         CLI   CPYLN,CPYLN4Q       Must be at least this length                 
         BL    *+10                No                                           
         MVC   GLMOA,CPYGLMOA      Save value                                   
         OC    GLMOA,GLMOA         Was this set ?                               
         BNZ   *+10                Yes                                          
         MVC   GLMOA,=X'FFFF'      No, set to infinity                          
*                                                                               
         MVI   OFFNEW,NO           Not on new offices (1 char)                  
         TM    CPYSTAT4,CPYSOFF2   Is this company on 2 character               
         BZ    RQF06               No, yes force post by office                 
*&&UK*&& OI    RNSW,RNOF                                                        
         MVI   OFFNEW,YES          On new office (2 char)                       
*                                                                               
RQF06    MVI   RQSW,0              INIT REQUEST SWITCHES                        
         CLI   QOPT1,C'C'          Correction run. Backout and repost           
         BNE   *+8                                                              
         OI    RQSW,RQREV                                                       
*                                                                               
         CLI   PROGPROF+4,YES      PRINT OVERALL SUMMARY                        
         BNE   *+8                                                              
         OI    RQSW,RQSM                                                        
*                                                                               
         CLI   QOPT1,C'R'          IS THIS THE RUN AFTER CORRECTION?            
         BNE   RQF08               NO, SKIP THE PROFILE                         
         MVI   QOPT1,C' '          YES, CLEAR THE OPTION                        
*                                                                               
         CLI   PROGPROF+8,YES      HONOR MOA LOCK FOR CORRECTION                
         BNE   *+8                                                              
         OI    RQSW,RQMNT                                                       
*                                                                               
RQF08    CLI   QLEDGER,C' '        IF REQUESTING ONE LEDGER                     
         BE    *+8                                                              
         NI    RQSW,ALL-RQSM       NO REQUEST OVERALL SUMMARY                   
*                                                                               
         TM    RQSW,RQSM           IF OVERALL SUMMARY FOR REQUEST               
         BNO   *+8                                                              
         NI    RNSW,ALL-RNSM       NO OVERALL SUMMARY FOR RUN                   
*                                                                               
         CLI   SYSPROF+1,YES       PROFILE FOR INVOICE REG SYSTEM               
         BNE   *+8                                                              
         OI    RQSW,RQIR                                                        
*                                                                               
         CLI   SYSEQU,ACMSUSA                                                   
         BE    RQF10                                                            
         CLI   QOPT2,C'D'          IN UK DEFAULT IS LIVE                        
         BNE   *+8                                                              
         OI    RQSW,RQDF                                                        
         B     RQF12                                                            
*                                                                               
RQF10    OI    RQSW,RQDF           IN US DEFAULT IS DRAFT                       
         CLI   QOPT2,C'L'                                                       
         BNE   *+8                                                              
         NI    RQSW,ALL-RQDF       MAKE IT LIVE                                 
*                                                                               
RQF12    TM    RQSW,RQDF           IF ANY LIVE REQUESTS                         
         BO    *+8                                                              
         OI    RNSW,RNLV           SET LIVE FOR RUN                             
                                                                                
         TM    RQSW,RQDF           IF ANY LIVE REQUESTS                         
         BZ    *+8                                                              
         MVI   RCWRITE,NO          Force this to no                             
                                                                                
         CLC   SAVEDATE,QSTART                                                  
         BNL   *+10                                                             
         MVC   SAVEDATE,QSTART                                                  
         MVC   MOSDT(1),QSTART+1                                                
         MVC   MOSDT+1(1),QSTART+3                                              
         CLC   QSTART+2(2),=C'10'                                               
         BL    RQF14                                                            
         MVI   MOSDT+1,C'A'                                                     
         CLC   QSTART+2(2),=C'11'                                               
         BL    RQF14                                                            
         MVI   MOSDT+1,C'B'                                                     
         CLC   QSTART+2(2),=C'12'                                               
         BL    RQF14                                                            
         MVI   MOSDT+1,C'C'                                                     
*                                                                               
RQF14    MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,START3)                                  
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(6,HDATE)                                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,45                                        
         MVC   WORK+10(2),=C'01'                                                
         SR    R2,R2                                                            
         BCTR  R2,0                                                             
         GOTO1 (RF),(R1),WORK+6,WORK,(R2)                                       
         GOTO1 DATCON,DMCB,(0,WORK),(1,ENDATE)                                  
         MVI   ERLGR,X'FF'         MARK END OF ERROR LIST                       
         XC    AGLRULEC,AGLRULEC   ASSUME NO G/L RULES @ CMP LEVEL              
         MVI   ELCODE,GLRELQ       LOOK FOR RULES ELEMENT X'E6'                 
         L     R4,ADCMPEL                                                       
         BAS   RE,NEXTEL                                                        
         BNE   *+8                                                              
         ST    R4,AGLRULEC         SAVE ADDRESS OF RULE ELEMENT                 
         CLI   QOPT7,C'T'          TRACE TB                                     
         BNE   *+8                                                              
         OI    RNSW,RNTRC                                                       
*                                                                               
         MVI   FCRDMOSP,NO         TEST FOR MOS PASSIVES                        
         CLI   QACCOUNT,C' '       TEST SPECIFIC ACCOUNTS                       
         BH    RQFX                                                             
                                                                                
         USING MOSPASD,R2          BUILD PASSIVE POINTER KEY                    
         LA    R2,IOA                                                           
         XC    MOSPKEY,MOSPKEY                                                  
         MVI   MOSPTYP,MOSPTYPQ                                                 
         MVC   MOSPCPY,RCCOMPFL                                                 
         MVC   WORK,IOA                                                         
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,MOSPASD,MOSPASD                       
         CLC   MOSPKEY(MOSPULA-MOSPKEY),WORK  TEST CONTROL RECORD               
         BNE   RQFX                                                             
         CLC   START3,MOSPLMOS     TEST LOW MONTH FROM ACLD                     
         BL    RQFX                CAN'T USE POINTERS                           
         MVI   FCRDMOSP,YES        SET USE PASSIVES                             
                                                                                
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         MVC   ACMMSTR,START3                                                   
         MVC   ACMMEND,START3                                                   
         OI    ACMINDS,ACMIMOSR                                                 
*                                                                               
RQFX     B     XIT                                                              
         DROP  R2,R4,RF                                                         
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LEDGER                                                    *         
***********************************************************************         
         SPACE 1                                                                
LGF00    CLI   MODE,LEDGFRST                                                    
         BNE   ACF00                                                            
         XC    AGLRULE,AGLRULE     CLEAR A(RULES ELEMENT)                       
         LA    R5,LDGTOT                                                        
         BAS   RE,CLR              CLEAR LEDGER TOTALS                          
                                                                                
         USING BIND,R7                                                          
         L     R7,ADOFT            A(OFFICE SUMMARY)                            
         XC    BININ,BININ                                                      
         MVI   LGSW,0              INIT LEDGER SWITCHS                          
         MVI   LGSW2,0                                                          
         OI    LGSW,LGBY           BYPASS LEDGER                                
         OI    LGSW,LGSM           PRINT SUMMARY                                
         MVI   FCRDACC,NO          SKIP ACCOUNTS                                
         MVI   FCRDTRNS,NO         AND TRANSACTIONS                             
                                                                                
         USING LDGRECD,R2                                                       
         L     R2,ADLEDGER                                                      
         GOTOR GETLV1,DMCB,LDGKUNT,ADLEDGER                                     
         CLC   =C'S9',LDGKUNT      G/L POSTING CONTROL                          
         BE    XIT                                                              
         DROP  R2                                                               
                                                                                
         NI    LGSW,ALL-LGBY       TURN-OFF BYPASS                              
         MVI   FCRDACC,YES         SET READ SWITCHES                            
         CLI   PROGPROF+3,YES      SUPPRESS LEDGER SUMMARY                      
         BNE   *+8                                                              
         NI    LGSW,LGSM           TURN OFF SUMMARY                             
*                                                                               
         CLI   PROGPROF+6,YES      USE G/L OFFICE RULES                         
         BNE   LGF02                                                            
         OI    LGSW,LGRL           USE OFFICE LEDGER RULES                      
         CLI   PROGPROF+7,YES      USE G/L OFFICE RULES FOR TRANS               
         BNE   *+8                                                              
         OI    LGSW2,LGRLT         USE OFFICE LEDGER RULES FOR TRANS            
*                                                                               
LGF02    MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,YES                                                     
         MVI   MDADSP,0                                                         
         MVI   RCSUBPRG,0                                                       
                                                                                
         USING CPYELD,R4                                                        
         L     R4,ADCMPEL          GET COMPANY ELEMENT                          
                                                                                
         USING ACTRECD,R6                                                       
         L     R6,ADLEDGER         A(LEDGER RECORD)                             
         CLC   ACTKCULA+1(2),CPYPROD IS IT PRODUCTION?                          
         BNE   LGF04                                                            
*                                                                               
         USING ACLELD,R4                                                        
         L     R4,ADLDGHIR         FOR PRODUCTION USE MEDIA CODE                
         OI    LGSW,LGPR           PRODUCTION SWITCH                            
         SR    R1,R1                                                            
         IC    R1,ACLVLEN+(L'ACLVALS)   LENGTH OF LEVEL B                       
         AHI   R1,3                                                             
         STC   R1,MDADSP           DISPLACEMENT TO MEDIA CODE                   
         MVI   RCSUBPRG,1          SPECIAL SPROG FOR PRODUCTION                 
*                                                                               
LGF04    MVC   AGLRULE,AGLRULEC                                                 
         L     R4,ADLEDGER                                                      
         MVI   ELCODE,GLRELQ       GL RULES ELEMENT X'E6'                       
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         ST    R4,AGLRULE                                                       
*                                                                               
         USING LDGELD,R4                                                        
         USING PSTD,R7                                                          
         L     R7,ADPST            POSTING INSTRUCTIONS                         
         L     R4,ADLDGEL          FROM LEDGER RECORD                           
         MVC   OFFP,LDGOPOS        SAVE OFFICE POSITION                         
         MVC   LGOFFC,LDGOFFC      DEFAULT OFFICE FROM LEDGER                   
*                                                                               
         USING GLPELD,R4                                                        
         SR    R5,R5                                                            
         MVI   ELCODE,GLPELQ                                                    
LGF06    XC    PSTF(PSTLNQ),PSTF   CLEAR THE ENTRY                              
         BAS   RE,NEXTEL           GET POSTING INSTRUCTIONS                     
         BNE   LGF08                                                            
         MVC   PSTTO(10),GLPACC1   G/L ACCOUNT                                  
         CLI   GLPLN,26            NEW LENGTH                                   
         BL    *+10                                                             
         MVC   PSTTO(14),GLPACC1                                                
         OC    PSTTO,SPACES                                                     
         MVC   PSTFR,GLPSUB        FROM                                         
         TM    LGSW,LGPR           IS THIS PRODUCTION                           
         BZ    LGF07                                                            
         CLI   PSTFR,C'*'          POSTING BY OFFICE                            
         BNE   LGF07                                                            
         CLI   PSTFR+1,C' '        COULD BE MEDIA *                             
         BE    LGF07                                                            
         OI    LGSW,LGOF           TURN ON GET RULES BY OFFICE                  
         MVC   PSTFR,SPACES                                                     
*                                                                               
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         SR    R1,R1                                                            
         TM    ACMINDS,ACMINEWO    TWO BYTE OFFICES IN USE?                     
         BZ    *+8                                                              
         LA    R1,1                                                             
         EXMVC R1,PSTFR,GLPSUB+1   MOVE OFFICE CODE TO "FROM" ACCOUNT           
*                                                                               
LGF07    LA    R2,L'PSTFR-1        GET LENGTH OF FROM DATA                      
         LA    RF,PSTFR+(L'PSTFR-1)                                             
         CLI   0(RF),X'40'         FIND LAST NON-BLANK                          
         BNE   *+12                                                             
         BCTR  R2,0                                                             
         BCTR  RF,0                                                             
         B     *-12                                                             
         STC   R2,PSTFRLN          LENGTH FOR COMPARE                           
*                                                                               
         AHI   R7,PSTLNQ                                                        
         AHI   R5,1                COUNT THEM                                   
         B     LGF06                                                            
*                                                                               
LGF08    LTR   R5,R5                                                            
         BZ    XIT                 NO POSTING INSTRUCTIONS FOR LEDGER           
         LA    R1,MXPST            MAX. POSTING INSTRUCTIONS                    
         CR    R5,R1                                                            
         BNH   *+6                                                              
         DC    H'0'                TOO MANY POSTING INSTRUCTIONS                
*                                                                               
*              SORT TABLE IN DESCENDING SEQUENCE ON "FROM" ACCOUNT              
*                                                                               
         LA    R0,PSTLNQ           RECORD LENGTH                                
         LA    RF,L'PSTFR          LENGTH OF SORT FIELD                         
         GOTO1 XSORT,DMCB,(1,ADPST),(R5),(R0),(RF),0                            
         B     XIT                                                              
         DROP  R4,R6,R7,RF                                                      
         EJECT                                                                  
***********************************************************************         
* FIRST FOR ACCOUNT                                                   *         
***********************************************************************         
         SPACE 1                                                                
ACF00    CLI   MODE,PROCACC                                                     
         BNE   PTN00                                                            
         MVI   OFARULE,0                                                        
         MVI   OFTRULE,0                                                        
         LA    R5,ACCTOT                                                        
         BAS   RE,CLR              CLEAR ACCOUNT TOTALS                         
         MVI   FCRDTRNS,NO         SKIP THE TRANSACTIONS                        
         TM    LGSW,LGBY           BYPASS THIS LEDGER                           
         BO    XIT                                                              
*                                                                               
         USING BIND,R7                                                          
         L     R7,ADBUKTAB         A(OFFICE SUMMARY)                            
         XC    BININ,BININ                                                      
         DROP  R7                                                               
                                                                                
         MVI   FLTRULE,0                                                        
         MVC   FLTOFFC,SPACES                                                   
         TM    OFFP,X'F0'          IS IT IN A FILTER                            
         JNO   *+8                 NO                                           
         BRAS  RE,COMPFLT          FIND LEVEL OF COMPOSIST FILTER VALUE         
                                                                                
         USING RSTELD,R4                                                        
         L     R4,ADACCSTA         ACCOUNT STATUS                               
         MVC   ACOFFC,SPACES                                                    
         CLI   RSTLN,RSTLN2Q       TEST OLD ELEMENT                             
         BL    ACF00A                                                           
         MVC   ACOFFC,RSTOFFC      SAVE DEFAULT OFFICE FOR ACCOUNT              
         MVI   OFARULE,GLDOFLOW    OFFICE IS FROM ACCOUNT LEVEL A               
                                                                                
ACF00A   MVC   TOACC,SPACES        "TO" ACCOUNT                                 
         MVC   FRACC,SPACES        "FROM" ACCOUNT                               
         MVI   BFREC,C' '          INITIALIZE THE BUFFALO RECORD                
         MVC   BFREC+1(L'BFREC-1),BFREC                                         
         MVI   BFSTAT,0                                                         
         MVI   ACCRULE,0                                                        
         LA    R5,BFBK             CLEAR ACCUMS                                 
         LA    R0,BFNBK                                                         
         ZAP   0(L'BFBK,R5),PKZERO                                              
         LA    R5,L'BFBK(R5)                                                    
         BCT   R0,*-10                                                          
***********************************************************************         
*              PRODUCTION LEDGER                                                
***********************************************************************         
         USING PPRELD,R4                                                        
         TM    LGSW,LGPR           IS IT PRODUCTION LEDGER                      
         BZ    ACF05                                                            
         OI    BFSTAT,PRODLG       SET PRODUCTION STATUS                        
         OI    BFSTAT,LDGSRC       SET RULE FROM LEDGER                         
         MVI   ACCRULE,GLDTOLDG    GL ACCOUNT FROM LEDGER                       
*&&UK*&& L     R4,ADLVCSUP         Job level                                    
*&&UK*&& MVI   OFARULE,GLDOFJOB    JOB                                          
*&&UK*&& BRAS  RE,GETPPROF                                                      
*&&UK*&& BE    ACF01               Yes                                          
         L     R4,ADLVBSUP         Product level                                
         MVI   OFARULE,GLDOFPRD                                                 
         BRAS  RE,GETPPROF                                                      
         BE    ACF01               Yes                                          
         L     R4,ADLVASUP         Client level                                 
         MVI   OFARULE,GLDOFCLI                                                 
         BRAS  RE,GETPPROF                                                      
                                                                                
         USING RSTELD,R4                                                        
ACF01    MVC   ACOFFC,SPACES                                                    
         L     R4,ADLVASTA                                                      
         CLI   RSTLN,RSTLN2Q       TEST OLD ELEMENT                             
         BL    ACF02                                                            
         CLC   RSTOFFC,SPACES                                                   
         BNH   ACF02               No value set                                 
         MVC   ACOFFC,RSTOFFC      GLOFFC FROM CLIENT LEVEL ACOUNT              
         MVI   OFARULE,GLDOFACL    Client account record                        
         DROP  R4                                                               
                                                                                
ACF02    L     R4,ADACC                                                         
         MVC   FRACC(2),1(R4)      UNIT LEDGER                                  
*                                                                               
ACF03    SR    R6,R6               POST BY MEDIA                                
         IC    R6,MDADSP           DISPLACEMENT TO MEDIA CODE                   
         AR    R6,R4                                                            
         MVC   FRACC+2(1),0(R6)    MEDIA CODE                                   
*                                                                               
         USING PMDELD,R4                                                        
         L     R4,ADCMPEL                                                       
         MVI   ELCODE,PMDELQ                                                    
ACF04    BAS   RE,NEXTEL           GET MEDIA NAME                               
         BE    *+6                                                              
         DC    H'0'                MISSING MEDIA RECORD                         
                                                                                
         CLC   PMDCODE,FRACC+2     MATCH MEDIA CODE                             
         BNE   ACF04                                                            
         MVC   BFNME(15),PMDSPACE  NAME IS MEDIA DESCRIPTION                    
         B     ACF11                                                            
*                                                                               
*              NON-PRODUCTION LEDGER                                            
*                                                                               
ACF05    L     R4,ADACCNAM                                                      
         LA    R5,BFNME                                                         
         BAS   RE,GETNME           SAVE FROM ACCOUNT NAME                       
         L     R4,ADACC                                                         
         MVC   FRACC,1(R4)         SET "FROM" ACCOUNT                           
         XC    ADLEVS(ADLEVQ*L'ADLEVS),ADLEVS   CLEAR A(ACCT RECS)              
*                                                                               
         USING ACLELD,R4                                                        
         L     R4,ADLDGHIR                                                      
         MVC   ADLEV1,ADHEIRA                                                   
         MVC   ADNAM1,ADLVANAM                                                  
                                                                                
ACF05B   CLI   ACLVLEN+(L'ACLVALS*1),0  Any level B ?                           
         BE    ACF05C                   No                                      
         MVC   ADLEV2,ADHEIRB                                                   
         MVC   ADNAM2,ADLVBNAM                                                  
                                                                                
ACF05C   CLI   ACLVLEN+(L'ACLVALS*2),0  Any level C ?                           
         BE    ACF05D                   No                                      
         MVC   ADLEV3,ADHEIRC                                                   
         MVC   ADNAM3,ADLVCNAM                                                  
                                                                                
ACF05D   CLI   ACLVLEN+(L'ACLVALS*3),0  Any level D ?                           
         BE    ACF05F                   No                                      
         MVC   ADLEV4,ADHEIRD                                                   
         MVC   ADNAM4,ADLVDNAM                                                  
                                                                                
ACF05F   LA    R5,ADLEVS           START OF LIST OF ADDRESSES                   
         LA    R6,ADLEVQ           # POSSIBLE LEVELS                            
         MVI   ACCRULE,GLDTOLVD    Start at Level D                             
*                                                                               
ACF06    DS    0H                  Check all 4 levels                           
         ICM   R4,15,0(R5)         Skip if not a valid account level            
         BZ    ACF08                                                            
         MVI   ELCODE,GLPELQ       X'15' POSTING ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   ACF08                                                            
                                                                                
         USING GLPELD,R4                                                        
         OI    BFSTAT,ACCSRC       SOURCE IS ACCOUNT RECORD                     
         MVC   TOACC(10),GLPACC1   SET TO ACCOUNT                               
         CLI   GLPLN,26                                                         
         BL    *+10                                                             
         MVC   TOACC,GLPACC1       STOP IF FOUND A VALID GL ACCT                
         CLI   PROGPROF,C'P'       PROFILE TO HAVE POINTER LEVEL ACCT           
         BNE   ACF25               BECOME CONTRA IN GL POSTING                  
         ICM   R4,15,0(R5)                                                      
         MVC   BFPTRACT,1(R4)                                                   
         ICM   R4,15,4(R5)         A(NAME ELEMENT)                              
         LA    R5,BFPTRNM                                                       
         BAS   RE,GETNME           SAVE FROM ACCOUNT NAME                       
         B     ACF25                                                            
*                                                                               
ACF08    ZIC   RF,ACCRULE          Adjust level from D to C to B to A           
         BCTR  RF,0                                                             
         STC   RF,ACCRULE          SET  TO NEXT HIGHER LEVEL                    
         AHI   R5,L'ADLEVS         BUMP TO NEXT HIGHER LEVEL                    
         BCT   R6,ACF06                                                         
*                                                                               
*              GET "TO" ACCOUNT FROM LEDGER RULES                               
*                                                                               
         OI    BFSTAT,LDGSRC       SOURCE IS LEDGER                             
         MVI   ACCRULE,GLDTOLDG    GL ACCOUNT FROM LEDGER                       
ACF11    SR    R1,R1                                                            
*                                                                               
         USING PSTD,R7                                                          
         L     R7,ADPST            LOOK FOR POSTING RULE                        
ACF12    CLC   PSTFR,SPACES        IF IT'S THE DEFAULT                          
         BE    ACF20               USE IT                                       
         OC    PSTFR,PSTFR         IF END OF TABLE                              
         BZ    ACF15               IT'S AN ERROR                                
         LA    RE,CLOFFC                                                        
         LA    R1,L'CLOFFC-1                                                    
         TM    LGSW,LGOF                                                        
         BO    *+12                                                             
         LA    RE,FRACC+2                                                       
         IC    R1,PSTFRLN          LENGTH OF FROM DATA                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PSTFR(0),0(RE)      MATCH RULE TO THE "FROM ACCOUNT"             
         BE    ACF20               MATCHED ACCOUNT TO THE RULE                  
         LA    R7,PSTLNQ(R7)                                                    
         B     ACF12                                                            
*                                                                               
ACF15    OI    LGSW,LGNP           NO POSTING INSTRUCTIONS                      
         B     XIT                                                              
*                                                                               
ACF20    MVC   TOACC,PSTTO                                                      
ACF25    XC    LSTSTAR,LSTSTAR                                                  
         LA    R0,L'TOACC-1         FIND OFFICE OVERRIDE POSITION               
         LA    R5,TOACC+(L'TOACC-1)                                             
         CLI   0(R5),C'*'                                                       
         BNE   *+16                                                             
         ST    R5,LSTSTAR          A(OVERRIDE POSITION)                         
         OI    BFSTAT,PTRN+POFCL   POST TRANSACTION LEVEL, BY OFFICE            
         B     *+10                                                             
         BCTR  R5,0                                                             
         BCT   R0,*-22                                                          
*                                                                               
         MVI   FCRDTRNS,YES        OK, TO READ TRANSACTIONS                     
*&&UK*&& TM    RNSW,RNOF           POST TO G/L BY OFFICE                        
*&&UK*&& BZ    ACF30                                                            
         CLI   OFFP,C'T'           Office in transaction?                       
         BE    ACF32               Yes                                          
         CLI   OFFP,0              Not set then default is transaction          
         BE    ACF32               Yes                                          
         B     XIT                 No, exit                                     
*                                                                               
ACF30    TM    RNSW,RNTOF          Post by transaction office?                  
         BZ    XIT                 No                                           
ACF32    OI    BFSTAT,PTRN         Yes                                          
         B     XIT                                                              
         DROP  R4,R7                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R3                                                        
PTN00    CLI   MODE,PROCTRNS                                                    
         BNE   ACL00                                                            
         L     R3,ADTRANS                                                       
         CLI   TRNEL,TRNELQ        BAD ELEMENT                                  
         BNE   XIT                                                              
         CLC   TRNBTCH(2),MOSDT    WRONG MOS                                    
         BNE   XIT                                                              
                                                                                
         USING TRNRECD,R6                                                       
         LR    R6,R3                                                            
         SH    R6,DATADISP                                                      
         OC    TRNRECD+ACCOPEEL(2),TRNRECD+ACCOPEEL                             
         BNZ   XIT                 IGNORE PEELED TRANSACTIONS                   
         CLC   TRNOFFC,=C'**'      IGNORE PURCHASE ORDERS                       
         BE    XIT                                                              
                                                                                
         TM    RQSW,RQMNT                                                       
         BZ    PTN02                                                            
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(X'81',START3),(6,WORK+6)                            
         L     R4,4(R1)            ACTUAL LENGTH OF OUTPUT FIELD                
         STCM  R4,8,BYTE           STORE FOR BMONVAL CALL                       
         GOTO1 =V(BMONVAL),DMCB,(BYTE,WORK+6),(25,ADCOMFAC),(0,WORK+20)X        
               ,(RCCOMPFL,0)                                                    
         LA    R1,WORK+20                                                       
         USING BMONVALD,R1                                                      
         CLI   BMOERR,BMOEOKQ      EVERYTHING OK?                               
         BE    PTN02                                                            
         TM    BMOERR,BMOELOKQ     MOA LOCKED?                                  
         BZ    PTN02                                                            
         OC    BMOLCKP,BMOLCKP     LAST LOCKED MOA                              
         BZ    PTN02                                                            
         XC    WORK(10),WORK                                                    
         MVC   WORK(L'BMOLCKP),BMOLCKP                                          
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+3)                                  
         GOTO1 ADDAY,DMCB,WORK+3,WORK+3,31                                      
         GOTO1 DATCON,DMCB,(0,WORK+3),(1,WORK+3)                                
         MVC   START3,WORK+3                                                    
         B     PTN02A                                                           
*******************************************************                         
         USING ACMD,RF             ERROR OCCURING.                              
PTN02    L     RF,AMONACC          COMPARE START DATE TO AVOID DECADE           
         CLC   ACMMDTE(2),START3                                                
         BNE   XIT                                                              
         DROP  RF                                                               
                                                                                
PTN02A   XC    AREVGLD,AREVGLD                                                  
         MVI   TRNSW,0                                                          
         MVC   THISDATE,TRNDATE                                                 
         TM    RQSW,RQREV                                                       
         BO    PTN08                                                            
         TM    BFSTAT,PTRN         POST AT TRANSACTION LEVEL                    
         BZ    PTN09                                                            
                                                                                
PTN08    LA    RE,BFBK             CLEAR ACCUMS                                 
         LA    RF,BFNBK                                                         
         ZAP   0(L'BFBK,RE),PKZERO                                              
         LA    RE,L'BFBK(RE)                                                    
         BRCT  RF,*-10                                                          
*                                                                               
PTN09    ZAP   AMOUNT,TRNAMNT                                                   
         TM    RQSW,RQREV          REVERSAL                                     
         BZ    *+10                                                             
         MP    AMOUNT,=P'-1'                                                    
                                                                                
         TM    RQSW,RQIR           FIND ANY UNAUTH AMOUNTS                      
         BNO   PTN15                                                            
         TM    TRNSTAT,TRNSAUTH    AUTH IF BIT ON                               
         BO    PTN15                                                            
         LA    RF,BFUDR                                                         
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    RF,BFUCR                                                         
         AP    0(L'BFBK,RF),AMOUNT                                              
*                                                                               
         USING TRSELD,R4                                                        
PTN15    XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         MVI   TRSEL,TRSELQ       BUILD 60 ELEMENT                              
         MVI   TRSLN,TRSLNQ                                                     
         GOTO1 DATCON,DMCB,(1,THISDATE),(2,TRSDATE)                             
         MVI   EL60,NO                                                          
         LR    R4,R3                                                            
         MVI   ELCODE,TRSELQ       GET A 60 ELEMENT                             
         BAS   RE,NEXTEL                                                        
         BNE   PTN19               NOT FOUND - ADD NEW ONE                      
         CLI   TRSLN,9                                                          
         BL    PTN17               FOUND AN OLD ONE                             
         MVI   EL60,YES                                                         
         B     PTN21               FOUND A GOOD ONE                             
*                                                                               
PTN17    LA    R7,ELEMENT          SAVE DATE ADDED                              
         MVC   TRSDATE-TRSELD(L'TRSDATE,R7),TRSDATE                             
         GOTO1 HELLO,DMCB,(C'D',ACCFIL),(X'60',(R6)),0                          
*                                                                               
PTN19    LA    R4,ELEMENT                                                       
PTN21    CLC   TRSDATE,TODAY2      TEST ACTIVITY DATE                           
         BH    XIT                 SKIP ITEMS ADDED AFTER RUN DATE              
************************                                                        
* Deal with reversing  *                                                        
************************                                                        
         TM    RQSW,RQREV          TEST REVERSE OPTION                          
         BZ    PTN25                                                            
         XC    REVDATE,REVDATE                                                  
         TM    TRSSTAT,TRSSGLIP    If updated via input                         
         BO    XIT                 Yes, don't reverse                           
         CLI   TRSUPDT,0           Has update or reverse date                   
         BE    XIT                 Can't reverse if never updated               
         TM    RNSW,RNYES          RERUN=YES OR RERUN=ALL                       
         BZ    PTN23               Niether, so reverse                          
         CLC   TRSUPDT,TODAY2      Was it marked today                          
         BNE   PTN22               No, so check RERUN=ALL                       
         TM    TRSSTAT,TRSSGLUP    Yes, Has it been updated                     
         BO    XIT                 Must have been update again today            
         OI    TRNSW,TRNUPDB       Updated before                               
         B     PTN24               Date but no bit, so reversed already         
*                                                                               
PTN22    TM    RNSW,RNALL          RERUN=ALL                                    
         BZ    XIT                 Continue to reverse items if BO              
*                                                                               
PTN23    TM    TRSSTAT,TRSSGLUP    Was it updated                               
         BZ    XIT                 No. Can't reverse                            
         NI    TRSSTAT,ALL-TRSSGLUP     Unmark as updated                       
         OI    TRNSW,TRNREV                                                     
PTN24    GOTOR DATCON,DMCB,(2,TRSUPDT),(1,REVDATE)                              
         B     PTN33                                                            
                                                                                
*****************************                                                   
* Process as normal update  *                                                   
*****************************                                                   
PTN25    CLI   TRSUPDT,0           Updated ?                                    
         BE    PTN27               No                                           
         TM    TRSSTAT,TRSSGLUP    Update bit on too?                           
         BO    PTN29               Yes, see what we want to do with it          
*********************                                                           
* Not updated items *                                                           
*********************                                                           
PTN27    TM    RNSW,RNALL          RERUN=ALL                                    
         BO    PTN28               Update all                                   
         TM    RNSW,RNYES          RERUN=YES                                    
         BO    XIT                 Only process ones updated when on            
                                                                                
PTN28    OI    TRSSTAT,TRSSGLUP    mark as updated                              
         B     PTN33                                                            
****************************************************                            
* Only process items that have been updated before *                            
****************************************************                            
PTN29    OI    TRNSW,TRNUPDB       Updated before                               
         TM    RNSW,RNALL          RERUN=ALL                                    
         BO    PTN30               YES                                          
         TM    RNSW,RNYES          RERUN=YES                                    
         BZ    PTN31               NO                                           
                                                                                
PTN30    CLC   TRSUPDT,TODAY2      WAS IT UPDATED TODAY                         
         BH    XIT                                                              
         BL    PTN31                                                            
         TM    TRSSTAT,TRSSGLIP    WAS IT UPDATED VIA $INPUT?                   
         BZ    PTN33               NO                                           
*                                                                               
PTN31    LA    R8,BFPDR            PREVIOUS UPDATE FOR THIS MONTH               
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R8,BFPCR            ADD TO CREDITS                               
         AP    0(8,R8),AMOUNT                                                   
         OI    LGSW,LGPV           PREVIOUS ACTIVITY                            
         B     PTN41                                                            
**************************************************                              
* Process new items or qualified reversal items  *                              
**************************************************                              
         USING GLDELD,R5                                                        
PTN33    TM    RQSW,RQREV          Processing reversal item                     
         BZ    PTN36               No                                           
         CLC   TRSPMOS,GLMOA       New GL system?                               
         BL    PTN36               No                                           
                                                                                
         LR    R5,R3               Yes so find GLDELD, R4=A(TRNELD)             
         SR    R1,R1                                                            
PTN34    CLI   0(R5),EOR           End of record ?                              
         BNE   *+6                 No                                           
         DC    H'00'               Problem                                      
         CLI   0(R5),GLDELQ        X'63' General ledger details                 
         BNE   PTN34B                                                           
         TM    RNSW,RNYES          RERUN=YES OR RERUN=ALL                       
         BZ    PTN34A                                                           
         TM    TRNSW,TRNUPDB       Updated before                               
         BZ    PTN34A              No                                           
         CLC   REVDATE,GLDRDTE     Match on reverse date instead                
         BE    PTN35                                                            
****                               Should we do this                            
*                                  if RE-RUN=YES or ALL?                        
*                                                                               
PTN34A   TM    GLDIND,GLDREV       Is is already reversed?                      
         BO    PTN34B              Yes, so skip                                 
         CLC   REVDATE,GLDDATE     Match on date                                
         BE    PTN35                                                            
PTN34B   IC    R1,1(,R5)                                                        
         AR    R5,R1                                                            
         B     PTN34                                                            
                                                                                
PTN35    OI    GLDIND,GLDREV       Mark as reversed                             
         MVC   GLDRDTE,TODAY3      Set reversal date                            
         OI    TRNSW,TRNRGLD       Reverse posting based on GLDELD              
         ST    R5,AREVGLD          Save address                                 
         DROP  R5                                                               
                                                                                
PTN36    MVC   TRSUPDT,TODAY2      MOVE IN DATE                                 
                                                                                
         CLI   EL60,YES                                                         
         BE    PTN37                                                            
         LA    R4,ELEMENT                                                       
         GOTO1 HELLO,DMCB,(C'P',ACCFIL),(R6),(R4),0                             
*                                                                               
PTN37    TM    RQSW,RQDF           IS IT DRAFT                                  
         BO    *+8                                                              
         MVI   MODE,WRITRANS       WRITE IT IF LIVE                             
*                                                                               
PTN39    LA    R8,BFDR                                                          
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R8,BFCR             ADD TO CREDITS                               
         AP    0(8,R8),AMOUNT                                                   
         OI    LGSW,LGAC           CURRENT ACTIVITY                             
*                                                                               
PTN41    MVI   OFFSW,OFFSTRN       DEFAULT TO TRANSACTION PROCESSING            
         TM    BFSTAT,PTRN         POSTING AT TRANSACTION LEVEL                 
         BO    *+8                                                              
         MVI   OFFSW,OFFSACL       POST AS IF IN ACCLAST                        
         BRAS  RE,FIGOFF           GET OFFICES TO POST                          
                                                                                
         TM    BFSTAT,PTRN         POSTING AT TRANSACTION LEVEL                 
         BO    PTN45               Yes so continue                              
         TM    RQSW,RQREV          Processing reversal item                     
         BO    PTN50               Yes, so force to process at PROCTRN          
         B     PTN70               No, so hold till ACCLAST                     
                                                                                
PTN45    TM    BFSTAT,POFCL        REPLACE LAST CHARACTER OF ACCOUNT            
         BZ    PTN50                                                            
*                                                                               
         ICM   R5,15,LSTSTAR       OVERRIDE POSITION OF TOACC                   
         BZ    PTN50                                                            
         MVC   0(1,R5),GLOFFC      OFFICE CODE                                  
         CLI   0(R5),X'40'                                                      
         BH    *+8                                                              
         MVI   0(R5),C'*'          IF INVALID OFFICE MAKE IT AN *               
*                                                                               
PTN50    TM    RQSW,RQREV                                                       
         BZ    PTN60                                                            
         BRAS  RE,GLCHG            Will posting change ?                        
         BNE   PTN60               Yes, so continue                             
         MVI   MODE,PROCTRNS       No, reset not to write back                  
         J     XIT                 Don't process transaction                    
                                                                                
PTN60    BAS   RE,BUFSRT           PUT ALL TO BUFFALO/SORTER                    
                                                                                
PTN70    BAS   RE,GLELEM           NEW GL DETAIL ELEMENT                        
         J     XIT                                                              
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* LAST FOR THE ACCOUNT                                                *         
***********************************************************************         
         SPACE 1                                                                
ACL00    CLI   MODE,ACCLAST                                                     
         BNE   LGL00                                                            
         TM    LGSW,LGBY           BYPASS LEDGER?                               
         BO    XIT                                                              
         MVC   POOFFC,SPACES                                                    
         TM    BFSTAT,PTRN         POSTING AT TRANSACTION LEVEL                 
         BO    ACL20               ALREADY POSTED                               
         TM    RQSW,RQREV          Processing reversal item                     
         BO    ACL20                                                            
         MVI   OFFSW,OFFSACL       SET POSTING IN ACCLAST                       
         BRAS  RE,FIGOFF                                                        
         BRAS  RE,BUFSRT           ADD BUFFALO/SORTER RECORDS                   
                                                                                
ACL20    CLC   GLMOA,=X'FFFF'                                                   
         BE    ACL22                                                            
         BRAS  RE,PUTBUK           Put new GL records now                       
                                                                                
ACL22    TM    BFSTAT,PRODLG       IS IT PRODUCTION                             
         BO    XIT                 PRINT SUMMARY AT LEDGER LAST                 
         BAS   RE,ACTS             PRINT ACCOUNT SUMMARY                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LAST FOR LEDGER                                                     *         
***********************************************************************         
         SPACE 1                                                                
LGL00    CLI   MODE,LEDGLAST                                                    
         BNE   RQL00                                                            
         TM    LGSW,LGAC+LGPV      ANY ACTIVITY                                 
         BZ    LGL12               NO ACTIVITY                                  
         TM    BFSTAT,PRODLG       IS IT PRODUCTION LEDGER                      
         BNO   *+8                 IF NOT, ALREADY PRINTED SUMMARY              
         BAS   RE,ACTS             PRINT SUMMARY FOR PRODUCTION                 
         LA    R5,LDGTOT           R5 = LEDGER TOTALS                           
         LA    R6,REQTOT           R6 = REQUEST TOTALS                          
         BAS   RE,ADDUP            ADD LEDGER TO REQUEST TOTALS                 
         BAS   RE,TOTLN            TOTAL LINE  FOR LEDGER                       
*&&UK*&& TM    RNSW,RNOF+RNTOF     POST BY OFFICE                               
*&&UK*&& BZ    LGL05                                                            
         BAS   RE,OFS              PRINT THE OFFICE SUMMARY                     
*                                                                               
*                                  LEDGER SUMMARY                               
*                                                                               
LGL05    TM    LGSW,LGSM           PRINT LEDGER SUMMARY                         
         BNO   LGL10                                                            
         TM    LGSW,LGAC           CURRENT LEDGER ACTIVITY                      
         BZ    LGL10                                                            
         MVI   RCSUBPRG,2                                                       
         LA    R4,BFLGLV           SET LEVEL FOR LEDGER                         
         BAS   RE,SUM              PRINT THE LEDGER SUMMARY                     
         LA    R5,LDGTOT           R5 = LEDGER TOTALS                           
         BAS   RE,TOTLN            TOTAL LINE  FOR LEDGER                       
*                                                                               
LGL10    LA    R0,BFSUM            SET RECORD TYPE                              
*                                  ADD TO REQUEST AND RUN SUMMARIES             
         GOTO1 BUFFALO,DMCB,=C'ADD',((R0),ADBUF),1,2,(X'80',3)                  
         TM    LGSW,LGAC           CURRENT LEDGER ACTIVITY                      
         BZ    LGL12                                                            
         BRAS  RE,POST             BUILD POSTING FILE FOR LEDGER                
         LA    R3,P+1                                                           
         MVC   0(L'AC@CNTPT,R3),AC@CNTPT                                        
         LA    R3,L'AC@CNTPT+1(R3)                                              
         L     R2,ADLEDGER                                                      
         MVC   0(1,R3),1(R2)                                                    
         MVI   1(R3),C'9'                                                       
         MVC   2(2,R3),2(R2)                                                    
         BRAS  RE,PRN                                                           
*                                                                               
LGL12    LA    R0,BFPST            CLEAR POSTING RECORDS                        
         GOTO1 BUFFALO,DMCB,=C'CLEAR',((R0),ADBUF),(X'80',1)                    
*                                  CLEAR SUMMARY RECORDS                        
         LA    R4,BFLGLV           SET LEVEL FOR LEDGER                         
         LA    R0,BFSUM            SET RECORD TYPE                              
         GOTO1 BUFFALO,DMCB,=C'CLEAR',((R0),ADBUF),(X'80',(R4))                 
         TM    LGSW,LGNP           NO POSTING INSTRUCTIONS                      
         BNO   XIT                                                              
         MVC   P+1(L'AC@LGRMR),AC@LGRMR  LEDGER MISSING POSTING ...             
         BRAS  RE,PRN                                                           
         LA    RF,ERLGR            LIST OF LEDGERS WITH ERRORS                  
         LA    R0,MXERLG           MAXIMUM NUMBER OF ERRORS                     
         L     R2,ADLEDGER                                                      
         CLI   0(RF),X'FF'         FIND END OF LIST                             
         BE    *+14                                                             
         LA    RF,L'ERLGR(RF)                                                   
         BCT   R0,*-12                                                          
         DC    H'0'                TOO MANY ERRORS                              
         MVC   0(L'ERLGR,RF),1(R2) SAVE LEDGER CODE                             
         MVI   2(RF),X'FF'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LAST FOR REQUEST                                                    *         
***********************************************************************         
         SPACE 1                                                                
RQL00    CLI   MODE,REQLAST                                                     
         BNE   RNL00                                                            
         LA    R5,REQTOT           R5 = REQUEST TOTALS                          
         LA    R6,RUNTOT           R6 = RUN TOTALS                              
         BAS   RE,ADDUP            ADD REQUEST TO RUN TOTALS                    
         LA    R4,BFRQLV           SET LEVEL FOR REQUEST                        
         MVI   RCSUBPRG,6                                                       
         TM    RQSW,RQSM           REQUEST SUMMARY                              
         BNO   RQL15               SKIP SUMMARY                                 
         MVI   RCSUBPRG,7                                                       
         BAS   RE,SUM              REQUEST SUMMARY                              
*                                                                               
RQL15    BRAS  RE,PRN              SKIP A LINE                                  
         MVC   P+1(L'AC@TREQ),AC@TREQ                                           
         LA    R5,REQTOT           R5 = REQUEST TOTALS                          
         BAS   RE,TOTLN            TOTAL LINE                                   
*                                                                               
RQL20    LA    R0,BFSUM            CLEAR REQUEST LEVEL                          
         GOTO1 BUFFALO,DMCB,=C'CLEAR',((R0),ADBUF),(X'80',(R4))                 
         LA    RF,ERLGR                                                         
         CLI   0(RF),X'FF'         ANY ERRORS                                   
         BE    RQL27               NO ERRORS                                    
         BRAS  RE,PRN              SKIP A LINE                                  
         MVC   P+1(L'AC@FOLLG),AC@FOLLG FOLLOWING MISSING RULES                 
         BRAS  RE,PRN                                                           
         LA    R3,P+1                                                           
*                                                                               
RQL23    CLI   0(RF),X'FF'         FIND END OF LIST                             
         BE    RQL25                                                            
         MVC   0(L'ERLGR,R3),0(RF) LEDGER CODE TO PRINT                         
         LA    RF,L'ERLGR(RF)                                                   
         LA    R3,3(R3)                                                         
         B     RQL23                                                            
*                                                                               
RQL25    BRAS  RE,PRN              PRINT THE LIST OF LEDGERS                    
                                                                                
         USING BIND,R7                                                          
RQL27    L     R7,ADERR            ERROR RECORDS                                
         ICM   R3,15,BININ         NUMBER IN TABLE                              
         BZ    RQL30                                                            
         OI    RQSW,RQER           PRINT THE ERROR REPORT                       
         MVI   FORCEHED,YES                                                     
         MVI   RCSUBPRG,8                                                       
         BAS   RE,ACTS                                                          
         NI    RQSW,ALL-RQER                                                    
                                                                                
         USING ACMD,RF                                                          
RQL30    TM    RQSW,RQREV                                                       
         BZ    XIT                                                              
         L     RF,AMONACC                                                       
         MVI   ACMMODE,REQFRST     SIGNAL CONTROLLER TO RESET REQFRST           
         MVI   QOPT1,C'R'          Now run it to repost to new GL acc           
         B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* LAST FOR THE RUN                                                    *         
***********************************************************************         
         SPACE 1                                                                
RNL00    CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         TM    UPSI,UPSICNT                                                     
         BZ    RNL02                                                            
         MVC   P+1(4),=C'ADD='                                                  
         CURED ADD28,(12,P+9),0,COMMAS=YES                                      
         GOTOR ACREPORT                                                         
         MVC   P+1(8),=C'CHANGED='                                              
         CURED CHG28,(12,P+9),0,COMMAS=YES                                      
         GOTOR ACREPORT                                                         
                                                                                
RNL02    CP    RCRQTOT,=P'1'       HOW MANY REQUESTS                            
         BE    RNL05               ONLY ONE DON'T NEED RUN TOTALS               
         TM    RNSW,RNSM           RUN SUMMARY                                  
         BO    RNL03               DO THE RUN SUMMARY                           
         B     RNL04               ONE LINE FOR ALL REQUESTS                    
*                                                                               
RNL03    MVI   RCSUBPRG,7                                                       
         LA    R4,BFRNLV           SET LEVEL FOR RUN                            
         BAS   RE,SUM              PRINT THE SUMMARY                            
*                                                                               
RNL04    BRAS  RE,PRN              SKIP A LINE                                  
         MVC   P+1(L'AC@TAREQ),AC@TAREQ                                         
         LA    R5,RUNTOT           R5 = RUN TOTALS                              
         BAS   RE,TOTLN            TOTAL LINE                                   
*                                                                               
RNL05    BRAS  RE,CLSPOST          CLOSE POSTING FILE                           
                                                                                
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         MVI   ACMMODE,REQFRST     SIGNAL CONTROLLER TO RESET REQFRST           
         MVC   QMOSEND,QSTART      TRIAL BALANCE THRU                           
         MVC   QRECORD,SPACES                                                   
         MVC   QPROG,=C'25'                                                     
         MVI   QUNIT,C'G'          MAKE REQUEST FOR UNIT G ONLY                 
         MVC   QSEQ,SVQSEQ         RESTORE REQUESTED SEQUENCE                   
         MVC   FCSEQ,SVFCSEQ                                                    
         XC    WORK,WORK           CREATE END-OF-LEDGER SORT RECORDS...         
         MVI   WORK,ACMATBS        . . . FOR GB                                 
         MVC   WORK+1(SMKLNQ-1),XFFS                                            
         GOTO1 ADSORTER,DMCB,=C'PUT',WORK                                       
         MVI   WORK,ACMATPL        . . . AND GP                                 
         MVC   WORK+1(SMKLNQ-1),XFFS                                            
         GOTO1 ADSORTER,DMCB,=C'PUT',WORK                                       
         OI    RNSW,RNTB           SET TRIAL BALANCE MODE                       
XIT      XIT1                                                                   
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* CONTROL RECORDS TO BUFFALO AND SORTER                               *         
***********************************************************************         
         SPACE 1                                                                
BUFSRT   NTR1                                                                   
         LA    R0,BFNBK            NUMBER OF BUCKETS                            
         LA    R1,BFBK                                                          
         CP    0(L'BFBK,R1),PKZERO                                              
         BNE   *+16                ANY NON-ZERO IS OK TO POST                   
         LA    R1,L'BFBK(R1)                                                    
         BCT   R0,*-14                                                          
         B     XIT                 ALL ZERO, NO NEED TO POST                    
*                                                                               
         USING GLDELD,R4                                                        
         USING ACTRECD,R6                                                       
         TM    TRNSW,TRNRGLD       Reverse posting based on GLDELD              
         BZ    BUFS02                                                           
         L     R6,ADACC            A(Account record)                            
         L     R4,AREVGLD          A(GLDEL) to use                              
         MVC   BFACCT,ACTKULA      Unit S                                       
         MVC   SVBFSTAT,BFSTAT     Save this off                                
         NI    BFSTAT,TURNOFF-(ACCSRC+LDGSRC)                                   
         CLI   GLDTORUL,GLDTOLDG   At time of original marking - ledger         
         BNE   *+8                                                              
         OI    BFSTAT,LDGSRC       Set to ledger                                
         CLI   GLDTORUL,GLDTOLVA   At time of original marking - acct.          
         BL    *+8                                                              
         OI    BFSTAT,ACCSRC       Set to account                               
                                                                                
         TM    BFSTAT,PRODLG       IF IT'S PRODUCTION                           
         BZ    BUFS01                                                           
         MVC   BFACCT+2(L'BFACCT-2),SPACES                                      
         SR    RE,RE               POST BY MEDIA                                
         IC    RE,MDADSP           DISPLACEMENT TO MEDIA CODE                   
         LA    RE,ACTKULA(RE)                                                   
         MVC   BFACCT+2(1),0(RE)   Move in media code                           
                                                                                
BUFS01   MVC   BFCNTR,GLDULA       Unit G                                       
         MVC   BFOFFC,SPACES       Clear                                        
         TM    GLDIND,GLDBYOFF     Posted by office ?                           
         BZ    *+10                No                                           
         MVC   BFOFFC,GLDOFFC      Yes                                          
         MVI   BFTYPE,0            FOR ACCOUNT SUMMARY - NO TYPE                
         B     BUFS05              Post                                         
                                                                                
BUFS02   MVC   BFACCT,FRACC        SET FROM ACCOUNT                             
         MVC   BFCNTR,TOACC        AND TO   ACCOUNT                             
         MVI   BFTYPE,0            FOR ACCOUNT SUMMARY - NO TYPE                
         MVC   BFOFFC,SPACES                                                    
*&&UK*&& TM    RNSW,RNOF+RNTOF     POST TO G/L BY OFFICE                        
*&&UK*&& BZ    BUFS05                                                           
         LA    R3,ACOFFC           ADD OFFICE FOR POSTING                       
         CLI   0(R3),X'40'         FIRST TRY ACCOUNT GLOFFC                     
         BH    BUFS03                                                           
         LA    R3,LGOFFC           THEN LEDGER GLOFFC                           
         CLI   0(R3),X'40'                                                      
         BH    BUFS03                                                           
         LA    R3,POOFFC           THEN OFFPOS=F1,OFFPOS=1,OFFPOS=T             
         CLI   0(R3),X'40'                                                      
         BH    BUFS03                                                           
         LA    R3,COOFFC           DEFAULT IS COMPANY                           
         MVC   BFOFFC,0(R3)                                                     
         OC    BFOFFC,SPACES                                                    
         GOTOR BINADD,DMCB,BFREC,ADERR                                          
*                                                                               
BUFS03   MVC   BFOFFC,0(R3)                                                     
         OC    BFOFFC,SPACES                                                    
*                                                                               
BUFS05   GOTOR BINADD,DMCB,BFREC,ADACT                                          
         MVI   BFTYPE,BFSUM        LEDGER LEVEL (POSTING)                       
         TM    TRNSW,TRNRGLD       Reverse posting based on GLDELD              
         BZ    BUFS06                                                           
         MVC   BFACCT,GLDULA       Unit G                                       
         MVC   BFCNTR,GLDCULA      Unit S                                       
         B     BUFS09                                                           
                                                                                
BUFS06   XC    BFACCT,BFCNTR       SWITCH "FROM" WITH "TO"                      
         XC    BFCNTR,BFACCT                                                    
         XC    BFACCT,BFCNTR                                                    
         TM    BFSTAT,PRODLG       IF IT'S PRODUCTION                           
         BO    BUFS09              CONTRA REMAINS SJX X=MEDIA CODE              
         TM    BFSTAT,ACCSRC       IS RULE FROM ACCOUNT                         
         BO    *+10                                                             
         MVC   BFCNTR+2(12),SPACES IF NOT, FORCE CONTRA BY U/L                  
         CLI   PROGPROF,YES        POST BY CONTRA U/L ONLY                      
         BNE   *+10                                                             
         MVC   BFCNTR+2(12),SPACES                                              
         CLI   PROGPROF,C'L'       POST BY CONTRA U/L ONLY                      
         BNE   *+10                                                             
         MVC   BFCNTR+2(12),SPACES                                              
         CLI   PROGPROF,C'P'       POST BY POINTER ACCOUNT                      
         BNE   BUFS09                                                           
         CLC   BFPTRACT,SPACES                                                  
         BNH   BUFS09                                                           
         MVC   BFCNTR,BFPTRACT                                                  
         MVC   BFNME,BFPTRNM                                                    
                                                                                
BUFS09   MVI   BFTYPE,BFPST        SET POSTING TYPE                             
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BFREC,1                               
         MVI   BFTYPE,BFSUM        SET LEDGER SUMMARY TYPE                      
         MVC   HALF,BFOFFC         SAVE THE OFFICE                              
         L     RE,AMONACC                                                       
         TM    ACMINDS-ACMD(RE),ACMIEMUD+ACMINEWO                               
         BO    *+10                                                             
         MVC   BFOFFC,SPACES       CLEAR OFFICE (UNLESS EMULATING)              
         GOTO1 BUFFALO,DMCB        PUT SUMMARY RECORD                           
*                                                                               
*              NOW ADD THE SORT RECORDS FOR THE TRIAL BALANCE                   
*                                                                               
         MVC   SMLEDGER,BFACCT+1   LEDGER IS ALWAYS THERE                       
         CLI   SVQSEQ,QSEQOFF      FOR OFFICE SEQUENCE, OFFICE IS HIGH          
         BNE   BUFS11                                                           
         MVC   SMOFFCO,HALF                                                     
         MVC   SMACCTO,BFACCT      FOLLOWED BY ACCOUNT                          
         B     BUFS15                                                           
*                                                                               
BUFS11   MVC   SMACCT,BFACCT       "TO" ACCOUNT                                 
         XC    SMOFFC,SMOFFC                                                    
         L     RE,AMONACC                                                       
         TM    ACMINDS-ACMD(RE),ACMIEMUD+ACMINEWO                               
         BZ    BUFS15                                                           
         MVC   SMOFFC,HALF         OFFICE (IF NEW OFFICES)                      
*                                                                               
BUFS15   MVC   SMBK(L'SMBK*SMNBK),BFBK  ACCUMS                                  
         GOTO1 ADSORTER,DMCB,=C'PUT',SMREC                                      
         TM    TRNSW,TRNRGLD       Reverse posting based on GLDELD              
         BZ    *+10                                                             
         MVC   BFSTAT,SVBFSTAT     Restore                                      
                                                                                
*&&UK*&& TM    RNSW,RNOF+RNTOF     POSTING BY OFFICE                            
*&&UK*&& BZ    XIT                                                              
         MVI   BFTYPE,0            CLEAR KEY                                    
         XC    BFACCT,BFACCT                                                    
         XC    BFCNTR,BFCNTR                                                    
         MVC   BFOFFC,HALF         RESTORE THE OFFICE                           
         MVC   BYTE,BFSTAT                                                      
         MVI   BFSTAT,0                                                         
         GOTOR BINADD,DMCB,BFREC,ADOFT   ADD TO OFFICE SUMMARY                  
         MVC   BFSTAT,BYTE                                                      
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
         USING GLDELD,R4                                                        
GLCHG    NTR1                                                                   
         ICM   R4,15,AREVGLD       A(GLDEL reversing)                           
         BNZ   *+6                                                              
         DC    H'00'                                                            
         CLC   GLDULA,TOACC        Compare GB or GP Account                     
         BNE   GLCHGYES                                                         
                                                                                
         MVC   WORK(2),SPACES                                                   
*&&UK*&& TM    RNSW,RNOF+RNTOF                                                  
*&&UK*&& BZ    GLCHG20                                                          
         LA    R3,ACOFFC                                                        
         CLI   0(R3),X'40'                                                      
         BH    GLCHG10                                                          
         LA    R3,LGOFFC                                                        
         CLI   0(R3),X'40'                                                      
         BH    GLCHG10                                                          
         LA    R3,POOFFC                                                        
         CLI   0(R3),X'40'                                                      
         BH    GLCHG10                                                          
         LA    R3,COOFFC                                                        
GLCHG10  MVC   WORK,0(R3)                                                       
         OC    BFOFFC,SPACES                                                    
                                                                                
GLCHG20  CLC   GLDOFFC,WORK        Compare Office code                          
         BNE   GLCHGYES                                                         
                                                                                
         TM    BFSTAT,PRODLG       If not production then                       
         BZ    GLCHG30             we don't care                                
         CLC   GLDCNTRA,BFNME      Compare contra, name changed                 
         BNE   GLCHGYES                                                         
                                                                                
GLCHG30  CLC   GLDOFFO,OFFORIG     Compare original office                      
         BNE   GLCHGYES                                                         
                                                                                
         SR    RE,RE               RE=0, no change so skip transaction          
GLCHGYES LTR   RE,RE               RE<>0 then change                            
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD NEW GENERAL LEDGER DETAIL ELEMENT TO TRANSACTION                          
***********************************************************************         
                                                                                
         USING GLDELD,R4                                                        
         USING TRNRECD,R6                                                       
GLELEM   NTR1                                                                   
         TM    TRNSW,TRNUPDB       Was this updated already                     
         JO    XIT                 Yes, so don't update again                   
                                                                                
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         MVC   SVTRNMOA,ACMMDTE                                                 
         DROP  RF                                                               
                                                                                
         L     R4,ADTRANS                                                       
         LR    R6,R4                                                            
         SH    R6,DATADISP         START OF RECORD                              
         SR    RF,RF                                                            
         ICM   RF,3,TRNRLEN                                                     
         AHI   RF,GLDLNQ                                                        
         CHI   RF,1900             To big ?                                     
         BNH   *+6                 No                                           
         DC    H'00'                                                            
                                                                                
         XC    NEWGLEL,NEWGLEL     SAY NO                                       
         SR    RF,RF                                                            
         TM    TRNSW,TRNREV        Reversing ?                                  
         BZ    GLELEM10            No                                           
         ICM   R4,15,AREVGLD       A(GLDEL reversing)                           
         BNZ   GLELEM50                                                         
         DC    H'00'               Problem                                      
                                                                                
GLELEM10 CLI   0(R4),EOR           END OF RECORD                                
         JE    GLELEM20            ADD NEW ELEMENT                              
         CLI   0(R4),GLDELQ        X'63' FIND UNUSED ELEMENT                    
         JNE   GLELEM18                                                         
         TM    RNSW,RNYES          RERUN=YES OR RERUN=ALL                       
         BZ    GLELEM16                                                         
         CLC   GLDDATE,TODAY3      Was it marked today                          
         BE    GLELEM50            Yes, so use existing element                 
                                                                                
GLELEM16 OC    GLDDATE,GLDDATE     Any date marked ?                            
         JNZ   *+8                                                              
         ST    R4,NEWGLEL          FOUND unused element                         
                                                                                
GLELEM18 IC    RF,1(,R4)                                                        
         AR    R4,RF                                                            
         J     GLELEM10                                                         
                                                                                
GLELEM20 ICM   R4,15,NEWGLEL       A(New GL element)                            
         BNZ   *+8                                                              
         LA    R4,ELEMENT                                                       
         MVI   GLDEL,GLDELQ        X'63' GENERAL LEDGER DETAILS                 
         MVI   GLDLN,GLDLNQ        LENGTH OF ELEMENT                            
         MVC   GLDDATE,TODAY3      Same date as TRSUPDT                         
         MVC   GLDUPTM,DAYTIME                                                  
         MVC   GLDTODAY,TODAYP     Today, not based on DATE card                
         MVC   GLDTORUL,ACCRULE                                                 
         MVI   GLDPRG#,GLDAC25     VIA AC25                                     
         MVC   GLDPMOS,START3                                                   
                                                                                
GLELEM22 LA    R3,SPACES                                                        
         MVC   GLDOFRUL,OFARULE    Set default rule                             
*&&UK*&& TM    RNSW,RNOF+RNTOF     POST TO G/L BY OFFICE                        
*&&UK*&& BZ    GLELEM30            NO                                           
         OI    GLDIND,GLDBYOFF                                                  
         LA    R3,ACOFFC           ADD OFFICE FOR POSTING                       
         CLI   0(R3),X'40'         FIRST TRY ACCOUNT GLOFFC                     
         BH    GLELEM30                                                         
         MVI   GLDOFRUL,GLDOFLDG                                                
         LA    R3,LGOFFC           THEN LEDGER GLOFFC                           
         CLI   0(R3),X'40'                                                      
         BH    GLELEM30                                                         
         MVC   GLDOFRUL,OFTRULE    COULD BE FITLER,ACCT OR TRANS                
         LA    R3,POOFFC           THEN OFFPOS=F1,OFFPOS=1,OFFPOS=T             
         CLI   0(R3),X'40'                                                      
         BH    GLELEM30                                                         
         MVI   GLDOFRUL,GLDOFCPY                                                
         LA    R3,COOFFC           DEFAULT IS COMPANY                           
                                                                                
GLELEM30 MVC   GLDOFFC,0(R3)       GL POSTING OFFICE                            
         OC    GLDOFFC,SPACES                                                   
         MVC   GLDOFFO,OFFORIG                                                  
                                                                                
GLELEM34 MVC   GLDULA,TOACC        GL ACCOUNT                                   
         MVC   GLDCNTRA,BFNME      GL CONTRA ACCOUNT IS MEDIA NAME              
         TM    BFSTAT,PRODLG       IF IT'S PRODUCTION                           
         BO    GLELEM40            Contra reflects media code                   
         MVC   GLDCCPY,RCCOMPFL    Company code                                 
         MVC   GLDCULA,FRACC       GL CONTRA ACCOUNT                            
         TM    BFSTAT,ACCSRC       IS RULE FROM ACCOUNT                         
         BO    *+10                                                             
         MVC   GLDCACT,SPACES      IF NOT, FORCE CONTRA BY U/L                  
                                                                                
         CLI   PROGPROF,YES        POST BY CONTRA U/L ONLY                      
         BNE   *+10                                                             
         MVC   GLDCACT,SPACES                                                   
                                                                                
         CLI   PROGPROF,C'L'       POST BY CONTRA U/L ONLY                      
         BNE   *+10                                                             
         MVC   GLDCACT,SPACES                                                   
                                                                                
         CLI   PROGPROF,C'P'       POST BY POINTER ACCOUNT                      
         BNE   GLELEM40                                                         
         CLC   BFPTRACT,SPACES                                                  
         BNH   GLELEM40                                                         
         MVC   GLDCCPY,RCCOMPFL                                                 
         MVC   GLDCULA,BFPTRACT                                                 
                                                                                
GLELEM40 OC    NEWGLEL,NEWGLEL     ADD NEW GLDELQ                               
         JNZ   GLELEM50            NO                                           
         GOTOR HELLO,DMCB,(C'P',ACCFIL),(R6),(R4),0                             
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R4,16(,R1)          Load location of element                     
                                                                                
GLELEM50 TM    UPSI,UPSI63         Dump to see GLDEL                            
         JZ    GLELEM55                                                         
         LH    R0,DATADISP                                                      
         GOTOR =V(PRNTBL),DMCB,=C'63 Elem',(R6),C'DUMP',(R0),=C'1R',   X        
               (C'P',V(PRINT))                                                  
                                                                                
         USING BUKTABD,R5                                                       
GLELEM55 LA    R5,BUKREC                                                        
         XC    BUKREC,BUKREC                                                    
         MVC   BUKGLOTH,SPACES                                                  
         CLC   SVTRNMOA,GLMOA                                                   
         JNL   GLELEM60            This one okay so create                      
         CLI   OFFNEW,YES                                                       
         JE    GLELEMX             Don't bother                                 
         CLC   GLMOA,=X'FFFF'                                                   
         JE    GLELEMX             Don't create not set at all                  
                                                                                
GLELEM60 OI    GLDIND,GLDUBUK      New GL bucket updated                        
         MVC   GLDPMOS,START3                                                   
         MVC   BUKGLACT,GLDLDG                                                  
         MVC   BUKGLOFF,GLDOFFC                                                 
         MVC   BUKGLCUL,SPACES     Pad with spaces                              
         MVC   BUKGLMOA,SVTRNMOA   Gotten from MONACC                           
                                                                                
         L     RF,ADTRANS                                                       
         CLI   TRNKLDG,C'J'        Production ?                                 
         JE    GLELEM80            Okay as is                                   
         CLI   TRNKLDG,C'R'        Recievables                                  
         JNE   GLELEM68                                                         
GLELEM62 CLI   0(RF),EOR           End of record                                
         BE    GLELEM80            Done                                         
         CLI   0(RF),MDTELQ        X'1A', full word version                     
         BE    GLELEM63                                                         
         CLI   0(RF),MDPELQ        X'6A', packed version                        
         BE    GLELEM64                                                         
         LLC   R1,1(,RF)                                                        
         AR    RF,R1                                                            
         J     GLELEM62                                                         
                                                                                
         USING MDTELD,RF                                                        
GLELEM63 LA    RE,MDTSYS           System media                                 
         TM    MDTSTAT,MDTSMIQ     MI record ?                                  
         JZ    GLELEM66                                                         
         CLC   MDTMED2,SPACES                                                   
         JNH   GLELEM66                                                         
         LA    RE,MDTMED2          MI record                                    
         J     GLELEM66                                                         
                                                                                
         USING MDPELD,RF                                                        
GLELEM64 LA    RE,MDPSYS           System media                                 
         TM    MDPSTAT,MDPSMIQ     MI record ?                                  
         JZ    GLELEM66                                                         
         CLC   MDPMED2,SPACES                                                   
         JNH   GLELEM66                                                         
         LA    RE,MDPMED2          MI record                                    
                                                                                
GLELEM66 MVC   BUKSM,0(RE)         Save off system/media                        
         J     GLELEM80                                                         
         DROP  RF                                                               
                                                                                
GLELEM68 DS    0H                                                               
*&&US                                                                           
         BRAS  RE,ISCLRNCE         Is clearance releated transaction            
         JNE   GLELEM78                                                         
         MVC   BUKGLCUL,SPACES                                                  
         MVC   BUKGLCLI,TRNKCCPY+12                                             
         CLC   TRNKCCPY+12(3),SPACES                                            
         JE    GLELEM72                                                         
         CLC   TRNKCCPY(12),SPACES                                              
         JE    GLELEM80            Client is true contra                        
         MVC   BUKGLCUL,=X'4141'   Has client & rep/station                     
         B     GLELEM80                                                         
                                                                                
GLELEM72 CLI   TRNKCLDG,C'/'       Is media/cli                                 
         BNE   GLELEM80                                                         
         SR    RF,RF                                                            
         ICM   RF,1,LENCLI                                                      
         BZ    GLELEM80                                                         
         AHI   RF,1                One more for media                           
         EXMVC RF,BUKGLCUL,TRNKCUNT                                             
         B     GLELEM80                                                         
*&&                                                                             
GLELEM78 GOTOR GETLV1,DMCB,TRNKCUNT,0                                           
         SR    R1,R1                                                            
         ICM   R1,1,DMCB           Get len of 1st level of contra               
         BZ    GLELEM80            Not value contra u/l                         
         BCTR  R1,0                One for EX instr.                            
         EXMVC R1,BUKGLCUL,TRNKCUNT                                             
                                                                                
         USING TRNELD,RF                                                        
GLELEM80 L     RF,ADTRANS                                                       
         MVC   BUKGLSOF,TRNOFFC    Transaction office code                      
         ZAP   BUKGLDR,PKZERO      Debit                                        
         ZAP   BUKGLCR,PKZERO      Credit                                       
         LA    RE,BUKGLDR                                                       
         TM    TRNSTAT,TRNSDR      Debit                                        
         BO    *+8                 Yes                                          
         LA    RE,BUKGLCR                                                       
         ZAP   0(L'BUKGLDR,RE),AMOUNT                                           
         DROP  RF                                                               
                                                                                
         TM    RQSW,RQREV          Test reverse option                          
         BZ    GLELM86                                                          
         TM    TRNSW,TRNREV        Was this reversed ?                          
         BZ    XIT                 Only reverse if item is reversed             
                                                                                
GLELM86  MVI   BUKGLIND,NO                                                      
         GOTOR BINADD,DMCB,BUKREC,ADBUKTAB                                      
                                                                                
GLELEMX  J     XIT                                                              
         DROP  R5,R6                                                            
         EJECT ,                                                                
***********************************************************************         
* See if tranaction is clearance related                                        
***********************************************************************         
         USING TRNRECD,R2                                                       
         USING TRNELD,R3                                                        
ISCLRNCE NTR1                                                                   
         L     R2,ADTRANS                                                       
         LR    R3,R2                                                            
         SH    R2,DATADISP                                                      
         CLI   TRNKLDG,C'C'        Cash side of clearance                       
         JE    ISCLR10                                                          
         CLI   TRNKLDG,C'S'        Spot / Net                                   
         JE    ISCLR10                                                          
         CLI   TRNKLDG,C'T'        Spot / Net                                   
         JE    ISCLR10                                                          
         CLI   TRNKLDG,C'U'        Net                                          
         JE    ISCLR10                                                          
         CLI   TRNKLDG,C'P'        Print                                        
         JE    ISCLR20                                                          
         CLI   TRNKLDG,C'Q'        Print                                        
         JE    ISCLR20                                                          
         J     ISCLR_NO            No                                           
                                                                                
ISCLR10  CLI   TRNTYPE,33          Clearance                                    
         JE    ISCLR50                                                          
         CLI   TRNTYPE,34          Clearance                                    
         JE    ISCLR50                                                          
         CLI   TRNKLDG,C'C'        Cash side of clearance                       
         JE    ISCLR20                                                          
         CLI   TRNTYPE,129         Check                                        
         JE    ISCLR_YS                                                         
         J     ISCLR30                                                          
                                                                                
ISCLR20  CLI   TRNTYPE,49          Clearance                                    
         JE    ISCLR50                                                          
         CLI   TRNTYPE,50          Clearance                                    
         JE    ISCLR50                                                          
         CLI   TRNKLDG,C'C'        Cash side of clearance                       
         JE    ISCLR30                                                          
         CLI   TRNTYPE,129         Check                                        
         JE    ISCLR_YS                                                         
                                                                                
ISCLR30  CLI   TRNTYPE,13          Online (Input) Clearance                     
         JE    ISCLR_YS                                                         
         CLI   TRNTYPE,37          Void                                         
         JE    ISCLR60                                                          
         CLI   TRNTYPE,36          Manual check                                 
         JNE   ISCLR_NO                                                         
         J     ISCLR60                                                          
                                                                                
ISCLR50  CLI   TRNKLDG,C'C'        Cash                                         
         BE    ISCLR60                                                          
         CLI   0(R3),EOR                                                        
         JE    ISCLR_NO                                                         
         CLI   0(R3),XPYELQ        X'46' extra payment, clearance               
         JE    ISCLR_YS            At this point I say it is                    
         LLC   RF,1(,R3)                                                        
         AR    R3,RF                                                            
         J     ISCLR50                                                          
                                                                                
ISCLR60  CLI   TRNKCLDG,C'/'       Special case                                 
         JE    ISCLR_YS                                                         
         CLC   TRNKCACT+9(3),SPACES                                             
         JNH   ISCLR_NO                                                         
         CLC   TRNKCCPY(12),SPACES                                              
         JE    ISCLR_YS                                                         
         CLI   TRNKCUNT,C'S'       See if unit S                                
         JE    ISCLR_NO            Yes, so not clearance                        
                                                                                
ISCLR_YS SR    RE,RE                                                            
ISCLR_NO LTR   RE,RE                                                            
         J     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
* PRINT SUMMARY OF "FROM" "TO" ACCOUNTS                               *         
***********************************************************************         
         SPACE 1                                                                
ACTS     NTR1                                                                   
         L     R7,ADACT            ACCOUNT SUMMARY RECORDS                      
         TM    RQSW,RQER           PRINT THE ERROR REPORT                       
         BNO   *+8                                                              
         L     R7,ADERR            ERROR RECORDS                                
         USING BIND,R7                                                          
         ICM   R3,15,BININ         NUMBER IN TABLE                              
         BZ    XIT                                                              
         XC    BININ,BININ         CLEAR FOR NEXT TIME                          
         LA    R7,BINTABLE                                                      
         MVC   BFREC,0(R7)                                                      
         OI    PRNSW,PRNACN+PRNCAN PRINT ACCOUNT AND CONTRA NAMES               
         NI    PRNSW,ALL-(PRNACT+PRNCAT) TURNOFF TOTAL SWITCHES                 
         LA    R5,ACCTOT                                                        
         BAS   RE,CLR              CLEAR ACCOUNT TOTALS                         
         LA    R5,CACTOT                                                        
         BAS   RE,CLR              AND CONTRA TOTALS                            
*                                                                               
ACTS03   LA    R5,BFBK             R5 RECORD TOTALS                             
         LA    R6,CACTOT           R6 CONTRA TOTALS                             
         BAS   RE,ADDUP            ADD RECORD TO CONTRA TOTALS                  
         LA    R6,ACCTOT           R6 ACCOUNT TOTALS                            
         BAS   RE,ADDUP            ADD RECORD TO ACCOUNT TOTALS                 
         BAS   RE,ZERO             TEST FOR ZERO AMOUNTS                        
         BNE   ACTS05                                                           
         LA    R7,BFLNQ(R7)        SKIP NIL DATA                                
         MVC   BFREC,0(R7)         SAVE NEXT RECORD                             
         BCT   R3,ACTS03           LOOK AT NEXT                                 
         B     ACTS16              END OF RECORDS                               
*                                                                               
ACTS05   LA    R8,P                                                             
         TM    PRNSW,PRNACN        PRINT ACCOUNT CODE / NAME                    
         BNO   ACTS08              ALREADY PRINTED                              
         BRAS  RE,PRN              SKIP A LINE                                  
         MVC   P+1(12),BFACCT+2    FROM ACCOUNT                                 
         TM    RQSW,RQER           IS THIS THE OFFICE DEFAULT REPORT            
         BNO   *+10                                                             
         MVC   P+1(14),BFACCT      NEED UNIT AND LEDGER                         
         MVC   WORK(L'BFNME),BFNME NAME                                         
         LA    RF,P+6                                                           
         TM    BFSTAT,PRODLG       IF PRODUCTION IT'S MEDIA                     
         BO    ACTS07                                                           
         LA    RF,P+15                                                          
*                                                                               
ACTS07   GOTO1 CHOPPER,DMCB,(35,WORK),(25,(RF)),(C'P',2)                        
         CLC   PSECOND,SPACES      DID NAME TAKE 2 LINE                         
         BE    *+8                                                              
         LA    R8,PSECOND          PRINT THE REST ON LINE 2                     
         MVC   100(7,R8),AC@ACC                                                 
         TM    BFSTAT,ACCSRC       IS IT FROM ACCOUNT POINTER                   
         BO    *+10                                                             
         MVC   100(7,R8),AC@LGR    LEDGER                                       
         NI    PRNSW,ALL-PRNACN    DON'T DO IT AGAIN                            
         MVC   FRACC,BFACCT        SAVE FROM ACCOUNT                            
*                                                                               
ACTS08   TM    PRNSW,PRNCAN        PRINT CONTRA "TO" NAME                       
         BNO   ACTS10                                                           
         CLI   P+1,C' '            IF PRINTING FROM ACCOUNT                     
         BH    ACTS09              ALREADY SKIPPED A LINE                       
         CLI   BFOFFC,C' '         POSTING BY OFFICE                            
         BE    ACTS09              IF NOT, NO NEED FOR A BLANK LINE             
         BRAS  RE,PRN              ELSE, LEAVE A BLANK LINE HERE                
*                                                                               
ACTS09   MVC   41(14,R8),BFCNTR    TO ACCOUNT                                   
         NI    PRNSW,ALL-PRNCAN                                                 
*                                                                               
ACTS10   LA    R5,BFBK             R5 RECORD TOTALS                             
         BAS   RE,EDIT             EDIT THE LINE DEBITS/CREDITS                 
*                                                                               
ACTS12   MVC   TOACC,BFCNTR        SAVE LAST "TO" ACCOUNT                       
         MVC   59(2,R8),BFOFFC                                                  
         BRAS  RE,PRN              PRINT THE ACCOUNT LINE                       
         SH    R3,=H'1'                                                         
         BZ    ACTS16              END OF RECORDS                               
         LA    R7,BFLNQ(R7)                                                     
         MVC   BFREC,0(R7)         SAVE NEXT RECORD                             
         CLC   BFACCT,FRACC        SAME "FROM" ACCOUNT                          
         BE    ACTS14                                                           
         OI    PRNSW,PRNACN        NEXT TIME PRINT NEW NAME                     
         XC    TOACC,TOACC         FORCE NEW CONTRA                             
*                                                                               
ACTS14   CLC   BFCNTR,TOACC        IS IT THE SAME "TO" ACCOUNT                  
         BNE   ACTS15              CHANGE OF "TO" ACCOUNT                       
         OI    PRNSW,PRNCAT        NEED CONTRA TOTAL                            
         B     ACTS03                                                           
*                                                                               
ACTS15   TM    BFSTAT,PRODLG       FOR PRODUCTION                               
         BO    *+8                 SKIP THE TOTAL                               
         OI    PRNSW,PRNACT        TURN ON ACCOUNT TOTAL                        
*                                                                               
ACTS16   LA    R5,CACTOT                                                        
         TM    PRNSW,PRNCAT        IS CONTRA TOTAL REQUIRED                     
         BNO   ACTS17                                                           
         MVC   P+57(L'AC@TOTAL),AC@TOTAL                                        
         LA    R8,P                                                             
         BAS   RE,EDIT             TOTAL FOR THIS "TO" ACCOUNT                  
         BRAS  RE,PRN                                                           
*                                                                               
ACTS17   BAS   RE,CLR              CLEAR CONTRA TOTAL                           
         NI    PRNSW,ALL-PRNCAT    TURNOFF CONTRA TOTAL                         
         OI    PRNSW,PRNCAN        PRINT NAME FOR NEXT                          
         LTR   R3,R3               EOF                                          
         BNZ   ACTS03              PROCESS NEXT                                 
         LA    R5,ACCTOT           R5 ACCOUNT TOTALS                            
         TM    PRNSW,PRNACT        IS ACCOUNT TOTAL                             
         BNO   ACTS25                                                           
         MVI   P+41,C'*'                                                        
         MVC   P+42(L'AC@TOTLS),AC@TOTLS                                        
         LA    RE,P+42+L'AC@TOTLS                                               
         CLI   0(RE),C' '          FIND LAST CHARACTER                          
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         MVI   1(RE),C'*'          ADD A TRAIL *                                
         LA    R8,P                                                             
         BAS   RE,EDIT             TOTAL FOR THIS ACCOUNT                       
         BRAS  RE,PRN                                                           
*                                                                               
ACTS25   LA    R6,LDGTOT           R6 LEDGER TOTALS                             
         BAS   RE,ADDUP            ADD ACCOUNT TO LEDGER TOTALS                 
         BAS   RE,CLR              CLEAR ACCOUNT TOTALS                         
         B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT A SUMMARY FROM BUFFALO RECORDS                                *         
* R4 = LEVEL NUMBER                                                   *         
***********************************************************************         
         SPACE 1                                                                
SUM      NTR1                                                                   
         MVI   FORCEHED,YES        NEW PAGE FOR SUMMARY                         
         XC    LSTACC,LSTACC                                                    
         XC    BFREC,BFREC         CLEAR BUFFALO RECORD                         
         NI    PRNSW,ALL-PRNTOT+PRNCATS RESET PRINT ACCNT/CONTRA TOTALS         
         LA    R0,BFSUM                                                         
         GOTO1 BUFFALO,DMCB,=C'HIGH',((R0),ADBUF),BFREC,(R4)                    
         B     SUM01                                                            
*                                                                               
SUM00    LA    R0,BFSUM                                                         
         GOTO1 BUFFALO,DMCB,=C'SEQ',((R0),ADBUF),BFREC,(R4)                     
SUM01    TM    DMCB+8,X'80'        IS IT EOF, ON BUFFALO                        
         BNO   *+16                PRINT LAST ACCOUNT TOTAL                     
         MVI   BFACCT,X'FF'        SET EOF INDICATOR                            
         MVI   BFCNTR,X'FF'                                                     
         MVI   BFNME,X'FF'                                                      
         CLC   BFACCT,LSTACC       CHANGE ON ACCOUNT?                           
         BNE   SUM02               YES                                          
         LA    R1,BFCNTR           CONTRA ACCOUNT CODE                          
         TM    BFSTAT,PRODLG       FOR PRODUCTION                               
         BZ    *+8                                                              
         LA    R1,BFNME+3          USE MEDIA NAME                               
         CLC   LSTCNTNM,0(R1)      SAME CONTRA-ACCOUNT NAME?                    
         BE    SUM04                                                            
*                                                                               
SUM02    TM    PRNSW,PRNCATS       DO WE PRINT CONTRA-ACCOUNT TOTAL?            
         BNO   SUM03                                                            
         LA    R5,CACTOT           EDIT THE CONTRA-ACCOUNT TOTAL                
         LA    R8,P                                                             
         BAS   RE,EDIT                                                          
         MVC   P+57(L'AC@TOTAL),AC@TOTAL                                        
         BRAS  RE,PRN                                                           
*                                                                               
SUM03    L     RE,AMONACC                                                       
         TM    ACMINDS-ACMD(RE),ACMIEMUD+ACMINEWO                               
         BNO   *+8                                                              
         BRAS  RE,PRN              SKIP A LINE                                  
         LA    R5,CACTOT                                                        
         BAS   RE,CLR              CLEAR CONTRA-ACCOUNT TOTALS                  
         NI    PRNSW,ALL-PRNCATS   RESET PRINT CONTRA-ACCOUNT TOTALS            
         XC    LSTCNTNM,LSTCNTNM   CLEAR LAST CONTRA-ACCOUNT NAME               
*                                                                               
SUM04    CLC   BFACCT,LSTACC       SAME "TO" ACCOUNT?                           
         BE    SUM06                                                            
         TM    PRNSW,PRNTOT        DO WE NEED ACCOUNT TOTAL                     
         BNO   SUM05                                                            
         LA    R5,ACCTOT           EDIT THE ACCOUNT TOTAL                       
         LA    R8,P                                                             
         BAS   RE,EDIT                                                          
         MVC   P+10(L'AC@TOTFR),AC@TOTFR                                        
         MVC   P+21(14),LSTACC                                                  
         MVI   SPACING,2                                                        
         BRAS  RE,PRN                                                           
*                                                                               
SUM05    LA    R5,ACCTOT                                                        
         BAS   RE,CLR              CLEAR ACCOUNT TOTALS                         
         NI    PRNSW,ALL-PRNTOT    RESET PRINT ACCOUNT TOTALS                   
         CLI   BFACCT,X'FF'        EOF                                          
         BE    XIT                                                              
*                                                                               
SUM06    LA    R5,BFBK                                                          
         LA    R6,ACCTOT                                                        
         BAS   RE,ADDUP            ADD RECORD AMOUNTS TO ACCOUNT TOTAL          
         BAS   RE,ZERO             TEST THE TOTALS                              
         BZ    SUM00               NOTHING TO PRINT                             
         LA    R8,P                                                             
         CLC   BFACCT,LSTACC       IF SAME "TO" ACCOUNT                         
         BNE   SUM07                                                            
         LA    R1,BFCNTR           AND DIFFERENT CONTRA-ACCOUNT                 
         TM    BFSTAT,PRODLG                                                    
         BZ    *+8                                                              
         LA    R1,BFNME+3                                                       
         CLC   LSTCNTNM,0(R1)                                                   
         BE    SUM08                                                            
         OI    PRNSW,PRNTOT        THIS ACCOUNT WILL NEED A TOTAL               
         B     SUM08                                                            
*                                                                               
SUM07    BRAS  RE,PRN              SKIP A LINE                                  
         MVC   P+1(14),BFACCT      ACCOUNT CODE                                 
         BAS   RE,TONAME                                                        
         MVC   LSTACC,BFACCT                                                    
         CLC   PSECOND,SPACES      IS THERE DATA ON LINE 2                      
         BE    *+8                                                              
         LA    R8,PSECOND          AMOUNTS GO ON LINE 2                         
*                                                                               
SUM08    LA    R5,BFBK                                                          
         BAS   RE,EDIT                                                          
         LA    R5,BFBK                                                          
         LA    R6,CACTOT                                                        
         BAS   RE,ADDUP            ADD RECORD AMOUNTS TO CNTRA-ACCT TOT         
         LA    R1,BFCNTR           CONTRA ACCOUNT CODE                          
         TM    BFSTAT,PRODLG       FOR PRODUCTION                               
         BZ    *+8                                                              
         LA    R1,BFNME+3          USE MEDIA NAME                               
         CLC   LSTCNTNM,0(R1)      SAME CONTRA-ACCOUNT NAME?                    
         BE    *+14                                                             
         MVC   41(14,R8),0(R1)                                                  
         B     *+8                                                              
         OI    PRNSW,PRNCATS       THIS CONTRA-ACCOUNT NEEDS A TOTAL            
         MVC   LSTCNTNM,0(R1)                                                   
         L     RE,AMONACC                                                       
         TM    ACMINDS-ACMD(RE),ACMIEMUD+ACMINEWO                               
         BNO   *+10                                                             
         MVC   59(2,R8),BFOFFC     OFFICE CODE                                  
         BRAS  RE,PRN              PRINT THE ACCOUNT LINE                       
         B     SUM00                                                            
         EJECT                                                                  
***********************************************************************         
* OFFICE SUMMARY (IF POSTING BY OFFICE)                               *         
***********************************************************************         
         SPACE 1                                                                
OFS      NTR1                                                                   
         L     R7,ADOFT            A(OFFICE TABLE)                              
         USING BIND,R7                                                          
         ICM   R3,15,BININ         NUMBER IN TABLE                              
         BZ    XIT                 NO OFFICE ENTRIES                            
         XC    BININ,BININ         CLEAR TABLE FOR NEXT TIME                    
         MVI   SPACING,2                                                        
         BRAS  RE,PRN              SKIP A FEW LINES                             
         LA    R7,BINTABLE                                                      
         MVC   BFREC,0(R7)         GET THE FIRST RECORD                         
         LA    RF,P+6                                                           
         TM    BFSTAT,PRODLG       PRODUCTION                                   
         BO    *+8                                                              
         LA    RF,P+15                                                          
         MVC   0(L'AC@OFFTS,RF),AC@OFFTS  OFFICE TOTALS                         
         LA    R8,P                SET PRINT LINE                               
*                                                                               
OFS03    MVC   BFREC,0(R7)         GET THE FIRST RECORD                         
         MVC   P+59(2),BFOFFC      OFFICE CODE                                  
         LA    R5,BFBK             EDIT DEBITS CREDITS                          
         BAS   RE,EDIT                                                          
         BRAS  RE,PRN              PRINT THE OFFICE LINE                        
         LA    R7,BFLNQ(R7)                                                     
         BCT   R3,OFS03                                                         
*                                                                               
         LA    R5,LDGTOT           LEDGER TOTAL                                 
         BAS   RE,EDIT             EDIT CURRENT DEBITS/CREDITS                  
         MVI   P+41,C'*'                                                        
         MVC   P+43(L'AC@TOTLS),AC@TOTLS                                        
         MVI   P+45+L'AC@TOTLS,C'*'                                             
         BRAS  RE,PRN              PRINT THE CURRENT TOTALS                     
         B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* FORMAT AND PRINT THE TOTAL LINES                                   *          
**********************************************************************          
         SPACE 1                                                                
TOTLN    NTR1                                                                   
         CLC   P,SPACES            DON'T SPACE A LINE IF DATA TO PRINT          
         BNE   *+8                                                              
         BRAS  RE,PRN                                                           
         LA    R8,P                                                             
         BAS   RE,EDIT             EDIT CURRENT DEBITS/CREDITS                  
         MVI   P+41,C'*'                                                        
         MVC   P+43(L'AC@TOTLS),AC@TOTLS                                        
         MVI   P+45+L'AC@TOTLS,C'*'                                             
         MVI   SPACING,2                                                        
         BRAS  RE,PRN              PRINT THE CURRENT TOTALS                     
*                                                                               
         LA    R5,L'BFBK*2(R5)     R5 TO PREVIOUS UPDATES                       
         BAS   RE,ZERO             TEST FOR ZERO ACCUMULATORS                   
         BZ    TOTL03                                                           
         BAS   RE,EDIT             EDIT THE PREVIOUS DEBITS/ CREDITS            
         MVC   P+28(L'AC@PRVUP),AC@PRVUP   PREVIOUS UPDATES                     
         MVC   P+29+L'AC@PRVUP(3),HDATE    MONTH                                
         MVI   SPACING,2                                                        
         BRAS  RE,PRN                                                           
*                                                                               
TOTL03   TM    RQSW,RQIR           INVOICE REGISTER SYSTEM                      
         BNO   XIT                                                              
         LA    R5,L'BFBK*2(R5)     R5 TO UNAUTHORIZED                           
         BAS   RE,ZERO             ANY NON-ZERO DATA                            
         BZ    XIT                 NO PREVIOUS UPDATES                          
         BAS   RE,EDIT             EDIT THE DEBITS/ CREDITS                     
         MVI   P+30,C'*'                                                        
         MVC   P+32(L'AC@UATHT),AC@UATHT                                        
         MVI   P+34+L'AC@UATHT,C'*'                                             
         BRAS  RE,PRN                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET THE NAME OF THE TO ACCOUNT                                      *         
***********************************************************************         
         SPACE 1                                                                
TONAME   NTR1                                                                   
         MVC   IOA(42),SPACES                                                   
         MVC   IOA(1),RCCOMPFL     SET KEY FOR TO ACCOUNT                       
         MVC   IOA+1(14),BFACCT                                                 
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCOUNT,IOA,IOA                              
         CLC   BFACCT,IOA+1                                                     
         BNE   XIT                                                              
         LA    R4,IOA                                                           
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETEL            GET NAME ELEMENT                             
         BE    *+6                                                              
         DC    H'0'                NO NAME                                      
         LA    R5,WORK                                                          
         BAS   RE,GETNME           GET NAME INTO WORK                           
         GOTO1 CHOPPER,DMCB,(36,WORK),(25,P+15),(C'P',2)                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
*              TEST FOR ZERO ACCUMULATOR AMOUNTS                                
*                                                                               
ZERO     CP    0(8,R5),PKZERO                                                   
         BNER  RE                                                               
         CP    8(8,R5),PKZERO                                                   
         BR    RE                                                               
*                                                                               
*              R2=OUTPUT                                                        
*              R5=INPUT                                                         
*                                                                               
EDIT     NTR1  ,                                                                
         LA    R2,63(R8)                                                        
         LA    R3,2                                                             
*                                                                               
EDIT2    CP    0(8,R5),PKZERO                                                   
         BE    EDIT4                                                            
         CURED (P8,0(R5)),(17,0(R2)),2,MINUS=YES,COMMAS=YES                     
*                                                                               
EDIT4    LA    R5,8(R5)                                                         
         LA    R2,18(R2)                                                        
         BCT   R3,EDIT2                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ADD TO HIGHER LEVEL ACCUMS                                       
*              R5= LOWER LEVEL ACCUMS                                           
*              R6= HIGHER LEVEL                                                 
*                                                                               
ADDUP    LR    RF,R5               SAVE R5                                      
         LA    R0,BFNBK            NUMBER OF ACCUMS                             
         AP    0(L'BFBK,R6),0(L'BFBK,R5)                                        
         LA    R5,L'BFBK(R5)                                                    
         LA    R6,L'BFBK(R6)                                                    
         BCT   R0,*-14                                                          
         LR    R5,RF               RESTORE R5                                   
         BR    RE                                                               
*                                                                               
*                                                                               
*              CLEAR TOTAL LINE                                                 
*              R5= LEVEL ACCUMS                                                 
*                                                                               
CLR      LA    R0,BFNBK            NUMBER OF ACCUMS                             
         ZAP   0(L'BFBK,R5),PKZERO                                              
         LA    R5,L'BFBK(R5)                                                    
         BCT   R0,*-10                                                          
         BR    RE                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         USING NAMELD,R4                                                        
GETNME   SR    R1,R1                                                            
         MVC   0(36,R5),SPACES                                                  
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         EX    R1,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R5),NAMEREC                                                  
         DROP  R4                                                               
         EJECT                                                                  
XFFS     DC    (SMKLNQ)X'FF'       X'FF'S                                       
         EJECT                                                                  
PHASES   DS    0AL1                ** LOADED PHASES **                          
         DC    AL1(QPOSTWRK)                                                    
PHASEN   EQU   (*-PHASES)/L'PHASES                                              
                                                                                
         EJECT                                                                  
RELOTAB  DS    0A                                                               
         DC    V(HELLO)                                                         
         DC    V(HELEN)                                                         
         DC    A(TRIBAL)                                                        
         DC    A(CONPGE)                                                        
         DC    A(ACCTAB)                                                        
         DC    A(OFFTAB)                                                        
         DC    A(BUKTAB)                                                        
         DC    A(ERRTAB)                                                        
         DC    A(POSTLST)                                                       
         DC    A(POSTBUFF)                                                      
         DC    A(POSTOTAB)                                                      
         DC    A(BUFFALOC)                                                      
         DC    V(ACADDBUK)                                                      
         DC    X'FF'                                                            
         DROP  R9,RB                                                            
         EJECT                                                                  
***********************************************************************         
* Run first                                                                     
***********************************************************************         
RNF00    NTR1  BASE=*,LABEL=*                                                   
         L     RE,=A(RELOTAB)      RELOCATE THE A / V TYPES                     
         LA    R1,ATYPES                                                        
RNF02    MVC   0(4,R1),0(RE)                                                    
         LA    RE,4(,RE)                                                        
         LA    R1,4(,R1)                                                        
         CLI   0(RE),X'FF'                                                      
         BNE   RNF02                                                            
*                                                                               
         USING MASTD,R3                                                         
         L     R3,ADMASTC          DDMASTER                                     
         MVC   UPSI,MCUPSI                                                      
         DROP  R3                                                               
                                                                                
         MVC   ADDREC,=CL8'ADDREC'                                              
         MVC   PUTREC,=CL8'PUTREC'                                              
         MVC   GETREC,=CL8'GETREC'                                              
         MVC   ACCDIR,=CL8'ACCDIR'                                              
         MVC   ACCMST,=CL8'ACCMST'                                              
         MVC   ACCFIL,=CL8'ACCFIL'                                              
         MVC   ACCOUNT,=CL8'ACCOUNT'                                            
         MVI   PKZERO,X'0C'                                                     
         ZAP   ADD28,PKZERO                                                     
         ZAP   CHG28,PKZERO                                                     
                                                                                
         TIME  TU                                                               
         ST    R0,DAYTIME        Time of day in seconds                         
                                                                                
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         MVC   SYSEQU,ACMSYS       SAVE UK /  US SYSTEM                         
         MVI   RNSW,0              INIT RUN FIRST SWITCH                        
         CLI   RCRERUN,YES         RERUN=YES                                    
         BNE   *+8                                                              
         OI    RNSW,RNYES                                                       
         CLI   RCRERUN,C'A'        RERUN=ALL                                    
         BNE   *+8                                                              
         OI    RNSW,RNYES+RNALL                                                 
         CLI   PROGPROF+4,YES      PRINT RUN SUMMARY                            
         BNE   *+8                                                              
         OI    RNSW,RNSM                                                        
*&&UK*&& CLI   PROGPROF+5,YES      POST TO G/L BY OFFICE                        
*&&UK*&& BNE   *+8                                                              
*&&UK*&& OI    RNSW,RNOF                                                        
         CLI   PROGPROF+5,C'T'     POST TO G/L BY TRANSACTION OFFICE            
         BNE   *+8                                                              
         OI    RNSW,RNTOF                                                       
         MVI   PRNSW,0             INIT PRINT SWITCH                            
         XC    SAVEDATE,SAVEDATE                                                
         MVI   SVQSEQ,0                                                         
         GOTOR DATCON,DMCB,(5,0),(1,TODAYP)                                     
*                                                                               
         LA    R0,TOTNBK           CLEAR LEVEL ACCUMS                           
         LA    R1,TOTBK                                                         
         ZAP   0(L'BFBK,R1),PKZERO                                              
         LA    R1,L'BFBK(R1)                                                    
         BCT   R0,*-10                                                          
*                                                                               
         LA    R1,SMKLNQ           KEY FOR SORT                                 
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         MVC   SORTCARD+16(2),DUB+1  KEY LENGTH FOR SORT RECORD                 
*                                                                               
         LA    R1,SMLNQ            RECORD LENGTH                                
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         MVC   RECCARD+22(3),DUB    FOR RECCARD                                 
*                                                                               
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUF                                       
         GOTO1 DATCON,DMCB,(4,RCDATE),(X'20',TODAY)                             
         GOTO1 (RF),(R1),,(2,TODAY2)                                            
         GOTO1 (RF),(R1),,(1,TODAY3)                                            
*                                                                               
         USING MASTD,R2                                                         
         L     R2,ADMASTC                                                       
         MVC   LANG,MCLANG                                                      
         MVC   CTRY,MCCTRY                                                      
*                                                                               
         L     RF,=A(DCLIST)                                                    
         GOTO1 ADDICTAT,DMCB,C'L   ',(RF),DSLIST                                
         XC    ID,ID                                                            
*                                                                               
         L     R3,=A(PHASES)       R3=A(LOADABLE PHASE LIST)                    
         LHI   R0,PHASEN                                                        
         LA    R4,APHASES          R4=A(PHASE ADDRESSES)                        
         MVC   MCDUB,=CL8'T00AXX'                                               
RNF07    GOTOR HEXOUT,DMCB,0(R3),MCDUB+4,1                                      
         GOTOR MCVLOADM,DMCB,0                                                  
         BE    *+6                 MUSTTEST PHASE NOT FOUND                     
         DC    H'0'                                                             
         MVC   0(L'APHASES,R4),4(R1)                                            
         LA    R3,L'PHASES(,R3)                                                 
         LA    R4,L'APHASES(,R4)                                                
         BCT   R0,RNF07                                                         
         J     XIT                                                              
                                                                                
SORTCARD DC    C'SORT FIELDS=(01,00,A),FORMAT=BI,WORK=1 '                       
RECCARD  DC    C'RECORD TYPE=F,LENGTH=(000) '                                   
         LTORG                                                                  
         DROP  R2,RF                                                            
         EJECT                                                                  
***********************************************************************         
         USING LV1D,R4                                                          
GETLV1   NTR1  BASE=*,LABEL=*                                                   
         LR    R6,R1                                                            
         L     R2,4(,R1)           Ledger record if supplied                    
         L     R3,0(,R1)           Unit/Ledger to find                          
         SR    R5,R5               R5=Value to return (len of 1st lvl)          
         LA    R4,LV1LIST                                                       
         SR    R0,R0                                                            
         ICM   R0,1,LV1#OF         Number of entries                            
         BZ    GETLV100                                                         
                                                                                
GETLV020 CLC   LV1UL,0(R3)         Match on unit/ledger                         
         BE    GETLV300                                                         
         LA    R4,LV1LNQ(,R4)      Next entry                                   
         BRCT  R0,GETLV020                                                      
                                                                                
         USING LDGRECD,R2                                                       
GETLV100 LLC   RF,LV1#OF                                                        
         AHI   RF,1                                                             
         STC   RF,LV1#OF           Increase value                               
         CLI   LV1#OF,LV1MAX#                                                   
         BNH   *+6                                                              
         DC    H'00'               Need to increase LV1MAX#                     
                                                                                
         MVC   LV1UL,0(R3)         Set unit ledger                              
         MVI   LV1LEN,0            Set to zero                                  
         LTR   R2,R2               Ledger record passed                         
         BNZ   GETLV200            Yes, extract value and save                  
         LA    R2,IOKEY                                                         
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,RCCOMPFL                                                 
         MVC   LDGKUNT(2),0(R3)                                                 
         GOTOR DATAMGR,DMCB,DMREAD,ACCFIL,IOKEY,IOA,0                           
         BNE   GETLVXIT                                                         
         LA    R2,IOA                                                           
                                                                                
         USING ACLELD,R2                                                        
GETLV200 AH    R2,DATADISP                                                      
                                                                                
GETLV210 CLI   0(R2),EOR           End of record                                
         BE    GETLVXIT            Should never happen                          
         CLI   0(R2),ACLELQ        X'16' Account lengths                        
         BE    GETLV220                                                         
         LLC   RF,1(,R2)                                                        
         AR    R2,RF                                                            
         B     GETLV210                                                         
                                                                                
GETLV220 LA    R5,2                Just unit/ledger                             
         CLI   ACLELLVA,L'GLBKSCA                                               
         BH    GETLV230            Can't do it, 1st level too big               
         LLC   RF,ACLELLVA                                                      
         AR    R5,RF                                                            
                                                                                
GETLV230 STC   R5,LV1LEN           Save value                                   
                                                                                
GETLV300 IC    R5,LV1LEN           Return length of 1st level + 2               
         DROP  R2                                                               
                                                                                
GETLVXIT STC   R5,0(,R6)           Return value                                 
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FIND FILTER VALUE BASED ON OFFPOS AND WHAT LEVEL IT WILL COME FROM            
*      SAVE VALUE IN FLTOFFC AND RULE VALUE IN FLTRULE                          
***********************************************************************         
COMPFLT  NTR1  BASE=*,LABEL=*                                                   
         SR    R8,R8                                                            
         SR    R2,R2                                                            
         IC    R2,OFFP                                                          
         SHI   R2,X'F0'            (F1 TO F5) - F1 = 1 TO 5                     
         LR    RF,R2                                                            
         SLL   R2,4                SHIFT TO HIGH ORDER 4 BITS                   
                                                                                
         BCTR  RF,0                LESS ONE FOR ZERO INDEXING                   
         LA    R4,RSTLVSQ          # OF LEVELS TO CHECK                         
         LA    R5,RSTLVS           LEVEL 4 DOWN TO 1                            
COMPFLT2 SR    R1,R1                                                            
         ICM   R1,3,0(R5)          AL2(ADLVxSTA)                                
         LA    R1,ACWORKD(R1)                                                   
         ICM   R3,15,0(R1)         A(RSTELD) LEVEL 4 TO 1                       
         BZ    COMPFLT4                                                         
         IC    R8,FLTTAB(RF)                                                    
         AR    R3,R8               POINT TO FITLER VALUE                        
         CLI   0(R3),X'40'         IS IT SET ?                                  
         BH    COMPFLT6            Yes, found one                               
                                                                                
COMPFLT4 AHI   R5,L'RSTLVS         NEXT                                         
         BRCT  R4,COMPFLT2                                                      
                                                                                
COMPFLT6 OR    R2,R4               R2 = (Filter #, Act level)                   
         STC   R2,FLTRULE          SET FILTER OFFICE RULE                       
         MVC   FLTOFFC,0(R3)       Save value of filter                         
         J     XIT                                                              
                                                                                
RSTLVS   DC    AL2(ADLVDSTA-ACWORKD)                                            
         DC    AL2(ADLVCSTA-ACWORKD)                                            
         DC    AL2(ADLVBSTA-ACWORKD)                                            
         DC    AL2(ADLVASTA-ACWORKD)                                            
RSTLVSQ  EQU   (*-RSTLVS)/L'RSTLVS                                              
                                                                                
FLTTAB   DC    AL1(RSTFILT1-RSTELD)                                             
         DC    AL1(RSTFILT2-RSTELD)                                             
         DC    AL1(RSTFILT3-RSTELD)                                             
         DC    AL1(RSTFILT4-RSTELD)                                             
         DC    AL1(RSTFILT5-RSTELD)                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINARY TABLE                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R7                                                          
BINADD   NTR1  BASE=*,LABEL=*                                                   
         L     R7,4(,R1)           BINSRCH PARAMETERS                           
         L     R3,0(,R1)           A(RECORD)                                    
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,(R3)),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1                                                           
         JE    BINADDX             NOT FOUND - ADDED                            
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R6,R6                                                            
         IC    R6,BINFRST          DISP. TO FIRST BUCKET                        
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         SR    R5,R5                                                            
         IC    R5,BINNUMB          NUMBER OF BUCKETS                            
         TM    BINSTAT,X'80'                                                    
         BO    BINBIN              DATA IS BINARY                               
         AP    0(8,R4),0(8,R3)     ADD NEW TO OLD                               
         LA    R4,8(,R4)                                                        
         LA    R3,8(,R3)                                                        
         BCT   R5,*-14                                                          
         J     BINADDX                                                          
*                                                                               
BINBIN   L     RE,0(,R3)                                                        
         L     RF,0(,R4)                                                        
         AR    RF,RE                                                            
         ST    RF,0(,R4)                                                        
         LA    R3,4(,R3)                                                        
         LA    R4,4(,R4)                                                        
         BCT   R5,BINBIN                                                        
         DROP  R7                                                               
*                                                                               
BINADDX  J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK FOR OFFICE IN PPRELD, X'24' ELEMENT                          
***********************************************************************         
         USING PPRELD,R4                                                        
GETPPROF MVC   CLOFFC,SPACES                                                    
         MVC   CLOFFC(1),PPRUFORA  OLD 1 BYTE OFFICE (UK)                       
         CLI   SYSEQU,ACMSEUR      US OR UK                                     
         JE    *+10                UK                                           
         MVC   CLOFFC,PPRGAOFF     2 BYTE OFFICE (US)                           
         CLC   CLOFFC,SPACES                                                    
         JNH   GETPPRNO                                                         
         CLI   SPACES,C' '         SET CC TO EQUAL                              
         BR    RE                                                               
                                                                                
GETPPRNO LTR   RE,RE                                                            
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* FIGURE OUT OFFICE TO USE FOR POSTING                                          
***********************************************************************         
FIGOFF   NTR1  BASE=*,LABEL=*                                                   
         MVC   OFFORIG,SPACES      ORIGIN OFFICE                                
         MVC   POOFFC,SPACES                                                    
         CLI   OFFSW,OFFSTRN       TRANSACTION PROCESS                          
         BNE   FIGOFF10                                                         
                                                                                
         USING TRNELD,R4                                                        
         L     R4,ADTRANS                                                       
         TM    BFSTAT,PRODLG       Is it production?                            
         BO    *+8                 Yes                                          
         MVI   OFTRULE,GLDOFTRN    Transaction office                           
         OC    POOFFC,TRNOFFC      Set office code                              
         DROP  R4                                                               
                                                                                
FIGOFF10 TM    BFSTAT,PRODLG       IS IT PRODUCTION                             
         BZ    FIGOFF14                                                         
         MVC   OFTRULE,OFARULE                                                  
         MVC   POOFFC,CLOFFC       GET CLIENT OFFICE                            
         OC    POOFFC,SPACES                                                    
         CLI   OFFSW,OFFSACL       PROCESS AS IF IN ACCOUNT LAST ?              
         BE    FIGOFF80            YES                                          
                                                                                
FIGOFF14 CLI   OFFSW,OFFSTRN       POSTING AT TRANSACTION LEVEL                 
         BE    FIGOFF80            YES                                          
*&&UK                              NO, MUST BE ACCLAST OR LIKE ACCLAST          
         TM    RNSW,RNOF           POST TO G/L BY OFFICE                        
         BO    FIGOFF42            YES                                          
         MVC   POOFFC,SPACES       NO, SO CLEAR OUT                             
         MVI   OFTRULE,0                                                        
         B     FIGOFF90                                                         
*&&                                                                             
FIGOFF42 TM    OFFP,X'F0'          IS IT IN A FILTER                            
         BNO   FIGOFF60            NO                                           
         MVC   OFTRULE,FLTRULE     SAVE OFFICE FILTER RULE                      
         MVC   POOFFC(1),FLTOFFC   GET OFFICE CODE FROM FILTER                  
         OC    POOFFC,SPACES                                                    
         B     FIGOFF80                                                         
                                                                                
FIGOFF60 SR    R1,R1               LENGTH FOR MOVE                              
         TM    OFFP,X'40'          NEW OFFICE CODES - 2 BYTE                    
         BNO   *+8                                                              
         LA    R1,1                SET 2 BYTE LENGTH                            
         MVC   BYTE,OFFP                                                        
         NI    BYTE,X'FF'-X'40'    TURN OFF X'40' FOR NEW OFFICES               
         CLI   BYTE,1              CHECK FOR VALID OFFICE POSITION              
         BNL   *+6                 MUST BE 1                                    
         DC    H'0'                INVALID OFFICE POSITION                      
                                                                                
         CLI   BYTE,12             THRU 12                                      
         BNH   *+6                                                              
         DC    H'0'                INVALID OFFICE POSITION                      
                                                                                
         SR    R3,R3                                                            
         IC    R3,BYTE                                                          
         L     R4,ADACC            A(ACCOUNT RECORD)                            
         LA    R4,2(R3,R4)         R4 TO OFFICE CODE                            
         EXMVC R1,POOFFC,0(R4)     EXTRACT THE OFFICE CODE FROM KEY             
         MVI   OFTRULE,GLDOFACT                                                 
*                                                                               
         USING GLRELD,R4                                                        
FIGOFF80 MVC   GLOFFC,POOFFC       SET DEFAULT                                  
         MVC   OFFORIG,POOFFC      ORIGINAL OFFICE                              
         TM    LGSW,LGRL           USE G/L OFFICE RULES                         
         BZ    FIGOFF90            NO                                           
         ICM   R4,15,AGLRULE       OFFICE OVERRIDE ELEMENT                      
         BZ    FIGOFF90                                                         
         SR    R3,R3                                                            
         IC    R3,GLRPRS           NUMBER OF OFFICE CODES                       
         LA    R4,GLROFFP                                                       
FIGOFF82 CLC   0(2,R4),POOFFC      MATCH TO SUBSIDIARY OFFICE                   
         BE    FIGOFF85                                                         
         AHI   R4,4                NEXT RULE                                    
         BRCT  R3,FIGOFF82                                                      
         B     FIGOFF90            NO OVERRIDE                                  
                                                                                
FIGOFF85 MVC   GLOFFC,2(R4)        CHANGE THE OFFICE CODE                       
                                                                                
*        CLI   OFFSW,OFFSACL       ACCOUNT LAST PROCESSING                      
*        BE    FIGOFF88            YES, SO SET                                  
         TM    LGSW2,LGRLT         USE G/L RULES FOR OFFICE FOR TRANS?          
         BZ    FIGOFF90            NO                                           
                                                                                
FIGOFF88 MVC   POOFFC,GLOFFC       YES, SO USE GLOFFC INSTEAD                   
         DROP  R4                                                               
                                                                                
FIGOFF90 DS    0H                                                               
         J     XIT                                                              
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* Get ledger SJ and save of lengths for client/product                          
***********************************************************************         
         USING LDGRECD,R2                                                       
GETSJ    NTR1  BASE=*,LABEL=*                                                   
         MVI   LENCLI,0            Set to zero                                  
         MVI   LENPRD,0                                                         
         LA    R2,IOKEY                                                         
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,RCCOMPFL                                                 
         MVC   LDGKUNT(2),=C'SJ'                                                
         GOTOR DATAMGR,DMCB,DMREAD,ACCFIL,IOKEY,IOA,0                           
         BNE   GETSJX                                                           
         DROP  R2                                                               
                                                                                
         LA    R2,IOA                                                           
         LR    R3,R2                                                            
         AH    R3,DATADISP                                                      
         SR    RF,RF                                                            
GETSJ10  CLI   0(R3),EOR           End of record                                
         BE    GETSJX                                                           
         CLI   0(R3),ACLELQ        X'16' Ledger levels                          
         BE    GETSJ20                                                          
         IC    RF,1(,R3)                                                        
         AR    R3,RF                                                            
         B     GETSJ10                                                          
                                                                                
         USING ACLELD,R3                                                        
GETSJ20  MVC   LENCLI,ACLELLVA     Length  of client                            
         MVC   LENCLPR,ACLELLVB    Length of client & product                   
         LLC   RF,ACLELLVB                                                      
         LLC   R1,ACLELLVA                                                      
         SR    RF,R1                                                            
         STC   RF,LENPRD           Length of client                             
         DROP  R3                                                               
                                                                                
GETSJX   J     XIT                                                              
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* Add or put new GL bucket records now                                *         
***********************************************************************         
NEW      USING BUKELD,ELEMENT                                                   
         USING BIND,R7                                                          
         USING BUKTABD,R3                                                       
DIR      USING GLBRECD,IOKEY                                                    
REC      USING GLBRECD,IOA                                                      
                                                                                
PUTBUK   NTR1  BASE=*,LABEL=*                                                   
         L     R7,ADBUKTAB                                                      
         LA    R3,BINTABLE         Start of table                               
         ICM   R0,15,BININ         # of records to process                      
         JZ    PUTBUKX                                                          
                                                                                
         USING ACTRECD,R6                                                       
PUTBUK10 L     R6,ADACC              Account record                             
         XC    ELEMENT,ELEMENT                                                  
         MVI   NEW.BUKEL,BUKELQ      X'45' element                              
         MVI   NEW.BUKLN,BUKLN7Q     Length                                     
         MVC   NEW.BUKMOS,BUKGLMOA                                              
         ZAP   NEW.BUKDR7,BUKGLDR    Debit                                      
         ZAP   NEW.BUKCR7,BUKGLCR    Credit                                     
                                                                                
         MVC   DIR.GLBKEY,SPACES                                                
         MVI   DIR.GLBKTYP,GLBKTYPQ  X'28'                                      
         MVC   DIR.GLBKCPY,ACTKCPY                                              
         MVC   DIR.GLBKGLA,BUKGLACT  GL account                                 
         MVC   DIR.GLBKGOFF,BUKGLOFF GL office                                  
         MVC   DIR.GLBKSULA,ACTKULA  Unit S/Ledger/Account                      
         CLC   BUKGLMOA,GLMOA                                                   
         JNL   PUTBUK12            Continue as usual                            
         CLI   OFFNEW,YES          Should never happen                          
         JNE   PUTBUK14            Skip some detail if prior to GLMOA           
         DC    H'00'                                                            
                                                                                
PUTBUK12 MVC   DIR.GLBKSOFF,BUKGLSOF Unit S office                              
         MVC   DIR.GLBKSCU(L'BUKGLCUL),BUKGLCUL                                 
                                                                                
PUTBUK14 CLI   ACTKLDG,C'J'          Production                                 
         JNE   PUTBUK20                                                         
         MVC   DIR.GLBKGSJ,BFNME                                                
         CLC   BUKGLMOA,GLMOA                                                   
         JL    PUTBUK30            Skip some detail if prior to GLMOA           
         ZIC   R1,MDADSP           Disp to media (C/U/L included)               
         LA    RE,ACTKCULA                                                      
         AR    RE,R1                                                            
         MVC   DIR.GLBKMEDC,0(RE)  Move in media code                           
         SHI   R1,4                Less C/U/L and 1 for EX instr                
         MVC   DIR.GLBKCP,SPACES                                                
         EXMVC R1,DIR.GLBKCP,ACTKACT Move in client/product                     
         J     PUTBUK30                                                         
         DROP  R6                                                               
                                                                                
PUTBUK20 CLC   BUKGLOTH,SPACES                                                  
         JNH   PUTBUK30                                                         
         CLC   BUKGLMOA,GLMOA                                                   
         JL    PUTBUK30            Skip some detail if prior to GLMOA           
         MVC   DIR.GLBKSCA,BUKGLOTH Move in client code                         
                                                                                
PUTBUK30 DS    0H                                                               
         GOTOR DATAMGR,DMCB,(X'80',DMREAD),ACCDIR,DIR.GLBKEY,          +        
               REC.GLBKEY,IOWRK                                                 
         CLI   8(R1),0             Error ?                                      
         JE    PUTBUK40                                                         
                                                                                
***********************************************************************         
* Add new record                                                                
***********************************************************************         
         TM    8(R1),X'10'         Record not found                             
         BO    *+6                                                              
         DC    H'00'                                                            
                                                                                
         XC    REC.GLBKEY(100),REC.GLBKEY                                       
         MVC   REC.GLBKEY,DIR.GLBKEY                                            
         LA    RF,GLBRFST-GLBRECD                                               
         STH   RF,REC.GLBRLEN                                                   
         GOTOR ADDCAC,REC.GLBRECD                                               
         GOTOR ACADDBUK,DMCB,(X'80',REC.GLBKEY),NEW.BUKEL,HELLO                 
         MVC   REC.GLBRUPDT,TODAY2                                              
         MVC   REC.GLBRUPTM,DAYTIME                                             
         TM    UPSI,UPSIDKEY       Dump key or 28                               
         BZ    CCC                                                              
         LA    R5,GLBRFST-GLBRECD                                               
         LA    RF,REC.GLBRECD                                                   
         GOTOR =V(PRNTBL),DMCB,=C'Key',(RF),C'DUMP',(R5),=C'1D',       X        
               (C'P',V(PRINT))                                                  
CCC      AP    ADD28,=P'1'         Count how many                               
         CLI   RCWRITE,YES                                                      
         BNE   PUTBUK60            Don't actually write                         
         TM    RNSW,RNYES          RERUN=YES OR RERUN=ALL                       
         BO    PUTBUK70                                                         
         GOTOR DATAMGR,DMCB,(X'80',ADDREC),ACCMST,DISK,REC.GLBKEY,IOWRK         
         CLI   8(R1),0             Error ?                                      
         JE    PUTBUK70                                                         
         DC    H'00'                                                            
                                                                                
***********************************************************************         
* Get record and update bucket then putrec                                      
***********************************************************************         
PUTBUK40 DS    0H                                                               
         MVC   DISK,REC.GLBKDA     Get disk address from read                   
         GOTOR DATAMGR,DMCB,(X'80',GETREC),ACCMST,DISK,REC.GLBKEY,IOWRK         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         GOTOR ADDCAC,REC.GLBRECD                                               
         GOTOR ACADDBUK,DMCB,(X'80',REC.GLBKEY),NEW.BUKEL,HELLO                 
         MVC   REC.GLBRUPDT,TODAY2                                              
         MVC   REC.GLBRUPTM,DAYTIME                                             
         AP    CHG28,=P'1'                                                      
                                                                                
         CLI   RCWRITE,YES                                                      
         BNE   PUTBUK60            Don't actually write                         
         TM    RNSW,RNYES          RERUN=YES OR RERUN=ALL                       
         BO    PUTBUK60                                                         
         GOTOR DATAMGR,DMCB,(X'80',PUTREC),ACCMST,DISK,REC.GLBKEY,IOWRK         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
***********************************************************************         
* Write back directory                                                          
***********************************************************************         
                                                                                
PUTBUK60 DS    0H                                                               
         MVC   DIR.GLBKEY,REC.GLBKEY                                            
         MVC   DIR.GLBKSTA,REC.GLBRSTA                                          
         MVC   DIR.GLBKUPDT,TODAY2                                              
         MVC   DIR.GLBKUPTM,DAYTIME                                             
         MVC   DIR.GLBKDA,DISK                                                  
         CLI   RCWRITE,YES                                                      
         BNE   PUTBUK70            Don't actually write                         
         TM    RNSW,RNYES          RERUN=YES OR RERUN=ALL                       
         BO    PUTBUK70                                                         
         GOTOR DATAMGR,DMCB,(X'80',DMWRT),ACCDIR,DIR.GLBKEY,DIR.GLBKEY          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
PUTBUK70 MVI   BUKGLIND,YES        Mark as updated                              
         AHI   R3,BUKRLNQ          Next record                                  
         BRCT  R0,PUTBUK10                                                      
                                                                                
PUTBUKX  J     XIT                                                              
         DROP  NEW,DIR,REC                                                      
         DROP  R3,R7                                                            
         EJECT                                                                  
***********************************************************************         
* Build CACELD element from NAMELD on account record & add to GLBRECD           
***********************************************************************         
         USING ACTRECD,R6                                                       
ADDCAC   NTR1                                                                   
         LR    R4,R1               A(GLBRECD)                                   
         L     R3,ADACCNAM         A(Lowest level name)                         
         TM    LGSW,LGPR           IS THIS PRODUCTION                           
         BZ    *+8                                                              
         L     R3,ADLVBNAM         A(Level 2 name - cli/prod)                   
                                                                                
         USING NAMELD,R3                                                        
         USING CACELD,R5                                                        
         LTR   R3,R3               Any address ?                                
         BZ    ADDCACX                                                          
         CLI   0(R3),NAMELQ        Must be name element                         
         BNE   ADDCACX                                                          
         LA    R5,WORK             Add new contra name element                  
         XC    WORK,WORK                                                        
         MVC   CACCNT,SPACES                                                    
         MVI   CACEL,CACELQ        X'43' contra name                            
         LA    RF,L'ACTKCULA       C/U/L/A                                      
         TM    LGSW,LGPR           IS THIS PRODUCTION                           
         BZ    *+8                                                              
         IC    RF,MDADSP           C/U/L/Client/Product                         
         BCTR  RF,0                                                             
         EXMVC RF,CACCNT,ACTKCULA                                               
                                                                                
ADDCAC20 SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         EXMVC RF,CACNAME,NAMEREC                                               
         AHI   RF,CACLN1Q+1                                                     
         STC   RF,CACLN                                                         
         GOTOR ACADDBUK,DMCB,(X'80',(R4)),WORK,HELLO                            
                                                                                
ADDCACX  J     XIT                                                              
         DROP  R3,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* CREATE POSTING RECORDS FOR LEDGER                                   *         
***********************************************************************         
         SPACE 1                                                                
POST     NTR1  BASE=*,LABEL=*                                                   
         ZAP   POSTDR,PKZERO       CLEAR TOTALS                                 
         ZAP   POSTCR,PKZERO                                                    
         L     RE,ADPSTOTB         CLEAR POSTING TABLE BY OFFICE                
         LA    R0,256              MAXIMUM OF 256 OFFICES                       
*                                                                               
PST01    XC    0(2,RE),0(RE)       CLEAR OFFICE CODE                            
         ZAP   2(8,RE),PKZERO      CLEAR DEBITS                                 
         ZAP   10(8,RE),PKZERO     CLEAR CREDITS                                
         LA    RE,18(RE)           BUMP TO NEXT ENTRY                           
         BCT   R0,PST01                                                         
*                                                                               
         LA    R6,AREA                                                          
         MVC   0(2,R6),=H'136'                                                  
         MVC   2(2,R6),=H'0'                                                    
         LA    R6,4(R6)                                                         
         USING PSHEADD,R6                                                       
         MVC   PSHDEL(2),=X'5046'  SET HEADER ELEMENT                           
         SR    R1,R1                                                            
         IC    R1,PSHDLEN                                                       
         LA    R3,0(R1,R6)                                                      
         USING TRNELD,R3                                                        
         MVC   TRNEL(2),=X'441D'   SET TRANSACTION ELEMENT                      
         SR    R1,R1                                                            
         IC    R1,TRNLN                                                         
         LA    RF,0(R1,R3)                                                      
         USING TRSELD,RF                                                        
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSPMOS,START3      PASS MOA                                     
         SR    R1,R1                                                            
         IC    R1,TRSLN                                                         
         LA    RF,0(R1,RF)                                                      
         MVI   0(RF),0             MARK END OF RECORD                           
         DROP  RF                                                               
*                                                                               
         XC    BFREC,BFREC         CLEAR BUFFALO RECORD                         
         LA    R0,BFPST                                                         
         GOTO1 BUFFALO,DMCB,=C'HIGH',((R0),ADBUF),BFREC,1                       
         B     PST03                                                            
*                                                                               
PST02    LA    R0,BFPST                                                         
         GOTO1 BUFFALO,DMCB,=C'SEQ',((R0),ADBUF),BFREC,1                        
PST03    TM    DMCB+8,X'80'        IS IT EOF, ON BUFFALO                        
         BO    PST10                                                            
         MVC   PSHDACC(1),RCCOMPFL COMPANY                                      
         MVC   PSHDACC+1(14),BFACCT    ACCOUNT                                  
         MVC   PSHDANAL,SPACES                                                  
         MVC   PSHDSBAC,BFNME          NAME AS CONTRA                           
         MVC   PSHDSBNM,SPACES         POST NAME IS SPACES                      
         TM    BFSTAT,PRODLG           FOR PRODUCTION                           
         BO    PST05                                                            
         MVC   PSHDSBAC(1),RCCOMPFL                                             
         MVC   PSHDSBAC+1(14),BFCNTR   CONTRA ACCOUNT                           
         MVC   PSHDSBNM,BFNME          CONTRA NAME                              
         CLI   PSHDSBAC+3,C' '         POSTING BY LEDGER                        
         BNE   PST05                                                            
         L     R4,ADLDGNAM             USE LEDGER NAME                          
         LA    R5,PSHDSBNM                                                      
         BRAS  RE,GETNME                                                        
*                                                                               
PST05    MVC   TRNDATE,ENDATE                                                   
         MVC   TRNREF,TODAY                                                     
         MVI   TRNSUB,X'00'                                                     
         MVI   TRNTYPE,X'19'                                                    
         MVI   TRNSTAT,TRNSDR                                                   
         MVC   TRNBTCH,SPACES                                                   
         MVC   TRNBTCH(2),MOSDT                                                 
         MVC   TRNOFFC,BFOFFC                                                   
         MVI   TRNNARR,X'40'                                                    
         ZAP   POSTAMT,BFDR                                                     
         AP    POSTDR,BFDR         ADD TO TOTAL DEBITS                          
         BAS   RE,CHKPOST          CHECK POSTING AMOUNT                         
         MVI   TRNSTAT,0                                                        
         ZAP   POSTAMT,BFCR                                                     
         AP    POSTCR,BFCR         ADD TO TOTAL CREDITS                         
         BAS   RE,CHKPOST          CHECK POSTING AMOUNT                         
         L     RE,ADPSTOTB         ADD TO OFFICE POSTING TABLE                  
*                                                                               
PST07    CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                MORE THAN 256 OFFICES                        
         OC    0(2,RE),0(RE)       AN EMPTY SLOT?                               
         BZ    PST09               YES                                          
         CLC   TRNOFFC,0(RE)       MATCH ON OFFICE?                             
         BE    PST09                                                            
         LA    RE,18(RE)                                                        
         B     PST07               NO -- KEEP LOOKING                           
*                                                                               
PST09    MVC   0(2,RE),TRNOFFC     OFFICE CODE                                  
         AP    2(8,RE),BFDR        ACCUMULATE DEBITS                            
         AP    10(8,RE),BFCR       ACCUMULATE CREDITS                           
         B     PST02                                                            
*                                                                               
*              CREATE POSTING TO CONTROL LEDGER                                 
*                                                                               
*                                                                               
PST10    LA    R5,LDGTOT           LEDGER TOTALS - FROM REPORT                  
         CP    POSTDR,0(8,R5)                                                   
         BE    *+6                                                              
         DC    H'0'                DEBITS DON'T EQUAL REPORT                    
         CP    POSTCR,8(8,R5)                                                   
         BE    *+6                                                              
         DC    H'0'                CREDITS DON'T EQUAL REPORT                   
         MVC   PSHDACC,SPACES                                                   
         MVC   PSHDACC(1),RCCOMPFL COMPANY                                      
         L     R2,ADLEDGER                                                      
         MVC   PSHDACC+1(1),1(R2)  UNIT                                         
         MVI   PSHDACC+2,C'9'                                                   
         MVC   PSHDACC+3(2),2(R2)  LEDGER                                       
         MVC   PSHDSBAC,SPACES                                                  
         MVC   PSHDSBAC(3),0(R2)                                                
         MVC   PSHDSBNM,SPACES                                                  
*                                                                               
         L     R2,ADPSTOTB         OFFICE POSTING TABLE                         
PST15    MVC   TRNOFFC,0(R2)       OFFICE CODE (MIGHT BE SPACES!)               
         MVI   TRNSTAT,TRNSDR      DEBIT  TO OFFSET CREDITS                     
         ZAP   POSTAMT,10(8,R2)    TOTAL CREDITS                                
         BAS   RE,CHKPOST                                                       
         MVI   TRNSTAT,0           CREDIT TO OFFSET DEBITS                      
         ZAP   POSTAMT,2(8,R2)     AND DEBITS                                   
         BAS   RE,CHKPOST                                                       
         LA    R2,18(R2)           BUMP TO NEXT OFFICE                          
         OC    0(2,R2),0(R2)                                                    
         JZ    XIT                 END OF OFFICES                               
         CLI   0(R2),X'FF'                                                      
         JE    XIT                 END OF TABLE                                 
         B     PST15                                                            
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK THAT POSTING AMOUNT ISN'T TOO LARGE                           *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R3                                                        
CHKPOST  ST    RE,SAVRE                                                         
CHKPOST1 LA    RF,AMTHI                                                         
         CP    POSTAMT,AMTHI       TEST OVER MAXIMUM TRNAMNT                    
         BH    CHKPOST3                                                         
         LA    RF,AMTLO                                                         
         CP    POSTAMT,AMTLO       TEST LARGE NEGATIVE NUMBER                   
         BL    CHKPOST3                                                         
         ZAP   TRNAMNT,POSTAMT                                                  
         BAS   RE,ADDPOST          OK, TO ADD POSTING                           
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                                                               
CHKPOST3 ZAP   TRNAMNT,0(L'AMTHI,RF)                                            
         BAS   RE,ADDPOST          POST THE MAX                                 
         SP    POSTAMT,0(L'AMTHI,RF) ADJUST REMAINDER                           
         B     CHKPOST1                                                         
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ADD THE POSTING RECORD TO THE WORKER FILE                           *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R3                                                        
ADDPOST  CP    TRNAMNT,PKZERO                                                   
         BER   RE                                                               
         TM    RQSW,RQDF           DRAFT                                        
         BOR   RE                                                               
         CLI   RCPOSTNG,NO                                                      
         BER   RE                                                               
*                                                                               
         NTR1  ,                                                                
         OC    ID,ID               TEST ID SET                                  
         BNZ   *+8                                                              
         BAS   RE,OPNPOST                                                       
*                                                                               
         L     R4,ADPBUF                                                        
         GOTO1 WORKER,DMCB,=C'ADD',(R4),ID,AREA                                 
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AP    TOTCNT,=P'1'                                                     
         LA    R4,DDSDR                                                         
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+14                                                             
         AP    TOTCSH,TRNAMNT                                                   
         LA    R4,DDSCR                                                         
         AP    0(8,R4),TRNAMNT                                                  
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* OPEN/CLOSE WORKER FILE                                              *         
***********************************************************************         
         SPACE 1                                                                
OPNPOST  NTR1  ,                                                                
         MVC   ID(2),ORIGINUM                                                   
         MVC   ID+2(3),=C'A25'                                                  
         CLI   SYSEQU,ACMSUSA                                                   
         BE    OPNPOST1                                                         
         PACK  DUB(2),RCDATE(3)    GET DAY (UK)                                 
         B     OPNPOST2                                                         
*                                                                               
OPNPOST1 PACK  DUB(2),RCDATE+3(3)  GET DAY (US)                                 
*                                                                               
OPNPOST2 MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'P'                                                        
         OI    ID+13,X'01'         ALLOW DUPLICATE WORKER FILES                 
         L     R4,ADPBUF                                                        
         TM    RNSW,RNYES          TEST RERUN                                   
         BZ    *+8                                                              
         BAS   RE,CHKD             CHECK FOR A DUPLICATE FILE                   
         GOTO1 WORKER,DMCB,=C'OPEN',(R4),ID,AREA                                
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK FOR A DUPLICATE WORKER FILES                                  *         
***********************************************************************         
         SPACE 1                                                                
CHKD     NTR1  ,                                                                
         MVC   IDX,ID                                                           
CHKD1    GOTO1 WORKER,DMCB,=C'INDEX',(R4),IDX,AREA                              
         TM    8(R1),X'80'         TEST EOF                                     
         JO    XIT                                                              
         CLC   IDX(UKFILNO-UKKEY),ID  TEST SAME FILE ID                         
         BNE   CHKD1                                                            
                                                                                
         USING UKRECD,RF                                                        
         LA    RF,IDX                                                           
         TM    UKSTAT,X'08'        TEST FILE ON KEEP                            
         BO    CHKD1                                                            
         DC    H'0'                DUPLICATE POSTINGS                           
         J     XIT                                                              
         DROP  RF                                                               
*                                                                               
AMTHI    DC    PL(L'TRNAMNT)'99999999999'                                       
AMTLO    DC    PL(L'TRNAMNT)'-99999999999'                                      
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT HEADLINES                                                     *         
***********************************************************************         
         SPACE 1                                                                
PRN      NTR1  BASE=*,LABEL=*                                                   
         CLI   RCSUBPRG,3                                                       
         JE    PRN12                                                            
         MVC   HEAD5+82(L'AC@MTH),AC@MTH     FOR THE MONTH                      
         MVC   HEAD5+89(6),HDATE                                                
         TM    RQSW,RQREV                                                       
         JNO   *+10                                                             
         MVC   HEAD6+82(L'AC@RVRSL),AC@RVRSL  REVERSAL                          
         TM    RQSW,RQDF                                                        
         JNO   *+10                                                             
         MVC   HEAD5+53(L'AC@DRAFT),AC@DRAFT  DRAFT                             
         CLI   RCSUBPRG,8          ERROR SUMMARY                                
         JNE   PRN03                                                            
         MVC   HEAD5+1(L'AC@DEFOS),AC@DEFOS   DEFAULT OFFICE                    
         MVC   HEAD8+58(L'AC@OFF),AC@OFF      OFFICE                            
         MVC   HEAD9+58(L'AC$OFF),AC$OFF      ------                            
         J     PRN12                                                            
*                                                                               
PRN03    CLI   RCSUBPRG,1                                                       
         JH    PRN10                                                            
*&&UK*&& TM    RNSW,RNOF+RNTOF     POST TO G/L BY OFFICE                        
*&&UK*&& JZ    PRN12                                                            
         MVC   HEAD8+57(L'AC@OFF),AC@OFF      OFFICE                            
         MVC   HEAD9+57(L'AC$OFF),AC$OFF      ------                            
         J     PRN12                                                            
*                                                                               
PRN10    CLI   RCSUBPRG,2          LEDGER SUMMARY?                              
         JE    *+12                                                             
         CLI   RCSUBPRG,7          OVERALL SUMMARY?                             
         JNE   PRN12                                                            
*&&UK*&& TM    RNSW,RNOF           POST TO G/L BY OFFICE                        
*&&UK*&& JZ    PRN12                                                            
         MVC   HEAD8+57(L'AC@OFF),AC@OFF      OFFICE                            
         MVC   HEAD9+57(L'AC$OFF),AC$OFF      ------                            
*                                                                               
PRN12    CLI   FORCEHED,YES                                                     
         JE    PRN15                                                            
         TM    PRNSW,PRNSKIP       TEST LAST A SKIP                             
         JNO   PRN15                                                            
         CLC   P,SPACES            TEST ANOTHER SKIP                            
         JE    XIT                                                              
*                                                                               
PRN15    NI    PRNSW,ALL-PRNSKIP                                                
         CLC   P,SPACES                                                         
         JNE   *+8                                                              
         OI    PRNSW,PRNSKIP       SET LAST SKIP                                
         GOTO1 ACREPORT                                                         
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CLOSE WORKER FILE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING PSSUBFD,R6                                                       
CLSPOST  NTR1  BASE=*,LABEL=*                                                   
         CLI   RCPOSTNG,NO                                                      
         JE    XIT                                                              
         OC    ID,ID               TEST ANY POSTING ID                          
         JZ    XIT                                                              
                                                                                
         LA    R6,AREA                                                          
         MVC   0(2,R6),=H'34'                                                   
         MVC   2(2,R6),=H'0'                                                    
         LA    R6,4(R6)                                                         
         MVI   PSSBEL,X'52'                                                     
         MVI   PSSBLEN,X'1D'                                                    
         MVC   PSSBDESC,=CL15'G/L UPDATE'                                       
         ZAP   PSSBRECS,TOTCNT                                                  
         ZAP   PSSBCASH,TOTCSH                                                  
         MVI   PSSBCASH+6,0                                                     
         L     R4,ADPBUF                                                        
         GOTO1 WORKER,DMCB,=C'ADD',(R4),ID,AREA                                 
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 (RF),(R1),=C'CLOSE',(R4)                                         
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         DROP  R6,RB                                                            
         EJECT                                                                  
***********************************************************************         
* DICTIONARY LIST                                                               
***********************************************************************         
DCLIST   DS    0C                                                               
         DCDDL AC#CNTPT,20,L                                                    
         DCDDL AC#DRAFT,7,L                                                     
         DCDDL AC#MTH,6                                                         
         DCDDL AC#OFF,9,L                                                       
         DCDDL AC#OFF,9,LU                                                      
         DCDDL AC#PRVUP,17,L                                                    
         DCDDL AC#REQ,11,L                                                      
         DCDDL AC#RVRSL,13,L                                                    
         DCDDL AC#TAREQ,25,L                                                    
         DCDDL AC#TOTFR,10                                                      
         DCDDL AC#TOTLS,6                                                       
         DCDDL AC#TOTAL,5                                                       
         DCDDL AC#TREQ,21                                                       
         DCDDL AC#TUPDF,17,L                                                    
         DCDDL AC#UATHT,19,L                                                    
         DCDDL AC#OFFTS,20,L                                                    
         DCDDL AC#ACC,7,L                                                       
         DCDDL AC#LGR,7,L                                                       
         DCDDL AC#NIL,5,L                                                       
         DCDDL AC#RECNF,25,L                                                    
         DCDDL AC#LGRMR,70,L                                                    
         DCDDL AC#FOLLG,70,L                                                    
         DCDDL AC#DEFOS,70,L                                                    
DCLISTX  DS    X'00'                                                            
         EJECT                                                                  
TRIBAL   DS    0D                                                               
         NMOD1 0,*TRIBAL*,R9                                                    
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
         EJECT                                                                  
*              REQFRST                                                          
*                                                                               
TRQF     CLI   MODE,REQFRST                                                     
         BNE   TUNF                                                             
         MVI   FCRDMOSP,NO                                                      
*                                                                               
TRQF03   TM    RNSW,RNTRC         TRACE TB                                      
         BNO   *+8                                                              
         MVI   RCTRACE,YES                                                      
         MVI   FCRDACC,YES                                                      
         MVI   FCRDTRNS,YES                                                     
         ZAP   RCENNUM,PKZERO                                                   
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAY3)                                
         GOTO1 PROLLER,DMCB,0,(8,ACCUMS),8,5                                    
         ZAP   MYOBAL,PKZERO       CLEAR OFFICE BUCKETS FOR OLD FILE            
         ZAP   ODCASH,PKZERO                                                    
         ZAP   OCCASH,PKZERO                                                    
         ZAP   OBCASH,PKZERO                                                    
         ZAP   BUFFJUNK,=P'1'      SO BUFFALO RETURNS ALL RECORDS               
*                                                                               
         L     RF,ADBUF            RESET BUFFALO FOR OFFICE TOTAL RECS          
         USING BUFFALOD,RF                                                      
         MVC   BUFFROWS,=F'1'      NEW NUMBER OF ROWS                           
         MVC   BUFFCOLS,=F'5'      NEW NUMBER OF COLUMNS                        
         MVC   BUFFLCOM,=F'1'      NEW LENGTH OF COMMENT                        
         MVI   BUFFLIST,BUFFKEYQ   NEW KEY LENGTH                               
         GOTO1 BUFFALO,DMCB,=C'RESET',ADBUF                                     
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVC   HEADMON,SPACES                                                   
         MVC   HEADMON(L'AC@MTH),AC@MTH                                         
         MVC   WORK(4),SAVEDATE                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(6,HEADMON+9)                               
         GOTO1 (RF),(R1),,(1,SVDATE)                                            
         LA    R2,REQGEN                                                        
         USING LEVGEND,R2                                                       
         XC    LEVGEN,LEVGEN                                                    
         MVI   LEVLET,C'R'                                                      
         MVI   LEVNUM,1                                                         
         MVI   LEVACMNO,8                                                       
         MVC   LEVKEY(L'LEVKEY+L'LEVDESC+L'LEVNAME),SPACES                      
         MVC   LEVNAME(L'AC@REQ),AC@REQ  REQUEST                                
         B     TBXIT                                                            
         DROP  R2,RF                                                            
         EJECT                                                                  
*****************************************                                       
*              UNITFRST                                                         
*****************************************                                       
TUNF     CLI   MODE,UNITFRST                                                    
         BNE   TLGF                                                             
         USING LEVGEND,R2                                                       
         LA    R2,UNITGEN                                                       
         XC    LEVGEN,LEVGEN                                                    
         MVI   LEVLET,C'U'                                                      
         MVI   LEVNUM,1                                                         
         MVI   LEVACMNO,7                                                       
         MVC   LEVKEY(L'LEVKEY+L'LEVDESC+L'LEVNAME),SPACES                      
                                                                                
         USING NAMELD,R4                                                        
         L     R4,ADUNTNAM                                                      
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMEREC-NAMELD+1                                              
         EXMVC R1,LEVNAME,NAMEREC                                               
         B     TBXIT                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
*************************************************                               
*              LEDGFRST                                                         
*************************************************                               
TLGF     CLI   MODE,LEDGFRST                                                    
         BNE   TOFF                                                             
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUF,(X'80',1)                           
         XC    SVBUFKEY,SVBUFKEY   NO BUFFALO GETS YET                          
         MVI   FRSTBUFF,C'Y'                                                    
         XC    MYWORK,MYWORK       BUILD END OF LEDGER RECORD IN MYWORK         
         L     RF,ADLEDGER         CURRENT LEDGER                               
         MVC   MYWORK(1),LDGKLDG-LDGRECD(RF)                                    
         MVC   MYWORK+1(SMKLNQ-1),XFF REMAINDER IS X'FF'                        
*                                                                               
TLGF10   GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   R4,15,4(R1)         THERE'S ALWAYS AT LEAST ONE RECORD           
         BNZ   *+6                                                              
         DC    H'0'                FOR LEDGERS B AND P                          
         MVC   SMREC,0(R4)                                                      
         CLC   SMREC(SMKLNQ),MYWORK IS THIS THE END OF THE LEDGER?              
         BE    TLGF15              YES                                          
         MVC   OFFCODE,SMOFFCO                                                  
         CLI   QSEQ,QSEQOFF        TEST OFFICE SEQUENCE                         
         BE    *+10                                                             
         MVC   OFFCODE,SMOFFC                                                   
         L     RF,AMONACC                                                       
         TM    ACMINDS-ACMD(RF),ACMINEWO                                        
         BNO   TLGF12                                                           
         BAS   RE,OFCFLT            FILTER OFFICE - LIMITED ACCESS              
         BNE   TLGF10                                                           
         XC    BUFFREC(BUFFRECQ),BUFFREC                                        
         CLI   QSEQ,QSEQOFF        IN OFFICE SEQUENCE, OFFICE IS FIRST          
         BNE   TLGF12                                                           
         MVC   BUFFOFCO,SMOFFCO    OFFICE CODE                                  
         MVC   BUFFACTO,SMACCTO    ACCOUNT                                      
         B     TLGF13                                                           
*                                                                               
TLGF12   MVC   BUFFACCT,SMACCT     ACCOUNT                                      
         MVC   BUFFOFFC,SMOFFC     OFFICE CODE                                  
*                                                                               
TLGF13   ZAP   BUFFOB,PKZERO       NO BALANCE FORWARD                           
         MVC   BUFFOD,SMDR         DEBITS                                       
         MVC   BUFFOC,SMCR         CREDITS                                      
         ZAP   BUFFMY,BUFFOD                                                    
         SP    BUFFMY,BUFFOC       MANUAL BALANCE                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BUFFREC                               
         CLI   QSEQ,QSEQOFF                                                     
         BNE   TLGF10                                                           
         MVI   BUFFLEVL,X'FE'      OFFICE TOTALS FOR WHOLE LEDGER               
         XC    BUFFACTO,BUFFACTO   ACROSS ALL ACCOUNTS FOR AN OFFICE            
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BUFFREC                               
         MVI   BUFFLEVL,X'FF'      LEDGER-WIDE TOTAL ACROSS OFFICES             
         XC    BUFFOFCO,BUFFOFCO                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BUFFREC                               
         B     TLGF10                                                           
*                                                                               
TLGF15   CLI   QSEQ,QSEQOFF        FOR OFFICE SEQUENCE,                         
         BNE   TLGF25                                                           
         L     RF,AMONACC                                                       
         TM    ACMINDS-ACMD(RF),ACMINEWO                                        
         BNO   TLGF25                                                           
         XC    WORK,WORK           READ ALL OFFICE/ACCOUNT KEYS                 
         LA    R2,WORK                                                          
         USING OAPRECD,R2                                                       
         MVI   OAPKTYP,OAPKTYPQ    OFFICE/ACCOUNT POINTER TYPE                  
         MVC   OAPKCPY,RCCOMPFL    COMPANY                                      
         MVI   OAPKUNT,C'G'        UNIT G                                       
         L     RF,ADLEDGER         CURRENT LEDGER                               
         MVC   OAPKLDG,LDGKLDG-LDGRECD(RF)                                      
         MVC   FULL,WORK           SAVE FIRST PORTION OF KEY                    
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,WORK,WORK                             
*                                                                               
TLGF20   CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   FULL,WORK           SAME TYPE/COMPANY/UNIT/LEDGER?               
         BNE   TLGF25              NO - WE'RE DONE                              
         MVC   OFFCODE,OAPKOFF                                                  
         BAS   RE,OFCFLT           FILTER LIMIT ACCESS                          
         BNE   TLGF23                                                           
*                                                                               
TLGF22   XC    BUFFREC(BUFFRECQ),BUFFREC                                        
         MVC   BUFFOFCO,OAPKOFF    OFFICE CODE                                  
         MVC   BUFFACTO(2),FULL+2  UNIT/LEDGER                                  
         MVC   BUFFACTO+2(12),OAPKACT  ACCOUNT                                  
         ZAP   BUFFOB,PKZERO                                                    
         ZAP   BUFFOD,PKZERO                                                    
         ZAP   BUFFOC,PKZERO                                                    
         ZAP   BUFFMY,PKZERO                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BUFFREC                               
         MVI   BUFFLEVL,X'FE'      OFFICE TOTALS FOR WHOLE LEDGER               
         XC    BUFFACTO,BUFFACTO   ACROSS ALL ACCOUNTS FOR AN OFFICE            
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BUFFREC                               
         MVI   BUFFLEVL,X'FF'      LEDGER-WIDE TOTAL ACROSS OFFICES             
         XC    BUFFOFCO,BUFFOFCO                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BUFFREC                               
*                                                                               
TLGF23   GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,WORK,WORK                             
         B     TLGF20                                                           
         DROP  R2                                                               
*                                                                               
TLGF25   MVI   RCSUBPRG,4                                                       
         CLI   PROGPROF+2,C'N'                                                  
         BE    TLGF30                                                           
         MVI   RCSUBPRG,5                                                       
         MVC   WORK(4),SAVEDATE                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 ADDAY,DMCB,WORK,WORK,F'-1'                                       
         GOTO1 DATCON,DMCB,(0,WORK),(6,HEADMONF)                                
*                                                                               
TLGF30   XC    LEVACONT(4*L'LEVACONT),LEVACONT                                  
         MVI   SPACESW,0           FORCE AT LEAST ONE TOTAL LINE.               
         MVI   ACTIVE,NO                                                        
         MVC   LASTFIGS,SPACES                                                  
         MVI   LASTSPAC,2          SO I DON'T SPACE AT BEGINNING                
         MVI   FORCEHED,YES                                                     
                                                                                
         USING LEVGEND,R2                                                       
         LA    R2,LEDGEN                                                        
         XC    LEVGEN,LEVGEN                                                    
         MVI   LEVLET,C'L'                                                      
         MVI   LEVNUM,1                                                         
         MVI   LEVACMNO,6                                                       
         MVC   LEVKEY(L'LEVKEY+L'LEVDESC+L'LEVNAME),SPACES                      
                                                                                
         USING NAMELD,R4                                                        
         L     R4,ADLDGNAM                                                      
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMEREC-NAMELD+1                                              
         EXMVC R1,LEVNAME,NAMEREC                                               
         BAS   RE,STR                                                           
         ZAP   BCASH,PKZERO                                                     
         ZAP   DCASH,PKZERO                                                     
         ZAP   CCASH,PKZERO                                                     
         MVI   BUFSW,0                                                          
         B     TBXIT                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************                                             
*              OFFIRST                                                          
***********************************                                             
TOFF     CLI   MODE,OFFIRST                                                     
         BNE   TLFA                                                             
         CLI   QSEQ,QSEQOFF                                                     
         BNE   TBXIT                                                            
         ZAP   BCASH,PKZERO                                                     
         ZAP   DCASH,PKZERO                                                     
         ZAP   CCASH,PKZERO                                                     
         BAS   RE,SETOFC           SET THE OFFICE                               
         BAS   RE,BUFOF            CHECK FOR LOWER OFFICES IN BUFFALO           
         MVI   FORCEHED,YES        BREAK A PAGE FOR NEW OFFICE                  
         B     TBXIT                                                            
         EJECT                                                                  
**************************************                                          
*              LEVAFRST ETC.                                                    
**************************************                                          
TLFA     CLI   MODE,LEVAFRST                                                    
         BNE   TLFB                                                             
         USING LEVGEND,R2                                                       
         LA    R2,LEVAGEN                                                       
         L     R4,ADLVANAM                                                      
         L     R6,ADHEIRA                                                       
         MVI   FIRSTLOW,YES                                                     
         B     TLFX                                                             
*                                                                               
TLFB     CLI   MODE,LEVBFRST                                                    
         BNE   TLFC                                                             
         LA    R2,LEVBGEN                                                       
         L     R4,ADLVBNAM                                                      
         L     R6,ADHEIRB                                                       
         B     TLFX                                                             
*                                                                               
TLFC     CLI   MODE,LEVCFRST                                                    
         BNE   TPAC                                                             
         LA    R2,LEVCGEN                                                       
         L     R4,ADLVCNAM                                                      
         L     R6,ADHEIRC                                                       
*                                                                               
TLFX     CLI   QSEQ,QSEQOFF        TEST OFFICE SEQUENCE                         
         BNE   TLFX7                                                            
*                                                                               
TLFX3    BAS   RE,NXTBUF           GET NEXT BUFFALO                             
         TM    BUFSW,BUFEOF                                                     
         BO    TLFX7                                                            
         CLI   BUFFLEVL,0                                                       
         BNE   TLFX7               TEST EOF                                     
         CLC   BUFFOFCO,OFFCODE                                                 
         BNE   TLFX7                                                            
         SR    R1,R1                                                            
         IC    R1,LEVTLEN          TEST FOR LOWER ACCOUNT                       
         AHI   R1,1                                                             
         EXCLC R1,BUFFACTO,1(R6)                                                
         BNL   TLFX7                                                            
         BAS   RE,FRSTL            PROCESS LOWER LEVEL A'S IN BUFFAL            
*                                                                               
TLFX7    BAS   RE,LUP                                                           
         B     TBXIT                                                            
         DROP  R2                                                               
         EJECT                                                                  
****************************************                                        
*              PROCACC                                                          
****************************************                                        
TPAC     CLI   MODE,PROCACC                                                     
         BNE   TOAF                                                             
         MVI   ACTIVE,YES          SWITCH DENOTES PRESENCE OF LOW LEVEL         
         BAS   RE,SETACC           SET R2 TO ACCOUNT LEVEL                      
         USING LEVGEND,R2                                                       
*                                                                               
         L     R4,ADACCNAM                                                      
         L     R6,ADACC                                                         
         BAS   RE,LUP                                                           
         CLI   QSEQ,QSEQOFF        TEST OFFICE SEQUENCE                         
         BE    TPAC07              YES                                          
         XC    BUFFREC(BUFFRECQ),BUFFREC                                        
         MVC   BUFFACCT,ACTKULA-ACTRECD(R6) UNIT/LEDGER/ACCOUNT                 
         MVC   BUFFOFFC,XFF        FORCE HIGH OFFICE CODE                       
         ZAP   BUFFOB,PKZERO                                                    
         ZAP   BUFFOD,PKZERO                                                    
         ZAP   BUFFOC,PKZERO                                                    
         ZAP   BUFFMY,PKZERO                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BUFFREC                               
*                                                                               
         L     RF,AMONACC                                                       
         TM    ACMINDS-ACMD(RF),ACMIEMUD+ACMINEWO                               
         BNO   TPAC07                                                           
         CLI   QSEQ,C'D'           SHOW OFFICE DETAIL?                          
         BNE   *+8                 NO                                           
         BAS   RE,TPRN             SKIP A LINE                                  
         LA    R4,P                FOR NEW FILE-SHOW ACCT NAME FIRST            
         USING PLINED,R4                                                        
         MVC   PLKEY,LEVKEY                                                     
         MVC   PLDESC,LEVNAME                                                   
         CLI   QSEQ,C'D'           SHOW OFFICE DETAIL?                          
         BNE   TPAC07              NO -- DON'T PRINT YET                        
         BAS   RE,TPRN                                                          
*                                                                               
TPAC07   L     R4,ADACCBAL                                                      
         LTR   R4,R4                                                            
         BZ    TBXIT                                                            
         L     R6,ADACC                                                         
         AH    R6,DATADISP                                                      
         SR    R1,R1                                                            
*                                                                               
TPAC09   CLI   0(R6),0                                                          
         BE    TPAC13                                                           
         CLI   0(R6),APOELQ                                                     
         BE    TPAC11                                                           
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         LTR   R1,R1                                                            
         BNZ   TPAC09                                                           
         DC    H'0',C'ZERO LENGTH ELEMENT'                                      
*                                                                               
         USING APOELD,R6                                                        
         USING ABLELD,R4                                                        
TPAC11   CLC   TODAY3,APOPLDT     FIX UP BALANCE ELEMENT IF WE PEELED           
         BNE   TPAC13             TODAY                                         
         SP    ABLFRWD,APODR                                                    
         AP    ABLFRWD,APOCR                                                    
         AP    ABLDR,APODR                                                      
         AP    ABLCR,APOCR                                                      
*                                                                               
TPAC13   EQU   *                                                                
         ZAP   BCASH,PKZERO                                                     
         ZAP   DCASH,PKZERO                                                     
         ZAP   CCASH,PKZERO                                                     
         B     TBXIT                                                            
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
*              OFFICE ACCOUNT FIRST                                             
*                                                                               
TOAF     CLI   MODE,OFACFRST                                                    
         BNE   TPTN                                                             
         CLI   QSEQ,C'D'                                                        
         BNE   TBXIT                                                            
         ZAP   MYOBAL,PKZERO       CLEAR OFFICE BUCKETS                         
         ZAP   ODCASH,PKZERO                                                    
         ZAP   OCCASH,PKZERO                                                    
         ZAP   OBCASH,PKZERO                                                    
         BAS   RE,SETOFC                                                        
         B     TBXIT                                                            
         EJECT                                                                  
*              PROCTRNS                                                         
*                                                                               
TPTN     CLI   MODE,PROCTRNS                                                    
         BNE   TACL                                                             
         L     R4,ADTRANS                                                       
         USING TRNELD,R4                                                        
         CLI   TRNEL,TRNELQ                                                     
         BNE   TBXIT                                                            
         LR    R6,R4                                                            
         SH    R6,DATADISP                                                      
         USING TRNRECD,R6                                                       
         OC    TRNRECD+ACCOPEEL(2),TRNRECD+ACCOPEEL                             
         BZ    TPTN03                                                           
         CLC   TRNRECD+ACCOPEEL(2),TODAY2 IGNORE PREVIOUSLY PEELED              
         BL    TBXIT                                                            
*                                                                               
TPTN03   DS    0H                                                               
         XC    BUFFREC(BUFFRECQ),BUFFREC                                        
         ZAP   BUFFOB,PKZERO                                                    
         ZAP   BUFFOD,PKZERO                                                    
         ZAP   BUFFOC,PKZERO                                                    
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         CLI   ACMATYP,ACMATPL    IS IT P&L TYPE (I.E.  GP)                     
         BNE   *+14                                                             
         CLC   ACMMDTE,ACMFDTE    SKIP TRANSACTIONS BEFORE                      
         BL    TBXIT              FISCAL YEAR                                   
         CLC   ACMMDTE,SVDATE     OR IF MOS AFTER QSTART                        
         BH    TBXIT              SKIP                                          
         BE    TPTN05             IF EQUAL PUT IN CURRENT                       
         CLC   ACMMDTE,ACMFDTE    FOR GB IF BEFORE START                        
         BL    *+12               ADD TO BALANCE FORWARD                        
         CLI   PROGPROF+2,C'N'    SEPARATE THIS MONTH'S ACTIVITY                
         BE    TPTN05                                                           
         TM    TRNSTAT,TRNSDR     TRNAMNT TO LAST BALANCE                       
         BO    TPTN04                                                           
         SP    BCASH,TRNAMNT      CREDIT                                        
         SP    OBCASH,TRNAMNT                                                   
         SP    BUFFOB,TRNAMNT                                                   
         B     TPTN20                                                           
         DROP  RF                                                               
*                                                                               
TPTN04   AP    BCASH,TRNAMNT      DEBIT                                         
         AP    OBCASH,TRNAMNT                                                   
         AP    BUFFOB,TRNAMNT                                                   
         B     TPTN20                                                           
*                                                                               
TPTN05   TM    TRNSTAT,TRNSDR                                                   
         BO    TPTN09                                                           
         AP    CCASH,TRNAMNT      CREDIT                                        
         AP    OCCASH,TRNAMNT                                                   
         AP    BUFFOC,TRNAMNT                                                   
         B     TPTN20                                                           
*                                                                               
TPTN09   AP    DCASH,TRNAMNT      DEBIT                                         
         AP    ODCASH,TRNAMNT                                                   
         AP    BUFFOD,TRNAMNT                                                   
*                                                                               
TPTN20   CLI   QSEQ,QSEQOFF                                                     
         BNE   TBXIT                                                            
         MVC   BUFFOFCO,OFFCODE    OFFICE CODE                                  
         ZAP   BUFFMY,BUFFOB                                                    
         AP    BUFFMY,BUFFOD                                                    
         SP    BUFFMY,BUFFOC       MANUAL BALANCE                               
         MVI   BUFFLEVL,X'FE'      OFFICE TOTALS FOR WHOLE LEDGER               
         XC    BUFFACTO,BUFFACTO   ACROSS ALL ACCOUNTS FOR AN OFFICE            
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BUFFREC                               
         MVI   BUFFLEVL,X'FF'      LEDGER-WIDE TOTAL ACROSS OFFICES             
         XC    BUFFOFCO,BUFFOFCO                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BUFFREC                               
         B     TBXIT                                                            
         DROP  R4,R6                                                            
         EJECT                                                                  
*              ACCLAST                                                          
*                                                                               
TACL     CLI   MODE,ACCLAST                                                     
         BNE   TOAL                                                             
         CLI   QSEQ,QSEQOFF        TEST OFFICE SEQUENCE                         
         BE    TBXIT                                                            
         CLI   FCSEQ,FCSEQACC      TEST ACCOUNT SEQUENCE                        
         BNE   TACL2                                                            
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         AP    BCASH,ACMABAL                                                    
         B     TACL3                                                            
TACL2    ZAP   BCASH,PKZERO        DETAIL SEQUENCE                              
         ZAP   DCASH,PKZERO                                                     
         ZAP   CCASH,PKZERO                                                     
         DROP  RF                                                               
*                                                                               
TACL3    MVC   BUFFREC(BUFFKEYQ),SVBUFKEY    LAST BUFFALO RECORD READ           
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUF,BUFFREC,1                            
         TM    DMCB+8,X'80'                                                     
         BZ    *+6                                                              
         DC    H'0'                GOT EOF -- IMPOSSIBLE                        
         CLI   FRSTBUFF,C'Y'       FIRST TIME FOR THIS LEDGER?                  
         BE    TACL5               YES                                          
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUF,BUFFREC,1                             
         TM    DMCB+8,X'80'                                                     
         BZ    *+6                                                              
         DC    H'0'                GOT EOF -- IMPOSSIBLE                        
*                                                                               
TACL5    MVC   SVBUFKEY,BUFFREC    SAVE WHERE WE ARE                            
         MVI   FRSTBUFF,C'N'       WE'VE BEGUN READING BUFFALO RECORDS          
         L     R2,ADACC                                                         
         CLC   BUFFACCT,1(R2)      COMPARE BUFFALO RECORD TO FILE               
         BE    *+10                MATCH                                        
         BL    TACL13              ACCOUNT IS NOT ON THE FILE                   
         DC    H'0'                CANNOT BE HIGHER                             
         CLC   BUFFOFFC,XFF        IS THIS THE END-OF-ACCOUNT RECORD?           
         BE    TACL17              YES -- WE'RE DONE FOR THIS ACCOUNT           
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         TM    ACMINDS,ACMINEWO    TWO BYTE OFFICES IN USE?                     
         BO    TACL7                                                            
         AP    DCASH,BUFFOD                                                     
         AP    CCASH,BUFFOC                                                     
         B     TACL3                                                            
*                                                                               
TACL7    AP    BCASH,BUFFOB        ADD ACCOUNT TOTALS                           
         AP    DCASH,BUFFOD                                                     
         AP    CCASH,BUFFOC                                                     
         CLI   QSEQ,C'D'                                                        
         BNE   TACL3                                                            
         LA    R4,P                PRINT TOTALS FOR THIS OFFICE                 
         USING PLINED,R4                                                        
         LA    R3,BUFFACCS                                                      
         BAS   RE,EDT                                                           
*                                                                               
         MVC   OFFCODE,BUFFOFFC                                                 
         BAS   RE,GETOFN           GET OFFICE NAME                              
         MVC   PLOFFCD,BUFFOFFC    OFFICE CODE                                  
         LA    R0,L'OFFNAME        AND NAME                                     
         GOTO1 CHOPPER,DMCB,((R0),OFFNAME),(27,PLOFFNM),(C'P',2)                
         BAS   RE,TPRN                                                          
*                                                                               
         MVI   BUFFLEVL,X'FE'      OFFICE TOTALS FOR WHOLE LEDGER               
         XC    BUFFACCT,BUFFACCT   TOTALS ARE ACROSS ACCOUNTS                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BUFFREC                               
         MVI   BUFFLEVL,X'FF'      LEDGER-WIDE TOTAL ACROSS OFFICES             
         XC    BUFFOFFC,BUFFOFFC                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BUFFREC                               
         B     TACL3                                                            
*                                                                               
TACL13   MVC   P+1(14),BUFFACCT    MISSING AN ACCOUNT SOMEHOW                   
         MVC   P+16(2),=C'**'                                                   
         MVC   P+19(L'AC@RECNF),AC@RECNF                                        
         LA    R2,BUFFOD                                                        
         LA    R5,P+67                                                          
         LA    R3,2                                                             
*                                                                               
TACL15   CURED (P8,0(R2)),(14,0(R5)),2,MINUS=YES                                
         CLI   P+80,C'-'                                                        
         BNE   *+10                                                             
         MVC   P+80(2),=C'CR'                                                   
         LA    R2,8(R2)                                                         
         LA    R5,14(R5)                                                        
         BCT   R3,TACL15                                                        
*                                                                               
         MVI   SPACING,2                                                        
         BAS   RE,TPRN                                                          
         B     TACL3                                                            
*                                                                               
TACL17   BAS   RE,ROLLUP                                                        
         B     TBXIT                                                            
         DROP  R4,RF                                                            
         EJECT                                                                  
*              OFFICE ACCOUNT LAST                                              
*                                                                               
TOAL     CLI   MODE,OFACLAST                                                    
         BNE   TOFL                                                             
         CLI   QSEQ,QSEQOFF                                                     
         BNE   TOAL9                                                            
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         AP    OBCASH,ACMOBAL                                                   
         AP    BCASH,ACMOBAL                                                    
*                                                                               
         L     R6,ADACC                                                         
         BAS   RE,SETACC                                                        
         MVC   CURBUFF,BUFFREC                                                  
         USING LEVGEND,R2                                                       
TOAL3    BAS   RE,NXTBUF                                                        
         TM    BUFSW,BUFEOF                                                     
         BO    TOAL7                                                            
         CLI   BUFFLEVL,0                                                       
         BNE   TOAL7                                                            
         CLC   BUFFOFCO,OFFCODE                                                 
         BNE   TOAL7                                                            
         CLC   BUFFACTO,1(R6)                                                   
         BE    TOAL5                                                            
         BH    TOAL7                                                            
         BAS   RE,NEWACC           PROCESS LOWER LEVEL ACCOUNTS                 
         MVC   BUFFREC(BUFFRECQ),CURBUFF RESTORE THIS ACCT DR/CR                
         MVI   ACTIVE,YES          SWITCH DENOTES PRESENCE OF LOW LEVEL         
         L     R4,ADACCNAM                                                      
         BAS   RE,LUP                                                           
         B     TOAL3                                                            
*                                                                               
TOAL5    MVC   SVBUFKEY,BUFFREC    SAVE LAST KEY PROCESSED                      
         AP    DCASH,BUFFOD                                                     
         AP    CCASH,BUFFOC                                                     
         BAS   RE,ROLLUP                                                        
         ZAP   ODCASH,PKZERO                                                    
         ZAP   OCCASH,PKZERO                                                    
         ZAP   OBCASH,PKZERO                                                    
         MVC   BUFFOFCO,OFFCODE    OFFICE CODE                                  
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         ZAP   BUFFOB,ACMOBAL                                                   
         ZAP   BUFFMY,ACMOBAL                                                   
         ZAP   BUFFOD,PKZERO                                                    
         ZAP   BUFFOC,PKZERO                                                    
         MVI   BUFFLEVL,X'FE'      OFFICE TOTALS FOR WHOLE LEDGER               
         XC    BUFFACTO,BUFFACTO   ACROSS ALL ACCOUNTS FOR AN OFFICE            
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BUFFREC                               
         MVI   BUFFLEVL,X'FF'      LEDGER-WIDE TOTAL ACROSS OFFICES             
         XC    BUFFOFCO,BUFFOFCO                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BUFFREC                               
         B     TBXIT                                                            
*                                                                               
TOAL7    MVC   BUFFREC(BUFFRECQ),CURBUFF RESTORE THIS ACCT DR/CR                
         BAS   RE,ROLLUP                                                        
         B     TBXIT                                                            
*                                                                               
TOAL9    CLI   QSEQ,C'D'                                                        
         BNE   TBXIT                                                            
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         AP    OBCASH,ACMOBAL                                                   
         ZAP   DUB,OBCASH                                                       
         AP    DUB,ODCASH                                                       
         SP    DUB,OCCASH                                                       
         ZAP   MYOBAL,DUB                                                       
         XC    BUFFREC(BUFFRECQ),BUFFREC                                        
         L     RF,ADACC                                                         
         MVC   BUFFACCT,ACTKULA-ACTRECD(RF) UNIT/LEDGER/ACCOUNT                 
         MVC   BUFFOFFC,OFFCODE    OFFICE CODE                                  
         MVC   BUFFACCS,OBCASH     SAVE OFFICE ACCMULATORS                      
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BUFFREC                               
         B     TBXIT                                                            
         DROP  R2,RF                                                            
         EJECT                                                                  
*              OFFLAST                                                          
*                                                                               
TOFL     CLI   MODE,OFFLAST                                                     
         BNE   TLLA                                                             
         CLI   QSEQ,QSEQOFF                                                     
         BNE   TBXIT                                                            
*                                                                               
TOFL20   BAS   RE,NXTBUF           GET NEXT BUFFALO                             
         TM    BUFSW,BUFEOF                                                     
         BO    TOFL50                                                           
         CLI   BUFFLEVL,0                                                       
         BNE   TOFL50              TEST EOF                                     
         CLC   BUFFOFCO,OFFCODE                                                 
         BNE   TOFL50                                                           
         LA    R6,XFF                                                           
         BAS   RE,BUFLV            GET LAST BUFFALO RECORDS                     
         B     TOFL20                                                           
*                                                                               
TOFL50   BAS   RE,OFFTOT           OFFICE TOTAL                                 
         B     TBXIT                                                            
         EJECT                                                                  
*              LAST LEVELS                                                      
*                                                                               
TLLA     CLI   MODE,LEVALAST                                                    
         BNE   TLLB                                                             
         LA    R2,LEVAGEN                                                       
         L     R6,ADHEIRA                                                       
         B     TLLX                                                             
*                                                                               
TLLB     CLI   MODE,LEVBLAST                                                    
         BNE   TLLC                                                             
         LA    R2,LEVBGEN                                                       
         L     R6,ADHEIRB                                                       
         B     TLLX                                                             
*                                                                               
TLLC     CLI   MODE,LEVCLAST                                                    
         BNE   TLGL                                                             
         LA    R2,LEVCGEN                                                       
         L     R6,ADHEIRC                                                       
*                                                                               
TLLX     CLI   QSEQ,QSEQOFF                                                     
         BNE   TLLX9                                                            
*                                                                               
TLLX3    BAS   RE,NXTBUF           GET THE NEXT BUFFALO RECORD                  
         TM    BUFSW,BUFEOF                                                     
         BO    TLLX9                                                            
         CLI   BUFFLEVL,0                                                       
         BNE   TLLX9                                                            
         CLC   BUFFOFCO,OFFCODE                                                 
         BNE   TLLX9                                                            
         USING LEVGEND,R2                                                       
         SR    R1,R1                                                            
         IC    R1,LEVTLEN          TOTAL LENGTH OF THIS LEVEL                   
         AH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   BUFFACTO(0),1(R6)                                                
         BNE   TLLX9                                                            
         BAS   RE,NEWACC           PROCESS THE NEW RECORD                       
         B     TLLX3               AND GET NEXT                                 
*                                                                               
TLLX9    BAS   RE,LVL                                                           
         B     TBXIT                                                            
         DROP  R2                                                               
         EJECT                                                                  
*              LEDGLAST ETC.                                                    
*                                                                               
TLGL     CLI   MODE,LEDGLAST                                                    
         BNE   TUNL                                                             
         CLI   QSEQ,QSEQOFF        OFFICE SEQUENCE OR. . .                      
         BE    *+12                                                             
         CLI   QSEQ,C'D'           . . . OFFICE DETAIL?                         
         BNE   *+8                                                              
         BAS   RE,BUFOF            FINISH LAST BUFFALO RECORDS                  
         LA    R2,LEDGEN                                                        
         BAS   RE,LVL                                                           
*                                                                               
         CLI   QSEQ,QSEQOFF        OFFICE SEQUENCE OR. . .                      
         BE    *+12                                                             
         CLI   QSEQ,C'D'           . . . OFFICE DETAIL?                         
         BNE   TBXIT                                                            
*                                                                               
         MVI   RCSUBPRG,9          YES -- DO OFFICE RECAP                       
         CLI   PROGPROF+2,C'N'                                                  
         BE    *+8                                                              
         MVI   RCSUBPRG,10                                                      
         MVI   FORCEHED,YES                                                     
         XC    BUFFREC(BUFFRECQ),BUFFREC                                        
         MVI   BUFFLEVL,X'FE'      GET OFFICE TOTALS FOR THE LEDGER             
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUF,BUFFREC,1                            
*                                                                               
TLGL05   TM    DMCB+8,X'80'        ANYTHING TO PRINT?                           
         BO    TBXIT               NO                                           
         CLI   BUFFLEVL,X'FE'      STILL LOOKING AT OFFICE TOTALS?              
         BNE   TLGL10                                                           
         MVC   OFFCODE,BUFFOFFC       GET OFFICE CODE                           
         CLI   QSEQ,QSEQOFF                                                     
         BNE   *+10                                                             
         MVC   OFFCODE,BUFFOFCO                                                 
         MVI   SPACING,2                                                        
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         MVC   PLKEY(2),OFFCODE                                                 
         MVC   PLDESC,SPACES                                                    
         BAS   RE,GETOFN           GET OFFICE NAME                              
         MVC   PLDESC,OFFNAME                                                   
         LA    R3,BUFFACCS         ACCUMULATORS                                 
         BAS   RE,EDT                                                           
         BAS   RE,TPRN                                                          
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUF,BUFFREC,1                             
         B     TLGL05                                                           
         DROP  R4                                                               
*                                                                               
TLGL10   CLI   BUFFLEVL,X'FF'      LEDGER TOTAL RECORD?                         
         BE    *+6                                                              
         DC    H'0'                BUFFALO LEDGER TOTAL RECORD VANISHED         
*                                                                               
         USING PLINED,R4                                                        
         USING NAMELD,R3                                                        
         LA    R4,P                                                             
         MVC   PLSPACES,SPACES     CLEAR POSSIBLE GARBAGE                       
         MVC   PLTOT(L'AC@TOTFR),AC@TOTFR  TOTAL FOR ..                         
         L     R3,ADLDGNAM                                                      
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         EXMVC R1,PLTOT+11,NAMEREC                                              
         LA    R3,BUFFACCS         ACCUMULATORS                                 
         BAS   RE,EDT                                                           
         BAS   RE,TPRN                                                          
         B     TBXIT                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*              UNITLAST                                                         
***********************************************************************         
TUNL     CLI   MODE,UNITLAST                                                    
         BNE   TRQL                                                             
         LA    R2,UNITGEN                                                       
         BAS   RE,LVL                                                           
         B     TBXIT                                                            
         EJECT                                                                  
***********************************************************************         
*              REQLAST                                                          
***********************************************************************         
TRQL     CLI   MODE,REQLAST                                                     
         BNE   TRNL                                                             
         LA    R2,REQGEN                                                        
         BAS   RE,LVL                                                           
         MVI   FCRDTRNS,C'Y'                                                    
         MVI   FCRDHIST,C'Y'                                                    
         B     TBXIT                                                            
         EJECT                                                                  
*              RUNLAST - NOW DO POSTING SUMMARY IF NECESSARY                    
*                                                                               
TRNL     CLI   MODE,RUNLAST                                                     
         BNE   TBXIT                                                            
         GOTO1 ADCPG,DMCB,(RC)   PRINT CONTROL PAGE                             
         B     TBXIT                                                            
         EJECT                                                                  
***********************************************************************         
* CONTROL PROCESSING OF NEW OFFICE RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
BUFOF    NTR1  ,                   OFFICE FIRST                                 
BUFOF1   MVC   SVMODE,MODE                                                      
         BAS   RE,NXTBUF           GET NEXT BUFFALO                             
         TM    BUFSW,BUFEOF                                                     
         BO    TBXIT                                                            
         CLI   BUFFLEVL,0                                                       
         BNE   TBXIT                                                            
         CLI   SVMODE,LEDGLAST                                                  
         BE    *+14                                                             
         CLC   BUFFOFCO,OFFCODE    TEST UP TO MONACC OFFICE                     
         BNL   TBXIT               YES, OK TO RETURN                            
         BAS   RE,NEWOFF           SET NEW OFFICE                               
         BAS   RE,NEWLVA           ADD HIGH LEVELS                              
         BAS   RE,NEWLVB                                                        
         BAS   RE,NEWLVC                                                        
         BAS   RE,NEWACC           PROCESS ACCOUNT                              
*                                                                               
BUFOF7   BAS   RE,NXTBUF           GET NEXT BUFFALO                             
         TM    BUFSW,BUFEOF                                                     
         BO    BUFOF9                                                           
         CLI   BUFFLEVL,0                                                       
         BNE   BUFOF9                                                           
         CLC   BUFFOFCO,OFFCODE                                                 
         BNE   BUFOF9              FINISHED THIS OFFICE                         
         BAS   RE,LEVBRK           SET THE LEVEL BREAK                          
         BAS   RE,BUFLV            PROCESS LEVELS                               
         MVC   MODE,SVMODE         RESET MODE                                   
         B     BUFOF7              AND GET NEXT                                 
*                                                                               
BUFOF9   BAS   RE,LSTLV            PROCESS LAST TIME                            
         BAS   RE,OFFTOT           PRINT OFFICE TOTAL                           
         BAS   RE,SETOFC           RESET MONACC OFFICE                          
         MVI   FORCEHED,YES                                                     
         B     BUFOF1                                                           
         EJECT                                                                  
***********************************************************************         
* CONTROL PROCESSING OF FIRST LEVEL BREAKS                           *          
***********************************************************************         
         SPACE 1                                                                
         USING LEVGEND,R2                                                       
FRSTL    NTR1  ,                   LEVEL FIRST                                  
         MVC   SVMODE,MODE                                                      
         BAS   RE,NXTBUF           GET NEXT BUFFALO                             
         TM    BUFSW,BUFEOF                                                     
         BO    TBXIT                                                            
         CLI   BUFFLEVL,0                                                       
         BNE   TBXIT                                                            
         CLI   MODE,LEVAFRST                                                    
         BE    FRSTLA                                                           
         CLI   MODE,LEVBFRST                                                    
         BE    FRSTLB                                                           
         B     FRSTLC                                                           
*                                                                               
FRSTLA   BAS   RE,NEWLVA           ADD HIGH LEVELS                              
FRSTLB   BAS   RE,NEWLVB                                                        
FRSTLC   BAS   RE,NEWLVC                                                        
         BAS   RE,NEWACC           PROCESS ACCOUNT                              
*                                                                               
FRSTL7   BAS   RE,NXTBUF           GET NEXT BUFFALO                             
         TM    BUFSW,BUFEOF                                                     
         BO    FRSTL9                                                           
         CLI   BUFFLEVL,0                                                       
         BNE   FRSTL9                                                           
         CLC   BUFFOFCO,OFFCODE                                                 
         BNE   FRSTL9              FINISHED THIS OFFICE                         
         SR    R1,R1                                                            
         IC    R1,LEVTLEN          TEST FOR LOWER ACCOUNT                       
         AH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   BUFFACTO(0),1(R6)   TEST AGAINST REAL ACCOUNT                    
         BNL   FRSTL9                                                           
         BAS   RE,LEVBRK           SET THE LEVEL BREAK                          
         BAS   RE,BUFLV            PROCESS LEVELS                               
         MVC   MODE,SVMODE         RESET MODE                                   
         B     FRSTL7              AND GET NEXT                                 
*                                                                               
FRSTL9   BAS   RE,LSTLV            PROCESS LAST TIME                            
         B     TBXIT                                                            
         EJECT                                                                  
***********************************************************************         
* CONTROL PROCESSING OF NEW LEVEL BREAK                               *         
***********************************************************************         
         SPACE 1                                                                
         USING LEVGEND,R2                                                       
BUFLV    NTR1  ,                   LEVEL  FIRST                                 
         CLI   MODE,OFACLAST                                                    
         BE    BUFLVD                                                           
         CLI   MODE,OFFLAST                                                     
         BE    BUFLVD                                                           
         L     R2,SVR2                                                          
         SR    R1,R1                                                            
         IC    R1,LEVTLEN          TOTAL LENGTH OF THIS LEVEL                   
         AHI   R1,1                                                             
         MVC   NEWKEY,SPACES                                                    
         MVC   NEWKEY(1),RCCOMPFL  NEW ACCOUNT KEY                              
         EXMVC R1,NEWKEY+1,BUFFACTO                                             
         CLI   MODE,LEVCFRST                                                    
         BE    BUFLVC                                                           
         CLI   MODE,LEVBFRST                                                    
         BE    BUFLVB                                                           
         CLI   MODE,LEVAFRST                                                    
         BE    BUFLVA                                                           
         DC    H'0'                                                             
BUFLVA   BAS   RE,NEWLVA                                                        
BUFLVB   BAS   RE,NEWLVB                                                        
BUFLVC   BAS   RE,NEWLVC                                                        
BUFLVD   BAS   RE,NEWACC                                                        
         B     TBXIT                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS NEW OFFICE FOR THE FIRST TIME                               *         
***********************************************************************         
         SPACE 1                                                                
NEWOFF   NTR1  ,             OFFICE FIRST                                       
         MVC   OFFCODE,BUFFOFCO                                                 
         MVI   FORCEHED,YES        BREAK A PAGE FOR NEW OFFICE                  
         BAS   RE,GETOFN           SET OFFICE NAME                              
         B     TBXIT                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS NEW LEVEL FOR THE FIRST TIME                                *         
***********************************************************************         
         SPACE 1                                                                
NEWLVA   NTR1  ,             LEVEL A FIRST                                      
         BAS   RE,LSTLV      CLEANUP LAST TIME A B C                            
         OI    CNTBK,CNTBKA  SET CONTROL BREAK A                                
         LA    R2,LEVAGEN                                                       
         MVI   FIRSTLOW,C'Y'                                                    
         B     NEWLVX                                                           
*                                                                               
NEWLVB   NTR1  ,             LEVEL B FIRST                                      
         BAS   RE,LSTLVC                                                        
         BAS   RE,LSTLVB                                                        
         LA    R2,LEVBGEN                                                       
         CLI   LOWLEV,C'C'                                                      
         BL    TBXIT                                                            
         OI    CNTBK,CNTBKB  SET CONTROL BREAK B                                
         B     NEWLVX                                                           
*                                                                               
NEWLVC   NTR1  ,             LEVEL C FIRST                                      
         BAS   RE,LSTLVC                                                        
         LA    R2,LEVCGEN                                                       
         CLI   LOWLEV,C'D'                                                      
         BL    TBXIT                                                            
         OI    CNTBK,CNTBKC  SET CONTROL BREAK C                                
*                                                                               
NEWLVX   SR    R1,R1                                                            
         IC    R1,LEVTLEN          TOTAL LENGTH OF THIS LEVEL                   
         AH    R1,=H'1'                                                         
         MVC   NEWKEY,SPACES                                                    
         MVC   NEWKEY(1),RCCOMPFL  NEW ACCOUNT KEY                              
         EXMVC R1,NEWKEY+1,BUFFACTO                                             
         BAS   RE,GETACC                                                        
         MVC   LEVNAME,WORK                                                     
         SR    R4,R4                                                            
         LA    R6,NEWKEY                                                        
         BAS   RE,LUP                                                           
         B     TBXIT                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS NEW ACCOUNT                                                 *         
***********************************************************************         
         SPACE 1                                                                
NEWACC   NTR1  ,                                                                
         BAS   RE,SETACC           SET LOW ACCOUNT                              
         MVC   NEWKEY,SPACES                                                    
         MVC   NEWKEY(1),RCCOMPFL  NEW ACCOUNT KEY                              
         MVC   NEWKEY+1(14),BUFFACTO                                            
         BAS   RE,GETACC           GET NAME FOR NEW ACCOUNT                     
         MVC   LEVNAME,WORK                                                     
         MVI   ACTIVE,YES                                                       
         ZAP   SVBCASH,BCASH                                                    
         ZAP   SVDCASH,DCASH                                                    
         ZAP   SVCCASH,CCASH                                                    
         ZAP   BCASH,PKZERO                                                     
         ZAP   DCASH,BUFFOD        ADD TO THIS ACCOUNT TOTAL                    
         ZAP   CCASH,BUFFOC                                                     
         MVI   NEWSW,YES                                                        
         LA    R6,NEWKEY                                                        
         SR    R4,R4                                                            
         BAS   RE,LUP                                                           
         BAS   RE,ROLLUP                                                        
         MVI   NEWSW,NO                                                         
         MVC   SVBUFKEY,BUFFREC    SAVE LAST KEY PROCESSED                      
         ZAP   BCASH,SVBCASH                                                    
         ZAP   DCASH,SVDCASH                                                    
         ZAP   CCASH,SVCCASH                                                    
         B     TBXIT                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS LAST TIME BREAKS FOR BUFFALO RECORDS                        *         
***********************************************************************         
         SPACE 1                                                                
LSTLV    NTR1  ,                                                                
         BAS   RE,LSTLVC                                                        
         BAS   RE,LSTLVB                                                        
         BAS   RE,LSTLVA                                                        
         B     TBXIT                                                            
*                                                                               
LSTLVC   TM    CNTBK,CNTBKC        LAST LEVEL C                                 
         BNOR  RE                                                               
         LR    R0,RE                                                            
         LA    R2,LEVCGEN                                                       
         NI    CNTBK,ALL-CNTBKC                                                 
         BAS   RE,LVL                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
LSTLVB   TM    CNTBK,CNTBKB        LAST LEVEL B                                 
         BNOR  RE                                                               
         LR    R0,RE                                                            
         LA    R2,LEVBGEN                                                       
         NI    CNTBK,ALL-CNTBKB                                                 
         BAS   RE,LVL                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
LSTLVA   TM    CNTBK,CNTBKA        LAST LEVEL A                                 
         BNOR  RE                                                               
         LR    R0,RE                                                            
         LA    R2,LEVAGEN                                                       
         NI    CNTBK,ALL-CNTBKA                                                 
         BAS   RE,LVL                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET NEXT BUFFALO RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
NXTBUF   NTR1  ,                                                                
         TM    BUFSW,BUFEOF                                                     
         BO    TBXIT                                                            
         MVC   BUFFREC(BUFFKEYQ),SVBUFKEY    LAST RECORD PROCESSED              
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUF,BUFFREC,1                            
         TM    DMCB+8,X'80'                                                     
         BNO   NXTBUF3                                                          
         OI    BUFSW,BUFEOF                                                     
         B     TBXIT                                                            
*                                                                               
NXTBUF3  OC    SVBUFKEY,SVBUFKEY   TEST FIRST TIME                              
         BZ    TBXIT                                                            
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUF,BUFFREC,1                             
         TM    DMCB+8,X'80'                                                     
         BNO   TBXIT                                                            
         OI    BUFSW,BUFEOF                                                     
         B     TBXIT                                                            
         EJECT                                                                  
***********************************************************************         
* SET A LEVEL BREAK FOR NEW RECORDS                                   *         
***********************************************************************         
         SPACE 1                                                                
LEVBRK   NTR1  ,                                                                
         LA    R2,LEVAGEN          SET R2 TO ACCOUNT LEVEL                      
LEVBRK3  MVI   MODE,OFACLAST                                                    
         CLC   LEVLET,LOWLEV                                                    
         BE    LEVBRKX                                                          
         SR    R1,R1                                                            
         IC    R1,LEVTLEN                                                       
         AH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   BUFFACTO(0),NEWKEY+1                                             
         BNE   LEVBRK5                                                          
         LA    R2,LEVLNQ(R2)                                                    
         B     LEVBRK3                                                          
*                                                                               
LEVBRK5  MVI   MODE,LEVAFRST                                                    
         CLI   LEVLET,C'A'                                                      
         BE    LEVBRKX                                                          
         MVI   MODE,LEVBFRST                                                    
         CLI   LEVLET,C'B'                                                      
         BE    LEVBRKX                                                          
         MVI   MODE,LEVCFRST                                                    
         CLI   LEVLET,C'C'                                                      
         BE    LEVBRKX                                                          
         DC    H'0'                                                             
*                                                                               
LEVBRKX  ST    R2,SVR2                                                          
         B     TBXIT                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT THE OFFICE TOTAL                                              *         
***********************************************************************         
         SPACE 1                                                                
OFFTOT   NTR1  .                                                                
         XC    BUFFREC(BUFFRECQ),BUFFREC                                        
         MVI   BUFFLEVL,X'FE'      GET OFFICE TOTALS FOR THE LEDGER             
         MVC   BUFFOFCO,OFFCODE                                                 
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUF,BUFFREC,1                            
         TM    DMCB+8,X'80'        RECORD FOUND?                                
         BO    OFFTOT3             NO                                           
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         MVC   PLTOT(L'AC@TOTFR),AC@TOTFR  TOTAL FOR ..                         
         MVC   PLSPACES,SPACES     CLEAR POSSIBLE GARBAGE                       
         MVC   PLTOT+11(L'OFFNAME),OFFNAME                                      
         LA    R3,BUFFACCS                                                      
         BAS   RE,EDT                                                           
         BAS   RE,TPRN                                                          
*                                                                               
OFFTOT3  MVC   OFFCODE,SPACES                                                   
         MVC   OFFNAME,SPACES                                                   
         B     TBXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GET AN ACCOUNT RECORD                                               *         
***********************************************************************         
         SPACE 1                                                                
GETACC   NTR1  ,                                                                
         MVC   WORK(L'AC@RECNF),AC@RECNF                                        
         MVC   IOA(42),SPACES      READ ACCOUNT RECORD FOR NAME                 
         MVC   IOA(15),NEWKEY                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCOUNT,IOA,IOA                              
         CLC   NEWKEY,IOA                                                       
         BNE   TBXIT                                                            
         LA    R1,IOA                                                           
         AHI   R1,49                                                            
         SR    R0,R0                                                            
*                                                                               
GETACC3  CLI   0(R1),0                                                          
         BE    TBXIT                                                            
         CLI   0(R1),NAMELQ                                                     
         BE    GETACC5                                                          
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     GETACC3                                                          
*                                                                               
GETACC5  MVC   WORK(36),SPACES                                                  
         SR    RF,RF                                                            
         IC    RF,1(,R1)                                                        
         SHI   RF,NAMEREC-NAMELD+1                                              
         EXMVC RF,WORK,NAMEREC-NAMELD(R1)                                       
         B     TBXIT                                                            
         EJECT                                                                  
***********************************************************************         
* ADD ACCOUNT TOTALS TO HIGHER LEVELS AND PRINT THIS ACCOUNT          *         
***********************************************************************         
         SPACE 1                                                                
ROLLUP   NTR1  ,                                                                
         GOTO1 PROLLER,DMCB,1,ACCUMS,1                                          
         L     R3,DMCB                                                          
         ZAP   0(8,R3),BCASH                                                    
         ZAP   8(8,R3),DCASH                                                    
         ZAP   16(8,R3),CCASH                                                   
         ZAP   DUB,BCASH                                                        
         AP    DUB,DCASH                                                        
         SP    DUB,CCASH                                                        
         ZAP   24(8,R3),DUB                                                     
         CLI   PROGPROF+1,C'Y'                                                  
         BNE   *+14                                                             
         CLC   0(32,R3),=4PL8'0'   DONT ADD DOWN IF WE AREN'T                   
         BE    ROLLUP3             GOING TO PRINT THIS ACCOUNT                  
         GOTO1 PROLLER,DMCB,6,ACCUMS                                            
*                                                                               
ROLLUP3  BAS   RE,SETACC                                                        
*                                                                               
ROLLUP7  BAS   RE,LVL                                                           
         B     TBXIT                                                            
         EJECT                                                                  
***********************************************************************         
* SET R2 TO ACCOUNT LEVEL  FOR CURRENT LEVEL                          *         
***********************************************************************         
         SPACE 1                                                                
         USING LEVGEND,R2                                                       
SETACC   LA    R2,LEVAGEN                                                       
         LA    R0,4                                                             
*                                                                               
SETACC3  CLC   LEVLET,LOWLEV                                                    
         BER   RE                                                               
         LA    R2,L'LEVGEN(R2)                                                  
         BCT   R0,SETACC3                                                       
         DC    H'0',C'HORRIBLE PROBLEM'                                         
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SET OFFICE CODE/NAME FROM MONACC                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING ACMD,RF                                                          
SETOFC   LR    R0,RE                                                            
         L     RF,AMONACC                                                       
         MVC   OFFNAME,ACMOFNAM                                                 
         L     RE,ACMAOFA                                                       
         MVC   OFFCODE,OFAKOFF-OFARECD(RE) OFFICE CODE                          
         BAS   RE,GETOFN                                                        
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* GET OFFICE NAME FROM MONACC                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING ACMD,RF                                                          
GETOFN   NTR1  ,                                                                
         L     RF,AMONACC                                                       
         MVC   OFFNAME,SPACES                                                   
         MVC   OFFNAME(14),=C'UNKNOWN OFFICE'                                   
         ICM   R0,15,ACMNOFNB      R0=N'OFFICES                                 
         BZ    TBXIT               NONE                                         
         L     R2,ACMAOFNB         R2=A(OFFICE NAME BUFFER)                     
         CLC   OFFCODE,0(R2)       MATCH ON OFFICE CODE                         
         BE    *+16                                                             
         LA    R2,ACMOFNL(R2)      NEXT ENTRY                                   
         BCT   R0,*-14                                                          
         B     TBXIT               NOT IN BUFFER                                
*                                                                               
         MVC   OFFNAME,2(R2)                                                    
         B     TBXIT                                                            
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* OFFICE FILTER ROUTINE                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ACMD,RF                                                          
OFCFLT   NTR1  ,                                                                
         SR    R0,R0                                                            
         L     RF,AMONACC                                                       
         ICM   R0,3,ACMOFCN        R0=N'ENTRIES IN OFFICE LIST                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING OFFALD,R1                                                        
         L     R1,ACMAOFL                                                       
         OC    OFFANEWA,OFFANEWA   TEST FOR LIMITED ACCESS                      
         BNZ   *+14                YES                                          
         CLC   OFFAOFFC,SPACES     TEST FOR A REQUESTED OFFICE FILTER           
         BE    TBXIT               NO-ALLOW ACCESS                              
         LA    R1,ACMOFCL                                                       
         CLC   OFFCODE,0(R1)                                                    
         BE    TBXIT                                                            
         LA    R1,L'OAPKOFF(R1)                                                 
         BCT   R0,*-14                                                          
         LTR   RB,RB                                                            
         B     TBXIT                                                            
         DROP  R1,RF                                                            
         EJECT                                                                  
***********************************************************************         
*       STRUCTUR SETS UP A TABLE OF CONTROL INFORMATION USING                   
*       THE LEDGER STRUCTURE AND THE PROFILE OPTIONS                            
***********************************************************************         
         USING LEVGEND,R6                                                       
STR      NTR1  ,                                                                
         LA    R6,LEVAGEN                                                       
         LA    R1,4                                                             
         XC    LEVGEN,LEVGEN                                                    
         LA    R6,L'LEVGEN(R6)                                                  
         BCT   R1,*-10                                                          
                                                                                
         LA    R6,LEVAGEN                                                       
         XC    REQLOW,REQLOW                                                    
         L     R4,ADLDGHIR                                                      
         LA    R4,ACLVALS-ACLELD(R4)                                            
         LA    R3,4                FOUR POSSIBLE LEVELS                         
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         SR    R5,R5                                                            
         IC    R5,=C'A'                                                         
         LA    RE,5                LEVEL A =ROW 5 OF ACCUMULATORS               
         LA    RF,4                ALWAYS 4 LEVELS REQUIRED                     
*                                                                               
STR03    EQU   *                   LOOP BUILDS TABLE                            
         IC    R2,0(R4)                                                         
         LTR   R2,R2                                                            
         BZ    STR05               RUN OUT OF LEVELS                            
         STC   R2,LEVTLEN          TOTAL LENGTH OF THIS LEVEL                   
         STC   R1,LEVKDISP                                                      
         STC   R5,LEVLET                                                        
         STC   RF,LEVNUM                                                        
         SR    R2,R1                                                            
         STC   R2,LEVKLEN                                                       
         AR    R1,R2                                                            
         STC   RE,LEVACMNO                                                      
         MVC   LEVDESC,ACLVDESC-ACLVALS(R4)                                     
         MVC   LEVNAME,SPACES                                                   
         MVC   LEVKEY,SPACES                                                    
         BCTR  RE,0                                                             
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         BCT   RF,*+8                                                           
         STC   R5,REQLOW                                                        
         LA    R5,1(R5)                                                         
         LA    R4,L'ACLVALS(R4)    NEXT LEVEL                                   
         LA    R6,L'LEVGEN(R6)                                                  
         BCT   R3,STR03                                                         
*                                                                               
STR05    BCTR  R5,0                                                             
         STC   R5,LOWLEV                                                        
         OC    REQLOW,REQLOW                                                    
         BNZ   *+10                                                             
         MVC   REQLOW,LOWLEV                                                    
         B     TBXIT                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*              LUP  MAINTAINS THE VARIABLE FIELDS IN LEVAGEN ETC.               
***********************************************************************         
LUP      NTR1  ,                                                                
         USING LEVGEND,R2                                                       
         USING NAMELD,R4                                                        
         USING ACTRECD,R6                                                       
         CLI   LEVNUM,0                                                         
         BE    TBXIT                                                            
         LTR   R4,R4               TEST NAME REQUIRED                           
         BZ    LUP02                                                            
         MVC   LEVNAME,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMEREC-NAMELD+1                                              
         EXMVC R1,LEVNAME,NAMEREC                                               
*                                                                               
LUP02    LA    R5,LEVKEY                                                        
         MVC   LEVKEY,SPACES                                                    
         CLC   LEVLET,LOWLEV                                                    
         BE    LUP03                                                            
         BAS   RE,LUP11                                                         
         B     TBXIT                                                            
*                                                                               
LUP03    LA    R2,LEVAGEN                                                       
LUP05    BAS   RE,LUP11                                                         
         CLC   LEVLET,LOWLEV                                                    
         BE    TBXIT                                                            
         SR    R1,R1                                                            
         IC    R1,LEVKLEN                                                       
         LA    R5,1(R1,R5)                                                      
         AHI   R2,L'LEVGEN                                                      
         B     LUP05                                                            
*                                                                               
LUP11    SR    R1,R1                                                            
         IC    R1,LEVKDISP                                                      
         SR    R3,R3                                                            
         IC    R3,LEVKLEN                                                       
         BCTR  R3,0                                                             
         LA    R1,3(R1,R6)                                                      
         EXMVC R3,0(R5),0(R1)                                                   
         BR    RE                                                               
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
*        LVL HANDLES PRINTING OF REPORT & RESETING OF                           
*        ACCUMULATORS                                                           
***********************************************************************         
LVL      NTR1  ,                                                                
         USING LEVGEND,R2                                                       
         CLI   LEVNUM,0                                                         
         BE    LVL25                                                            
         SR    R3,R3                                                            
         IC    R3,LEVACMNO                                                      
         GOTO1 PROLLER,DMCB,1,ACCUMS,(R3)                                       
         L     R3,DMCB                                                          
*                                                                               
*              RULES FOR THE PRINTING OF LINES                                  
*                                                                               
         CLI   LEVLET,C'D'         HIGH LEVELS                                  
         BNH   LVL03                                                            
         CLI   LEVLET,C'R'                                                      
         BE    *+14                SKIP TEST IF REQLAST                         
         CLC   0(32,R3),=4PL8'0'                                                
         BE    LVL25                                                            
         CLC   LEVLET,REQLOW                                                    
         BE    LVL13                                                            
         CLC   LASTFIGS,0(R3)                                                   
         BE    LVL25                                                            
         MVC   LASTFIGS,0(R3)                                                   
         B     LVL13                                                            
*                                                                               
LVL03    CLI   ACTIVE,YES          HAVE WE HAD A LOW LEVEL ACCOUNT              
         BNE   LVL25                                                            
         CLC   LEVLET,LOWLEV       LOW LEVEL                                    
         BNE   LVL05                                                            
         CLI   PROGPROF+1,YES                                                   
         BNE   LVL11                                                            
         CLC   0(32,R3),=4PL8'0'                                                
         BE    LVL25                                                            
         B     LVL11                                                            
*                                                                               
LVL05    CLC   LEVLET,REQLOW       MIDDLE LEVELS                                
         BNE   LVL07                                                            
         CLC   0(32,R3),=4PL8'0'                                                
         BE    LVL25                                                            
         B     LVL11                                                            
*                                                                               
LVL07    CLI   LEVLET,C'A'                                                      
         BL    LVL13                                                            
         CLI   LEVLET,C'D'                                                      
         BH    LVL09                                                            
         SR    RF,RF                                                            
         IC    RF,LEVLET                                                        
         BCTR  RF,0                                                             
         SLL   RF,28                                                            
         SRL   RF,26                                                            
         EX    0,EXLVL(RF)                                                      
         LH    RF,0(R1)                                                         
         AHI   RF,1                                                             
         STH   RF,0(R1)                                                         
*                                                                               
LVL09    SR    RF,RF                                                            
         IC    RF,LEVLET           CHECK LEVEL COUNT                            
         CLI   LEVLET,C'C'                                                      
         BH    LVL13                                                            
         SLL   RF,28                                                            
         SRL   RF,26                                                            
         EX    0,EXLVL(RF)                                                      
         CLC   0(2,R1),=H'1'                                                    
         BH    LVL13                                                            
         CLI   SPACESW,C' '        ONLY 1 LINE FOR LEVEL(S).                    
         BE    LVL25               LINE SKIPPED ALREADY, NO MORE.               
         CLI   LASTSPAC,1          IF LAST LINE WAS NOT SKIPPED                 
         BNE   LVL25               AND WAS NOT A TOTAL LINE,                    
         CLI   FORCEHED,YES                                                     
         BE    LVL25               DON'T SKIP IF WE'RE AT TOP OF PAGE           
         MVC   P+1(109),SPACES     SKIP A LINE HERE.                            
         BAS   RE,TPRN                                                          
         B     LVL25                                                            
*                                                                               
LVL11    SR    RF,RF                                                            
         IC    RF,LEVLET           UPDATE LEVEL COUNT                           
         BCTR  RF,0                                                             
         SLL   RF,28                                                            
         SRL   RF,26                                                            
         EX    0,EXLVL(RF)                                                      
         LH    RF,0(R1)                                                         
         AHI   RF,1                                                             
         STH   RF,0(R1)                                                         
*                                                                               
*              NOW WE PRINT THE LINE                                            
*                                                                               
LVL13    LA    R4,P                                                             
         USING PLINED,R4                                                        
         CLC   LEVLET,LOWLEV                                                    
         BNE   LVL17                                                            
         CLI   FIRSTLOW,YES                                                     
         BNE   LVL15                                                            
         MVI   FIRSTLOW,NO                                                      
         L     RF,AMONACC                                                       
         TM    ACMINDS-ACMD(RF),ACMIEMUD+ACMINEWO                               
         BNO   *+12                NOT ON NEW FILE/NEW OFFICES                  
         CLI   QSEQ,QSEQOFF        TEST OFFICE SEQUENCE                         
         BNE   LVL15               NO LINE SKIP FOR ACCOUNT SEQUENCE            
         CLI   LASTSPAC,1                                                       
         BNE   LVL15                                                            
         MVC   P+1(109),SPACES                                                  
         BAS   RE,TPRN                                                          
*                                                                               
LVL15    MVC   PLKEY,LEVKEY                                                     
         MVC   PLDESC,LEVNAME                                                   
         MVI   SPACING,1                                                        
         CLI   QSEQ,QSEQOFF        TEST OFFICE SEQUENCE                         
         BE    LVL23                                                            
         L     RF,AMONACC                                                       
         TM    ACMINDS-ACMD(RF),ACMIEMUD+ACMINEWO                               
         BNO   LVL23               NOT ON NEW FILE/NEW OFFICES                  
         CLI   QSEQ,C'D'           SHOW OFFICE DETAIL?                          
         BNE   LVL23               NO                                           
         MVC   PLKEY,SPACES        CLEAR OUT KEY FIELD                          
         MVC   PLDESC,SPACES       AND NAME                                     
         MVC   PLDESC(7),=C'*TOTAL*'                                            
         B     LVL23                                                            
*                                                                               
LVL17    CLI   LASTSPAC,1                                                       
         BNE   LVL19                                                            
         CLI   SPACESW,C' '        IF WE'VE JUST SKIPPED A LINE,                
         BE    LVL19               DON'T SKIP ANOTHER ONE.                      
         MVC   P+1(109),SPACES                                                  
         BAS   RE,TPRN             NEED A BLANK LINE                            
*                                                                               
LVL19    MVI   SPACING,2                                                        
         MVC   PLSPACES,SPACES     CLEAR POSSIBLE GARBAGE                       
         MVC   PLTOT(L'AC@TOTFR),AC@TOTFR  TOTAL FOR ..                         
         CLI   LEVLET,C'D'                                                      
         BNH   LVL21                                                            
         MVC   PLDESC,LEVNAME      REQUEST,UNIT,LEDGER TOTALS                   
         B     LVL23                                                            
*                                                                               
LVL21    MVI   AREA,C' '                                                        
         MVC   AREA+1(L'AREA-1),AREA                                            
         LA    R1,AREA                                                          
         MVC   0(L'LEVDESC,R1),LEVDESC                                          
         LA    R1,L'LEVDESC+1(R1)                                               
         MVC   0(L'LEVNAME,R1),LEVNAME                                          
         LA    R1,L'LEVNAME+1(R1)                                               
         MVI   0(R1),C'('                                                       
         AHI   R1,1                                                             
         MVC   0(L'LEVKEY,R1),LEVKEY                                            
         LA    R5,L'AREA                                                        
         GOTO1 ADSQUASH,DMCB,AREA,(R5)                                          
         L     R5,DMCB+4                                                        
         LA    R5,AREA(R5)                                                      
         MVI   0(R5),C')'                                                       
         L     R0,DMCB+4                                                        
         AHI   R0,1                                                             
         LA    R5,L'PLDESC                                                      
         GOTO1 CHOPPER,DMCB,((R0),AREA),((R5),PLDESC),(C'P',2)                  
*                                                                               
LVL23    CLI   NEWSW,YES           TEST NEW ACCOUNT                             
         BNE   *+8                                                              
         MVI   P+16,C'*'                                                        
         MVC   LASTSPAC,SPACING                                                 
         BAS   RE,EDT                                                           
         BAS   RE,TPRN                                                          
*                                                                               
LVL25    SR    R3,R3                                                            
         IC    R3,LEVACMNO         CLEAR 1ST N ROWS OF ACCUMULATORS             
*                                                                               
LVL27    GOTO1 PROLLER,DMCB,2,ACCUMS,(R3)                                       
         BCT   R3,LVL27                                                         
         CLC   LEVLET,LOWLEV                                                    
         BE    LVL29                                                            
         CLI   LEVLET,C'D'                                                      
         BH    LVL29                                                            
         SR    RF,RF                                                            
         IC    RF,LEVLET                                                        
         SLL   RF,28                                                            
         SRL   RF,26                                                            
         EX    0,EXLVL(RF)                                                      
         XC    0(L'LEVACONT,R1),0(R1)                                           
*                                                                               
LVL29    CLI   LEVLET,C'A'                                                      
         BNE   *+8                                                              
         MVI   ACTIVE,NO                                                        
         B     TBXIT                                                            
*                                                                               
EXLVL    LA    R1,LEVACONT                                                      
         LA    R1,LEVBCONT                                                      
         LA    R1,LEVCCONT                                                      
         LA    R1,LEVDCONT                                                      
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        EDITOR,EDITS FIGURES ONTO PLINE                                        
***********************************************************************         
EDT      NTR1  ,                                                                
         USING PLINED,R4                                                        
         LA    R5,PLFIGS                                                        
         LA    R2,4                                                             
*                                                                               
EDT03    MVC   9(L'AC@NIL,R5),AC@NIL                                            
         CP    0(8,R3),PKZERO      R3 PTS TO LINE OF ACCUMLATORS                
         BE    EDT05                                                            
         MVC   9(L'AC@NIL,R5),SPACES                                            
         CURED (P8,0(R3)),(15,0(R5)),2,MINUS=YES                                
*                                                                               
EDT05    LA    R5,L'PLFIGS(R5)                                                  
         LA    R3,8(R3)                                                         
         BCT   R2,EDT03                                                         
         B     TBXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
***********************************************************************         
TPRN     NTR1  ,                                                                
         MVC   SPACESW,P+17                                                     
         CLI   RCSUBPRG,9          NOT DURING OFFICE RECAP                      
         BE    TPRN01                                                           
         CLI   RCSUBPRG,10                                                      
         BE    TPRN01                                                           
         CLI   QSEQ,C'D'                                                        
         BNE   *+14                                                             
         MVC   HEAD4+47(18),=C'WITH OFFICE DETAIL'                              
         B     TPRN01                                                           
         CLI   QSEQ,QSEQOFF                                                     
         BNE   TPRN01                                                           
         MVC   HEAD4+51(9),=C'BY OFFICE'                                        
*                                                                               
TPRN01   MVC   HEAD5+83(L'HEADMON),HEADMON                                      
         CLI   RCSUBPRG,9                                                       
         BE    TPRN02                                                           
         CLI   RCSUBPRG,10                                                      
         BE    TPRN02                                                           
         CLI   QSEQ,QSEQOFF                                                     
         BNE   TPRN02                                                           
         MVC   HEAD6+83(2),OFFCODE                                              
         MVC   HEAD6+86(23),OFFNAME ONLY ROOM FOR 23 CHARACTERS                 
*                                                                               
TPRN02   TM    RNSW,RNLV           ANY LIVE REQUESTS                            
         BO    *+10                                                             
         MVC   HEAD5+53(L'AC@DRAFT),AC@DRAFT                                    
         CLI   PROGPROF+2,NO                                                    
         BE    TPRN03                                                           
         CLI   RCSUBPRG,3                                                       
         BE    TPRN03                                                           
         MVC   HEAD8+57(L'HEADMONF),HEADMONF                                    
         MVC   HEAD8+75(L'HEADMONF),HEADMON+9                                   
         MVC   HEAD8+102(L'HEADMONF),HEADMON+9                                  
TPRN03   GOTO1 ACREPORT                                                         
*                                                                               
TBXIT    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              CONSTANTS, LITERAL POOL                                          
***********************************************************************         
XFF      DC    (SMKLNQ)X'FF'       X'FF'S                                       
         EJECT                                                                  
         LTORG                                                                  
         DROP  R9,RB                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT INTERNAL CONTROL PAGE                                         *         
***********************************************************************         
                                                                                
CONPGE   DS    0D                                                               
         NMOD1 0,*CONPGE*                                                       
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
         GOTO1 ADSORTER,DMCB,=C'END'                                            
         L     R7,LOGOC                                                         
         USING LOGOD,R7                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 LOGO,DMCB,(R7)                                                   
         DROP  R7                                                               
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         CP    TOTCNT,PKZERO                                                    
         BE    CONPX                                                            
         MVI   RCSUBPRG,3                                                       
*                                                                               
         USING RUNXTRAD,R6                                                      
         USING MASTD,R7                                                         
         USING REMOTED,RF                                                       
         L     R6,VEXTRAS                                                       
         L     R7,ADMASTC                                                       
         L     RF,MCVREMOT                                                      
         XC    MCARC,MCARC           Get rid of this                            
         XC    REMOTKEY,REMOTKEY     TOTAL PAGE GOES TO DATA CONT               
         MVI   REMOTJID,C'A'                                                    
         MVC   REMOTJID+1(2),QPROG                                              
         MVI   REMOTCLS,C'T'                                                    
         MVI   REMOTCPY,X'01'                                                   
         MVC   REMOTDST,=AL2(1106)   ID=ZIP                                     
         MVC   REMOTKEY(8),MCJOB                                                
         MVC   REMOTFNO,REMOTKEY                                                
         XC    REMOTRTY,REMOTRTY                                                
         XC    REMOTARC,REMOTARC                                                
         NI    REMOTTYP,X'FF'-X'60'                                             
         NI    REMOTTY1,X'FF'-(REMOTAEQ+REMOTARQ+REMOTADQ)                      
         MVI   REMOTPRT,0                                                       
         MVI   FORCEHED,YES                                                     
         MVC   PAGE,=H'1'                                                       
         LA    R2,P+63                                                          
         LA    R3,DDSDR                                                         
         CP    0(8,R3),PKZERO                                                   
         BE    CONP05                                                           
         CURED (P8,0(R3)),(15,0(R2)),2,MINUS=YES                                
*                                                                               
CONP05   LA    R3,8(R3)                                                         
         LA    R2,P+81                                                          
         CP    0(8,R3),PKZERO                                                   
         BE    CONP07                                                           
         CURED (P8,0(R3)),(15,0(R2)),2,MINUS=YES                                
*                                                                               
CONP07   GOTO1 ACREPORT                                                         
         GOTO1 POSTWRK,DMCB,(RA),ID  UPDATE THE FILE                            
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
*                                                                               
CONPX    XIT1                                                                   
         DROP  R6,R7,RF                                                         
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* TABLES AND BUFFERS                                                  *         
***********************************************************************         
                                                                                
MXACC    EQU   200                                                              
         DS    0D                  ACCOUNT SUMMARY TABLE                        
         DC    CL8'*ACCTAB*'                                                    
ACCTAB   DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(BFLNQ)          RECORD LENGTH                                
         DC    AL4(BFKLNQ)         DISP. TO KEY/ KEY LENGTH                     
         DC    AL4(MXACC)          MAX. IN TABLE                                
         DC    AL1(BFNBK)          NUMBER OF BUCKETS                            
         DC    AL1(BFBK-BFBGN)     DISP. TO BUCKETS                             
         DC    X'00'               STATUS                                       
         DC    AL1(0)                                                           
         DS    (MXACC*BFLNQ)C      TABLE                                        
*                                                                               
MXOFF    EQU   80                                                               
         DS    0D                  OFFCIE SUMMARY TABLE                         
         DC    CL8'*OFFTAB*'                                                    
OFFTAB   DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(BFLNQ)          RECORD LENGTH                                
         DC    AL4(BFKLNQ)         DISP. TO KEY/ KEY LENGTH                     
         DC    AL4(MXOFF)          MAX. IN TABLE                                
         DC    AL1(BFNBK)          NUMBER OF BUCKETS                            
         DC    AL1(BFBK-BFBGN)     DISP. TO BUCKETS                             
         DC    X'00'               STATUS                                       
         DC    AL1(0)                                                           
         DS    (MXOFF*BFLNQ)C      TABLE                                        
*                                                                               
MXBUKS   EQU   500                                                              
         DS    0D                  OFFCIE SUMMARY TABLE                         
         DC    CL8'**BUKS**'                                                    
BUKTAB   DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(BUKRLNQ)        RECORD LENGTH                                
         DC    AL4(BUKKLNQ)        DISP. TO KEY/ KEY LENGTH                     
         DC    AL4(MXBUKS)         MAX. IN TABLE                                
         DC    AL1(2)              NUMBER OF BUCKETS                            
         DC    AL1(BUKGL$-BUKTABD) DISP.  TO BUCKETS                            
         DC    X'00'               STATUS                                       
         DC    AL1(0)                                                           
         DS    (MXBUKS*BUKRLNQ)C   TABLE                                        
*                                                                               
MXERR    EQU   1000                                                             
         DS    0D                  ACCOUNT ERROR TABLE                          
         DC    CL8'*ERRTAB*'                                                    
ERRTAB   DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(BFLNQ)          RECORD LENGTH                                
         DC    AL4(BFKLNQ)         DISP. TO KEY/ KEY LENGTH                     
         DC    AL4(MXERR)          MAX. IN TABLE                                
         DC    AL1(BFNBK)          NUMBER OF BUCKETS                            
         DC    AL1(BFBK-BFBGN)     DISP. TO BUCKETS                             
         DC    X'00'               STATUS                                       
         DC    AL1(0)                                                           
         DS    (MXERR*BFLNQ)C      TABLE                                        
*                                                                               
*                                                                               
MXPST    EQU   100                                                              
         DS    0D                                                               
         DC    CL8'*POSTLST'                                                    
POSTLST  DS    (MXPST)CL(PSTLNQ) POSTING INSTRUCTIONS                           
*                                                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'POSTBUFF'                                                    
POSTBUFF DC    4500X'00'                                                        
*                                                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'POSTOTAB'                                                    
POSTOTAB DS    256XL18             POSTING TABLE BY OFFICE (256 MAX)            
*                                   FIRST 2 BYTES ARE OFFICE CODE               
*                                   THEN PL8 DEBITS, PL8 CREDITS                
         DC    AL1(EOT)            END OF TABLE MARKER                          
*                                                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'*BUFFALO'                                                    
         BUFF  LINES=2000,ROWS=3,COLUMNS=6,FLAVOR=PACKED,              X        
               KEYLIST=(32,A),COMMENT=86                                        
         EJECT                                                                  
***********************************************************************         
* DSECTS                                                                        
***********************************************************************         
WORKD    DSECT                                                                  
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOR      EQU   0                                                                
EOT      EQU   X'FF'                                                            
TURNOFF  EQU   X'FF'                                                            
                                                                                
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
HELLO    DS    V                   V(HELLO)                                     
HELEN    DS    V                   V(HELEN)                                     
ADTRL    DS    A                   A(TRIBAL)                                    
ADCPG    DS    A                   A(CONPGE)                                    
ADACT    DS    A                   A(ACCTAB)                                    
ADOFT    DS    A                   A(OFFTAB)                                    
ADBUKTAB DS    A                   A(BUKTAB)                                    
ADERR    DS    A                   A(ERRTAB)                                    
ADPST    DS    A                   A(POSTLST)                                   
ADPBUF   DS    A                   A(POSTBUFF)                                  
ADPSTOTB DS    A                   A(POSTOTAB)                                  
ADBUF    DS    A                   A(BUFFALOC)                                  
ACADDBUK DS    V                   V(ACADDBUK)                                  
*                                                                               
APHASES  DS    0A                  ** ADDRESSES OF LOADED PHASES **             
POSTWRK  DS    A                                                                
*                                                                               
ADLEVS   DS    0XL8                                                             
ADLEV4   DS    A                   A(LEVEL 4 ACCOUNT RECORD)                    
ADNAM4   DS    A                   A(LEVEL 4 NAME ELEMENT)                      
ADLEV3   DS    A                   A(LEVEL 3 ACCOUNT RECORD)                    
ADNAM3   DS    A                   A(LEVEL 3 NAME ELEMENT)                      
ADLEV2   DS    A                   A(LEVEL 2 ACCOUNT RECORD)                    
ADNAM2   DS    A                   A(LEVEL 2 NAME ELEMENT)                      
ADLEV1   DS    A                   A(LEVEL 1 ACCOUNT RECORD)                    
ADNAM1   DS    A                   A(LEVEL 1 NAME ELEMENT)                      
ADLEVQ   EQU   4                   POSSIBLE # OF LEVELS                         
*                                                                               
NEWGLEL  DS    A                   A(New GL element)                            
LSTSTAR  DS    F                   A(OFFICE OVERRIDE IN ACCOUNT)                
*                                                                               
LANG     DS    CL1                                                              
CTRY     DS    CL1                                                              
SYSEQU   DS    CL1                                                              
OFFNEW   DS    CL1                 Yes/No                                       
*                                                                               
RNSW     DS    XL1                 OPTION SWITCHES FOR THE RUN                  
RNSM     EQU   X'80'               PRINT A RUN SUMMARY                          
RNTB     EQU   X'40'               TRIAL BALANCE MODE                           
RNOF     EQU   X'20'               POST TO G/L BY OFFICE                        
RNLV     EQU   X'10'               AT LEAST ONE REQUEST WAS LIVE                
RNTOF    EQU   X'08'               POST TO G/L BY TRANSACTION OFFICE            
RNTRC    EQU   X'04'               TRACE TRIAL BALANCE                          
RNYES    EQU   X'02'               RERUN=YES                                    
RNALL    EQU   X'01'               RERUN=ALL                                    
ALL      EQU   X'FF'                                                            
*                                                                               
RQSW     DS    XL1                 OPTION SWITCHES FOR THE REQUEST              
RQSM     EQU   X'80'               PRINT A REQUEST SUMMARY                      
RQDF     EQU   X'40'               REQUEST IS DRAFT MODE                        
RQIR     EQU   X'20'               INVOICE REGISTER SYSTEM                      
RQER     EQU   X'10'               PRINT ERROR SUMMARY                          
RQREV    EQU   X'08'               REVERSE POSTINGS                             
RQMNT    EQU   X'04'               HONOR MOA LOCK FOR CORRECTION                
*                                                                               
LGSW     DS    XL1                 OPTION SWITCHES FOR THE LEDGER               
LGSM     EQU   X'80'               PRINT A LEDGER SUMMARY                       
LGBY     EQU   X'40'               BYPASS THE LEDGER                            
LGAC     EQU   X'20'               CURRENT LEDGER ACTIVITY                      
LGPV     EQU   X'10'               PREVIOUS LEDGER ACTIVITY                     
LGNP     EQU   X'08'               LEDGER HAS NO DEFAULT POSTING RULE           
LGPR     EQU   X'04'               PRODUCTION LEDGER                            
LGOF     EQU   X'02'               GET PRODUCTION RULES BY OFFICE               
LGRL     EQU   X'01'               USE G/L RULES FOR OFFICE                     
*                                                                               
LGSW2    DS    XL1                 OPTION SWITCHES FOR THE LEDGER II            
LGRLT    EQU   X'01'               USE G/L RULES FOR OFFICE IN TRANS            
*                                                                               
OFFSW    DS    CL1                 OFFICE MODE SWITCH                           
OFFSTRN  EQU   PROCTRNS                                                         
OFFSACL  EQU   ACCLAST                                                          
*                                                                               
TRNSW    DS    XL1                 Tranaction switch                            
TRNUPDB  EQU   X'80'               .  Item that already updated                 
TRNREV   EQU   X'40'               .  Item was reversed                         
TRNRGLD  EQU   X'20'               .  Reversing GLDELD                          
*                                                                               
SVBFSTAT DS    XL1                 Save off BFSTAT when TRNRGLD is on           
*                                                                               
OFFORIG  DS    CL2                                                              
ACCRULE  DS    CL1                 TO ACCOUNT RULE                              
OFARULE  DS    CL1                 Office rule for account                      
OFTRULE  DS    CL1                 Office rule for transaction                  
FLTRULE  DS    XL1                 Filter rule  saved at PROCACC                
FLTOFFC  DS    CL1                 Filter value saved at PROCACC                
*                                                                               
PRNSW    DS    XL1                 PRINTING OPTIONS                             
PRNTOT   EQU   X'80'               PRINT ACCOUNT TOTAL IN SUMMARY               
PRNACN   EQU   X'40'               PRINT ACCOUNT NAME                           
PRNACT   EQU   X'20'               PRINT ACCOUNT TOTAL                          
PRNCAN   EQU   X'10'               PRINT CONTRA ACCOUNT NAME                    
PRNCAT   EQU   X'08'               PRINT CONTRA ACCOUNT TOTAL                   
PRNCATS  EQU   X'04'               PRINT CONTRA-ACCNT TOTAL IN SUMMARY          
PRNSKIP  EQU   X'02'               LAST LINE WAS A BLANK                        
*                                                                               
COOFFC   DS    CL2                 COMPANY DEFAULT OFFICE                       
LGOFFC   DS    CL2                 LEDGER DEFAULT OFFICE                        
ACOFFC   DS    CL2                 ACCOUNT DEFAULT OFFICE                       
CLOFFC   DS    CL2                 CLIENT/PRODUCT OFFICE (PROD)                 
POOFFC   DS    CL2                 POSTING OFFICE                               
GLOFFC   DS    CL2                 GL RULE OFFICE                               
OFFP     DS    XL1                 OFFICE POSITION                              
*                                                                               
ID       DS    CL16                WORK FILE ID                                 
IDX      DS    CL16                WORK FILE ID INDEX                           
*                                                                               
AMOUNT   DS    PL8                 TRANSACTION AMOUNT                           
POSTDR   DS    PL8                 LEDGER POSTING TOTALS                        
POSTCR   DS    PL8                                                              
POSTAMT  DS    PL8                                                              
*                                                                               
TOACC    DS    CL14                TO ACCOUNT                                   
FRACC    DS    CL14                FROM ACCOUNT                                 
SVTO     DS    CL14                SAVE THE TO ACCOUNT                          
LSTACC   DS    CL14                ACCOUNT FOR LAST TO NAME                     
LSTCNTNM DS    CL14                LAST CONTRA-ACCOUNT NAME                     
AGLRULEC DS    A                   A(G/L RULES ELEMENT @ CMP LEVEL)             
AGLRULE  DS    A                   A(G/L RULES ELEMENT)                         
AREVGLD  DS    A                   A(GLDEL) to reverse                          
*                                                                               
MYWORK   DS    CL64                                                             
MYWORK2  DS    12D                                                              
ELCODE   DS    CL1                                                              
EL60     DS    CL1                                                              
ELEMENT  DS    CL255               ELEMENT AREA (60 AND 63 ELEMENT)             
*                                                                               
MDADSP   DS    XL1                 DISPLACEMENT TO JOB MEDIA CODE               
*                                                                               
LENCLI   DS    XL1                 Length of client                             
LENPRD   DS    XL1                 Length of product                            
LENCLPR  DS    XL1                 Length of client & product                   
*                                                                               
TODAY    DS    CL6                                                              
TODAY2   DS    CL2                                                              
TODAY3   DS    CL3                                                              
ENDATE   DS    CL3                                                              
MOSDT    DS    CL2                                                              
HDATE    DS    CL6                                                              
SVTRNMOA DS    XL2                 Saved transaction MOA (see GLELEM)           
GLMOA    DS    XL2                 Packed MOA                                   
*                                                                               
TODAYP   DS    XL3                                                              
YYMMDD   DS    PL3                 TEMPORARY DATE FIELD                         
*                                                                               
SAVEDATE DS    CL4                                                              
THISDATE DS    CL3                                                              
SVDATE   DS    CL3                 SAVEDATE PACKED                              
REVDATE  DS    XL3                 Packed   TRSUPDT when reversing              
SAVRE    DS    F                                                                
*                                                                               
TOTBK    EQU   *                                                                
ACCTOT   DS    (BFNBK)PL(L'BFBK)   ACCOUNT TOTALS                               
CACTOT   DS    (BFNBK)PL(L'BFBK)   CONTRA TOTALS                                
LDGTOT   DS    (BFNBK)PL(L'BFBK)   LEDGER TOTALS                                
REQTOT   DS    (BFNBK)PL(L'BFBK)   REQUEST TOTALS                               
RUNTOT   DS    (BFNBK)PL(L'BFBK)   RUN TOTALS                                   
TOTCNT   DS    PL(L'BFBK)          NUMBER POSTINGS                              
TOTCSH   DS    PL(L'BFBK)          TOTAL CASH                                   
DDSDR    DS    PL(L'BFBK)          TOTAL DEBITS                                 
DDSCR    DS    PL(L'BFBK)          TOTAL CREDITS                                
TOTNBK   EQU   (*-TOTBK)/L'BFBK                                                 
*                                                                               
MXERLG   EQU   15                  MAXIMUM NUMBER OF LEDGER ERRORS              
ERLGR    DS    (MXERLG)CL2         LIST OF LEDGERS MISSING RULES                
*                                                                               
LEVACONT DS    H                                                                
LEVBCONT DS    H                                                                
LEVCCONT DS    H                                                                
LEVDCONT DS    H                                                                
*                                                                               
HEADMONF DS    CL6                                                              
HEADMON  DS    CL30                                                             
START3   DS    PL3                 QSTART IN 2 BYTE FORMAT                      
*                                                                               
BCASH    DS    PL8                                                              
DCASH    DS    PL8                                                              
CCASH    DS    PL8                                                              
*                                                                               
OBCASH   DS    PL8                 OFFICE BALANCE                               
ODCASH   DS    PL8                 OFFICE DEBITS                                
OCCASH   DS    PL8                 OFFICE CREDITS                               
MYOBAL   DS    PL8                 OFFICE MANUAL BALANCE                        
*                                                                               
SVBCASH  DS    PL8                                                              
SVDCASH  DS    PL8                                                              
SVCCASH  DS    PL8                                                              
*                                                                               
ADD28    DS    PL8                 Number added                                 
CHG28    DS    PL8                 Number changed                               
*                                                                               
OFFCODE  DS    CL2                 OFFICE CODE                                  
OFFNAME  DS    CL36                OFFICE NAME                                  
*                                                                               
REQGEN   DS    CL(LEVLNQ)          )                                            
UNITGEN  DS    CL(LEVLNQ)          )                                            
LEDGEN   DS    CL(LEVLNQ)          )                                            
LEVAGEN  DS    CL(LEVLNQ)          )-LEVGEND RECORDS                            
LEVBGEN  DS    CL(LEVLNQ)          )                                            
LEVCGEN  DS    CL(LEVLNQ)          )                                            
LEVDGEN  DS    CL(LEVLNQ)          )                                            
*                                                                               
UPSI     DS    X                   User debug switch                            
UPSI63   EQU   X'80'               .  Dump out X'63' element                    
UPSICNT  EQU   X'40'               .  Print out count at end                    
UPSIDKEY EQU   X'20'               .  Dump x'28' keys on add                    
UPSIOLD  EQU   X'10'               .  Old way vs New                            
*                                                                               
DAYTIME  DS    XL4                                                              
LASTFIGS DS    CL32                                                             
LASTSPAC DS    CL1                                                              
LOWLEV   DS    CL1                 LETTER OF LOWEST LEVEL THIS LEDGER           
REQLOW   DS    CL1                 LETTER OF LOWEST REQUESTED LEVEL             
FIRSTLOW DS    CL1                                                              
ACTIVE   DS    CL1                                                              
SPACESW  DS    CL1                                                              
SVQSEQ   DS    CL1                 SAVE FIRST REQUESTED REPORT SEQUENCE         
SVFCSEQ  DS    CL1                 SAVE FIRST REQUESTED REPORT SEQUENCE         
FRSTBUFF DS    CL1                 'Y' = FIRST BUFFALO READ FOR LEDGER          
PKZERO   DS    PL1                 Zero packed                                  
ACCUMS   DS    CL8,(8*5)PL8                                                     
*                                                                               
BUKREC   DS    CL(BUKRLNQ)                                                      
*                                                                               
BUFFREC  DS    0D                  BUFFALO RECORD                               
BUFFLEVL DS    X                   SORT LEVEL                                   
*                                   X'00' = LOW LEVEL OFFICE TOTALS             
*                                   X'FE' = OFFICE TOTALS FOR LEDGER            
*                                   X'FF' = LEDGER TOTALS ACROSS OFFICE         
BUFFACCT DS    CL14                ACCOUNT                                      
BUFFOFFC DS    CL2                 OFFICE CODE                                  
         ORG   BUFFACCT                                                         
BUFFOFCO DS    CL2                 OFFICE CODE (WHEN QSEQ = C'O')               
BUFFACTO DS    CL14                ACCOUNT (WHEN QSEQ = C'O')                   
BUFFKEYQ EQU   *-BUFFREC           BUFFALO KEY LENGTH                           
         DS    XL1                 SPARE                                        
BUFFACCS DS    0CL32               ACCUMULATORS                                 
BUFFOB   DS    PL8                 OFFICE BALANCE                               
BUFFOD   DS    PL8                 OFFICE DEBITS                                
BUFFOC   DS    PL8                 OFFICE CREDITS                               
BUFFMY   DS    PL8                 OFFICE MANUAL BALANCE                        
BUFFRECQ EQU   *-BUFFREC           BUFFALO RECORD LENGTH OF TRUE DATA           
BUFFJUNK DS    PL8                 ALWAYS P'1', SO BUFFALO RETURNS RECS         
*                                                                               
CURBUFF  DS    CL(BUFFRECQ)                                                     
SVBUFKEY DS    CL(BUFFKEYQ)        SAVED BUFFALO KEY                            
SVMODE   DS    XL1                                                              
SVR2     DS    F                                                                
*                                                                               
BUFSW    DS    X                   BUFFALO CONTROL SWITCH                       
BUFEOF   EQU   X'80'                                                            
*                                                                               
NEWKEY   DS    CL15                NEW ACCOUNT KEY                              
NEWSW    DS    CL1                                                              
CNTBK    DS    XL1                 CONTROL BREAKS                               
CNTBKA   EQU   X'80'               LEVEL A                                      
CNTBKB   EQU   X'40'                     B                                      
CNTBKC   EQU   X'20'                     C                                      
         EJECT                                                                  
*                                                                               
*              BUFFALO RECORD FOR LEDGER/REQUEST SUMMARY                        
*                                                                               
BFREC    DS    0CL(BFLNQ)                                                       
BFBGN    DS    0C                                                               
BFTYPE   DS    XL1                 RECORD TYPE                                  
BFPST    EQU   X'01'               POSTING RECORD (ONLY LEVEL 1)                
*                                  TO...  OFFICE  FROM...                       
BFSUM    EQU   X'02'               SUMMARY RECORD                               
*                                  TO...          FROM...                       
BFLGLV   EQU   1                   LEVEL 1 FOR LEDGER SUMMARY                   
BFRQLV   EQU   2                   LEVEL 2 FOR REQUEST SUMMARY                  
BFRNLV   EQU   3                   LEVEL 3 FOR RUN SUMMARY                      
*                                                                               
BFACCT   DS    CL14                ACCOUNT                                      
BFCNTR   DS    CL14                CONTRA                                       
BFOFFC   DS    CL2                 OFFICE                                       
*                                                                               
BFSTAT   DS    XL1                                                              
PRODLG   EQU   X'80'               PRODUCTION LEDGER                            
LDGSRC   EQU   X'40'               .  SOURCE IS THE LEDGER                      
ACCSRC   EQU   X'20'               .  SOURCE IS THE ACCOUNT                     
PTRN     EQU   X'10'               .  POST BY TRANSACTION                       
POFCL    EQU   X'08'               .  PUT OFFICE IN LAST BYTE OF ACCT           
BFKLNQ   EQU   *-BFBGN                                                          
                                                                                
BFPTRACT DS    CL14                                                             
BFPTRNM  DS    CL36                                                             
BFNME    DS    CL36                CONTRA NAME                                  
BFBK     DS    0PL8                BUFFALO ACCUMULATORS                         
BFDR     DS    PL8                 DEBITS                                       
BFCR     DS    PL8                 CREDITS                                      
BFPDR    DS    PL8                 PREVIOUS DEBITS                              
BFPCR    DS    PL8                 PREVIOUS CREDITS                             
BFUDR    DS    PL8                 UNAUTHORIZED DEBITS                          
BFUCR    DS    PL8                 UNAUTHORIZED CREDITS                         
BFNBK    EQU   (*-BFBK)/L'BFBK                                                  
BFLNQ    EQU   *-BFBGN             RECORD LENGTH                                
         EJECT                                                                  
*              SORT RECORD FOR RUN SUMMARY AND T.B.                             
*                                                                               
SMREC    DS    0CL(SMLNQ)                                                       
SMBGN    DS    0C                                                               
SMLEDGER DS    C                   LEDGER                                       
SMACCT   DS    CL14                TO ACCOUNT                                   
SMOFFC   DS    CL2                 OFFICE                                       
         ORG   SMACCT                                                           
SMOFFCO  DS    CL2                 OFFICE (WHEN QSEQ = 'O')                     
SMACCTO  DS    CL14                ACCOUNT (WHEN QSEQ = 'O')                    
SMKLNQ   EQU   *-SMBGN             LENGTH OF KEY                                
*                                                                               
SMBK     DS    0PL8                                                             
SMDR     DS    PL8                 DEBITS                                       
SMCR     DS    PL8                 CREDITS                                      
SMNBK    EQU   (*-SMBK)/L'SMBK                                                  
SMLNQ    EQU   *-SMBGN             RECORD LENGTH                                
*                                                                               
DISK     DS    F                                                                
ACCFIL   DS    CL8                                                              
ACCDIR   DS    CL8                                                              
ACCMST   DS    CL8                                                              
GETREC   DS    CL8                                                              
ADDREC   DS    CL8                                                              
PUTREC   DS    CL8                                                              
ACCOUNT  DS    CL8                                                              
IOKEY    DS    CL(GLBRFST-GLBRECD)                                              
IOWRK    DS    CL96                                                             
*                                                                               
LV1MAX#  EQU   80                                                               
LV1#OF   DS    X                                                                
LV1LIST  DS    (LV1MAX#)XL(LV1LNQ)                                              
*                                                                               
DSLIST   DS    0C                                                               
         DSDDL PRINT=YES                                                        
*                                                                               
*                                                                               
         DS    0F                                                               
AREA     DS    CL200                                                            
*                                                                               
IOA      DS    CL2000                                                           
WORKLQ   EQU   *-WORKD                                                          
                                                                                
         EJECT                                                                  
*                                                                               
*              DSECT FOR POSTING TABLE ELEMENTS                                 
*                                                                               
PSTD     DSECT                                                                  
PSTF     DS    0C                                                               
PSTFR    DS    CL10                FROM ACCOUNT                                 
PSTFRLN  DS    CL1                 LENGTH-1 OF FROM ACCOUNT                     
PSTTO    DS    CL14                TO ACCOUNT                                   
PSTLNQ   EQU   *-PSTF                                                           
*                                                                               
*                                                                               
*                                  DSECT FOR PRINT LINE                         
*                                                                               
PLINED   DSECT                                                                  
PLINE    DS    0CL110                                                           
         DS    CL1                                                              
PLKEY    DS    CL15                KEY                                          
         DS    CL1                                                              
PLDESC   DS    CL32                DESCRIPTION                                  
         ORG   PLDESC                                                           
         DS    CL1                                                              
PLOFFCD  DS    CL2                 OFFICE CODE                                  
         DS    CL1                                                              
PLOFFNM  DS    CL28                OFFICE NAME                                  
PLFIGS   DS    4CL15               FIGURES                                      
*                                                                               
         ORG   PLINE                                                            
         DS    CL1                                                              
PLSPACES DS    CL5                 SPACES WHEN 'TOTALS FOR' FOLLOWS             
PLTOT    DS    CL10                USUALLY SET TO 'TOTALS FOR'                  
         DS    CL1                                                              
*                                                                               
*                                                                               
*              DSECT FOR LEVAGEN ETC. CONTROLS SUBROUTINES SUCH                 
*              AS LVUP & LEVLAST.                                               
*                                                                               
LEVGEND  DSECT                                                                  
LEVGEN   DS    0CL(LEVLNQ)                                                      
LEVLET   DS    CL1                 'A','B',ETC.                                 
LEVNUM   DS    CL1                 IF ZERO SHOWS LEVEL NOT WANTED               
LEVACMNO DS    CL1                 ROW NO FOR PROLLER TABLE                     
LEVKDISP DS    CL1                 DISPLACEMENT THIS LEVELS PART OF KEY         
LEVKLEN  DS    CL1                 LENGTH OF THIS LEVELS PART OF KEY            
LEVTLEN  DS    CL1                 LENGTH OF ALL LEVELS TO THIS PART            
LEVKEY   DS    CL15                              CHANGED BY LVUP                
LEVDESC  DS    CL15                'CLIENT','PRODUCT',ETC.                      
LEVNAME  DS    CL36                'RUMBLES LTD' CHANGED BY LVUP                
LEVLNQ   EQU   *-LEVLET                                                         
                                                                                
LV1D     DSECT                                                                  
LV1UL    DS    CL2                                                              
LV1LEN   DS    XL1                                                              
LV1LNQ   EQU   *-LV1D                                                           
         EJECT                                                                  
***********************************************************************         
*        DSECT for updating new GL bucket/pointers                              
***********************************************************************         
BUKTABD  DSECT                                                                  
BUKGLACT DS    CL13                Unit G                                       
BUKGLOFF DS    CL2                 Unit G office                                
BUKGLSOF DS    CL2                 Unit S office                                
BUKGLCUL DS    CL2                 Unit S contra unit/ledger                    
BUKGLOTH DS    CL7                 Spaces                                       
         ORG   BUKGLOTH                                                         
BUKGLCLI DS    CL3                 Client for payables                          
         ORG   BUKGLOTH                                                         
BUKSM    DS    CL2                 System/Media when ACTKUNT(2)=SR              
         ORG                                                                    
BUKGLMOA DS    XL2                 MOA                                          
BUKGLIND DS    CL1                 Yes/No added or wrote back record            
BUKKLNQ  EQU   *-BUKTABD                                                        
BUKGL$   DS    0PL8                                                             
BUKGLDR  DS    PL8                 Debit                                        
BUKGLCR  DS    PL8                 Credit                                       
BUKRLNQ  EQU   *-BUKTABD                                                        
***********************************************************************         
*              DSECT FOR THE BINSRCH LIST                                       
***********************************************************************         
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISP. TO FIRST BUCKET                        
BINSTAT  DS    CL1                 X'80' BINARY DATA                            
         DS    CL1                 SPARE                                        
BINTABLE DS    0CL1                                                             
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
BUKELD   DSECT                                                                  
         ORG   BUKDR                                                            
BUKDR7   DS    PL7                                                              
BUKCR7   DS    PL7                                                              
BUKLN7Q  EQU   *-BUKELD                                                         
                                                                                
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
* ACLANGEQU                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACLANGEQU                                                      
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT ON                                                               
       ++INCLUDE ACMASTD                                                        
         PRINT OFF                                                              
* ACOFFALD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACOFFALD                                                       
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* DDBUFFALOD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DMWRKRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKRK                                                        
         PRINT ON                                                               
* ACBMONVALD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBMONVALD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049ACREP2502 02/25/20'                                      
         END                                                                    
